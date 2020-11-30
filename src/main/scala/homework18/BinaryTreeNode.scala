package homework18

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case ins: Insert    => doInsert(ins)
    case cont: Contains => doContains(cont)
    case rem: Remove    => doRemove(rem)
  }

  def finish(op: Operation) = op.requester ! OperationFinished(op.id)

  private def getNextNode(nextElem: Int) = {
    val nextPos = 
      if (nextElem > elem) 
        Right
      else 
        Left
    
    (nextPos -> subtrees.get(nextPos))
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      removed = false
      finish(m)
    } else {
      getNextNode(m.elem) match {
        case (_, Some(nextRef)) => nextRef ! m
        case (pos, None) => 
          val newNode = context.actorOf(props(m.elem, false))
          subtrees += (pos -> newNode)
          finish(m)
      }
    }
  }

  private def doContains(m: Contains): Unit = {
    if(m.elem == elem) {
      m.requester ! ContainsResult(m.id, !removed)
    } else {
      getNextNode(m.elem) match {
        case (_, Some(nextRef)) => nextRef ! m
        case (_, None) => m.requester ! ContainsResult(m.id, false)
      }
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      finish(m)
    } else {
      getNextNode(m.elem) match {
        case (_, Some(nextRef)) => nextRef ! m
        case (_, None) => finish(m)
      }
    }
  }
}
