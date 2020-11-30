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
  import BinaryTreeSet._, Operation._, OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case ins: Insert    => doInsert(ins)
    case cont: Contains => doContains(cont)
    case rem: Remove    => doRemove(rem)
  }

  def finish(op: Operation) = op.requester ! OperationFinished(op.id)

  private def forwardToNextNodeOr(m: Operation, onNone: Position => Unit) = {
    val nextPos = 
      if (m.elem > elem) 
        Right
      else 
        Left
    
    subtrees.get(nextPos) match {
      case Some(nextRef) => nextRef ! m
      case None => onNone(nextPos)
    }
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      removed = false
      finish(m)
    } else {
      forwardToNextNodeOr(m, { pos =>
        val newNode = context.actorOf(props(m.elem, false))
        subtrees += (pos -> newNode)
        finish(m)
      })
    }
  }

  private def doContains(m: Contains): Unit = {
    if(m.elem == elem) {
      m.requester ! ContainsResult(m.id, !removed)
    } else {
      forwardToNextNodeOr(m, _ => m.requester ! ContainsResult(m.id, false))
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      finish(m)
    } else {
      forwardToNextNodeOr(m, _ => finish(m))
    }
  }
}
