# Table of Contents
- [Table of Contents](#table-of-contents)
- [Eval](#eval)
  - [Basic functionality](#basic-functionality)
    - [Eval.now](#evalnow)
    - [Eval.later](#evallater)
    - [Eval.always](#evalalways)
  - [Stack-safety](#stack-safety)
    - [Example #1](#example-1)
    - [Example #2](#example-2)
    - [Eval.defer](#evaldefer)
    - [Additional example](#additional-example)
  - [Eval.memoize](#evalmemoize)
    - [Example](#example)
  - [Conclusions](#conclusions)
  - [Used resources:](#used-resources)

# Eval
I decided to do my homework about Eval data type. Full code for each example is accessible in this directory.

Eval is one of fundamental data types from Cats library, and it is used a lot inside of it (State, Foldable, etc.). It is designed to provide stack-safe lazy evaluation of your expressions with something that is called trampolining. Trampolining is basically a more advanced implementation of tail recursion. Scala compiler can optimise only a very specific recursive functions. For example, a function that recursively reverses a string:
```scala
def reverse (in: String, out: String): String =
  if (in.length != 0) 
    reverse(in drop 1, s"${in.head}$out")
  else out

reverse("abc", "") // returns "cba"
```
This function is optimised by compiler and provides stack safety. But what if we wanted to create tail-recursion using two functions that call each other? Let's try to imagine some bizarre mutual recursion example (couldn't come up with something practical yet simple):
```scala
def findCharWithFinder (
  f: (Char, String) => Boolean,
  str: String
): Boolean = 
  if (0 < str.length) f(str.head, str drop 1)
  else false

val fFinder: (Char, String) => Boolean = 
  (c, str) => 
    if (c == 'f') true 
    else findCharWithFinder(fFinder, str)

val hFinder: (Char, String) => Boolean = 
  (c, str) => 
    if (c == 'h') true 
    else findCharWithFinder(hFinder, str)
```
`findCharWithFinder` returns true if char exists in a string, and false if it does not. For char comparison it uses function `f` passed as a parameter. 

You can tell that those functions are mutually recursive. Furthermore, they are actually tail-recursive in relation to each other! In every one of those function call is the last thing this function does. So, they should be optimised by compiler and be stack-safe, right? Wrong.
```scala
findCharWithFinder(
  fFinder,
  List.fill(1_000)("abcdefg").mkString
) // returns true

findCharWithFinder(
  hFinder,
  List.fill(1_000)("abcdefg").mkString
) // throws StackOverflowException
```
## Basic functionality
This is where `cats.Eval` and "trampolining" comes to the rescue. Let's check out some basic functionality. We'll import it and create some function with expensive computations:
```scala
import cats.Eval

def superExpensiveCalculation (a: Int, b: Int) = a + b 
// good enough
```
### Eval.now
`Eval.now` is the first way to create Eval. Expression that you pass to it will be evaluated "now", therefore the name. You can compare it to the basic `val`. It is called "eager" computation.
```scala
val evalNow = Eval.now {
  println(">>>Evaluating evalNow...")
  superExpensiveCalculation(2, 2)
}
```
### Eval.later
`Eval.later` creates an Eval, which value is evaluated... later, when we access it. After evaluation it is memoized (cached), so it is evaluated only once. It is comparable to `lazy val`. This computation is described as "lazy and memoized".
```scala
val evalLater = Eval.later {
  println(">>>Evaluating evalLater...")
  superExpensiveCalculation(2, 2)
}
```
### Eval.always
`Eval.always` creates an Eval, which value is evaluated every time you access it. It is never memoized by default. You can compare it to `def`. It is a "lazy" computation.
```scala
val evalAlways = Eval.always {
  println(">>>Evaluating evalAlways...")
  superExpensiveCalculation(2, 2)
}
```
### Example
You can access value stored in Eval with `Eval.value`. Let's see this behaviour in action: 
```scala
println("Evals are already initialised but their value haven't been used yet")
println()
println(s"evalNow value is ${evalNow.value}")
println(s"evalNow value is ${evalNow.value}")
println()
println(s"evalLater value is ${evalLater.value}")
println(s"evalLater value is ${evalLater.value}")
println()
println(s"evalAlways value is ${evalAlways.value}")
println(s"evalAlways value is ${evalAlways.value}")
```
```
Program output: 

>>>Evaluating evalNow...
Evals are already initialised but their value haven't been used yet

evalNow value is 4
evalNow value is 4

>>>Evaluating evalLater...
evalLater value is 4
evalLater value is 4

>>>Evaluating evalAlways...
evalAlways value is 4
>>>Evaluating evalAlways...
evalAlways value is 4
```
[Full code of example above](https://github.com/vijexa/evo-scala-bootcamp-homework/blob/master/src/main/scala/homework10/EvalTest.scala)

As expected, `evalNow` is evaluated only once even before we use it. `evalLater` is not evaluated when we initialise it, but when we use it for the first time. For the second time it uses memoized value. `evalAlways` evaluates it's expression every time it is accessed.

## Stack-safety
But why do you care? It seems kind of useless, especially `Eval.now`. Well, turns out Evals have its uses. Let's go back to our mutual recursion, but now we'll use Evals. Let's put previous code that wasn't using eval in an object `NoEval`, and create new object `UsingEval`:
### Example #1
[Full code](https://github.com/vijexa/evo-scala-bootcamp-homework/blob/master/src/main/scala/homework10/MutualRec2.scala)
```scala
object NoEval {
  def findCharWithFinder (
    f: (Char, String) => Boolean,
    str: String
  ): Boolean = 
    if (0 < str.length) f(str.head, str drop 1)
    else false

  val fFinder: (Char, String) => Boolean = 
    (c, str) => 
      if (c == 'f') true 
      else findCharWithFinder(fFinder, str)

  val hFinder: (Char, String) => Boolean = 
    (c, str) => 
      if (c == 'h') true 
      else findCharWithFinder(hFinder, str)
}

object UsingEval {
  def findCharWithFinder (
    f: (Char, String) => Eval[Boolean],
    str: String
  ): Eval[Boolean] = 
    Eval.always(0 < str.length).flatMap{
      case true  => f(str.head, str drop 1)
      case false => Eval.False
    }

  val fFinder: (Char, String) => Eval[Boolean] = 
    (c, str) => 
      Eval.always(c == 'f').flatMap{
        case true  => Eval.True
        case false => findCharWithFinder(fFinder, str)
      }

  val hFinder: (Char, String) => Eval[Boolean] = 
    (c, str) => 
      Eval.always(c == 'h').flatMap{
        case true  => Eval.True
        case false => findCharWithFinder(hFinder, str)
      }
}
```
First of all, all our functions return `Eval[Boolean]` now. We are not returning `Boolean`! We are returning a monad `Eval` that contains some value of `Boolean`. Secondly, we are evaluating our comparison in `Eval.always`, which gives us an Eval that we can use.  You can also use `Eval.now` or `Eval.later` in this case it's not very important. Finally, our main trick here is that we are using `flatMap`. `Eval` is a monad, and like every well mannered monad it can be flatMapped over it to extract its value and return another instance of this monad. The special thing about `Eval.flatMap` is that it is lazy and therefore **stack-safe**. This trick let's us to defer actual execution without blowing up the stack. Here is what is written about it in the source code:
```scala
/**
 * Lazily perform a computation based on an Eval[A], using the
 * function `f` to produce an Eval[B] given an A.
 *
 * This call is stack-safe -- many .flatMap calls may be chained
 * without consumed additional stack during evaluation. It is also
 * written to avoid left-association problems, so that repeated
 * calls to .flatMap will be efficiently applied.
 *
 * Computation performed in f is always lazy, even when called on an
 * eager (Now) instance.
 */
def flatMap[B](f: A => Eval[B]): Eval[B]
```
Everything we write in passed to `flatMap` function is evaluated lazily! Amazing! In fact, as seen from `flatMap` signature, it returns another Eval (well, that's almost a definition for monad). Same laziness rule applies to `map` too. Let's try it now:
```scala
val string = List.fill(1_000)("abcdefg").mkString

try {
  print("Finding f without using eval: ")
  println(
    NoEval.findCharWithFinder (
      NoEval.fFinder,
      string
    ) // returns true
  )

  print("Finding h without using eval: ")
  println(
    NoEval.findCharWithFinder(
      NoEval.hFinder,
      string
    ) // throws StackOverflowException
  )
} catch {
  case _: StackOverflowError => println("Oops, stackoverflow")
}

print("Finding f with eval: ")
println(
  UsingEval.findCharWithFinder (
    UsingEval.fFinder,
    string
  ).value
) // returns true

print("Finding h with eval: ")
println(
  UsingEval.findCharWithFinder (
    UsingEval.hFinder,
    string
  ).value
) // returns false, and no stack overflow!

```
```
Program output:

Finding f without using eval: true
Finding h without using eval: Oops, stackoverflow
Finding f with eval: true
Finding h with eval: false
```
As you can see, everything works as expected. Regular recursion indeed throws an exception, while our lazy recursion using Eval reliably evaluates to Boolean.

Let's imagine that we want to implement factorial function, but for some reason we cannot or don't want to use tail recursion. Let's try to implement it like in first example:
### Example #2
[Full code](https://github.com/vijexa/evo-scala-bootcamp-homework/blob/master/src/main/scala/homework10/EvalFactorial.scala)
```scala
import cats.Eval

val n = 10000

def factorialNoEval (x: BigInt): BigInt =
  if (x == 0)
    1
  else
    x * factorialNoEval(x - 1)

def factorialEval (x: BigInt): Eval[BigInt] =
  Eval.now(x == 0).flatMap{
    case true  => Eval.now(1)
    case false => factorialEval(x - 1).map(_ * x)
  }

try {
  print(s"factorialEval($n).value: ")
  println(factorialEval(n).value)
  print(s"factorialNoEval($n): ")
  println(factorialNoEval(n))
} catch {
  case _: StackOverflowError => println("Stack overflow!")
}
```
Factorial of 10000 is a huge value, so I've stripped most of it from output.
```
Program output:

factorialEval(10000).value: 28462596809170545189064132121198688901480514017027992307941799942744113400037644437729907867577847758158840621423175288300423399401535187....... 
factorialNoEval(10000): Stack overflow!
```
 So, what happens here? `factorialNoEval` is a regular recursive function, prone to stack overflow. As expected, it cannot calculate factorial of 10000. Now, let's look closer at `factorialEval`.
```scala
def factorialEval (x: BigInt): Eval[BigInt] =
  Eval.now(x == 0).flatMap{
    case true  => Eval.now(1)
    case false => factorialEval(x - 1).map(_ * x)
  }
```
The algorithm itself hasn't changed, but now we are using monadic function `map` to implement factorial multiplication. As we remember, it is stack safe and lazy for Evals. But with `flatMap` this method looks kind of... Cumbersome?

### Eval.defer

Actually, there is a method called `Eval.defer` that allows us to do the same thing but cleaner. Let's rewrite our factorial using it: 
```scala
def factorialEval (x: BigInt): Eval[BigInt] =
  if (x == 0) Eval.now(1)
  else
    Eval.defer(factorialEval(x - 1).map(_ * x)) 
```
What `Eval.defer does`? Let's look at its signature:
```scala
def defer[A](a: => Eval[A]): Eval[A]
```
Long story short, it allows us to transform expression that returns `Eval[A]` into `Eval[A]` itself. As you can understand from its name, it *defers* evaluation of passed expression. It makes evaluation of this expression stack-safe. But wait, wasn't `.map` stack-safe too? Can't we do it like this?
```scala
def factorialEval (x: BigInt): Eval[BigInt] =
  if (x == 0) Eval.now(1)
  else
    factorialEval(x - 1).map(_ * x)
```
Long story short, this results in stack overflow. But why, have I lied to you about stack safety? Well, let's look at this method a bit closer. What happens here is that we haven't hidden our actual recursion into any `Eval`! `factorialEval(x - 1)` is not lazy and it never was! So, each "iteration" we are explicitly going deeper into recursion, without deferring it. This results in stack overflow. `Eval.defer`, as well as `Eval.flatMap`, helps us to prevent this.

### Additional example
[Modified example from Cats documentation](https://github.com/vijexa/evo-scala-bootcamp-homework/blob/master/src/main/scala/homework10/MutualRec1.scala)

## Eval.memoize
`Eval.always` can be memoized at any time after its creation. It can be useful if some former part of your computation is very expensive, but can't change in the future, while some latter part of your computation depends on some variable and therefore should be reevaluated every time. We can use `Eval.memoize` for this:
### Example
[Full Code](https://github.com/vijexa/evo-scala-bootcamp-homework/blob/master/src/main/scala/homework10/MemoizeTest.scala)
```scala
import cats.Eval

val eval = 
  Eval.always(println("First expensive computation"))
  .map(_ => println("Second expensive computation"))
  .map(_ => println("Third expensive computation"))
  .memoize
  .map(_ => println("Fourth expensive computation" +
    " that depends on some dynamic variable" +
    " and cannot be memoized"))

println("Accessing eval for the first time:")
eval.value
println("\nAccessing eval for the second time:")
eval.value
```
```
Program output:

Accessing eval for the first time:
First expensive computation
Second expensive computation
Third expensive computation
Fourth expensive computation that depends on some dynamic variable and cannot be memoized

Accessing eval for the second time:
Fourth expensive computation that depends on some dynamic variable and cannot be memoized
```

## Conclusions
Eval is a nice tool to make heavy computations on large amounts of data safer. Most of the time Eval is used without you knowing about it, but I think it is important to know what happens under the hood of Cats library.

## Used resources: 
* Cats.Eval – https://typelevel.org/cats/datatypes/eval.html
* Demystified Scala Eager Lazy Memoized - How Cats Eval Can Safe Your Recursive Stack For Overflowing – https://edward-huang.com/tech/scala/programming/functional-programming/2020/01/12/demystified-scala-eager-lazy-memoized-how-cats-eval-can-safe-your-recursive-stack-for-overflowing/
* Trampolining and stack safety in Scala – https://medium.com/@olxc/trampolining-and-stack-safety-in-scala-d8e86474ddfa
