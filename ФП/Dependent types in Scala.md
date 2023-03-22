#ФП/Scala #ФП/ЗависимыеТипы 

[оригинал](https://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html)

[@OweinReese](https://twitter.com/OweinReese)


[Ограничения зависимых от путей типов](https://stackoverflow.com/questions/73832836/scala-3-dealing-with-path-dependent-types)


## What is a Dependent Type?

```

trait DepValue{
  type V
  val value: V
}

def magic(that: DepValue): that.V = that.value
							
```

The return type of "magic" _depends_ on the argument passed in.

#### Toy Example

```

def mk[T](x: T) = new DepValue{ 
  type V = T
  val value = x
}
val depInt = mk(1)
val depString = mk("two")

val itWorks: Int = magic(depInt)
val again: String = magic(depString)
						
```

#### Other Forms

```

trait Foo{
	class Bar
	def doNothing(b: Bar){}
}
val f1 = new Foo{}
val b1 = new f1.Bar()
val f2 = new Foo{}
val b2 = new f2.Bar()

f1.doNothing(b1) //fine
f1.doNothing(b2) //won't compile
						
```

#### In the Standard Library

```

package scala.collection.generic

trait IsTraversableOnce[Repr]{
  type A
  val conversion: Repr => GenTraversableOnce[A]
}

trait IsTraversableLike[Repr]{
  type A
  def conversion(repr: Repr): GenTraversableLike[A, Repr]
}
						
```

### First Example

Building a type extractor.

```

trait Inner[F]{
  type T
}
						
```

```

object Inner{
  def apply[F](implicit inner: Inner[F]) = inner

  implicit def mk[F[_], A] = new Inner[F[A]]{
    type T = A
  }
}
							
```

### A More Practical Example

```

trait IsFuture[F]{
  type T

  def apply(f: F): Future[T]
}

object IsFuture{
  def apply[F](implicit isf: IsFuture[F]) = isf

  implicit def mk[A] = new IsFuture[Future[A]]{
    type T = A

    def apply(f: Future[A]): Future[A] = f
  }
}
						
```

### Use as an Implicit Guard

```

def logResult[Thing](thing: Thing)
  (implicit isf: IsFuture[Thing]): Future[isf.T] = 
    isf(thing) map{ x =>
      log info s"I got a result of $x"
      x
    }
  						
```

Here, IsFuture witnesses that the type "Thing" is a Future and allows us to convert it to it's explicit and fully qualified type.

#### Take Away

1.  Use companion objects to hide implicit creation and any other boilerplate that might be needed to ultimately construct the dependently typed object.
2.  Expose the constructed type classes using an apply that returns the implicit sought.
3.  Dependent types can used as type parameters
4.  Dependent types allow us to safely cross type boundaries.

### Capturing More Than One Type

```

trait Apart[F]{
  type T
  type W[X]

  def apply(f: F): W[T]
}
						
```

```

object Apart{
  def apply[F](implicit apart: Apart[F]) = apart

  implicit def mk[F[_], A] = new Apart[F[A]]{
    type T = A
    type W[X] = F[X]

    def apply(f: F[A]): W[T] = f
  }
}
							
```

### An Aside

This is more than a type constructor

```
type W[X]
```

It is an equation.

Valid declarations

```

trait Demo[F[_]]{
  type W[X] = F[X]
  type Ignore[X] = F[Int]
  type Identity[X] = X
  type Const[X] = Int
}
							
```

### Type Refinements

```

object Apart{
  def apply[F](implicit apart: Apart[F]) = apart

  type Aux[FA, A, F[_]] = Apart[FA]{ type T = A; type W[X] = F[X] }

  implicit def mk[F[_], A]: Aux[F[A], A, F] = new Apart[F[A]]{
    type T = A
    type W[X] = F[X]

    def apply(f: F[A]): W[T] = f
  }
}
						
```

What does this give us?

### Dependent Type Capture

```

import scalaz._

def mapZero[Thing, F[_], A](thing: Thing)
  (implicit apart: Apart.Aux[Thing, A, F], 
                f: Functor[F], 
                m: Monoid[A]): F[A] =
    f.map(apart(thing))(_ => m.zero)
							
```

### Take Away

1.  You are not restricted to a single abstract type when using dependent types.
2.  Type refinements can be used as a mechanism to capture the depedent types within the same implicit declaration.
3.  Captured types do not count as a "free" type parameter.
4.  Captured types can be used by _other_ type classes to place constraints on types

### Type Level "if" Statement

ApplyEither is like a compile time "orElse."

```

val out = ApplyEither(1, {x: Int => 42}, {x: Double => "no"})
assert(out == 42)

val out2 = ApplyEither(2.0, {x: Int => 42}, {x: Double => "no"})
assert(out2 == "no")
						
```

The function that compiles is the function that is chosen.

### The Set Up

```

object ApplyEither{
  def apply[T, F, G](t: T, f: F, g: G)
    (implicit ea: EApply[T, F, G]): ea.Out = ea(t, f, g)
}

trait EApply[T, F, G]{
  type Out

  def apply(t: T, f: F, g: G): Out
}
						
```

### Define EApply Creation

```

object EApply extends LowPriorityEApply{
  def apply[T, F, G](implicit ea: EApply[T, F, G]) = ea

  implicit def fapply[T, R, G] = new EApply[T, T => R, G] {
      type Out = R
      def apply(t: T, f: T => R, g: G) = f(t)
    }
}
trait LowPriorityEApply{
  implicit def gapply[T, R, F] = new EApply[T, F, T => R] {
      type Out = R
      def apply(t: T, f: F, g: T => R) = g(t)
    }
}
						
```

### Take Away

1.  Pattern matching on types only works if the types are mutually exclusive
2.  Use implicit resolution order to avoid implicit ambiguity.
3.  Don't forget to use the dependent type to help the compiler avoid "Any" or "AnyRef."

### Compile Time Loops

Limitations on Inner and Apart?

```

case class Foo[V](value: V)

def zero[T, A](t: T)(implicit inner: Inner.Aux[T, A], 
                              m: Monoid[A]): inner.T = m.zero

zero(Foo(Foo(1))) //won't compile!
						
```

Neither goes deeper than the first type constructor.

### A Typeclass Like "Inner"

```

trait Unwrap[F]{
  type Inner
}
						
```

but...

### Recurse Over the Types

```

object Unwrap extends LowPriorityUnwrap{
  def apply[F](implicit unwrap: Unwrap[F]) = unwrap

  type Aux[T, A] = Unwrap[T] { type Inner = A }

  implicit def nested[F[_], G](implicit unwrap: Unwrap[G]) =
    new Unwrap[F[G]]{
      type Inner = unwrap.Inner
    }
}
trait LowPriorityUnwrap{
  implicit def bottom[F[_], A] =
    new Unwrap[F[A]]{
      type Inner = A
    }
}
						
```

### Take Two

```

def zero[T, A](t: T)(implicit unwrap: Unwrap.Aux[T, A], 
                              m: Monoid[A]): unwrap.Inner = m.zero

val out = zero(Foo(Foo(1)))
assert(out == 0)
						
```

This compiles and now can access the inner type parameter of the nested Foo, Int.

### Take Away

1.  Dependent types can be used to pass type information around when solving for types.
2.  Implicitly generated traits can be defined in terms of themselves, albeit with different type parameters.
3.  Loops require a type structure that can be recursed over.

### Doing Something Useful

Let's use types to guide the behavior of our application in a more direct manner. Who likes writing this?

```

def annoy[A](that: Future[List[Set[Int]]], 
                f: Int => A): Future[List[Set[A]]] =
  that map{ 
    _ map{
      _ map f
  }
}
						
```

### Define a Nested Mapper

```

object MapIt{
  def apply[A, B, C](in: A, f: B => C)
    (implicit mapper: Mapper[A, B, C]): mapper.Out = mapper(in, f)
}
trait Mapper[A, B, C]{
  type Out

  def apply(a: A, f: B => C): Out
}
						
```

### Define the Type Recursive Algorithm

```

object Mapper{
  def apply[A, B, C](implicit mapper: Mapper[A, B, C]) = mapper

  implicit def recur[F[_], A, B, C](implicit nested: Mapper[A, B, C], 
                                             f: Functor[F]) =
    new Mapper[F[A], B, C] {
      type Out = F[nested.Out]
      def apply(fa: F[A], g: B => C): Out = f.map(fa)(nested(_, g))
    }
  implicit def base[F[_], A, B >: A, C](implicit f: Functor[F]) =
    new Mapper[F[A], B, C]{
      type Out = F[C]
      def apply(fa: F[A], g: B => C) = f.map(fa)(g)
    }
}
						
```

### Type Equality

Value equality checks out.

```

val in = List(List(1))
val out = MapIt(in, {x: Int => x+1})

assert(List(List(2)) == out)
						
```

but value equality doesn't test types...

```

scala> identity[List[List[Int]]](out)
type mismatch;

found: Mapper[List[List[Int]], Int, Int]#Out
required: List[List[Int]]
							
```

### Type Refinement to the Rescue

```

object Mapper{
  type Aux[A, B, C, Out0] = Mapper[A, B, C]{ type Out = Out0 }

  implicit def recur[F[_], A, B, C]
    (implicit nested: Mapper[A, B, C], 
              f: Functor[F]): Aux[F[A], B, C, F[nested.Out]] =

  implicit def base[F[_], A, B >: A, C]
    (implicit f: Functor[F]): Aux[F[A], B, C, F[C]] =
}
						
```

### Take Away

1.  You can create the mother of all abstractions using dependent types.
2.  Type inference can fail in spectacular ways, especially with dependent types.
3.  Type refinement can help the compiler recapture "lost" type information.

### Proving Algorithmic/Structural Correctness

If an algorithm or a structure has invariants, you can prove those invariants within the type system, i.e. if it compiles, it's correct!

### Balanced Tree

A balanced tree where the left branch must be greater than or equal to right branch in height.

```

import shapeless._

sealed trait Tree[+T]{
  type N <: Nat
}
object Leaf extends Tree[Nothing]{
  type N = _0
}
trait Branch[T] extends Tree[T]{
  def value: T
  def left: Tree[T]
  def right: Tree[T]
}
						
```

### Another Aside

I'm cheating. I'm going to use Nat's from Shapeless. There's a few type classes/witnesses I get for free:

1.  Type level ordering: `LT[N,M]` and `LTEq[N,M]`.
2.  Type level operators: `Mult[N, M]`, `Div[N,M]`, `Sum[N,M]`, `Pow[N,M]` and `Min[N,M]`.
3.  Type level difference (where left Nat >= right Nat) `Diff[N,M]`.

### The Proof

```

object Branch {
  type Aux[T, N0 <: Nat] = Branch[T] { type N = N0 }

  def apply[T, D <: Nat](value0: T, left0: Tree[T], right0: Tree[T])
    (implicit diff: Diff.Aux[left0.N, right0.N, D],
              lte: LTEq[D, _1]): Aux[T, Succ[left0.N]] =
      new Branch[T] {
        val value = value0
        val left = left0
        val right = right0
        type N = Succ[left0.N]
      }
}
						
```

### Does it Work?

```

val b1 = Branch(1, Leaf, Leaf)
val b2 = Branch(2, Leaf, Leaf)
val higher = Branch(3, b1, Leaf)
val higher2 = Branch(4, b2, b1)

val nope = Branch(42, higher, Leaf) //won't compile
val nadda = Branch(42, Leaf, b1) //won't do it either
						
```

Sure does!

### More General Case

What we'd really like is if we could have a balanced tree not restricted to left heavy.

```

val lower = Branch(1, Leaf, Leaf)
val right = Branch(42, Leaf, lower)
						
```

The problem is how we originally defined branch creation.

### Use an Indirection

You can't directly map to a single method for mutually exclusive proofs.

```

object Branch{
  def apply[T](value: T, left0: Tree[T], right0: Tree[T])
    (implicit tb: TreeBuilder[T, left0.N, right0.N]): Aux[T, tb.Height] = 
      tb(value, left0, right0)
}
trait TreeBuilder[T, N <: Nat, M <: Nat]{
  type Height <: Nat
  def apply(v: T, l: Tree[T], r: Tree[T]): Branch.Aux[T, Height]
}
						
```

### Define Like Before

```

object TreeBuilder with LowPriorityTreeBuilder{
  def apply[T, N <: Nat, M <: Nat]
  	(implicit tb: TreeBuilder[T, N, M]): Aux[T, N, M, tb.Height] = tb

  type Aux[T, N <: Nat, M <: Nat, Height0 <: Nat] = 
    TreeBuilder[T, N, M] {type Height = Height0}
						
```

A paradox, the Aux capturing the dependent type is defined in terms of the dependent type.

### Half The Proof

```

  implicit def mkLeft[T, N0 <: Nat, M <: Nat, D <: Nat]
    (implicit diff: Diff.Aux[N0, M, D], 
              lte: LTEq[D, _1]): Aux[T, N0, M, Succ[N0]] =
      new TreeBuilder[T, N0, M] { self =>
        type Height = Succ[N0]
        def apply(value0: T, left0: Tree[T], right0: Tree[T]) =
          new Branch[T] {
            val value = value0
            val left = left0
            val right = right0
            type N = self.Height
          }
      }
}
						
```

Flip "right" and "left" for the other half

### Take Away

1.  Proofs are only as good as their use can be enforced, i.e. don't let your code work around it.
2.  Multiple valid conditions sometimes require the use of a secondary type class that nests the proof.
3.  Sometimes the type refinement has to be defined by the type that has paradoxically been "lost."

### Final Thoughts

1.  There are compile time costs.
2.  Sometimes there are run time costs too.
3.  Understand that proofs are programs in and of themselves except they can't be easily unit tested. It compiles or it doesn't!