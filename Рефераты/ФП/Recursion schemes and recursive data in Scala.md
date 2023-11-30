#ФП/Scala 
# [AST playground: recursion schemes and recursive data](https://kubuszok.com/2019/ast-playground-recursion-schemes-and-recursive-data/ "AST playground: recursion schemes and recursive data")


So, we parsed an input - file, stream of characters or string - and we got a nicely structured data in the form of a tree and/or an algebraic data type. What now?

## A motivating example

Let’s say we need to generate PDF documents. We found out, that the easiest way to do it in our case is to simply create an HTML document which can be easily styled and then printed into a PDF using something like `wkhtmltopdf`. This tool allows a page numeration, footers, headers, etc (header and footer are passed as separate parameters pointing to other HTML files). We could define the structure of such a PDF generator config as:

```scala
type HTML = File

final case class CreatedDocument(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML]
)
```

> Actually, when I had to implement something like that the HTML was _never_ static, so I used Liquid file as input, passed into Liquid parser a template and parameters, which were the second argument of the call, write the generated HTML into a temporary file and use that file as `wkhtmltopdf` input, but this needlessly complicated the example so I skipped it.
> 
> Also, I really recommend you to try as much as possible in Ammonite (REPL), as things click much easier when you actually try them.

Then, you can implement the routine (since it’s not important for this article, we’ll just mock the actual implementations):

```scala
type PDF  = File

def generatePDF(config: CreatedDocument): PDF = {
  // 1. check that inputs exists
  // 2. call whktmltopdf with the right arguments
  // 3. check that output is a valid PDF
  ???
}
```

However, after a while you discover, that you need to generate one file containing something that could be easier generated as separate documents. E.g. several versions of the same document, but in a different language - each of them containing page numeration starting at 1 (because of legal reasons). And after a merge, each part still should start at page 1. Hmm, `wkhtmltopdf` isn’t nice for that (who would like an ad hoc multilingual template with hacks used to reset page counters at the right places?). But merging 2 PDFs using `pdftk` sounds simple! (Also more logical and easier to maintain). So we create another config:

```scala
final case class MergedDocuments(
  inputs: NonEmptyList[CreatedDocument]
)
```

which you can run as:

```scala
def mergePDFs(config: MergedDocuments): PDF = {
  val inputs = config.inputs.map(generatePDF)
  // 1. check inputs are valid
  // 2. run pdftk to merge them into one file
  ???
}
```

However, you don’t always need to merge documents - and _merging_ one document is pointless. Also occasionally you might need to merge in something that is already PDF and don’t have to be converted from HTML. So maybe we should restructure this config a bit:

```scala
sealed trait DocumentConfig
final case class ExistingDocument(
  file: PDF
) extends DocumentConfig
final case class CreatedDocument(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML]
) extends DocumentConfig
final case class MergedDocuments(
  // NEL is not a part of Scala std library
  // you can use one from cats or scalaz
  inputs: NonEmptyList[DocumentConfig]
) extends DocumentConfig

def generatePDF: DocumentConfig => PDF = {
  case ExistingDocument(file) => file
  case CreatedDocument(i, h, f) =>
    // 1. check that inputs exists
    // 2. call whktmltopdf with the right arguments
    // 3. check that output is a valid PDF
    ???
  case MergedDocuments(is) =>
    val inputs = is.map(generatePDF)
    // 1. check inputs are valid
    // 2. run pdftk to merge them into one file
    ???
}
```

It looks nice. You use this procedure for a while and then suddenly you learn, that you have to add support for DjVu, Mobi, and whatever (because your code is now used not only for legal documents but also for something that people would like to read on their Kindle for some reason). You look at the implementations and they are like:

```scala
def generateSth: DocumentConfig => Sth = {
  case ExistingDocument(file) =>
    // well, here we have to actually assume,
    // that the document is of type Sth
    file
  case CreatedDocument(i, h, f) =>
    // call some command
    ???
  case MergedDocuments(is) =>
    // recursively call function on all inputs
    val inputs = is.map(generateSth)
    // and then call some other command
    ???
}
```

At this point, you start to suspect, that the common part could probably get extracted into a separate function.

### First refactoring

Let’s try designing this function that would traverse our tree, where all specific operations will be passed as arguments:

```scala
def generate[Sth](
  create: (HTML, Option[HTML], Option[HTML]) => Sth,
  merge:  NonEmptyList[Sth] => Sth
): DocumentConfig => Sth = {
  case ExistingDocument(file) =>
    file
  case CreatedDocument(i, h, f) =>
    create(i, h, f)
  case MergedDocuments(is) =>
    val inputs = is.map(generate(create, merge))
    merge(inputs)
}

// Let's assume for our convenience, that we already
// defined all functions that we pass into generate:
val generatePDF: DocumentConfig => Future[PDF] =
  generate[PDF](createPDF)(mergePDFs)
val generateDjVu: DocumentConfig => Future[DjVu] =
  generate[DjVu](createDjVu)(mergeDjVus)
```

It works, but there is a tiny, little issue - `HTML`, `PDF` etc are type aliases for `File`. If we use an actual newtype (or tagged type or value class), then `ExistingDocument` will have to become parametric. And as a result whole `DocumentConfig` will have to be parametric:

```scala
sealed trait DocumentConfig[+A]

// Now, we define this file in a type-safe way
final case class ExistingDocument[+A](
  file: A 
) extends DocumentConfig[+A]

// This node of a tree does not contain anything specific
// to A so it can be used for all trees (thus A=Nothing)
final case class CreatedDocument(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML]
) extends DocumentConfig[Nothing]

// this also needs to be parametrized
final case class MergedDocuments[+A](
  inputs: NonEmptyList[DocumentConfig[A]]
) extends DocumentConfig[A]
```

```scala
// of course, we also need to add this type param everywhere

def generate[Sth](
  create: (HTML, Option[HTML], Option[HTML]) => Sth,
  merge:  NonEmptyList[Sth] => Sth
): DocumentConfig[Sth] => Sth = {
  case ExistingDocument(file) =>
    file
  case CreatedDocument(i, h, f) =>
    create(i, h, f)
  case MergedDocuments(is) =>
    val inputs = is.map(generate(create, merge))
    merge(inputs)
}

val generatePDF: DocumentConfig[PDF] => Future[PDF] =
  generate[PDF](createPDF)(mergePDFs)
val generateDjVu: DocumentConfig[DjVu] => Future[DjVu] =
  generate[DjVu](createDjVu)(mergeDjVus)
```

Now, it should work in a type-safe way.

### Unexpected consequences

This parametrization actually, gave us 2 interesting properties. First - we can now generate a document, by recursively collapse different `DocumentConfig`s into one `Sth`, just like we did in `generatePDF` and `generateDjvu` functions.

The other consequence is that we can now map over `A` (turn `DocumentConfig` into a functor):

```scala
// we can add a map to ADT

sealed trait DocumentConfig[+A] {
  
  def map[B](f: A => B): DocumentConfig[B]
}

final case class ExistingDocument[+A](
  file: A 
) extends DocumentConfig[+A] {
  
  def map[B](f: A => B): DocumentConfig[B] =
    ExistingDocument(f(file))
}

final case class CreatedDocument(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML]
) extends DocumentConfig[Nothing] {
  
  def map[B](f: Nothing => B): DocumentConfig[B] = this
}

final case class MergedDocuments[+A](
  inputs: NonEmptyList[DocumentConfig[A]]
) extends DocumentConfig[A] {
  
  def map[B](f: A => B): DocumentConfig[B] =
    MergedDocuments(inputs.map(f))
}
```

```scala
// and use it here

val djvu2pdf: DjVu => PDF

// generate one Djvy, then convert to PDF

val generateDjVu: DocumentConfig[DjVu] => DjVu =
  generate[DjVu](createDjVu)(mergeDjVus)
val generateThenConvert: DocumentConfig[DjVu] => PDF =
  generateDjVu andThen (_.map(djvu2pdf))

// convert DjVus to PDFs, then generate one PDF

val generatePDF: DocumentConfig[PDF] => PDF =
  generate[PDF](createPDF)(mergePDFs)
val convertThenGenerate: DocumentConfig[DjVu] => PDF =
  (_.map(djvu2pdf)) andThen generatePDF
```

It seems that it will open up a lot of possibilities for simplifying toolchain in the future.

## Recursion schemes

The idea that we started to develop - that if we split traversing/collapsing/etc of recursive data structures and mapping/doing something specific with their values - is known as **recursion schemes**. We don’t think about them in our everyday life, but actually, a lot of operations we use are recursion schemes! For instance, if I modified `generate` a bit, renamed it `fold` and made it a method on `DocumentConfig`:

```scala
sealed trait DocumentConfig[+A] {
  
  def map[B](f: A => B): DocumentConfig[B]
  
  // unfortunatelly we have to pass this create around
  def fold(create: (HTML,
                    Option[HTML],
                    Option[HTML]) => A))
          (f: (A, A) => B): A =
    this match {
      case ExistingDocument(a) =>
        a
      case CreatedDocument(i, h, f) =>
        create(i, h, f)
      case MergedDocuments(is) =>
        val inputs = is.map(_.fold(create)(f))
        // recursively merges all As
        def mergeAll(a: A): List[A] => A = {
          case b :: tail =>
            mergeAll(merge(a, b))(tail)
          case Nil =>
            a
        }
        mergeAll(inputs.head)(inputs.tail)
    }
}
```

then it would appear, that conceptually:

```scala
val djvu1: Djvu = generate[DjVu](createDjVu)(mergeDjVus)
```

is the same as:

```scala
val djvu2: Djvu = documentDjVu.fold(createDjVu)(mergeDjVu)
```

It traverses the structure which is preserving its inner order and it combines items inside of it, using `f` as a way of combining items. In our case, there is also the `create` part that obfuscates the design a bit, but that can be easily fixed: instead of considering 2 separate cases for existing document and created document, we can use just one type defined with `Either:`

```scala
sealed trait DocumentConfig[+A] {
  
  def map[B](f: A => B): DocumentConfig[B]
  
  // this got simpler
  def fold(f: (A, A) => A): A =
    this match {
      case CreatedDocument(a) =>
        a
      case MergedDocuments(is) =>
        val inputs = is.map(_.fold(create)(f))
        // recursively merges all As
        def mergeAll(a: A): List[A] => A = {
          case b :: tail =>
            mergeAll(merge(a, b))(tail)
          case Nil =>
            a
        }
        mergeAll(inputs.head)(inputs.tail)
    }
}

// to support both existing docs and created
// we'll just set:
//   A = Either[Sth,(HTML, Opt[HTML], Opt[HTML])]
final case class CreatedDocument[+A](
  params: A
) extends DocumentConfig[A] {
  
  def map[B](f: A => B): DocumentConfig[B] =
    CreatedDocument(f(params))
}

final case class MergedDocuments[+A](
  inputs: NonEmptyList[DocumentConfig[A]]
) extends DocumentConfig[A] {
  
  def map[B](f: A => B): DocumentConfig[B] =
    MergedDocuments(inputs.map(f))
}
```

```scala
// these types got nasty-long :P
val documentDjVu: DocumentConfig[
  Either[Djvu,
         (HTML, Option[HTML], Option[HTML])]
]

val djvu: DjVu = documentDjVu.map {
  case Left(djvu) =>
    djvu
  case Right((input, header, footer)) =>
    createDjVu(input, header, footer)
}.fold(mergeDjvu)
```

This should look much closer to what we got used to doing with `map`s and `fold`s.

## Catamorphism

Let’s say we have a function that combines `A`s together: `f: (A, A) => A` (which better be associative and even better commutative). Because in general we cannot guarantee, that there is even 1 element (and sometimes it is convenient for us to assume that there are always 2, so that we can always call `f` without any special cases), we might add some `zero: A`, which we could use if an element is missing (and we would call it `zero` as this should be something, that behaves like a neutral element of `f` operation). Then we arrive at the same `fold` we know from other data structures:

```scala
List(1, 2, 3, 4, 5).fold(0) { _ + _ } // 15
```

We could have to update our structure to reflect that:

```scala
sealed trait DocumentConfig[+A] {
  
  //  ... all the other methods ...
  
  def fold(zero: A)
          (f: (A, A) => A): A =
    this match {
      case CreatedDocument(a) => f(zero, a)
      case MergeDocuments(is) =>
        is.fold(zero) { (a, docA) =>
          docA.fold(create)(a)(f)
        }
    }
}
```

As we can see, we have a recursion scheme that collapses the whole `F[A]` into `A`. Sure, we need to provide `zero` and `f`, but once we compose them, we get:

```scala
val fold: F[A] => A = _.fold(zero)(f)
```

This `F[A] => A` is called a **catamorphism**. _Cata-_ comes from Greek _down_ same as in cataclysm or catastrophe (I think this exact same sentence appears in every damn introduction to recursive schemes). I think at this point we can skip convincing everyone that `fold`s are useful and present in our everyday life.

### F-algebras

Small digression. When we speak about an algebra, we mean some set `A` and some operator. An operator could be unary `A => A`, binary `(A, A) => A`, etc. Basically, some amount of `A` values as a tuple (or element of A^nAn Cartesian set) as input an. single `A` as output - in other words, a function is closed under `A`.

What if we also allowed `Seq[A]`? For instance, we could generalize binary `+` to combine any number of numbers. Or calculate a maximum/minimum. The same thing could be done for `Option[A]`. Actually, all operators could be expressed as `F[A] => A`. Even unary operator (such `F` that `F[A] = A` is usually called `Id`). If the `F` is also a functor (hardly ever isn’t), then we can call such algebra an **F-algebra**. Because FP programmers love category theory and category theory makes F-algebras generalization of _normal_ algebras, we don’t have to use _normal_ algebras anymore, use only F-algebras, and start calling _them_ algebras instead. So from now on algebra is a function `F[A] => A`. So catamorphisms are now an example of algebra.

```scala
// algebra's definition from Matryoshka library

type Algebra[F[_], A] = F[A] => A

// It allows us to treat Algebra as a functor
// and define things like natural transformation:

val x: Algebra[F, ?] ~> Id // basically:
                           // def x[A]: (F[A] => A) => A
                           // so it provides F[A]
                           // and applies it for any A
```

## Anamorphism

What is the opposite of `fold`? `Unfold`:

`A => F[A]`

Examples? Factorization of numbers (`Int => List[Int]`), split on `String` (`String => Array[String]`). If we loosen the requirement a bit, that there should be `F[A]` and allow there `F[B]` (because you might have run `map` with `f: A => B`), then every parser also becomes an example of an **anamorphism** - _ana-_ from Greek _up_ (an exact oppose of _cata-down_).

### Coalgebra

If `F[A] => A` is an algebra, then when we reverse the arrows, we should get… a coalgebra. Yes, anamorphism is an example of a coalgebra.

```scala
// coalgebra definition from Matryoshka library

type Coalgebra[F[_], A] = A => F[A]
```

And since we mentioned parsers…

## Hylomorphism

What would happen if we took anamorphism - `A => F[A]` - and catamorphism - `F[A] => A`? Well, on first sight we get some `A => A`, but slightly overcomplicated. Again, let us relax a bit: let our anamorphism be some `A => F[B]` and catamorphism some `F[B] => C`. Then, when we combine them, we’ll get a `A => C` with `F[B]` as intermediate representation.

This **hylomorphism** (_hylo-_ - _matter_) is what virtually every compiler is. In compiler a front-end - a parser generating AST - is anamorphism, while a back-end generating output from AST is catamorphism. Sure, compilers are not the only use case of hylomorphisms (basically every time you parse something and then generate something else from the parsing result you have hylo-), but you might understand why all FP compiler programmers (e.g. people writing Quasar Analytics and half the commercial Haskell programmers) would shove these definitions down your throat.

So, is that all there is to working with AST? Well, of course not! There are a lot of things we could do like

## Removing recurrence from definitions

Let’s get back to our AST. When we remove all the methods and only look at the data definition we get:

```scala
sealed trait DocumentConfig[+A]

final case class CreatedDocument[A](
  params: A
) extends DocumentConfig[A]

final case class MergedDocuments[A](
  inputs: NonEmptyList[DocumentConfig[A]] // recurrence here
) extends DocumentConfig[A]
```

As we can see, this definition explicitly uses recursion. So if we want to use a recursive scheme we have to write the traverse part manually, since it gets too complex for some derivation to kick-in.

Let’s think if we could remove it from there. We could, for instance, use a parameter instead of direct recursion call.

```scala
// Notice, that we have 2 parameters now:
// A - handles the content of our ADT/AST,
// S - is used to handle recursion.
// If our data structure was originally monomorphic
// then S would be enough.

sealed trait DocumentConfig[+A, +S]

final case class CreatedDocument[+A, +S](
  params: A
) extends DocumentConfig[A, Nothing]

final case class MergedDocuments[+A, +S](
  inputs: NonEmptyList[S] // recurrence disappeared
) extends DocumentConfig[A, S]
```

Okay, so how our document would look now?

```scala
type Args = (HTML, Option[HTML], Option[HTML])

val document: DocumentConfig[
  Nothing,
  DocumentConfig[Either[File, Args], Nothing]
] =
  MergedDocuments(NonEmptyList.of(
    CreatedDocument(Left(pdf)),
    CreatedDocument(Right(arguments)),
  ))
```

Hmm, it gets quite long. And, all of the types in `MergedDocuments` have to match, so we are losing some flexibility we had. If we want to push our small experiment forward. Ideally, no matter how many levels of nesting we would use, we should end up with the same type. If only there were a utility that does exactly that…

### Fixed point

Let’s do a small digression to remember a few things.

-   a type is a set. A function that takes, some types (sets) and returns some type (set) is called a type constructor. E.g. `List[_]` can take a `String` and return a `List[String]` which is a proper type,
    
-   in mathematics there is a concept of a fixed point. A fixed point of function ff is such an argument pp, that when passed into ff gives us pp
    
    f(p) = pf(p)=p

If we put that all together, we can think that:

-   if there was a type, that we could pass into a specific type constructor and get the same type we passed, this type would be a fixed point of that type constructor,
-   to implement such fixed point we could _forget_ arguments of `F[A]` and turn it into `F[_]`, then no matter how many nestings we would do, at the end we would still end up with `F[_]`.

Someone already came up with the solution to this problem, and it looks like this:

```scala
case class Fix[F[_]](unfix: F[Fix[F]])
```

Let’s use a slightly simpler example than `DocumentConfig` which at this point already have 2 parameters.

```scala
// example like this appear in virtually
// all recursive schemes courses, cliche :P
sealed trait Expression
final case class Value(value: Int)
    extends Expression
final case class Add(a1: Expression,
                     a2: Expression)
    extends Expression

val expression: Expression =
  Add(
    Add(Value(1), Value(2)),
    Add(Value(3), Value(4))
  )

// looks similar?
val evaluate: Expression => Int = {
  case Value(value) =>
    value
  case Add(a1, a2) =>
    calculate(a1) + calculate(a2)
}

evaluate(expression) // 10
```

Then, we would rewrite it to get rid of recursion in definition:

```scala
sealed trait Expression[+A]
final case class Value(value: Int)
    extends Expression[Nothing]
final case class Add[A](a1: A,
                        a2: A)
    extends Expression[A]
```

Well, we have restructured our data to get rid of recursion in the data definition. So where is the benefit? Well, to see it, we need to implement `map` (or `Functor` if you prefer):

```scala
// let's go with Functor with extension methods
// Cats, Scalaz - pick up your own poison

implicit val expressionFunctor: Functor[Expression] =
  new Functor[Expression] {
    
    // notice that we are actually mapping over
    // the parameter used to remove recursion
    def map[A, B](fa: Expression[A])
                 (f: A => B): Expression[B] =
      fa match {
                case Value(value) => Value(value)
        case Add(a1, a2)  => Add(f(a1), f(a2))
      }
  }
```

```scala
type ExpressionFix = Fix[Expression]

def value(v: Int): ExpressionFix =
  Fix[Expression](Value(v))
def add(a1: ExpressionFix,
        a2: ExpressionFix): ExpressionFix =
  Fix[Expression](Add(a1, a2))

val expressionFix: ExpressionFix =
  add(
    add(value(1), value(2)),
    add(value(3), value(4))
  )
```

Then, we will use that functor to handle mapping over our ADT and combine it with code that handles the folding of `F[A]` and adds handling of the recursion added with a fixed point:

```scala
def cata[F[_]: Functor, A](f: F[A] => A)(fix: Fix[F]): A =
  f(fix.unfix.map(cata[F, A](f)))
```

which we ca use like:

```scala
val evaluate: ExpressionFix => Int =
  cata[Expression, Int] {
    case Value(value) => value
    case Add(a1, a2)  => a1 + a2
  } _

evaluate(expressionFix)
```

As we see this recursive scheme used the fact, that we turned recursive definition into a functor, to handle recursive calls for us:

-   we are passing `ExpressionFix` which is actually `FixExpression`,
-   it contains inside `Expression[Fix[Expression]]` (or `Expression[ExpressionFix]` if you prefer), so we can `map` over it using the same `cata` we are currently in (that is `cata[F, A](f)`) to turn it into an `Expression[A]`,
-   since the data tree is finite, at some point we reach a leaf, that can be mapped without a recursive call - at this point we get out first `Expression[A]`,
-   from this moment on we will go back taking each `Expression[A]` and combining them into `A` using `Expression[A] => A` (a catamorphism) until we combine everything into a single `A`.

Quite a lot happens for such a short code!

Once this all start making sense for us (this is a good moment to take a break and play around in REPL until it sinks), we can get back to our `DocumentConfig` example.

What makes it more complex is the fact, that there was already a parameter `A` (which we would like to use to `map` the content), so now that we added another parameter to get rid of recursion, we actually made it a bifuntor. And for this `cata` implementation we must point it to the right parameter. Types also will get a bit messier, since we will use kind projectors to point which of the parameters should be mapped and which should stay constant.

Soo, that’s how the data definitions will look like:

```scala
// here we use kind-projector compiler plugin
// import $plugin.$ivy.`org.spire-math::kind-projector:0.9.4`

type DocumentConfigFix[A] = Fix[DocumentConfig[A, +?]]

def create[A](from: A): DocumentConfigFix[A] =
   Fix[DocumentConfig[A, ?]](CreatedDocument(from)) 

def merge[A](
  nel: NonEmptyList[DocumentConfigFix[A]]
): DocumentConfigFix[A] =
  Fix[DocumentConfig[A, ?]](MergedDocuments(nel))

val document: DocumentConfigFix[Either[File, Args]] =
  merge(NonEmptyList.of(
    create(Left(pdf): Either[File, Args]),
    create(Right(arguments): Either[File, Args])
  ))
```

now, the obligatory functor:

```scala
implicit def documentFunctor[A] =
  new Functor[DocumentConfig[A, ?]] {
  
    def map[S, T](fs: DocumentConfig[A, S])
                 (f: S => T): DocumentConfig[A, T] =
      fs match {
        case CreatedDocument(params) =>
           CreatedDocument(params)
        case MergedDocuments(inputs) =>
           MergedDocuments(inputs.map(f))
      }
  }
```

And finally, we can use `cata` for `fold`ing over `DocumentConfig`:

```scala
// for clarity I extracted these
val generateFile: Args => File
val mergeFiles: NonEmptyList[File] => File

val generate: DocumentConfigFix[Either[File, Args]] => File =
  cata[DocumentConfig[Either[File, Args], ?], File] {
    // if a document exists, extract it
    case CreatedDocument(Left(file)) =>
      file
    // if we have arguments, generate document
    case CreatedDocument(Right(args)) =>
      generateFile(args)
    // if we have documents, merge them
    case MergedDocuments(files) =>
      mergeFiles(files)
  } _

val generated: File = generate(document)
```

Let’s analyze how much our code changed:

-   initially, we encoded recursion both in a data structure as well as in virtually all functions that worked on it,
-   then, we split tree traversing from the actual logic - it allowed us to get more of the essence of our business logic and gave greater flexibility,
-   finally, we got rid of recursion in our data definition, we replaced it with a functor, fixed points and generic `cata` that takes a catamorphism and does the heavy lifting for us.

Of course, this solution is not without its issues. The types got a lot more complex, and now we have this `Functor` that maps over `S` (or whatever we name the parameter responsible for the recursion), which means that if your structure should also be mappable over `A`, we need to figure out how to handle it. Some parts of the application got a bit harder to explain to newcomers (though the business logic part got extracted and exposed, so it is easier to spot meaningful code as it is less entangled with the code for handling accidental complexity).

However, none of the approaches we saw so far handle our next use case - potentially infinite data structures.

## Infinite nesting

So far we worked with examples, where whole AST could be calculated at once. That is not always the case.

Imagine we used one of the examples from the previous post - the infix calculator - and we modified it so that once a user closes the line, you should treat the expression as final, evaluate it and display result. And that we should do this with every new line.

If we are reading a whole file at once, we can parse everything, then pass this computed value to some recursive scheme and fold it, map it or whatever we want to do with it. But what if we want to implement a REPL? What if we are receiving a stream of characters from some remote source and we don’t know its size? What if lines to evaluate might not arrive all at once, but we still need to be able to process whatever we already received and respond as soon as we are able to?

If we ask such a question, then we will start to understand, that finite recursive data is just one possibility. We must also be able to handle potentially infinite recursive data.

### Data on demand

Let’s create some simplified example:

```scala
// defining a whole Expr AST would
// unnecessarily complicate the example,
// so let just make it String
final case class Expr(unparsed: String)

sealed trait Program[+S]
final case class CurrentStep[+S](current: Expr, next: S)
  extends Program[S]
case object Finished
  extends Program[Nothing]
```

This ADT could be used to define something REPL-like: we take the `current: Expr`, run it, and then move on to the next step (so, we are basically traversing a list of `Expr`).

But, as we noticed earlier, with our current tools we would have to create a whole `Program` before we could start to traverse it. But, what if we created `next: S` lazily, as we need it? Scala already contains something very similar, just less generic: a ` Stream`.

### Corecursion

`Stream` is virtually a lazily evaluated list. Among many utilities it provide, there is an `iterate` function:

```scala
val stream = Stream.iterate(1) { i =>
  i + 1
}

stream.foreach { i =>
  println(i)
}
// 1
// 2
// 3
// ...
```

It takes some initial element, and a function that calculates its successor, and from that, it generates as many elements as we need.

This approach is called [**corecursion**](https://en.wikipedia.org/wiki/Corecursion). With recursion you usually start at the whole result and then break it down into smaller and smaller chunk until you get to the terminating condition:

-   when it comes to data you start at the top-level object and then go deeper and deeper inside into smaller objects defined in the same way as the top-level object,
-   with a function you calculate the value by composing partial solutions defined using the same function.

You can think of it at always starting at the root of some tree and proceeding calculations by going to the leaves. Reaching the result will always require going from the root to a leaf.

Corecursion is dual to that. You start with some seed and build up new elements from the previous ones. The subtle difference is that there is nothing, that requires corecursion to be a finite process. If you wanted to calculate `sum` of numbers in a `List`, you will have it once you go from its `head` to `Nil`. With `Stream` you might not be able to calculate such sum, as `Stream` can generate new elements endlessly - you can, however, calculate partial results or calculate next element basin on a previous one.

```scala
// simplified implementations

sealed trait List[+A]
case object Nil
  extends List[Nothing]
final case class Cons[+A](
    head: A, tail: List[A])
  extends List[A]

sealed trait Stream[+A]
case object NilStream
  extends Stream[Nothing]
final case class ConsStream[A](
    head: A, iterate: A => Stream[A])
  extends Stream[A] {
    
  lazy val tail = iterate(head)
}

// create some values

val list = Cons(1, Cons(2, Cons(3, Nil)))

val finiteStream: ConsStream[Int] = ConsStream(1, (i: Int) => {
  if (i >= 1000) NilStream
  else ConsStream(i * 2, finiteStream.iterate)
})

val infiniteStream: ConsStream[Int] = ConsStream(1, (i: Int) => {
  ConsStream(i * 2, infiniteStream.iterate)
})

// work on them

val sum: List[Int] => Int = {
  case Cons(head, tail) => head + sum(tail)
  case Nil              => 0
}
sum(list) // 6

def showFirstN(n: Int): Stream[Int] => Unit = {
  case cons: ConsStream[Int] if n > 0 =>
    println(cons.head)
    showFirstN(n-1)(cons.tail)
  case _ => ()
}

// stops before 16 iterations - stream as 11 elements
showFirstN(16)(finiteStream)
// 1
// 2
// 4
// 8
// 16
// 32
// 64
// 128
// 256
// 512
// 1024

// unlimitted new values - if not for the stop
// it would procude forever
showFirstN(16)(infiniteStream)
// 1
// 2
// 4
// 8
// 16
// 32
// 64
// 128
// 256
// 512
// 1024
// 2048
// 4096
// 8192
// 16384
// 32768
```

If what we get using recursion (induction) is called **data** (all values are already there), then what we obtain using corecursion ([coinduction](https://en.wikipedia.org/wiki/Coinduction)) is called **codata**.

So, could we define `Program` using codata?

### Nu - greatest fixed point

Looking at how we defined things in our simplified `Stream`, we need some `current: A` as well as `calculateNext: A => F[A]`. That means, that in order to obtain `next: F[A]` we would have to do something like:

```scala
lazy val next = calculateNext(current)
```

We could also encode it like this:

```scala
final class LazyFix[F[_], A](
  val a: A,
  val f: A => F[A]
  def unfix = f(a)
)
```

However, remembering how `Fixed` looked, we might expect that `A` will have to disappear from the list of parameters to allow things like `F[LazyFixedPoint[F]]`. We can use path-dependent types and refined types to achieve that (also `A => F[A]` is `Coalgebra[F, A]`, so let’s use that name):

```scala
sealed trait LazyFix[F[_]] {
  type A
  val a: A
  val f: Coalgebra[F, A]
  def unfix = f(a)
}
object LazyFix {
  
  def apply[F[_], A1](a1: A1,
                      f1: Coalgebra[F, A1]) =
    new LazyFix[F] {
      type A = A1
      val a = a1
      val f = f1
    }
}
```

Let’s try to use it with `Program` we defined before:

```scala
// we will need (again) a functor for mapping over
// the parameter we use for recursion
implicit val programFunctor = new Functor[Program] {
  
  def map[S, T](fa: Program[S])(f: S => T): Program[T] =
    fa match {
      case CurrentStep(current, next) =>
        CurrentStep(current, f(next))
      case Finished => Finished
    }
}

// Here we are defining how we want to
// create a new step out of a previous step
// - notice that Program is parametrized with
//   Expr: Expr => Program[Expr].
// BTW, this would be a good place to e.g.
// read a line from the input and put it into Expr.
// I haven't done that because in REPL where
// we can test this code it gets messy if both
// REPL and we try to read from STD In.
val nextStep: Coalgebra[Program, Expr] = {
    case expr @ Expr(str) => 
  import scala.util._
  // finish on empty input
  if (str.isEmpty) Finished
  else {
    // on non-empty, increment if number
    // set 0 otherwise
    val newStr = Try(str.toInt)
      .map(_ + 1)
      .getOrElse(0)
      .toString
    val newExpr = Expr(newStr)
    CurrentStep(expr, newExpr)
  }
}

// Since Program is basically a hardcoded lazy list
// of expressions, we start with some Expr and
// use nextStep to generate next expression.
def start(from: Expr): Program[LazyFix[Program]] =
  nextStep(from).map(LazyFix(_, nextStep))
  // This part around map is actuallt quite clever
  // nextStep(from) is of type Program[SomeA].
  // SomeA is the type hidden inside LazyFix, that
  // is used as argument of Program[SomeA] generator.
  //
  // We pass it to LazyFix together with nextStep
  // so both types match, we don't know them,
  // then can only be used to generate a sequence
  // of Program[_], where we will extract data from
  // Program[_] without having to pay attention to
  // what SomeA is.

// I know it's kind of cheating, and I should
// have implemented it using cata, but it
// was difficult because of reasons.
@scala.annotation.tailrec
def foldLeft[A](zero: A)
               (f: (A, Expr) => A)
               (program: Program[LazyFix[Program]]): A =
  program match {
    case CurrentStep(expr, fix) =>
      (foldLeft(f(zero, expr))
               (f)
               (fix.unfix.map(LazyFix(_, fix.f))))
                // same trick as in start
    case Finished =>
      zero
  }

// If you run it in REPL, you'll see
// that it works like a infinite loop
// and it is completelly stack-safe!
// (tail recursion)
foldLeft[Unit](()) { (_, expr) =>
  println(expr.str)
} (start(Expr("0")))
```

Nice! Our `LazyFix` seems to work. This should be a good moment to mention, that this structure is not actually called `LazyFix`.

When we have a function ff, it might have several fixed points xx such that x = f(x)x=f(x). For f(x) = x^3f(x)=x3 we have 3 such points: \{-1,0,1\}{−1,0,1}. So, we can point the greatest and the smallest (least) fixed point. Similar thing happens when our ff is a type constructor.

The [**greatest fixed point**](https://en.wikipedia.org/wiki/Greatest_fixpoint), that is used in type theory to define corecursive definitions/codata, is called \nuν. This Greek letter is read as `Nu`, which is why in `Matryoshka` library you can find:

```scala
sealed abstract class Nu[F[_]] {
  type A
  val a: A
  val unNu: Coalgebra[F, A]
}

object Nu {
  def apply[F[_], B](f: Coalgebra[F, B], b: B): Nu[F] =
    new Nu[F] {
      type A = B
      val a = b
      val unNu = f
    }
}
```

This is almost identical to the code of what we called `LazyFix` (because I ripped it off from this library).

You might wonder what else can we do with `Nu`, but I will get back to it in a moment after I mention the last fixed point.

## Getting back to recursion

### Mu - least fixed point

Just like we have the greatest fixed point, we also have the [**least fixed point**](https://en.wikipedia.org/wiki/Least_fixed_point). It is named \muμ which we read `Mu`. It is used to define recursive structures and data.

```scala
final case class Mu[F[_]](unMu: Algebra[F, ?] ~> Id)
//   val unMu: Algebra[F, ?] ~> Id
// is kind of like
//   def unMu[A]: (F[A] => A) => Id[A]
// and since Id[A] = A
//   def unMu[A]: (F[A] => A) => A
// so we can expect that the way it works
// is that for any type A it has some F[A] to
// pass into algebra to get a value A.
```

As we see, this fixed point moved applying recursion from `cata`:

```scala
def cata[F[_], A](f: F[A] => A)(fix: Mu[F]): A =
  fix.unMu(f)
```

Now, let’s try to implement `cata` for `DocumentConfig` from before.

```scala
type DocumentConfigMu[A] = Mu[DocumentConfig[A, ?]]

def create[A](from: A): DocumentConfigMu[A] = {
  type DocumentConfigA[S] = DocumentConfig[A, S]
  Mu[DocumentConfigA](new (Algebra[DocumentConfigA, ?] ~> Id) {
   //def apply[B](fb: DocumentConfigA[B] => B): B = {
     def apply[B](fb: Algebra[DocumentConfigA, B]): Id[B] = {
       val t: DocumentConfigA[Mu[DocumentConfigA]] =
         CreatedDocument(from)
       fb(t.map(cata(fb)))
     }
   })
}

def merge[A](
  nel: NonEmptyList[DocumentConfigMu[A]]
): DocumentConfigMu[A] =
{
  type DocumentConfigA[S] = DocumentConfig[A, S]
  Mu[DocumentConfigA](new (Algebra[DocumentConfigA, ?] ~> Id) {
   //def apply[B](fb: DocumentConfigA[B] => B): B = {
     def apply[B](fb: Algebra[DocumentConfigA, B]): Id[B] = {
       val t: DocumentConfigA[Mu[DocumentConfigA]] =
         MergedDocuments(nel)
       fb(t.map(cata(fb)))
     }
   })
}

val document: DocumentConfigMu[Either[File, Args]] =
  merge(NonEmptyList.of(
    create(Left(pdf): Either[File, Args]),
    create(Right(arguments): Either[File, Args])
  ))
```

The structures we used to store as nested values, now are created as values within a function, when we run `unMu`. Since the complexity was moved to the creation of `Mu` we might as well extract it into a separate function:

```scala
def embed[F[_]: Functor](t: F[Mu[F]]): Mu[F] =
  Mu[F](new (Algebra[F, ?] ~> Id) {
  //def apply[A](fa: F[A] => A): A
    def apply[A](fa: Algebra[F, A]): Id[A] =
      fa(t.map(cata(fa)))
  })
```

Then we could refactor `create` and `merge` into:

```scala
def create[A](from: A): DocumentConfigMu[A] =
  embed[DocumentConfig[A, ?]](CreatedDocument(from))

def merge[A](
  nel: NonEmptyList[DocumentConfigMu[A]]
): DocumentConfigMu[A] =
  embed[DocumentConfig[A, ?]](MergedDocuments(nel))
```

We can call this `cata` just like the one for `Fix`:

```scala
val generateFile: Args => File
val mergeFiles: NonEmptyList[File] => File

val fold: DocumentConfigMu[Either[File, Args]] => File =
  cata[DocumentConfig[Either[File, Args], ?], File] {
    // if a document exists, extract it
    case CreatedDocument(Left(file)) =>
      file
    // if we have arguments, generate document
    case CreatedDocument(Right(args)) =>
      generateFile(args)
    // if we have documents, merge them
    case MergedDocuments(files) =>
      mergeFiles(files)
  } _

val generated: File = generate(document)
```

You might wonder: _why should I need it if I have `Fix`?_ Good question! From [Valentin Kasas’ gist](https://github.com/vil1/recursion-schemes-cookbook/tree/master/Mu-Nu) we can learn, that in Scala `Fix` works differently to how it works in Haskell - in Scala, it is eager (thus closer to `Mu`), while in Haskell it is lazy (thus it behaves closer to `Nu`). This difference comes from the very difference in language design, but since it heavily affects the behavior it could be a source of confusion, which might be a reason why in the wild you will see people using either `Mu` or `Nu`. Still, `Fix` is taught, because it has the simplest definition of all fixed points, so it is the best for an introduction of the idea. Once we get it, we can stick to either `Mu` or `Nu` for clarity.

## Recursion and corecursion as type classes (Matryoshka)

If we get deeper into recursive schemes and install e.g. [Matryoshka](https://github.com/slamdata/matryoshka/), then we’ll quickly discover, that there are more recursive schemes than just cata-, ana- and hylomorphism. Actually, they are representatives of a much bigger group that could be sorted into **folds** (e.g. a catamorphism), **unfolds** (e.g. an anamorphism), **refolds**, that is unfolds followed by folds (e.g. hylomorphism) and **reunfolds**, that is folds followed by unfolds. They could appear in **generalized** version, where instead of simple `F[A] => A` or `A => F[A]` you’ll have `F[G[A]]`. They could use circuit breaking to terminate computation in **Elgot** and **CoElgot** algebras. There is quite a lot of other schemes, that you could just pick (and compose) basing on your current use case. How to manage the development of all of them, especially if we have like 3 distinct fixed points implementations?

The solution used in Matryoshka were type classes. For data operations (finite, recursive structures) there is a type class `Recursive`:

```scala
trait Based[T] { // T = Fix, Mu, or Nu
  type Base[A]   // data type, e.g. Expression
}                // or DocumentSchema[A, ?]
```

```scala
trait Recursive[T] extends Based[T] { self =>
  // ...

  def project(t: T)(implicit BF: Functor[Base]): BaseT[T]

  def cata[A](t: T)(f: Algebra[Base, A])
                   (implicit BF: Functor[Base]): A =
    hylo(t)(f, project)

  def cataM[M[_]: Monad, A](t: T)
                           (f: AlgebraM[M, Base, A])
                           (implicit BT: Traverse[Base]): M[A] =
    cata[M[A]](t)(_.sequence flatMap f)
  
  // ...
}
```

When we look into the `Recursive` type class we’ll see a lot of useful goodies for us to use. We can use them easily thanks to instances and syntaxes defined in the companion object:

```scala
object Recursive {
  // ...
  
  // Aux pattern - workaround for compiler limitation
  // when it comes to inferring path dependent types
  // (see: posts about implicits)
  type Aux[T, F[_]] = Recursive[T] { type Base[A] = F[A] }
  
  trait Ops[T, F[_]] { 
    def typeClassInstance: Aux[T, F]
    def self: T
    
    def project(implicit BF: Functor[F]): F[T] =
      typeClassInstance.project(self)
    def cata[A](f: Algebra[F, A])
               (implicit BF: Functor[F]): A =
      typeClassInstance.cata[A](self)(f)
    def cataM[M[_]: Monad, A](f: AlgebraM[M, F, A])
                             (implicit BT: Traverse[F]): M[A] =
      typeClassInstance.cataM[M, A](self)(f)
    // ...
  }
 
  // extended by e.g. matryoshka package object
  // to provide us with all the nice recursive schemes
  trait ToRecursiveOps {
    implicit def toRecursiveOps[T, F[_]]
        (target: T)
        (implicit tc: Aux[T, F]): Ops[T, F] =
      new Ops[T, F] {
        val self = target
        val typeClassInstance = tc
      }
  }
  
  // ...
}
```

So, we can use the recursive schemes and fixed points without defining everything ourselves:

```scala
// if you want to test it in REPL use a fresh one
// to not mix our definitions and matryoshka's ones

// import $ivy.`com.slamdata::matryoshka-core:0.21.3`

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._

sealed trait Expression[+A]
final case class Value(value: Int)
    extends Expression[Nothing]
final case class Add[A](a1: A,
                        a2: A)
    extends Expression[A]

object Expression {

  // Matryoshka works on Scalaz, so you
  // have to provide Scalaz Functor
  implicit val expressionFunctor: Functor[Expression] =
    new Functor[Expression] {

      // notice that we are actually mapping over
      // the parameter used to remove recursion
      def map[A, B](fa: Expression[A])
                   (f: A => B): Expression[B] =
        fa match {
          case Value(value) => Value(value)
          case Add(a1, a2)  => Add(f(a1), f(a2))
        }
    }
}

def value(v: Int): Mu[Expression] =
  Mu(Value[A](v))
def add(a1: Mu[Expression],
        a2: Mu[Expression]): Mu[Expression] =
  Mu(Add(a1, a2))

val evaluate: Algebra[Expression, Int] = {
  case Value(value) => value
  case Add(a1, a2)  => a1 + a2
}

add(
  add(value(1), value(2)),
  add(value(3), value(4))
).cata(evaluate)
```

Same with `Corecursive`:

```scala
trait Corecursive[T] extends Based[T] { self =>
  // ...

  def embed(t: Base[T])
           (implicit BF: Functor[Base]): T

  def ana[A](a: A)
            (f: Coalgebra[Base, A])
            (implicit BF: Functor[Base]): T =
    hylo(a)(embed, f)

  def anaM[M[_]: Monad, A]
    (a: A)
    (f: CoalgebraM[M, Base, A])
    (implicit BT: Traverse[Base]): M[T] =
    hyloM[M, Base, A, T](a)(embed(_).point[M], f)

  // ...
}
```

```scala
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._

final case class Expr(unparsed: String)

sealed trait Program[+S]
final case class CurrentStep[+S](current: Expr, next: S)
  extends Program[S]
case object Finished
  extends Program[Nothing]

object Program {
  
  implicit val programFunctor = new Functor[Program] {
  
    def map[S, T](fa: Program[S])(f: S => T): Program[T] =
      fa match {
        case CurrentStep(current, next) =>
          CurrentStep(current, f(next))
        case Finished => Finished
      }
    }
}

val nextStep: Coalgebra[Program, Expr] = {
    case expr @ Expr(str) => 
  import scala.util._
  // finish on empty input
  if (str.isEmpty) Finished
  else {
    // on non-empty, increment if number
    // set 0 otherwise
    val newStr = Try(str.toInt)
      .map(_ - 1)
      .getOrElse(0)
      .toString
    val newExpr = Expr(newStr)
    CurrentStep(expr, newExpr)
  }
}

// we are providing:
// * starting Expr
// * Coalgebra[Program, Expr]
// * Functor[Program] (implicitly)
// * Nu as a fixed point
val program: Nu[Program] =
  Expr("start").ana[Nu[Program]](nextStep)

// project "evaluates" Nu
program.project 
// CurrentStep(Expr("start"), matryoshka.data.Nu$$anon$2@14142d59)

// helper that will drop 1 result
// or return Finished if we are done
def drop1(prog: Nu[Program]): Nu[Program] =
  prog.project match {
    case CurrentStep(_, next) => next
    case Finished             => prog
  }

// "start" -> "0" -> "1" -> "2"
drop1(drop1(drop1(program))).project 
// CurrentStep(Expr("2"), matryoshka.data.Nu$$anon$2@3d8fbae3)

Expr("").ana[Nu[Program]](nextStep).project 
// Finished
```

If we dig into the library we might find, that its authors for convenience created another type class `Birecursive`, because quite a lot of these operations can be used for both `Recursive` and `Corecursive` data structures (though, [some of them are clearly not stack-safe](https://github.com/slamdata/matryoshka/issues/30)).

Another interesting finding is predefined data in a recursive scheme friendly form:

```scala
// in matryoshka.fixedpoint package object

// Natural numbers:
// Mu(None) = 0
// Mu(Some(Mu(None))) = 1
// Mu(Some(Mu(Some(Mu(None))))) = 2
// etc
type Nat = Mu[Option]

// potentially infinite number
type Conat = Nu[Option]


// a thing from matryoshka.patterns
// generalization of a List
sealed abstract class ListF[A, B]
final case class ConsF[A, B](car: A, cdr: B)
    extends ListF[A, B]
final case class NilF[A, B]()
    extends ListF[A, B]
// used in:

// eager, finite list
type List[A]   = Mu[ListF[A, ?]]
// lazy, potentially infinite list
type Colist[A] = Nu[ListF[A, ?]]


// this, is lazy always infinite list
type Stream[A] = Nu[(A, ?)]
// Since definition doesn't give us any
// opportunity to terminate, the computations
// can go on forever (but since we evaluate
// them lazily, one step at a time, it is not
// a problem).
```

Most of these are rather self-descriptive. If you add information, that there are some extension methods making you use e.g. `List` and `Stream` just like their implementations from Scala’s built-in library, then you might understand why some recursion heavy projects (read: compilers) might want to use them instead.

The exceptions that would require more description are `Free` and `Cofree`.

```scala
// these 2 are from matryoshka.patterns
// W[A] with some environment E (product)
final case class EnvT[E, W[_], A](run: (E, W[A]))
// F[A] or some environment E (coproduct)
final case class CoEnv[E, F[_], A](run: E \/ F[A])
// (but what are environments?).

// They are used in:

// Free algebra
type Free[F[_], A] = Mu[CoEnv[A, F, ?]]
// virtually: A \/ F[Free[F, A]]]
// which makes it a fixed point as well

// Cofree algebra
type Cofree[F[_], A] = Mu[EnvT[A, F, ?]]
// virtually: (A, F[Cofree[F, A]])
// which makes it another fixed point
```

As we see by definition _co-_ refers to _complimentary to free_ (product vs coproduct), as `Cofree` is **not** a codata.

### Free algebra

But first things first. What is a free algebra? We already talked [free monod](https://kubuszok.com/2018/algebras-we-love/#free-monoids) and [free monad](https://kubuszok.com/2018/different-ways-to-understand-a-monad/#free-monads). In both cases, we took some type `F[_]` and generated a new type out of it, which was guaranteed to be a monoid/monad. Free algebra is the generalization of that approach.

We start with some set KK of algebras of the same type (monads, monoids, etc.). We have two algebras of that type A, B \in KA,B∈K. Now, if you

-   define some subset S \subseteq AS⊆A called **generators**,
-   define some function f: S \rightarrow Bf:S→B (**embedding**),
-   and for any such ff you can unambiguously extend it to homomorphism h: A \rightarrow Bh:A→B

then you can call this AA a **free algebra**.

For instance, if you want to create a [free monoid](https://kubuszok.com/2018/algebras-we-love/#free-monoids) F_AFA​ out of some AA, you can say, that F_A = List_AFA​=ListA​ and neutral element of F_AFA​ is an empty list. The generators could be all single element lists of AA. Now, if we get a f: A \rightarrow Bf:A→B (which we can turn easily into a function from a single element list of AA into BB), we can translate an empty list of AA as a neutral element of BB, and take any other list of AA and turn it into a sequence of elements of monoid BB which we will combine according to rules of BB. As we see free monoid is (unsurprisingly) a free algebra.

Let’s see how it would work using Matryoshka’s `Free`:

```scala
// e.g. Free monoid defined using a list
type FreeMonoid[A] = Free[List, A]
// you can imagine that this is virtually equal to
type FreeMonoid[A] = A \/ List[FreeMonoid[A]]
// (pseudocode skipping Mu)
// If you'll try to imagine it, you will get
// something like (again, pseudocode skipping Mu):
List(
  List(A,
       List(A, A),
       List(
         List(A, A)
       )
      ),
  List(List(A, A))
)
// which you can read as: Lists are brackets,
// and everything inside list are operands
// where you put + inbetween during mapping
// to another monoid:
(
  (B +
       (B + B) +
       (
         (B + B)
       )
      ) +
  ((B + B))
)
// Actually, for all examples of Free
// assume that I use pseudocode - this Mu
// is important for type safety but
// it muddy the water when it comes to
// illustrating how AST looks.
```

Of course, that was mostly pseudocode, but it should explain the idea: the `Free` let us preserve some structure, that didn’t have any value in the context of `Free` itself, but which would matter once we mapped this `A` to something else. Here, in a **free monoid**, recursively nested lists of `A`s didn’t have value on their own, however they allowed us to translate `A => B` in such a way, that `B`s would be monoidally composed in the same order in which we composed `A`s in our free monoid.

What about [free monads](https://kubuszok.com/2018/different-ways-to-understand-a-monad/#free-monads)? Well, they are also something, that we use to record how we compose some `F[A]`s monadically, so that we could replay the same operations into some other monads. We already learned that they can be implemented with something like this:

```scala
sealed abstract class FreeMonad[S[_], A] {
  
  def flatMap[B](f: A => Free[S, B]): Free[S, B] =
    FlatMapped(this, f)
  
  def map[B](f: A => B): Free[S, B] =
    flatMap(a => FreeMonad.unit(f(a)))
}

object FreeMonad {
  
  def unit[S[_], A](a: A): FreeMonad[S, A] = Pure(a)
  def lift[S[_], A](sa: S[A]): FreeMonad[S, A] = Suspend(sa)
}

case class Pure[S[_], A](a: A) extends FreeMonad[S, A]
case class Suspend[S[_], A](sa: S[A]) extends FreeMonad[S, A]
case class FlatMapped[S[_], A, B](
    fsa: FreeMonad[S, A],
    f: A => FreeMonad[S, B]
) extends FreeMonad[S, B]
```

But we could also implement that using `Free` (everything below is pseudocode, I want to show the idea, not an obscure implementation):

```scala
type FreeMonad[F[_], A] =
  Free[F[A] \/ (FreeMonad[?, A], A => FreeMonad[?, B]), A]
// which we can unroll into:
type FreeMonad[F[_], A] =
  A \/ F[A] \/ (FreeMonad[F, A], A => FreeMonad[F, B])
// which correspond to:
// A  -> Pure[F, A]
// F[A] -> Suspend[F, A]
// (FreeMonad[F, A], A => FreeMonad[F, B]) -> FlatMapped
```

Here, our generator is `F[_]` and `f`s are natural transformations.

Since there are free monoid and free monads implementations that are easier to use than something, these are just examples, but in general, you can use `Free` if you want to use some `F` to branch your tree and store leaves in `A` and forget about that whole theoretical meaning. Examples:

-   `Free[Option, A]` would give you an arbitrary large nesting of `Option`s which would be terminated by either `None` or `A` (no idea who would need that, but…),
    
    ```scala
    Some(Some(Some(None)))
    Some(Some(Some(A)))
    None
    A
    ```
    
-   `Free[Lambda[B => (A, Option[B], Option[B])], A]` would be a binary tree,
    
    ```scala
    (
      A, // value
      Some( // left node
        (A, None, None) // is a leaf
      ),
      Some( // right node
        (   // is not a leaf
          A,
          Some(A), // another way of describing leafs
          None
        )
      )
    )
    ```
    
-   `Free[List, A]` would give you a tree, where nodes do not store values, but only aggregate subtrees.
    

I guess it would be more useful to compiler programmers because it allows you creating AST ad hoc without defining a separate data type.

### Cofree algebra

What `Cofree` does? It would still produce a tree, however, it would not have a way of terminating computations if `F[Result]` doesn’t provide it. On the other hand, it would provide an `A` for each node of the tree as metadata.

For instance, for `Cofree[Option, A]`, we would get a `List` (where a tail would be next `Some` of `Cofree` or None of `Cofree`), which head would always be present - so virtually a `NonEmptyList`.

```scala
(A -> Some(A -> Some(A -> None)))
```

`Cofree[Id, A]`, is similar to `Nu[(A, ?)]` - an infinite stream of data.

```scala
(A, (A, (A, (A, ...))))
```

> At this point difference in implementations of e.g. streams lies in what recursive scheme you can use in Matryoshka. And in fact, that `cata` doesn’t work on deeply nested (and surely never works for infinite) data, so `Cofree[Id, A]` is more like a trivia than something useful.

Additionally, since for each `Cofree[F, A]`:

-   you can always **extract** `A`,
-   you can **coflatMap** it with `F[A] => B` into `Cofree[F, B]` - you just assign `B` as metadata for a node created out of a whole subtree

`Cofree` is also a [comonad](https://kubuszok.com/2018/different-ways-to-understand-a-monad/#comonads) example.

Considering how flexible these data types are (depending on `F`, that we pass into them), we might wonder why they aren’t more popular. Well, among some compiler developers they are popular. But outside?

-   usually, you don’t need to be able to get from `NonEmptyList` to `Stream` by changing one type parameter,
-   using hardcoded `NonEmptyList`, `Stream`, etc is almost always faster - both when it comes to performance and maintenance,
-   while in some languages such approach would be justifiable (as it would be the only implementation), if there are some existing implementations that don’t rely on laziness which is alien to e.g. Scala, it might be easier to just use the existing, less _elegant_ implementations.

## Not only schemes

We talked _a lot_ about recursive schemes, but they are not everything when it comes to recursion. So, let’s talk about some other, but (closely) related things.

### Trampoline to stack-safety

If we would like to calculate `cata` for e.g. `F[S]` with 2000 nesting, it would be a valid input. But the way it works with `Fix` is that we have to reach the last element of the tree/leaf before we will be able to start moving back aggregating result on out way to root. What will happen if we will try to calculate `cata` for really deeply nested tree?

```scala
// Adds n nestings on on top of acc.
//
// acc stands for accumulator, as we call
// the things in tail recursive function calla
// that gather the results.
@scala.annotation.tailrec
def nDocs(
  n: Int,
  acc: DocumentConfigMu[String]
): DocumentConfigMu[String] =
  if (n <= 0) acc
  else nDocs(n - 1,
             merge(NonEmptyList.of(acc, create(n.toString))))

// 10000 merges, each combining only 2 files at a time
val bigDocument = nDocs(10000, create("start"))

// We call cata (our own) and...
cata[DocumentConfig[String, ?], String] {
  case CreatedDocument(value) => value
  case MergedDocuments(nel) => nel.mkString_(",")
} (bigDocument)
/* ...in my Ammonite REPL it throws:
java.lang.StackOverflowError
  cats.data.NonEmptyList.map(NonEmptyList.scala:76)
  ammonite.$sess.cmd3$$anon$1.map(cmd3.sc:12)
  ammonite.$sess.cmd3$$anon$1.map(cmd3.sc:2)
  cats.Functor$Ops.map(Functor.scala:12)
  cats.Functor$Ops.map$(Functor.scala:12)
  cats.Functor$ToFunctorOps$$anon$4.map(Functor.scala:12)
  ammonite.$sess.cmd14$$anon$1.apply(cmd14.sc:5)
  ammonite.$sess.cmd14$$anon$1.apply(cmd14.sc:2)
  ammonite.$sess.cmd6$.cata(cmd6.sc:2)
  ammonite.$sess.cmd14$$anon$1.$anonfun$apply$1(cmd14.sc:5)
  cats.data.NonEmptyList.map(NonEmptyList.scala:76)
  ammonite.$sess.cmd3$$anon$1.map(cmd3.sc:12)
  ammonite.$sess.cmd3$$anon$1.map(cmd3.sc:2)
  cats.Functor$Ops.map(Functor.scala:12)
  cats.Functor$Ops.map$(Functor.scala:12)
  cats.Functor$ToFunctorOps$$anon$4.map(Functor.scala:12)
  ammonite.$sess.cmd14$$anon$1.apply(cmd14.sc:5)
  ammonite.$sess.cmd14$$anon$1.apply(cmd14.sc:2)
  ammonite.$sess.cmd6$.cata(cmd6.sc:2)
  ammonite.$sess.cmd14$$anon$1.$anonfun$apply$1(cmd14.sc:5)
  cats.data.NonEmptyList.map(NonEmptyList.scala:76)
  ...
*/
```

Well, it seems that it breaks.

This is a part of a much bigger problem that is the stack-safety of recursive functions.

When we call a function on JVM (or any other von Neumann architecture), what computer does is put all variables in local scope as well as the current position in a program on the stack - there is one defined for each thread your program is working on. Once the function terminates it uses the stack to know where to go back and recreate the environment from before the call (as your computer can remember only the limited amount of variables at a time).

If we have a recursive function, this function will put things on the stack until it gets to the point where it can finally start going back. If the nesting it large enough the size of things we need to store on the stack will exceed its capacity and JVM will throw a `StackOverflow` exception:

```scala
def factorial(n: Int): BigInt =
  if (n <= 0) 1
  else n * factorial(n-1)

factorial(10000) // java.lang.StackOverflowError
```

Scala (and several other languages, but not Java) is able to perform something **tail recursion optimization**. It means, that if a function doesn’t have to perform any additional calculations when it returns, the compiler can underneath replace recursive calls with a `while` loop because there is no point in storing all that data on the stack when it will never be used. To ensure that the function we are writing is optimized in such a way (and never throws `StackOverflow`) we can use the annotation:

```scala
@scala.annotation.tailrec
def factorial(n: Int, acc: BigInt = 1): BigInt =
  if (n <= 0) acc
  else factorial(n-1, acc * n)

factorial(10000) // reeeeeealy big number
```

Thing is, not every language support tail call optimization. For dealing with such situation a technique was invented called a **trampoline**. It works more or less this way:

-   let’s say your function does one step at a time,
-   instead of calling itself directly in a tail-recursive manner (that is - you would not do anything with a returned value other than return it yourself), it returns arguments for the next call,
-   you return enough information from the function to determine if this is the final result, or if you should call it again and with what arguments.

You can imagine that you _jump_ into a function and it _bounces_ you out and then you bounce until you get to the result (and preventing the stack from spilling), which is why you call it a trampoline.

For instance, you could implement it like this:

```scala
def trampoline[A, B](f: A => Either[A, B])
                    (a: A): B = {
  var result: Either[A, B] = Left(a)
  while (true) {
    result match {
      case Left(a)  => result = f(a)
      case Right(b) => return b
    }
  }
}

def factorial(n: Int) = trampoline[(Int, BigInt), BigInt] {
  case (a, b) =>
    if (a <= 0) Right(b)
    else Left((a-1) -> (b * a))
} (n -> 1)

factorial(10000)
```

Another way of implementing it with the usage of so-called **continuation passing style**. In CPS we pass a function we would call to receive the next step of the computation. If we combine CPS with a trampoline we get a solution where we are evaluating one step at a time and where the next step is defined using a function. We can define that easily using objects (notice that normally CPF doesn’t require them and looks different to what we see below):

```scala
sealed trait Trampoline[A]
object Trampoline {
  final case class Return[A](
    a: A
  ) extends Trampoline[A]
  final case class Suspend[A](
    thunk: () => Trampoline[A]
  ) extends Trampoline[A]
  
  def done[A](a: A): Trampoline[A] =
    Return(a)
  def defer[A](thunk: => Trampoline[A]): Trampoline[A] =
    Suspend(() => thunk)
}

def run[A](t: Trampoline[A]): A = {
  var result: Trampoline[A] = t
  result match {
    case Trampoline.Suspend(thunk) =>
      result = run(thunk())
    case Trampoline.Return(a) =>
      return a
  }
}

def factorial(n: Int, acc: BigInt = 1): Trampoline[BigInt] =
  Trampoline.defer[BigInt] {
    if (n <= 0) Trampoline.done(acc)
    else factorial(n - 1, acc * n)
  }

run(factorial(10000))
```

Such definition of a trampoline is actually a monad, so we can define a `Monad` instance for it and use it (and evaluate it stack-safely):

```scala
implicit val trampolineMonad = new Monad[Trampoline] {
  def pure[A](a: A): Trampoline[A] = Trampoline.done(a)
  def flatMap[A, B](fa: Trampoline[A])
                   (f: A => Trampoline[B]): Trampoline[B] =
    Trampoline.defer[B] {
      f(run(fa))
    }
  def tailRecM[A, B](a: A)
                    (f: A => Trampoline[Either[A, B]])
      : Trampoline[B] =
    flatMap(Trampoline.defer(f(a))) {
      case Left(a2) => tailRecM[A, B](a2)(f)
      case Right(b) => Trampoline.done(b)
    }
}

val factorialTailRec
  : ((Int, BigInt)) => Trampoline[Either[(Int, BigInt),
                                         BigInt]] = {
  case (n, acc) =>
    if (n <= 0) Trampoline.done(
      Right(acc): Either[(Int, BigInt), BigInt]
    )
    else Trampoline.done(
      Left((n - 1) -> (acc * n)): Either[(Int, BigInt), BigInt]
    )
  }

run(trampolineMonad.tailRecM(10000 -> (1: BigInt))    
                            (factorialTailRec))
```

Such trampoline technique is used in `Free` monads implementation when you are evaluating `Free` into you algebra of choice using `foldMap`. It is also used internally in Cats Effects, Monix or ZIO to run calculation - this way you can write a recursive IO definition and when you run it, it won’t break your stack.

You might probably ask: do libraries like Matryoshka use trampolines (or similar technique) to ensure stack safety? [At the moment](https://github.com/slamdata/matryoshka/blob/2233e287fab4ab8cd509663f2f384822af2ff32c/core/shared/src/main/scala/matryoshka/instances/fixedpoint/package.scala#L319), unfortunately, [no](https://github.com/slamdata/matryoshka/issues/30). While it uses `scalaz.Free.Trampoline` in a few places, in general, it is not stack-safe. Though if the community will demand it maintainers might consider implementing it.

### Generalized Algebraic Data Types

Let’s say we want to check if a seat is available in a cinema for a particular movie. We must consider the case when this is a single ticket, but also a case if someone buys a group ticket for several seats. We could model is with something like this:

```scala
sealed trait Ticket
final case class SingleTicket(
  movie: String,
  seat:  String
) extends Ticket
final case class GroupTicket(
  movie: String,
  seats: NonEmptyList[String]
) extends Ticket
```

Then we could check the seats in a database running a slightly different query for a single ticket and for a group ticket:

```
val checkSeats: Ticket => Query[Boolean] = {
  case SingleTicket(owner) => // ...
  case GroupTicket(owners) => // ...
}
```

The query is run in a transaction, the result should have no issues with race conditions, we are happy.

But then we see that both cases are awfully similar, and we could probably benefit a bit from making the code more generic. For instance, instead of `String` or `List[String]` we might notice that these are just 2 special cases of some `F[String]`: `F = Id` for a single ticker and `F = NonEmptyList` for a group ticket:

```scala
// SingleTicket = Ticket[Id]
// GroupTicket  = Ticket[List]
final case class Ticket[F[_]](
  movie: String,
  seats: F[String]
)
```

We might modify query a bit, to run against one-seat-one-movie check, but we still run it in a transaction, so things should be fine:

```scala
val checkSeat: (String, String) => Query[Boolean]

// requires implicit Applicative[F]
ticket.seats.traverse { seat =>
  checkSeat(ticket.movie, seat)
} // Query[F[Boolean]]


```

We can use more other generic methods as well, but this should show us that there is some benefit in making things more generic and using type parameters to describe certain parts of our domain. However, it creates some issue: now we allowed also things like: `Ticket[Option]` (no person ticket doesn’t make sense!), `Ticket[Future]` (we wanted it to represent a value, not an ongoing computation) or `Ticket[Either[String, ?]]` (should validation be a part of the ticket?).

Finally, we arrive at the conclusion, that we would like to have both:

-   type parameters giving us access to all benefits coming with them,
-   some way of constraining the possible representation to only these which makes sense for our domain.

As a matter of fact, we can do it. We can **define type parameters in our ADT’s signature** and then **hardcode them in the specific cases**.

```scala
sealed trait Ticker[F[_]] {
  val movie: String
  val seats: F[String]
}
final case class SingleTicket(
  movie: String,
  seats: String
) extends Ticket[String]
final case class GroupTicket(
  movie: String,
  seats: NonEmptyList[String]
) extends Ticket[List[String]]
```

```scala
// generic operations work
ticket.seats.traverse { seat =>
  checkSeat(ticket.movie, seat)
} // Query[F[Boolean]]

// pattern matching knows exact types
def checkSeats[F[_]: Applicative]
  : Ticket[F] => Query[Boolean] = {
  case SingleTicket(movie, seat) =>
    // seat is known to be Id[String]
    checkSeat(movie, seat)
  case GroupTicket(movie, seats) =>
    // seats known to be NEL[String]
    seats.traverse(checkSeat(movie, _))
}

// Impossible to instantiate:
// - Ticket[Option]
// - Ticket[Future]
// - Ticket[Either[String, ?]]
// - ...
```

Such a parametric ADT with some of its parameters hardcoded in some of its type constructors/cases is called **generalized abstract data type**.

Just like any other ADT, GADT can be used for defining recursive structures. Our first example, `DocumentConfig`, we rewrote from:

```scala
sealed trait DocumentConfig[+A]

final case class ExistingDocument[+A](
  file: A 
) extends DocumentConfig[+A]

final case class CreatedDocument(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML]
) extends DocumentConfig[Nothing]

final case class MergedDocuments[+A](
  inputs: NonEmptyList[DocumentConfig[A]]
) extends DocumentConfig[A]
```

into:

```scala
sealed trait DocumentConfig[+A]

// to support both existing docs and created
// we'll just set:
//   A = Either[Sth,(HTML, Opt[HTML], Opt[HTML])]
final case class CreatedDocument[+A](
  params: A
) extends DocumentConfig[A]

final case class MergedDocuments[+A](
  inputs: NonEmptyList[DocumentConfig[A]]
) extends DocumentConfig[A]
```

And then we turned recursive `DocumentConfig` into a functor definition with a fixed point.

However, we could have tried another way. If how we generate PDF or DjVu was a part of PDF definition we could go with GADT:

```scala
sealed trait DocumentConfig[+A]

final case class ExistingDocument[+A](
  file: A 
) extends DocumentConfig[+A]

final case class CreatedPDF(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML],
  generate: (HTML, Option[HTML], Option[HTML]) => PDF
) extends DocumentConfig[PDF]

final case class CreatedDjVu(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML],
  generate: (HTML, Option[HTML], Option[HTML]) => DjVu
) extends DocumentConfig[DjVu]

final case class MergedDocuments[+A](
  inputs: NonEmptyList[DocumentConfig[A]]
) extends DocumentConfig[A]
```

This way we would have access to all relevant information in pattern matching, while we would still be able to `map` over all values (if we needed to map `PDF` we could then create it). The resulting GADT can also be rewritten to use recursive schemes:

```scala
sealed trait DocumentConfig[+A, S]

final case class ExistingDocument[+A, S](
  file: A 
) extends DocumentConfig[+A, S]

final case class CreatedPDF(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML],
  generate: (HTML, Option[HTML], Option[HTML]) => PDF
) extends DocumentConfig[PDF, S]

final case class CreatedDjVu(
  input: HTML,
  header: Option[HTML],
  footer: Option[HTML],
  generate: (HTML, Option[HTML], Option[HTML]) => DjVu
) extends DocumentConfig[DjVu, S]

final case class MergedDocuments[+A](
  inputs: NonEmptyList[S]
) extends DocumentConfig[A, S]

type DocumentConfigMu[A] = Mu[DocumentConfig[A, ?]]
```

The result is something that I would expect to find in some more advanced projects working heavily with recursive data, like e.g. compilers.

## Summary

In this article, we scratched the surface of several functional solutions to some problems regarding recursive data, operations on them as well as ADT in general.

I intended to show (and hopefully did), that:

-   recursive schemes are actually everywhere - every day we make use of the fact that we can split operation on data type to the part which traverses the tree and the part which performs an action in each node,
-   by replacing recursion in your ADT by a functor with a fixed-point applied to type parameters, you can get plenty of recursive schemes almost for free,
-   thanks to several different fixed-points you can easily define if your data should be recursive or corecursive just by changing the fixed-point,
-   even if you cannot use tail recursion for some reason and you need to get into a deep, deep data structure, there are tools like a trampoline, which might help you achieve your goal.

While knowledge about fixed-points might not be as relevant in your everyday CRUD application, everyone can benefit from awareness, that quite a lot of your data can provide some way of traversing, mapping and folding itself, which could lead to code that is more readable and easier to maintain.

Finally, I want to say that, sure a lot of this looks difficult, but it only looks like this until you start to play around with REPL a bit and see, that it works, and it’s the lack of familiarity that makes look alien and overcomplicated. (That, and the lack of newbie-friendly documentation). Once you give it a shot, recursive schemes appear to be a lot friendlier than they look like.