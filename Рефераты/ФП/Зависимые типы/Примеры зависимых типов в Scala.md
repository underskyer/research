

[Tagless final for humans](https://noelwelsh.com/talks/tagless-final-for-humans/) (доклад Ноэля Уелша)
[Functional Programming Strategies In Scala with Cats, $14.4](https://scalawithcats.com/dist/scala-with-cats.pdf#page=390)


## Кортежи как разнородные списки

[Typelevel fix point (HFix)](https://jto.github.io/articles/typelevel-fix/)
- shapeless
- refined


## Литеральные типы

[Scala 3 Metaprogramming: реализация списка с известным на этапе компиляции размером](https://habr.com/ru/articles/776460/)

## Capabilies Capturing
типы с захваченными возможностями - это зависимые типы!!
[What's in the Box. Ergonomic and Expressive Capture Tracking over Generic Data Structures](https://ar5iv.labs.arxiv.org/html/2509.07609)
- [Introduction to Scala 3's Capture Checking and Separation Checking](https://tanishiking.github.io/posts/introduction-to-scala-3s-capture-checking-and-separation-checking/) статья Рикито Танигучи.
- [Hands on Capture Checking](https://nrinaudo.github.io/articles/capture_checking.html) Николя Ринаудо
- [Capturing Types](https://dl.acm.org/doi/10.1145/3618003?__cf_chl_tk=EVog0Zfgh8RVS2idCoiY.HbIU014NCF2FTQA_XMQi8Y-1785146520-1.0.1.1-uD53hFjBsDo9RkIi8CHcRa9dumgpet7SQfPyt6tf2OA) мощная статья от авторов Scala

## Match Types
[Match Types in Scala 3 (Baeldung)](https://www.baeldung.com/scala/match-types)
[сопоставления с шаблонами](https://docs.scala-lang.org/scala3/reference/new-types/match-types.html) 
[Что не понравилось в Match Types](https://chugunkov.dev/2021/06/29/match-types-problems.html)
[Scala 3: dealing with path dependent types](https://stackoverflow.com/questions/73832836/scala-3-dealing-with-path-dependent-types)  (StackOverflow)

## Функтор
Функтор из категории типов в категорию категорий
```scala
type Hom[A, F[_]]
type Hom[F[_], A]
```
переводит конструкторы типов в функторы, уважая композицию

## Path-Dependent Types

[Understanding Scala's Path-Dependent Types](https://reintech.io/blog/understanding-scalas-path-dependent-types)
[оригинал](https://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html)
[Ограничения зависимых от путей типов](https://stackoverflow.com/questions/73832836/scala-3-dealing-with-path-dependent-types)

## AnyKind

https://cytrowski.me/posts/anykind-scala-3-type-shapes/


