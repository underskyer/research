С позиции расширений Кана, вложение Йонеды — это способ превратить любой тип данных в «обработчик самого себя». В Scala это выражается через полиморфную функцию (CPS).

## Реализация вложения Йонеды (Yoneda Embedding)
Вложение $Y:\mathcal{C}\rightarrow [\mathcal{C}^{op},\mathbf{Set}]$ сопоставляет объекту $A$ функтор $\text{Hom}(A,-)$. В Scala для произвольного функтора $F$ вложение Йонеды выглядит так: scala// Это и есть «пространство Йонеды» для функтора F
```scala
// Согласно лемме, Yoneda[F, A] изоморфно F[A]
trait Yoneda[F[_], A] {
  // Натуральное преобразование: для любого типа B и функции A => B, возвращает F[B]
  def apply[B](f: A => B): F[B]
}

object Yoneda {
  // Вложение (lift): из обычного функтора в пространство Йонеды
  // Это расширение Кана тождественного функтора вдоль самого себя (Lan Id Id)
  def embed[F[_]: cats.Functor, A](fa: F[A]): Yoneda[F, A] = 
    new Yoneda[F, A] {
      def apply[B](f: A => B): F[B] = cats.Functor[F].map(fa)(f)
    }

  // Обратное преобразование (lower): из пространства Йонеды в F[A]
  // Просто применяем тождественную функцию
  def run[F[_], A](y: Yoneda[F, A]): F[A] = y.apply(identity)
}
```

## Пример: Оптимизация через расширение
Польза здесь в том, что в Yoneda метод map — это просто композиция функций в памяти, а не обход контейнера. Это левое расширение Кана, которое «откладывает» вычисления. scala// Допустим, у нас есть тяжелый список
```scala
case class HeavyList[A](items: List[A])

// Реализуем map для Yoneda без использования Functor исходного типа!
// Это демонстрирует, что Yoneda[F, A] — это «лучшая аппроксимация» F
implicit def yonedaFunctor[F[_]]: cats.Functor[Yoneda[F, *]] = 
  new cats.Functor[Yoneda[F, *]] {
    def map[A, B](y: Yoneda[F, A])(f: A => B): Yoneda[F, B] =
      new Yoneda[F, B] {
        // Композиция функций: g(f(x))
        def apply[C](g: B => C): F[C] = y.apply(g compose f)
      }
  }

// Использование:
val heavy = HeavyList(List(1, 2, 3))
val embedded = Yoneda.embed(heavy)(new cats.Functor[HeavyList] {
  def map[A, B](fa: HeavyList[A])(f: A => B): HeavyList[B] = {
    println("Реальный проход по данным!")
    HeavyList(fa.items.map(f))
  }
})

// Эти операции НЕ вызывают "Реальный проход по данным"
val transformed = cats.Functor[Yoneda[HeavyList, *]].map(embedded)(_ + 1)
val transformed2 = cats.Functor[Yoneda[HeavyList, *]].map(transformed)(_ * 10)

// Только здесь выполнится один единственный map
val finalResult = Yoneda.run(transformed2) 
// Выведет: "Реальный проход по данным!"
// HeavyList(List(20, 30, 40))
```

## Расширение Кана вдоль Йонеды (Интерпретатор)
Теперь самое интересное: как расширить функтор с «маленьких» объектов на «все» вложение Йонеды. Это концепция Coyoneda (дуальное вложение), которая используется для создания интерпретаторов без наличия Functor у исходного типа. scala// Левое расширение Кана Lan_Y F. Позволяет сделать любой тип данных функтором.
```scala
trait Coyoneda[F[_], A] {
  type Pivot
  val fi: F[Pivot]
  val k: Pivot => A
}

object Coyoneda {
  // Расширяем любой тип F до функтора (это и есть Lan_Y F)
  def lift[F[_], A](fa: F[A]): Coyoneda[F, A] = 
    new Coyoneda[F, A] {
      type Pivot = A
      val fi = fa
      val k = identity
    }

  // Расширение Кана (Lan) дает нам возможность трансформировать 
  // структуру, даже если F — не функтор
  def map[F[_], A, B](c: Coyoneda[F, A])(f: A => B): Coyoneda[F, B] =
    new Coyoneda[F, B] {
      type Pivot = c.Pivot
      val fi = c.fi
      val k = f compose c.k
    }

  // И наконец: расширение интерпретатора (натурального преобразования)
  // Это реализация универсального свойства расширения Кана
  def run[F[_], G[_]: cats.Functor, A](c: Coyoneda[F, A])(f: F ~> G): G[A] =
    cats.Functor[G].map(f(c.fi))(c.k)
}

// ПРИМЕР: Интерпретация DSL, который сам по себе НЕ является функтором
sealed trait Action[A]
case class Fetch(id: Int) extends Action[String]

import cats.~>
import cats.Id

val myAction: Coyoneda[Action, Int] = 
  Coyoneda.map(Coyoneda.lift(Fetch(1)))(_.length) // Расширили Action до функтора!

val interpreter = new (Action ~> Id) {
  def apply[A](fa: Action[A]): Id[A] = fa match {
    case Fetch(id) => s"User$id"
  }
}

// Расширение Кана автоматически "пробрасывает" интерпретатор через все map-ы
val result: Id[Int] = Coyoneda.run(myAction)(interpreter) // вернет 5 ("User1".length)
```

## Почему это полезно в 2026 году?
- Zero-cost Abstractions: Вы определяете минимальную логику (Action), а расширение Кана (Coyoneda) автоматически превращает её в богатый интерфейс с map, flatMap и т.д., не требуя от вас реализации этих методов для каждого типа.
- Де decoupling: Вы можете менять структуру данных, не меняя логику обработки, потому что расширение Кана гарантирует, что "путь" от вложенного объекта к результату всегда существует и единственен.
- Оптимизация компилятора: Такие структуры позволяют JVM/GraalVM лучше оптимизировать цепочки вызовов, превращая их в плоские циклы. 


- 