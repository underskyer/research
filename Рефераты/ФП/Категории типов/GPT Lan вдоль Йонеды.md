С позиции расширений Кана, вложение Йонеды — это способ превратить любой тип данных в «обработчик самого себя». В Scala это выражается через полиморфную функцию (CPS).

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
