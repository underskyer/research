
# Мотивация
```scala
trait Derive[F[_], A]:  
  def value: F[A]  
  
object Derive extends LowPriorityDerive:  
  inline def apply[F[_], A](using d: Derive[F, A]): F[A] = d.value  
  
  given pure[F[_] : Monad, A](using a: A): Derive[F, A] = new Derive[F, A]:  
    def value: F[A] = summon[Monad[F]].pure(a)  
  
trait LowPriorityDerive:  
  // Правило: сначала функция фиксирует тип A, потом ищем Derive[F, A]  
  given bind[F[_] : Monad, A, B](using f: A => F[B], da: Derive[F, A]): Derive[F, B] = new Derive[F, B]:  
    def value: F[B] = summon[Monad[F]].flatMap(da.value)(f)
```
Критически важен порядок неявных параметров! Сначала ищется `A => F[B]` и при этом фиксируется `A`, и только потом ищется `Derive[F, A]`!


# Теория
Изоморфизм Карри-Ховарда
# Задача о ферзях


# Логические эффекты


# Контекстные паттерны в Scala

`NoGiven[A =:= B]`

# Сложности и перспективы


# Дополнительная литература



