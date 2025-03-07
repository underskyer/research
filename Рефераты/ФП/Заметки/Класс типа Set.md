Любой ли обобщённый тип можно интерпретировать, как описание класса типов? Рассмотрим такой фрагмент кода:
```scala
def isValid[A: Set] // Тип A должен принадлежать некому классу Set
  : A => Boolean    // Аннотация типа не обязательна
  = summon[Set[A]].contains

given validInts // Помещаем значение класса Set типа Int в контекст
  : Set[Int]
  = Set(2, 3, 5, 7, 11)
isValid(42)     // Класс Set для типа Int неявно найден в контексте
//isValid("42") // Ошибка! В контексте не найден класс Set для типа String!
```
Безотносительно того, что размещать в контексте неявное значение "слаботипизированного" типа `Set[Int]` достаточно безответственно, но, в целом, очевидно, что ответ на поставленный выше вопрос утвердительный.
