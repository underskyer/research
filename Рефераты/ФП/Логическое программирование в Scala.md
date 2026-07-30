
# Мотивация

Scala, как и многие современные языки программирования, позволяют закодировать одну и ту же логику в разных стилях. Давайте посмотрим как это может выглядеть на примере простой задачи:
- загрузить текст по ссылке;
- найти в тексте лимитированное количество чисел;
- вывести их в консоль;
- если распечатали хотя бы одно число, то вернуть ExitCode 0.

Конечно же, программа у нас будет написана по лучшим стандартам с поддержкой асинхронности и чистых ленивых вычислений. В качестве системы эффектов возьмём [CE](https://typelevel.org/cats-effect/), так как она предлагает наиболее фундаментальные абстракции.

Сперва сформулируем бизнес-функции программы:
```scala
import cats.effect.{ExitCode, IO}

type ArgsParser        = List[String]        => IO[ScanUrl]      // получаем url из списка аргументов
type UrlLoader         = ScanUrl             => IO[Content]      // загружаем текст
type LimitGetter       = Any                 => NumLimit         // получаем лимит
type ContentParser     = ParsingContent      => IO[List[Num]]    // извлекаем числа
type NumberPrinter     = Num                 => IO[NumberPrinted]// выводим в консоль одно число
type NumbersAggregator = List[NumberPrinted] => ExitCode         // определяем код выхода
```
Это верхнеуровневая бизнес-логика и для простоты мы не будем опускаться на уровень ООП-шных сервисов.

Наши функции переводят программы из одного состояния в следующее, из них только начальное и конечное жёстко фиксированы сигнатурой метода `IOApp.run: List[String] => ExitCode`. Прочие состояния опишем как классы-обёртки над примитивными типами:
```scala
case class ScanUrl(str: String)                           // Адрес сканируемого сайта
case class Content(str: String)                           // Контент сайта
case class NumLimit(n: Int)                               // Ограничение на количество чисел
type ParsingContent = (content: Content, limit: NumLimit) // Контент с ограниченим для разбора
case class Num(n: Int)                                    // Искомые числа
case class NumberPrinted()                                // Признак выполненой задачи печати
```
Для примера нам достаточно только типобезопасности, поэтому обойдёмся простыми `case class` вместо более строгих решений вроде [непрозрачных псевдонимов](https://docs.scala-lang.org/scala3/book/types-opaque-types.html) или [уточённых типов](https://iltotore.github.io/iron/docs/overview.html).

Теперь нужно предоставить реализации бизнес-функций. Для сетевого запроса воспользуемся «янтарным» клиентом из [http4s](https://http4s.org/), для которого подключим журналирование из [log4cats](https://typelevel.org/log4cats/), а лимит чисел просто захардкодим:
```scala
import cats.effect.{ExitCode, IO}  
import logic.models.*  
import org.http4s.ember.client.EmberClientBuilder  
import org.typelevel.log4cats.LoggerFactory  
import org.typelevel.log4cats.slf4j.Slf4jFactory

given loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]  
  
given argsParser: ArgsParser = args =>  
  IO  
    .fromOption(args.headOption)(new Throwable("Передайте Url!"))  
    .map(ScanUrl.apply)  
  
given urlLoader: UrlLoader = url =>  
  EmberClientBuilder  
    .default[IO].build  
    .use(_.expect[String](url.str))  
    .map(Content.apply)  
  
given contentParser: ContentParser =  
  case (content, limit) => IO:  
    "\\d+".r  
      .findAllIn(content.str)  // ищеем подстроки из цифр
      .flatMap(_.toIntOption)  
      .take(limit.n)           // ограничиваем количество
      .map(Num.apply)  
      .toList  
  
given limitGetter: LimitGetter = _ => NumLimit(10) // или можно прочесть из конфигурации  
  
given numberPrinter: NumberPrinter = num =>  
  IO  
    .println(num.n)  
    .as(NumberPrinted())  
  
given numbersAggregator: NumbersAggregator =  
  case _ :: _ => ExitCode.Success  // если распечатали хоть что-то, то 0
  case Nil    => ExitCode.Error    // если ничего не нашлось, то 1
```
В глаза сразу бросается «толстый намёк» в виде `given`, но пока можете не обращать внимания. По сути, это обычные переменные `val` только с опцией размещения значений в контекст области видимости. Шаги и их реализации нарочно выбраны «разношёрстными»: есть функции с эффектом `IO` и «простые» (`numbersAggregator`, `limitGetter`), с одним аргументом и с несколькими (`contentParser`). Таким способом можно захватить побольше аспектов типичного программного продукта.

В наиболее популярном императивном стиле 
```scala
object ioApp extends IOApp:
  def run(args: List[String]) = for  
    url     <- argsParser(args)  
    content <- urlLoader(url)  
    limit   =  limitGetter(())  
    nums    <- contentParser(content, limit)  
    printed <- nums.traverse(numberPrinter)  
  yield numbersAggregator(printed)
```

Псевдо-функциональный «монадический» стиль:
```scala
def run(args: List[String]) =  
  argsParser(args)  
    .flatMap(urlLoader)  
    .tupleRight(limitGetter(()))  
    .flatMap(contentParser)  
    .flatMap(_.traverse(numberPrinter))  
    .map(numbersAggregator)
```

Чисто функциональный стиль:
```scala
val program =
  argsParser                     andThenF  
  (urlLoader mergeF limitGetter) andThenF  
  contentParser                  andThenTraverse  
  numberPrinter                  andThenMap  
  numbersAggregator  
  
def run(args: List[String]) = program(args)
```

Стиль логического программирования:
```scala
def run(using args: List[String]) = Infer[IO[ExitCode]]
```


Уникальность типов моделей предметной области 


# Теория
Изоморфизм Карри-Ховарда

Match Types — зависят от свидетельств равенства типов, лежащих в контексте
# Задача о ферзях


# Логические эффекты


# Контекстные паттерны в Scala

`NoGiven[A =:= B]`

# Сложности и перспективы


# Дополнительная литература



