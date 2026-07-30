
# Мотивация

Scala, как и прочие современные языки программирования позволяют закодировать одну и ту же логику в разных стилях. Давайте посмотрим как это может выглядеть на примере эмуляции простой задачи:
- из параметров программы достать адрес сайта;
- загрузить с сайта его содержимое;
- найти там лимитированное количество чисел;
- вывести несколько первых чисел в консоль;
- если распечатали хотя бы одно число, то вернуть ExitCode 0.

Сперва смоделируем бизнес-функции программы:
```scala
import cats.effect.{ExitCode, IO}

type ArgsParser =        List[String]        => IO[ScanUrl]  
type UrlLoader =         ScanUrl             => IO[Content]  
type LimitGetter =       Any                 => NumLimit  
type ContentParser =     LimitedContent      => IO[List[Num]]  
type NumberPrinter =     Num                 => IO[NumberPrinted]  
type NumbersAggregator = List[NumberPrinted] => ExitCode
```

Опишем необходимые модели:
```scala
case class ScanUrl(str: String)  
case class Content(str: String)  
case class Num(n: Int)  
case class NumLimit(n: Int)  
type LimitedContent = (content: Content, limit: NumLimit)  
case class NumberPrinted()
```

Предоставляем реализации функций:
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
      .findAllIn(content.str)  
      .flatMap(_.toIntOption)  
      .take(limit.n)  
      .map(Num.apply)  
      .toList  
  
given limitGetter: LimitGetter = _ => NumLimit(10) // к примеру, читаем из конфишурации  
  
given numberPrinter: NumberPrinter = num =>  
  IO  
    .println(num.n)  
    .as(NumberPrinted())  
  
given numbersAggregator: NumbersAggregator =  
  case _ :: _ => ExitCode.Success  
  case Nil    => ExitCode.Error
```

Императивный стиль:
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


# Теория
Изоморфизм Карри-Ховарда

Match Types — зависят от свидетельств равенства типов, лежащих в контексте
# Задача о ферзях


# Логические эффекты


# Контекстные паттерны в Scala

`NoGiven[A =:= B]`

# Сложности и перспективы


# Дополнительная литература



