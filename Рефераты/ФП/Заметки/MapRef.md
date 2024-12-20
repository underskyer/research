
```scala
     Either
       .cond(shardCount >= 1,
         List
           .fill(shardCount)(())
           .traverse(_ => Concurrent[F].ref[Map[K, V]](Map.empty))
           .map(fromNonEmptySeqRefs[F, K, V] compose NonEmptySeq.fromSeqUnsafe),
       )
       .sequence
       .flatMap(ApplicativeError[F, Throwable].fromEither)
```
