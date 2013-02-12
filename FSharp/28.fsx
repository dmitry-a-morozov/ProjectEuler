
{ 3 .. 2 .. 1001 }
|> Seq.collect (fun side ->
    seq {
        yield side * side;
        yield side * side - side - 1;
        yield side * side - (side - 1) * 2;
        yield side * side - (side - 1) * 3;
    })
|> Seq.fold (+) 1