
Seq.unfold (fun(x, y) -> Some(x + y, (y, x + y))) (1, 1)
|> Seq.takeWhile (fun x -> x <= 4000000)
|> Seq.filter (fun x -> x % 2 = 0)
|> Seq.sum
