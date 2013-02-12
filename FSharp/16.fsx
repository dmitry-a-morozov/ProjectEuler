
2I ** 1000
|> Seq.unfold(fun x -> if x = 0I then None else Some(x % 10I, x / 10I))
|> Seq.sum
