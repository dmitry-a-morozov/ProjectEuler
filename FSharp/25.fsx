
(1I, 1I) 
|> Seq.unfold (fun(current, next) -> Some(current, (next, current + next)))
|> Seq.takeWhile (fun x -> x / 10I ** (1000 - 1) = 0I)
|> Seq.length 
|> (+) 1