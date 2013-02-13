open System

DateTime(1901, 1, 1).AddMonths
|> Seq.initInfinite 
|> Seq.takeWhile(fun date -> date <= DateTime(2000,12,31))
|> Seq.filter (fun x -> x.DayOfWeek = DayOfWeek.Sunday)
|> Seq.length

