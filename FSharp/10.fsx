let primes = 
    seq {    
        let knownComposites = System.Collections.Generic.HashSet() 
        yield 2 
        for i in 3..2..int 1E7 do
            let found = knownComposites.Contains(i)
            if not found then 
                yield i
                for j in i..i..int 1E7 do 
                    knownComposites.Add(j) |> ignore          
    }

primes 
    |> Seq.takeWhile (fun x -> x < 2000000)
    |> Seq.map int64
    |> Seq.sum
