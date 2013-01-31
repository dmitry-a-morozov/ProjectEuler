
let divisors n =
    seq {
        yield 1
        let root = n |> float |> sqrt |> int
        for i = 2 to root - 1 do
            if n % i = 0 then
                yield i
                yield n/i
    }

let divisorSum = divisors >> Seq.sum

let isAmicable a =
    let b = divisorSum a
    a <> b && a = divisorSum b

{ 1..9999 } |> Seq.filter isAmicable |> Seq.sum
