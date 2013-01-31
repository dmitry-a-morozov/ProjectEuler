
let primeFactors x = 
    let rec loop reminder factor result = 
        if reminder <= factor  then factor :: result 
        elif reminder % factor = 0 then loop (reminder / factor) factor (factor :: result)
        else loop reminder (factor + 1) result
    loop x 2 []

let numberOfDivisors  x = primeFactors x |> Seq.countBy id |> Seq.fold (fun acc (factor, exp) -> acc * (exp + 1)) 1

Seq.unfold (fun (x, acc) -> Some (acc + x, (x + 1, acc + x))) (1,0)
|> Seq.find (fun x -> numberOfDivisors x > 500)
