

let ``1..9`` = [""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
let ``10..19`` =  ["ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"]
let dozens = [""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"]

let spellNumber n = 
    [
        if n = 1000 then yield! "onethousand"
        else 
            let hundredsDigit = n / 100
            if hundredsDigit > 0 then yield! sprintf "%shundred" ``1..9``.[hundredsDigit] 
            let moduloOf100 = n % 100
            if hundredsDigit <> 0 && moduloOf100 <> 0 then yield! "and"
            if moduloOf100 >= 10 && moduloOf100 <= 19 then yield! ``10..19``.[moduloOf100 - 10]
            else
                yield! dozens.[moduloOf100 / 10] + ``1..9``.[moduloOf100 % 10]
    ]

[ 1 .. 1000 ]
|> List.collect spellNumber
|> List.length