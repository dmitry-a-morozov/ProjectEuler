
let reverse x = 
    let rec loop rem acc = 
        if rem = 0 then acc 
        else loop (rem / 10) (acc * 10 + (rem % 10))
    loop x 0
    
[
    for x in 100..999 do
        for y in 100..999 do
            let product = x * y
            let isPalindrome = product = reverse product
            if isPalindrome then yield product 
]
|> List.max 
