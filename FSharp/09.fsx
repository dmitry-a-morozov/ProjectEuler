
let mutable result = 0 
for a = 1 to 1000 do
    for b = a + 1 to 1000 do
        let c = 1000 - a - b
        if a * a + b * b = c * c then 
            result <- a * b * c 
result
