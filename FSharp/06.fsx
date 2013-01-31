
let sumOfSquares = [for x in 1..100 -> x * x] |> List.sum 
let sum = List.sum [1..100]
let squareOfSum = sum * sum;
    
squareOfSum - sumOfSquares 
