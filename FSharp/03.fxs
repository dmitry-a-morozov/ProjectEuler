
let rec largestFactor x factor = 
    if factor = x then factor 
    elif x % factor = 0L then largestFactor (x / factor) factor 
    else largestFactor x (factor + 1L)

largestFactor 600851475143L 2L
