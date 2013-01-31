
let rec gcd n m = if m = 0L then n else gcd m (n % m)
let lcm n m = n * m / gcd n m

List.reduce lcm [1L .. 20L]
