
let hotpo = Seq.unfold <| function
    | 1L -> None
    | x when x % 2L = 0L -> Some(x, x / 2L)
    | x -> Some(x, 3L * x + 1L)
    
{ 1L .. 999999L } |> Seq.maxBy (hotpo >> Seq.length)
