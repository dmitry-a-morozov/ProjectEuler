
let countChange sum coins = 
    let rec loop sum coins =
        if sum = 0 then [[]]
        else 
            match coins with
            | h::t -> 
                [sum .. -h .. 0] 
                |> List.mapi(fun i reminder -> [for xs in loop reminder t -> List.replicate i h @ xs])
                |> List.concat 
            | [] -> []
    loop sum coins |> List.length 

countChange 200 [200;100;50;20;10;5;2;1] 
