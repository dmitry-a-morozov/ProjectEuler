
type Suit = 
    Spades | Diamonds | Clubs | Hearts
    static member Parse = function
        | 'S' -> Spades
        | 'D' -> Diamonds
        | 'C' -> Clubs
        | 'H' -> Hearts
        | c -> invalidArg "Suit" (string c)

type Face = int

[<Literal>] 
let Jack = 11
[<Literal>]
let Queen = 12
[<Literal>]
let King = 13
[<Literal>]
let Ace = 14

type Card = Face * Suit

let parseFace = function
    | 'A' -> Ace | 'K' -> King | 'Q' -> Queen | 'J' -> Jack
    | 'T' -> 10
    | x when x >= '2' && x <= '9' -> int x - int '0'
    | c -> invalidArg "Face" (string c)

type Hand = Card[]

let parseCard (s : string) = 
    match s.ToCharArray() with
    | [| face ; suit |] -> parseFace face, Suit.Parse suit
    | _ -> invalidArg "Card" s

let twoHands (s :string) = 
    let xs = s.Split(' ')
    assert (xs.Length = 10)
    xs.[..4] |> Array.map parseCard |> Array.sort |> Array.rev,
    xs.[5..] |> Array.map parseCard |> Array.sort |> Array.rev

let consecutive(c1 : Card, c2 : Card) = 
    match c1, c2 with
    | (Ace, _), (King, _) -> true
    | (King, _), (Queen, _) -> true
    | (Queen, _), (Jack, _) -> true
    | (Jack, _), (10, _) -> true
    | (x, _), (y, _) -> x - 1 = y 

let (|Flush|_|) (hand : Hand) = if hand |> Array.map snd |> Seq.distinct |> Seq.length = 1 then Some() else None
let (|Straight|_|) (hand : Hand) = if hand |> Seq.pairwise |> Seq.map consecutive |> Seq.reduce (&&) then Some() else None
let (|StraightFlush|_|) = function | Straight & Flush -> Some() | _ -> None
let (|RoyalFlush|_|) = function | StraightFlush & [|(Ace, _); _|] -> Some() | _ -> None

let (|FourOfAKind|_|) (hand : Hand) = 
    match hand with
    | [| (v1, _); (v2, _); (v3, _); (v4, _); tail |] 
    | [| tail; (v1, _); (v2, _); (v3, _); (v4, _) |] when v1 = v2 && v2 = v3 && v3 = v4 -> Some(v1, tail)
    | _ -> None

let (|ThreeOfAKind|_|) (hand : Hand) = 
    match hand with
    | [| (v1, _); (v2, _); (v3, _); x; y |] 
    | [| x; (v1, _); (v2, _); (v3, _); y |] 
    | [| x; y; (v1, _); (v2, _); (v3, _) |] when v1 = v2 && v2 = v3 -> Some(v1, (x, y))
    | _ -> None

let (|FullHouse|_|) = function
    | ThreeOfAKind(three, ((x, _), (y, _))) when x = y -> Some(three, x)
    | _ -> None

let (|TwoPairs|_|) (hand : Hand) = 
    match hand with
    | [| (v11, _); (v12, _); (v21, _); (v22, _); tail |]  
    | [| (v11, _); (v12, _); tail; (v21, _); (v22, _) |]
    | [| tail; (v11, _); (v12, _); (v21, _); (v22, _) |]
        when v11 = v12 && v21 = v22 -> Some(v11, v21, tail)
    | _ -> None

let (|OnePairs|_|) (hand : Hand) = 
    match hand with
    | [| (v1, _); (v2, _); x; y; z |]  
    | [| x; (v1, _); (v2, _); y; z |] 
    | [| x; y; (v1, _); (v2, _); z |] 
    | [| x; y; z; (v1, _); (v2, _) |] 
        when v1 = v2 -> Some(v1, (x, y, z))
    | _ -> None

let compareCards t1 t2 = 
    if t1 < t2 then 2
    elif t1 > t2 then 1
    else invalidOp (sprintf "Tie. First hand: %A. Second hand: %A" t1 t2)

let compareHands(first, second) =
    match first, second with
    | RoyalFlush, RoyalFlush -> compareCards first second
    | RoyalFlush, _ -> 1
    | _, RoyalFlush -> 2

    | StraightFlush, StraightFlush -> compareCards first second
    | StraightFlush, _ -> 1 
    | _, StraightFlush -> 2

    | FourOfAKind x, FourOfAKind y -> compareCards x y
    | FourOfAKind _, _ -> 1
    | _, FourOfAKind _ -> 2

    | FullHouse x, FullHouse y -> compareCards x y
    | FullHouse _, _ -> 1 
    | _, FullHouse _ -> 2

    | Flush, Flush -> compareCards first second
    | Flush, _ -> 1 
    | _, Flush -> 2

    | Straight, Straight -> compareCards first second
    | Straight, _ -> 1 
    | _, Straight -> 2

    | ThreeOfAKind x, ThreeOfAKind y -> compareCards x y
    | ThreeOfAKind _, _ -> 1
    | _, ThreeOfAKind _ -> 2

    | TwoPairs x, TwoPairs y -> compareCards x y
    | TwoPairs _, _ -> 1
    | _, TwoPairs _ -> 2

    | OnePairs x, OnePairs y -> compareCards x y
    | OnePairs _, _ -> 1
    | _, OnePairs _ -> 2

    | _ -> compareCards first second

let testHands, testWinExpections = 
    [
        "5H 5C 6S 7S KD 2C 3S 8S 8D TD"
        "5D 8C 9S JS AC 2C 5C 7D 8S QH"
        "2D 9C AS AH AC 3D 6D 7D TD QD"
        "4D 6S 9H QH QC 3D 6D 7H QD QS"
        "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"
    ],
    [ 2; 1; 2; 1; 1]

assert (testHands |> List.map (twoHands >> compareHands) = testWinExpections)

let input = 
    (new System.Net.WebClient())
     .DownloadString("http://projecteuler.net/project/poker.txt")
     .Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)

input
|> Array.map (twoHands >> compareHands)
|> Array.filter ((=) 1)
|> Array.sum





    