
//TYPES

//In the card game poker...

type Card = Face * Suit

and Suit =     Spades | Diamonds | Clubs | Hearts

//The cards are valued in the order:
//2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

and Face = int

[<Literal>] 
let Jack = 11
[<Literal>]
let Queen = 12
[<Literal>]
let King = 13
[<Literal>]
let Ace = 14

// ... a hand consists of five cards ..
type Hand = Card * Card * Card * Card * Card

//RULES

//High Card: Highest value card.
let highCard x y = 
    if x < y then 2
    elif x > y then 1
    else invalidOp (sprintf "Tie. First hand: %A. Second hand: %A" x y)

//One Pair: Two cards of the same value.
let (|OnePairs|_|) = function
    | (f1, _), (f2, _), x, y, z 
    | x, (f1, _), (f2, _), y, z  
    | x, y, (f1, _), (f2, _), z  
    | x, y, z, (f1, _), (f2, _)  
        when f1 = f2 -> Some(f1, (x, y, z))
    | hand -> None

//Two Pairs: Two different pairs.
let (|TwoPairs|_|) = function
    | (f11, _), (f12, _), (f21, _), (f22, _), tail  
    | (f11, _), (f12, _), (f21, _), tail, (f22, _)
    | (f11, _), (f12, _), tail, (f21, _), (f22, _)
    | (f11, _), tail, (f12, _), (f21, _), (f22, _)
    | tail, (f11, _), (f12, _), (f21, _), (f22, _)
        when f11 = f12 && f21 = f22 -> Some(f11, f21, tail)
    | _ -> None

//Three of a Kind: Three cards of the same value.
let (|ThreeOfAKind|_|) = function
    | (f1, _), (f2, _), (f3, _), x, y 
    | x, (f1, _), (f2, _), (f3, _), y 
    | x, y, (f1, _), (f2, _), (f3, _) 
        when f1 = f2 && f2 = f3 -> Some(f1, (x, y))
    | _ -> None

//Straight: All cards are consecutive values.
let (|Straight|_|) (c1, c2, c3, c4, c5) = 
    let consecutiveCards = function
        | (Ace, _), (King, _) 
        | (King, _), (Queen, _) 
        | (Queen, _), (Jack, _) 
        | (Jack, _), (10, _) -> true
        | (x, _), (y, _) -> x - 1 = y 

    if Seq.forall consecutiveCards [(c1, c2); (c2, c3); (c3, c4); (c4, c5)]
    then Some()
    else  None

//Flush: All cards of the same suit.
let (|Flush|_|) ((_, s1), (_, s2), (_, s3), (_, s4), (_, s5)) = 
    if s1 = s2 && s2 = s3 && s3 = s4 && s4 = s5 
    then Some() 
    else None

//Full House: Three of a kind and a pair.
let (|FullHouse|_|) = function 
    | ThreeOfAKind(three, ((f1, _), (f2, _))) 
        when f1 = f2 -> Some(three, f1) 
    | _ -> None

//Four of a Kind: Four cards of the same value.
let (|FourOfAKind|_|) = function
    | (f1, _), (f2, _), (f3, _), (f4, _), tail 
    | tail, (f1, _), (f2, _), (f3, _), (f4, _) 
        when f1 = f2 && f2 = f3 && f3 = f4 -> Some(f1, tail)
    | _ -> None

//Straight Flush: All cards are consecutive values of same suit.
let (|StraightFlush|_|) = function 
    | Straight & Flush -> Some() 
    | _ -> None

//Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
let (|RoyalFlush|_|) = function 
    | ((Ace, _), _, _, _, _) & StraightFlush -> Some() 
    | _ -> None

(*
If two players have the same ranked hands 
then the rank made up of the highest value wins; 
for example, a pair of eights beats a pair of fives. 
But if two ranks tie, for example, 
both players have a pair of queens, 
then highest cards in each hand are compared; 
if the highest cards tie then 
the next highest cards are compared, and so on.
*)

let compareHands(first, second) =
    match first, second with
    | RoyalFlush, RoyalFlush -> highCard first second
    | RoyalFlush, _ -> 1
    | _, RoyalFlush -> 2

    | StraightFlush, StraightFlush -> highCard first second
    | StraightFlush, _ -> 1 
    | _, StraightFlush -> 2

    | FourOfAKind x, FourOfAKind y -> highCard x y
    | FourOfAKind _, _ -> 1
    | _, FourOfAKind _ -> 2

    | FullHouse x, FullHouse y -> highCard x y
    | FullHouse _, _ -> 1 
    | _, FullHouse _ -> 2

    | Flush, Flush -> highCard first second
    | Flush, _ -> 1 
    | _, Flush -> 2

    | Straight, Straight -> highCard first second
    | Straight, _ -> 1 
    | _, Straight -> 2

    | ThreeOfAKind x, ThreeOfAKind y -> highCard x y
    | ThreeOfAKind _, _ -> 1
    | _, ThreeOfAKind _ -> 2

    | TwoPairs x, TwoPairs y -> highCard x y
    | TwoPairs _, _ -> 1
    | _, TwoPairs _ -> 2

    | OnePairs x, OnePairs y -> highCard x y
    | OnePairs _, _ -> 1
    | _, OnePairs _ -> 2

    | _ -> highCard first second

//PLUMBING

let parseFace = function
    | 'A' -> Ace | 'K' -> King | 'Q' -> Queen | 'J' -> Jack 
    | 'T' -> 10
    | x when x >= '2' && x <= '9' -> int x - int '0'
    | c -> invalidArg "Face" (string c)

let parseSuit = function
    | 'S' -> Spades
    | 'D' -> Diamonds
    | 'C' -> Clubs
    | 'H' -> Hearts
    | c -> invalidArg "Suit" (string c)

let parseCard (s : string) = 
    match s.ToCharArray() with
    | [| face ; suit |] -> parseFace face, parseSuit suit
    | _ -> invalidArg "Card" s

let parseTwoHands (s : string) = 
    let fiveItemsArrayToHand xs = 
        xs 
        |> Array.map parseCard 
        |> Array.sort 
        |> fun xs -> xs.[4], xs.[3], xs.[2], xs.[1], xs.[0]

    let xs = s.Split(' ')
    assert (xs.Length = 10)
    let first = fiveItemsArrayToHand xs.[..4]
    let second = fiveItemsArrayToHand xs.[5..]
    first, second

//TRAINING SET

let testHands, testWinExpections = 
    [
        "5H 5C 6S 7S KD 2C 3S 8S 8D TD"
        "5D 8C 9S JS AC 2C 5C 7D 8S QH"
        "2D 9C AS AH AC 3D 6D 7D TD QD"
        "4D 6S 9H QH QC 3D 6D 7H QD QS"
        "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"
    ],
    [ 2; 1; 2; 1; 1]

assert (testHands |> List.map (parseTwoHands >> compareHands) = testWinExpections)

//VALIDATION SET

let input = 
    (new System.Net.WebClient())
     .DownloadString("http://projecteuler.net/project/poker.txt")
     .Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)

input
|> Array.map (parseTwoHands >> compareHands)
|> Array.filter ((=) 1)
|> Array.sum

