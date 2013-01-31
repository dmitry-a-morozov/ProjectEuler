
{ 1I .. 100I } 
|> Seq.reduce (*) 
|> string
|> Seq.map (string >> int)
|> Seq.sum
