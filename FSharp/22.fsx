
seq {
    let wc = new System.Net.WebClient()
    let data = wc.DownloadString("http://projecteuler.net/project/names.txt")
    for s in data.Split(',') -> s.Trim('"')
}
|> Seq.sort
|> Seq.map (fun name -> List.sum [for c in name -> int c - int 'A' + 1])
|> Seq.mapi (fun pos value -> (pos + 1) * value)
|> Seq.sum