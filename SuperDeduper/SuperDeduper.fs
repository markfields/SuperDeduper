namespace SuperDeduper

open System.IO

module Main =
  let parseCommandArgs argv =
    let (|DirInfo|_|) (input as path:string) =
      try
        new DirectoryInfo(path) |> Some
      with
        _ -> None

    match argv |> List.ofArray with
    | [DirInfo info] -> Result.Ok info
    | _ -> Result.Error "Couldn't get info about that path"

  type HashMap() =
    let dict = new System.Collections.Generic.Dictionary<string, string list>()

    // Adds the item and returns the hash for quick lookup
    member this.AddItem (item: string) =
      let hash = item.Length.ToString()  //** Pretend hash function
      let items = 
        if dict.ContainsKey hash
        then dict.[hash]
        else []
      dict.[hash] <- item::items
      hash
    
    member this.GetItems (hash: string) =
      if dict.ContainsKey hash
      then Some dict.[hash]
      else None


  let rec traverse (root: DirectoryInfo) =
    let children = root.GetFileSystemInfos()
    ()

  [<EntryPoint>]
  let main argv =
    printfn "%A" argv
    result {
      let! (rootInfo) = argv |> parseCommandArgs

      return ()
    } |> ignore
    0 // return an integer exit code
