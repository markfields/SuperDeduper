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



  [<EntryPoint>]
  let main argv =
    printfn "%A" argv
    result {
      let! (rootInfo) = argv |> parseCommandArgs
      return ()
    } |> ignore
    0 // return an integer exit code
