namespace SuperDeduper

open System.IO
open System.Security.Cryptography

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

    member this.AddItem (hash: string) (item: string) =
      let items = 
        if dict.ContainsKey hash
        then dict.[hash]
        else []
      dict.[hash] <- item::items
    
    member this.GetItems (hash: string) =
      if dict.ContainsKey hash
      then Some dict.[hash]
      else None

  type FileTreeNode =
    | Directory of DirectoryNode
    | File of FileNode
    | TooLong of int
  and DirectoryNode = {
    path: string;
    hash: string;
    children: FileTreeNode list;
  }
  and FileNode = {
    path: string;
    hash: string;
  }

  let rec buildSubtree (fsi: FileSystemInfo): Result<FileTreeNode, string> =
    if fsi.FullName.Length >= 248
    then
      printfn "%A" fsi.FullName
      TooLong fsi.FullName.Length |> Result.Ok
    else
      //** Hashing NYI
      match fsi with
      | :? DirectoryInfo as di ->
        result {
          let! childNodes =
            di.GetFileSystemInfos()
            |> List.ofArray
            |> List.map buildSubtree
            |> allOkOrElse "Invalid node in subtree"
          
          return Directory { path = di.FullName; hash = ""; children = childNodes; }
        }
      | :? FileInfo as fi -> File { path = fi.FullName; hash = ""; } |> Result.Ok
      | _ -> Result.Error "Unrecognized FileSystemInfo subtype"    

  let printFileTree (root: FileTreeNode) =
    let printTabs = List.fold (fun _ s -> printf "%s" s) ()
    let rec printSubtree (tabs: string list) = function
      | Directory dir -> printTabs tabs; printfn "%s" dir.path; dir.children |> (List.map (printSubtree <| "  "::tabs)) |> ignore
      | File _ -> printTabs tabs; printfn "f";
      | TooLong _ -> printTabs tabs; printfn "\\\\TOO LONG\\\\";
    root |> printSubtree []

  [<EntryPoint>]
  let main argv =
    printfn "START: %A" System.DateTime.Now
    result {
      let! (rootInfo) = argv |> parseCommandArgs
      let! tree = rootInfo |> buildSubtree
      printfn "TREE: %A" System.DateTime.Now
      tree |> printFileTree
      return ()
    } |> ignore
    printfn "END: %A" System.DateTime.Now
    0 // return an integer exit code
