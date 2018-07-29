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
  and DirectoryNode = {
    path: string;
    hash: string;
    children: FileTreeNode list;
  }
  and FileNode = {
    path: string;
    hash: string;
  }

  type FileTreeNode
  with
    static member FromFileSystemInfo (fsi: FileSystemInfo) =
      // Hashing NYI
      match fsi with
      | :? DirectoryInfo as di -> Directory { path = di.FullName; hash = ""; children = di.GetFileSystemInfos(); } |> Result.Ok
      | :? FileInfo as fi -> File { path = fi.FullName; hash = ""; } |> Result.Ok
      | _ -> Result.Error "Unrecognized FileSystemInfo subtype"

  let rec buildSubtree (fsi: FileSystemInfo): Result<FileTreeNode, string> =
    //** Hashing NYI
    match fsi with
    | :? DirectoryInfo as di ->
      result {
        let! childNodes =
          di.GetFileSystemInfos()
          |> List.ofArray
          |> List.map buildSubtree
          |> List.fold
              (fun (list: Result<FileTreeNode list, string>) (next: Result<FileTreeNode, string>) ->
                match list, next with
                | Ok l, Ok n -> n::l |> Result.Ok
                | _, _ -> Result.Error "Invalid node in subtree"
              )
              (Result.Ok [])
        
        return Directory { path = di.FullName; hash = ""; children = childNodes; }
      }
    | :? FileInfo as fi -> File { path = fi.FullName; hash = ""; } |> Result.Ok
    | _ -> Result.Error "Unrecognized FileSystemInfo subtype"    

  [<EntryPoint>]
  let main argv =
    printfn "START: %A" System.DateTime.Now
    result {
      let! (rootInfo) = argv |> parseCommandArgs
      let! tree = rootInfo |> buildSubtree
      return ()
    } |> ignore
    printfn "END: %A" System.DateTime.Now
    0 // return an integer exit code
