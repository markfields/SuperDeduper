namespace SuperDeduper

open System
open System.IO
open System.Security.Cryptography
open System.Text

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

    let sha1 = SHA1.Create()

    member this.ComputeHash: string -> string = Encoding.ASCII.GetBytes >> sha1.ComputeHash >> Convert.ToBase64String
    member this.ComputeFileHash: FileStream -> string = sha1.ComputeHash >> Convert.ToBase64String

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

  let hashMap = HashMap()

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

  type FileTreeNode
  with
    member node.Hash = 
      match node with
      | Directory dn -> dn.hash
      | File fn -> fn.hash
      | TooLong _ -> ""

  let hashFile (fi: FileInfo): string =
    try
      fi.OpenRead() |> hashMap.ComputeFileHash
    with
      _ -> "CANNOTREAD"

  let combineHashes (childNodes: FileTreeNode list) =
    let folder (sb:StringBuilder) (node: FileTreeNode) = sb.Append(node.Hash)
    let computeBuilderHash (sb: StringBuilder) = sb.ToString() |> hashMap.ComputeHash
    childNodes
    |> List.fold folder (new StringBuilder())
    |> computeBuilderHash

  let rec buildSubtree (fsi: FileSystemInfo): Result<FileTreeNode, string> =
    if fsi.FullName.Length >= 248
    then
      printfn "%A" fsi.FullName
      TooLong fsi.FullName.Length |> Result.Ok
    else
      match fsi with
      | :? DirectoryInfo as di ->
        result {
          let! childNodes =
            di.GetFileSystemInfos()
            |> List.ofArray
            |> List.map buildSubtree
            |> allOkOrElse "Invalid node in subtree"

          let path = di.FullName
          let hash = childNodes |> combineHashes
          let directory = Directory { path = path; hash = hash; children = childNodes; }

          hashMap.AddItem hash path
          return directory
        }
      | :? FileInfo as fi ->
        let path = fi.FullName
        let hash = fi |> hashFile
        let file = File { path = path; hash = hash; } |> Result.Ok
        hashMap.AddItem hash path
        file
      | _ -> Result.Error "Unrecognized FileSystemInfo subtype"    

  let printFileTree (root: FileTreeNode) =
    let printTabs = List.fold (fun _ s -> printf "%s" s) ()
    let rec printSubtree (tabs: string list) = function
      | Directory dir -> printTabs tabs; printfn "%s  %s" dir.hash dir.path; dir.children |> (List.map (printSubtree <| "  "::tabs)) |> ignore
      | File file -> printTabs tabs; printfn "%s" file.hash;
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
