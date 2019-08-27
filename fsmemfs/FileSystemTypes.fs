namespace LowLevelDesign.FsMemFs

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type NodeProperties = {
    Name : string
    CreatedFileTimeUtc : uint64
    LastModifiedFileTimeUtc : uint64
    FileAttributes : uint32
 }

type FileNodeProperties = {
    FileSize : uint64
    AllocationSize : uint64
    FileData : byte array
    FileIndex : uint64
 }

[<NoComparison>]
type TreeNode =
    | FileNode of NodeProperties * FileNodeProperties
    | FolderNode of NodeProperties * Dictionary<string, TreeNode>


exception FolderDoesNotExist of string

type FileTree(root : TreeNode) =

    let splitPath (path : string) = path.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries)

    let rec findNode node index (splits : string array) =
        match node with
        | FileNode _ -> Some node
        | FolderNode _ when index = splits.Length -> Some node
        | FolderNode(_, children) ->
            let p = splits.[index]
            match children.TryGetValue(p) with
            | (true, child) -> findNode child (index + 1) splits
            | (false, _) -> None

    member this.AddOrUpdateNode (path : string) node =
        let rec findOrCreateFolderNode node index (splits : string array) =
            match node with
            | FileNode _ -> raise (FolderDoesNotExist(sprintf "should be folder but is file: '%s'" path))
            | FolderNode _ when index = splits.Length -> node
            | FolderNode(_, children) ->
                let p = splits.[index]
                match children.TryGetValue(p) with
                | (true, child) -> findOrCreateFolderNode child (index + 1) splits
                | (false, _) ->
                    let child = FolderNode ({ Name = p
                                              CreatedFileTimeUtc = uint64 (DateTime.UtcNow.ToFileTimeUtc())
                                              LastModifiedFileTimeUtc = uint64 (DateTime.UtcNow.ToFileTimeUtc())
                                              FileAttributes = uint32 FileAttributes.Directory
                                            }, Dictionary<string, TreeNode>(StringComparer.OrdinalIgnoreCase))
                    children.[p] <- child
                    findOrCreateFolderNode child (index + 1) splits

        let splits = splitPath path
        assert (splits.Length <> 0)
        let parent = if splits.Length = 1 then root
                     else findOrCreateFolderNode root 0 splits.[0..(splits.Length - 2)]
        match parent with
        | FolderNode(_, children) ->
            match node with
            | FileNode({ Name = name }, _)
            | FolderNode({ Name = name }, _) -> children.[name] <- node
        | _ -> assert false; invalidOp "should never happen"

    member this.FindNode(path : string) =
        findNode root 0 (splitPath path)

    member this.FindParentNode(path : string) =
        let splits = splitPath path
        if splits.Length <= 1 then
            Some root // root is the parent of root
        else
            findNode root 0 splits.[0..(splits.Length - 2)]

    member this.RemoveNode(path : string) =
        let endIndex = path.LastIndexOfAny([| '/'; '\\' |])
        let name = if endIndex = -1 then path else path.Substring(endIndex + 1)
        let parentPath = if endIndex = -1 then "" else path.Substring(0, endIndex)
        let parent = this.FindNode parentPath
        match parent with
        | Some(FolderNode(_, children)) -> children.Remove(name) |> ignore
        | _ -> raise (FolderDoesNotExist parentPath)


type DirectorySearchContext(parent : TreeNode, node : TreeNode, pattern : string, marker : string) =

    let pattern = if pattern = null then "*" else pattern
    let marker = if marker = null then "" else marker

    let mutable index = 0

    let entries =
        match node with
            | FileNode _ -> assert false; [||]
            | FolderNode(_, children) ->
                // TODO implement simple string matching instead of using regexes
                let regexMatch s =
                    let regex = "^" + Regex.Escape(pattern).Replace("~", "\\~").Replace(">", ".").Replace("<", ".*") + "$"
                    Regex.IsMatch(s, regex, RegexOptions.IgnoreCase)

                let i = if pattern = "*" || pattern = "." || pattern = "?" then
                            [| (".", node); ("..", parent) |]
                        else [||]

                let items = children.Values
                            |> Seq.choose (fun v ->
                                match v with
                                | FileNode(props, _)
                                | FolderNode(props, _) ->
                                    if pattern = "*" || regexMatch props.Name then
                                        Some(props.Name, v)
                                    else None)
                            |> Seq.toArray |> Array.append i

                if marker <> "" then
                    index <- (items |> Array.findIndex (fun (name, _) -> name = marker)) + 1

                items

    member this.Entries = entries

    member this.Index
        with get () = index
        and set (value) = index <- value

