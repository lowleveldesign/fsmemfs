module LowLevelDesign.FsMemFs.Tests.FileTreeTests

open FsUnit
open LowLevelDesign.FsMemFs
open NUnit.Framework
open System
open System.Collections.Generic
open System.IO

let fileProps = { FileSize = 0UL; AllocationSize = 0UL; FileData = [||]; FileIndex = 0UL }

let createSampleTree() =
    let aNodes = [
                    "aa", FileNode ({ Name = "aa"
                                      CreatedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                                      LastModifiedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                                      FileAttributes = uint32 FileAttributes.Archive
                                    }, fileProps)
                    "ab", FileNode ({ Name = "ab"
                                      CreatedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                                      LastModifiedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                                      FileAttributes = uint32 FileAttributes.Archive
                                    }, fileProps)
                 ] |> dict |> Dictionary

    let nodes = [
                    "a", FolderNode ({ Name = "a"
                                       CreatedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                                       LastModifiedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                                       FileAttributes = uint32 FileAttributes.Directory }, aNodes)
                    "b", FolderNode ({ Name = "b"
                                       CreatedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                                       LastModifiedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                                       FileAttributes = uint32 FileAttributes.Directory }, aNodes)
                ] |> dict |> Dictionary

    FileTree (FolderNode ({ Name = "root"
                            CreatedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                            LastModifiedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                            FileAttributes = uint32 FileAttributes.Directory }, nodes))

[<Test>]
let ``Find node in a tree``() =
    let tree = createSampleTree()

    let root = tree.FindNode("\\")
    match root with
    | Some(FolderNode({ Name = name }, _)) -> name |> should equal "root"
    | _ -> false |> should be True

    let root = tree.FindNode("")
    match root with
    | Some(FolderNode({ Name = name }, _)) -> name |> should equal "root"
    | _ -> false |> should be True

    tree.FindNode("b/c") |> should equal None

    match (tree.FindNode("b")) with
    | Some(FolderNode({ Name = name }, _)) -> name |> should equal "b"
    | _ -> false |> should be True

    match (tree.FindNode("/b")) with
    | Some(FolderNode({ Name = name }, _)) -> name |> should equal "b"
    | _ -> false |> should be True

    match (tree.FindNode("a\\aa")) with
    | Some(FileNode({ Name = name }, _)) -> name |> should equal "aa"
    | _ -> false |> should be True

    match (tree.FindNode("a\\///aa")) with
    | Some(FileNode({ Name = name }, _)) -> name |> should equal "aa"
    | _ -> false |> should be True

    tree.FindNode("a\\cc") |> should equal None

    tree.FindNode("c") |> should equal None

[<Test>]
let ``Find nearest node in a tree``() =
    let tree = createSampleTree()

    match tree.FindParentNode("") with
    | Some(FolderNode({ Name = name }, _)) -> name |> should equal "root" 
    | _ -> false |> should be True

    match tree.FindParentNode("b/c") with
    | Some(FolderNode({ Name = name }, _)) ->
        name |> should equal "b"
    | _ -> false |> should be True

    match (tree.FindParentNode("\\\\a\\aa")) with
    | Some(FolderNode({ Name = name }, _)) ->
        name |> should equal "a"
    | _ -> false |> should be True

    match (tree.FindParentNode("\\a\\desktop")) with
    | Some(FolderNode({ Name = name }, _)) ->
        name |> should equal "a"
    | _ -> false |> should be True

[<Test>]
let ``Add and update a node in a tree``() =
    let tree = createSampleTree()

    let newFile = FileNode ({ Name = "baaa"
                              CreatedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                              LastModifiedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                              FileAttributes = uint32 FileAttributes.Archive
                            }, fileProps)
    tree.AddOrUpdateNode "b/ba/baa/baaa" newFile
    tree.FindNode "b/ba/baa/baaa" |> should equal (Some newFile)

    let newFile = FileNode ({ Name = "baaa"
                              CreatedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                              LastModifiedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                              FileAttributes = uint32 FileAttributes.Archive
                            }, fileProps)
    tree.AddOrUpdateNode "b/ba/baa/baaa" newFile
    tree.FindNode "b/ba/baa/baaa" |> should equal (Some newFile)

[<Test>]
let ``Remove a node from a tree``() =
    let tree = createSampleTree()

    let newFile = FileNode ({ Name = "baaa"
                              CreatedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                              LastModifiedFileTimeUtc = uint64 (DateTime.Now.ToFileTimeUtc())
                              FileAttributes = uint32 FileAttributes.Archive
                            }, fileProps)
    tree.AddOrUpdateNode "b/ba/baa/baaa" newFile
    tree.FindNode "b/ba/baa/baaa" |> should equal (Some newFile)
    tree.RemoveNode "b/ba/baa" |> should be True
    tree.FindNode "b/ba/baa/baaa" |> should equal (None)
