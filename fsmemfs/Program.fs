module LowLevelDesign.FsMemFs.Program

[<EntryPoint>]
let main _ =
    use service = new MemoryFileSystemService()
    service.Run()
