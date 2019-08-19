namespace LowLevelDesign.FsMemFs

open Fsp
open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices

type FspFileInfo = Fsp.Interop.FileInfo

type MemoryFileSystem() =
    inherit FileSystemBase()

    let className = typedefof<MemoryFileSystem>.Name

    let MEMFS_SECTOR_SIZE = 512us;
    let MEMFS_SECTORS_PER_ALLOCATION_UNIT = 1us;

    let maxFileNodes = 1024UL;
    let maxFileSize = 16 * 1024 * 1024;

    let rootNode = (FolderNode ({ Name = "root"
                                  CreatedFileTimeUtc = uint64 (DateTime.UtcNow.ToFileTimeUtc())
                                  LastModifiedFileTimeUtc = uint64 (DateTime.UtcNow.ToFileTimeUtc())
                                  FileAttributes = uint32 FileAttributes.Directory },
                                  Dictionary<string, TreeNode>(StringComparer.OrdinalIgnoreCase)))
    let fileTree = FileTree(rootNode)

    (* ********************************************* *
     *                  HELPERS                      *
     * ********************************************* *)

    let getFileInfo node =
        match node with
        | FileNode(props, fileProps) ->
            FspFileInfo (
                           FileAttributes = props.FileAttributes,
                           FileSize = fileProps.FileSize,
                           AllocationSize = fileProps.AllocationSize,
                           CreationTime = props.CreatedFileTimeUtc,
                           LastAccessTime = props.LastModifiedFileTimeUtc,
                           LastWriteTime = props.LastModifiedFileTimeUtc,
                           ChangeTime = props.LastModifiedFileTimeUtc,
                           ReparseTag = 0u,
                           IndexNumber = 0UL,
                           HardLinks = 0u
                       )
        | FolderNode(props, _) ->
            FspFileInfo (
                           FileAttributes = uint32 props.FileAttributes,
                           FileSize = 0UL,
                           AllocationSize = 0UL,
                           CreationTime = props.CreatedFileTimeUtc,
                           LastAccessTime = props.LastModifiedFileTimeUtc,
                           LastWriteTime = props.LastModifiedFileTimeUtc,
                           ChangeTime = props.LastModifiedFileTimeUtc,
                           ReparseTag = 0u,
                           IndexNumber = 0UL,
                           HardLinks = 0u
                       )

    let setFileSize (node, newSize : uint64, setAllocationSize : bool) =
        let changeAllocationSize (props, fileProps) newSize =
            if newSize <= uint64 maxFileSize then
                try
                    let data = Array.zeroCreate (int32 newSize)
                    let len = int32 (min newSize fileProps.AllocationSize)
                    Array.Copy(fileProps.FileData, data, len)
                    let node = FileNode (props, { FileData = data; FileSize = uint64 len; AllocationSize = newSize })
                    Ok node
                with
                | :? OutOfMemoryException ->
                    Error FileSystemBase.STATUS_INSUFFICIENT_RESOURCES
            else
                Error FileSystemBase.STATUS_DISK_FULL

        let changeFileSize (props, fileProps) newSize =
            let fileSize = fileProps.FileSize
            if newSize > fileSize then
                Array.Clear(fileProps.FileData, int32 fileSize, int32 (newSize - fileSize))
            let node = FileNode(props, { fileProps with FileSize = newSize })
            Ok node

        match node with
        | FileNode(props, fileProps) as node ->
            match fileProps with
            | { AllocationSize = allocationSize } when setAllocationSize && allocationSize <> newSize ->
                changeAllocationSize (props, fileProps) newSize

            | { FileSize = fileSize; AllocationSize = allocationSize } when not setAllocationSize && fileSize <> newSize ->
                if allocationSize < newSize then
                    let allocationUnit = uint64 (MEMFS_SECTOR_SIZE * MEMFS_SECTORS_PER_ALLOCATION_UNIT)
                    let allocationSize = (newSize + allocationUnit - 1UL) / allocationUnit * allocationUnit
                    match changeAllocationSize (props, fileProps) allocationSize with
                    | Ok(FileNode(props, fileProps)) ->
                        changeFileSize (props, fileProps) newSize
                    | Ok(FolderNode _) ->
                        assert false; Error FileSystemBase.STATUS_INVALID_DEVICE_REQUEST
                    | err -> err
                else
                    changeFileSize (props, fileProps) newSize

            | _ ->
                Ok node
        | _ ->
            Error FileSystemBase.STATUS_OBJECT_PATH_NOT_FOUND

    (* ********************************************* *
     *             CREATE / OPEN                     *
     * ********************************************* *)

    override this.Open(path, _, _, _, fileDesc, fileInfo, _) =
        Logging.logVerbose className "Open" path

        fileDesc <- path

        match fileTree.FindNode path with
        | Some node ->
            fileInfo <- getFileInfo node
            FileSystemBase.STATUS_SUCCESS
        | None ->
            FileSystemBase.STATUS_NOT_FOUND

    override this.Create(path, createOptions, _, fileAttributes, _, allocationSize, _, fileDesc, fileInfo, _) =
        Logging.logVerbose className "Create" path

        fileDesc <- path

        let isFolder = createOptions &&& FileSystemBase.FILE_DIRECTORY_FILE <> 0u
        let props = {
            Name = Path.GetFileName(path.TrimEnd('/', '\\'))
            CreatedFileTimeUtc = uint64 (DateTime.UtcNow.ToFileTimeUtc())
            LastModifiedFileTimeUtc = uint64 (DateTime.UtcNow.ToFileTimeUtc())
            FileAttributes = if isFolder then fileAttributes else fileAttributes ||| uint32 FileAttributes.Archive
        }
        if isFolder then
            let node = FolderNode ({ props with FileAttributes = props.FileAttributes },
                                   Dictionary<string, TreeNode>(StringComparer.OrdinalIgnoreCase))

            fileTree.AddOrUpdateNode path node
            fileInfo <- getFileInfo node
            FileSystemBase.STATUS_SUCCESS
        else
            let node = FileNode (props, { FileSize = 0UL; AllocationSize = 0UL; FileData = [||] })
            match setFileSize (node, allocationSize, true) with
            | Ok node ->
                fileTree.AddOrUpdateNode path node
                fileInfo <- getFileInfo node
                FileSystemBase.STATUS_SUCCESS
            | Error err -> err

    (* ********************************************* *
     *                   READS                       *
     * ********************************************* *)

    override this.Read(_, fileDesc, buffer, offset, length, bytesTransferred) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "Read" path

        match fileTree.FindNode path with
        | None | Some(FolderNode _) -> assert false; FileSystemBase.STATUS_INVALID_DEVICE_REQUEST
        | Some(FileNode(_, fileProps)) ->
            if offset >= fileProps.FileSize then
                bytesTransferred <- 0u
                FileSystemBase.STATUS_END_OF_FILE
            else
                let endOffset : uint64 = min fileProps.FileSize (offset + uint64 length)
                let bytesRead = int32 (endOffset - offset)
                Marshal.Copy(fileProps.FileData, int offset, buffer, bytesRead)
                bytesTransferred <- uint32 bytesRead
                FileSystemBase.STATUS_SUCCESS

    override this.ReadDirectoryEntry(_, fileDesc, pattern, marker, context, fileName, fileInfo) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "ReadDirectoryEntry" path

        match fileTree.FindNode path with
        | None | Some(FileNode _) -> false
        | Some(FolderNode _ as node) ->
            if context = null then
                let parent = fileTree.FindParentNode path
                assert parent.IsSome
                context <- DirectorySearchContext(parent.Value, node, pattern, marker)
            let searchContext : DirectorySearchContext = downcast context

            let index = searchContext.Index
            if searchContext.Entries.Length > index then
                searchContext.Index <- index + 1
                let name, node = searchContext.Entries.[index]
                fileName <- name
                fileInfo <- getFileInfo node
                true
            else
                false

    override this.GetFileInfo(_, fileDesc, fileInfo) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "GetFileInfo" path

        match fileTree.FindNode path with
        | Some node ->
            fileInfo <- getFileInfo node
            FileSystemBase.STATUS_SUCCESS
        | None ->
            FileSystemBase.STATUS_NOT_FOUND

    (* ********************************************* *
     *                  WRITES                       *
     * ********************************************* *)

    override this.Write(_, fileDesc, buffer, offset,
                         length, writeToEndOfFile, constrainedIo,
                         bytesTransferred, fileInfo) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "Write" path

        match fileTree.FindNode path with
        | Some(FileNode(_, fileProps)) when constrainedIo && offset >= fileProps.FileSize ->
            FileSystemBase.STATUS_SUCCESS
        | Some(FileNode(_, fileProps) as node) ->
            let offset = if not constrainedIo && writeToEndOfFile then fileProps.FileSize else offset
            let endOffset = if constrainedIo then min fileProps.FileSize (offset + uint64 length)
                            else offset + uint64 length
            if endOffset > fileProps.FileSize then
                match setFileSize (node, endOffset, false) with
                | Ok(FileNode(_, fileProps) as node) ->
                    fileTree.AddOrUpdateNode path node

                    let len = int32 (endOffset - offset)
                    Marshal.Copy(buffer, fileProps.FileData, int32 offset, len)
                    bytesTransferred <- uint32 len

                    fileInfo <- getFileInfo node
                    FileSystemBase.STATUS_SUCCESS
                | Ok(FolderNode _) -> assert false; FileSystemBase.STATUS_INVALID_DEVICE_REQUEST
                | Error err -> err
            else
                let len = int32 (endOffset - offset)
                Marshal.Copy(buffer, fileProps.FileData, int32 offset, len)
                bytesTransferred <- uint32 len

                fileInfo <- getFileInfo node
                FileSystemBase.STATUS_SUCCESS
        | _ ->
            assert false
            FileSystemBase.STATUS_INVALID_DEVICE_REQUEST

    override this.Overwrite(_, fileDesc, fileAttributes,
                             replaceFileAttributes, allocationSize, fileInfo) =
         let path : string = downcast fileDesc
         Logging.logVerbose className "Overwrite" path

         let node = fileTree.FindNode path
         assert node.IsSome
         match setFileSize (node.Value, allocationSize, true) with
         | Ok(FileNode(props, fileProps)) ->
             let fileAttributes = if replaceFileAttributes then
                                      fileAttributes ||| uint32 FileAttributes.Archive
                                  else
                                      props.FileAttributes ||| fileAttributes ||| uint32 FileAttributes.Archive

             let node = FileNode ({ Name = props.Name
                                    CreatedFileTimeUtc = uint64 (DateTime.UtcNow.ToFileTimeUtc())
                                    LastModifiedFileTimeUtc = uint64 (DateTime.UtcNow.ToFileTimeUtc())
                                    FileAttributes = fileAttributes }, { fileProps with FileSize = 0UL })

             fileTree.AddOrUpdateNode path node
             fileInfo <- getFileInfo node
             FileSystemBase.STATUS_SUCCESS
         | Ok(FolderNode _) -> assert false; FileSystemBase.STATUS_INVALID_DEVICE_REQUEST
         | Error err -> err

    override this.SetFileSize(_, fileDesc, newSize, setAllocationSize, fileInfo) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "SetFileSize" path

        let node = fileTree.FindNode path
        assert node.IsSome
        match setFileSize (node.Value, newSize, setAllocationSize) with
        | Ok node ->
            fileTree.AddOrUpdateNode path node
            fileInfo <- getFileInfo node
            FileSystemBase.STATUS_SUCCESS
        | Error status -> status

    override this.Rename(_, fileDesc, fileName, newFileName, replaceIfExists) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "Rename" (sprintf "'%s' -> '%s'" path newFileName)

        assert (String.Equals(path, fileName, StringComparison.OrdinalIgnoreCase))

        match fileTree.FindNode path with
        | Some currentNode ->
            match fileTree.FindNode newFileName with
            | Some _ when not replaceIfExists ->
                FileSystemBase.STATUS_OBJECT_NAME_COLLISION
            | Some(FolderNode _) ->
                FileSystemBase.STATUS_ACCESS_DENIED
            | newNode ->
                if newNode.IsSome then
                    fileTree.RemoveNode newFileName

                fileTree.RemoveNode path

                let name = Path.GetFileName(newFileName.TrimEnd([| '/'; '\\' |]))
                let newNode = match currentNode with
                              | FileNode(props, fileProps) -> FileNode({ props with Name = name }, fileProps)
                              | FolderNode(props, children) -> FolderNode({ props with Name = name }, children)
                fileTree.AddOrUpdateNode newFileName newNode

                FileSystemBase.STATUS_SUCCESS
        | None ->
            FileSystemBase.STATUS_OBJECT_PATH_NOT_FOUND

    override this.Cleanup(_, fileDesc, _, flags) =
        let isFlagSet flag value =
            value &&& flag <> 0u

        let path : string = downcast fileDesc
        Logging.logVerbose className "Cleanup" path

        match fileTree.FindNode path with
        | Some node ->
            if isFlagSet FileSystemBase.CleanupDelete flags then
                fileTree.RemoveNode path
            else
                match node with
                | FileNode(props, fileProps) as node ->
                    let props = if isFlagSet FileSystemBase.CleanupSetArchiveBit flags then
                                    { props with FileAttributes = props.FileAttributes ||| uint32 FileAttributes.Archive }
                                else props

                    let systemTime = uint64 (DateTime.UtcNow.ToFileTimeUtc())
                    let props = if isFlagSet FileSystemBase.CleanupSetChangeTime flags then
                                    { props with LastModifiedFileTimeUtc = systemTime }
                                else props
                    let props = if isFlagSet FileSystemBase.CleanupSetLastWriteTime flags then
                                    { props with LastModifiedFileTimeUtc = systemTime }
                                else props
                    // TODO: we are not checking the lastAccessTime

                    if isFlagSet FileSystemBase.CleanupSetAllocationSize flags then
                        let allocationUnit = uint64 (MEMFS_SECTOR_SIZE * MEMFS_SECTORS_PER_ALLOCATION_UNIT)
                        let allocationSize = (fileProps.FileSize + allocationUnit - 1UL) / allocationUnit * allocationUnit
                        match setFileSize (node, allocationSize, true) with
                        | Ok(FileNode(_, fileProps)) ->
                            fileTree.AddOrUpdateNode path (FileNode(props, fileProps))
                        | Ok(FolderNode _) ->
                            assert false
                        | Error err ->
                            Logging.logError className "Cleanup" (sprintf "'%s' -> 0x%x" path err)
                    else
                        fileTree.AddOrUpdateNode path (FileNode(props, fileProps))
                | FolderNode(props, children) ->
                    let systemTime = uint64 (DateTime.UtcNow.ToFileTimeUtc())
                    let props = if isFlagSet FileSystemBase.CleanupSetChangeTime flags then
                                    { props with LastModifiedFileTimeUtc = systemTime }
                                else props
                    let props = if isFlagSet FileSystemBase.CleanupSetLastWriteTime flags then
                                    { props with LastModifiedFileTimeUtc = systemTime }
                                else props
                    let node = FolderNode(props, children)
                    fileTree.AddOrUpdateNode path node
        | None -> ()

    override this.Flush(_, fileDesc, fileInfo) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "Flush" path
        
        let node = fileTree.FindNode path
        assert node.IsSome
        fileInfo <- (getFileInfo node.Value)
        
        FileSystemBase.STATUS_SUCCESS

    override this.SetBasicInfo(_, fileDesc, fileAttributes, creationTime,
                                _, lastWriteTime, _, fileInfo) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "SetBasicInfo" path

        match fileTree.FindNode path with
        | Some(FileNode(props, _))
        | Some(FolderNode(props, _)) as node ->
            let props = {
                Name = props.Name
                CreatedFileTimeUtc = if creationTime <> 0UL then creationTime
                                     else props.CreatedFileTimeUtc
                LastModifiedFileTimeUtc = if lastWriteTime <> 0UL then lastWriteTime
                                          else props.LastModifiedFileTimeUtc
                FileAttributes = if fileAttributes <> uint32 -1 then fileAttributes else props.FileAttributes
            }
            let node = match node.Value with
                       | FileNode(_, data) -> FileNode(props, data)
                       | FolderNode(_, children) -> FolderNode(props, children)

            fileTree.AddOrUpdateNode path node
            fileInfo <- getFileInfo node
            FileSystemBase.STATUS_SUCCESS
        | None -> FileSystemBase.STATUS_INVALID_DEVICE_REQUEST

    (* ********************************************* *
     *                   MISC                        *
     * ********************************************* *)

    override this.Init(host) =
        let host = host :?> FileSystemHost
        host.SectorSize <- MEMFS_SECTOR_SIZE
        host.SectorsPerAllocationUnit <- MEMFS_SECTORS_PER_ALLOCATION_UNIT
        host.VolumeCreationTime <- uint64 (DateTime.Now.ToFileTimeUtc())
        host.VolumeSerialNumber <- uint32 (host.VolumeCreationTime / (10000UL * 1000UL));
        host.CaseSensitiveSearch <- false
        host.CasePreservedNames <- true;
        host.UnicodeOnDisk <- true;
        host.PostCleanupWhenModifiedOnly <- true;
        host.PassQueryDirectoryPattern <- true
        FileSystemBase.STATUS_SUCCESS;

    override this.ExceptionHandler(ex) =
        Logging.logError className "ExceptionHandler" (ex.ToString())
        FileSystemBase.STATUS_UNEXPECTED_IO_ERROR

    override this.GetVolumeInfo(volumeInfo) =
        Logging.logVerbose className "GetVolumeInfo" ""

        // TODO: next version
        volumeInfo.TotalSize <- maxFileNodes * uint64 maxFileSize;
        volumeInfo.FreeSize <- volumeInfo.TotalSize //(maxFileNodes - FileNodeMap.Count()) * (UInt64)MaxFileSize;
        //VolumeInfo.SetVolumeLabel(VolumeLabel);
        FileSystemBase.STATUS_SUCCESS

    override this.CanDelete(_, fileDesc, _) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "CanDelete" path

        match fileTree.FindNode path with
        | Some(FolderNode(_, children)) when children.Count > 0 ->
            FileSystemBase.STATUS_DIRECTORY_NOT_EMPTY
        | Some _ ->
            FileSystemBase.STATUS_SUCCESS
        | None ->
            FileSystemBase.STATUS_INVALID_DEVICE_REQUEST

    override this.GetSecurityByName(path, fileAttributes, _) =
        Logging.logVerbose className "GetSecurityByName" path

        match fileTree.FindNode path with
        | Some(FileNode(props, _))
        | Some(FolderNode(props, _)) ->
            fileAttributes <- uint32 props.FileAttributes
            FileSystemBase.STATUS_SUCCESS
        | None ->
            FileSystemBase.STATUS_OBJECT_NAME_NOT_FOUND

    override this.Close(_, fileDesc) =
        let path : string = downcast fileDesc
        Logging.logVerbose className "Close" path
