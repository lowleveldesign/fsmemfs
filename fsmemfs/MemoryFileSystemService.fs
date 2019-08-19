namespace LowLevelDesign.FsMemFs

open Fsp
open System
open System.Diagnostics
open System.IO
open System.Reflection

exception CommandLineError of string

type ServiceArgs = {
    MountPoint : string
    EnableVerboseLog : bool
    LogPath : string option
    DriverLogPath : string option
 }

type MemoryFileSystemService() =
    inherit Service("FsMemFsService")

    let className = typedefof<MemoryFileSystemService>.Name
    let logger = TraceSource(className)

    let mutable host : FileSystemHost = null

    let parseArgs (args : string array) =
        let args = args |> Array.collect (fun a -> a.Split()) |> List.ofArray

        let rec parseArg (lastArg : string) (args : string list) =
            let isArg (a : string) = a.StartsWith("-", StringComparison.Ordinal)
            let extractArgName (a : string) = a.TrimStart('-')

            match args with
            | [] ->
                Map.empty<string, string option>
            | a :: rest when isArg a && lastArg = "" ->
                parseArg (extractArgName a) rest
            | a :: rest when isArg a ->
                let m = parseArg (extractArgName a) rest
                m |> Map.add lastArg None
            | a :: rest ->
                let m = parseArg "" rest
                m |> Map.add lastArg (Some a)

        parseArg "" args

    let parseAndValidate args =
        let getArgVal (args : Map<string, string option>) key =
            if not (args.ContainsKey(key)) || args.[key].IsNone then None
            else args.[key]

        let parsedArgs = parseArgs args
        if (getArgVal parsedArgs "m").IsNone then
            raise (CommandLineError "Mountpoint (-m) is missing.")

        {
            MountPoint = parsedArgs.["m"].Value
            EnableVerboseLog = parsedArgs.ContainsKey("v")
            LogPath = getArgVal parsedArgs "l"
            DriverLogPath = getArgVal parsedArgs "D"
        }

    override this.OnStart(args : string array) =
        try
            let msg = sprintf "Starting service, args: %A" args
            logger.TraceInformation(msg)

            let args = parseAndValidate args
            let debugFlags = if args.EnableVerboseLog then uint32 -1 else 0u
            let switch = if args.EnableVerboseLog then SourceSwitch("default", Level = SourceLevels.Verbose)
                         else SourceSwitch("default", Level = SourceLevels.Information)
            Logging.Logger.Switch <- switch
            
            if args.LogPath.IsSome then
                Logging.Logger.Listeners.Add(new TextWriterTraceListener(args.LogPath.Value)) |> ignore
            
            if args.DriverLogPath.IsSome then
                if FileSystemHost.SetDebugLogFile(args.DriverLogPath.Value) < 0 then
                    raise (CommandLineError "Cannot open driver log file.")
            
            host <- new FileSystemHost(MemoryFileSystem())

            if host.Mount(args.MountPoint, null, true, debugFlags) < 0 then
                raise (IOException("Mounting file system failed"))
        with
        | CommandLineError err ->
            Service.Log(Service.EVENTLOG_ERROR_TYPE, (sprintf "fsmemfs: %s" err))
            Service.Log(Service.EVENTLOG_INFORMATION_TYPE,
                        sprintf "F# Memory File System v%s based on WinFsp"
                            (Assembly.GetExecutingAssembly().GetName().Version.ToString()))

            let message = """
Copyright (C) 2019 Sebastian Solnica (lowleveldesign.org)

Fsmemfs uses WinFsp (WinFsp - Windows File System Proxy, Copyright (C) Bill Zissimopoulos) to mount
the user-mode file system. You need to have it installed in order to run and build fsmemfs.
Check the WinFsp GitHub page (https://github.com/billziss-gh/winfsp) to learn more and download the installer.

Usage: fsmemfs OPTIONS

Options:
  -m VALUE     [required] mount point, could be a drive (G:) or a folder (C:\memfs)
  -v           [optional] enable verbose log
  -l VALUE     [optional] a path to a file where fsmemfs should write logs
  -D VALUE     [optional] a path to a file where WinFsp should write debug logs
"""
            Service.Log(Service.EVENTLOG_ERROR_TYPE, message)
            reraise()

    interface IDisposable with
        member this.Dispose() =
            if host <> null then
                host.Dispose()
