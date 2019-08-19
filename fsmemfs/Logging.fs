module LowLevelDesign.FsMemFs.Logging

open System.Diagnostics

let Logger = TraceSource("applogger", SourceLevels.Verbose)

Logger.Listeners.Remove("default")
Logger.Listeners.Add(new ConsoleTraceListener()) |> ignore

let private buildMessage source action message =
    sprintf "%s [%s] %s" source action message

let private logEvent (evt : TraceEventType) source action message =
    let log = buildMessage source action message
    Logger.TraceEvent(evt, 0, log)

let logVerbose = logEvent TraceEventType.Verbose
    
let logInfo = logEvent TraceEventType.Information

let logWarning = logEvent TraceEventType.Warning

let logError = logEvent TraceEventType.Error

let logCritical = logEvent TraceEventType.Critical

