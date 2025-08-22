module LogLevels

let message (logLine: string): string =
    logLine.Substring(logLine.IndexOf " ").Trim()

let logLevel(logLine: string): string =
    logLine.Substring(0, logLine.IndexOf(" ")).Replace("[","").Replace("]","").Replace(":","").ToLower()

let reformat(logLine: string): string =
    $"{(message logLine)} ({(logLevel logLine)})"  
