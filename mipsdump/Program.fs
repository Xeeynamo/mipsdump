open System
open System.Globalization
open System.IO
open Argu
open Disassembler

type CliArguments =
    | [<Unique>] [<Mandatory>] [<First>] Input of exePath:string
    | [<Unique>] Output of outPath:string
    | [<Unique>] Start of s:string
    | [<Unique>] End of e:string
    | [<Unique>] Idc of idcPath:string
    | [<Unique>] Base_Address of baseAddress:string
    | [<Unique>] Use_Alias
    | [<Unique>] No_Branch_Analysis

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "executable file compiled in MIPS machine code to disassemble"
            | Output _ -> "output file that will contain the disassembled code\nprints to stdout when not specified"
            | Start _ -> "start file offset in hexadecimal\nwhen not specified starts from the beginning of the file"
            | End _ -> "end file offset in hexadecimal\nwhen not specified then disassemble until the end of the file"
            | Idc _ -> "IDC file path from IDA Pro\nuseful if you have user-defined function labels"
            | Base_Address _ -> "virtual memory start address in hexadecimal\nfor PS1 executable it is recommended to specify a value of 8000F800\nby default it is assigned to 80010000"
            | Use_Alias _ -> "replace some instructions with aliases to make the disassembly easier to read"
            | No_Branch_Analysis _ -> "do not scan assembly to create labels\nonly used for fast-produced raw disassembly"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "mipsdump")
    let parseHex (s: string) = UInt32.Parse (s, NumberStyles.HexNumber)
    try
        let parsed = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let exePath = parsed.GetResult Input
        use stream = File.OpenRead(exePath)
        let labels =
            if parsed.Contains Idc then
                let idcPath = parsed.GetResult Idc
                Idc.parseLabels (File.ReadAllLines(idcPath))
            else Map.empty
        let flags =
            if parsed.Contains Use_Alias then Flags.UseAlias else Flags.None
            ||| if parsed.Contains No_Branch_Analysis then Flags.NoBranchAnalysis else Flags.None
        let baseAddress = if parsed.Contains Base_Address then (parseHex (parsed.GetResult Base_Address)) else 0x80010000u
        let s = if parsed.Contains Start then (parseHex (parsed.GetResult Start)) else 0u
        let e = if parsed.Contains End then (parseHex (parsed.GetResult Start)) else (uint stream.Length)
        
        use reader = new BinaryReader (stream)
        let disasm = Disassembler.disassembleStreamRange reader s e baseAddress labels flags
        if parsed.Contains Output then
            let outPath = parsed.GetResult Output
            File.WriteAllLines(outPath, disasm)
        else for line in disasm do printfn "%s" line
        0
    with e ->
        eprintfn "%s" e.Message
        1
