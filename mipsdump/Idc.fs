module Idc

open System
open System.Globalization

let (|Hexadecimal|Decimal|Unknown|) (str: string) =
    let isHexDigit (ch: char) = Char.IsDigit(ch) || (ch >= 'A' && ch <= 'F') || (ch >= 'a' && ch <= 'f')
    if str.Length > 2 && str[..1].ToLower() = "0x" then
        if str[2..] |> String.forall(isHexDigit) then
            Hexadecimal str[2..]
        else
            Unknown
    else
        if str |> String.forall(Char.IsDigit) then
            Decimal
        else
            Unknown
                
let asDigit (str:string) =
    match str with
    | Hexadecimal hexStr -> Some (UInt32.Parse (hexStr, NumberStyles.HexNumber))
    | Decimal -> Some (UInt32.Parse str)
    | Unknown -> None

let asString (str:string) =
    if str.Length >= 2 && str[0] = '"' && str[^0] = '"' then
        Some str[1..^1]
    else
        None

let parseLabels (content:string[]) =
    let (|SetName|_|) (tokens:string[]) =
        if tokens.Length < 3 then None else
        match (tokens[0], asDigit tokens[1], asString tokens[2]) with
        | "set_name", Some addr, Some label -> Some (addr, label)
        | _, _, _ -> None
    let (|CreateData|_|) (tokens:string[]) =
        if tokens.Length < 2 then None else
        match (tokens[0], asDigit tokens[1]) with
        | "create_dword", Some addr -> Some (addr, $"word_{addr:x08}")
        | "create_word", Some addr -> Some (addr, $"half_{addr:x08}")
        | "create_byte", Some addr -> Some (addr, $"byte_{addr:x08}")
        | "make_array", Some addr -> Some (addr, $"array_{addr:x08}")
        | "create_strlit", Some addr -> Some (addr, $"asc_{addr:x08}")
        | _, _ -> None
    let parseIdcLine (line:string) =
        let tokens =
            line.Split('\t', ' ', ',', '(', ')', ';')
            |> Array.filter(fun token -> token.Length > 0)
        match tokens with
        | SetName (addr, label) -> Some (addr, label)
        | CreateData (addr, label) -> Some (addr, label)
        | _ -> None
    content |> Array.choose parseIdcLine |> Map.ofArray
