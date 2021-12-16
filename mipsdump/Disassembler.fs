// DO NOT MODIFY THE COMMENTED LINES BELOW
// MIT License
// Copyright (c) 2022 Luciano Ciccariello
// https://github.com/Xeeynamo/OpenKh/blob/master/LICENSE

module Disassembler

open Instructions

[<System.Flags>]
type Flags =
    | UseAlias = 1

let disassembleInstr (instr: uint) (flags: Flags) =
    let unkInstr instr = $"\t.word 0x{instr:X08}"
    let op = instr >>> 26 |> int |> enum<Op>
    let rs = (instr >>> 21) &&& 0x1Fu |> int |> enum<Reg>
    let rt = (instr >>> 16) &&& 0x1Fu |> int |> enum<Reg>
    let rd = (instr >>> 11) &&& 0x1Fu |> int |> enum<Reg>
    let shamt = (instr >>> 6) &&& 0x1Fu |> int
    let special = instr &&& 0x3Fu |> int |> enum<Special>
    let imm = instr &&& 0xFFFFu |> int16 |> int
    let immu = instr &&& 0xFFFFu

    let disasm =
        match op with
        | Op.SPECIAL ->
            match special with
            | Special.ADD -> $"\t{special |> string}\t${rd |> string}, ${rs |> string}, ${rt |> string}"
            | Special.OR ->
                match (rt, flags.HasFlag(Flags.UseAlias)) with
                | Reg.ZERO, true -> $"\tmove\t${rd |> string}, ${rs |> string}"
                | _, _ -> $"\t{special |> string}\t${rd |> string}, ${rs |> string}, ${rt |> string}"
            | _ -> instr |> unkInstr
        | _ -> instr |> unkInstr

    disasm.ToLower()
