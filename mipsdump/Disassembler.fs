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
            | Special.MFHI | Special.MTHI | Special.MFLO | Special.MTLO ->
                $"\t{special |> string}\t${rd |> string}"
            | Special.MULT | Special.MULTU | Special.DIV | Special.DIVU ->
                $"\t{special |> string}\t${rs |> string}, ${rt |> string}"
            | Special.SLL | Special.SRL | Special.SRA ->
                $"\t{special |> string}\t${rd |> string}, ${rt |> string}, {shamt}"
            | Special.ADD | Special.ADDU | Special.AND
            | Special.XOR | Special.NOR | Special.SLT | Special.SLTU ->
                $"\t{special |> string}\t${rd |> string}, ${rs |> string}, ${rt |> string}"
            | Special.SLLV | Special.SRAV | Special.SRLV ->
                $"\t{special |> string}\t${rd |> string}, ${rt |> string}, ${rs |> string}"
            | Special.SUB ->
                match (rs, flags.HasFlag(Flags.UseAlias)) with
                | Reg.ZERO, true -> $"\tneg\t${rd |> string}, ${rt |> string}"
                | _, _ -> $"\t{special |> string}\t${rd |> string}, ${rs |> string}, ${rt |> string}"
            | Special.SUBU ->
                match (rs, flags.HasFlag(Flags.UseAlias)) with
                | Reg.ZERO, true -> $"\tnegu\t${rd |> string}, ${rt |> string}"
                | _, _ -> $"\t{special |> string}\t${rd |> string}, ${rs |> string}, ${rt |> string}"
            | Special.OR ->
                match (rt, flags.HasFlag(Flags.UseAlias)) with
                | Reg.ZERO, true -> $"\tmove\t${rd |> string}, ${rs |> string}"
                | _, _ -> $"\t{special |> string}\t${rd |> string}, ${rs |> string}, ${rt |> string}"
            | Special.SYSCALL ->
                $"\t{special |> string}\t0x{instr >>> 6:X}"
            | Special.BREAK ->
                $"\t{special |> string}\t0x{instr >>> 16:X}"
            | _ -> instr |> unkInstr
        | _ -> instr |> unkInstr

    disasm.ToLower()
