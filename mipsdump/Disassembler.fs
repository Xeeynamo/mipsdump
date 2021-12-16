// DO NOT MODIFY THE COMMENTED LINES BELOW
// MIT License
// Copyright (c) 2022 Luciano Ciccariello
// https://github.com/Xeeynamo/OpenKh/blob/master/LICENSE

module Disassembler

open Instructions

[<System.Flags>]
type Flags =
    | None = 0
    | UseAlias = 1

let disassembleInstr (instr: uint) (flags: Flags) =
    let unkInstr instr = $".word 0x{instr:X08}"
    let op = instr >>> 26 |> int |> enum<Op>
    let rs = (instr >>> 21) &&& 0x1Fu |> int |> enum<Reg>
    let rt = (instr >>> 16) &&& 0x1Fu |> int |> enum<Reg>

    let special =
        let rd = (instr >>> 11) &&& 0x1Fu |> int |> enum<Reg>
        let funct = instr &&& 0x3Fu |> int |> enum<Special>
        let shamt = (instr >>> 6) &&& 0x1Fu |> int

        match (funct, rd, rs, rt, flags.HasFlag(Flags.UseAlias)) with
        | Special.SLL, Reg.ZERO, Reg.ZERO, Reg.ZERO, true when shamt = 0 -> "nop"
        | Special.SUB, _, Reg.ZERO, _, true -> $"neg\t${rd |> string}, ${rt |> string}"
        | Special.SUBU, _, Reg.ZERO, _, true -> $"negu\t${rd |> string}, ${rt |> string}"
        | Special.OR, _, _, Reg.ZERO, true -> $"move\t${rd |> string}, ${rs |> string}"
        | Special.MFHI, _, Reg.ZERO, Reg.ZERO, _
        | Special.MTHI, _, Reg.ZERO, Reg.ZERO, _
        | Special.MFLO, _, Reg.ZERO, Reg.ZERO, _
        | Special.MTLO, _, Reg.ZERO, Reg.ZERO, _ -> $"{funct |> string}\t${rd |> string}"
        | Special.MULT, Reg.ZERO, _, _, _
        | Special.MULTU, Reg.ZERO, _, _, _
        | Special.DIV, Reg.ZERO, _, _, _
        | Special.DIVU, Reg.ZERO, _, _, _ -> $"{funct |> string}\t${rs |> string}, ${rt |> string}"
        | Special.SLL, _, Reg.ZERO, _, _
        | Special.SRL, _, Reg.ZERO, _, _
        | Special.SRA, _, Reg.ZERO, _, _ -> $"{funct |> string}\t${rd |> string}, ${rt |> string}, {shamt}"
        | Special.ADD, _, _, _, _
        | Special.SUB, _, _, _, _
        | Special.ADDU, _, _, _, _
        | Special.SUBU, _, _, _, _
        | Special.AND, _, _, _, _
        | Special.OR, _, _, _, _
        | Special.XOR, _, _, _, _
        | Special.NOR, _, _, _, _
        | Special.SLT, _, _, _, _
        | Special.SLTU, _, _, _, _ -> $"{funct |> string}\t${rd |> string}, ${rs |> string}, ${rt |> string}"
        | Special.SLLV, _, _, _, _
        | Special.SRAV, _, _, _, _
        | Special.SRLV, _, _, _, _ -> $"{funct |> string}\t${rd |> string}, ${rt |> string}, ${rs |> string}"
        | Special.SYSCALL, _, _, _, _ -> $"syscall\t0x{instr >>> 6:X}"
        | Special.BREAK, _, _, _, _ -> $"break\t0x{instr >>> 16:X}"
        | _, _, _, _, _ -> instr |> unkInstr

    let disasm =
        match op with
        | Op.SPECIAL -> special
        | Op.ADDI | Op.SLTI ->
            let imms = int16 (instr &&& 0xffffu)
            $"{op |> string}\t${rt |> string}, ${rs |> string}, {imms}"
        | Op.ADDIU | Op.SLTIU | Op.ANDI | Op.ORI | Op.XORI ->
            let immu = uint16 (instr &&& 0xffffu)
            $"{op |> string}\t${rt |> string}, ${rs |> string}, {immu}"
        | _ -> instr |> unkInstr

    $"\t{disasm}".ToLower()
