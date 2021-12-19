// DO NOT MODIFY THE COMMENTED LINES BELOW
// MIT License
// Copyright (c) 2022 Luciano Ciccariello
// https://github.com/Xeeynamo/OpenKh/blob/master/LICENSE

module Disassembler

open Instructions
open System.IO

[<System.Flags>]
type Flags =
    | None = 0
    | UseAlias = 1

let intsToString (n: int) =
    let mightBeDecimal = abs n < 16 || (n % 5) = 0 || (n % 3) = 0
    if mightBeDecimal then n.ToString() else
    if n >= 0 then $"0x{n:X}" else $"-0x{-n:X}"
let intuToString (n: uint) =
    let mightBeDecimal = n < 16u || (n % 5u) = 0u || (n % 3u) = 0u
    if mightBeDecimal then n.ToString() else $"0x{n:X}"

let disassembleInstr (instr: uint) (flags: Flags) =
    let unkInstr instr = $".word 0x{instr:X08}"
    let hexs (value:int) =
        if value = 0 then "" else
        if value > 0 then $"0x{value:X}" else $"-0x{-value:X}"
    let imms instr = int (int16 (instr &&& 0xffffu))
    let immu instr = uint (uint16 (instr &&& 0xffffu))
    let op = instr >>> 26 |> int |> enum<Op>
    let rs = (instr >>> 21) &&& 0x1Fu |> int |> enum<Reg>
    let rt = (instr >>> 16) &&& 0x1Fu |> int |> enum<Reg>
    let useAlias = flags.HasFlag(Flags.UseAlias)

    let special =
        let rd = (instr >>> 11) &&& 0x1Fu |> int |> enum<Reg>
        let funct = instr &&& 0x3Fu |> int |> enum<Special>
        let shamt instr = (instr >>> 6) &&& 0x1Fu |> int

        match (funct, rd, rs, rt) with
        | Special.SLL, Reg.ZERO, Reg.ZERO, Reg.ZERO when useAlias && shamt instr = 0 -> "nop"
        | Special.SUB, _, Reg.ZERO, _ when useAlias = true -> $"neg\t${string rd}, ${string rt}"
        | Special.SUBU, _, Reg.ZERO, _ when useAlias = true -> $"negu\t${string rd}, ${string rt}"
        | Special.OR, _, _, Reg.ZERO when useAlias = true -> $"move\t${string rd}, ${string rs}"
        | Special.MFHI, _, Reg.ZERO, Reg.ZERO
        | Special.MTHI, _, Reg.ZERO, Reg.ZERO
        | Special.MFLO, _, Reg.ZERO, Reg.ZERO
        | Special.MTLO, _, Reg.ZERO, Reg.ZERO -> $"{string funct}\t${string rd}"
        | Special.MULT, Reg.ZERO, _, _
        | Special.MULTU, Reg.ZERO, _, _
        | Special.DIV, Reg.ZERO, _, _
        | Special.DIVU, Reg.ZERO, _, _ -> $"{string funct}\t${string rs}, ${string rt}"
        | Special.SLL, _, Reg.ZERO, _
        | Special.SRL, _, Reg.ZERO, _
        | Special.SRA, _, Reg.ZERO, _ -> $"{string funct}\t${string rd}, ${string rt}, {shamt instr}"
        | Special.ADD, _, _, _
        | Special.SUB, _, _, _
        | Special.ADDU, _, _, _
        | Special.SUBU, _, _, _
        | Special.AND, _, _, _
        | Special.OR, _, _, _
        | Special.XOR, _, _, _
        | Special.NOR, _, _, _
        | Special.SLT, _, _, _
        | Special.SLTU, _, _, _ -> $"{string funct}\t${string rd}, ${string rs}, ${string rt}"
        | Special.SLLV, _, _, _
        | Special.SRAV, _, _, _
        | Special.SRLV, _, _, _ -> $"{string funct}\t${string rd}, ${string rt}, ${string rs}"
        | Special.JR, Reg.ZERO, _, Reg.ZERO
        | Special.JALR, Reg.ZERO, _, Reg.ZERO -> $"{string funct}\t${string rs}"
        | Special.SYSCALL, _, _, _ -> $"syscall\t0x{instr >>> 6:X}"
        | Special.BREAK, _, _, _ -> $"break\t0x{instr >>> 16:X}"
        | _, _, _, _ -> unkInstr instr
    let cop =
        let cop = uint op &&& 3u
        match rs |> int |> enum<Cop> with
        | Cop.MFC -> $"MFC{cop}\t${string rt}, ${(instr >>> 11) &&& 0x1Fu}"
        | Cop.MTC -> $"MTC{cop}\t${string rt}, ${(instr >>> 11) &&& 0x1Fu}"
        | Cop.CFC -> $"CFC{cop}\t${string rt}, ${(instr >>> 11) &&& 0x1Fu}"
        | Cop.CTC -> $"CTC{cop}\t${string rt}, ${(instr >>> 11) &&& 0x1Fu}"
        | _ -> $"COP{cop}\t0x{instr &&& 0x1FFFFFFu:X}"

    let disasm =
        match op with
        | Op.SPECIAL -> special
        | Op.LUI -> $"lui\t${string rt}, 0x{immu instr:X}"
        | Op.ADDIU when rs = Reg.ZERO && useAlias = true -> $"li\t${string rt}, {intsToString (imms instr)}"
        | Op.ORI when rs = Reg.ZERO && useAlias = true -> $"li\t${string rt}, {intuToString (immu instr)}"
        | Op.ADDI | Op.SLTI -> $"{string op}\t${string rt}, ${string rs}, {intsToString (imms instr)}"
        | Op.ADDIU | Op.SLTIU -> $"{string op}\t${string rt}, ${string rs}, {intuToString (immu instr)}"
        | Op.ANDI | Op.ORI | Op.XORI -> $"{string op}\t${string rt}, ${string rs}, 0x{immu instr:X}"
        | Op.LB | Op.LH | Op.LWL | Op.LW | Op.LBU | Op.LHU | Op.LWR
        | Op.SB | Op.SH | Op.SWL | Op.SW | Op.SWR ->
            $"{string op}\t${string rt}, {hexs (imms instr)}(${string rs})"
        | Op.J | Op.JAL -> $"{string op}\t0x{(instr &&& 0x3FFFFFFu) <<< 2:X}"
        | Op.LWC0 | Op.LWC1 | Op.LWC2| Op.LWC3
        | Op.SWC0 | Op.SWC1 | Op.SWC2| Op.SWC3 ->
            $"{string op}\t${int rt}, {hexs (imms instr)}(${string rs})"
        | Op.C0 | Op.C1 | Op.C2 | Op.C3 -> cop
        | _ -> unkInstr instr

    disasm.ToLower()

let disassembleSeq (instrs: seq<uint>) (flags: Flags) =
    let disasm x = disassembleInstr x flags
    instrs |> Seq.map disasm

let disassembleStream (reader: BinaryReader) (instrCount: int) (flags: Flags) =
    disassembleSeq (Seq.init instrCount (fun _ -> reader.ReadUInt32())) flags

let disassembleStreamRange (reader: BinaryReader) (offsetStart: uint) (offsetEnd: uint) (flags: Flags) =
    let s = reader.BaseStream.Seek(int64 offsetStart, SeekOrigin.Begin)
    let e = (min (int64 offsetEnd) reader.BaseStream.Length)
    disassembleStream reader (int (e - s) / 4) flags
