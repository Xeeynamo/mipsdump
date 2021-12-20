// DO NOT MODIFY THE COMMENTED LINES BELOW
// MIT License
// Copyright (c) 2022 Luciano Ciccariello
// https://github.com/Xeeynamo/OpenKh/blob/master/LICENSE

module Disassembler

open Instructions
open System
open System.IO

[<System.Flags>]
type Flags =
    | None = 0
    | UseAlias = 1
    | NoBranchAnalysis = 2

let intsToString (n: int) =
    let mightBeDecimal = abs n < 16 || (n % 5) = 0 || (n % 3) = 0
    if mightBeDecimal then n.ToString() else
    if n >= 0 then $"0x{n:X}" else $"-0x{-n:X}"
let intuToString (n: uint) =
    let mightBeDecimal = n < 16u || (n % 5u) = 0u || (n % 3u) = 0u
    if mightBeDecimal then n.ToString() else $"0x{n:X}"
let imms instr = int (int16 (instr &&& 0xffffu))
let immu instr = uint (uint16 (instr &&& 0xffffu))

let disassembleInstr (instr: uint) (addr: uint) (labels: Map<uint32, string>) (flags: Flags) =
    let strlow (value: Enum) = (string value).ToLower()
    let unkInstr instr = $".word 0x{instr:x08}"
    let hexs (value:int) = if value >= 0 then $"0x{value:x}" else $"-0x{-value:x}"
    let hexp (value:int) = if value = 0 then "" else hexs value
    let ofBranch (imm: int) =
        let relAddr = (imm + 1) <<< 2
        let absAddr = addr + (uint relAddr)
        match labels.TryGetValue absAddr with
        | true, label -> label
        | false, _ -> hexs ((imm + 1) <<< 1)
    let ofLabel addr =
        match labels.TryGetValue(addr) with
        | true, label -> label
        | false, _ -> $"0x{addr:x08}"
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
        | Special.SUB, _, Reg.ZERO, _ when useAlias = true -> $"neg\t${strlow rd}, ${strlow rt}"
        | Special.SUBU, _, Reg.ZERO, _ when useAlias = true -> $"negu\t${strlow rd}, ${strlow rt}"
        | Special.OR, _, _, Reg.ZERO when useAlias = true -> $"move\t${strlow rd}, ${strlow rs}"
        | Special.MFHI, _, Reg.ZERO, Reg.ZERO
        | Special.MTHI, _, Reg.ZERO, Reg.ZERO
        | Special.MFLO, _, Reg.ZERO, Reg.ZERO
        | Special.MTLO, _, Reg.ZERO, Reg.ZERO -> $"{strlow funct}\t${strlow rd}"
        | Special.MULT, Reg.ZERO, _, _
        | Special.MULTU, Reg.ZERO, _, _
        | Special.DIV, Reg.ZERO, _, _
        | Special.DIVU, Reg.ZERO, _, _ -> $"{strlow funct}\t${strlow rs}, ${strlow rt}"
        | Special.SLL, _, Reg.ZERO, _
        | Special.SRL, _, Reg.ZERO, _
        | Special.SRA, _, Reg.ZERO, _ -> $"{strlow funct}\t${strlow rd}, ${strlow rt}, {shamt instr}"
        | Special.ADD, _, _, _
        | Special.SUB, _, _, _
        | Special.ADDU, _, _, _
        | Special.SUBU, _, _, _
        | Special.AND, _, _, _
        | Special.OR, _, _, _
        | Special.XOR, _, _, _
        | Special.NOR, _, _, _
        | Special.SLT, _, _, _
        | Special.SLTU, _, _, _ -> $"{strlow funct}\t${strlow rd}, ${strlow rs}, ${strlow rt}"
        | Special.SLLV, _, _, _
        | Special.SRAV, _, _, _
        | Special.SRLV, _, _, _ -> $"{strlow funct}\t${strlow rd}, ${strlow rt}, ${strlow rs}"
        | Special.JR, Reg.ZERO, _, Reg.ZERO
        | Special.JALR, Reg.ZERO, _, Reg.ZERO -> $"{strlow funct}\t${strlow rs}"
        | Special.SYSCALL, _, _, _ -> $"syscall\t0x{instr >>> 6:X}"
        | Special.BREAK, _, _, _ -> $"break\t0x{instr >>> 16:X}"
        | _, _, _, _ -> unkInstr instr
    let regimm =
        let regimm = rt |> int |> enum<Regimm>
        match regimm with
        | Regimm.BLTZ | Regimm.BGEZ | Regimm.BLTZAL | Regimm.BGEZAL ->
            $"{strlow regimm}\t${strlow rs}, {ofBranch (imms instr)}"
        | _ -> unkInstr instr
    let cop =
        let cop = uint op &&& 3u
        match rs |> int |> enum<Cop> with
        | Cop.MFC -> $"mfc{cop}\t${strlow rt}, ${(instr >>> 11) &&& 0x1Fu}"
        | Cop.MTC -> $"mtc{cop}\t${strlow rt}, ${(instr >>> 11) &&& 0x1Fu}"
        | Cop.CFC -> $"cfc{cop}\t${strlow rt}, ${(instr >>> 11) &&& 0x1Fu}"
        | Cop.CTC -> $"ctc{cop}\t${strlow rt}, ${(instr >>> 11) &&& 0x1Fu}"
        | _ -> $"cop{cop}\t0x{instr &&& 0x1FFFFFFu:x}"

    match op with
    | Op.SPECIAL -> special
    | Op.REGIMM -> regimm
    | Op.BEQ | Op.BNE -> $"{strlow op}\t${strlow rs}, ${strlow rt}, {ofBranch (imms instr)}"
    | Op.BLEZ | Op.BGTZ -> $"{strlow op}\t${strlow rs}, {ofBranch (imms instr)}"
    | Op.LUI -> $"lui\t${strlow rt}, 0x{immu instr:X}"
    | Op.ADDIU when rs = Reg.ZERO && useAlias = true -> $"li\t${strlow rt}, {intsToString (imms instr)}"
    | Op.ORI when rs = Reg.ZERO && useAlias = true -> $"li\t${strlow rt}, {intuToString (immu instr)}"
    | Op.ADDI | Op.SLTI -> $"{strlow op}\t${strlow rt}, ${strlow rs}, {intsToString (imms instr)}"
    | Op.ADDIU | Op.SLTIU -> $"{strlow op}\t${strlow rt}, ${strlow rs}, {intuToString (immu instr)}"
    | Op.ANDI | Op.ORI | Op.XORI -> $"{strlow op}\t${strlow rt}, ${strlow rs}, 0x{immu instr:x}"
    | Op.LB | Op.LH | Op.LWL | Op.LW | Op.LBU | Op.LHU | Op.LWR
    | Op.SB | Op.SH | Op.SWL | Op.SW | Op.SWR ->
        $"{strlow op}\t${strlow rt}, {hexp (imms instr)}(${strlow rs})"
    | Op.J | Op.JAL -> $"{strlow op}\t{ofLabel (((instr &&& 0x3FFFFFFu) <<< 2) ||| 0x80000000u)}"
    | Op.LWC0 | Op.LWC1 | Op.LWC2| Op.LWC3
    | Op.SWC0 | Op.SWC1 | Op.SWC2| Op.SWC3 ->
        $"{strlow op}\t${int rt}, {hexp (imms instr)}(${strlow rs})"
    | Op.C0 | Op.C1 | Op.C2 | Op.C3 -> cop
    | _ -> unkInstr instr

let analyzeBranches (instrs: uint[]) (baseAddr: uint) (labels: Map<uint32, string>) =
    let addLabel (instr: uint) (addr: uint) (labels: Map<uint32, string>) =
        let relAddr = (imms instr + 1) <<< 2
        let absAddr = uint (addr + (uint relAddr))
        if labels.ContainsKey(absAddr) = false then
            labels.Add(uint absAddr, $"loc_{absAddr:x}")
        else labels
    let addJumpLabel (instr: uint) (labels: Map<uint32, string>) =
        let absAddr = uint ((instr &&& 0x3FFFFFFu) <<< 2) ||| 0x80000000u
        if labels.ContainsKey(absAddr) = false then
            labels.Add(uint absAddr, $"sub_{absAddr:x}")
        else labels
        
    let mutable curAddr = baseAddr
    let mutable moreLabels = labels
    for instr in instrs do
        let op = instr >>> 26 |> int |> enum<Op>
        moreLabels <-
            match op with
            | Op.BEQ | Op.BNE | Op.BLEZ | Op.BGTZ -> addLabel instr curAddr moreLabels
            | Op.REGIMM ->
                let regimm = (instr >>> 16) &&& 0x1Fu |> int |> enum<Regimm>
                match regimm with
                | Regimm.BLTZ | Regimm.BGEZ | Regimm.BLTZAL | Regimm.BGEZAL -> addLabel instr curAddr moreLabels
                | _ -> moreLabels
            | Op.J | Op.JAL -> addJumpLabel instr moreLabels
            | _ -> moreLabels
        curAddr <- curAddr + 4u
    moreLabels

let disassembleData (instrs: uint[]) (baseAddr: uint32) (labels: Map<uint32, string>) (flags: Flags) =
    let moreLabels =
        if flags.HasFlag(Flags.NoBranchAnalysis)
        then labels
        else analyzeBranches instrs baseAddr labels
    seq {
        for i in 0..(instrs.Length - 1) do
            let addr = baseAddr + (uint i <<< 2)
            let strDisasm = disassembleInstr instrs[i] addr moreLabels flags
            yield!
                match moreLabels.TryGetValue addr with
                | true, label -> [|$"\n{label}:"; $"\t{strDisasm}"|]
                | false, _ -> [|$"\t{strDisasm}"|]
    }

let disassembleStream (reader: BinaryReader) (instrCount: int) (baseAddr: uint32) (labels: Map<uint32, string>) (flags: Flags) =
    disassembleData (Array.init instrCount (fun _ -> reader.ReadUInt32())) baseAddr labels flags

let disassembleStreamRange (reader: BinaryReader) (offsetStart: uint) (offsetEnd: uint) (baseAddr: uint32) (labels: Map<uint32, string>) (flags: Flags) =
    let s = reader.BaseStream.Seek(int64 offsetStart, SeekOrigin.Begin)
    let e = (min (int64 offsetEnd) reader.BaseStream.Length)
    disassembleStream reader (int (e - s) / 4) (baseAddr + offsetStart) labels flags
