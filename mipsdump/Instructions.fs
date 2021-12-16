// DO NOT MODIFY THE COMMENTED LINES BELOW
// MIT License
// Copyright (c) 2022 Luciano Ciccariello
// https://github.com/Xeeynamo/OpenKh/blob/master/LICENSE

module public Instructions

type Reg =
    | ZERO = 0x00 | AT = 0x01 | V0 = 0x02 | V1 = 0x03
    | A0 = 0x04 | A1 = 0x05 | A2 = 0x06 | A3 = 0x07
    | T0 = 0x08 | T1 = 0x09 | T2 = 0x0A | T3 = 0x0B
    | T4 = 0x0C | T5 = 0x0D | T6 = 0x0E | T7 = 0x0F
    | S0 = 0x10 | S1 = 0x11 | S2 = 0x12 | S3 = 0x13
    | S4 = 0x14 | S5 = 0x15 | S6 = 0x16 | S7 = 0x17
    | T8 = 0x18 | T9 = 0x19 | K0 = 0x1A | K1 = 0x1B
    | GP = 0x1C | SP = 0x1D | FP = 0x1E | RA = 0x1F
type Regimm =
    | BLTZ = 0x00 | BGEZ = 0x01 | BLTZAL = 0x10 | BGEZAL = 0x11
type Op =
    | SPECIAL = 0x00 | REGIMM = 0x01 | J = 0x02 | JAL = 0x03
    | BEQ = 0x04 | BNE = 0x05 | BLEZ = 0x06 | BGTZ = 0x07
    | ADDI = 0x08 | ADDIU = 0x09 | SLTI = 0x0A | SLTIU = 0x0B
    | ANDI = 0x0C | ORI = 0x0D | XORI = 0x0E | LUI = 0x0F
    | MFC0 = 0x10 | MFC1 = 0x11 | MFC2 = 0x12 | MFC3 = 0x13
    | LB = 0x20 | LH = 0x21 | LWL = 0x22 | LW = 0x23
    | LBU = 0x24 | LHU = 0x25 | LWR = 0x26
    | SB = 0x28 | SH = 0x29 | SWL = 0x2a | SW = 0x2b
    | SWR = 0x2e
    | LWC0 = 0x30 | LWC1 = 0x31 | LWC2 = 0x32 | LWC3 = 0x33
    | SWC0 = 0x38 | SWC1 = 0x39 | SWC2 = 0x3a | SWC3 = 0x3b
type Special =
    | SLL = 0x00 | SRL = 0x02 | SRA = 0x03
    | SLLV = 0x04 | SRLV = 0x06 | SRAV = 0x07
    | JR = 0x08 | JALR = 0x09
    | SYSCALL = 0x0C | BREAK = 0x0D
    | MFHI = 0x10 | MTHI = 0x11 | MFLO = 0x12 | MTLO = 0x13
    | MULT = 0x18 | MULTU = 0x19 | DIV = 0x1A | DIVU = 0x1B
    | ADD = 0x20 | ADDU = 0x21 | SUB = 0x22 | SUBU = 0x23
    | AND = 0x24 | OR = 0x25 | XOR = 0x26 | NOR = 0x27
    | SLT = 0x2A | SLTU = 0x2B

let s16to32 (value:int16) = (value |> uint) &&& 0xFFFFu
let u16to32 (value:uint16) = (value |> uint) &&& 0xFFFFu

// R-type instructions
let SLL (dst:Reg) (src:Reg) (value:uint) = uint Special.SLL ||| (uint dst <<< 11) ||| (uint src <<< 16) ||| ((value &&& 0x1Fu) <<< 6)
let SRL (dst:Reg) (src:Reg) (value:uint) = uint Special.SRL ||| (uint dst <<< 11) ||| (uint src <<< 16) ||| ((value &&& 0x1Fu) <<< 6)
let SRA (dst:Reg) (src:Reg) (value:uint) = uint Special.SRA ||| (uint dst <<< 11) ||| (uint src <<< 16) ||| ((value &&& 0x1Fu) <<< 6)
let SLLV (dst:Reg) (left:Reg) (right:Reg)  = uint Special.SLLV ||| (uint dst <<< 11) ||| (uint left <<< 16) ||| (uint right <<< 21)
let SRAV (dst:Reg) (left:Reg) (right:Reg)  = uint Special.SRAV ||| (uint dst <<< 11) ||| (uint left <<< 16) ||| (uint right <<< 21)
let SRLV (dst:Reg) (left:Reg) (right:Reg)  = uint Special.SRLV ||| (uint dst <<< 11) ||| (uint left <<< 16) ||| (uint right <<< 21)
let JR (reg:Reg) = uint Special.JR ||| (uint (reg:Reg) <<< 21)
let JALR (reg:Reg) = uint Special.JALR ||| (uint (reg:Reg) <<< 21)
let SYSCALL (code:uint) = uint Special.SYSCALL ||| ((code &&& 0xffffffu) <<< 6)
let BREAK (code:uint) = uint Special.BREAK ||| ((code &&& 0x3ffu) <<< 16)
let MFHI (reg:Reg) = uint Special.MFHI ||| (uint (reg:Reg) <<< 11)
let MTHI (reg:Reg) = uint Special.MTHI ||| (uint (reg:Reg) <<< 11)
let MFLO (reg:Reg) = uint Special.MFLO ||| (uint (reg:Reg) <<< 11)
let MTLO (reg:Reg) = uint Special.MTLO ||| (uint (reg:Reg) <<< 11)
let MULT (dst:Reg) (src:Reg) = uint Special.MULT ||| (uint src <<< 16) ||| (uint dst <<< 21)
let MULTU (dst:Reg) (src:Reg) = uint Special.MULTU ||| (uint src <<< 16) ||| (uint dst <<< 21)
let DIV (dst:Reg) (src:Reg) = uint Special.DIV ||| (uint src <<< 16) ||| (uint dst <<< 21)
let DIVU (dst:Reg) (src:Reg) = uint Special.DIVU ||| (uint src <<< 16) ||| (uint dst <<< 21)
let ADD (dst:Reg) (left:Reg) (right:Reg) = uint Special.ADD ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let ADDU (dst:Reg) (left:Reg) (right:Reg) = uint Special.ADDU ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let SUB (dst:Reg) (left:Reg) (right:Reg) = uint Special.SUB ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let SUBU (dst:Reg) (left:Reg) (right:Reg) = uint Special.SUBU ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let AND (dst:Reg) (left:Reg) (right:Reg) = uint Special.AND ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let OR (dst:Reg) (left:Reg) (right:Reg) = uint Special.OR ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let XOR (dst:Reg) (left:Reg) (right:Reg) = uint Special.XOR ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let NOR (dst:Reg) (left:Reg) (right:Reg) = uint Special.NOR ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let SLT (dst:Reg) (left:Reg) (right:Reg) = uint Special.SLT ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)
let SLTU (dst:Reg) (left:Reg) (right:Reg) = uint Special.SLTU ||| (uint dst <<< 11) ||| (uint left <<< 21) ||| (uint right <<< 16)

// Alias R-type instructions
let NOP = SLL Reg.ZERO Reg.ZERO 0u
let MOVE(dst:Reg) (src:Reg) = OR dst src Reg.ZERO
let NEG(dst:Reg) (src:Reg) = SUB dst Reg.ZERO src
let NEGU(dst:Reg) (src:Reg) = SUBU dst Reg.ZERO src

// I-type instructions
let BLTZ (reg:Reg) (imm:int16) = s16to32 imm ||| (uint Regimm.BLTZ <<< 16) ||| (uint reg <<< 21) ||| (uint Op.REGIMM <<< 26)
let BGEZ (reg:Reg) (imm:int16) = s16to32 imm ||| (uint Regimm.BGEZ <<< 16) ||| (uint reg <<< 21) ||| (uint Op.REGIMM <<< 26)
let BLTZAL (reg:Reg) (imm:int16) = s16to32 imm ||| (uint Regimm.BLTZAL <<< 16) ||| (uint reg <<< 21) ||| (uint Op.REGIMM <<< 26)
let BGEZAL (reg:Reg) (imm:int16) = s16to32 imm ||| (uint Regimm.BGEZAL <<< 16) ||| (uint reg <<< 21) ||| (uint Op.REGIMM <<< 26)
let BEQ (left:Reg) (right:Reg) (imm:int16) = s16to32 imm ||| (uint right <<< 16) ||| (uint left <<< 21) ||| (uint Op.BEQ <<< 26)
let BNE (left:Reg) (right:Reg) (imm:int16) = s16to32 imm ||| (uint right <<< 16) ||| (uint left <<< 21) ||| (uint Op.BNE <<< 26)
let BLEZ (reg:Reg) (imm:int16) = s16to32 imm ||| (uint reg <<< 21) ||| (uint Op.BLEZ <<< 26)
let BGTZ (reg:Reg) (imm:int16) = s16to32 imm ||| (uint reg <<< 21) ||| (uint Op.BGTZ <<< 26)
let ADDI (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.ADDI <<< 26)
let ADDIU (dst:Reg) (src:Reg) (imm:uint16) = u16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.ADDIU <<< 26)
let SLTI (left:Reg) (right:Reg) (imm:int16) = s16to32 imm ||| (uint left <<< 16) ||| (uint right <<< 21) ||| (uint Op.SLTI <<< 26)
let SLTIU (left:Reg) (right:Reg) (imm:int16) = s16to32 imm ||| (uint left <<< 16) ||| (uint right <<< 21) ||| (uint Op.SLTIU <<< 26)
let ANDI (dst:Reg) (src:Reg) (imm:uint16) = u16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.ANDI <<< 26)
let ORI (dst:Reg) (src:Reg) (imm:uint16) = u16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.ORI <<< 26)
let XORI (dst:Reg) (src:Reg) (imm:uint16) = u16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.XORI <<< 26)
let LUI (dst:Reg) (imm:uint16) = u16to32 imm ||| (uint dst <<< 16) ||| (uint Op.LUI <<< 26)
let LB (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LB <<< 26)
let LH (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LH <<< 26)
let LWL (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LWL <<< 26)
let LW (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LW <<< 26)
let LBU (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LBU <<< 26)
let LHU (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LHU <<< 26)
let LWR (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LWR <<< 26)
let SB (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SB <<< 26)
let SH (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SH <<< 26)
let SWL (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SWL <<< 26)
let SW (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SW <<< 26)
let SWR (dst:Reg) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SWR <<< 26)

// Alias I-type instructions
let BAL (reg:Reg) (imm:int16) = BGEZAL reg imm
let LI (dst:Reg) (imm:int16) = ADDIU dst Reg.ZERO (uint16 imm)
let LIU (dst:Reg) (imm:uint16) = ORI dst Reg.ZERO (uint16 imm)

// Co-processor instructions
let MFC0 (dst:Reg) (src:uint) = ((src &&& 0x1Fu) <<< 11) ||| (uint dst <<< 16) ||| (uint Op.MFC0 <<< 26)
let MFC1 (dst:Reg) (src:uint) = ((src &&& 0x1Fu) <<< 11) ||| (uint dst <<< 16) ||| (uint Op.MFC1 <<< 26)
let MFC2 (dst:Reg) (src:uint) = ((src &&& 0x1Fu) <<< 11) ||| (uint dst <<< 16) ||| (uint Op.MFC2 <<< 26)
let MFC3 (dst:Reg) (src:uint) = ((src &&& 0x1Fu) <<< 11) ||| (uint dst <<< 16) ||| (uint Op.MFC3 <<< 26)
let LWC0 (dst:uint) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LWC0 <<< 26)
let LWC1 (dst:uint) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LWC1 <<< 26)
let LWC2 (dst:uint) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LWC2 <<< 26)
let LWC3 (dst:uint) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.LWC3 <<< 26)
let SWC0 (dst:uint) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SWC0 <<< 26)
let SWC1 (dst:uint) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SWC1 <<< 26)
let SWC2 (dst:uint) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SWC2 <<< 26)
let SWC3 (dst:uint) (src:Reg) (imm:int16) = s16to32 imm ||| (uint dst <<< 16) ||| (uint src <<< 21) ||| (uint Op.SWC3 <<< 26)
let BC0F (offset:int16) = s16to32 offset ||| (0x10u <<< 26) ||| (0x10u <<< 20) ||| (0u <<< 16)
let BC0T (offset:int16) = s16to32 offset ||| (0x10u <<< 26) ||| (0x10u <<< 20) ||| (1u <<< 16)
let BC1F (offset:int16) = s16to32 offset ||| (0x11u <<< 26) ||| (0x10u <<< 20) ||| (0u <<< 16)
let BC1T (offset:int16) = s16to32 offset ||| (0x11u <<< 26) ||| (0x10u <<< 20) ||| (1u <<< 16)
let BC2F (offset:int16) = s16to32 offset ||| (0x12u <<< 26) ||| (0x10u <<< 20) ||| (0u <<< 16)
let BC2T (offset:int16) = s16to32 offset ||| (0x12u <<< 26) ||| (0x10u <<< 20) ||| (1u <<< 16)
let BC3F (offset:int16) = s16to32 offset ||| (0x13u <<< 26) ||| (0x10u <<< 20) ||| (0u <<< 16)
let BC3T (offset:int16) = s16to32 offset ||| (0x13u <<< 26) ||| (0x10u <<< 20) ||| (1u <<< 16)
let COP0 (offset:uint32) = (offset &&& 0x1FFFFFFu) ||| (1u <<< 25) ||| (0x10u <<< 26)
let COP1 (offset:uint32) = (offset &&& 0x1FFFFFFu) ||| (1u <<< 25) ||| (0x11u <<< 26)
let COP2 (offset:uint32) = (offset &&& 0x1FFFFFFu) ||| (1u <<< 25) ||| (0x12u <<< 26)
let COP3 (offset:uint32) = (offset &&& 0x1FFFFFFu) ||| (1u <<< 25) ||| (0x13u <<< 26)

// J-type instructions
let J (addr:uint) = (addr &&& 0x3FFFFFFu) ||| (uint Op.J <<< 26)
let JAL (addr:uint) = (addr &&& 0x3FFFFFFu) ||| (uint Op.JAL <<< 26)
