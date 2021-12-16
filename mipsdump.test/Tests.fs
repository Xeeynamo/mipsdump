// DO NOT MODIFY THE COMMENTED LINES BELOW
// MIT License
// Copyright (c) 2022 Luciano Ciccariello
// https://github.com/Xeeynamo/OpenKh/blob/master/LICENSE

module Tests

open Xunit
open Instructions
open type Instructions.Reg
open Disassembler

[<Fact>]
let ``Assemble instructions correctly`` () =
    // R-type instructions
    Assert.Equal(0x001afec0u, SLL RA K0 0x1bu)
    Assert.Equal(0x001afec2u, SRL RA K0 0x1bu)
    Assert.Equal(0x001afec3u, SRA RA K0 0x1bu)
    Assert.Equal(0x037af804u, SLLV RA K0 K1)
    Assert.Equal(0x037af806u, SRLV RA K0 K1)
    Assert.Equal(0x037af807u, SRAV RA K0 K1)
    Assert.Equal(0x03e00008u, JR RA)
    Assert.Equal(0x03e00009u, JALR RA)
    Assert.Equal(0x03fb72ccu, SYSCALL 0xfedcbu)
    Assert.Equal(0x03cc000du, BREAK 0x3ccu)
    Assert.Equal(0x0000f810u, MFHI RA)
    Assert.Equal(0x0000f811u, MTHI RA)
    Assert.Equal(0x0000f812u, MFLO RA)
    Assert.Equal(0x0000f813u, MTLO RA)
    Assert.Equal(0x03fa0018u, MULT RA K0)
    Assert.Equal(0x03fa0019u, MULTU RA K0)
    Assert.Equal(0x03fa001Au, DIV RA K0)
    Assert.Equal(0x03fa001Bu, DIVU RA K0)
    Assert.Equal(0x035bf820u, ADD RA K0 K1)
    Assert.Equal(0x035bf821u, ADDU RA K0 K1)
    Assert.Equal(0x035bf822u, SUB RA K0 K1)
    Assert.Equal(0x035bf823u, SUBU RA K0 K1)
    Assert.Equal(0x035bf824u, AND RA K0 K1)
    Assert.Equal(0x035bf825u, OR RA K0 K1)
    Assert.Equal(0x035bf826u, XOR RA K0 K1)
    Assert.Equal(0x035bf827u, NOR RA K0 K1)
    Assert.Equal(0x035bf82au, SLT RA K0 K1)
    Assert.Equal(0x035bf82bu, SLTU RA K0 K1)
    
    // Alias R-type instructions
    Assert.Equal(0x00000000u, NOP)
    Assert.Equal(0x0340f825u, MOVE RA K0)
    Assert.Equal(0x001af822u, NEG RA K0)
    Assert.Equal(0x001af823u, NEGU RA K0)
    
    // I-type instructions
    Assert.Equal(0x07503333u, BLTZAL K0 (int16 0x3333))
    Assert.Equal(0x07513333u, BGEZAL K0 (int16 0x3333))
    Assert.Equal(0x07e03333u, BLTZ RA (int16 0x3333))
    Assert.Equal(0x07e13333u, BGEZ RA (int16 0x3333))

    Assert.Equal(0x13fa3333u, BEQ RA K0 (int16 0x3333))
    Assert.Equal(0x17fa3333u, BNE RA K0 (int16 0x3333))
    Assert.Equal(0x1be03333u, BLEZ RA (int16 0x3333))
    Assert.Equal(0x1fe03333u, BGTZ RA (int16 0x3333))
    Assert.Equal(0x235f3333u, ADDI RA K0 (int16 0x3333))
    Assert.Equal(0x275f3333u, ADDIU RA K0 (uint16 0x3333))
    Assert.Equal(0x2b5f3333u, SLTI RA K0 (int16 0x3333))
    Assert.Equal(0x2f5f3333u, SLTIU RA K0 (int16 0x3333))
    Assert.Equal(0x335f3333u, ANDI RA K0 (uint16 0x3333))
    Assert.Equal(0x375f3333u, ORI RA K0 (uint16 0x3333))
    Assert.Equal(0x3B5f3333u, XORI RA K0 (uint16 0x3333))
    Assert.Equal(0x3c1fccccu, LUI RA (uint16 0xcccc))
    Assert.Equal(0x835f3333u, LB RA K0 (int16 0x3333))
    Assert.Equal(0x875f3333u, LH RA K0 (int16 0x3333))
    Assert.Equal(0x8B5f3333u, LWL RA K0 (int16 0x3333))
    Assert.Equal(0x8F5f3333u, LW RA K0 (int16 0x3333))
    Assert.Equal(0x935f3333u, LBU RA K0 (int16 0x3333))
    Assert.Equal(0x975f3333u, LHU RA K0 (int16 0x3333))
    Assert.Equal(0x9B5f3333u, LWR RA K0 (int16 0x3333))
    
    Assert.Equal(0xa35f3333u, SB RA K0 (int16 0x3333))
    Assert.Equal(0xa75f3333u, SH RA K0 (int16 0x3333))
    Assert.Equal(0xab5f3333u, SWL RA K0 (int16 0x3333))
    Assert.Equal(0xaf5f3333u, SW RA K0 (int16 0x3333))
    Assert.Equal(0xbb5f3333u, SWR RA K0 (int16 0x3333))
    
    // Alias I-type instructions
    Assert.Equal(0x07513333u, BAL K0 (int16 0x3333))
    Assert.Equal(0x241f7fffu, LI RA (int16 0x7fff))
    Assert.Equal(0x341fccccu, LIU RA (uint16 0xcccc))

    // J-type instructions
    Assert.Equal(0x0bccddeeu, J 0x3ccddeeu)
    Assert.Equal(0x0fccddeeu, JAL 0x3ccddeeu)

    // Co-processor instructions
    Assert.Equal(0x401fd000u, MFC0 RA 26u)
    Assert.Equal(0x441fd000u, MFC1 RA 26u)
    Assert.Equal(0x481fd000u, MFC2 RA 26u)
    Assert.Equal(0x4c1fd000u, MFC3 RA 26u)
    Assert.Equal(0xc3fa3333u, LWC0 26u RA (int16 0x3333))
    Assert.Equal(0xc7fa3333u, LWC1 26u RA (int16 0x3333))
    Assert.Equal(0xcbfa3333u, LWC2 26u RA (int16 0x3333))
    Assert.Equal(0xcffa3333u, LWC3 26u RA (int16 0x3333))
    Assert.Equal(0xe3fa3333u, SWC0 26u RA (int16 0x3333))
    Assert.Equal(0xe7fa3333u, SWC1 26u RA (int16 0x3333))
    Assert.Equal(0xebfa3333u, SWC2 26u RA (int16 0x3333))
    Assert.Equal(0xeffa3333u, SWC3 26u RA (int16 0x3333))
    Assert.Equal(0x41003333u, BC0F (int16 0x3333))
    Assert.Equal(0x41013333u, BC0T (int16 0x3333))
    Assert.Equal(0x45003333u, BC1F (int16 0x3333))
    Assert.Equal(0x45013333u, BC1T (int16 0x3333))
    Assert.Equal(0x49003333u, BC2F (int16 0x3333))
    Assert.Equal(0x49013333u, BC2T (int16 0x3333))
    Assert.Equal(0x4d003333u, BC3F (int16 0x3333))
    Assert.Equal(0x4d013333u, BC3T (int16 0x3333))
    Assert.Equal(0x43234567u, COP0 0x1234567u)
    Assert.Equal(0x47234567u, COP1 0x1234567u)
    Assert.Equal(0x4a123456u, COP2 0x123456u)
    Assert.Equal(0x4e123456u, COP3 0x123456u)

let noFlags = Flags.None
let useAlias = Flags.UseAlias

[<Fact>]
let ``Disassemble basic instructions`` () =
    Assert.Equal("sll\t$v0, $a0, 6", disassembleInstr (SLL V0 A0 6u) noFlags)
    Assert.Equal("srl\t$v0, $a0, 12", disassembleInstr (SRL V0 A0 12u) noFlags)
    Assert.Equal("sra\t$v0, $a0, 18", disassembleInstr (SRA V0 A0 18u) noFlags)
    Assert.Equal("sllv\t$v0, $a0, $a1", disassembleInstr (SLLV V0 A0 A1) noFlags)
    Assert.Equal("srav\t$v0, $a0, $a1", disassembleInstr (SRAV V0 A0 A1) noFlags)
    Assert.Equal("srlv\t$v0, $a0, $a1", disassembleInstr (SRLV V0 A0 A1) noFlags)
    Assert.Equal("syscall\t0x123", disassembleInstr (SYSCALL 0x123u) noFlags)
    Assert.Equal("break\t0x123", disassembleInstr (BREAK 0x123u) noFlags)
    Assert.Equal("mfhi\t$v0", disassembleInstr (MFHI V0) noFlags)
    Assert.Equal("mthi\t$v0", disassembleInstr (MTHI V0) noFlags)
    Assert.Equal("mflo\t$v0", disassembleInstr (MFLO V0) noFlags)
    Assert.Equal("mtlo\t$v0", disassembleInstr (MTLO V0) noFlags)
    Assert.Equal("mult\t$v0, $a0", disassembleInstr (MULT V0 A0) noFlags)
    Assert.Equal("multu\t$v0, $a0", disassembleInstr (MULTU V0 A0) noFlags)
    Assert.Equal("div\t$v0, $a0", disassembleInstr (DIV V0 A0) noFlags)
    Assert.Equal("divu\t$v0, $a0", disassembleInstr (DIVU V0 A0) noFlags)
    Assert.Equal("add\t$v0, $a0, $a1", disassembleInstr (ADD V0 A0 A1) noFlags)
    Assert.Equal("addu\t$v0, $a0, $a1", disassembleInstr (ADDU V0 A0 A1) noFlags)
    Assert.Equal("sub\t$v0, $a0, $a1", disassembleInstr (SUB V0 A0 A1) noFlags)
    Assert.Equal("subu\t$v0, $a0, $a1", disassembleInstr (SUBU V0 A0 A1) noFlags)
    Assert.Equal("and\t$v0, $a0, $a1", disassembleInstr (AND V0 A0 A1) noFlags)
    Assert.Equal("or\t$v0, $a0, $a1", disassembleInstr (OR V0 A0 A1) noFlags)
    Assert.Equal("xor\t$v0, $a0, $a1", disassembleInstr (XOR V0 A0 A1) noFlags)
    Assert.Equal("nor\t$v0, $a0, $a1", disassembleInstr (NOR V0 A0 A1) noFlags)
    Assert.Equal("slt\t$v0, $a0, $a1", disassembleInstr (SLT V0 A0 A1) noFlags)
    Assert.Equal("sltu\t$v0, $a0, $a1", disassembleInstr (SLTU V0 A0 A1) noFlags)

    Assert.Equal("addi\t$v0, $a0, -3", disassembleInstr (ADDI V0 A0 (int16 -3)) noFlags)
    Assert.Equal("addiu\t$v0, $a0, 50000", disassembleInstr (ADDIU V0 A0 (uint16 50000)) noFlags)
    Assert.Equal("slti\t$v0, $a0, 3", disassembleInstr (SLTI V0 A0 (int16 3)) noFlags)
    Assert.Equal("sltiu\t$v0, $a0, 3", disassembleInstr (SLTIU V0 A0 (int16 3)) noFlags)
    Assert.Equal("andi\t$v0, $a0, 3", disassembleInstr (ANDI V0 A0 (uint16 3)) noFlags)
    Assert.Equal("ori\t$v0, $a0, 3", disassembleInstr (ORI V0 A0 (uint16 3)) noFlags)
    Assert.Equal("xori\t$v0, $a0, 3", disassembleInstr (XORI V0 A0 (uint16 3)) noFlags)

[<Fact>]
let ``Disassemble instructions with aliases`` () =
    Assert.Equal("sll\t$zero, $zero, 0", disassembleInstr NOP noFlags)
    Assert.Equal("nop", disassembleInstr NOP useAlias)

    Assert.Equal("or\t$v0, $a0, $zero", disassembleInstr (MOVE V0 A0) noFlags)
    Assert.Equal("move\t$v0, $a0", disassembleInstr (MOVE V0 A0) useAlias)

    Assert.Equal("sub\t$v0, $zero, $a0", disassembleInstr (NEG V0 A0) noFlags)
    Assert.Equal("neg\t$v0, $a0", disassembleInstr (NEG V0 A0) useAlias)

    Assert.Equal("subu\t$v0, $zero, $a0", disassembleInstr (NEGU V0 A0) noFlags)
    Assert.Equal("negu\t$v0, $a0", disassembleInstr (NEGU V0 A0) useAlias)
    
[<Fact>]
let ``Disassemble unknown instructions`` () =
    Assert.Equal(".word 0xffffffff", disassembleInstr (0xffffffffu) noFlags)
