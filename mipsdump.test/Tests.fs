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
let ``Assemble instructions`` () =
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
    Assert.Equal(0x03cc8d0du, BREAK 0x3ccu 0x234u)
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
    Assert.Equal(0x4084a000u, MTC0 A0 20u)
    Assert.Equal(0x4484a000u, MTC1 A0 20u)
    Assert.Equal(0x4884a000u, MTC2 A0 20u)
    Assert.Equal(0x4c84a000u, MTC3 A0 20u)
    Assert.Equal(0x4044a000u, CFC0 A0 20u)
    Assert.Equal(0x4444a000u, CFC1 A0 20u)
    Assert.Equal(0x4844a000u, CFC2 A0 20u)
    Assert.Equal(0x4c44a000u, CFC3 A0 20u)
    Assert.Equal(0x40c4a000u, CTC0 A0 20u)
    Assert.Equal(0x44c4a000u, CTC1 A0 20u)
    Assert.Equal(0x48c4a000u, CTC2 A0 20u)
    Assert.Equal(0x4cc4a000u, CTC3 A0 20u)
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

let labels = [|(0x80010df4u, "TestFunction")|] |> Map.ofArray
let assertDisasm (instr: uint) (expected: string) =
    Assert.Equal(expected, disassembleInstr instr 0u labels Flags.UseAlias)

[<Fact>]
let ``Basic instructions`` () =
    assertDisasm (SLL V0 A0 6u) "sll\t$v0, $a0, 6"
    assertDisasm (SRL V0 A0 12u) "srl\t$v0, $a0, 12"
    assertDisasm (SRA V0 A0 18u) "sra\t$v0, $a0, 18"
    assertDisasm (SLLV V0 A0 A1) "sllv\t$v0, $a0, $a1"
    assertDisasm (SRAV V0 A0 A1) "srav\t$v0, $a0, $a1"
    assertDisasm (SRLV V0 A0 A1) "srlv\t$v0, $a0, $a1"
    assertDisasm (SYSCALL 0x123u) "syscall\t0x123"
    assertDisasm (BREAK 0x123u 0x222u) "break\t0x123, 0x222"
    assertDisasm (BREAK 0x123u 0x222u) "break\t0x123, 0x222"
    assertDisasm (MFHI V0) "mfhi\t$v0"
    assertDisasm (MTHI V0) "mthi\t$v0"
    assertDisasm (MFLO V0) "mflo\t$v0"
    assertDisasm (MTLO V0) "mtlo\t$v0"
    assertDisasm (MULT V0 A0) "mult\t$v0, $a0"
    assertDisasm (MULTU V0 A0) "multu\t$v0, $a0"
    assertDisasm (DIV V0 A0) "div\t$zero, $v0, $a0"
    assertDisasm (DIVU V0 A0) "divu\t$zero, $v0, $a0"
    assertDisasm (ADD V0 A0 A1) "add\t$v0, $a0, $a1"
    assertDisasm (ADDU V0 A0 A1) "addu\t$v0, $a0, $a1"
    assertDisasm (SUB V0 A0 A1) "sub\t$v0, $a0, $a1"
    assertDisasm (SUBU V0 A0 A1) "subu\t$v0, $a0, $a1"
    assertDisasm (AND V0 A0 A1) "and\t$v0, $a0, $a1"
    assertDisasm (OR V0 A0 A1) "or\t$v0, $a0, $a1"
    assertDisasm (XOR V0 A0 A1) "xor\t$v0, $a0, $a1"
    assertDisasm (NOR V0 A0 A1) "nor\t$v0, $a0, $a1"
    assertDisasm (SLT V0 A0 A1) "slt\t$v0, $a0, $a1"
    assertDisasm (SLTU V0 A0 A1) "sltu\t$v0, $a0, $a1"
    assertDisasm (JR RA) "jr\t$ra"
    assertDisasm (JALR RA) "jalr\t$ra"

    assertDisasm (ADDI V0 A0 (int16 -3)) "addi\t$v0, $a0, -3"
    assertDisasm (ADDIU V0 A0 (uint16 50000)) "addiu\t$v0, $a0, 50000"
    assertDisasm (SLTI V0 A0 (int16 3)) "slti\t$v0, $a0, 3"
    assertDisasm (SLTIU V0 A0 (int16 3)) "sltiu\t$v0, $a0, 3"
    assertDisasm (ANDI V0 A0 (uint16 3)) "andi\t$v0, $a0, 0x3"
    assertDisasm (ORI V0 A0 (uint16 3)) "ori\t$v0, $a0, 0x3"
    assertDisasm (XORI V0 A0 (uint16 3)) "xori\t$v0, $a0, 0x3"
    assertDisasm(LUI S0 (uint16 100)) "lui\t$s0, 0x64"

[<Fact>]
let ``Instructions with pointer`` () =
    assertDisasm (LB V0 S0 (int16 100)) "lb\t$v0, 0x64($s0)"
    assertDisasm (LH V0 S0 (int16 -100)) "lh\t$v0, -0x64($s0)"
    assertDisasm (LWL V0 S0 (int16 100)) "lwl\t$v0, 0x64($s0)"
    assertDisasm (LW V0 S0 (int16 100)) "lw\t$v0, 0x64($s0)"
    assertDisasm (LBU V0 S0 (int16 100)) "lbu\t$v0, 0x64($s0)"
    assertDisasm (LHU V0 S0 (int16 100)) "lhu\t$v0, 0x64($s0)"
    assertDisasm (LWR V0 S0 (int16 100)) "lwr\t$v0, 0x64($s0)"
    assertDisasm (SB V0 S0 (int16 100)) "sb\t$v0, 0x64($s0)"
    assertDisasm (SH V0 S0 (int16 -100)) "sh\t$v0, -0x64($s0)"
    assertDisasm (SWL V0 S0 (int16 100)) "swl\t$v0, 0x64($s0)"
    assertDisasm (SW V0 S0 (int16 100)) "sw\t$v0, 0x64($s0)"
    assertDisasm (SWR V0 S0 (int16 0)) "swr\t$v0, ($s0)"

[<Fact>]
let ``Coprocessor instructions`` () =
    assertDisasm (MFC0 V0 20u) "mfc0\t$v0, $20"
    assertDisasm (MFC1 V0 20u) "mfc1\t$v0, $20"
    assertDisasm (MFC2 V0 20u) "mfc2\t$v0, $20"
    assertDisasm (MFC3 V0 20u) "mfc3\t$v0, $20"
    assertDisasm (MTC0 V0 20u) "mtc0\t$v0, $20"
    assertDisasm (MTC1 V0 20u) "mtc1\t$v0, $20"
    assertDisasm (MTC2 V0 20u) "mtc2\t$v0, $20"
    assertDisasm (MTC3 V0 20u) "mtc3\t$v0, $20"
    assertDisasm (CFC0 V0 20u) "cfc0\t$v0, $20"
    assertDisasm (CFC1 V0 20u) "cfc1\t$v0, $20"
    assertDisasm (CFC2 V0 20u) "cfc2\t$v0, $20"
    assertDisasm (CFC3 V0 20u) "cfc3\t$v0, $20"
    assertDisasm (CTC0 V0 20u) "ctc0\t$v0, $20"
    assertDisasm (CTC1 V0 20u) "ctc1\t$v0, $20"
    assertDisasm (CTC2 V0 20u) "ctc2\t$v0, $20"
    assertDisasm (CTC3 V0 20u) "ctc3\t$v0, $20"
    assertDisasm (LWC0 20u A0 (int16 0x20)) "lwc0\t$20, 0x20($a0)"
    assertDisasm (LWC1 20u A0 (int16 0x20)) "lwc1\t$20, 0x20($a0)"
    assertDisasm (LWC2 20u A0 (int16 0x20)) "lwc2\t$20, 0x20($a0)"
    assertDisasm (LWC3 20u A0 (int16 0x20)) "lwc3\t$20, 0x20($a0)"
    assertDisasm (SWC0 20u A0 (int16 0x20)) "swc0\t$20, 0x20($a0)"
    assertDisasm (SWC1 20u A0 (int16 0x20)) "swc1\t$20, 0x20($a0)"
    assertDisasm (SWC2 20u A0 (int16 0x20)) "swc2\t$20, 0x20($a0)"
    assertDisasm (SWC3 20u A0 (int16 -0x20)) "swc3\t$20, -0x20($a0)"
    assertDisasm (COP0 0u) "cop0\t0x0"
    assertDisasm (COP1 0x80000u) "cop1\t0x80000"
    assertDisasm (COP2 0x1FFFFFFu) "cop2\t0x1ffffff"
    assertDisasm (COP3 0x2000000u) "cop3\t0x0"

[<Fact>]
let ``Jump instructions`` () =
    assertDisasm (J 1u) "j\t0x80000004"
    assertDisasm (J 0x3FFFFFFu) "j\t0x8ffffffc"
    assertDisasm (JAL 0x140u) "jal\t0x80000500"

    assertDisasm (JAL 0x437Du) "jal\tTestFunction"

[<Fact>]
let ``Branch instructions`` () =
    assertDisasm (BEQ A0 S0 (int16 -1)) "beq\t$a0, $s0, 0x0"
    assertDisasm (BNE A0 S0 (int16 -1)) "bne\t$a0, $s0, 0x0"

    assertDisasm (BLEZ A0 (int16 -1)) "blez\t$a0, 0x0"
    assertDisasm (BGTZ A0 (int16 -1)) "bgtz\t$a0, 0x0"

    assertDisasm (BLTZ A0 (int16 -1)) "bltz\t$a0, 0x0"
    assertDisasm (BGEZ A0 (int16 -1)) "bgez\t$a0, 0x0"
    assertDisasm (BLTZAL A0 (int16 -1)) "bltzal\t$a0, 0x0"
    assertDisasm (BGEZAL A0 (int16 -1)) "bgezal\t$a0, 0x0"

    assertDisasm (BEQ A0 S0 (int16 -3)) "beq\t$a0, $s0, -0x4"
    assertDisasm (BEQ A0 S0 (int16 -5)) "beq\t$a0, $s0, -0x8"
    assertDisasm (BEQ A0 S0 (int16 -7)) "beq\t$a0, $s0, -0xc"
    assertDisasm (BEQ A0 S0 (int16 1)) "beq\t$a0, $s0, 0x4"
    assertDisasm (BEQ A0 S0 (int16 3)) "beq\t$a0, $s0, 0x8"

[<Fact>]
let ``Use aliases`` () =
    let assertNoAliasDisasm (instr: uint) (expected: string) =
        Assert.Equal(expected, disassembleInstr instr 0u Map.empty Flags.None)
    let assertAliasDisasm (instr: uint) (expectedNoAlias: string) (expectedAlias: string) =
        assertNoAliasDisasm instr expectedNoAlias
        assertDisasm instr expectedAlias

    assertAliasDisasm NOP "sll\t$zero, $zero, 0" "nop"
    assertAliasDisasm (MOVE V0 A0) "or\t$v0, $a0, $zero" "move\t$v0, $a0"
    assertAliasDisasm (NEG V0 A0) "sub\t$v0, $zero, $a0" "neg\t$v0, $a0"
    assertAliasDisasm (NEGU V0 A0) "subu\t$v0, $zero, $a0" "negu\t$v0, $a0"
    assertAliasDisasm (LIU S0 (uint16 50000)) "ori\t$s0, $zero, 0xc350" "li\t$s0, 50000"
    assertAliasDisasm (BEQ S0 ZERO (int16 1)) "beq\t$s0, $zero, 0x4" "beqz\t$s0, 0x4"
    assertAliasDisasm (BNE S0 ZERO (int16 1)) "bne\t$s0, $zero, 0x4" "bnez\t$s0, 0x4"

    assertNoAliasDisasm (LI S0 (int16 50000)) "addiu\t$s0, $zero, 50000"
    assertDisasm (LI S0 (int16 -5000)) "li\t$s0, -5000"

[<Fact>]
let ``Analyze branches and disassemble`` () =
    let data = [|
        NOP
        BLEZ A0 (int16 -1)
    |]
    let disasm =
        (disassembleData data 0x80010000u Map.empty Flags.UseAlias)
        |> String.concat "\n"
    Assert.Equal("\tnop\n\nloc_80010004:\n\tblez\t$a0, loc_80010004", disasm)

[<Fact>]
let ``Analyze jumps and disassemble`` () =
    let data = [|
        NOP
        J (0x80010000u >>> 2)
    |]
    let disasm =
        (disassembleData data 0x80010000u Map.empty Flags.UseAlias)
        |> String.concat "\n"
    Assert.Equal("\nsub_80010000:\n\tnop\n\tj\tsub_80010000", disasm)

[<Fact>]
let ``Unknown instructions`` () =
    assertDisasm 0xffffffffu ".word 0xffffffff"
    assertDisasm 0x45584520u ".word 0x45584520"
    assertDisasm 0x000a2964u ".word 0x000a2964"
    assertDisasm 0x3d657571u ".word 0x3d657571"
    assertDisasm 0x44547465u ".word 0x44547465"

[<Theory>]
[<InlineData(1, "1")>]
[<InlineData(2, "2")>]
[<InlineData(3, "3")>]
[<InlineData(4, "4")>]
[<InlineData(8, "8")>]
[<InlineData(10, "10")>]
[<InlineData(0x10, "0x10")>]
[<InlineData(20, "20")>]
[<InlineData(0x20, "0x20")>]
[<InlineData(100, "100")>]
[<InlineData(0x80, "0x80")>]
[<InlineData(0x100, "0x100")>]
[<InlineData(500, "500")>]
[<InlineData(5000, "5000")>]
[<InlineData(55, "55")>]
[<InlineData(33, "33")>]
let ``Convert digit as a string`` (value: int) (expected: string) =
    Assert.Equal(expected, intsToString value)
    Assert.Equal($"-{expected}", intsToString -value)