# MIPS dump

An experimental MIPS decompiler written in F#.

This is not yet in an usable state.

## Why?

I did not like the output from `mips-linux-gnu-objdump` as it needed some corrections to be compiled again by `mips-linux-gnu-as` and it lacked labels support from binary files.

IDA Pro and Ghidra also produces an output that I am not a fan of, but as it contains labels I am adding support to parse an IDC file to import all the offsets.

Finally I never worked on a F# project so far, so this might be a good opportunity to learn with hands on.

## Features

This is perfectly compatible with MIPS-I processors, used for example by PlayStation 1 video-games.

### Inline compiler

Allows to generate MIPS-compatible binary code:

C-style function:

```c
int sum(int a, int b)
{
    return a + b;
}
```

Functionally identical using the inline compiler:

```fsharp
open Instructions
open type Instructions.Reg

let sum = [|
	JR RA;
	ADD V0 A0 A1;
	|]
```

Some instructions do not accept large numbers. For instance `addi` accepts a 16-bit signed number, so you need to pass a `int16` to let the compiler to be happy:

```fsharp
let n = int16 -123
let op1 = ADDI A0 A1 n
let op2 = ADDIU V0 A0 (uint16 123)
```

### Decompile single instruction

`Instructions.disassembleInstr` will decompile a single `uint32` into assembly code. It also accepts [flags](#customise-the-disassembly-with-flags).

For example, `disassembleInstr 0x00851021u Flags.None` will produce `addu $v0, $a0, $a1`.

### Customise the disassembly with flags

* `UseAlias`: instructions such as `or $v0, $a0, $zero` are print as `move $v0, $a0`

## Resources

The following [MIPS Instruction Set reference by Charles Price](https://www.cs.cmu.edu/afs/cs/academic/class/15740-f97/public/doc/mips-isa.pdf) had everything I needed to know to build this tool.

## Many thanks

Thanks to my friend [fatim](https://github.com/fatim) who introduced me in F# and that helped me to start this journey :)
