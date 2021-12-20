# mipsdump

A functional MIPS disassembler written in F# that produces 1:1 assembly code that can be compiled with GNU's `as`.

## Why?

I did not like the output from `mips-linux-gnu-objdump` as it needed some corrections to be compiled again by `mips-linux-gnu-as` and it lacked labels support from binary files.

IDA and Ghidra also produces an output that I am not a fan of, but as it contains labels I am adding support to parse an IDC file to import all the offsets.

Finally I never worked on a F# project before this one, so this was be a good opportunity to learn with hands on.

## Compile from the source code

The tool can be compiled and used from any operating system (Windows, Linux, macOS) and from any processor architecture (x86, x64, ARM, ARM64).

1. Install the latest version of [.NET Core SDK](https://dotnet.microsoft.com/download)
1. Download the source code and nagivate to the folder `mipsdump` from your terminal
1. Run `dotnet publish -c Release`
1. The generated program will now be located at `./bin/Release/net6.0/publish`

## Usage

Once you have the executable, invoke the following command on your terminal to receive help on all the current available features:

```shell
./mipsdump --help
```

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

`Disassembler.disassembleInstr` will decompile a single `uint32` into assembly code. It also accepts [flags](#customise-the-disassembly-with-flags).

For example, `disassembleInstr 0x00851021u 0x0u Map.Empty Flags.None` will produce `addu $v0, $a0, $a1`.

### Generate labels

By default the program will scan the assembly for branches and jumps to generate labels, useful to have branching in the code more readable.

Without labels generation:

```asm
    sw    $zero, ($s0)
    addiu $s0, $s0, -4
    bnez  $a0, -4
    addiu $a0, $a0, -1
```

With labels generation:

```asm
loc_80010914:
    sw    $zero, ($s0)
    addiu $s0, $s0, -4
    bnez  $a0, loc_80010914
    addiu $a0, $a0, -1
```

### Import labels

IDA is one of the most famous products to disassemble and decompile softwares. You can create rename generated labels and export the database as IDC and share it across your team or internet. This tool is able to parse the IDC file to capture the user-defined labels.

### Customise the disassembly with flags

* `UseAlias`: instructions such as `or $v0, $a0, $zero` are print as `move $v0, $a0`
* `NoBranchAnalysis`: disable generation of labels

## Resources

The following [MIPS Instruction Set reference by Charles Price](https://www.cs.cmu.edu/afs/cs/academic/class/15740-f97/public/doc/mips-isa.pdf) had everything I needed to know to build this tool.

## Many thanks

Thanks to my friend [fatim](https://github.com/fatim) who introduced me in F# and that helped me to start this journey :)
