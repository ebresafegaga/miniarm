# Miniarm
A simple arm emulator 


# Building & Running

## With Nix (Recommended)
The easiest way to build it is with [Nix](https://nixos.org/).

If you don't have Nix installed, you can install it using the following: 

```bash
$ sh <(curl -L https://nixos.org/nix/install) --no-daemon
```

See [here](https://nixos.org/download.html) for more details about the installation.

Once you have Nix installed you should be able to run this: 
```bash 
$ nix-shell
```

This will drop you into a shell which has all the build dependecies.

Now you can build the project using the following command: 

```bash
$ dune build
```

You will find the executable program at `_build/default/bin/main.exe`

## With Opam 
If you're an OCaml user, you can install dependecies with [opam](https://opam.ocaml.org/).

You can install dependecies using the following: 

```bash
$ opam install . --deps-only --with-doc --with-test
```

And build the project with this: 

```bash 
$ opam exec -- dune build
```

You will find the executable program at `_build/default/bin/main.exe`

# Features 
This is a *very* simple arm32 emulator. Right now it only supports the following instructions: 
* `add`
* `sub`
* `mul` 
* `div` 
* `ldr` 
* `str`
* `mov` 
* `bx` 

Given how simple it is, `miniarm` expects your program to be just a sequence of instructions which ends with `bx lr`


# Usage
```bash
$ miniarm --help
```

```bash 
miniarm -f [FILE]
  -f Specify the file to execute
  -help  Display this list of options
  --help  Display this list of options
```

## Example 
Say we have the following in a file called `example.arm`: 

```asm
mov r1, #12
mov r2, #13
mov r3, #14
mov r4, #15
bx lr
```

You can copy `_build/default/bin/main.exe` into your working directory and rename it as `miniarm` (or just use it directly like: `_build/default/bin/main.exe` --help)

And we run this: 
```bash 
$ miniarm -f example.arm
```

We'd get this as output: 
```bash
Ast.R6 = 0
Ast.R2 = 13
Ast.R14 = 0
Ast.R8 = 0
Ast.R7 = 0
Ast.R3 = 14
Ast.R13 = 0
Ast.R12 = 0
Ast.R5 = 0
Ast.R4 = 15
Ast.R9 = 0
Ast.R11 = 0
Ast.R0 = 0
Ast.R10 = 0
Ast.R1 = 12
Ast.R15 = 5
```

This is still a WIP.
