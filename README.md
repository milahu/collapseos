# Collapse OS

*Bootstrap post-collapse technology*

Collapse OS is a Forth operating system and a collection of tools and
documentation with a single purpose: preserve the ability to program micro-
controllers through civilizational collapse.

It it designed to:

1. Run on minimal and improvised machines.
2. Interface through improvised means (serial, keyboard, display).
3. Edit text files.
4. Compile assembler source files for a wide range of MCUs and CPUs.
5. Read and write from a wide range of storage devices.
6. Assemble itself and deploy to another machine.

Additionally, the goal of this project is to be as self-contained as possible.
With a copy of this project, a capable and creative person should be able to
manage to build and install Collapse OS without external resources (i.e.
internet) on a machine of her design, built from scavenged parts with low-tech
tools.

## Getting started

Usage documentation is in-system, so access to documentation requires you to
run Collapse OS. Fortunately, doing so in an emulator is easy.

See `/emul/README.md` for getting an emulated system running.

Then, run `0 LIST` for an introduction, follow instructions from there.

Alternatively, there's also [Michael Schierl's JS Collapse OS emulator][jsemul]
which is awesome and allows you to run Collapse OS from your browser, but it
isn't always up to date. The "Javascript Forth" version is especially awesome:
it's not a z80 emulator, but a *javascript port of Collapse OS*!

## Organisation of this repository

* `blk`: Collapse OS filesystem's content. That's actually where Collapse OS'
         source code is located. Everything else is peripheral.
* `recipes`: collection of recipes that assemble Collapse OS on a specific
             machine.
* `tools`: Tools for working with Collapse OS from "modern" environments. For
           example, tools for facilitating data upload to a Collapse OS machine
           through a serial port.
* `emul`: Tools for running Collapse OS in an emulated environment.
* `tests`: Automated test suite for the whole project.

## Status

The project unfinished but is progressing well! See [Collapse OS' website][web]
for more information.

## Looking for the assembler version?

The Forth-based Collapse OS is the second incarnation of the concept. The first
one was entirely written in z80 assembly. If you're interested in that
incarnation, checkout the `z80asm` branch.

[web]: https://collapseos.org
[jsemul]: https://schierlm.github.io/CollapseOS-Web-Emulator/
