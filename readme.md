# Collapse OS

*Bootstrap post-collapse technology*

[Winter is coming](why.md) and Collapse OS aims to soften the blow. It is a Forth [(why Forth?)](forth.md) operating system and a collection of tools and documentation with a single purpose: preserve the ability to program microcontrollers through [civilizational collapse](civ.md). It is designed to:

1.  Run on minimal and improvised machines.
2.  Interface through improvised means (serial, keyboard, display).
3.  Edit text and binary contents.
4.  Compile assembler source for a wide range of MCUs and CPUs.
5.  Read and write from a wide range of storage devices.
6.  Assemble itself and deploy to another machine.

Additionally, the goal of this project is to be as self-contained as possible. With a copy of this project, a capable and [creative](why.md#creative) person should be able to manage to build and install Collapse OS without external resources (i.e. internet) on a machine of her design, built from scavenged parts with low-tech tools.

## Features

-   Runs on Z80, 8086, 6809 and 6502 machines with very little resources. [See hardware support list](hardware.md).
-   Can assemble Z80, AVR, 8086, 6809 and 6502 binaries.
-   Can disassemble 6502 and 6809.
-   Can program AVR microcontrollers.
-   Has a command line text editor similar to Forth's traditional editor as well as a visual text editor inspired by UNIX' VI.
-   Has a visual binary editor.
-   Has the full power of a reasonably well-featured Forth interpreter.
-   Can be built from a POSIX environment with minimal tooling: only cc and make are needed.
-   Excluding machine-specific ports, less than 2000 lines of code.

## Lives in Dusk OS

As of July 2023, the Collapse OS project lives in [Dusk OS](http://duskos.org), an operating system designed to be maximally useful during what I call [the first stage of collapse](why.md) (Collapse OS is for the second stage).

It's still the same self-contained, self-hosting OS that it was before, it's just that instead of seeding its bootstrapping process from POSIX, it does so from Dusk OS (which itself bootstraps from POSIX).

Going through Dusk OS dispenses us from having to maintain a POSIX VM for Collapse OS because Dusk invokes Collapse OS' cross-compiling process through a thin compatibility layer (both are Forths with many semantics similarities).

Also, the "old" Collapse OS, due to how it cross-compiles itself, had to maintain a binary blob to kickstart the POSIX VM and that binary blob was sometimes quite a hassle to update properly (chicken and egg type of problems). This blob also irked bootstrapping purists. It doesn't exist anymore. Dusk doesn't have such a blob either, making Collapse OS bootstrapping process entirely blob-less.

You don't have to familiarize yourself with Dusk OS to build one of the [Collapse OS ports](hardware.md). Each port's build process wraps the Dusk OS part in its Makefile and yields a Collapse OS image that has no relation to Dusk.

This transition isn't quite done yet, but it's well underway. One thing I haven't done yet is test all supported hardware. If you're trying to deploy Collapse OS on supported hardware now, I'd recommend using the "old" Collapse OS, confirm that it works, then try the new one and let me know if you notice a regression.

<span id="try"></span>

## Getting started

The best place to start is with the [RC2014 port of Collapse OS](https://git.sr.ht/~vdupras/collapseos-rc2014). It's the "canonical" machine-specific port of the project so it's the best maintained. The port has a built-in emulator so trying Collapse OS is one "make emul" away.

One thing that the RC2014 port doesn't have, however, is a Grid subsystem, so you can't use it to try applications like the Visual Editor. To do so, you can use the [PC/AT port of Collapse OS](https://git.sr.ht/~vdupras/collapseos-pc). Emulation of that port is done with QEMU.

If you're looking for the old Collapse OS distribution, from when it wasn't living inside Dusk OS yet, you can [download it here](download.md).

Documentation is in text files in Dusk's "fs/doc/cos" directory. Begin with "intro.txt". Alternatively, James Stanley hosts an [online Collapse OS documentation browser](https://incoherency.co.uk/collapseos/). However, at the time of this writing, this documentation was still generated from the "old" Collapse OS, so it's outdated.

Another interesting alternative for documentation is [Michael Schierl's PDF export of it](https://schierlm.github.io/CollapseOS-Web-Emulator/documentation.html) ([code that generates it](https://github.com/schierlm/CollapseOS-Web-Emulator/tree/master/pdf)). Unlike James' export, it's not automatically kept up to date, but it's a great way to print the whole thing at once. However, at the time of this writing, this documentation was still generated from the "old" Collapse OS, so it's outdated.

You can also try Collapse OS directly on your browser with [Michael Schierl's JS Collapse OS emulator](https://schierlm.github.io/CollapseOS-Web-Emulator/) which is awesome but it isn't always up to date. The "Javascript Forth" version is especially awesome: it's not a z80 emulator, but a *javascript port of Collapse OS*!

<span id="funding"></span>

## Funding

You are inspired by Collapse OS and would like to fund its development? I don't do any kind of crowdfunding, but if you happen to be a rich philanthropist, [maybe we can do something.](http://duskos.org/funding.html)

## Discussion

My name is Virgil Dupras and I can be reached at hsoft@hardcoded.net.

Discussion about Collapse OS takes place on a [private mailing list](http://duskos.org/discuss.html).

There's also [Tumble Forth](https://tumbleforth.hardcoded.net), a blog I've started where I write articles around Collapse OS and Dusk OS. Interesting discussions can happen around those articles.

## Related efforts

Here is a list of related efforts that I find noteworthy:

-   [Dusk OS](http://duskos.org)
-   [Tumble Forth](https://tumbleforth.hardcoded.net)
-   [Public Domain Books to Restart Computer Technology](https://blogbyjoshcogliati.blogspot.com/2021/05/public-domain-books-to-restart-computer.html)
-   [Civboot: a civilizational bootstrapper](https://github.com/civboot/civboot)
-   [Simplifier](https://simplifier.neocities.org)
-   [Sci.Electronics.Repair FAQ](https://www.repairfaq.org/)
-   [The Vintage Technology Digital Archive](http://vtda.org)
-   [A big collection of Apple-related documentation](https://www.apple.asimov.net/)
-   [Daniel Marks' electronic designs focusing on resilience](https://github.com/profdc9/)
-   [Michael Schierl's UXN port of Collapse OS](https://github.com/schierlm/collapseos-uxn)
-   [Deadly Optimism, Useful Pessimism](https://richardheinberg.com/museletter-353-deadly-optimism-useful-pessimism)
