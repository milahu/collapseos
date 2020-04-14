# Collapse OS

*Bootstrap post-collapse technology*

Collapse OS is a z80 kernel and a collection of programs, tools and
documentation that allows you to assemble an OS that, when completed, will be
able to:

1. Run on minimal and improvised machines.
2. Interface through improvised means (serial, keyboard, display).
3. Edit text files.
4. Compile assembler source files for a wide range of MCUs and CPUs.
5. Read and write from a wide range of storage devices.
6. Replicate itself.

Additionally, the goal of this project is to be as self-contained as possible.
With a copy of this project, a capable and creative person should be able to
manage to build and install Collapse OS without external resources (i.e.
internet) on a machine of her design, built from scavenged parts with low-tech
tools.

## Forth reboot in process

You are currently looking at the `forth` branch of the project, which is a
Forth reboot of Collapse OS. You can see why I'm doing this in the [related
github issue][forth-issue].

Documentation is lacking, it's not ready yet, this is a WIP branch.

## See it in action

Michael Schierl has put together [a set of emulators running in the browser that
run Collapse OS in different contexts][jsemul].

Using those while following along with the [User Guide](doc/) is your quickest
path to giving Collapse OS a try.

## Organisation of this repository

* `forth`: Forth is slowly taking over this project (see issue #4). It comes
           from this folder.
* `recipes`: collection of recipes that assemble parts together on a specific
             machine.
* `blk`: Collapse OS filesystem's content. See `000` for intro.
* `doc`: User guide for when you've successfully installed Collapse OS.
* `tools`: Tools for working with Collapse OS from "modern" environments. For
           example, tools for facilitating data upload to a Collapse OS machine
           through a serial port.
* `emul`: Emulated applications.
* `tests`: Automated test suite for the whole project.

## Status

The project unfinished but is progressing well! See [Collapse OS' website][web]
for more information.

## Discussion

For a general discussion of Collapse OS and the ecosystem of technologies and ideas that may develop around it refer to [r/collapseos][discussion]

A more traditional [mailing list][listserv] and IRC (#collapseos on freenode) channels are also maintained.

[libz80]: https://github.com/ggambetta/libz80
[web]: https://collapseos.org
[jsemul]: https://schierlm.github.io/CollapseOS-Web-Emulator/
[discussion]: https://www.reddit.com/r/collapseos
[listserv]: http://lists.sonic.net/mailman/listinfo/collapseos
[forth-issue]: https://github.com/hsoft/collapseos/issues/4  

