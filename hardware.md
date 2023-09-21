# [Collapse OS](readme.md) — Hardware Support

Below is a list of hardware configurations that Collapse OS is known to work on as well as a link to their particular port. *Memory* is the amount of memory, in KB, the machine has, or the minimal amount of memory tried if it's variable. Some machines have more than 64KB of memory, but we never use paging, so we say "64". *Input/Output* is the type of device used to interact with the machine. *BLK medium* is the type of medium being used for Block support. *Self-hosting* is whether Collapse OS can assemble and deploy itself from within that machine. "No" doesn't mean it can't be done, just that it hasn't been tried yet.

<table style="width:100%;">
<colgroup>
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr class="header">
<th>Name</th>
<th>CPU</th>
<th>Mem</th>
<th>Input</th>
<th>Output</th>
<th>BLK medium</th>
<th>Self-hosting?</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><a href="https://git.sr.ht/~vdupras/collapseos-rc2014">RC2014</a></td>
<td>Z80</td>
<td>32</td>
<td>Serial I/O<br />
PS/2 keyboard</td>
<td>Serial I/O</td>
<td>SD card</td>
<td>Yes</td>
</tr>
<tr class="even">
<td><a href="https://git.sr.ht/~vdupras/collapseos-sms">Sega Master System</a><br />
or MegaDrive (Genesis)</td>
<td>Z80</td>
<td>8</td>
<td>D-Pad<br />
PS/2 keyboard</td>
<td>Builtin video</td>
<td>SD card</td>
<td>Yes</td>
</tr>
<tr class="odd">
<td><a href="https://git.sr.ht/~vdupras/collapseos-trs804p">TRS-80 4P</a></td>
<td>Z80</td>
<td>64</td>
<td>Builtin keyboard<br />
RS-232</td>
<td>Builtin video<br />
RS-232</td>
<td>5 1/4 floppy</td>
<td>Yes</td>
</tr>
<tr class="even">
<td><a href="https://git.sr.ht/~vdupras/collapseos-appleiie">Apple IIe</a></td>
<td>6502</td>
<td>64</td>
<td>Builtin keyboard</td>
<td>Builtin video</td>
<td>Floppy<br />
SD card</td>
<td>Yes</td>
</tr>
<tr class="odd">
<td><a href="https://git.sr.ht/~vdupras/collapseos-pc">PC/AT</a></td>
<td>8086</td>
<td>64</td>
<td>BIOS</td>
<td>BIOS</td>
<td>BIOS</td>
<td>Yes</td>
</tr>
<tr class="even">
<td><a href="https://git.sr.ht/~vdupras/collapseos-ti84">TI-84+</a></td>
<td>Z80</td>
<td>64</td>
<td>Builtin keyboard</td>
<td>Builtin LCD</td>
<td>None</td>
<td>No</td>
</tr>
<tr class="odd">
<td><a href="https://git.sr.ht/~vdupras/collapseos-z80mbc2">Z80-MBC</a></td>
<td>Z80</td>
<td>64</td>
<td>Serial I/O</td>
<td>Serial I/O</td>
<td>SD card</td>
<td>No</td>
</tr>
<tr class="even">
<td><a href="https://git.sr.ht/~vdupras/collapseos-coco2">TRS-80 Color Computer 2</a></td>
<td>6809</td>
<td>64</td>
<td>Builtin keyboard</td>
<td>Builtin video</td>
<td>None</td>
<td>No</td>
</tr>
</tbody>
</table>

If you get Collapse OS running on an unlisted platform, please [let me know](discuss.md).
