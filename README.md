
### Introduction

This is a skeleton C64 program that will allow you to add your own custom functions and commands to the C64's BASIC. We all know how much the C64's BASIC **sucks**, so with this you can add the commands that you've always wanted!

A lot of the code in here comes from the book **The Advanced Machine Language Book for the Commodore 64** by Abacus Software. There's a section in the later part of the book that shows you how to add your own custom commands.

I simply took this and ran with it. I also commented it as best as I could. I know that it can be very hard to follow other people's 6510 assembly, but I'm really hoping it's clear what each line of code does.

### Goals

I grew up during the 8-bit era and really acquired a passion for technology, mostly because of how simple things were. It was easy to get started with BASIC. I was always disappointed in the C64's BASIC and dreamed of being able to create my own extensions (like David Simons did). I was too poor to be able to afford an assembler, so my dream was put on hold.

Now, with tools like Kick Assembler and VICE plus every C64 programming book known to man being available on the Internet Archive, I'm able to **finally** do what I've always wanted!

My goals are:

- Make C64 BASIC more usable. I don't have a domain in mind, and I'm not really targeting games. I'm sure it would be fun to build in some really cool sprite commands, but I think that may be beyond me at the moment.
- Create an easy-to-use, well-documented framework to allow others to add their own commands/functions to C64 BASIC.
- Add examples that others can use to create their own commands.
- Get better at 6510 assembly. It's not a current skill, but it absolutely will teach you to really break problems down.
- Have fun and hopefully excite others.


Suggestions and pull requests are welcome!


### Building

This is built with Kick Assembler. The latest version should be fine. There's a Makefile at the root of the directory that you can use to build and run it.

The code assembles at $c000 (49152) which is a 4k block of RAM that's popular with ML programmers.

Once it starts in VICE, type:

```basic
NEW
SYS49152
```

There are now a handful of commands available:

- `BACKGROUND` - Sets the background color: Example: `BACKGROUND 2` would set it to red
- `BORDER` - Sets the border color. Works the same as BACKGROUND
- `CLS` - Clears the screen. Nice and simple.
- `WOKE` - A 16-bit version of the POKE command. Example: `WOKE $61, $0400` would put $00 into $61 and $04 into $62.
- `MEMCOPY` - A fast way to move bytes around in memory. Example: `MEMCOPY $0428, $0400, $28` would copy the 2nd line of text to the first. This could be used to quickly scroll portions of the screen.
- `MEMFILL` - No more slow FOR/NEXT loops to POKE values into memory. Example: `MEMFILL $0400, 1000, 32` would clear the default screen.
- `BANK` - Select the current VIC bank. Must be between 0-3 where 0 starts at $0000 and 3 starts at $c000. This affects all graphics on the machine including screen, chars and sprites
- `SCREEN` - Select the 1k offset for the current video screen. The default is 1 ($0400) in bank 0. Each bank can hold 16 screens (not all can be used for screen RAM), so the value here has to be between 0 and 15.

As well as a couple of functions:

- `WEEK` - A version of the PEEK function which returns a 16-bit word instead of an 8-bit byte. Example: `PRINT WEEK($61)`
- `SCRLOC` - Returns the absolute address for the start of screen RAM. At startup `PRINT SCRLOC(0)` would return `1024`. You can use the `BANK` and `SCREEN` commands to relocate the screen.

In addition to the commands and functions, BASIC will now accept integers as either HEX or binary. So for example: `POKE $0400, $0a` or `POKE $d020, %00001111`. You're not limited to 16-bit values, so `PRINT $d00000` is perfectly valid.

### Extending

There are 4 hooks currently implemented:

- `ICRNCH` - This routine is what BASIC uses to tokenize input. All commands and functions have a single byte token that gets stored internally. This is redirected to the `ConvertToTokens` routine which allows us to tokenize our custom commands/functions.
- `IQPLOP` - This is the routine that BASIC uses to convert from token to text and is used when LISTing programs. We redirect this to the `ConvertFromTokens` routine so that our commands/functions can be expanded from their token form.
- `IGONE` - This is what BASIC calls when it's found a command token and needs to call its execution routine. This is directed to the `ExecuteCommand` routine which has a lookup table to decide, based on the token, which routine to call.
- `IEVAL` - When BASIC finds a function, it calls this to execute the function's routine. We use the routine `ExecuteFunction` to handle that which also has a lookup table to determine which routine goes with which function.

The `CmdTab` and `FunTab` labels point at the lookup tables for commands and functions and they **must** be in token number order.

The `NewTab` label points at your command/function string table which defines the commands and functions. Each must have the high bit of the last character in its string set. Token numbers for this table start at $cc and you must keep your functions and commands grouped together with commands coming first.

In order to identify the start/end token for commands, set `CMDSTART`, `CMDEND`, `FUNSTART` and `FUNEND`
