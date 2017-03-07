# Manual for Tali Forth for the 65c02 
Scot W. Stevenson <scot.stevenson@gmail.com> 

(THIS TEXT IS UNDER DEVELOPMENT AND MERELY A COLLECTION OF NOTES)

## Overview


## Installation


### Running the provided binary

Tali comes with an assembled version that should run out of the box with the
[py65mon](https://github.com/mnaberez/py65) simulator, a Python program. In this
version, Tali is compiled to 16k and starts at $C000. Note that py65mon
hardcodes the input and output routines at the beginning of $F000. To install
py65mon with Linux, use `sudo pip install -U py65`

(If you don't have PIP installed, you will have to add it first with
```
sudo apt-get install python-pip
```
There is a setup.py script as part of the package, too.) To start the emulator,
run:
```
py65mon --mpu 65C02 -r ophis.bin
```

## Assembly

Tali Forth was written with vim and the [Ophis 2.1
cross-assembler](http://michaelcmartin.github.io/Ophis/). Ophis uses a slightly
different format than other assemblers, but is in Python and therefore will run
on almost any operating system. To install Ophis on Windows, use the link
provided above. For Linux:

```
git clone https://github.com/michaelcmartin/Ophis
cd src
sudo python setup.py install
```

Switch to the folder where the Tali code lives, and assemble with

```
ophis --65c02 taliforth.asm
```

Development was performed with [py65mon](https://github.com/mnaberez/py65) which
is also in Python. To install on Linux:

```
sudo pip install -U py65
```

(If you don't have PIP installed, you will have to add it first with something like

```
sudo apt-get install python-pip
```

There is a setup.py script as part of the package, too.) To start the emulator, run:

```
py65mon --mpu 65C02 -r ophis.bin
```

Note that for testing and development, Tali Forth compiles to 16k and is
installed starting $C000. This is because py65mon hardcodes the input and output
routines at the beginning of $F000.

## Gotchas

Tali has a 16-bit cell size (use `1 cells 8 * .` to get the cells size in bits
with any Forth), which can trip up calculations when compared to the _de facto_
standard Gforth with 64 bits. Take this example:
```
( Gforth ) DECIMAL 1000 100 UM* HEX SWAP U. U.  186A0 0  OK
( Tali )   DECIMAL 1000 100 UM* HEX SWAP U. U.  86A0 1  OK
```
Tali has to use the upper cell of a double-celled number to correctly report the
result, while Gforth doesn't. If the conversion from double to single is only
via a DROP instruction, this will produce different results.


## Testing

There is no automatic or formal test suite available at this time. The file
docs/testwords.md includes a list of words that will help with some general
cases.


## Notes for Developers 

Any feedback and comments is welcome. Feel free to adapt Tali Forth to your own
system. Please note:

- The X register should not be changed without saving its pointer status

- The Y register is free to be changed by subroutines. This means it should not
  be expected to survive subroutines unchanged.


## Frequently Asked Questions

### Why "Tali" Forth?

I like the name, and we're probably not going to have anymore kids I can give it
to.

(If it sounds vaguely familiar, you're probably thinking of Tali'Zorah vas
Normandy, a character in the "Mass Effect" universe created by EA / BioWare.
This software has absolutely nothing to do with either the game or the companies
and neither do I, expect that I've played the games and enjoyed them.)

### Then who is "Liara"?

Liara Forth is a STC Forth for the big sibling of the 6502, the 65816. 

