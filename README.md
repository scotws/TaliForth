# Tali Forth for the 65c02 
Scot W. Stevenson <scot.stevenson@gmail.com>
First version: 19. Jan 2014 
This version:  26. Dec 2016

This is the README.txt version released with Tali Forth BETA 001. Note that this
is a BETA release of an incomplete program. See the .ods spreadsheet for a list
of supported instructions, or type WORDS once Tali Forth is running. Tali Forth
is hosted at GitHub, you can find the most current version at
[https://github.com/scotws/TaliForth](https://github.com/scotws/TaliForth).


## Introduction

Tali Forth is a Subroutine Threaded Code (STC) implementation for the 65c02 closely modeled on the ANS Forth standard. I wrote it because I wanted to understand the inner workings of Forth and have a modern implementation of the language for my 65c02 single board computer, the [Übersquirrel](http://uebersquirrel.blogspot.de/). It is released in the public domain with no warranty of any kind -- use at your own risk. See COPYING.txt for details.

Tali Forth aims to be, in rough order of priority: 

- **Simple**. The primary aim is to understand a complete Forth system byte by byte.
  This is why the STC format was chosen, and why the dictionary has a rather
  strange structure (see dictionary.txt for details). This is also the reason
  the source code is perversely over-commented. 

- **Specific**. Many Forths available are "general" implementations with a small
  core adapted to the target processor. Tali Forth was written for the 65c02
  8-bit MPU and that MPU only, with its strengths and limitations in mind.

- **Standardized**. Most Forths available for the 6502/65c02 are based on old 
  standards, for example FIG Forth. Learning Forth with them is like trying to
  learn modern English with Chaucer. Tali Forth follows the ANS Draft Standard
  200x, in the hope that it will soon be the current standard.  
  
- **Speedy**. Tali Forth places speed over size (within reason). The aim is to keep
  it and the kernel routines in 8k of ROM for single board computers. A further
  8k could be used for a library of Forth routines.



## Implementation Notes (BETA 001)

A lot of words have barely been tested. Most are unoptimized.

Tali Forth uses subroutine threading for longer words but automatically compiles
shorter routines in native 65c02 code. In the current version, the decision
which words are compiled how is on an ad-hoc basis; in future, this will follow
a rule based on speed and size of the routine. 

As a very simple implementation, Tali Forth does not support multitasking and is
not thread safe in any sense of the word. There are no individual USER
variables. Interrupts are currently not implemented in the kernel or Forth code. 

The "functional" reference is [gforth](http://www.gnu.org/software/gforth/).
Code that works on Tali Forth should produce the same result in gforth or have a
good reason why it is different. 


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
ophis --65c02 Tali-main-B001.asm 
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
installed starting $C000 instead of $E000. This is because py65mon hardcodes the
input and output routines at the beginning of $F000 and I'm too lazy to modify
them (at the moment). 


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


## Why "Tali" Forth?

I like the name, and we're probably not going to have anymore kids I can give it
to. 

(If it sounds vaguely familiar, you're probably thinking of Tali'Zorah vas
Normandy, a character in the "Mass Effect" universe created by EA / BioWare.
This software has absolutely nothing to do with either the game or the companies
and neither do I, expect that I've played the games and enjoyed them.) 
