; TALI FORTH FOR THE 65C02 
; Scot W. Stevenson <scot.stevenson@gmail.com>
;
; First version 19. Jan 2014
; This version  10. Feb 2015 (first BETA)
; -----------------------------------------------------------------------------

; This program is placed in the public domain. 

; Note that it is distributed in the hope that it will be useful, but WITHOUT 
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
; FITNESS FOR A PARTICULAR PURPOSE. Use at your own risk. See COPYING.txt 
; for details.

; -----------------------------------------------------------------------------
; Developed with the Ophis assembler and py65mon simulator
; -----------------------------------------------------------------------------
.org FORTH 

; =============================================================================
; HELPER DEFINITIONS
; =============================================================================

; Character set (ASCII) 
.alias AscCC  $03               ; break (Control-C) ASCII character
.alias AscBS  $08               ; backspace ASCII character
.alias AscLF  $0A               ; line feed ASCII character
.alias AscCR  $0D               ; carriage return ASCII character
.alias AscESC $1B               ; Escape ASCII character
.alias AscSP  $20               ; space ASCII character
.alias AscDEL $7F               ; DEL ASCII character

; Dictionary flags
.alias IM     %10000000         ; Immediate
.alias NC     %01000000         ; Native Compile
.alias CO     %00100000         ; Compile Only

; =============================================================================
; HARDWARE MAP
; =============================================================================

; -----------------------------------------------------------------------------
; RAM 
; Assumes continuous RAM memory present from $0000 on. Minimum amount of 
; RAM required is TODO kb

.alias RamStart         $0000   ; must include Zero Page and Stack
.alias RamSize          $7FFF   ; default $8000 for 32 kb x 8 bit RAM
.alias RamEnd           [RamStart + RamSize]     
.alias PadOffset        $FF     ; Distance between Pad area and CP 

;==============================================================================
; MEMORY MAP
;==============================================================================

; -----------------------------------------------------------------------------
; ZERO PAGE
; The zero page acts as the Parameter (Data) Stack. It starts at $7F and grows
; downward towards $00 (128 bytes --> 64 cells) which makes over- and underlow
; easier to detect (highest bit must be 0 for a valid entry). Growing downward
; makes stack manipulations easier to understand, as entries are accessed
; by adding to the stack pointer (X register) instead of subtracting.

.alias SPMAX    $00     ; top of parameter (data) stack
.alias SP0      $7F     ; bottom of parameter (data) stack

; The top 16 bytes of the zero page ($F0 to $FF) and the 16 bytes above 
; the stack ($80 to $8F) are left unused as a "flood plain" in case of 
; stack over- or underflow as these conditions are only tested for after
; the fact.

; -----------------------------------------------------------------------------
; FORTH REGISTERS, POINTERS, AND FLAGS
; Forth registers and pointers start on the zero page at $90. IP and WRDWRK
; could probably be replaced by clever use of TMPADR, but this way we make
; sure that they won't be overwritten

.alias DP       $90   ; Dictionary Pointer (last entry; 2 bytes)
.alias CP       $92   ; Compiler Pointer (next free RAM byte; 2 bytes)
;               $94   ; UNUSED TODO close this gap
.alias STATE    $95   ; Compile state flag, TRUE is compile (2 bytes)
.alias BASE     $97   ; Number base, default decimal ($0A) (1 byte) 
.alias CIBA     $98   ; Address of the Current Input Buffer (CIB) (2 bytes)
.alias CIBN     $9A   ; Number of chars in the Current Input Buffer (2 bytes)
.alias INP      $9C   ; Input Buffer Pointer (>IN; 2 bytes)
.alias OUTPORT  $9E   ; Output port (default 0, 2 bytes)
.alias INPORT   $A0   ; Input port (default 0, 2 bytes)
.alias TBLLOC   $A2   ; Location of table currently being used (2 bytes)
.alias IP       $A4   ; Instruction Pointer, current xt (2 bytes)
.alias WRKWRD   $A6   ; WORKWORD: xt (link) of word being compiled (2 bytes)
.alias TMPADR   $A8   ; Temporary storage for addresses (2 bytes)
.alias TMPADR1  $AA   ; Temporary storage for more addresses (2 bytes)
.alias TMPADR2  $AC   ; Temporary storage for even more addresses (2 bytes)
.alias TMPCNT   $AE   ; Temporary storage for counters (2 bytes)
.alias TMPX     $B0   ; Temporary storage for X Register (1 byte)
.alias FLAG     $B1   ; Generic flag (1 byte)
.alias FLAG2    $B2   ; Generic flag (1 byte)
.alias OUTP     $B3   ; Output pointer for formated output (2 bytes)

; The zero page entries $D0 to $EF are reserved for the kernel and are 
; defined in Tali-Kernel.asm

; -----------------------------------------------------------------------------
; RETURN STACK
; Tali Forth uses the 65c02 stack as the Return Stack, from $100 to $1ff. The 
; CPU stack pointer is used as the Return Stack Pointer
.alias  RP0     $FF   ; bottom of return stack (pointer value)

; -----------------------------------------------------------------------------
; OTHER RAM AREAS
; The Dictionary Pointer (DP) should start off pointing to WORDS: If there is
; a break in the dictionary's link structure, this makes it easier to find it. 
; Note that the last entry in the dictionary should always be BYE for the 
; same reason
.alias  TIB     $0200    ; Terminal Input Buffer, $400 to $4FF
.alias  SYSPAD  $0300    ; System scratch pad 
.alias  CP0     $0400    ; Start of free RAM (Compiler Pointer)
.alias  DP0     l_words  ; First entry in the dictionary at start

; -----------------------------------------------------------------------------
; CONSTANTS
.alias  PADSIZE         $FF     ; Size of the PAD area
.alias  TIBSIZE         $00FE   ; Size of the Terminal Input Buffer

; =============================================================================
; INITILIZE SYSTEM (COLD BOOT)
; =============================================================================
; IRQ, BRK and Cold Boot commands end up here
k_irqv:
COLD:   ; Load default values to registers and pointers
        ; TODO move this to a table once we know what we are doing 

        ; Initialize Dictionary Pointer (DP)
        lda #<DP0
        sta DP
        lda #>DP0
        sta DP+1

        ; Compile Pointer (CP): First available space in RAM  
        lda #<CP0
        sta CP
        lda #>CP0
        sta CP+1

        ; Initialize number base to ten (DECIMAL)
        lda #$0A
        sta BASE

        ; Set input and output ports to zero. We don't have an error port in this
        ; version
        stz OUTPORT
        stz OUTPORT+1
        stz INPORT
        stz INPORT+1

        ; We start out with the Terminal Input Buffer (TIB) as the 
        ; Current Input Buffer (CIB) ... 
        ; TODO see if we really need this, since it is repeated below
        lda #<TIB       ; LSB
        sta CIBA 
        lda #>TIB       ; MSB
        sta CIBA+1

        ; ... and declare that buffer to be empty
        stz CIBN
        stz CIBN+1

        ; Reset stack pointer
        ldx #SP0        

; -----------------------------------------------------------------------------
; COMPILE HIGH-LEVEL COMMANDS 
; We use Tali Forth to compile even more Tali Forth. Putting these high-level 
; commands in strings and compiling them during system boot is a quick way to 
; get a working system, and makes it easier to later include new commands 
; in the dictionary. 
.scope 
        ldy #$00        

_loop:  ; make room on the Data Stack
        dex
        dex

        ; put address on Zero Page for easy handling. We end up pointing to the next 
        ; entry in the table. All entries are counted strings
        lda fhltbl,y
        sta 1,x         ; LSB
        iny
        lda fhltbl,y
        sta 2,x         ; MSB 
        iny

        ; a value of 0000 means we've completed the table and can quit 
        ora 1,x         ; MSB is still in A 
        beq _done

        ; we store the high-level instructions as counted strings for ease 
        ; of handling. Use COUNT to convert them to the modern (addr u) 
        ; format
        jsr l_count

        ; Top of stack now points to string of command, so EVALUATE. Note 
        ; we currently don't support the ANS Forth word SOURCE-ID; if we 
        ; ever do, it will need to be handled here
        phy 
        jsr l_eval 
        ply 

        bra _loop 

_done:  
.scend
; -----------------------------------------------------------------------------
; We're ready to go. Print intro strings. If this gets any longer, we might
; want to convert this to a loop and table structure
        jsr l_cr
        lda #$00                ; title 
        jsr f_prtzerostr
        lda #$01                ; version
        jsr f_prtzerostr
        lda #$02                ; disclaimer
        jsr f_prtzerostr
        lda #$03                ; mini-instructions
        jsr f_prtzerostr
        jsr l_cr

; =============================================================================
; START MAIN LOOP  
; =============================================================================
; Note we can't call a subroutine here because ABORT goes directly to QUIT 
; and QUIT resets the Return Stack

        jmp l_abort

; Whimsical style note: Indentation of main code increased past this level 
; to show we're passed the boot sequence 

; =============================================================================
; HELPER ROUTINES
; =============================================================================
; Internal helper routines, not to be accessed by outside. These start with
; "f_" 

; -----------------------------------------------------------------------------
; CONVERT LOWER CASE ASCII LETTER TO UPPER CASE 
; Takes char in A and converts any letters to upper case. If not a letter or 
; already upper case, leave original value. Called by TOUPPER 
.scope
f_toupper:
                cmp #'a
                bcc _done       ; not lower case, too low
                cmp #'z+1
                bcs _done       ; not lower case, too high

                adc #$e0        ; offset to upper case (wraps)

_done:          rts             ; we're good (finally)
.scend 
; -----------------------------------------------------------------------------
; CONVERT STRING FROM LOWER CASE TO UPPER 
; Assumes that string is on stack as ( addr u ) and converts it in place. 
; Calls f_toupper, destroys A, Y and changes TMPCNT, TMPADR and TMPADR1
.scope
f_strtoupper:   lda 3,x         ; LSB of addr
                sta TMPADR
                lda 4,x         ; MSB of addr
                sta TMPADR+1

                ldy 1,x         ; LSB of u, we ignore MSB
                dey             ; adjust length 

_loop:          lda (TMPADR),y
                jsr f_toupper
                sta (TMPADR),y
                dey
                bpl _loop

                rts
.scend 
; -----------------------------------------------------------------------------
; COMPILE SUBROUTINE JUMP / JUMP / WORD
; Routines to compile instructions such as "jsr l_words" or "jmp l_words" into a 
; word that is created by another Forth word. Use either for subroutine jumps
; 
;       jsr f_cmpljsr
;       .word <addr>
; 
; which includes the opcode for JSR, or 
;
;       jsr f_cmpljmp
;       .word <addr>
;
; which includes the opcode for JMP, or 
;
;       jsr f_cmplword
;       .word <word>
;
; which simply adds the word in little-endian format. Used in various places. Note
; this trades memory savings for a 12 clock cycle overhead because of JSR/RTS.
; Note this uses FLAG. Note this may not be used for words that require native
; compile
.scope
f_cmplword:     lda #$00        ; just compile word in little-endian
                bra _common

f_cmpljsr:      lda #$20        ; compile "JSR" opcode 
                bra _common

f_cmpljmp:      lda #$4C        ; compile "JMP" opcode; falls through to _common

                ; opcode doubles as a non-zero flag 
_common:        sta FLAG  

                ; pull address/word off of stack, increase by one because of
                ; the way the 65c02 handles subroutines
                ply             ; LSB of address
                pla             ; MSB of address 
                iny
                bne +
                inc 
*               sty TMPADR1     ; LSB
                sta TMPADR1+1   ; MSB

                ldy #$00 

                ; see if we're just adding a word
                lda FLAG
                beq _wordonly

                ; this is either f_cmpljsr or f_cmpljmp, so we use the opcode 
                ; that doubled as the flag and save that first 
                sta (CP),y
                iny
                
                ; continue with common part: compile word 
_wordonly:      lda (TMPADR1)   ; LSB
                sta (CP),y
                iny

                inc TMPADR1
                bne +
                inc TMPADR+1

*               lda (TMPADR1)   ; MSB
                sta (CP),y
                iny

                ; save new CP
                tya
                clc
                adc CP
                sta CP
                bcc +
                inc CP+1

                ; restore the correct return address. We have already added
                ; two to the return address, so we just need to push it on 
                ; the stack
*               lda TMPADR1+1   ; MSB
                pha
                lda TMPADR1     ; LSB
                pha 

                rts
.scend
; -----------------------------------------------------------------------------
; COMPILE/EXECUTE MAIN ROUTINE
; This is the core routine called by EVALUTE and QUIT. We process one line 
; only. Uses Y and TMPCNT
.scope
f_compexe: 
_parseword:     ; PARSE-NAME ("text" -- addr u) 
                jsr l_prsnm

                ; If PARSE-NAME returns zero on top of the stack, it means 
                ; that no characters were left in the line and we need a 
                ; new line
                lda 1,x
                sta TMPCNT+1    ; save a copy of u for number check 
                ora 2,x
                bne +

                jmp _doneline

                ; Though we let the user input words in upper or lower case, 
                ; we only work with upper case internally, so we convert
                ; the string now. We have (addr u) on the stack. 
*               jsr f_strtoupper 

                ; PARSE-NAME returned the word it found as (addr n), while
                ; FIND wants a counted string (cs-addr). We jump to a 
                ; special entry point in the FIND routine to avoid having
                ; to convert things ("FIND internal", l_findint)
                
                ; see if the word the stack points to is in the dictionary,
                ; returns zero if not 
                jsr l_findint

                lda 1,x         ; we only need to check LSB for flag 
                bne _found

                ; Attempt to convert number. FIND has returned (addr 0),
                ; we need to send (addr u) to NUMBER. Good thing we saved
                ; n to TMPCNT+1
                lda TMPCNT+1
                sta 1,x
                stz 2,x         ; paranoid

                jsr l_number    ; returns (n -1 | d -1 | addr 0 ) 

                ; if there was a failure, it isn't a number either, so
                ; complain and abort
                lda 1,x
                beq _parseerror 

                ; it's a legit number, so drop flag off stack 
                inx
                inx             ; now (n | d) 

                ; did we get the number in compile or interpret mode?
                lda STATE
                ora STATE+1
                beq _parseword  ; interpret, so we're done. Get next word

                ; TODO We only handle single-cell numbers correctly here 
                ; Impletement 2LITERAL and (2LITERAL) and change the 
                ; flags from NUMBER so we can distinguish between single
                ; and double numbers

                ; Compile the number as a literal 
                jsr f_cmpljsr
                .word l_plit

                ; compile our number
                jsr l_comma

                ; we're done, get next word
                bra _parseword


_parseerror:    ; Word not found and it isn't a number, so complain 
                ; and abort. We land here with (addr 0) and u in TMPCNT

                ; print offending word for easier diagnostics
                lda TMPCNT
                sta 1,x
                lda TMPCNT+1 
                sta 2,x         ; paranoid, should be zero anyway 
                jsr l_type 

                lda #$0b        ; code for syntax error string
                jmp error

_found:         ; Found word, stack is now (xt f). Save the xt that was 
                ; returned before we do anything else. 
                lda 3,x         ; LSB
                sta IP
                lda 4,x         ; MSB
                sta IP+1

                ; Compile or interpret? 
                lda STATE 
                ora STATE+1
                bne _compile

                ; Interpret. But make sure this is not a compile-only word 
                ; by checking the flag in bit 5 of the Length Byte. 
                ldy #$02
                lda (IP),y
                and #%00100000
                beq _execute

                lda #$0c        ; code for "compile-only word" error string
                jmp error

_execute:       ; We already have saved the xt and we don't care about the
                ; flag in interpret mode, so we dump both 
                inx             
                inx
                inx
                inx

                ; Only JMP has the addressing mode we need, and all our 
                ; Forth commands end with a RTS instruction. We fake the 
                ; return address by pushing the correct address to the 
                ; 65c02's stack and then doing a normal JMP. When we return, 
                ; we land on a NOP so we don't have to DEC the return address
                lda #>_doneexec         ; push MSB first
                pha
                lda #<_doneexec
                pha 

                jmp (IP)

_doneexec:      ; Keep the NOP here as the landing site for the indirect 
                ; subroutine jump (easier and quicker than adjusting the
                ; return address on the 65c02's stack)
                nop             
                
                ; Check for stack over- or underflow. Note that this check
                ; happens after the fact so we keep "floodplain"
                ; bytes on both sides of the stack (see memory map)
                txa
                bpl + 

                lda #$07                ; code for stack error
                jmp error 

                ; We're good, get next word
*               jmp _parseword


_compile:       ; Compile. First, see if the Precedence bit is set. If yes, 
                ; it's an immediate word and we execute it even though 
                ; we're in compile mode. 
                lda 1,x
                dec                     ; is A = $01?
                beq _execute

                ; Call COMPILE, and let it do the hard work. First though
                ; drop the flag, leaving just the xt on the stack
                inx
                inx

                jsr l_cmpc

                ; That's quite enough of this word, let's get the next one
                jmp _parseword

                ; we're all done on this line
_doneline:      rts
.scend
; -----------------------------------------------------------------------------
; Convert byte in A to two ASCII hex digits and print them via f_putchr, usually
; to the screen. Calls f_nib2asc, which does all the real work
.scope
f_byte2hexasc: 
                pha             ; save copy of A

                ; convert hi nibble
                lsr             
                lsr
                lsr
                lsr
                jsr f_nib2asc

                ; convert lo nibble
                pla
                pha             ; we want to return original A
                jsr f_nib2asc
                pla

                rts
.scend
; -----------------------------------------------------------------------------
; Convert lower nibble of A to ASCII hex number equivalent and print it via
; f_putchr. Called by f_byte2hexasc
.scope
f_nib2asc:
                and #$0F        
                ora #'0
                cmp #'9+1
                bcc +
                adc #$06

*               jmp f_putchr    ; JSR/RTS
.scend
; -----------------------------------------------------------------------------
; OUTPUT CHARACTER TO CURRENT PORT. This is a general routine used by 
; EMIT, TYPE, SPACE and others. Assumes that character to print is in A. 
; If this gets any larger than three channels, consider making this a table.
; TODO test VIA routines
.scope
f_putchr:       phy
                ldy OUTPORT
                bne _c1

                ; PORT 0: DEFAULT, Terminal, ASCI 
                ply
                jmp k_wrtchr    ; JSR/RTS

_c1:            dey
                bne _c2

                ; PORT 1: VIA Port A output
                ply
                jmp k_wrtchrVIAa        ; JSR/RTS

_c2:            dey
                bne _err

                ; PORT 2: VIA Port B output
                ply
                jmp k_wrtchrVIAb        ; JSR/RTS

_err:           lda #$08        ; string code for unknown channel 
                jmp error
.scend
; -----------------------------------------------------------------------------
; PRINT ZERO-TERMINATED STRING TO CURRENT PORT. Used internally to print
; strings, as zero-terminated is easier to work with on the 65c02 than 
; counted strings. Accepts number of string in A. 
.scope
f_wrtzerostr:   ; version without a final linefeed
                ldy #$00 
                bra _common 

f_prtzerostr:   ; version with a final linefeed
                ldy #$FF 
_common:        phy

                asl
                tay
                lda strtbl,y
                sta TBLLOC
                iny
                lda strtbl,y
                sta TBLLOC+1

                ldy #$00
*               lda (TBLLOC),y
                beq _linefeed
                jsr f_putchr
                iny
                bra -
                
_linefeed:      ; get flag to see if we print a final linefeed or not 
                lda #AscLF
                ply
                bne f_putchr    ; JSR/RTS

_done:          rts
.scend
; -----------------------------------------------------------------------------
; INPUT CHARACTER FROM CURRENT PORT. This is a general routine used by 
; KEY, ACCEPT and others. Returns the character in A. 
; If this gets any larger than three ports , consider making this a table.
; TODO test VIA routines
.scope
f_getchr:       phy
                ldy INPORT
                bne _c1

                ; PORT 0: DEFAULT, Terminal, ASCI 
                ply
                jmp k_getchr    ; JSR/RTS

_c1:            dey             ; if 1 this turns to 0
                bne _c2

                ; PORT 1: VIA Port A input
                ply
                jmp k_getchrVIAa        ; JSR/RTS

_c2:            dey
                bne _err

                ; PORT 2: VIA Port B input
                ply
                jmp k_getchrVIAb        ; JSR/RTS

_err:           lda #$08        ; string code for wrong channel
                jmp error       ; JSR/RTS
.scend
; -----------------------------------------------------------------------------
; COMPARE TOS/NOS and return results in form of the 65c02 flags
; Adapted from Lance A. Leventhal "6502 Assembly Language Subroutines". 
; For signed numbers, Z signals equality and N which number is larger: 
;       if TOS = NOS: Z=1 and N=0
;       if TOS > NOS: Z=0 and N=0
;       if TOS < NOS: Z=0 and N=1
; For unsigned numbers, Z signals equality and C which number is larger:
;       if TOS = NOS: Z=1 and N=0
;       if TOS > NOS: Z=0 and C=1
;       if TOS < NOS: Z=0 and C=0
; Compared to the book routine, WORD1 (MINUED) is TOS 
;                               WORD2 (SUBTRAHEND) is NOS
.scope
f_cmp16:        ; compare LSB. We do this first to set the Carry Flag
                lda 1,x         ; LSB of TOS
                cmp 3,x         ; LSB of NOS 
                beq _equal 

                ; low bytes are not equal, compare MSB
                lda 2,x         ; MSB of TOS
                sbc 4,x         ; MSB of NOS 
                ora #$01        ; Make Zero Flag 0 because we're not equal
                bvs _overflow 
                bra _notequal

_equal:         ; low bytes are equal, so we compare high bytes
                lda 2,x         ; MSB of TOS
                sbc 4,x         ; MSB of NOS 
                bvc _done

_overflow:      ; handle overflow because we use signed numbers
                eor #$80        ; complement negative flag

_notequal:      ora #$01        ; if overflow, we can't be equal 

_done:          rts 
.scend 
; =============================================================================
; CODE FIELD ROUTINES
; =============================================================================
; Code field routines start with fc_

; -----------------------------------------------------------------------------
; DOCONST Execute a constant: Push the data in the first two bytes of the
; Data Field onto the stack. 
.scope
fc_docon:       ; value is stored in the two bytes after the JSR
                ; return address
                pla                     ; LSB of return address
                sta TMPADR2
                pla                     ; MSB of return address
                sta TMPADR2+1

                ; make room on stack and save the value there
                dex
                dex

                ; start LDY off with one instead of zero because of how JSR
                ; stores the address for RTS
                ldy #$01
                lda (TMPADR2),y         ; LSB 
                sta 1,x
                iny
                lda (TMPADR2),y         ; MSB 
                sta 2,x

                ; the RTS takes us back to the original caller 
                rts
.scend
; -----------------------------------------------------------------------------
; DOVAR Execute a variable: Push the address of the first bytes of the 
; Data Field on the stack. This is called with JSR instead of JMP, so we can 
; pick the address of the calling variable off the 65c02's stack. The final
; RTS takes us to the original caller. 
.scope
fc_dovar:       ; pull return address off of the machine's stack, adding one
                ; because of the way the 65c02 handles subroutines
                ply             ; LSB
                pla             ; MSB
                iny
                bne +
                inc
*               sty TMPADR2     ; LSB
                sta TMPADR2+1   ; MSB

                ; get variable and push it on the stack 
                dex
                dex

                lda TMPADR2     ; LSB
                sta 1,x
                lda TMPADR2+1   ; MSB
                sta 2,x

                rts
.scend
; -----------------------------------------------------------------------------
; DODOES Runtime part for DOES>, installed by DOES> and used in combination 
; with (DOES). See http://www.bradrodriguez.com/papers/moving3.htm
; for details on how this works.
.scope
fc_dodoes:      ; Assumes the address of the CFA of the original defining word
                ; is on the top of the stack (for instance, CONSTANT). Save 
                ; it for a jump later. We have to add one byte because of the
                ; way that the 65c02 handles subroutines
                ply             ; LSB
                pla             ; MSB
                iny
                bne +
                inc
*               sty TMPADR2     ; LSB
                sta TMPADR2+1   ; MSB

                ; Next on the stack should be the address of the PFA of 
                ; the calling defined word, say the name of the constant
                ; we just defined. Push this on the Data Stack.
                dex
                dex

                pla 
                clc 
                adc #$01        ; add one because of JSR convention
                sta 1,x
                pla
                adc #$00        ; we only care about the carry
                sta 2,x

                ; Left on the stack is the return address from the original
                ; "main" routine. We leave that untouched, and JMP to the
                ; code fragment of the defining word
                jmp (TMPADR2) 
.scend
; =============================================================================
; ERROR ROUTINE
; =============================================================================
; PRINT ERROR STRING. Expect the error code (actually just the string number)
; in A. The jump to ABORT takes us back to the interpreter loop. 
.scope
error:          ; print a space 
                pha
                lda #AscSP
                jsr f_putchr

                ; print generic error string
                lda #$06                ; code for ">>>Error<<<" string
                jsr f_prtzerostr

                ; print specific error string, terminated by a line feed
                pla 
                jsr f_prtzerostr

                jmp l_abort
.scend
; =============================================================================
; DICTIONARY 
; =============================================================================

; -----------------------------------------------------------------------------
; BYE ( -- ) 
; BRK is for systems that have a monitor; use WAI or STP for standalone 
; systems. With an emulator, you can check if everything went well because
; X should be $7F after this instruction closes the program.
; ** THIS IS ALWAYS THE FINAL ENTRY IN THE DICTIONARY **
l_bye:          bra a_bye
                .byte NC+$03 
                .word $0000     ; no more links, end of dictionary
                .word z_bye
                .byte "BYE"

a_bye:          brk
z_bye:          rts             ; never reached, require for native compile

; ----------------------------------------------------------------------------
; COLD ( -- )
; Reboot the Forth system
l_cold:         bra a_cold
                .byte $04 
                .word l_bye     ; link to BYE
                .word z_cold
                .byte "COLD"
.scope
a_cold:         jmp COLD 

z_cold:         rts
.scend
; -----------------------------------------------------------------------------
; (LITERAL) ( -- x ) 
; Run-time routine for LITERAL: Push value in the two bytes immediately 
; following this word on the stack. This is a compile-only word and must 
; be called by JSR (no native compiling). Note we can't replace this by
; f_cmplword because we put things on the stack, not compile them
l_plit:         bra a_plit
                .byte CO+$09 
                .word l_cold    ; link to COLD
                .word z_plit
                .byte "(LITERAL)"
.scope
a_plit:         ; make room on stack
                dex
                dex

                ; get the value after the command
                ply             ; LSB
                pla             ; MSB
                iny
                bne +
                inc
*               sty TMPADR      ; LSB
                sta TMPADR+1


                ; get bytes after JSR address
                lda (TMPADR)    ; LSB
                sta 1,x
                inc TMPADR
                bne +
                inc TMPADR+1
*               lda (TMPADR)    ; LSB
                sta 2,x

                ; replace the new address on the stack 
                lda TMPADR+1
                pha
                lda TMPADR
                pha 

z_plit:         rts
.scend
; -----------------------------------------------------------------------------
; LITERAL ( n --  ) 
; During compilation, add number to compiled word so that (LITERAL) is called
; during execution. This is an immediate, compile-only word
l_lit:          bra a_lit
                .byte IM+CO+$07
                .word l_plit    ; link to (LITERAL)
                .word z_lit
                .byte "LITERAL"
.scope
a_lit:          ldy #$00

                ; compile the call to (LITERAL) 
                jsr f_cmpljsr
                .word l_plit
 
                ; now store the number provided on the stack
                jsr l_comma

z_lit:          rts
.scend
; -----------------------------------------------------------------------------
; ABORT ( -- ) 
; Reset the parameter (data) stack pointer and continue as QUIT 
; Note we can jump here via subroutine because we reset the stack pointer anyway
l_abort:        bra a_abort
                .byte $05 
                .word l_lit     ; link to LITERAL
                .word z_abort
                .byte "ABORT"

a_abort:        ldx #SP0        ; Reset stack pointer

                ; Set output and input to default (zero)
                stz OUTPORT
                stz OUTPORT+1
                stz INPORT
                stz INPORT+1

z_abort:        bra l_quit      ; ABORT always flows into quit
; -----------------------------------------------------------------------------
; QUIT ( -- ) 
; Endless interpreter loop. Resets the return stack so it can't be accessed by 
; subroutine calls without trickery.
; TODO see if this will compile okay
l_quit:         bra a_quit
                .byte $04 
                .word l_abort   ; link to ABORT 
                .word z_quit
                .byte "QUIT"
.scope
a_quit:         ; Reset the return stack (65c02 stack) pointer 
                txa
                ldx #RP0
                txs
                tax

                ; Default input buffer is the Terminal Input Buffer
                lda #<TIB       ; LSB
                sta CIBA 
                lda #>TIB       ; MSB
                sta CIBA+1

                ; Set number of chars in the Current Input Buffer to zero
                stz CIBN
                stz CIBN+1      ; paranoid, always zero 

                ; Reset interpreter state to interpret (not: compile). Note
                ; that state is defined as part of the ANS Forth Core standard
                ; to be one cell large. Anything non-zero is compiling
                stz STATE
                stz STATE+1     ; paranoid, always zero 

_getline:       ; Get one line of input from the user. Keelah se'lai! 

                ; Get address of Terminal Input Buffer (TIB)
                jsr l_source    

                ; max number of characters to get. Overwrites the number
                ; of chars in buffer that SOURCE just returned
                lda #<TIBSIZE
                sta 1,x
                lda #>TIBSIZE
                sta 2,x

                ; Get input line, print a space at the end as an offset
                ; to what the user gave us
                jsr l_accept    

                ; ACCEPT returns the number of characters given, which we
                ; put in CIBN. Note we only accept up to $FF chars, so 
                ; CIBN+1 is a dummy value, included in case we later want to
                ; expand this to 16 bit 
                lda 1,x
                sta CIBN
                stz CIBN+1      ; paranoid, always zero 

                inx             ; drop return value 
                inx

                ; Reset pointer (>IN) 
                stz INP
                stz INP+1       ; paranoid, always zero

                ; compile or execute
                jsr f_compexe

                ; Completed one line. If we're still compiling, print 
                ; "compiled", else print "ok"
                lda STATE 
                ora STATE+1
                beq _prtok

                ; Tell the user we've added the word to the definition
                ; (replaces the "ok" prompt)
                lda #$05
                jsr f_prtzerostr
                bra _clrstack

_prtok:         ; If we're done with the line, print "ok" (lower case)
                lda #$04
                jsr f_prtzerostr        ; drops through

_clrstack:      ; drop the address and number of characters that PARSE-NAME
                ; always returns 
                inx
                inx
                inx 
                inx

                ; Get the next line. This is an endless loop. We have to use
                ; JMP instead of BRA because the distance is too great
                jmp _getline

z_quit:         rts     ; never reached; required for native compile
.scend


; ----------------------------------------------------------------------------
; ABORTQ ( "message" -- ) ("ABORT"") 
; If TOP is TRUE, print a message and abort. This is a compile-only word
; One way to create this in Forth is CamelForth's 
;   : ?ABORT ( f addr u )  ROT IF TYPE ABORT THEN 2DROP ;
;   : ABORT" ( "msg" -- )  [COMPILE] S" ['] ?ABORT COMPILE, ;
l_abortq:       bra a_abortq
                .byte CO+IM+$06 
                .word l_quit    ; link to QUIT 
                .word z_abortq
                .byte "ABORT", $22
.scope
a_abortq:       ; parse message and save the address and length
                ; adds them to Dictionary and will push them to the Data Stack
                ; when run (S") 
                jsr l_squote 

                ; compile runtime component (ABORT)
                jsr f_cmpljsr
                .word l_pabortq

z_abortq:       rts
.scend
; ----------------------------------------------------------------------------
; PABORTQ ( f -- )   ("(ABORT")")
; Runtime component of ABORT". If flag on Data Stack is true, print message and
; abort
l_pabortq:      bra a_pabortq
                .byte CO+$08 
                .word l_abortq    ; link to ABORTQ
                .word z_pabortq
                .byte "(ABORT", $22, ")"

.scope
a_pabortq:      ; we started out with a flag on the Data Stack, but the first part
                ; of the word compiled by ABORT" pushed (addr u) on as well, so 
                ; our flag is on third place
                lda 5,x         ; third entry on Data Stack 
                ora 6,x
                beq _done

                jsr l_type
                jmp l_abort     ; don't need a JSR because we nuke the stacks

_done:          ; ABORT not called, drop the top three top entries on the Data
                ; stack 
                txa
                clc
                adc #$06
                tax

z_pabortq:      rts
.scend
; -----------------------------------------------------------------------------
; DUMP ( addr u -- ) 
; Print u bytes of memory starting at addr in a pretty way
l_dump:         bra a_dump
                .byte $04 
                .word l_pabortq    ; link to PABORTQ
                .word z_dump
                .byte "DUMP"
.scope 
a_dump:         ; start internal counter for 16 number per row
                jsr l_cr 
                ldy #$10

_loop:          ; if there are zero bytes left to display, we're done
                lda 1,x         ; LSB
                ora 2,x         ; MSB
                beq _done

                ; dump the contents
                lda (3,x) 

                jsr f_byte2hexasc
                jsr l_space

_nextchar:      ; print next character
                inc 3,x 
                bne _counter
                inc 4,x

_counter:       ; loop counter
                lda 1,x
                bne +
                dec 2,x
*               dec 1,x

                dey
                bne _loop
                bra a_dump

_done:          inx
                inx
                inx
                inx

z_dump:         rts
.scend
; -----------------------------------------------------------------------------
; SEE ( "name" -- ) 
; Print representation of word. This is strongly orientated towards debugging
; at the moment and might be rewritten later for more user-friendly content. 
; Note that SEE does not display the final RTS ($60) in the Data Field. 
l_see:          bra a_see
                .byte $03 
                .word l_dump    ; link to dump
                .word z_see
                .byte "SEE"
.scope
a_see:          ; get word from input 
                jsr l_prsnm

                ; if we got a zero in return, no word found, so complain 
                ; and quit
                lda 1,x         ; LSB
                ora 2,x         ; MSB
                bne _toupper

                ; SEE never returns anything, so we clean up by dropping
                ; (addr n) that PARSE-NAME left us 
                inx
                inx
                inx
                inx

                lda #$0a        ; Code for name not found during parsing
                jmp error

_toupper:       ; convert to upper case because we know that we are looking
                ; for a Forth word
                jsr f_strtoupper

                ; find name in dictionary. This is the internal version of
                ; FIND that takes (addr u) instead of the old-school
                ; (cs-addr) counted string. 
                jsr l_findint

                ; Check LSB of flag to see if we have a match. MSB is ignored
                lda 1,x         
                bne _found

                ; word not in dictionary, print error and abort
                lda #$0b        ; string code for syntax error
                jmp error

_found:         ; drop the flag we don't care about
                inx
                inx 

                ; move xt to a place we can index more easily. We use 
                ; TMPADR2 because DUMP uses TMPADR
                lda 1,x
                sta TMPADR2
                lda 2,x
                sta TMPADR2+1

                ; print formated information 

                ; -- print xt -- 
                jsr l_cr
                lda #$0F        ; code for "xt: " string
                jsr f_wrtzerostr

                ; we print the xt "human readable" though internal 
                ; storage is actual little endian 
                jsr l_dup
                jsr l_udot 
                jsr l_space
                jsr l_space

                ; -- print flags -- 
                ldy #$02        ; offset for Data Byte 
                lda (TMPADR2),y
                bpl _no_im
                pha 
                lda #$10        ; code for "immediate" string 
                jsr f_wrtzerostr
                pla 

_no_im:         asl
                bpl _no_nc      
                pha 
                lda #$12        ; code for "native compile" string 
                jsr f_wrtzerostr
                pla 

_no_nc:         asl
                bpl _no_co
                lda #$11        ; code for "compile-only" string 
                jsr f_wrtzerostr

_no_co:         jsr l_cr

                ; -- print link -- 
                lda #$13        ; code for "links to" string 
                jsr f_wrtzerostr
                jsr l_space

                dex             ; use >NAME to print name of string 
                dex

                ldy #$03        ; offset to link field
                lda (TMPADR2),y
                sta 1,x
                iny
                lda (TMPADR2),y
                sta 2,x

                jsr l_dup       ; print link as number
                jsr l_udot 
                jsr l_space

                jsr l_gtname    ; print link name string 
                jsr l_type
                jsr l_cr

                ; -- dump Data Field -- 
                lda #$14        ; code for "Data Field dump" string 
                jsr f_wrtzerostr

                ; get Data Field starting address
                ldy #$01        ; get offset from branch command
                lda (TMPADR2),y  
                inc             ; we start two bytes farther down
                inc             ; now we have correct offset for Data Field

                ; make room on stack
                dex
                dex
                dex
                dex
                
                ; get address of Data Field (a_xxxx)
                clc
                adc TMPADR2
                sta 3,x         ; LSB of address
                lda TMPADR2+1
                adc #$00        ; we only want the carry
                sta 4,x         ; MSB of address 
                
                ; get link to Code End (z_xxxx)
                ldy #$05        ; offset for Code End pointer
                lda (TMPADR2),y
                sec
                sbc 3,x
                sta 1,x         ; LSB of number of chars
                iny
                lda (TMPADR2),y  
                sbc 4,x 
                sta 2,x         ; MSB of number of chars 

                jsr l_dump

                ; clean up 
                inx
                inx

z_see:          rts
.scend
; -----------------------------------------------------------------------------
; DOTS  ( -- ) (".S")
; Print content of the stack non-destructively
l_dots:         bra a_dots
                .byte $02 
                .word l_see     ; link to SEE 
                .word z_dots
                .byte ".S"
.scope 
a_dots:         ; Quick test for the most common case: When X is $7F, we've got 
                ; zero entries on the stack 
                txa
                cmp #SP0
                bne _notzero    ; not zero, continue with printing

                ; print "(empty)" string 
                lda #$0E 
                jsr f_prtzerostr

                bra _finished

_notzero:       ; we print from the bottom up and stop when we hit X 
                ldy #SP0
                stx TMPX

_loop:          ; ironically, we need the stack for this  
                dex
                dex

                lda 0,y 
                sta 2,x         ; MSB first 
                dey
                lda 0,y
                sta 1,x         ; then LSB 

                dey
                cpy TMPX
                bmi _done

                phy
                jsr l_dot       ; print number
                ply
                
                bra _loop
                
_done:          ; add a space for readability 
                jsr l_space

                ; drop unused stack entry
                inx
                inx
_finished:
z_dots:         rts
.scend
; ----------------------------------------------------------------------------
; WORD ( c "name" -- c-addr ) Parse input and return counted string. This is 
; the archaic parsing word in Forth, since surplanted by PARSE and PARSE-NAME. 
; It is included here against better judgement because it is used in so many
; examples. WORD skips leading delimiters (PARSE doesn't), returns an old-style
; counted string and has a bunch of other problems. See the discussion at
; http://www.forth200x.org/documents/html/rationale.html#rat:core:PARSE . This 
; word might be deleted if space pressure because too large. 
l_word:         bra a_word
                .byte $04 
                .word l_dots    ; link to DOTS (".S")
                .word z_word
                .byte "WORD"

.scope
a_word:         ; skip over leading delimiters - this is like PARSE-NAME, 
                ; but unlike PARSE
                ldy INP

_iloop:         cpy CIBN        ; quit if we've reached end of input 
                beq _foundchr
                lda (CIBA),y 
                cmp 1,x         ; Ascii of delimiter, LSB is enough 
                bne _foundchr
                iny
                bra _iloop

_foundchr:      ; save index of where word starts
                sty INP

                jsr l_parse     ; returns ( addr u ) 

_makecaddr:     ; Put together a c-addr out of addr u. Note it is 
                ; currently not put in a transient area, but in 
                ; the dictionary. Since we won't be using WORD
                ; anyway that much, we don't care. Remember PAD is
                ; reserved for the user
                lda 3,x         ; move address to ZP for manipulation
                sta TMPADR
                lda 4,x
                sta TMPADR+1

                lda 1,x         ; save length of string in first byte
                sta (CP)

                lda CP          ; beginning of string is old HERE
                sta 3,x
                lda CP+1
                sta 4,x

                inc CP          ; increase CP
                bne +
                inc CP+1

*               tay
                phy             ; we'll need length again to update CP

                dey             ; convert number of chars to offset 

_loop:          lda (TMPADR),y
                sta (CP),y
                dey
                bpl _loop

                pla             ; update CP to after word 
                clc
                adc CP
                sta CP
                bcc _done
                inc CP+1

_done:          ; get rid of u because NOS is c-addr now 
                inx 
                inx

z_word:         rts
.scend
; -----------------------------------------------------------------------------
; PARSE-NAME ( "name" -- addr u ) 
; Parse the input buffer using a space as the delimiter, skipping leading
; spaces. Note this is a special form of PARSE. PARSE-NAME and PARSE 
; replace WORD in modern systems, see ANS Forth documentation for details. 
; Note we only accept buffers that are at most $FF chars in size. CIBN+1 is
; a dummy value that is always zero and included for possible later
; expansion to 16 bit
l_prsnm:        bra a_prsnm
                .byte $0A
                .word l_word    ; link to WORD
                .word z_prsnm
                .byte "PARSE-NAME"
.scope 
a_prsnm:        ; skip leading spaces. Note PARSE doesn't do any skipping
                ldy INP

_loop:          cpy CIBN        ; quit if we've reached end of input 
                beq _nochrs 
                lda (CIBA),y 
                cmp #AscSP
                bne _foundchr
                iny
                bra _loop

_foundchr:      ; save index of where word starts
                sty INP

                ; prepare stack for jump to PARSE 
                dex
                dex
                lda #AscSP      ; use space as delimiter 
                sta 1,x         
                stz 2,x         ; paranoid, always zero 

                jsr l_parse 

                bra _done 

_nochrs:        ; Only spaces found. Return beginning of CIB 
                ; as an address and zero as number on the stack. This 
                ; usually means we're at the end of the line.
                dex
                dex
                dex
                dex 

                ; return end of line as address 
                lda CIBA
                sta 1,x
                lda CIBA+1
                sta 2,x

                ; return zero as number of chars 
                stz 1,x
                stz 2,x         ; fall through to _done 
_done: 
z_prsnm:        rts
.scend
; -----------------------------------------------------------------------------
; PARSE ( c "name" -- addr u ) 
; Parse the input buffer using char as a delimiters. Do not repeat do not 
; skip leading delimiters; this is an important difference to PARSE-NAME. 
; PARSE (and PARSE-NAME) replace WORD in modern systems, see ANS Forth 
; documentation for details. Note we only accept buffers that are at 
; most $FF chars in size, CIBN+1 is a dummy value that is always zero 
; and included for possible later expansion
l_parse:        bra a_parse
                .byte $05 
                .word l_prsnm   ; link to PARSE-NAME 
                .word z_parse
                .byte "PARSE"
.scope                
a_parse:        ; save the delimiter char 
                lda 1,x
                sta TMPCNT      ; not used for counting!

                ; save offset of where word starts
                lda INP
                sta TMPCNT+1    ; save index for later length calculation

                ; save address as first return value for PARSE
                clc
                adc CIBA        ; LSB
                sta 1,x         ; overwrite char 
                lda CIBA+1      ; MSB
                adc #$00
                sta 2,x

                ; now find last character that is not a delimiter 
                stz FLAG        ; offset to save >IN with at end
                ldy INP

_loop:          ; see if we have reached the end of the line. Do this 
                ; comparison first so calculation of new >IN value will work
                cpy CIBN
                beq _eol

                ; not end of line, see if we have found the delimiter
                lda (CIBA),y
                cmp TMPCNT
                beq _found 
                iny
                bra _loop

_found:         ; if we have not reached the end of the line, we want >IN
                ; to point to the next character after the delimiter (not the
                ; delimiter itself) when we are done. This is why we define
                ; an offset and must compare for EOL first
                inc FLAG

_eol:           ; calculate length of string found. This is the same if we 
                ; found the delimiter or the end of the line 
                tya
                sec
                sbc TMPCNT+1    ; location of first char

                ; save length to stack
                dex
                dex
                sta 1,x
                stz 2,x         ; always zero 

                ; calculate new value for >IN, depending if we are at the
                ; end of the line or have just found the delimiter
                tya
                clc
                adc FLAG
                sta INP

                bra _finished

_nochrs:        ; No valid characters found. Return beginning of CIB 
                ; as an address and zero as number on the stack. This 
                ; usually means we're at the end of the line.
                dex
                dex

                ; Nobody really cares about this anyway as far as we can 
                ; tell, but the standard demands it 
                lda CIBA
                sta 1,x
                lda CIBA+1
                sta 2,x

                ; The zero is more important
                dex
                dex
                stz 1,x
                stz 2,x
_finished: 
z_parse:        rts
.scend
; -----------------------------------------------------------------------------
; ACCEPT ( addr n1 -- n2 ) 
; We use this as the main routine to accept and format input by the user
; Note it automatically echos characters. Uses Y, TMPCNT and TMPADR
l_accept:       bra a_accept
                .byte $06 
                .word l_parse   ; link to PARSE
                .word z_accept
                .byte "ACCEPT"
.scope
a_accept:       ; just quit if we were told to get zero bytes
                lda 1,x         ; LSB
                ora 2,x         ; MSB
                beq _gotzero

                .invoke load_addrn

                ; Start with an empty Current Input Buffer (CIB)
                ldy #$00        

_loop:          jsr f_getchr   

                cmp #AscLF      ; end input on line feed
                beq _done
                cmp #AscCR      ; end input with carriage return, too
                beq _done

                ; handle editing characters such as backspace etc
                cmp #AscDEL
                beq _del        ; del and backspace are the same for us
                cmp #AscBS
                bne _chkprt

                ; rubout input character
_del:           cpy #$00        ; if we're already at 0, don't decrease
                beq +
                dey
*               lda #AscBS      ; backspace, only moves cursor back
                jsr f_putchr
                lda #AscSP      ; overwrite char
                jsr f_putchr
                lda #AscBS      ; second backspace, moves cursor back again
                jsr f_putchr

                bra _loop

_chkprt:        ; TODO handle CTRL-c, CTRL-a, etc

                ; Make sure we're inside the printable character range
                cmp #AscSP      ; ASCII Code $20
                bcc _loop       ; too low, try again
                cmp #$7F        ; ASCII for "~"+ 1
                bcs _loop       ; too high, try again

                ; echo char
                jsr f_putchr

                ; We're good (finally)
                sta (TMPADR),y

                iny             ; loop control
                cpy TMPCNT
                beq _done

                bra _loop

_gotzero:       ldy #$00        ; drops through to _done

_done:          ; add a final space as delimiter (paranoid)
                ; TODO see if this is really necessary

                iny           
                lda #AscSP
                sta (TMPADR),y
                dey

                ; return length of string found
                tya 
                inx 
                inx
                sta 1,x         ; LSB
                stz 2,x         ; MSB is always zero

                ; print a final space 
                jsr l_space

z_accept:       rts
.scend
; -----------------------------------------------------------------------------
; FIND ( cs-addr -- addr 0 | xt 1 | xt -1 ) 
; Return the excution token that corresponds to the counted string at cs-addr.

; This word carries lots of historical baggage, such as the counted string
; it expects and that it returns the address instead of consuming it. We could
; optionally use SEARCH-WORDLIST and just dump the word list part, but FIND
; is still expected on systems. We keep the interface required by ANS Forth, 
; but call a different routine, l_findint ("FIND internal") for our stuff. 
; TODO figure out how to compile this
l_find:         bra a_find 
                .byte $04 
                .word l_accept  ; link to ACCEPT
                .word z_find
                .byte "FIND"
.scope
a_find:         ; convert antiquated counted string to normal format
                ; this is a hard-coded version of COUNT, see comments
                ; there. This gives us (addr u) on the stack
                lda (1,x)
                pha 
                inc 1,x
                bne +
                inc 2,x
*               pla
                dex
                dex
                sta 1,x
                stz 2,x

l_findint:      ; This is where FIND is used internally so we don't have to
                ; convert counted strings to (addr n) strings and vice versa

                ; we start with last entry in dictionary
                lda DP          
                sta TMPADR
                lda DP+1
                sta TMPADR+1
                
_sloop:         ; from the Length Byte of the header, get the length of 
                ; this dictionary entry's name string
                ldy #$02        ; length string is always at offset 2
                lda (TMPADR),y
                and #%00011111  ; mask everything except the length
                sta TMPCNT      ; save length of word, it stays in A  

                ; first quick test: see if strings have same length
                cmp 1,x         ; use LSB of length only, ignore MSB 
                bne _nomatch

                ; so now we know they have the same length
                ; second quick test: see if first character is the same
                ldy #$07        ; name string always starts offset 7
                lda (TMPADR),y
                cmp (3,x)       ; address of mystery string
                bne _nomatch

                ; Yep, a match. Time to do it the hard way and compare 
                ; the whole name strings

                ; We don't touch TMPADR because we'll need the link of this
                ; word later one way or another. Copy it to TMPADR2 with
                ; an offset of 7 so both TMPADR1 and TMPADR2 start with the same 
                ; first character of their strings
                lda 3,x         ; LSB of mystery string
                sta TMPADR1
                lda 4,x         ; MSB
                sta TMPADR1+1

                clc
                lda TMPADR
                adc #$07        ; first char of entry's name string
                sta TMPADR2
                lda TMPADR+1
                adc #$00        ; we only care about the carry
                sta TMPADR2+1 

                ; Now compare the strings one char at a time, from back to 
                ; front -- Forth doesn't have many words that are the
                ; same length and also have the the same first and the last 
                ; characters (TRUE and TYPE are examples), but words like 
                ; CHARS and CHAR+ or CELLS and CELL+ take longer if they are 
                ; compared front to back 
                ldy TMPCNT

_cmploop:       dey             ; we already know first char is the same
                beq _found 
                lda (TMPADR1),y
                cmp (TMPADR2),y
                bne _nomatch
                bra _cmploop

_found:         ; If we landed here, we've found the right word. Push xt to 
                ; the stack (same as link which we've conserved in TMPADR). 
                lda TMPADR
                sta 3,x         ; LSB of xt
                lda TMPADR+1
                sta 4,x         ; MSB
                
                ; see if this is an immedate word
                ldy #$02
                lda (TMPADR),y
                bmi _imm 

                ; not immedate
                lda #$FF        ; flag -1 
                sta 1,x         ; LSB
                sta 2,x         ; MSB

                bra _finished

_imm:           ; immediate 
                lda #$01        ; flag 1
                sta 1,x         ; LSB
                stz 2,x         ; MSB always zero

                bra _finished
                
_nomatch:       ; this is not the word we are looking for so get link to 
                ; next dictionary entry. 
                ldy #$03        ; link is always at offset 3
                lda (TMPADR),y  ; LSB
                pha 
                iny 
                lda (TMPADR),y  ; MSB
                sta TMPADR+1
                pla 
                sta TMPADR      ; LSB stays in A for next step
                
                ; quit if we're at the end of the list (link contains 0)
                ora TMPADR+1
                beq _alldone

                bra _sloop

_alldone:       stz 1,x         ; put fail flag ($0000) on stack
                stz 2,x
                
_finished:
z_find:         rts
.scend
; -----------------------------------------------------------------------------
; -TRAILING ( addr u1 -- addr u2) 
; Remove spaces at the end of c-addr. Note we assume that string is at most
; $FF chrs long, which is probably a violation of ANS Forth. Uses TMPADR
l_dashtrl:      bra a_dashtrl
                .byte NC+$09 
                .word l_find    ; link to FIND 
                .word z_dashtrl
                .byte "-TRAILING"
.scope 
a_dashtrl:      ; if length entry is zero, return a zero and leave the 
                ; address part untouched
                lda 1,x         ; LSB of n
                ora 2,x         ; MSB of n (should be zero anyway) 
                beq _gotzero

                lda 3,x         ; LSB of addr 
                sta TMPADR
                lda 4,x         ; MSB of addr
                sta TMPADR+1

                ; ignore MSB of length 
                lda 1,x
                tay
                dey             ; u is length, but we need the offset 

_loop:          lda (TMPADR),y
                cmp #AscSP
                bne _done
                dey
                bne _loop
                
                ; if it was spaces all the way down, fall through to _done
                ; this is probably a rare case, so we use DEY as a hack so
                ; we can use the more common routine
                dey

_done:          iny             ; Y is offset, we need the length 
                tya             ; store new n in LSB
                sta 1,x
                stz 2,x         ; always zero 

_gotzero:       
z_dashtrl:      rts
.scend
; ----------------------------------------------------------------------------
; /STRING (addr1 u1 n -- addr2 u2) 
; Adjust the character string by n characters. Negative numbers enlarge it
; to the left, postive ones cut it off from the left. We only accept strings
; of the length of $FF, in violation of ANS Forth, and also assume that u1
; is less that $FF
l_slstr:        bra a_slstr
                .byte NC+$07 
                .word l_dashtrl ; link to -TRAILING
                .word z_slstr
                .byte "/STRING"
.scope
a_slstr:        ; if n is zero, just return 
                lda 1,x
                ora 2,x
                beq _done 

                lda 5,x         ; LSB of addr1
                clc
                adc 1,x         ; LSB of n 
                sta 5,x
                bcs +
                lda 6,x         ; INC doesn't support 6,X 
                adc #$00        ; we only care about the carry 
                sta 6,x
                
*               lda 3,x         ; LSB of u1
                sec
                sbc 1,x         ; LSB of n 
                sta 3,x         ; drop through to _done 

_done:          ; drop number off stack  
                inx
                inx 

z_slstr:        rts
.scend
; ----------------------------------------------------------------------------
; \ ( "text<EOL>" -- ) BACKSLASH 
; Everything to end of line is a comment, so ignore it  
l_backslash:    bra a_backslash
                .byte NC+IM+$01 
                .word l_slstr   ; link to /STRING 
                .word z_backslash
                .byte $5C       ; ASCII for "\" (backslash) 
.scope
a_backslash:    ; we simply advance >IN to the end of line 
                lda CIBN
                sta INP 
                lda CIBN+1
                sta INP+1                

z_backslash:    rts
.scend
; ----------------------------------------------------------------------------
; PAREN ( -- ) Skip comment that is in parentheses ("(") 
l_paren:        bra a_paren
                .byte NC+CO+IM+$01 
                .word l_backslash    ; link to BACKSLASH
                .word z_paren
                .byte "("
.scope
a_paren:        ; use PARSE to find the ")" closing bracket
                dex
                dex
                lda #$29        ; ASCII for ")" 
                sta 1,x
                stz 2,x         ; paranoid, always zero  

                jsr l_parse

                ; we don't care about what parse returns  
                inx
                inx
                inx
                inx 

z_paren:        rts
.scend
; ----------------------------------------------------------------------------
; DOTPAR ( "string" -- ) (".(") 
; Display the string delimited by ")". Note that this prints immediately,
; regardless if this is interpreted or compiled
; In Forth, this would be defined as  : .( [CHAR] ) PARSE TYPE ; IMMEDIATE 
l_dotpar:       bra a_dotpar
                .byte NC+IM+$02 
                .word l_paren    ; link to PAREN
                .word z_dotpar
                .byte ".("
.scope
a_dotpar:       ; use PARSE to find the end of the string 
                dex
                dex
                lda #$29        ; ASCII for ")" 
                sta 1,x         
                stz 2,x         ; paranoid, always zero  

                jsr l_parse     ; returns (addr u) 

                ; if we were given a zero for length, just return  
                lda 1,x
                ora 2,x
                beq _done 

                ; otherwise, just TYPE to print the string 
                jsr l_type 

_done:
z_dotpar:       rts
.scend
; ----------------------------------------------------------------------------
; SQUOTE  ( "string" -- )  - S" - 
; Store the string delimited by " in memory and return the (addr u) pair. 
; The ANS Forth CORE word set defines this as a compile-only word, but the
; FILE set expands it to the interpreted. This makes it a state-sensitive word. 
l_squote:       bra a_squote
                .byte IM+$02       ; not a compile-only word
                .word l_dotpar     ; link to DOTPAR
                .word z_squote
                .byte "S", $22     ; results in S"
.scope
a_squote:       ; use PARSE to find the end of the string 
                dex
                dex
                lda #$22        ; ASCII for " 
                sta 1,x         
                stz 2,x

                jsr l_parse     ; returns (addr u) 

                ; if we were given a zero for length, just return  
                lda 1,x
                ora 2,x
                bne +

                inx             ; drop garbage from PARSE
                inx
                inx
                inx
                
                bra _done 
                
*               ; if this is being compiled, we have to jump over the string that
                ; we're going to save in memory so we don't slam into it 
                lda STATE
                beq _savestring

                ; this is compiled, add JMP 
                ldy #$00

                lda #$4c        ; opcode for jump, BRA could be too short
                sta (CP),y
                iny

                ; the destination of this jump is the CP plus the length of the
                ; string (in LSB of TOS) plus three bytes for the length of 
                ; the jmp instruction we're putting before the string. We'll use
                ; TMPADR2 for the calcultions
                lda CP+1
                sta TMPADR2+1
                lda CP          ; note first byte is already JMP opcode

                ; add three bytes for jump
                clc
                adc #$03
                bcc +
                inc TMPADR2+1

                ; add length of string
*               clc
                adc 1,x 
                bcc +
                inc TMPADR2+1

                ; we now have the MSB of the target address in TMPADR2+1 and the 
                ; LSB in A. 
*               sta (CP),y      ; LSB
                iny
                lda TMPADR2+1   ; MSB 
                sta (CP),y
                iny

                ; adjust CP like nothing ever happened
                tya
                clc
                adc CP
                sta CP
                bcc _savestring
                inc CP+1
                
                ; common step is to save the string in the dictionary
                ; for safekeeping. We do this regardless of if this is 
                ; compiled or interpreted. ANS only requires the PAD, but
                ; we love our strings

                ; the source address is in NOS. Save that to TMPADR
_savestring:    lda 3,x
                sta TMPADR
                lda 4,x
                sta TMPADR+1

                ; we still have the length of the string in the lower byte of 
                ; TOS. Note we only deal with strings >$FF for the moment
                ; so we ignore the MSB of TOS
                ldy 1,x         ; LSB of TOS
*               lda (TMPADR),y
                sta (CP),y 
                dey
                bpl - 

                ; The old CP is the first byte of the string, so we put that
                ; on NOS, replacing the buffer's address. The length, TOS, 
                ; stays the same
                lda CP          ; LSB
                sta 3,x
                lda CP+1
                sta 4,x         ; MSB

                ; now we have to move the CP by the length of the string, which
                ; is in the LSB of the TOS. Note again max string length is $FF
                ; bytes
                lda 1,x         ; LSB
                clc
                adc CP
                sta CP
                bcc +
                inc CP+1

                ; what happens now depends on the state: if we are in compile mode,
                ; we have more to do, if not, we're done because the address and
                ; length of the string are on the stack already.
*               lda STATE       ; cheat by only looking at the LSB
                beq _done       ; if 0, we're interpreting and everything is done

_compile:       ; This is compiling, so we push these in the dictionary so they
                ; pop up when the routine is called. We have ( addr u ) 
                jsr l_swap      ; ( u addr ) 

                jsr f_cmpljsr
                .word l_plit    ; compile (LITERAL) 
                jsr l_comma     ; compile address, now (u) 
                
                jsr f_cmpljsr
                .word l_plit    ; compile (LITERAL) 
                jsr l_comma     ; compile length, now ( -- ) 

_done:          
z_squote:       rts
.scend
; ----------------------------------------------------------------------------
; DOTQUOTE ( "text" -- ) (.") 
; Print string enclosed by quotes. ANS Forth wants this to be compile-only,
; so we follow that lead though lots of other Forths will always print 
; the string. We just call S" and add a TYPE jump routine to save space.
; Note for printing in interpreted mode, .( is the better choice these 
; days
l_dotq:         bra a_dotq
                .byte IM+CO+$02 
                .word l_squote      ; link to SQUOTE
                .word z_dotq
                .byte ".", $22          ; results in ." 
.scope
a_dotq:         ; we let S" do the heavy lifting
                jsr l_squote 
                ; now we just add TYPE
                jsr f_cmpljsr
                .word l_type

z_dotq:         rts
.scend
; ----------------------------------------------------------------------------
; BELL ( -- )
; Print ASCII character 7 to ring terminal bell. 
l_bell:         bra a_bell
                .byte NC+$04 
                .word l_dotq    ; link to DOTQ (.") 
                .word z_bell
                .byte "BELL"
.scope
a_bell:         lda #$07
                jsr f_putchr 

z_bell:         rts
.scend
; ----------------------------------------------------------------------------
; REGULAR ( -- )
; Change font to normal on an ANSI compatible terminal (reverses BOLD).
; Code is "ESC[0m"
l_regular:      bra a_regular
                .byte NC+$07 
                .word l_bell    ; link to BELL
                .word z_regular
                .byte "REGULAR"
.scope
a_regular:      lda #AscESC
                jsr f_putchr
                lda #$5B        ; ASCII for "["
                jsr f_putchr
                lda #'0
                jsr f_putchr
                lda #'m
                jsr f_putchr

z_regular:      rts
.scend
; ----------------------------------------------------------------------------
; BOLD ( -- )
; Change font to bold on an ANSI compatible terminal. Code is "ESC[1m"
l_bold:         bra a_bold
                .byte NC+$04 
                .word l_regular    ; link to REGULAR
                .word z_bold
                .byte "BOLD"
.scope
a_bold:         lda #AscESC
                jsr f_putchr
                lda #$5B        ; ASCII for "["
                jsr f_putchr
                lda #'1
                jsr f_putchr
                lda #'m
                jsr f_putchr

z_bold:         rts
.scend
; ----------------------------------------------------------------------------
; PAGE ( -- )
; Clears a page if supported by ANSI terminal codes. This is Clear Screen 
; "ESC[2J" plus moving the cursor to the top left of the screen. 
l_page:         bra a_page
                .byte $04 
                .word l_bold    ; link to BOLD
                .word z_page
                .byte "PAGE"
.scope
a_page:         lda #AscESC
                jsr f_putchr
                lda #$5B        ; ASCII for "["
                jsr f_putchr
                lda #'2
                jsr f_putchr
                lda #'J
                jsr f_putchr

                ; move cursor to top left of screen 
                jsr l_zero 
                jsr l_zero
                jsr l_atxy

z_page:         rts
.scend
; ----------------------------------------------------------------------------
; AT-XY ( n m -- )
; On an ANSI compatible terminal, place cursor at row n colum m. Code is 
; ESC[<n>;<m>H Do not use U. to print the numbers because the trailing space
; will not work with xterm (works fine with Mac OS X Terminals, though)
l_atxy:         bra a_atxy
                .byte NC+$05 
                .word l_page    ; link to PAGE
                .word z_atxy
                .byte "AT-XY"
.scope
a_atxy:         lda #AscESC
                jsr f_putchr
                lda #$5B        ; ASCII for "["
                jsr f_putchr
                lda 3,x         ; n (x) is in MSB
                jsr f_byte2hexasc
                lda #$3B        ; ASCII for ";"
                jsr f_putchr
                lda 1,x         ; m (y) is in LSB
                jsr f_byte2hexasc
                lda #'H         ; for Mac OS X 
                jsr f_putchr

                inx             ; 2DROP
                inx
                inx
                inx

z_atxy:         rts
.scend
; -----------------------------------------------------------------------------
; SPACES (u -- ) 
; Display u spaces
l_spaces:       bra a_spaces
                .byte NC+$06 
                .word l_atxy     ; link to AT-XY
                .word z_spaces
                .byte "SPACES"

.scope
a_spaces:       ; don't even start if we got a zero
                lda 2,x         ; MSB
                ora 1,x         ; LSB
                beq _done

                jsr l_space

                lda 1,x
                bne +
                dec 2,x 
*               dec 1,x
                bra a_spaces

_done:          inx
                inx
                
z_spaces:       rts
.scend
; -----------------------------------------------------------------------------
; SPACE ( -- ) 
; Display one space 
l_space:        bra a_space
                .byte NC+$05 
                .word l_spaces  ; link to SPACES
                .word z_space
                .byte "SPACE"

a_space:        lda #AscSP
                jsr f_putchr

z_space:        rts
; -----------------------------------------------------------------------------
; CR ( -- ) 
; Display a line feed. This works on Unix systems and OS X, not sure about 
; Windows systems 

l_cr:           bra a_cr
                .byte NC+$02 
                .word l_space   ; link to SPACE
                .word z_cr
                .byte "CR"

a_cr:           lda #AscLF      ; Line Feed 
                jsr f_putchr

z_cr:           rts
; -----------------------------------------------------------------------------
; BL ( -- c )
; Push ASCII character for space to parameter stack 
l_bl:           bra a_bl
                .byte NC+$02 
                .word l_cr      ; link to CR 
                .word z_bl
                .byte "BL"

a_bl:           dex
                dex
                lda #AscSp
                sta 1,x         ; LSB
                stz 2,x         ; paranoid, always zero 

z_bl:           rts
; ----------------------------------------------------------------------------
; >NUMBER  ( ud1 addr1 u1 -- ud2 addr2 u2 ) 
; Covert number string until bad character is found. The logic here is based
; on the routine by Phil Burk of the same name in pForth; see 
; https://code.google.com/p/pforth/source/browse/trunk/fth/numberio.fth for
; the origional Forth code. pForth is in the public domain. 
l_gtnum:        bra a_gtnum 
                .byte $07 
                .word l_bl      ; link to BL 
                .word z_gtnum
                .byte ">NUMBER"
.scope
a_gtnum:        ; TODO check to see if we have enough stuff on the stack

                ; get number of characters as counter, stop if zero. The 
                ; original pForth code uses ">R" here, we use TMPCNT 
                lda 1,x         ; we only accept 256 chars 
                beq _quit       ; stop right now if we got a zero 

                sta TMPCNT
                inx
                inx             ; now (ud1 addr1) 

_loop:          ; feed each character through DIGIT>NUMBER. 

                ; TODO see if we can't do this all with less pushing 
                ; stuff around on the stack (more pure assembler)

                ; first, get a character ("C@") 
                lda (1,x)       ; get first character
                dex
                dex
                sta 1,x         
                stz 2,x         ; paranoid, always zero; now (ud1 addr1 char)  

                ; next, get the base ("BASE @") 
                dex
                dex
                lda BASE
                sta 1,x
                stz 2,x         ; paranoid, always zero; now (ud1 addr1 char base)

                ; convert single digit through DIGIT>NUMBER
                jsr l_digit     ; now (ud1 addr1 n true) or ( ... char false)
                lda 1,x         ; only check LSB of flag 
                bne _accumulate 

                ; DIGIT>NUMBER returned a false flag, so return what 
                ; we have got so far
                inx
                inx             ; drop flag, now (ud addr char)
                lda TMPCNT      ; number of characters we haven't converted
                sta 1,x
                stz 2,x         ; always zero; now (ud2 addr2 n2)

                rts             ; okay because this won't be native compile

_accumulate:    ; Starting here, we show (ud1) as (ud1l ud1h)

                ; TODO At the moment, this is basically just 1:1 from 
                ; the pForth code with minor translations to assembler. 
                ; We need to figure out the basic math involved and rewrite
                ; it in assembler for speed. 
                inx             ; drop the flag 
                inx             ; now (ud1l ud1h addr1 n) 

                jsr l_swap      ; now (ud1l ud1h n addr1) 

                lda 2,x         ; ">R"
                pha
                lda 1,x
                pha             
                inx
                inx             ; now (ud1l ud1h n) (R: addr1)

                jsr l_swap      ; now (ud1l n ud1h) (R: addr1)

                ; fetch base radix ("BASE @") 
                dex
                dex 
                lda BASE
                sta 1,x
                stz 2,x         ; always zero; now (ud1l n ud1h base) 

                jsr l_umstar    ; "UM*", now (ud1l n h*b-l h*b-h) 

                inx             ; "DROP" 
                inx             ; now (ud1l n h*b-l)

                jsr l_rot       ; "ROT", now (n h*b-l ud1l)

                ; fetch base radix ("BASE @") 
                dex
                dex             
                lda BASE
                sta 1,x
                stz 2,x         ; always zero; now (n h*b-l ud1l base)

                jsr l_umstar    ; "UM*", now (n h*b-l l*b-l l*b-h) 

                jsr l_dplus     ; "D+", now (ud2l ud2h) 

                ; got to next character 
                ; TODO OMG. Rewrite this with less stack-pushing
                dex             ; "R>"
                dex
                pla 
                sta 1,x
                pla
                sta 2,x         ; now (ud2l ud2h addr1) 

                inc 1,x         ; "1+"
                bne +
                inc 2,x         ; now (ud2l ud2h addr1+1)

*               ; decrease counter. Note pForth does this with the Return
                ; Stack, we use TMPCNT 
                dec TMPCNT
                bne _loop 

_quit:          ; put counter on the top of the stack. pForth did this with
                ; R, we used TMPCNT
                dex
                dex 
                lda TMPCNT
                sta 1,x
                stz 2,x         ; always zero; now (ud2l ud2h addr1+n u2) 

z_gtnum:        rts
.scend
; ----------------------------------------------------------------------------
; NUMBER ( addr u -- n -1 | d -1 | addr 0 ) 
; Covert a string to a number, respecting "-" and "+". This is a wrapper
; around >NUMBER, which is called. This must not be natively compiled
l_number:       bra a_number
                .byte $06 
                .word l_gtnum   ; link to >NUMBER
                .word z_number
                .byte "NUMBER"
.scope
a_number:       ; make sure u is not zero 
                lda 1,x
                ora 2,x
                bne + 

                ; since we were given a zero, and that is the FALSE flag, 
                ; we just have to return. RTS is okay here because this 
                ; routine will not be natively compiled
                rts 

*               ; TODO see if we have enough numbers on the stack

                ; See if we have "-" as first char. We've already handled
                ; the case "-" as a separate word, so we don't have to worry
                ; about that 
                stz FLAG2       ; start with assumption of positive number

                lda (3,x)
                cmp #$2D        ; ASCII for "-" 
                bne _stack      ; no minus, continue with numbers 

                ; negative number. Set flag and move to next character
                inc FLAG2       ; non-zero means "minus"

                clc             ; move address one byte down 
                lda 3,x
                inc
                sta 3,x
                bne +
                lda 4,x
                inc
                sta 4,x

*               lda 1,x         ; subtract one from length 
                dec
                sta 1,x


_stack:         ; Prepare stack for >NUMBER
                dex
                dex
                dex
                dex             ; now (addr u x x)

                stz 1,x
                stz 2,x
                stz 3,x
                stz 4,x         ; now (addr u 0 0) 

                jsr l_2swap     ; now (0 0 addr u) 

                ; Hit >NUMBER, returns (ud addr u) 
                jsr l_gtnum 

                ; if u is zero, all chars were converted
                stz FLAG        ; set flag for single cell number (default)
                lda 1,x 
                beq _done

                ; Still have chars left over. In ANS Forth, if this is a dot
                ; (decimal point, "."), it is the sign that we want this to 
                ; be a double number. Traditionally, Forth recognizes other 
                ; characters anywhere in the number ("Starting Forth" in the
                ; 2003 edition lists ", . / - :") as well, but we'll stick 
                ; to the standard for now (see ANS Forth 8.3.1) 

                ; If this is really a double-cell dot, we should have only
                ; one character left over
                lda 1,x
                cmp #$01
                bne _fail 

                ; okay, it's only one char, but is it really a "."?
                lda (3,x)
                cmp #$2E         ; ASCII for "."
                bne _fail 

                ; user is requesting double-cell number, set flag
                lda #$FF
                sta FLAG        ; drops through to _done 

_done:          ; all chars have been converted, so drop the flag and 
                ; return either a double- or single-cell value 
                inx
                inx             ; now (ud addr) on the stack 
                inx
                inx             ; now (ud) on the stack 

                ; handle sign 
                lda FLAG2
                beq _pos        ; we're positive, so no worries

                ; Negative number. In this version, we just convert the whole 
                ; double number instead of figuring out if the user wants 
                ; single or double.
                ; TODO A bit brute force. See if the other way is better 
                lda 3,x
                eor #$FF        ; LSB, NOS  
                clc
                adc #$01
                sta 3,x

                lda 4,x         ; MSB, NOS
                eor #$FF
                adc #$00
                sta 4,x

                lda 1,x         ; LSB, TOS
                eor #$FF
                adc #$00
                sta 1,x

                lda 2,x         ; MSB, TOS
                eor #$FF
                adc #$00
                sta 2,x

_pos:           ; should this be a double-cell number?
                lda FLAG 
                beq _flag       ; single cell, overwrite top number 

                ; yes. Make double-cell number by creating room on the stack
                ; for the success flag. now ( n x | d x ) on stack 
                dex
                dex 

_flag:          ; set flag to success. If this is a single cell number, 
                ; this overwrites the top part of the double number 
                lda #$FF
                sta 1,x
                sta 2,x         ; now (n -1 | d -1) on the sack 

                rts             ; not native compiled so we can just return


_fail:          ; not all characters were converted, and it's not a 
                ; double-cell problem. We've got (ud addr u) and need to
                ; return (addr 0)
                lda 3,x         ; LSB of addr
                sta 7,x
                lda 4,x         ; MSB of addr 
                sta 8,x         ; now (addr udh addr u) on stack

                stz 5,x
                stz 6,x         ; now (addr 0 addr u) on stack

                inx             ; drop top two entries 
                inx
                inx
                inx             ; drop through to _quit
_quit:          
z_number:       rts
.scend
; ----------------------------------------------------------------------------
; DIGIT>NUMBER  ( char base -- n true | char false ) 
; Convert a single ASCII character to a number in the given radix. Inspired
; by Forth instruction DIGIT from pForth by Phil Burk. pForth is in the public
; domain.
l_digit:        bra a_digit
                .byte NC+$0C 
                .word l_number    ; link to NUMBER
                .word z_digit
                .byte "DIGIT>NUMBER"
.scope
a_digit:        lda 3,x         ; get char
                cmp #'0         ; see if we're below "0"
                bcc _notdigit

                ; then see if we're below "9", because then we're a normal
                ; number 
                cmp #'9+1       ; this is ":"
                bcc _checkbase

                ; check to see if we're in the gap between "9" and "A"
                cmp #'A-1       ; this is "@"
                bcc _notdigit
                
                ; probably a letter, so we convert it to upper case
                jsr f_toupper 

                ; get rid of the gap between "9" and "A" so we can treat
                ; the whole range as a number 
                sec
                sbc #$07        ; fall through to _checkbase

_checkbase:     ; make sure our number is inside the base range
                sec
                sbc #'0
                cmp BASE 
                bcc _success 

_notdigit:      ; assumes char still in NOS 
                stz 1,x
                stz 2,x 
               
                bra _done       ; don't use RTS here because of compile 

_success:       sta 3,x         ; put number in LSB 
                stz 4,x         ; paranoid, always zero 

                lda #$FF        ; flag for success
                sta 1,x
                sta 2,x
_done:
z_digit:        rts
.scend
; ----------------------------------------------------------------------------
; BINARY ( -- )
l_binary:       bra a_binary
                .byte NC+$06 
                .word l_digit    ; link to DIGIT
                .word z_binary
                .byte "BINARY"
.scope
a_binary:       lda #$02
                sta BASE

z_binary:       rts
.scend
; -----------------------------------------------------------------------------
; HEX ( -- ) 
; Change base radix to hexidecimal. 
l_hex:          bra a_hex
                .byte NC+$03 
                .word l_binary    ; link to BINARY
                .word z_hex
                .byte "HEX"

a_hex:          lda #$10
                sta BASE

z_hex:          rts
; -----------------------------------------------------------------------------
; DECIMAL ( -- ) 
; Change base radix to decimal.
l_decimal:      bra a_decimal
                .byte NC+$07 
                .word l_hex     ; link to HEX
                .word z_decimal
                .byte "DECIMAL"

a_decimal:      lda #$0A
                sta BASE

z_decimal:      rts
; -----------------------------------------------------------------------------
; SOURCE ( -- c_addr u) 
; Return the address of the Current Input Buffer (CIB) and how many
; characters it contains
l_source:       bra a_source
                .byte NC+$06 
                .word l_decimal ; link to DECIMAL
                .word z_source
                .byte "SOURCE"
.scope
a_source:       ; Address of Input Buffer
                dex             
                dex
                lda CIBA
                sta 1,x         ; LSB
                lda CIBA+1
                sta 2,x         ; MSB

                ; Number of chars in Buffer
                dex             
                dex
                lda CIBN        ; LSB
                sta 1,x
                lda CIBN+1      ; MSB
                sta 2,x         ; should be zero in this version 

z_source:       rts
.scend
; -----------------------------------------------------------------------------
; PLUSSTORE ( x addr -- ) ("+!")
l_plstore:      bra a_plstore
                .byte $02 
                .word l_source  ; link to SOURCE
                .word z_plstore
                .byte "+!"
.scope
a_plstore:      ; Move address to TMPADR so we can work with it 
                lda 1,x         ; LSB
                sta TMPADR
                lda 2,x         ; MSB 
                sta TMPADR+1

                ldy #$00
                lda (TMPADR),y  ; LSB of variable content
                clc 
                adc 3,x         ; LSB of x 
                sta (TMPADR),y

                ; handle MSB with carry 
                iny
                lda (TMPADR),y  ; MSB of variable content
                adc 4,x
                sta (TMPADR),y

_done:          inx             ; 2DROP
                inx
                inx
                inx
                
z_plstore:      rts
.scend
; ----------------------------------------------------------------------------
; CONSTANT ( n -- )
; Could be realized as  CREATE , DOES> @ . We do more in assembler, but let
; CREATE and , ("COMMA") do the heavy lifting. 
; See http://www.bradrodriguez.com/papers/moving3.htm for a primer on how
; this works in various Forths. 
l_constant:     bra a_constant
                .byte $08 
                .word l_plstore         ; link to PLUSSTORE ("+!")
                .word z_constant
                .byte "CONSTANT"
.scope
a_constant:     ; we let CREATE and do the heavy lifting
                jsr l_create

                ; CREATE gives us the subroutine jump to DOVAR, but we want
                ; DOCONST. Go back two bytes and replace the JSR target.
                sec
                lda CP
                sbc #$02
                sta TMPADR
                lda CP+1
                sbc #$00                ; we only care about the borrow
                sta TMPADR+1

                ldy #$00
                lda #<fc_docon
                sta (TMPADR),y
                iny
                lda #>fc_docon
                sta (TMPADR),y

                ; store the byte on the stack in the dictionary
                jsr l_comma 

z_constant:     rts
.scend
; ----------------------------------------------------------------------------
; VALUE ( n -- )
; VALUE is basically the same as CONSTANT, though we can change a VALUE 
; and not a constant. The difference is that TO replaces the number in-place
; in the dictionary, whereas a second CONSTANT adds a new dictionary entry.
l_value:        bra a_value
                .byte $05 
                .word l_constant    ; link to CONSTANT
                .word z_value
                .byte "VALUE"

.scope
a_value:        bra l_constant      ;JSR/RTS, use branch to save a byte
z_value:        
.scend

; ----------------------------------------------------------------------------
; TO ( n "name" -- ) 
; Change the value of a VALUE. Note that in theory this would work with
; CONSTANT as well, but we frown on this behavior. Note that while it is
; in violation of ANS Forth, we can change the number in a VALUE with
; <number> ' <value> >BODY +!   just as you can with Gforth
l_to:           bra a_to
                .byte NC+$02 
                .word l_value   ; link to VALUE
                .word z_to
                .byte "TO"

.scope
a_to:           jsr l_tick      ; ' 
                jsr l_gtbody    ; >BODY
                jsr l_store     

z_to:           rts
.scend
; ----------------------------------------------------------------------------
; 2VARIABLE ( "name" -- )
; Basically this boils down to CREATE 2 CELLS ALLOT a tiny bit quicker
; An alternate Forth definition is CREATE 0 , 0 ,
l_2var:         bra a_2var
                .byte $09
                .word l_to    ; link to TO
                .word z_2var
                .byte "2VARIABLE"
.scope
a_2var:         ; We let CREATE and ALLOT do the heavy lifting, because both
                ; are a bit too large for native coding. CREATE sets the 
                ; two following bytes to zero
                jsr l_create

                dex             ; Push 2 on stack
                dex
                lda #$04        ; two cells are four bytes
                sta 1,x         ; LSB
                stz 2,x         ; MSB paranoid, always zero 

                jsr l_allot 

z_2var:         rts
.scend
; -----------------------------------------------------------------------------
; VARIABLE ( "name" -- ) 
; Basically this boils down to CREATE 1 CELLS ALLOT a tiny bit quicker
; An alternate Forth definition is CREATE 0 , 
l_var:          bra a_var
                .byte $08 
                .word l_2var    ; link to 2VARIABLE
                .word z_var
                .byte "VARIABLE"

a_var:          ; We let CREATE and ALLOT do the heavy lifting, because both
                ; are a bit too large for native coding. CREATE sets the 
                ; two following bytes to zero
                jsr l_create

                dex             ; Push 2 on stack
                dex
                lda #$02        ; one cell is two bytes
                sta 1,x         ; LSB
                stz 2,x         ; MSB paranoid, always zero 

                jsr l_allot 

z_var:          rts
; ----------------------------------------------------------------------------
; PDOES ( -- addr ) ("(DOES>)")
; Compiled part of DOES>. This may not be natively compiled 
; See http://www.bradrodriguez.com/papers/moving3.htm for a discussion of 
; DOES> internal workings and the file create-does.txt for a walkthrough. 
l_pdoes:        bra a_pdoes
                .byte CO+$07 
                .word l_var     ; link to VARIABLE
                .word z_pdoes
                .byte "(DOES>)"

.scope
a_pdoes:        ; Get the address of the machine code that follows 
                ; this instruction, which is always a subroutine jump to 
                ; DODOES 
                pla
                sta TMPADR
                pla
                sta TMPADR+1

                ; Increase it by one because of the way the JSR works
                inc TMPADR
                bne +
                inc TMPADR+1

                ; CREATE automatically added a subroutine jump to DOVAR 
                ; to our new word. We need to replace it with a jump to 
                ; the address we just prepared in TMPADR. CREATE 
                ; has already modified the Dictionary Pointer (DP) so we can 
                ; use that as the base -- we can't just use the CP like
                ; CONSTANT because we don't know which instructions all 
                ; followed the CREATE command. 
*               lda DP
                sta TMPADR2
                lda DP+1
                sta TMPADR2+1

                ; Walking through the header, we get the offset that BRA uses
                ; for the jump to a_xxxx. Because of the way BRA works, we add 
                ; two further bytes, and another byte because we skip the
                ; JSR command
                ldy #$01
                lda (DP),y
                clc 
                adc #$03        ; faster by two 2 cycles that 3 * INC 

                ; now we add this to the new address. Note we don't have to 
                ; clear carry again because the last operation can never 
                ; come close to an overflow -- a byte saved is a byte earned!
                adc TMPADR2
                sta TMPADR2
                bcc +
                inc TMPADR2+1

                ; Now we store the address of the instruction after 
                ; JSR (DOES>) at the beginning of the data field
*               ldy #$00
                lda TMPADR
                sta (TMPADR2),y
                iny
                lda TMPADR+1
                sta (TMPADR2),y

                ; Done. Since we've removed the actual return address off 
                ; the 65c02 stack, we don't smash into the code that is
                ; actually reserved for the word we've just created, but 
                ; go back to whatever the main routine was.
z_pdoes:        rts
.scend
; ----------------------------------------------------------------------------
; DOES>  ( -- )
; Create the payload for defining our own defining words. 
; See http://www.bradrodriguez.com/papers/moving3.htm for a discussion of 
; DOES> internal workings. This may not be native compile
l_does:         bra a_does
                .byte CO+IM+$05 
                .word l_pdoes    ; link to PDOES
                .word z_does
                .byte "DOES>"
.scope
a_does:         ; compile a subroutine jump to (DOES>)
                jsr f_cmpljsr
                .word l_pdoes

                ; compile a subroutine jump to DODOES. This is the 
                ; CFA of the new word
                jsr f_cmpljsr
                .word fc_dodoes

z_does:         rts
.scend
; -----------------------------------------------------------------------------
; CREATE ( "name" -- ) 
; Create entry in dictionary for VARIABLE and others. We ignore all 
; alignment issues since this is an 8 bit machine. CREATE is used in its 
; generic form for variables, and in a modified form by : (COLON)
l_create:       bra a_create
                .byte $06 
                .word l_does    ; link to DOES>
                .word z_create
                .byte "CREATE"
.scope
a_create:       ; see if we were given a name. Ideally, this returns 
                ; (addr n) for the name in the Terminal Input Buffer
                jsr l_prsnm

                ; if we got a zero in return, no word found, complain and quit
                lda 1,x         ; LSB
                ora 2,x         ; MSB
                bne +

                ; Clean up the stack before we return; drops ( addr u ) from
                ; PARSE-NAME 
                inx
                inx
                inx
                inx

                lda #$0a        ; Code for name not found during parsing
                jmp error 

                ; found word, convert name to upper case 
*               jsr f_strtoupper

                ; remember the first free byte of memory. TMPADR now 
                ; contains the first byte of the new directory entry
                lda CP          ; LSB
                sta TMPADR 
                lda CP+1        ; MSB
                sta TMPADR+1

                ; Brutally enforce the limit on 31 character names by cutting 
                ; off LSB at five bits
                lda 1,x         ; we just take LSB of length 
                and #%00011111
                sta TMPCNT      ; save length of name string

                ; We'll need 7 bytes + length of the word name for the 
                ; new dictionary header. Note that this does not contain 
                ; the space for three bytes that are part of the VARIABLE
                ; Data Field, these are added later
                clc
                adc #$07
                sta TMPCNT+1    ; save length of header 

                ; reserve the bytes we'll need for the header. This overwrites 
                ; the length of the name string on the stack
                sta 1,x         ; LSB
                stz 2,x         ; MSB is ignored, we're never that long

                jsr l_allot     ; removes top entry of stack

                ; Now we just walk through the new header, filling it 
                ; byte by byte, using Y as the offset

                ; Byte 1 / offset 0: opcode for Branch Always (BRA) 
                ldy #$00
                lda #$80
                sta (TMPADR),y

                ; Byte 2 / offset 1: Offset to the Code Field Address 
                ; (CFA), which we execute when this word is called. Note
                ; that for most words, the classical difference between 
                ; CFA and Parameter Field Address (PFA) is non-existant.
                iny 
                lda #$04        ; a BRA offset of 0 points to next byte
                adc TMPCNT      ; length of name string 
                sta (TMPADR),y

                ; Byte 3 / offset 2: Length Byte
                iny
                lda TMPCNT      ; length of name string
                sta (TMPADR),y

                ; Byte 4+5 / offset 3+4 : Link Field 
                iny 
                lda DP          ; LSB
                sta (TMPADR),y
                iny
                lda DP+1        ; MSB
                sta (TMPADR),y

                ; Byte 6+7 / offset 5+6: Code End Field. For a variable, we 
                ; point one byte after the end of the Data Field so it will
                ; compile correctly. For creating of a word, we don't care
                ; because we will change this to the correct value in 
                ; SEMICOLON anyway. 
                iny
                tya

                clc 
                adc TMPCNT      ; length of name string
                adc #$05        ; point to one byte after variable space
                adc TMPADR      ; find correct offset 
                sta (TMPADR),y  ; store in Code Ende pointer
                iny
                lda TMPADR+1
                adc #$00        ; we only care about the carry
                sta (TMPADR),y

                ; Starting byte 8 / offset 7: Name string. Its address is now
                ; on the top of the stack. Subtract six so we can use Y for 
                ; both strings 
                iny 
                clc             ; not: SEC
                lda 1,x
                sbc #$06
                sta TMPADR1
                lda 2,x
                sbc #$00        ; we only care about the borrow 
                sta TMPADR1+1

_loop:          ; copy name string to dictionary entry
                lda (TMPADR1),y
                sta (TMPADR),y
                iny
                dec TMPCNT
                bne _loop

                ; Reserve three more bytes for the hardcoded subroutine
                ; call
                dex
                dex
                lda #$03
                sta 1,x         ; LSB
                stz 2,x         ; MSB, always zero

                jsr l_allot

                ; We've reached the Data Field. The first three bytes are 
                ; reserved for hardcoded subroutine call to the 
                ; variable subroutine. The BRA instruction at the beginning
                ; must point here
                lda #$20        ; opcode of JSR 
                sta (TMPADR),y
                iny
                lda #<fc_dovar  ; LSB
                sta (TMPADR),y
                iny
                lda #>fc_dovar  ; MSB
                sta (TMPADR),y

                ; New entry complete. The first byte of this entry is now 
                ; the new last entry in the dictionary
                lda TMPADR      ; LSB 
                sta DP
                lda TMPADR+1    ; MSB
                sta DP+1

                ; get rid of the address left on stack
                inx             
                inx

z_create:       rts
.scend

; ----------------------------------------------------------------------------
; NATCOMP ( -- ) ("NATIVE-COMPILE")
; Set the Native Compile flag of the most recently defined word. 
l_natcomp:      bra a_natcomp
                .byte NC+$0E 
                .word l_create  ; link to CREATE
                .word z_natcomp
                .byte "NATIVE-COMPILE"
.scope
a_natcomp:      ; offset for Length Byte is 2
                ldy #$02
                lda (DP),y
                ora #%01000000  ; make sure bit 5 is set
                sta (DP),y

z_natcomp:      rts
.scend
; ----------------------------------------------------------------------------
; CONLY ( -- ) ("COMPILE-ONLY")
; Set the Compile Only flag of the most recently defined word. The 
; alternative way to do this is to define a word ?COMPILE that makes sure 
; we're in compile mode
l_conly:        bra a_conly
                .byte NC+$0C
                .word l_natcomp         ; link to NATCOMP
                .word z_conly
                .byte "COMPILE-ONLY"

.scope
a_conly:        ; offset for Length Byte is 2
                ldy #$02
                lda (DP),y
                ora #%00100000  ; make sure bit 5 is set
                sta (DP),y

z_conly:        rts
.scend
; -----------------------------------------------------------------------------
; IMMEDIATE  ( -- ) 
; Make sure the most recently defined word is immediate. Will only affect the
; last word in the dictionary. Note that if the word is defined in ROM, this
; will have no affect, but will not produce an error message
l_immed:        bra a_immed
                .byte NC+$09 
                .word l_conly    ; link to COMPILE-ONLY
                .word z_immed
                .byte "IMMEDIATE"

a_immed:        ; offset for Length Byte is 2
                ldy #$02
                lda (DP),y
                ora #%10000000  ; make sure bit 7 is set
                sta (DP),y

z_immed:        rts
; -----------------------------------------------------------------------------
; POSTPONE ( "name" -- ) 
; Compile only word. If word that follows is immediate, compile it instead of
; executing it. If the word that follows it is not immediate, include it in
; such a form that it will be compiled when the word being defined is itself 
; use for a new word. Yes, this is tricky, but rather useful. POSTPONE in this 
; form expects a word (not xt) in the stream (not on the stack). This means 
; that we can't build words with "JSR l_postpo, JSR x" directly.
; TODO test this more 
l_postpo:       bra a_postpo
                .byte IM+CO+$08 
                .word l_immed   ; link to IMMEDIATE
                .word z_postpo
                .byte "POSTPONE"
.scope 
a_postpo:       ; PARSE-NAME and FIND get us xt and the mode flag 
                jsr l_prsnm

                ; if we got a zero, no word was provided, and we complain 
                lda 1,x
                ora 2,x
                bne _findit

                inx             ; dump the (addr n) that we got back anyway
                inx
                inx
                inx

                lda #$0a        ; string code for name not found
                jmp error


_findit:        ; convert to upper case because string must be Forth word
                ; or we are totally screwed
                jsr f_strtoupper

                ; see if the word the stack points to is in the dictionary,
                ; return zero if not. This is the "internal" version of
                ; FIND that doesn't require counted strings
                jsr l_findint

                lda 1,x         ; we only need to check LSB for flag 
                bne pp_found

                ; word not in dictionary, print error and abort
                lda #$0b        ; string code for syntax error
                jmp error
.scend

f_postpo_int:   ; This is the internal version of POSTPONE that lets us use
                ; the funtion in subroutine calls. It is used by adding the
                ; xt of the word we want to postpone AFTER the call:
                ; 
                ;       jsr f_postpo_int
                ;       .word <l_targetword>
                ; 
                ; The stack is not changed. Note this assumes that we know
                ; the word is in the dictionary
.scope 
                ; make room on the stack for xt and flag 
                dex
                dex
                dex
                dex

                ; The 65c02's stack contains xt LSB, xt MSB
                pla
                sta TMPADR
                sta 3,x         ; put xt at NOS because flag goes TOS
                pla
                sta TMPADR+1
                sta 4,x 

                ; see if this is an immedate word
                ldy #$02
                lda (TMPADR),y
                bmi _imm_flag

                ; not immedate
                lda #$FF        ; flag -1 
                sta 1,x         ; LSB
                sta 2,x         ; MSB
                bra _finished

_imm_flag:      ; immediate 
                lda #$01        ; flag 1
                sta 1,x         ; LSB
                stz 2,x         ; MSB always zero

_finished:      ; repair 65c02 stack for return jump 
                lda TMPADR
                clc
                adc #$02
                bcc + 
                inc TMPADR+1
*               tay
                lda TMPADR+1
                pha             ; push MSB first
                phy             ; push LSB

                ; we drop through to pp_found 

pp_found:       ; we start here with (xt f) on the stack, where f will be
                ; $0001 for immediate words and -1 ($FFFF) otherwise
                lda 2,x         ; we use bit 7 of MSB for testing 
                pha             ; wait with the test so we can clear stack
                inx             ; dump flag, leaving xt on TOS
                inx     
                pla 
                bpl _immediate
                
                ; we're not immediate, so enact "deferred compilation"
                ; by including   ' <NAME> COMPILE,  
                ; We already have xt so we just need to make sure it 
                ; gets put in the code

                ; compile (LITERAL)
                jsr f_cmpljsr
                .word l_plit

                ; compile the xt that we have sitting on the stack. We 
                ; can't use COMPILE, because that might decide to natively
                ; compile the xt and ruin everything. 
                jsr l_comma

                ; compile COMPILE,
                jsr f_cmpljsr
                .word l_cmpc
                
                bra _done

_immediate:     ; The word is immediate, so instead of executing it right now,
                ; we compile it. xt is on the stack
                jmp l_cmpc      ; JSR/RTS

_done: 
z_postpo:       rts
.scend
; -----------------------------------------------------------------------------
; LEFT BRACKET ( -- ) ("[") LBRACK
; Enter the interpretation state. This is an immediate and compile-only word. 
l_lbrack:       bra a_lbrack
                .byte IM+CO+NC+$01 
                .word l_postpo  ; link to POSTPONE 
                .word z_lbrack
                .byte "["

a_lbrack:       stz STATE
                stz STATE+1

z_lbrack:       rts
; -----------------------------------------------------------------------------
; RIGHT BRACKET ( -- ) ("]") RBRACK
; Enter the compile state. This is an immediate word. For obvious reasons, it
; cannot be compile-only, and native compile doesn't make sense either 

l_rbrack:       bra a_rbrack
                .byte IM+$01 
                .word l_lbrack  ; link to LEFT BRACKET ("[")
                .word z_rbrack
                .byte "]"

a_rbrack:       lda #$FF
                sta STATE
                sta STATE+1

z_rbrack:       rts
; -----------------------------------------------------------------------------
; SEMICOLON ( -- )  (";")
; End the compilation of a new word into the dictionary. When we enter 
; this, the DP is still pointing to the original last word in the 
; dictionary, CP to the next free byte, and WRKWRD contains the address
; of the word we've been compiling
l_semic:        bra a_semic
                .byte IM+CO+$01         ; This is an immediate word
                .word l_rbrack          ; link to RIGHT BRCKET ("]")
                .word z_semic
                .byte ";"
.scope
a_semic:        ; The current CP will be the byte our Code End link points
                ; to in the header (z_xxxx).
                ldy #$05
                lda CP
                sta (WRKWRD),y
                iny
                lda CP+1
                sta (WRKWRD),y

                ; Allocate one further byte and save the return from 
                ; subroutine (RTS) instruction there
                lda #$60                ; opcode for RTS
                sta (CP) 
                inc CP
                bne +
                inc CP+1

*               ; Save beginning of our word as new last word in dictionary               
                lda WRKWRD
                sta DP
                lda WRKWRD+1
                sta DP+1
                
                ; Word definition complete. Now set compile flag to zero
                ; so we return to interpret mode
                stz STATE
                stz STATE+1

z_semic:        rts
.scend
; -----------------------------------------------------------------------------
; COLON ( "name" -- )  (":")
; Start the compilation of a new word into the dictionary. We do this by 
; using part of the CREATE routine and then filling in the rest by hand 
; later
l_colon:        bra a_colon
                .byte $01 
                .word l_semic   ; link to SEMI-COLON (";")
                .word z_colon
                .byte ":"
.scope
a_colon:        ; if we're already compiling, complain and return
                lda STATE
                ora STATE+1
                beq +

                lda #$0d        ; error code for wrong mode
                jmp error 

*               jsr l_create 

                ; back up three bytes and overwrite the DOVAR jump command
                sec
                lda CP
                sbc #$03
                sta CP
                bcs +
                dec CP+1

                ; we save the link (xt) of the new word to make life easier
                ; when it is time to add it to the dictionary. COLON and 
                ; SEMICOLON are the only routines allowed to access this 
                ; variable; it is read by RECURSE
*               lda DP          ; LSB 
                sta WRKWRD
                lda DP+1        ; MSB 
                sta WRKWRD+1

                ; Basic header complete. Now set compile flag. From now on, 
                ; everything goes in the dictionary 
                lda #$FF
                sta STATE
                sta STATE+1

z_colon:        rts
.scend
; -----------------------------------------------------------------------------
; COMPILE, ( xt -- ) (CMPC)
; Compile the execution token into the current definition. It is an error if 
; we are not in the compile state. Because we are using subroutine threading,
; we can't use , (comma) to compile new words the traditional way. Note we 
; only code native fragments of up to $FF bytes. If the size is larger,
; we silently switch back to subroutine compiling 
l_cmpc:         bra a_cmpc
                .byte CO+$08 
                .word l_colon           ; link to COLON (":") 
                .word z_cmpc
                .byte "COMPILE,"
.scope
a_cmpc:         ; put xt on zero page where we can work with it better
                lda 1,x
                sta TMPADR
                lda 2,x
                sta TMPADR+1

                ; See if the dictionary wants us to compile natively ...
                ldy #$02                ; offset to Length Byte
                lda (TMPADR),y
                and #%01000000          ; mask all but NC bit
                beq _dojump             ; nope, compile a subroutine jump 

                ; ... but check how much code we're talking about here first 
                ldy #$01                ; offset of branch command
                lda (TMPADR),y
                inc                     ; we start two bytes farther down
                inc

                ; generate address of Data Field (a_xxxx) and 
                ; store it in TMPADR1
                clc 
                adc TMPADR              ; LSB
                sta TMPADR1
                lda TMPADR+1            ; MSB 
                bcc + 
                inc 
*               sta TMPADR1+1

                ; get address of last entry in Data Field (z_xxxx)
                ldy #$05                ; offset for Code End Pointer
                lda (TMPADR),y
                sec
                sbc TMPADR1             ; LSB 
                sta TMPADR2             ; LSB of size -- we'll use this 
                iny
                lda (TMPADR),y
                sbc TMPADR1+1           ; MSB 

                ; if the MSB is anything else than zero, the fragment is too
                ; big to be compiled natively and we switch over to compiling
                ; the code as a subroutine jump
                bne _dojump             
                
                ; now the address of the source is in TMPADR1, the 
                ; destination is the Compile Pointer (CP), and the number 
                ; of bytes is in TMPADR2
                dex
                dex
                dex
                dex

                lda TMPADR1
                sta 5,x         ; LSB of source
                lda TMPADR1+1
                sta 6,x         ; MSB of source

                lda CP          ; LSB of destination
                sta 3,x
                lda CP+1        ; MSB of destination 
                sta 4,x

                lda TMPADR2     ; number of bytes to copy 
                pha             ; save it because CMOVE> uses TMPADR2
                sta 1,x
                stz 2,x         ; always zero 

                ; let CMOVE> do the hard work 
                jsr l_cmovegt 
        
                ; now we have to allot the bytes we used 
                dex
                dex

                pla             ; retrieve the length of the fragment 
                sta 1,x
                stz 2,x         ; paranoid, always zero 
                jsr l_allot 
        
                bra _done
                
_dojump:        ; Compile as JSR command. Add the JSR opcode ($20). We 
                ; use Y as an index and later move the CP up
                lda #$20
                sta (CP)

                ldy #$01        
                lda 1,x         ; LSB of address
                sta (CP),y

                iny             
                lda 2,x         ; MSB
                sta (CP),y

                ; allot space we just used 
                lda #$03
                clc
                adc CP
                sta CP
                bcc + 
                inc CP+1 

*               inx             ; drop xt 
                inx
_done: 
z_cmpc:         rts
.scend
; -----------------------------------------------------------------------------
; STATE ( -- addr ) 
; Return the address of the STATE variable. This is a system variable that
; can only be changed by ABORT, QUIT, [, ], : and ; so we can't manipulate it 
; directly. 
l_state:        bra a_state
                .byte NC+$05 
                .word l_cmpc    ; link to COMPILE, 
                .word z_state
                .byte "STATE"
.scope
a_state:        dex
                dex
                lda #STATE
                sta 1,x
                stz 2,x         ; paranoid, always zero 
                
z_state:        rts
.scend
; -----------------------------------------------------------------------------
; EVALUATE ( addr u -- ) 
; Interpret Forth commands from string. We use this routine during boot 
; so that we can compile our own high-level functions
l_eval:         bra a_eval
                .byte $08 
                .word l_state   ; link to STATE
                .word z_eval
                .byte "EVALUATE"
.scope
a_eval:         ; if u is zero, abort 
                lda 1,x 
                ora 2,x
                bne +

                inx             ; clear stack and return 
                inx
                inx
                inx

                rts

                ; save Current Input Buffer (CIB)
*               lda CIBA+1      ; MSB first
                pha
                lda CIBA        ; then LSB
                pha
                lda CIBN+1       
                pha
                lda CIBN        
                pha 

                ; move string addresses to CIB
                lda 1,x         ; u LSB
                sta CIBN
                lda 2,x         ; u MSB (should be zero)
                sta CIBN+1
                lda 3,x         ; addr LSB
                sta CIBA
                lda 4,x         ; addr MSB
                sta CIBA+1

                ; clear >IN
                stz INP
                stz INP+1

                ; call common routine for compile/execute.
                jsr f_compexe

                ; Restore CIB to previous value 
                pla 
                sta CIBN
                pla 
                sta CIBN+1
                pla
                sta CIBA
                pla
                sta CIBA+1

z_eval:         rts
.scend
; -----------------------------------------------------------------------------
; EXECUTE ( xt -- ) 
; run word decribed by xt, dropping it off the stack
l_exe:          bra a_exe
                .byte NC+$07
                .word l_eval    ; link to EVALUTE 
                .word z_exe
                .byte "EXECUTE"
.scope
a_exe:          lda 1,x         ; LSB
                sta IP
                lda 2,x         ; MSB
                sta IP+1

                inx             ; DROP xt 
                inx

                ; Only JMP has the addressing mode we need, and all the 
                ; Forth commands end with a RTS instruction. We fake the 
                ; return address by pushing the correct address to the stack
                ; We'll land on a NOP so we don't have to DEC the return
                ; address
                lda #>_done     ; push MSB first
                pha
                lda #<_done
                pha 

                jmp (IP)

_done:          ; keep the NOP here as the landing site for the indirect 
                ; subroutine jump (easier and quicker than adjusting the
                ; return address on the stack)
                ; TODO this is ugly, see if we can't do better
                ; TODO see if z_exe should point to the NOP instead of the 
                ; RTS 
                nop             

z_exe:          rts
.scend
; -----------------------------------------------------------------------------
; GTNAME ( xt -- addr u) (">NAME")
; Given an execution token, return the address and length of the name string
; suitable for printing with TYPE. This is a non-standard word, and the use
; is different than the version in Gforth
l_gtname:       bra a_gtname
                .byte NC+$05 
                .word l_exe     ; link to EXECUTE
                .word z_gtname
                .byte ">NAME"
.scope
a_gtname:       ; xt is the same as the start address of the dictionary entry's 
                ; header
                lda 1,x         ; LSB
                sta TMPADR
                lda 2,x         ; MSB
                sta TMPADR+1

                ; The offset to the beginning of the Name Field is always
                ; seven bytes from the beginning of the dictionary entry
                ; itself 
                lda #$07
                clc 
                adc TMPADR 
                sta 1,x         ; LSB, overwrite value 
                lda TMPADR+1
                adc #$00        ; we only care about the carry
                sta 2,x

                dex
                dex 

                ; the length of the name string is stored in the second
                ; byte 
                ldy #$02
                lda (TMPADR),y

                ; mask everything except the length
                and #%00011111
                sta 1,x         ; LSB
                stz 2,x         ; paranoid, MSB is always zero 

z_gtname:       rts
.scend
; -----------------------------------------------------------------------------
; GTBODY ( xt -- addr ) (">BODY")
; Return Data Field address (PFA in old versions) of words that were created
; by CREATE. This is "the address that would have been returned if HERE had
; been called after CREATE". Note that >BODY returns garbage for words that
; haven't been defined with CREATE
l_gtbody:       bra a_gtbody
                .byte NC+$05 
                .word l_gtname  ; link to GTNAME (">NAME")
                .word z_gtbody
                .byte ">BODY"

a_gtbody:       ; lucky for us, a xt is the same as the start address
                ; of the dictionary entry's header
                lda 1,x         ; LSB
                sta TMPADR
                lda 2,x         ; MSB
                sta TMPADR+1

                ; The offset to "a_" is in the second byte of the BRA
                ; instruction at the beginning of each entry
                ldy #$01
                lda (TMPADR),y

                ; now we have to add two because of the way BRA works, and
                ; three to skip over the CFA to the PFA (CFA is always three
                ; bytes long). 
                clc
                adc #$05

                ; now add offset to address
                clc             ; paranoid, should never be required
                adc TMPADR      ; LSB
                sta 1,x 
                lda TMPADR+1    ; MSB
                adc #$00        ; we only care about the carry
                sta 2,x 

z_gtbody:       rts
; ----------------------------------------------------------------------------
; BRACKET-TICK ( "name" -- )  (['])
; Used in compilation: Store xt of word "name" in word so that the token is
; placed on the stack during runtime. This cannot really be called inside a
; assembler routine because it looks for the word in the input stream. This
; word cannot be natively compiled.
l_btick:        bra a_btick
                .byte IM+CO+$03
                .word l_gtbody  ; link to >BODY
                .word z_btick   ; dummy, cannot be natively compiled
                .byte "[']"

.scope
a_btick:        jsr l_tick
z_btick:        jmp l_lit       ; JSR/RTS
.scend
; -----------------------------------------------------------------------------
; TICK ( "name" -- xt ) ("'")
; Figure out the execution toke (xt) given the name. We let PARSE-NAME 
; and FIND do the heavy lifting
l_tick:         bra a_tick
                .byte NC+$01 
                .word l_btick;  link to [']
                .word z_tick
                .byte "'"
.scope
a_tick:         jsr l_prsnm

                ; if we got a zero, no word was found, and we complain 
                lda 1,x
                ora 2,x
                bne _gotword

                ; dump the (addr n) that we got back anyway
                inx             
                inx
                inx
                inx

                lda #$0A        ; string code for name not found
                jmp error 

_gotword:       ; see if the word the stack points to is in the dictionary,
                ; returns zero if not 
                jsr f_strtoupper 
                jsr l_findint

                lda 1,x         ; we only need to check LSB for flag 
                bne _found

                ; word not in dictionary, print error and abort
                ; TODO print offending word 

                lda #$0B        ; string code for syntax error
                jmp error

_found:         ; we don't care if word is immediate or whatever, just 
                ; drop the flag, leaving the xt. This is why we can't
                ; use TICK as a part of QUIT. 
                inx
                inx

z_tick:         rts
.scend
; ----------------------------------------------------------------------------
; DEFER ( "name" -- )
; Reserve a name that can be linked to various xt. The ANSI reference
; implementation is  CREATE ['] ABORT , DOES> @ EXECUTE ;  but ['] 
; doesn't lend itself to assembler coding as a jump (expects name in stream). 
l_defer:        bra a_defer
                .byte $05 
                .word l_tick    ; link to TICK
                .word z_defer
                .byte "DEFER"

.scope
a_defer:        jsr l_create
                ; ['] ABORT ,  does nothing more but put the xt of ABORT
                ; in the word, we can do that quicker
                jsr f_cmplword
                .word e_defer

_done:          ; continue with normal Forth word
                jsr l_pdoes     ; DOES> by hand: added (DOES>) and DODOES
                jsr fc_dodoes   
                jsr l_fetch
                jsr l_exe

z_defer:        rts

; Error subroutine for undefined DEFER: Complain and go to ABORT. 
e_defer:        lda #$16        ; code for DEFER ERROR 
                jmp error
.scend
; ----------------------------------------------------------------------------
; DEFERSTORE ( xt2 xt1 -- ) (DEFER!)
; Set the word xt1 (the DEFERed word) to execute xt2. ANS reference
; implementation is   : DEFER! >BODY ! ; 
l_deferstore:   bra a_deferstore
                .byte NC+$06 
                .word l_defer    ; link to DEFER
                .word z_deferstore
                .byte "DEFER!"

.scope
a_deferstore:   jsr l_gtbody    ; >BODY
                jsr l_store     ; !

z_deferstore:   rts
.scend
; ----------------------------------------------------------------------------
; DEFERFETCH ( xt1 -- xt2 ) (DEFER@)
; Fetch the xt that the DEFERed word xt1 is pointing to. ANS reference
; implemenation is   : DEFER@ >BODY @ ; 
l_deferfetch:   bra a_deferfetch
                .byte NC+$06 
                .word l_deferstore    ; link to DEFER!
                .word z_deferfetch
                .byte "DEFER@"

.scope
a_deferfetch:   jsr l_gtbody    ; >BODY
                jsr l_fetch     ; @

z_deferfetch:   rts
.scend
; -----------------------------------------------------------------------------
; COMMA ( x -- ) (",")
; Allot one cell (two bytes) and store x in memory. We ignore all alignment
; issues on the 8 bit machine.
l_comma:        bra a_comma
                .byte NC+$01 
                .word l_deferfetch    ; link to DEFER@
                .word z_comma
                .byte ","
.scope
a_comma:        lda 1,x         ; LSB
                sta (CP)
                inc CP          ; next byte
                bne +
                inc CP+1

*               lda 2,x         ; MSB
                sta (CP)
                inc CP          ; next byte
                bne _done
                inc CP+1

_done:          inx
                inx

z_comma:        rts
.scend
; -----------------------------------------------------------------------------
; ALLOT ( u -- ) 
; If u is zero, do nothing. If u > 0, reserve u address units (bytes). If
; u < 0, release u units of data space, though only to the beginning of the
; RAM originally available. Note we ignore alignment issues completely, as
; this is a 8 bit machine. 
l_allot:        bra a_allot
                .byte $05 
                .word l_comma   ; link to COMMA (",")
                .word z_allot
                .byte "ALLOT"
.scope
a_allot:        ; if we got a zero, forget the whole thing 
                lda 1,x         ; LSB
                ora 2,x         ; MSB
                beq _done

                ; if we have a positive value, reserve space
                lda 2,x
                bmi _neg

                ; positive value
                clc
                lda 1,x         ; LSB
                adc CP
                sta CP
                lda 2,x         ; MSB
                adc CP+1
                sta CP+1

                ; make sure we've not alloted more than we have
                sec
                lda #<RamEnd    ; LSB
                sbc CP          ; don't save result, just need the carry
                lda #>RamEnd    ; MSB
                sbc CP+1
                bpl +           ; We haven't deleted too much, continue

                ; use at most max of RAM
                ; TODO decide if we want to print an error message
                lda #<RamEnd
                sta CP
                lda #>RamEnd
                sta CP+1

*               bra _done

_neg:           ; if we have a negative value, release space
                sec
                lda CP          ; LSB
                sbc 1,x
                sta CP
                lda CP+1        ; MSB
                sbc 2,x
                sta CP+1

                ; free at most to the beginning of the dictionary space
                ; note this destroys the user's dictionary
                sec
                lda #<CP0       ; LSB
                sbc CP          ; don't save result, just need the carry
                lda #>CP0       ; MSB
                sbc CP+1
                bpl _done       ; CP > CP0, we're good

                ; set CP at most to original value
                lda #<CP0
                sta CP
                lda #>CP0
                sta CP+1        ; drops through to done

_done:          inx
                inx

z_allot:        rts
.scend
; -----------------------------------------------------------------------------
; ALIGNED ( adr -- adr ) 
; We can ignore alignment issues with an 8 bit CPU, this is provided for 
; compatibility with other Forths. 
l_aligned:      bra a_aligned
                .byte NC+$07 
                .word l_allot           ; link to ALLOT 
                .word z_aligned
                .byte "ALIGNED"

a_aligned:                      ; compiles empty 
z_aligned:      rts             ; never reached
; -----------------------------------------------------------------------------
; ALIGN ( -- ) 
; We can ignore alignment issues with an 8 bit CPU, this is provided for 
; compatibility with other Forths. 
l_align:        bra a_align
                .byte NC+$05 
                .word l_aligned         ; link to ALIGNED
                .word z_align
                .byte "ALIGN"

a_align:                        ; compiles empty 
z_align:        rts             ; never reached
; -----------------------------------------------------------------------------
; MOVE  ( addr1 addr2 u -- )      
; Copy u "address units" from addr1 to addr2. Since our address units are bytes, 
; this is just a front-end for CMOVE and CMOVE>. This is actually the only one
; of these three words that is in the CORE set. This word must not be natively 
; compiled
l_move:         bra a_move
                .byte $04 
                .word l_align           ; link to ALIGN 
                .word z_move
                .byte "MOVE"
.scope
a_move:         ; abort if number of bytes to move is zero 
                lda 1,x
                ora 2,x
                beq _equal

                ; compare MSB first
                lda 4,x         ; MSB of addr2
                cmp 6,x         ; MSB of addr1
                beq _cmplsb     ; wasn't not helpful, move to LSB

                bcc _tocmoveg   ; we want CMOVE>
                jmp cmoveint    ; CMOVE, JSR/RTS, skip check for zero

_cmplsb:        ; MSB were equal, so do the whole thing over
                lda 3,x         ; LSB of addr2
                cmp 5,x         ; LSB of addr1
                beq _equal      ; LSB is equal as well 

                bcc _tocmoveg   ; we want CMOVE>
                jmp cmoveint    ; CMOVE, JSR/RTS, skip check for zero

_tocmoveg:      jmp cmovegint   ; JSR/RTS, skip check for zero 

_equal:         ; drop three entries from Data Stack
                txa
                clc
                adc #$06
                tax

z_move:         rts
.scend
; -----------------------------------------------------------------------------
; CMOVE> ( addr1 addr2 u -- ) 
; Copy u bytes from addr1 to addr2, going high to low (addr1 is larger than
; addr2). Based on code in Leventhal, Lance A. "6502 Assembly Language 
; Routines", p. 201. This is too large to be compiled natively
l_cmovegt:      bra a_cmovegt
                .byte $06 
                .word l_move            ; link to MOVE 
                .word z_cmovegt
                .byte "CMOVE>"
.scope 
a_cmovegt:      ; abort if number of bytes to move is zero  
                lda 1,x
                ora 2,x
                beq _abort 

cmovegint:      ; move addresses to where we can work with them 
                lda 1,x
                sta TMPCNT
                lda 2,x
                sta TMPCNT+1
                lda 3,x         
                sta TMPADR2     ; use TMPADR2 because easier to remember
                lda 4,x
                sta TMPADR2+1
                lda 5,x
                sta TMPADR1     ; use TMPADR1 because easier to remember 
                lda 6,x
                sta TMPADR1+1
                
                phx             ; we'll need all the registers we have 

                ; move partial page first 
                lda TMPCNT+1
                clc 
                adc TMPADR1+1
                sta TMPADR1+1   ; point to last page of source 

                lda TMPCNT+1
                clc
                adc TMPADR2+1   
                sta TMPADR2+1   ; point to last page of destination 

                ; move the last partial page first
                ldy TMPCNT      ; length of last page
                beq _fullpage

_partial:       dey
                lda (TMPADR1),y
                sta (TMPADR2),y
                cpy #$00
                bne _partial

_fullpage:      ; use the MSB of counter as our page counter 
                ldx TMPCNT+1    ; X is safe on the stack 
                beq _done 

_outerloop:     dec TMPADR1+1   ; back up to previous pages 
                dec TMPADR2+1

_innerloop:     dey
                lda (TMPADR1),y
                sta (TMPADR2),y
                cpy #$00
                bne _innerloop

                dex
                bne _outerloop 

_done:          plx             ; drops through to _abort 

_abort:         ; clear up the stack and leave 
                txa
                clc
                adc #$06
                tax

z_cmovegt:      rts
.scend
; -----------------------------------------------------------------------------
; CMOVE ( addr1 addr2 u -- ) 
; Copy u bytes from addr1 to addr2, going low to high (addr2 is larger than
; addr1). Based on code in Leventhal, Lance A. "6502 Assembly Language 
; Routines", p. 201. This is too large to be compiled natively
l_cmove:        bra a_cmove
                .byte $05 
                .word l_cmovegt         ; link to CMOVE>
                .word z_cmove
                .byte "CMOVE"
.scope 
a_cmove:        ; abort if number of bytes to move is zero 
                lda 1,x
                ora 2,x
                beq _abort 

cmoveint:       ; move addresses to where we can work with them 
                lda 1,x
                sta TMPCNT
                lda 2,x
                sta TMPCNT+1
                lda 3,x         
                sta TMPADR2     ; use TMPADR2 because easier to remember
                lda 4,x
                sta TMPADR2+1
                lda 5,x
                sta TMPADR1     ; use TMPADR1 because easier to remember 
                lda 6,x
                sta TMPADR1+1
                
                phx             ; we'll need all the registers we have 

                ldy #$00
                ldx TMPCNT+1    ; number of pages to move 
                beq _dopartial

_page:          lda (TMPADR1),y
                sta (TMPADR2),y
                iny
                bne _page 

                inc TMPADR1+1
                inc TMPADR2+1
                dex
                bne _page 


_dopartial:     ldx TMPCNT      ; length of last page
                beq _done

_partial:       lda (TMPADR1),y
                sta (TMPADR2),y
                iny

                dex
                bne _partial

_done:          plx             ; drops through to _abort
 
_abort:         ; clear the stack  
                txa
                clc
                adc #$06
                tax

z_cmove:        rts
.scend 
; -----------------------------------------------------------------------------
; C-FETCH (addr -- char ) 
; Get a character (byte) from the address given and push it to the stack
l_cfetch:       bra a_cfetch
                .byte NC+$02
                .word l_cmove           ; link to CMOVE 
                .word z_cfetch
                .byte "C@"

a_cfetch:       lda (1,x)
                sta 1,x
                stz 2,x                 ; always zero 

z_cfetch:       rts
; -----------------------------------------------------------------------------
; C-STORE ( char addr -- ) ("C!")
; Store number given on second place on the stack in the address given. 
; In practive, this ignores the MSB of the char given
; TODO see if we want to store zero in the MSB
l_cstore:       bra a_cstore
                .byte NC+$02
                .word l_cfetch  ; link to CFETCH (C@)
                .word z_cstore
                .byte "C!"

a_cstore:       lda 3,x
                sta (1,x)
                inx
                inx
                inx
                inx

z_cstore:       rts
; -----------------------------------------------------------------------------
; C-COMMA ( char -- ) ("C,")
; Store one byte (char) in the dictionary
l_ccom:         bra a_ccom
                .byte NC+$02 
                .word l_cstore  ; link to CSTORE (C!)
                .word z_ccom
                .byte "C,"
.scope
a_ccom:         lda 1,x         ; we ignore the MSB completely
                sta (CP)

                ; increase pointer to next byte
                inc CP
                bne +
                inc CP+1

                ; TODO make sure haven't allocated more than we have

*               inx
                inx

z_ccom:         rts
.scend
; -----------------------------------------------------------------------------
; CHARS ( n -- n ) 
; Return how many address units n chars are. Since this is an 8 bit machine,
; this does absolutely nothing and is included for compatibility with other
; Forth versions
l_chars:        bra a_chars
                .byte NC+$05 
                .word l_ccom    ; link to CCOM (C,)
                .word z_chars
                .byte "CHARS"

a_chars:                        ; compiles empty 
z_chars:        rts
; -----------------------------------------------------------------------------
; CHAR+ ( addr -- addr ) 
; Add the size of one character to the address given. Since this an 8 bit
; machine, this means adding one
l_charpl:       bra a_charpl
                .byte NC+$05 
                .word l_chars   ; link to CHARS
                .word z_charpl
                .byte "CHAR+" 

.scope
a_charpl:       inc 1,x         ; LSB
                bne _done
                inc 2,x         ; MSB
_done:
z_charpl:       rts
.scend
; -----------------------------------------------------------------------------
; CELLS ( n -- n ) 
; Number of bytes that n cells require. Since this is an 8 bit machine with
; 16 bit cells, we multiply by two. Note: This duplicates the function of 
; 2* on this machine
l_cells:        bra a_cells
                .byte NC+$05 
                .word l_charpl  ; link to CHAR+
                .word z_cells
                .byte "CELLS"

a_cells:        asl 1,x         ; LSB
                rol 2,x         ; MSB 

z_cells:        rts
; -----------------------------------------------------------------------------
; CELL+ ( addr -- addr ) 
; Add the number of bytes ("address units") that one cell needs. Since this is
; an 8 bit machine with 16 bit cells, we add two bytes. 
l_cellpl:       bra a_cellpl
                .byte NC+$05 
                .word l_cells   ; link to CELLS
                .word z_cellpl
                .byte "CELL+"

a_cellpl:       ; This solution is three bytes longer, but two cycles shorter
                ; than the double INC solution (15/26 to 12/28). We stick with
                ; the slightly faster solution. 
                lda 1,x         ; LSB
                clc
                adc #$02
                sta 1,x

                lda 2,x         ; MSB 
                adc #$00        ; we only care about the carry
                sta 2,x 

z_cellpl:       rts
; -----------------------------------------------------------------------------
; FILL (addr u char -- ) 
; Fill u bytes of memory with char starting at addr. Note that this works
; on bytes, not on cells. On an 8-bit machine such as the 65c02, this is
; a serious pain in the rear. This is too long for native compile
l_fill:         bra a_fill
                .byte $04 
                .word l_cellpl  ; link to CELLPL (CELL+)
                .word z_fill
                .byte "FILL"
.scope
a_fill:         ; TODO check if we have enough stuff on the stack 

                ; save the char we're going to store
                lda 1,x
                pha 

                inx             ; drop char, we now have (addr u) 
                inx 

                ; put stack values in zero page
                .invoke load_addrn

                pla 
                ldy #$00
                stx TMPX
                ldx TMPCNT+1    ; load the MSB ("page count")
                beq _fragment   ; we only have a page fragment 
               
_page:          sta (TMPADR),y  ; fill one page
                iny
                bne _page

                inc TMPADR+1    ; handle page counter
                dex
                bne _page       ; drops through to _fragment

_fragment:      ; deal with the fragment                
                cpy TMPCNT
                beq _done 
                sta (TMPADR),y
                iny
                bne _fragment 

_done:          ldx TMPX

z_fill:         rts
.scend
; -----------------------------------------------------------------------------
; ERASE  ( addr u -- ) 
; Fill the memory with zeros starting at addr for u bytes. This is special
; case of FILL, which we just call. This is still small enough that we 
; compile it natively
l_erase:        bra a_erase
                .byte NC+$05 
                .word l_fill    ; link to FILL
                .word z_erase
                .byte "ERASE"

.scope
a_erase:        dex
                dex
                stz 1,x
                stz 2,x

                jsr l_fill      ; don't combine JSR/RTS so we can compile 

z_erase:        rts
.scend
; ----------------------------------------------------------------------------
; MARKER ( "name" -- )
; Create deletion boundry to restore dictionary to earlier state. Note that
; old dictionary entries are not erased, but will be overwritten. 
l_marker:       bra a_marker
                .byte IM+$06 
                .word l_erase   ; link to ERASE
                .word z_marker
                .byte "MARKER"
.scope
a_marker:       ; This is a defining word
                jsr l_create 

                ; add current DP as payload - this is the DP of the 
                ; marker itself
                ldy #$00
                lda DP
                sta (CP),y
                iny
                lda DP+1
                sta (CP),y
                iny 

                ; adjust CP
                tya
                clc
                adc CP
                sta CP
                bcc +
                inc CP+1

                ; continue with normal Forth word
*               jsr l_pdoes     ; DOES> by hand: added (DOES>) and DODOES
                jsr fc_dodoes   

                ; -- DOES> payload --- 
                jsr l_fetch     ; get the old DP

                ; What we get on the stack is the DP of the marker itself, 
                ; but we need the DP of the previous word
                lda 1,x
                sta TMPADR
                lda 2,x
                sta TMPADR+1
                inx
                inx

                ; the link to the previous DP is three bytes down
                ldy #$03
                lda (TMPADR),y
                sta DP
                iny
                lda (TMPADR),y
                sta DP+1

                ; adjust CP to the byte after end of this new word
                ; at offset 5 (the sixth byte) we have the z_* link to 
                ; the end of this word. Use this to get the new Compiler
                ; Pointer (CP) ...
                ldy #$05
                lda (DP),y  ; LSB
                sta CP
                iny
                lda (DP),y  ; MSB
                sta CP+1

                ; ... except that the RAM available points to the address
                ; one byte after that
                inc CP
                bne _done
                inc CP+1

_done: 
z_marker:       rts
.scend
; -----------------------------------------------------------------------------
; UNUSED ( -- u ) 
; Return amount of RAM available ("in address units"). Note that this 
; version of UNUSED currenty ignores the memory used by the PAD and any 
; buffers. Assumes memory is a continuous region starting at $0000
l_unused:       bra a_unused
                .byte NC+$06 
                .word l_marker    ; link to MARKER
                .word z_unused
                .byte "UNUSED"

a_unused:       dex
                dex

                lda #<RamEnd
                sec
                sbc CP
                sta 1,x         ; LSB

                lda #>RamEnd
                sbc CP+1
                sta 2,x         ; MSB

z_unused:       rts
; ----------------------------------------------------------------------------
; PAD ( -- addr ) 
; Return address of a temporary area in free memory for user use. The PAD
; is always a few bytes above the CP. Note ANS Forth defines the PAD as an
; area that is reserved for the user and not used by the system. 
l_pad:          bra a_pad
                .byte NC+$03 
                .word l_unused  ; link to UNUSED
                .word z_pad
                .byte "PAD"
.scope
a_pad:          dex
                dex

                lda CP
                clc
                adc #PadOffset  ; fixed number of bytes 
                sta 1,x         ; LSB 
                lda CP+1
                bcc +
                inc
*               sta 2,x         ; MSB

z_pad:          rts
.scend
; -----------------------------------------------------------------------------
; HERE ( -- addr ) 
; Push address of data space pointer on the data stack
l_here:         bra a_here
                .byte NC+$04 
                .word l_pad    ; link to PAD
                .word z_here
                .byte "HERE"

a_here:         dex
                dex
                lda CP          ; LSB
                sta 1,x
                lda CP+1        ; MSB
                sta 2,x

z_here:         rts
; -----------------------------------------------------------------------------
; BASE ( -- addr ) 
; Push address of base for number conversion on the data stack
; Note that BASE is on zero page so address MSB is always zero
l_base:         bra a_base
                .byte NC+$04 
                .word l_here     ; link to HERE
                .word z_base
                .byte "BASE"

a_base:         dex
                dex
                lda #BASE       ; LSB
                sta 1,x
                stz 2,x         ; MSB, always zero 

z_base:         rts
; -----------------------------------------------------------------------------
; KEY ( -- c ) 
; We accept all characters, not only the ASCII printable range. Note that ANS
; Forth standard says that they should not be echoed
l_key:          bra a_key
                .byte NC+$03 
                .word l_base    ; link to BASE
                .word z_key
                .byte "KEY"

a_key:          jsr f_getchr    ; returns key found in A

                dex
                dex
                sta 1,x         ; LSB
                stz 2,x         ; MSB, always zero 

z_key:          rts
; ----------------------------------------------------------------------------
; TOUPPER  ( char -- char ) 
; If char is lower case letter, convert to uppercase, else leave unchanged
; This is basically just a wrapper for l_toupper 
l_2upper:       bra a_2upper
                .byte NC+$07 
                .word l_key     ; link to KEY
                .word z_2upper
                .byte "TOUPPER"
.scope
a_2upper:       lda 1,x
                jsr f_toupper 
                sta 1,x
                stz 2,x         ; paranoid, always zero  

z_2upper:       rts
.scend
; ----------------------------------------------------------------------------
; BRACKET-CHAR ( "c" -- ) ("[CHAR]")
; CHAR for use during compiling
l_bchar:        bra a_bchar
                .byte CO+IM+$06 
                .word l_2upper    ; link to TOUPPER 
                .word z_bchar
                .byte "[CHAR]"
.scope
a_bchar:        ; get the next character in the stream
                jsr l_prsnm 

                ; if we got back a zero, we have a problem 
                lda 1,x
                ora 2,x
                bne + 

                lda #$0a        ; Code for name not found during parsing
                jmp error 

*               inx             ; drop number of characters, leaving address
                inx 

                lda (1,x)       ; get characters 
                sta 1,x
                stz 2,x         ; MSB always zero

                jsr l_lit       ; LITERAL 

z_bchar:        rts
.scend
; ----------------------------------------------------------------------------
; CHAR  ( "c" -- char ) 
; Take a character string and convert it to the equivalent ASCII number 
l_char:         bra a_char
                .byte NC+$04 
                .word l_bchar    ; link to BCHAR
                .word z_char
                .byte "CHAR"
.scope
a_char:         ; get the next character, returns ( addr u ) 
                jsr l_prsnm

                ; if we got back a zero, we have a problem 
                lda 1,x
                ora 2,x
                bne + 

                lda #$0a        ; Code for name not found during parsing
                jmp error 

*               inx             ; drop number of characters, leave addr 
                inx

                lda (1,x)       ; get character (equivalent to C@) 

                sta 1,x
                stz 2,x         ; MSB is always zero 

z_char:         rts
.scend
; -----------------------------------------------------------------------------
; EMIT ( x -- ) 
; Display character x. We just everything, not only ASCII printable range. 
; EMIT is redirectable: We define "port numbers" that correspond to 
; subroutines in the kernel (and have to be defined for the individual
; hardware). See OUT-PORT and IN-PORT.  Note we ignore the MSB completely
l_emit:         bra a_emit
                .byte NC+$04 
                .word l_char    ; link to CHAR
                .word z_emit
                .byte "EMIT"
.scope
a_emit:         lda 1,x         ; get LSB from stack 
                jsr f_putchr    

                inx
                inx

z_emit:         rts
.scend
; -----------------------------------------------------------------------------
; TYPE ( c-addr u -- ) 
; Print character string if u is not 0. Probably just too long for native 
; compiling
l_type:         bra a_type
                .byte $04 
                .word l_emit    ; link to EMIT
                .word z_type
                .byte "TYPE"
.scope
a_type:         lda 1,x         ; Skip if we got a zero
                ora 2,x
                beq _done

                ; get address 
                lda 3,x
                sta TMPADR
                lda 4,x
                sta TMPADR+1

                ; we only print strings up to a length of $FF char, so
                ; MSB of length is ignored
                ldy #$00

                ; CPY doesn't have the right mode to compare with 1,x so 
                ; we have to do this the hard way
                lda 1,x
                sta TMPCNT

*               lda (TMPADR),y
                jsr f_putchr
                iny 
                cpy TMPCNT
                bne -

_done:          ; clear stack 
                inx
                inx
                inx
                inx

z_type:         rts
.scend
; ----------------------------------------------------------------------------
; >IN ( -- addr )  ("GTIN)
; Return the address of the start of the input buffer to the start of the
; area that is to be parsed.
l_gtin:         bra a_gtin
                .byte NC+$03 
                .word l_type    ; link to TYPE
                .word z_gtin
                .byte ">IN"
.scope
a_gtin:         dex
                dex
                lda #<INP       ; INP is ">IN"
                sta 1,x
                lda #>INP       ; since INP is zero page, this is zero
                sta 2,x

z_gtin:         rts
.scend
; -----------------------------------------------------------------------------
; COUNT ( cs-addr -- c-addr u ) 
; Reformat counted string to a format with address and number of characters
; so type can use it. 
l_count:        bra a_count
                .byte NC+$05 
                .word l_gtin    ; link to GTIN
                .word z_count
                .byte "COUNT"
.scope
a_count:        lda (1,x)       ; Get number of characters (256 max)
                pha 

                ; move start address up by one 
                inc 1,x         ; LSB
                bne +
                inc 2,x         ; MSB

                ; save number of characters to stack 
*               pla
                dex
                dex
                sta 1,x         ; LSB
                stz 2,x         ; MSB, always zero

z_count:        rts
.scend
; ----------------------------------------------------------------------------
; 0< ( n -- f ) 
; Return a TRUE flag if and only if TOS is negative
l_0lt:          bra a_0lt
                .byte NC+$02 
                .word l_count   ; link to COUNT 
                .word z_0lt
                .byte "0<"
.scope
a_0lt:          lda 2,x         ; MSB
                bmi +

                ; TOS is positive, so return FALSE (zero on stack)
                lda #$00
                bra _store 

*               ; TOS is negative, so return TRUE ($FFFF on stack)
                lda #$FF

_store:         sta 1,x
                sta 2,x

z_0lt:          rts
.scend
; -----------------------------------------------------------------------------
; 0= (ZERO EQUAL) ( n -- f ) 
; Return a TRUE flag if and only if TOS is zero
l_0equ:         bra a_0equ
                .byte NC+$02 
                .word l_0lt    ; link to 0<
                .word z_0equ
                .byte "0="
.scope
a_0equ:         lda 1,x        ; LSB 
                ora 2,x        ; MSB
                beq +
                
                ; TOS is not $0000, so store zero on stack 
                lda #$00
                bra _store

*               ; TOS is $0000, so store $FFFF on stack 
                lda #$FF

_store:         sta 1,x
                sta 2,x 

z_0equ:         rts
.scend
; ----------------------------------------------------------------------------
; GREATER THAN  ( n n -- f ) 
; Set flag depending if NOS > TOS 
l_grthan:       bra a_grthan
                .byte NC+$01 
                .word l_0equ     ; link to ZERO EQU ("0=")
                .word z_grthan
                .byte ">"
.scope
a_grthan:       ; compare TOS and NOS
                jsr f_cmp16

                ; for signed numbers, NOS > TOS if Z=0 and N=1
                beq _false
                bpl _false

                lda #$FF 
                bra _done 

_false:         lda #$00        ; drop through to _done 

_done:          sta 3,x
                sta 4,x
                inx
                inx

z_grthan:       rts
.scend
; ----------------------------------------------------------------------------
; EQUAL  ( n n -- f ) "=" 
l_equal:        bra a_equal
                .byte NC+$01 
                .word l_grthan    ; link to ">"
                .word z_equal
                .byte "="
.scope
a_equal:        ; compare LSB and MSB in sequence 
                lda 1,x         ; LSB 
                cmp 3,x
                bne _false 

                lda 2,x         ; MSB
                cmp 4,x
                bne _false

                lda #$FF
                bra _done 

_false:         lda #$00        ; drop through to _done 

_done:          sta 3,x
                sta 4,x
                inx
                inx 

z_equal:        rts
.scend
; ----------------------------------------------------------------------------
; LESS THEN ( n n -- f ) 
l_lessthan:     bra a_lessthan
                .byte NC+$01 
                .word l_equal    ; link to "="
                .word z_lessthan
                .byte "<"
.scope
a_lessthan:     ; compare TOS and NOS
                jsr f_cmp16

                ; for signed numbers, NOS < TOS if Z=0 and N=0
                beq _false
                bmi _false

                lda #$FF 
                bra _done 

_false:         lda #$00        ; drop through to _done 

_done:          sta 3,x
                sta 4,x
                inx
                inx

z_lessthan:     rts
.scend
; -----------------------------------------------------------------------------
; ZERO ( -- 0 ) ("0")
; Commonly used number, hard-coded for speed
l_zero:         bra a_zero
                .byte NC+$01 
                .word l_lessthan    ; link to "<"
                .word z_zero
                .byte "0"

a_zero:         dex
                dex
                stz 1,x
                stz 2,x

z_zero:         rts
; -----------------------------------------------------------------------------
; ONE ( -- 1 ) ("1")
; Commonly used number, hard-coded for speed
l_one:          bra a_one
                .byte NC+$01 
                .word l_zero    ; link to ZERO ("0")
                .word z_one
                .byte "1"

.scope
a_one:          dex
                dex
                lda #$01
                sta 1,x         ; LSB
                stz 2,x         ; MSB

z_one:          rts
.scend
; -----------------------------------------------------------------------------
; TWO ( -- 2 ) ("2")
; Commonly used number, hard-coded for speed
l_two:          bra a_two
                .byte NC+$01 
                .word l_one     ; link to ONE ("1")
                .word z_two
                .byte "2"

a_two:          dex
                dex
                lda #$02
                sta 1,x         ; LSB
                stz 2,x         ; MSB, always zero 

z_two:          rts
; ----------------------------------------------------------------------------
; DDOTR ( d n -- ) ("D.R") 
; Print right-justified signed double-cell number
; Forth code is   >R TUCK DABS <# #S ROT SIGN #> R> OVER - SPACES TYPE
l_ddotr:        bra a_ddotr
                .byte $03 
                .word l_two     ; link to TWO ("2")
                .word z_ddotr
                .byte "D.R"
.scope
a_ddotr:        jsr l_tor       ; >R
                jsr l_tuck      ; TUCK
                jsr l_dabs      ; DABS  
                jsr l_ltnum     ; <#
                jsr l_nums      ; #S
                jsr l_rot       ; ROT
                jsr l_sign      ; SIGN
                jsr l_numgt     ; #>
                jsr l_fromr     ; >R
                jsr l_over      ; OVER 
                jsr l_minus     ; - 
                jsr l_spaces    ; SPACES
                jsr l_type      ; TYPE

z_ddotr:        rts
.scend
; ----------------------------------------------------------------------------
; DDOT ( d -- ) ("D.")
; Print signed double cell number 
; Forth code is  TUCK DABS <# #S ROT SIGN #> TYPE SPACE
l_ddot:         bra a_ddot
                .byte $02 
                .word l_ddotr    ; link to DDOTR
                .word z_ddot
                .byte "D."
.scope
a_ddot:         jsr l_tuck      ; TUCK
                jsr l_dabs      ; DABS  
                jsr l_ltnum     ; <#
                jsr l_nums      ; #S
                jsr l_rot       ; ROT
                jsr l_sign      ; SIGN
                jsr l_numgt     ; #>
                jsr l_type      ; TYPE
                jsr l_space     ; SPACE

z_ddot:         rts
.scend
; ----------------------------------------------------------------------------
; DOTR  ( n u -- ) (".R") 
; Print double cell number, right justified 
; Forth code:  >R DUP ABS 0 <# #S ROT SIGN #> R> OVER - SPACES TYPE 
l_dotr:         bra a_dotr
                .byte $02 
                .word l_ddot    ; link to DDOT
                .word z_dotr
                .byte ".R"
.scope
a_dotr:         jsr l_tor       ; >R
                jsr l_dup       ; DUP
                jsr l_abs       ; ABS 
                jsr l_zero      ; 0
                jsr l_ltnum     ; <#
                jsr l_nums      ; #S
                jsr l_rot       ; ROT
                jsr l_sign      ; SIGN
                jsr l_numgt     ; #>
                jsr l_fromr     ; >R
                jsr l_over      ; OVER 
                jsr l_minus     ; - 
                jsr l_spaces    ; SPACES
                jsr l_type      ; TYPE

z_dotr:         rts
.scend
; -----------------------------------------------------------------------------
; DOT ( n -- ) (".")
; Print single cell signed number on top of stack 
; Fort code is  DUP ABS 0 <# #S ROT SIGN #> TYPE SPACE 
l_dot:          bra a_dot
                .byte $01 
                .word l_dotr    ; link to DOTR
                .word z_dot
                .byte "."

a_dot:          jsr l_dup       ; DUP 
                jsr l_abs       ; ABS 
                jsr l_zero      ; ZERO 
                jsr l_ltnum     ; <#
                jsr l_nums      ; #S
                jsr l_rot       ; ROT 
                jsr l_sign      ; SIGN 
                jsr l_numgt     ; #>
                jsr l_type      ; TYPE
                jsr l_space     ; SPACE

z_dot:          rts
; ----------------------------------------------------------------------------
; UDDOT ( ud -- ) ("UD.") 
; Print unsigned double cell number. This routine is used by various others
; because it is so primitive; native Forth Code is  <# #S #> TYPE SPACE
l_uddot:        bra a_uddot
                .byte $03 
                .word l_dot     ; link to DOT (".") 
                .word z_uddot
                .byte "UD."

.scope
a_uddot:        jsr l_ltnum     ; <#
                jsr l_nums      ; #S
                jsr l_numgt     ; #>
                jsr l_type      ; TYPE
                jsr l_space     ; SPACE

z_uddot:        rts
.scend
; ----------------------------------------------------------------------------
; UDOTR ( ud n -- ) ("U.R")
; Print right-justified unsigned double-cell number
; Forth code is   >R 0 <# #S #> R> OVER - SPACES TYPE 
l_udotr:        bra a_udotr
                .byte $03 
                .word l_uddot    ; link to UDDOT
                .word z_udotr
                .byte "U.R"
.scope
a_udotr:        jsr l_tor       ; >R
                jsr l_zero      ; 0
                jsr l_ltnum     ; <#
                jsr l_nums      ; #S
                jsr l_numgt     ; #>
                jsr l_fromr     ; >R
                jsr l_over      ; OVER 
                jsr l_minus     ; - 
                jsr l_spaces    ; SPACES
                jsr l_type      ; TYPE

z_udotr:        rts
.scend
; -----------------------------------------------------------------------------
; UDOT ( u -- ) ("U.") 
; Print unsigned number on top of stack. Convert single-cell number on TOS
; to double-cell and then call UD.
l_udot:         bra a_udot
                .byte $02 
                .word l_udotr    ; link to UDOTR
                .word z_udot
                .byte "U."
.scope
a_udot:         ; Forth string is 0 UD.
                dex
                dex
                stz 1,x
                stz 2,x

                jsr l_uddot     ; UD.

z_udot:         rts
.scend
; ----------------------------------------------------------------------------
; NUMGT ( d -- addr n ) NUMBER GREATER ("#>") 
; Finish conversion of string, putting string address and length on stack 
; ready for TYPE 
l_numgt:        bra a_numgt
                .byte $02 
                .word l_udot    ; link to UDOT ("U.")
                .word z_numgt
                .byte "#>"
.scope
a_numgt:        ; We overwrite the values for the double cell number, 
                ; saving us a bunch of stack movements. First, put the
                ; address of the string in NOS and TOS
                lda OUTP
                sta 3,x         ; LSB of addr 
                sta 1,x
                lda OUTP+1
                sta 4,x         ; MSB
                sta 2,x

                ; Put number of characters on TOS. We could probably get
                ; away with just the LSB, but we do the whole thing for 
                ; sure 
                jsr l_pad       ; now ( addr addr pad ) on stack 

                sec
                lda 1,x         ; LSB of PAD addres
                sbc 3,x         ; LSB of leftmost character 
                sta 3,x

                lda 2,x         ; same with MSB
                sbc 4,x
                sta 4,x

                inx
                inx 

z_numgt:        rts
.scend
; ----------------------------------------------------------------------------
; SIGN ( n -- ) 
; If n is negative, add a minus to the beginning of the pictured output
; Code based on pForth http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
; Original Forth code is   0< IF ASCII - HOLD THEN 
l_sign:         bra a_sign
                .byte NC+$04 
                .word l_numgt    ; link to NUMGT
                .word z_sign
                .byte "SIGN"
.scope
a_sign:         lda 2,x         ; check MSB of TOS
                bmi _minus

                inx             ; get rid of number
                inx 

                bra _done 
                
_minus:         lda #$2D        ; char "-" 
                sta 1,x         ; overwrite n 
                stz 2,x         ; paranoid, always zero 

                jsr l_hold      ; drop through to _done
_done:
z_sign:         rts
.scend
; ----------------------------------------------------------------------------
; HOLD ( char -- ) 
; Add character to the beginning of the pictured output string
; Code based on pForth http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
; Original Forth code is   -1 HLD +!  HLD @  c!
l_hold:         bra a_hold
                .byte NC+$04 
                .word l_sign    ; link to SIGN
                .word z_hold
                .byte "HOLD"
.scope
a_hold:         ; This is actually pretty sneaky code: The new string is 
                ; constructed from back to front, saving the new character
                ; at the beginning of the output string. Since we use PAD as
                ; a starting address and work backward (!), the string is 
                ; constructed in the space between the end of the dictionary
                ; (as defined by CP) and the PAD. This allows us to satisfy 
                ; the ANS Forth condition that programs don't fool around
                ; with the PAD but still use its address

                ; Decrease the address we save the character to by one
                lda OUTP        ; -1 HLD +!
                bne +
                dec OUTP+1
*               dec OUTP

                ; Save character. We ignore the MSB which should be zero 
                ; anyway
                lda 1,x         ; HLD @ C!
                sta (OUTP)

                ; dump char off stack 
                inx
                inx

z_hold:         rts
.scend
; ----------------------------------------------------------------------------
; NUMS ( ud1 -- ud2 ) NUMBER SIGN S ("#S")
; Convert ud1 based on the rules for # and continue until ud1 is all used up
; Code based on pForth http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
; Original Forth code is  BEGIN # 2DUP OR 0= UNTIL
l_nums:         bra a_nums
                .byte NC+$02 
                .word l_hold    ; link to HOLD
                .word z_nums
                .byte "#S"
.scope
a_nums:         
_loop:          jsr l_num       ; convert a single number ("#"')

                ; stop when the double-cell number on top of stack is zero
                lda 1,x
                ora 2,x
                ora 3,x
                ora 4,x
                bne _loop

z_nums:         rts
.scend
; ----------------------------------------------------------------------------
; NUM ( ud1 -- ud2 ) NUMBER SIGN ("#")
; Divide ud1 by the number in BASE and giving the quotient ud2 and a remainder
; n as the least significant digit of ud1. Convert n to external form and 
; add the resulting character to the beginning of the pictured output string.
; Code based on pForth http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
; pForth is in the public domain. Original Forth code is 
; BASE @  MU/MOD ROT 9 OVER  < IF 7 + THEN  ASCII 0 + HOLD 
l_num:          bra a_num
                .byte NC+$01 
                .word l_nums    ; link to NUMS
                .word z_num
                .byte "#"
.scope
a_num:          jsr l_base      ; BASE
                jsr l_fetch     ; @, now ( ud1 base ) on stack 

                ; This is UD/MOD, though pForth calls for UM/MOD ("MU/MOD")
                jsr l_udmod     ; UD/MOD, now ( rem ud2 )
                jsr l_rot       ; ROT, now ( ud2 rem ) 

                ; convert number that is left over using the alphanumeric 
                ; character table for speed
                phx
                lda 1,x         ; take LSB of remainder, ignore MSB
                tax
                lda alphastr,x
                plx 

                sta 1,x         ; overwrites remainder 
                stz 2,x         ; paranoid, should always be zero

                jsr l_hold      ; HOLD, now ( ud2 ) on stack  

z_num:          rts
.scend
; ----------------------------------------------------------------------------
; LTNUMB ( -- ) LESS-NUMBER ("<#")
; Start the numeric converstion process. This does not set a flag or anything
; else to move us into a conversion mode -- just like gForth, you can call 
; these routines, thought it will probably blow up in your face. Code based 
; on pForth, see http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
; pForth is in the pubic domain. Original Forth is   PAD HLD !  
l_ltnum:        bra a_ltnum
                .byte NC+$02 
                .word l_num    ; link to NUM
                .word z_ltnum
                .byte "<#"
.scope
a_ltnum:        jsr l_pad       ; now ( pad ) on stack 

                lda 1,x         ; LSB of PAD
                sta OUTP
                lda 2,x         ; MSB of PAD 
                sta OUTP+1

                inx
                inx 

z_ltnum:        rts
.scend
; ----------------------------------------------------------------------------
; DMINUS ( d d -- d ) ("D-") 
; Subtract two double cell numbers
; TODO decide what to do with overflow 
l_dminus:       bra a_dminus
                .byte NC+$02 
                .word l_ltnum    ; link to LTNUM
                .word z_dminus
                .byte "D-"
.scope
a_dminus:       ; TODO see if we have enough words on the stack 
                sec

                lda 7,x         ; LSB of lower word
                sbc 3,x         
                sta 7,x

                lda 8,x         ; MSB of lower word
                sbc 4,x
                sta 8,x

                lda 5,x         ; LSB of upper word
                sbc 1,x         
                sta 5,x

                lda 6,x         ; MSB of upper word
                sbc 2,x
                sta 6,x

                inx
                inx
                inx
                inx

z_dminus:       rts
.scend
; ----------------------------------------------------------------------------
; D+ ( d d -- d ) 
; Add two double cell numbers
; TODO decide what to do with overflow
l_dplus:        bra a_dplus
                .byte NC+$02 
                .word l_dminus    ; link to DMINUS
                .word z_dplus
                .byte "D+"
.scope
a_dplus:        ; TODO see if we have enough words on the stack
                clc

                lda 3,x         ; LSB of lower word
                adc 7,x         
                sta 7,x

                lda 4,x         ; MSB of lower word
                adc 8,x
                sta 8,x

                lda 1,x         ; LSB of upper word
                adc 5,x         
                sta 5,x

                lda 2,x         ; MSB of upper word
                adc 6,x
                sta 6,x

                inx
                inx
                inx
                inx

z_dplus:        rts
.scend
; ----------------------------------------------------------------------------
; D>S  ( d -- n ) 
; Convert a double cell number to single-cell. Note that this doesn't respect
; the sign
l_dtos:         bra a_dtos
                .byte NC+$03 
                .word l_dplus    ; link to DPLUS
                .word z_dtos
                .byte "D>S"
.scope
a_dtos:         inx
                inx

z_dtos:         rts
.scend
; ----------------------------------------------------------------------------
; S>D ( n -- d ) 
; Convert a single cell number to double cells
l_stod:         bra a_stod
                .byte NC+$03 
                .word l_dtos    ; link to DTOS
                .word z_stod
                .byte "S>D"
.scope
a_stod:         dex
                dex

                lda 3,x         ; MSB of low byte
                bpl _pos 

                lda #$FF        ; extend negative sign
                sta 1,x
                sta 2,x
                bra _done

_pos:           stz 1,x         ; nope, we're positive
                stz 2,x         ; falls through to done 
_done: 
z_stod:         rts
.scend
; ----------------------------------------------------------------------------
; RSHIFT  ( x u -- x ) 
; Shift cell u bits to the right. We mask the anything except the lower
; 4 bit of u so we can maximally move 16 bit 
l_rshift:       bra a_rshift
                .byte NC+$06 
                .word l_stod    ; link to STOD ("S>D")
                .word z_rshift
                .byte "RSHIFT"
.scope
a_rshift:       ; max 16 bit shift, so we mask everything else. We completely
                ; ignore the MSB of u 
                lda 1,x
                and #%00001111
                beq _done         ; if it is zero, don't do anything

                tay
*               lsr 4,x
                ror 3,x
                dey
                bne - 

_done:          inx
                inx

z_rshift:       rts
.scend
; ----------------------------------------------------------------------------
; LSHIFT ( x u -- x ) 
; Shift cell u bits to the left. We mask the anything except the lower
; 4 bit of u so we shift maximal of 16 bit
l_lshift:       bra a_lshift
                .byte NC+$06 
                .word l_rshift    ; link to RSHIFT
                .word z_lshift
                .byte "LSHIFT"
.scope
a_lshift:       ; max 16 bit shift, so we mask everything else. We completely
                ; ignore the MSB of u 
                lda 1,x
                and #%00001111
                beq _done         ; if it is zero, don't do anything

                tay
*               asl 3,x
                rol 4,x
                dey
                bne - 

_done:          inx
                inx

z_lshift:       rts
.scend
; ----------------------------------------------------------------------------
; MIN ( n n -- n ) 
; Compare TOS and NOS and kept the one that is larger. Adapted from Lance A.
; Leventhal "6502 Assembly Language Subroutines." Negative Flag indicateds
; which number is larger. See also 
; http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html 
; TODO rewrite with f_cmp16
l_min:          bra a_min
                .byte NC+$03 
                .word l_lshift    ; link to LSHIFT
                .word z_min
                .byte "MIN"
.scope
a_min:          ; compare LSB. We do this first to set the Carry Flag
                lda 1,x         ; LSB of TOS
                cmp 3,x         ; LSB of NOS, this sets Carry 

                lda 2,x         ; MSB of TOS
                sbc 4,x         ; LSB of NOS
                bvc _noov       ; no overflow, so skip the next step

                ; handle overflow because we use signed numbers
                eor #$80

_noov:          ; if negative, NOS is larger and needs to be dumped
                bpl _keepnos

                ; move TOS to NOS
                lda 1,x         ; LSB 
                sta 3,x
                lda 2,x         ; MSB
                sta 4,x

_keepnos:       inx
                inx 

z_min:          rts
.scend
; ----------------------------------------------------------------------------
; MAX ( n n -- n ) 
; Compare TOS and NOS and keep the one that is larger. Adapted from Lance A. 
; Leventhal "6502 Assembly Language Subroutines". Negative Flag indicates 
; which number is larger. See also 
; http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html 
; TODO rewrite with f_cmp16
l_max:          bra a_max
                .byte NC+$03 
                .word l_min    ; link to MIN
                .word z_max
                .byte "MAX"
.scope
a_max:          ; compare LSB. We do this first to set the Carry Flag
                lda 1,x         ; LSB of TOS
                cmp 3,x         ; LSB of NOS, this sets Carry 

                lda 2,x         ; MSB of TOS
                sbc 4,x         ; LSB of NOS 
                bvc _noov       ; no overflow, so skip the next step

                ; handle overflow because we use signed numbers
                eor #$80        ; complement negative flag

_noov:          ; if negative, NOS is larger and needs to be kept
                bmi _keepnos

                ; move TOS to NOS 
                lda 1,x
                sta 3,x
                lda 2,x
                sta 4,x

_keepnos:       inx 
                inx
                
z_max:          rts
.scend
; ----------------------------------------------------------------------------
; DNEGATE  ( d -- d ) 
; Negate double cell number
l_dnegate:      bra a_dnegate
                .byte $07 
                .word l_max    ; link to MAX
                .word z_dnegate
                .byte "DNEGATE"
.scope
a_dnegate:      lda 3,x         ; LSB of low cell
                eor #$FF
                clc 
                adc #$01
                sta 3,x

                lda 4,x         ; MSB of low cell
                eor #$FF
                adc #$00        ; we only care about the carry
                sta 4,x

                lda 1,x         ; LSB of high cell
                eor #$FF
                adc #$00
                sta 1,x

                lda 2,x         ; MSB of high cell
                eor #$FF
                adc #$00
                sta 2,x

z_dnegate:      rts
.scend
; ----------------------------------------------------------------------------
; NEGATE  ( n1 -- n2 ) 
; Two's complement
l_negate:       bra a_negate
                .byte NC+$06 
                .word l_dnegate    ; link to DNEGATE
                .word z_negate
                .byte "NEGATE"
.scope
a_negate:       lda 1,x         ; LSB
                eor #$FF
                clc
                adc #$01
                sta 1,x

                lda 2,x         ; MSB
                eor #$FF
                adc #$00        ; we only care about the carry
                sta 2,x

z_negate:       rts
.scend
; -----------------------------------------------------------------------------
; INVERT  ( n -- complement ) 
.scope
l_invert:       bra a_invert
                .byte NC+$06 
                .word l_negate    ; link to NEGATE
                .word z_invert
                .byte "INVERT"

a_invert:       dex
                dex
                lda #$FF
                sta 1,x
                sta 2,x         ; still cotains $FF, continue with MSB

                eor 4,x         
                sta 4,x

                lda 1,x         ; LSB 
                eor 3,x
                sta 3,x

                inx
                inx

z_invert:       rts
.scend 
; -----------------------------------------------------------------------------
; XOR ( x x -- x )
l_xor:          bra a_xor
                .byte NC+$03 
                .word l_invert    ; link to INVERT
                .word z_xor
                .byte "XOR"

a_xor:          lda 1,x         ; LSB
                eor 3,x
                sta 3,x

                lda 2,x         ; MSB
                eor 4,x         
                sta 4,x

                inx
                inx

z_xor:          rts
; -----------------------------------------------------------------------------
; OR ( x x -- x )
l_or:           bra a_or
                .byte NC+$02 
                .word l_xor    ; link to XOR
                .word z_or
                .byte "OR"

a_or:           lda 1,x         ; LSB
                ora 3,x
                sta 3,x

                lda 2,x         ; MSB
                ora 4,x         
                sta 4,x

                inx
                inx

z_or:           rts
; -----------------------------------------------------------------------------
; AND ( x x -- x )  
l_and:          bra a_and
                .byte NC+$03 
                .word l_or    ; link to OR 
                .word z_and
                .byte "AND"
.scope
a_and:          lda 1,x         ; LSB
                and 3,x
                sta 3,x
                
                lda 2,x         ; MSB
                and 4,x
                sta 4,x

                inx
                inx

z_and:          rts
.scend
; ----------------------------------------------------------------------------
; MPLUS ( d n -- d ) ("M+") 
; Add double-cell and single-cell number, producing a double-cell number 
l_mplus:        bra a_mplus
                .byte NC+$02 
                .word l_and      ; link to AND
                .word z_mplus
                .byte "M+"
.scope
a_mplus:        clc
                lda 1,x         ; LSB 
                adc 5,x
                sta 5,x

                lda 2,x         ; MSB
                adc 6,x
                sta 6,x

                bcc _done

                ; inc doesn't have the 1,X mode we need, so we have to do
                ; this the hard way

                lda 3,x
                adc #$00        ; we're only interested in the carry
                sta 3,x
                lda 4,x
                adc #$00        ; again, only interested in the carry
                sta 4,x

_done:          inx
                inx

z_mplus:        rts
.scend
; ----------------------------------------------------------------------------
; /MOD ( n1 n2 -- n3 n4 ) 
; Single cell division, returns remainder n3 and quotient n4
; Forth code is >R S>D R> SM/REM
; ANS Forth allows to use either SM/REM or FM/MOD as routine, we use SM/REM 
; TODO see if we can simplify this in pure assembler 
l_slashmod:     bra a_slashmod
                .byte $04       ; TODO consider making this native compile
                .word l_mplus   ; link to MPLUS
                .word z_slashmod
                .byte "/MOD"
.scope
a_slashmod:     ; we can't temporarily hide the first entry on the stack
                ; by replacing >R by INX INX because S>D would overwrite 
                ; the top value 
                jsr l_tor       ; >R
                jsr l_stod      ; S>D
                jsr l_fromr     ; R>
                jsr l_smrem     ; SM/REM 

z_slashmod:     rts
.scend
; ----------------------------------------------------------------------------
; MOD ( n1 n2 -- n3 ) 
; Divide n1 by n2, giving the single cell remainder n3. ANS Forth allows 
; us to use either SM/REM or FM/MOD as routine, we use whatever /MOD uses
; here 
l_mod:          bra a_mod
                .byte NC+$03
                .word l_slashmod    ; link to SLASHMOD
                .word z_mod
                .byte "MOD"
.scope
a_mod:          jsr l_slashmod  ; Returns ( remain quotient )
                inx             ; drop the quotient 
                inx 

z_mod:          rts
.scend
; ----------------------------------------------------------------------------
; FM/MOD ( d n1 -- n2 n3 ) 
; Floored signed division. Compare SM/REM. n2 is the remainder, n3 is the quotient.
; Based on EForth, which is in the public domain. See http://www.forth.org/eforth.html 
; Original Forth code: DUP 0< DUP >R  IF NEGATE >R DNEGATE R> THEN
; >R DUP 0<  IF R@ + THEN  R> UM/MOD R>  IF SWAP NEGATE SWAP THEN 
; You could also define FM/MOD in Terms of SM/REM, see
; http://www.figuk.plus.com/build/arith.htm
l_fmmod:        bra a_fmmod
                .byte $06 
                .word l_mod     ; link to MOD
                .word z_fmmod
                .byte "FM/MOD"
.scope
a_fmmod:        ; if sign of n1 is negative, negate both n1 and d
                stz FLAG        ; default: n is positive 
                lda 2,x         ; MSB of n1 
                bpl _checkd

                inc FLAG        ; n is negative 
                jsr l_negate    ; NEGATE n1 
                jsr l_tor       ; >R
                jsr l_dnegate   ; DNEGATE
                jsr l_fromr     ; R>

_checkd:        ; if d is negative, add n1 to high cell of d
                lda 4,x         ; MSB of high word of d
                bpl _multi

                clc
                lda 1,x         ; LSB of n
                adc 3,x         ; LSB of dh
                sta 3,x
                lda 2,x         ; MSB 
                adc 4,x
                sta 4,x

_multi:         ; multiply
                jsr l_ummod     ; now ( rem quo ) 

                ; if n was negative, negate the result 
                lda FLAG
                beq _done

                inx             ; pretend that we swap
                inx
                jsr l_negate
                dex             ; move it back
                dex
_done: 
z_fmmod:        rts
.scend
; ----------------------------------------------------------------------------
; SM/REM  ( d n1 -- n2 n3 ) 
; Symmetic signed division. Compare FM/MOD. Based on F-PC 3.6 by Ulrich Hoffmann  
; F-PC is in the public domain. See http://www.xlerb.de/uho/ansi.seq 
; Original Forth Code: 
; OVER >R 2DUP XOR 0< >R ABS >R DABS R> UM/MOD R> ?NEGATE SWAP R> ?NEGATE SWAP 
l_smrem:        bra a_smrem
                .byte $06 
                .word l_fmmod    ; link to FMMOD
                .word z_smrem
                .byte "SM/REM"
.scope
a_smrem:        ; push MSB of high cell of d to stack so we can check its sign later
                lda 4,x
                pha

                ; XOR the MSB of the high cell of d and n1 so we figure out its sign
                ; later as well 
                lda 2,x
                eor 4,x
                pha 

                ; prepare division by getting absolute of n1 and d
                jsr l_abs       ; ABS

                inx             ; pretend we pushed n1 to R
                inx
                jsr l_dabs      ; DABS
                dex
                dex

                ; multiply
                jsr l_ummod     ; UM/MOD 

                ; if the XOR compiled above is negative, negate the quotient (n3)
                pla 
                bpl +
                jsr l_negate

*               ; if d was negative, negate the remainder (n2)
                pla
                bpl _done

                inx             ; pretend we pushed quotient to R
                inx
                jsr l_negate
                dex
                dex
_done: 
z_smrem:        rts
.scend
; ----------------------------------------------------------------------------
; UD/MOD ( ud1 u1 -- u2 ud2 ) 32/16 --> 32
; Divide double cell number by a single-cell number and return the quotient 
; ud2 as TOS in double-cell form and remainder u2
; Based on code from pForth, which is in the public domain. Original Forth is
; >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT
; TODO analyze and convert parts to assembler 
l_udmod:        bra a_udmod
                .byte $06 
                .word l_smrem    ; link to SMREM
                .word z_udmod
                .byte "UD/MOD"
.scope
a_udmod:        jsr l_tor       ; >R
                jsr l_zero      ; 0
                jsr l_rfetch    ; R@
                jsr l_ummod     ; UM/MOD
                jsr l_rot       ; ROT
                jsr l_rot       ; ROT
                jsr l_fromr     ; R>
                jsr l_ummod     ; UM/MOD
                jsr l_rot       ; ROT 

z_udmod:        rts
.scend
; ----------------------------------------------------------------------------
; UM/MOD ( ud u1 -- u2 u3 ) ("UM/MOD")  32/16 -> 16
; Divide double cell number by single number and return the quotient u3 as 
; the TOS and remainder as NOS. All numbers are unsigned. This is the basic 
; division operation the others use. Based on FIG Forth code, modified by 
; Garth Wilson, see http://6502.org/source/integers/ummodfix/ummodfix.htm 
l_ummod:        bra a_ummod
                .byte $06 
                .word l_udmod    ; link to UDMOD
                .word z_ummod
                .byte "UM/MOD"
.scope
a_ummod:        ; TODO see if we have enough stuff on the stack 

                ; prevent division by zero. We currently do not check for 
                ; overflow 
                lda 1,x
                ora 2,x
                bne _notzero

                lda #$09        ; "Division by zero" string 
                jmp error 

_notzero:       ; we loop 17 times 
                lda #$11
                sta TMPCNT

_loop:          ; rotate low cell of dividend one bit left (LSB) 
                rol 5,x
                rol 6,x

                ; loop control 
                dec TMPCNT
                beq _done

                ; rotate hi cell of dividend one bit left (MSB) 
                rol 3,x
                rol 4,x

                stz TMPADR      ; store the bit we got from hi cell MSB
                rol TMPADR 

                ; subtract dividend hi cell minus divisor 
                sec
                lda 3,x
                sbc 1,x
                sta TMPADR+1
                lda 4,x
                sbc 2,x

                tay             ; use Y as temporary storage
                lda TMPADR      ; include bit carried 
                sbc #$00
                bcc _loop 

                ; make result new dividend hi cell
                lda TMPADR+1
                sta 3,x
                sty 4,x         ; used as temp storage
                
                bra _loop

_done:          ; drop on from the data stack, swap quotient and remainder 
                inx
                inx
                jsr l_swap

z_ummod:        rts
.scend
; -----------------------------------------------------------------------------
; MSTAR ( n n -- dn ) ("M*")
; Multiply two 16 bit numbers, producing a 32 bit result. All numbers are 
; signed. This is adapted from FIG Forth code, the original Forth code is 
; OVER OVER XOR >R ABS SWAP ABS UM* R> D+-  where "D+-" is O< IF DNEGATE THEN 
; FIG Forth is in the public domain
l_mstar:        bra a_mstar
                .byte $02 
                .word l_ummod    ; link to UM/MOD
                .word z_mstar
                .byte "M*"
.scope
a_mstar:        ; figure out the sign
                lda 2,x         ; MSB of n1
                eor 4,x         ; MSB of n2
                pha

                ; get the absolute value of both numbers
                ; TODO see if this can be coded more effectively in pure
                ; assembler
                jsr l_abs
                jsr l_swap
                jsr l_abs

                jsr l_umstar    ; now ( d ) on stack 

                ; handle the sign 
                pla
                bpl _done
                jsr l_dnegate
_done:
z_mstar:        rts
.scend
; -----------------------------------------------------------------------------
; UMSTAR ( n n -- dn ) ("UM*")
; Multiply two 16 bit numbers, producing a 32 bit result. Everything is 
; unsigned. This is based on modified FIG Forth code by Dr. Jefyll, see  
; http://forum.6502.org/viewtopic.php?f=9&t=689 for a detailed discussion. We
; use the system scratch pad (SYSPAD) for temp storage (N in the original code)
; FIG Forth is in the public domain. Note old Forth versions such as FIG Forth
; call this "U*"
l_umstar:        bra a_umstar
                .byte $03 
                .word l_mstar    ; link to MSTAR
                .word z_umstar
                .byte "UM*"
.scope 
a_umstar:       ; clear carry for safety 
                clc
                
                ; copy top of the stack to SYSPAD+2, SYSPAD+3. To eliminate 
                ; CLC inside the loop, this value is reduced by 1 in advance. 
                lda 1,x         ; LSB
                sbc #$00
                sta SYSPAD+2
                lda 2,x         ; MSB 
                sbc #$00

                ; if top of the stack is zero, deal with it specially
                bcc _zero
                sta SYSPAD+3

                ; Put $00 in both A and SYSPAD. 
                ; First eight shifts are A --> SYSPAD --> 3,x
                ; Final eight shifts are A --> SYSPAD --> 4,x
                lda #$00
                sta SYSPAD 

                stx SYSPAD+4    ; tested later for exit from outer loop 
                dex
                dex

_outerloop:     ldy #$08 
                lsr 5,x         ; think "3,x" and later "4,x"

_innerloop:     bcc _noadd
                sta SYSPAD+1    ; save time by not CLC, see above
                lda SYSPAD
                adc SYSPAD+2
                sta SYSPAD
                lda SYSPAD+1
                adc SYSPAD+3

_noadd:         ror             ; shift
                ror SYSPAD
                ror 5,x         ; think "3,x" and later "4,x"
                dey 
                bne _innerloop  ; go back for one more shift?

                inx
                cpx SYSPAD+4
                bne _outerloop  ; go back for eight more shifts?

                ; all done
                sta 2,x         ; MSB of high word of result
                lda SYSPAD
                sta 1,x         ; LSB of high word of result 
                bra _done

_zero:          stz 3,x
                stz 4,x         ; drop through to RTS
_done: 
z_umstar:       rts
.scend 
; ----------------------------------------------------------------------------
; SSMOD (n1 n2 n3 -- n4 n5 ) STAR-SLASH-MOD ("*/MOD")
; Multiply n1 and n2, producing an intermediary result that is double celled,
; then divide by n3 and produce quotient n5 and remainder n4. We use SM/REM 
; and not FM/MOD in Tali Forth. ANS Forth standard gives >R M* R> SM/REM as 
; implementation
l_ssmod:        bra a_ssmod
                .byte NC+$05    ; native compile because of length 
                .word l_umstar  ; link to UMSTAR  ("UM*")
                .word z_ssmod
                .byte "*/MOD"
.scope
a_ssmod:        ; Instead of >R and R> to temporary push stuff on and off 
                ; the stack, we just lower the stack pointer so that M* 
                ; doesn't see that entry
                inx             ; replaces >R
                inx 
                jsr l_mstar     ; M*
                dex             ; replaces R>
                dex
                jsr l_smrem     ; SM/REM, could also be FM/MOD 
 
z_ssmod:        rts
.scend
; ----------------------------------------------------------------------------
; STARSLASH ( n1 n2 n3 -- n4 ) ("*/")
; Multiply n1 and n2, producing an intermediary result that is double celled,
; the divide by n3. We use SM/REM instead of FM/MOD 
; ANS Forth standard gives >R M* R> SM/REM SWAP DROP as implementation but
; we just call */ and then adapt the result
l_starslash:    bra a_starslash
                .byte NC+$02    ; native compile because of length
                .word l_ssmod   ; link to SSMOD
                .word z_starslash
                .byte "*/"
.scope
a_starslash:    jsr l_ssmod     ; */MOD
                jsr l_swap      ; SWAP 
                inx             ; DROP 
                inx     

z_starslash:    rts
.scend
; ----------------------------------------------------------------------------
; SLASH ( n n -- n ) ("/") 
; Divide single cell number by another single cell number, with sign. ANS Forth
; allows either FM/MOD as base or SM/REM. We use SM/REM and drop the 
; remainder 
; TODO see if we can simplify this 
l_slash:        bra a_slash
                .byte $01 
                .word l_starslash    ; link to STARSLASH ("*/")
                .word z_slash
                .byte "/"
.scope
a_slash:        ; we can't replace >R by INX INX and R> by DEX DEX because
                ; S>D would overwrite old value 
                jsr l_tor       ; >R
                jsr l_stod      ; S>D
                jsr l_fromr     ; R>
                jsr l_smrem     ; SM/REM 
                jsr l_swap      ; SWAP
                inx             ; DROP 
                inx 

z_slash:        rts
.scend
; -----------------------------------------------------------------------------
; STAR ( n n -- n ) ("*")
; Multiply two signed 16 bit numbers, producing a 16 bit result. This is nothing more
; than  UM* DROP  which strangely enough works
l_star:         bra a_star
                .byte NC+$01 
                .word l_slash    ; link to SLASH
                .word z_star
                .byte "*"

a_star:         jsr l_umstar
                inx
                inx

z_star:         rts
; -----------------------------------------------------------------------------
; MINUS ( x x -- x ) ("-")
l_minus:        bra a_minus
                .byte NC+$01 
                .word l_star    ; link to STAR 
                .word z_minus
                .byte "-"

a_minus:        sec
                lda 3,x         ; LSB
                sbc 1,x
                sta 3,x

                lda 4,x         ; MSB
                sbc 2,x
                sta 4,x

                inx
                inx

z_minus:        rts
; -----------------------------------------------------------------------------
; PLUS ( x x -- x ) ("+")
; Add the top cells on stack. 
l_plus:         bra a_plus
                .byte NC+$01 
                .word l_minus    ; link to MINUS 
                .word z_plus
                .byte "+"

a_plus:         clc
                lda 1,x         ; LSB
                adc 3,x
                sta 3,x

                lda 2,x         ; MSB
                adc 4,x
                sta 4,x

                inx
                inx

z_plus:         rts
; ----------------------------------------------------------------------------
; DABS ( d -- ud ) 
; Return the absolute value of a double number 
l_dabs:         bra a_dabs
                .byte NC+$04 
                .word l_plus     ; link to PLUS
                .word z_dabs
                .byte "DABS"
.scope
a_dabs:         lda 2,x         ; MSB of high cell
                bpl _done       ; positive, we're done 

                ; negative: calculate 0 - d
                ldy #$00
                sec

                tya 
                sbc 3,x         ; LSB, low cell
                sta 3,x

                tya             ; MSB, low cell
                sbc 4,x
                sta 4,x

                tya
                sbc 1,x         ; LSB, high cell
                sta 1,x

                tya
                sbc 2,x         ; MSB, high cell
                sta 2,x

_done: 
z_dabs:         rts
.scend
; -----------------------------------------------------------------------------
; ABS ( n -- u ) 
; Return the absolute value of a single number 
l_abs:          bra a_abs
                .byte NC+$03 
                .word l_dabs    ; link to DABS
                .word z_abs
                .byte "ABS"
.scope
a_abs:          lda 2,x         ; MSB
                bpl _done       ; positive number, we're already done

                ; negative: calculate 0 - n  
                sec
                lda #$00
                sbc 1,x         ; LSB
                sta 1,x

                lda #$00
                sbc 2,x         ; LSB
                sta 2,x

_done: 
z_abs:          rts
.scend
; ----------------------------------------------------------------------------
; RECURSE ( -- ) Copy recursive call to word currently being defined
; This may not be native compile. Test with 
; " : GCD ( a b -- gcd) ?DUP IF TUCK MOD RECURSE THEN ;" for instance with
; "784 48 GCD ." --> 16 ; example from
; http://galileo.phys.virginia.edu/classes/551.jvn.fall01/primer.htm
l_recurse:      bra a_recurse
                .byte IM+CO+$07 
                .word l_abs     ; link to ABS
                .word z_recurse
                .byte "RECURSE"
.scope
a_recurse:      ; the whole routine amounts to compiling a reference to 
                ; the word that is being compiled. First, we save the JSR
                ; instruction
                ldy #$00

                lda #$20        ; opcode for JSR
                sta (CP),y
                iny 

                ; next, we save the LSB and MSB of the xt of the word 
                ; we are currently working on, which is saved in WRKWRD
                lda WRKWRD      ; LSB
                sta (CP),y
                iny
                lda WRKWRD+1    ; MSB
                sta (CP),y
                iny

                ; update CP
                tya
                clc
                adc CP
                sta CP
                bcc z_recurse
                inc CP+1

z_recurse:      rts
.scend
; ----------------------------------------------------------------------------
; LEAVE ( -- ) Leave DO/LOOP construct. Note that this does not work with 
; anything but a DO/LOOP in contrast to other versions such as discussed at
; http://blogs.msdn.com/b/ashleyf/archive/2011/02/06/loopty-do-i-loop.aspx
; ": LEAVE POSTPONE BRANCH HERE SWAP 0 , ; IMMEDIATE COMPILE-ONLY"
; See loops.txt on details of how this works. This must be native compile
; and not IMMEDIATE
l_leave:        bra a_leave
                .byte NC+CO+$05 
                .word l_recurse    ; link to RECURSE
                .word z_leave
                .byte "LEAVE"
.scope
a_leave:        ; we dump the limit/start entries off the Return Stack
                ; (four bytes), so we need four PLAs 
                pla
                pla
                pla
                pla

                rts     ; keep this before z_leave so it is compiled

z_leave:        nop     ; dummy, not reached, not compiled
.scend
; ----------------------------------------------------------------------------
; PPLOOP ( n -- ) ("(+LOOP)")
; Runtime compile for loop control. This is used for both +LOOP and LOOP which
; are defined at high level. Note we use a fudge factor for loop 
; control so we can test with the Overflow Flag. See (DO) for details.
; This is Native Compile. The step value is TOS in the loop
l_pploop:       bra a_pploop
                .byte NC+CO+$07 
                .word l_leave    ; link to LEAVE
                .word z_pploop
                .byte "(+LOOP)"
.scope
a_pploop:       clc
                pla             ; LSB of index
                adc 1,x         ; LSB of step
                tay             ; temporary storage of LSB

                clv
                pla             ; MSB of index
                adc 2,x         ; MSB of step
                pha             ; put MSB of index back on stack

                tya             ; put LSB of index back on stack
                pha

                inx             ; dump step from TOS 
                inx

                ; if V flag is set, we're done looping and continue
                ; after the +LOOP instruction
                bvs _hack+3     ; skip over JMP instruction

_hack:          ; This is why this routine must be natively compiled: We 
                ; compile the opcode for JMP here without an address to 
                ; go to, which is added by the next next instruction of
                ; LOOP/+LOOP during compile time
                .byte $4C 

z_pploop:       rts             ; never reached
.scend
;----------------------------------------------------------------------------
; PLOOP ( addr -- ) ("+LOOP") 
; Compile-time part of +LOOP, is usually realized in Forth as 
; ": +LOOP POSTPONE (+LOOP) , POSTPONE UNLOOP ; IMMEDIATE COMPILE-ONLY"
; Note that LOOP uses this routine as well. We jump here with the address
; for looping as TOS, and the address for aborting the loop (LEAVE) as the
; second double-byte entry on the Return Stack (see DO and loops.txt for
; details).
l_ploop:        bra a_ploop
                .byte IM+CO+$05 
                .word l_pploop  ; link to (+LOOP)
                .word z_ploop
                .byte "+LOOP"

.scope
a_ploop:        ; compile (+LOOP) -- don't call f_cmpljsr because this must 
                ; be natively compiled
                dex
                dex
                lda #<l_pploop  ; add xt of (+LOOP) to the stack
                sta 1,x
                lda #>l_pploop
                sta 2,x
                jsr l_cmpc

                ; The address we need to loop back to is TOS
                ; Store it so (+LOOP) jumps back up there
                jsr l_comma

                ; compile an UNLOOP for when we're all done
                dex
                dex
                lda #<l_unloop  
                sta 1,x
                lda #>l_unloop
                sta 2,x
                jsr l_cmpc

                ; complete compile of DO/?DO by replacing the six
                ; dummy bytes by PHA instructions. The address where 
                ; they are located is below the RTS on the Return 
                ; Stack. This is a lot of work for something so little
                ; and can probably be improved
                ply             ; LSB
                sty TMPCNT      ; not used as counter 
                ply             ; MSB

                pla             ; LSB of target address
                sta TMPADR
                pla             ; MSB
                sta TMPADR+1

                ; replace RTS address before something bad happens
                phy             ; MSB
                ldy TMPCNT
                phy             ; LSB

                ; because of the way that RTS works we don't need to 
                ; save CP, but CP-1
                sec
                lda CP
                sbc #$01        ; DEC doesn't affect C-flag
                sta TMPADR1
                lda CP+1
                bcs +
                dec
*               sta TMPADR1+1
                
                ; now compile this in the DO/?DO routine
                ldy #$00

                lda #$A9        ; opcode for LDA immediate
                sta (TMPADR),y
                iny
                lda TMPADR1+1   ; MSB
                sta (TMPADR),y
                iny
                lda #$48        ; Opcode for PHA
                sta (TMPADR),y
                iny

                lda #$A9        ; opcode for LDA immediate
                sta (TMPADR),y
                iny
                lda TMPADR1     ; LSB
                sta (TMPADR),y
                iny
                lda #$48        ; Opcode for PHA
                sta (TMPADR),y

z_ploop:        rts
.scend
; ----------------------------------------------------------------------------
; LOOP ( -- addr )  Compile-time part of LOOP. This does nothing more but push
; 01 on the stack and then call +LOOP. In Forth, this is 
; ": LOOP POSTPONE 1 POSTPONE (+LOOP) , POSTPONE UNLOOP ; IMMEDIATE COMPILE-ONLY"
l_loop:         bra a_loop
                .byte IM+CO+$04 
                .word l_ploop    ; link to +LOOP 
                .word z_loop
                .byte "LOOP"

.scope
a_loop:         ; have the finished word put "01" on the stack
                jsr f_cmpljsr
                .word l_one 

                jmp l_ploop     ; JSR/RTS to +LOOP

z_loop:         rts             ; never reached 
.scend
; ----------------------------------------------------------------------------
; UNLOOP ( -- ; R: n1 n2 n3 -- ) 
; Drop loop control stuff from Return Stack. Note that 6 * PLA uses just
; as many bytes as a loop would
l_unloop:       bra a_unloop
                .byte NC+CO+$06 
                .word l_loop    ; link to LOOP
                .word z_unloop
                .byte "UNLOOP"
.scope
a_unloop:       ; drop fudge number (limit/start) from DO/?DO off the 
                ; Return Stack 
                pla
                pla
                pla
                pla

                ; now drop the LEAVE address that was below them off the
                ; Return Stack as well
                pla
                pla

z_unloop:       rts
.scend
; ----------------------------------------------------------------------------
; J ( -- n ) (R: n -- n)
; Copy second loop counter from Return Stack to stack. Note we use a fudge
; factor for loop control; see (DO) for more details. At this point, we
; have the "I" counter/limit and the LEAVE address on the stack above this
; (three entries), whereas the ideal Forth implementation would just have
; two. See loops.txt for details. Make this native compiled for speed
; TODO test this 
l_j:            bra a_j
                .byte NC+CO+$01 
                .word l_unloop    ; link to UNLOOP
                .word z_j
                .byte "J"

.scope
a_j:            dex
                dex

                ; get the fudged index off from the stack. it's
                ; easier to do math on the stack directly than to pop and
                ; push stuff around
                stx TMPX
                tsx

                sec
                lda $0107,x     ; LSB
                sbc $0109,x
                sta TMPCNT

                lda $0108,x     ; MSB
                sbc $010A,x

                ldx TMPX

                sta 2,x         ; MSB of de-fudged index
                lda TMPCNT
                sta 1,x         ; LSB of de-fudged index

z_j:            rts
.scend
; ----------------------------------------------------------------------------
; I ( -- n ) (R: n -- n) 
; Copy loop counter (top of Return Stack) to stack. Note that this is not the 
; same as R@ because we use a fudge factor for loop control; see (DO) for more
; details. Note we make this native compile for speed. 
l_i:            bra a_i
                .byte NC+CO+$01 
                .word l_j    ; link to J
                .word z_i
                .byte "I"
.scope
a_i:            dex
                dex

                ; get the fudged index off of the top of the stack. it's
                ; easier to do math on the stack directly than to pop and
                ; push stuff around
                stx TMPX
                tsx

                sec
                lda $0101,x     ; LSB
                sbc $0103,x
                sta TMPCNT

                lda $0102,x     ; MSB
                sbc $0104,x

                ldx TMPX

                sta 2,x         ; MSB of de-fudged index
                lda TMPCNT
                sta 1,x         ; LSB of de-fudged index
                
z_i:            rts             ; should be never reached, because NC 
.scend
; ----------------------------------------------------------------------------
; PQDO ( -- ) ("(?DO)")
; Runtime routine for ?DO -- note this just contains the parts required
; for the question mark, the rest of DO is handled by (DO) (see PDO) 
; This must be native compile
l_pqdo:         bra a_pqdo
                .byte CO+NC+$05 
                .word l_i    ; link to I
                .word z_pqdo
                .byte "(?DO)"

.scope
a_pqdo:         ; see if TOS and NOS are equal
                jsr l_2dup      
                jsr l_equal     ; gives us ( n1 n2 f ) 

                lda 1,x         ; just need one byte of flag
                beq _do_do      ; if not equal, just continue with (DO)

                ; we're equal, so dump everything and jump beyond the loop
                ; first, dump six entries off of the Data Stack
                txa
                clc
                adc #$06        
                tax

                ; second, abort the whole loop. We don't have the 
                ; limit/start parameters on the Return Stack yet, just the 
                ; address that points to the end of the loop. Dump the 
                ; RTS of ?DO and then just RTS ourselves
                pla
                pla
                rts

_do_do:         inx             ; clear flag from EQUAL off stack
                inx             ; this merges into (DO) 
z_pqdo:         nop             ; dummy: never reached, not compiled
.scend
; ----------------------------------------------------------------------------
; PDO ( limit start -- ; R: -- limit start ) ("(DO)") 
; Runtime routine for DO loop. Note that ANSI loops quit when the boundry of
; limit-1 and limit is reached, a different mechanism than the FIG Forth loop 
; (you can see which version you have by running a loop with start and limit
; as the same value, for instance 0 0 DO -- these will walk through the 
; number space). We use a "fudge factor" for the limit that makes the Overflow
; Flag trip when it is reached; see http://forum.6502.org/viewtopic.php?f=9&t=2026 
; for further discussion of this. The source given there for this idea is 
; Laxen & Perry F83. 
; This routine must be native compile (and should be anyway for speed). 
l_pdo:          bra a_pdo
                .byte CO+NC+$04 
                .word l_pqdo    ; link to (?DO)
                .word z_pdo
                .byte "(DO)"
.scope
a_pdo:          ; first step: create fudge factor (FUFA) by subtracting the limit
                ; from $8000, the number that will trip the overflow flag
                sec
                lda #$00
                sbc 3,x         ; LSB of limit
                sta 3,x         ; save FUFA for later use
                lda #$80
                sbc 4,x         ; MSB of limit
                sta 4,x         ; save FUFA for later use
                pha             ; FUFA replaces limit on R stack
                lda 3,x         ; LSB of limit
                pha             

                ; second step: index is FUFA plus original index
                clc
                lda 1,x         ; LSB of original index
                adc 3,x         ; add LSB of FUFA
                sta 1,x
                lda 2,x         ; MSB of orginal index
                adc 4,x         ; add MSB of FUFA
                pha
                lda 1,x         ; LSB of index
                pha

                ; we've saved the FUFA on the NOS of the R stack, so we can
                ; use it later. Clean the Data Stack
                inx
                inx
                inx
                inx

z_pdo:          rts
.scend
; ----------------------------------------------------------------------------
; ?DO ( limit start -- ) (R: -- limit start ) 
; Compile-time part of ?DO. This may not be native compiled.
l_qdo:          bra a_qdo
                .byte IM+CO+$03 
                .word l_pdo    ; link to PDO
                .word z_qdo
                .byte "?DO"

.scope
a_qdo:          ; ?DO shares most of its code with DO. Use a flag to decide
                ; which is which
                lda #$FF
                sta FLAG2 
                bra do_common   ; keep this routine close to DO

z_qdo:          rts             ; never reached
.scend
; ----------------------------------------------------------------------------
; DO ( limit start -- ) (R: -- limit start ) 
; Compile-time part of DO. ": DO POSTPONE (DO) HERE ; IMMEDIATE COMPILE-ONLY ;"
; To work with LEAVE, we compile a routine that pushes the end address to 
; the Return Stack at run time. This is based on a suggestion by Garth Wilson,
; see loops.txt for details. This may not be native compile.
l_do:           bra a_do
                .byte IM+CO+$02 
                .word l_qdo    ; link to ?DO
                .word z_do
                .byte "DO"

.scope
a_do:           ; DO and ?DO share most of their code, use the FLAG2 to 
                ; determine which is which.
                stz FLAG2       ; this is the original

do_common:      ; we push HERE to the 65c02's stack so LOOP/+LOOP
                ; knows where to compile the PHA instructions. We put this
                ; on the Return Stack so we avoid getting mixed up in 
                ; IF/THEN and other stuff that uses the Data Stack

                ; but first we need to move the actual return address
                ; for this routine out of the way
                ply             ; LSB
                sty TMPADR
                ply             ; MSB

                ; now we can save HERE 
                lda CP+1        ; MSB first
                pha
                lda CP
                pha             ; then LSB

                ; put RTS address back before something bad happens
                phy             ; MSB
                ldy TMPADR
                phy             ; LSB

                ; now we compile six dummy bytes that LOOP/+LOOP will
                ; replace by the actual LDA/PHA instructions
                lda #$05        ; we don't really care about the value 
                tay
                        
*               sta (CP),y
                dey
                bpl -

                ; update CP
                inc             ; we used 5 as a dummy value, this is why 
                clc
                adc CP
                sta CP
                bcc +
                inc CP+1

                ; compile the (?DO) portion of ?DO if appropriate
*               lda FLAG2
                beq _do_cmpl

                ; we came from ?DO, so compile (?DO) first
                dex
                dex
                lda #<l_pqdo    ; add xt of (?DO) to the stack
                sta 1,x
                lda #>l_pqdo
                sta 2,x
                jsr l_cmpc      ; drops through to _do_cmpl

                ; compile (DO) -- don't call f_cmpljsr because this must be
                ; natively compiled
_do_cmpl:       dex
                dex
                lda #<l_pdo     ; add xt of (DO) to the stack
                sta 1,x
                lda #>l_pdo
                sta 2,x
                jsr l_cmpc

                ; HERE, hardcoded for speed. We put it on the Data Stack
                ; where LOOP/+LOOP takes it from. Note this has nothing to
                ; do with the HERE we're saving for LEAVE
                dex             
                dex
                lda CP          ; LSB
                sta 1,x
                lda CP+1        ; MSB
                sta 2,x

z_do:           rts
.scend
; ----------------------------------------------------------------------------
; THEN ( addr -- ) Provide target for a forward reference for IF. Note that 
; the Forth implementation  ": THEN HERE SWAP ! ; IMMEDIATE COMPILE-ONLY"
; is used for verious other things. Fused and natively compiled for speed
l_then:         bra a_then
                .byte NC+CO+IM+$04 
                .word l_do    ; link to DO
                .word z_then
                .byte "THEN"

.scope
a_then:         ; Put differently, this routine has us store the CP at
                ; the address that is provided TOS. Note that INC doesn't
                ; allow 1,x addressing so we have to do this the hard way
                lda 1,x
                sta TMPADR
                lda 2,x
                sta TMPADR+1

                lda CP
                sta (TMPADR)
                ldy #$01
                lda CP+1
                sta (TMPADR),y

                inx
                inx
 
z_then:         rts
.scend
; ----------------------------------------------------------------------------
; AGAIN ( addr -- ) 
; This is a compile-only word, immediate. It assumes that the address we need
; to jump to is on the stack. Use JMP instead of BRA to make sure that we 
; always have the range. Note this is very close to the code for COMPILE, so 
; we might want to figure out a way to combine them
l_again:        bra a_again
                .byte NC+IM+CO+$05 
                .word l_then    ; link to THEN
                .word z_again
                .byte "AGAIN"
.scope
a_again:        ; Add the JMP absolute command ($4C). We use Y as an index 
                ; and later move the CP up
                ldy #$00
                lda #$4C
                sta (CP),y 
                iny             
                lda 1,x         ; LSB of address
                sta (CP),y
                iny             
                lda 2,x         ; MSB of address
                sta (CP),y
                iny

                ; allot space we just used 
                tya
                clc
                adc CP
                sta CP
                bcc +
                inc CP+1

*               inx             ; drop the address
                inx

z_again:        rts
.scend
; -----------------------------------------------------------------------------
; BEGIN ( -- addr ) 
; This is a compile-only word, basically just a version of HERE that is 
; immediate. Marks the entry point for a loop by putting the address on 
; the stack for AGAIN or a similiar word
l_begin:        bra a_begin
                .byte IM+NC+CO+$05 
                .word l_again   ; link to AGAIN 
                .word z_begin
                .byte "BEGIN"

a_begin:        ; same code as HERE, for speed     
                dex             
                dex
                lda CP          ; LSB
                sta 1,x
                lda CP+1        ; MSB
                sta 2,x

z_begin:        rts
; ----------------------------------------------------------------------------
; P0BRANCH ( f -- ) ("(0BRANCH)")
; Compiled component of 0BRANCH. Not for direct use by the user, but we
; expose it so it can be modified if wished. Note the name of this routine 
; is funny to German speakers. This, of course, is totally unintentional. 
l_p0branch:     bra a_p0branch
                .byte CO+$09 
                .word l_begin   ; link to BEGIN
                .word z_p0branch
                .byte "(0BRANCH)"
.scope
a_p0branch:     ; we use the return value on the 65c02 stack to determine
                ; where we want to return to. 
                pla             ; LSB
                sta TMPADR
                pla             ; MSB
                sta TMPADR+1

                ; see if flag is zero, which is the whole purpose of the
                ; operation after all 
                inx
                inx 
                lda $FF,x       ; LSB of flag 
                ora 0,x         ; MSB
                beq _zero       ; flag is FALSE (zero), so branch 

                ; flag is TRUE so we just skip over the next two bytes that contain
                ; the branch address. In other words, this is the part between 
                ; IF and THEN. 
                clc
                lda TMPADR
                adc #$02
                sta TMPADR1
                bcc +
                inc TMPADR+1

*               lda TMPADR+1
                sta TMPADR1+1

                bra _done

_zero:          ; flag is FALSE (0) so we take the jump to the address 
                ; given in the next two bytes. Note that the address on 
                ; the 65c02 stack points to the last byte of the 
                ; JSR instruction, not the next byte afterwards
                ldy #$01        ; not zero
                lda (TMPADR),y  ; LSB
                sta TMPADR1
                iny
                lda (TMPADR),y  ; MSB
                sta TMPADR1+1

                ; We have to subtract one byte from the address given because 
                ; of the effect of RTS
                lda TMPADR1
                bne +
                dec TMPADR1+1
*               dec TMPADR1
               
_done:          ; now we can finally push the address to the stack
                lda TMPADR1+1   ; MSB first
                pha
                lda TMPADR1     ; LSB on top 
                pha 
        
z_p0branch:     rts
.scend
; -----------------------------------------------------------------------------
; 0BRANCH ( f -- ) Branch if zero 
; Compiles the code required for branching if zero in TOS, which is
; (0BRANCH). Note this expects the next two bytes to be the address of 
; where to jump to when the test fails. This code must not be natively 
; compiled because we need the return address provided on the 65c02 stack 
; by JSR
l_0branch:      bra a_0branch
                .byte IM+CO+$07
                .word l_p0branch    ; link to P0BRANCH
                .word z_0branch
                .byte "0BRANCH"
.scope
a_0branch:      ; encode subroutine jump to (0BRANCH)
                jsr f_cmpljsr
                .word a_p0branch

z_0branch:      rts
.scend
; ----------------------------------------------------------------------------
; PBRANCH ( -- ) "(BRANCH)"
; Jump to address provided in next to bytes. Compile only
l_pbranch:      bra a_pbranch
                .byte CO+$08 
                .word l_0BRANCH ; link to 0BRANCH
                .word z_pbranch
                .byte "(BRANCH)"
.scope
a_pbranch:      ; we use the return value on the 65c02 stack to determine
                ; where we want to return to. 
                pla             ; LSB
                sta TMPADR
                pla             ; MSB
                sta TMPADR+1

                ; Note that the address on the 65c02 stack points to the 
                ; last byte of the JSR instruction, not the next byte 
                ; afterwards
                ldy #$01        ; not zero
                lda (TMPADR),y  ; LSB
                sta TMPADR1
                iny
                lda (TMPADR),y  ; MSB
                sta TMPADR1+1

                ; We have to subtract one byte from the address 
                ; given because of the effect of RTS
                lda TMPADR1
                bne +
                dec TMPADR1+1
*               dec TMPADR1
               
_done:          ; now we can finally push the address to the stack
                lda TMPADR1+1   ; MSB first
                pha
                lda TMPADR1     ; LSB on top 
                pha 

z_pbranch:      rts
.scend
; -----------------------------------------------------------------------------
; BRANCH ( -- ) Always branch 
; Expects offset in next two bytes. This cannot be natively compiled because we
; need the return address provided on the 65c02 stack by JSR. 
l_branch:       bra a_branch
                .byte IM+CO+$06 
                .word l_pbranch    ; link to PBRANCH
                .word z_branch
                .byte "BRANCH"
.scope
a_branch:       ; encode subroutine jump to (BRANCH)
                jsr f_cmpljsr
                .word a_pbranch

z_branch:       rts 
.scend
; -----------------------------------------------------------------------------
; TRUE ( -- f ) 
; Push a well-formed 16 bit TRUE flag to stack 
l_true:         bra a_true
                .byte NC+$04 
                .word l_branch  ; link to BRANCH
                .word z_true
                .byte "TRUE"

a_true:         dex
                dex
                lda #$FF
                sta 1,x
                sta 2,x

z_true:         rts
; -----------------------------------------------------------------------------
; FALSE ( -- f ) 
; Push a well-formed 16 bit FALSE flag to stack 
l_false:        bra a_false
                .byte NC+$05 
                .word l_true     ; link to TRUE
                .word z_false
                .byte "FALSE"

a_false:        dex
                dex
                stz 1,x
                stz 2,x

z_false:        rts
; -----------------------------------------------------------------------------
; ONEPLUS ( n -- n ) ("1+")
; Add one to number on top of the parameter stack 
l_1plus:        bra a_1plus
                .byte NC+$02 
                .word l_false    ; link to FALSE
                .word z_1plus
                .byte "1+"
.scope
a_1plus:        ; TODO make sure we have enough entries on the stack  

                inc 1,x         ; LSB
                bne _done
                inc 2,x         ; MSB
_done:
z_1plus:        rts
.scend
; -----------------------------------------------------------------------------
; ONEMINUS ( n -- n ) ("1-")
; Subtract one from number on top of the parameter stack 
l_1minus:       bra a_1minus
                .byte NC+$02 
                .word l_1plus    ; link to 1+ (ONEPLUS)
                .word z_1minus
                .byte "1-"
.scope
a_1minus:       lda 1,x
                bne +
                dec 2,x
*               dec 1,x

z_1minus:       rts
.scend
; -----------------------------------------------------------------------------
; DEPTH ( -- u ) 
; Push the number of elements (cells, not bytes) in the parameter stack on 
; the parameter stack (excluding the count u itself). This assumes that the 
; stack pointer X is valid
l_depth:        bra a_depth
                .byte NC+$05 
                .word l_1minus  ; link to 1MINUS ("1-")
                .word z_depth
                .byte "DEPTH"

a_depth:        ; We've got zero entries when X is $7F
                lda #SP0        ; $7F unless something was changed
                stx TMPX
                sec 
                sbc TMPX

                ; now divide by two because each cell is two bytes 
                lsr 

                ; push result to stack 
                dex
                dex
                sta 1,x
                stz 2,x         ; always zero 

z_depth:        rts
; ----------------------------------------------------------------------------
; PICK ( n n u -- n n n ) 
; Take the u-th element out of the stack and put it on TOS, overwriting the 
; current TOS. 0 PICK is equivalent to DUP, 1 PICK to OVER. Note that 
; using PICK is considered poor coding form. Also note that FIG Forth has 
; a different behavior for PICK than ANS Forth. 
; TODO use DEPTH to check for underflow 
l_pick:         bra a_pick
                .byte NC+$04 
                .word l_DEPTH   ; link to DEPTH
                .word z_pick
                .byte "PICK"

.scope
a_pick:         lda 1,x         ; we only recognize the LSB (stack is small)
                asl 
                clc
                adc #$03
                tay
                stx TMPADR
                stz TMPADR+1
                lda (TMPADR),y  ; MSB
                sta 1,x
                iny 
                lda (TMPADR),y  ; LSB
                sta 2,x

z_pick:         rts
.scend

; ----------------------------------------------------------------------------
; MROT ( m n o -- o m n )
; Rotate the top three entries upwards
l_mrot:         bra a_mrot
                .byte NC+$04 
                .word l_pick    ; link to PICK
                .word z_mrot
                .byte "-ROT"

.scope
a_mrot:         lda 2,x         ; MSB first
                pha
                lda 4,x
                sta 2,x
                lda 6,x
                sta 4,x
                pla
                sta 6,x

                lda 1,x         ; LSB second
                pha
                lda 3,x
                sta 1,x
                lda 5,x
                sta 3,x
                pla
                sta 5,x

z_mrot:         rts
.scend
; -----------------------------------------------------------------------------
; ROT (m n o -- n o m )
; Rotate the top three entries downwards

l_rot:          bra a_rot
                .byte NC+$03 
                .word l_mrot    ; link to MROT
                .word z_rot
                .byte "ROT"

a_rot:          lda 6,x         ; MSB first
                pha
                lda 4,x
                sta 6,x
                lda 2,x
                sta 4,x
                pla 
                sta 2,x

                lda 5,x         ; LSB second 
                pha
                lda 3,x
                sta 5,x
                lda 1,x
                sta 3,x
                pla
                sta 1,x

z_rot:          rts
; -----------------------------------------------------------------------------
; TUCK ( n m -- m n m ) 
; Traditionally, this is realized as SWAP OVER, we do it better in assembler
l_tuck:         bra a_tuck
                .byte NC+$04 
                .word l_rot     ; link to ROT
                .word z_tuck
                .byte "TUCK"

a_tuck:         dex 
                dex

                ; move m to TOS 
                lda 3,x         ; LSB
                sta 1,x         
                lda 4,x         ; MSB
                sta 2,x

                ; move n to old m
                lda 5,x         ; LSB
                sta 3,x         
                lda 6,x         ; MSB
                sta 4,x

                ; write m over old n
                lda 1,x         ; LSB
                sta 5,x
                lda 2,x         ; MSB
                sta 6,x

z_tuck:         rts
; -----------------------------------------------------------------------------
; NIP ( n m -- m ) 
; Delete NOS 
l_nip:          bra a_nip
                .byte NC+$03 
                .word l_tuck     ; link to TUCK 
                .word z_nip
                .byte "NIP"

a_nip:          lda 1,x         ; LSB
                sta 3,x
                lda 2,x         ; MSB
                sta 4,x

                inx
                inx

z_nip:          rts
; ----------------------------------------------------------------------------
; 2SWAP ( n1 n2 n3 n4 -- n3 n4 n1 n2 ) 
; Exchange the top two cell pairs 
l_2swap:        bra a_2swap
                .byte NC+$05 
                .word l_nip     ; link to NIP 
                .word z_2swap
                .byte "2SWAP"
.scope
a_2swap:        ; TODO see if we have enough stuff on the stack 

                ; exchange n4 with n2
                lda 1,x         ; LSB of n4
                pha 
                lda 2,x         ; MSB of n4
                pha 

                lda 5,x         ; LSB of n2
                sta 1,x
                lda 6,x         ; MSB of n2
                sta 2,x

                pla 
                sta 6,x
                pla
                sta 5,x

                ; exchange n3 with n1
                lda 3,x         ; LSB of n3
                pha
                lda 4,x         ; MSB of n3
                pha

                lda 7,x         ; LSB of n1
                sta 3,x
                lda 8,x         ; MSB of n1
                sta 4,x

                pla
                sta 8,x
                pla
                sta 7,x

z_2swap:        rts
.scend
; -----------------------------------------------------------------------------
; SWAP ( m n  -- n m ) 
l_swap:         bra a_swap
                .byte NC+$04 
                .word l_2swap    ; link to 2SWAP
                .word z_swap
                .byte "SWAP"
.scope
a_swap:         ; save next entry on stack (NOS)
                lda 3,x        ; LSB  
                pha
                lda 4,x        ; MSB
                pha

                ; move top of stack (TOS) to NOS 
                lda 1,x        ; LSB
                sta 3,x
                lda 2,x        ; MSB
                sta 4,x

                ; restore NOS to TOS 
                pla             
                sta 2,x        ; MSB
                pla
                sta 1,x        ; LSB

z_swap:         rts
.scend
; -----------------------------------------------------------------------------
; 2DUP ( n m -- n m n m ) 
; Duplicate top two entry on the stack 
l_2dup:         bra a_2dup
                .byte NC+$04 
                .word l_swap     ; link to SWAP 
                .word z_2dup
                .byte "2DUP"

a_2dup:         dex
                dex

                lda 6,x         ; MSB next on stack (NOS) 
                sta 2,x
                lda 5,x         ; LSB NOS
                sta 1,x
                
                dex
                dex

                lda 6,x         ; MSB top of stack (TOS) 
                sta 2,x
                lda 5,x         ; LSB TOS
                sta 1,x

z_2dup:         rts
; ----------------------------------------------------------------------------
; ?DUP ( n -- 0 | n n ) 
l_qdup:         bra a_qdup
                .byte NC+$04 
                .word l_2dup     ; link to 2DUP
                .word z_qdup
                .byte "?DUP"
.scope
a_qdup:         lda 1,x
                ora 2,x
                beq _done

                dex
                dex
                lda 3,x
                sta 1,x
                lda 4,x
                sta 2,x
_done:
z_qdup:         rts
.scend
; -----------------------------------------------------------------------------
; DUP ( n -- n n ) 
; Duplicate top entry on the stack 
l_dup:          bra a_dup
                .byte NC+$03 
                .word l_qdup    ; link to ?DUP
                .word z_dup
                .byte "DUP"

a_dup:          dex
                dex

                lda 3,x         ; LSB
                sta 1,x
                lda 4,x         ; MSB
                sta 2,x

z_dup:          rts
; ----------------------------------------------------------------------------
; 2OVER  ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 ) 
; Copy cell pair n1 n2 to top of the Data Stack 
l_2over:        bra a_2over
                .byte NC+$05 
                .word l_dup      ; link to DUP 
                .word z_2over
                .byte "2OVER"
.scope
a_2over:        ; TODO see if we have enough stuff on the stack 
                dex
                dex
                dex
                dex

                lda 11,x        ; LSB of n1
                sta 3,x
                lda 12,x        ; MSB of n1
                sta 4,x

                lda 9,x         ; LSB of n2
                sta 1,x
                lda 10,x        ; LSB of n2
                sta 2,x

z_2over:        rts
.scend
; -----------------------------------------------------------------------------
; OVER ( n m -- n m n ) 
l_over:         bra a_over
                .byte NC+$04 
                .word l_2over    ; link to 2OVER
                .word z_over
                .byte "OVER"

a_over:         dex
                dex

                lda 5,x         ; LSB
                sta 1,x
                lda 6,x         ; MSB
                sta 2,x

z_over:         rts
; -----------------------------------------------------------------------------
; R FETCH  ( -- n ) (R: n -- n) ("R@")
; Copy (not: move) x from Return Stack to Parameter Stack
; Remember we have to move the return address from the subroutine call 
; out of the way. We follow Gforth in that this is not a compile-only word,
; though the ANS Forth standard leaves the interpretation "undefined". This
; should not be natively compiled for that reason. 
l_rfetch:       bra a_rfetch
                .byte CO+$02 
                .word l_over     ; link to OVER
                .word z_rfetch
                .byte "R@"

a_rfetch:       dex
                dex

                ; save the return address
                pla             ; LSB
                sta TMPADR 
                pla             ; MSB
                sta TMPADR+1

                ; copy the value to the parameter stack 
                pla             ; LSB
                sta 1,x
                pla             ; MSB
                sta 2,x

                ; copy values back to the return stack 
                lda 2,x         ; MSB
                pha
                lda 1,x         ; LSB
                pha

                ; restore return address
                lda TMPADR+1    ; MSB
                pha
                lda TMPADR      ; LSB
                pha 

z_rfetch:       rts
; -----------------------------------------------------------------------------
; FROM R ( -- n ) (R: n -- ) ("R>") 
; Move x from Return Stack to Parameter Stack. Remember we have to move 
; the jump address out of the way first. This is a compile-only word
l_fromr:        bra a_fromr
                .byte CO+$02 
                .word l_rfetch   ; link to R FETCH ("R@")
                .word z_fromr
                .byte "R>"

a_fromr:        dex
                dex

                ; save the return address
                pla             ; LSB
                sta TMPADR
                pla             ; MSB
                sta TMPADR+1

                ; now we can access the data
                pla             ; LSB
                sta 1,x
                pla             ; MSB
                sta 2,x         

                ; restore return address
                lda TMPADR+1   ; MSB
                pha
                lda TMPADR     ; LSB
                pha 
                
z_fromr:        rts
; -----------------------------------------------------------------------------
; TO R ( n -- ) (R: -- n ) (">R")
; Move x to the Return Stack. Note that the Parameter (Data) Stack and the 
; Return Stack both have the LSB on top. Remember that we have to move the 
; original value off the stack to make the return jump work
; This is a compile-only word
l_tor:          bra a_tor
                .byte CO+$02 
                .word l_fromr    ; link to FROM R ("R>")
                .word z_tor
                .byte ">R"
.scope
a_tor:          ; save the return address
                pla             ; LSB
                sta TMPADR
                pla             ; MSB
                sta TMPADR+1

                ; now we can move the data 
                lda 2,x         ; MSB
                pha
                lda 1,x         ; LSB
                pha

                ; restore return address
                lda TMPADR+1   ; MSB
                pha
                lda TMPADR     ; LSB
                pha 

                inx
                inx

z_tor:          rts
.scend
; ----------------------------------------------------------------------------
; 2>R ( x1 x2 -- ) ( R: -- x1 x2  )
; Push two top entries to Return Stack. Is the same as SWAP >R >R
l_2gr:          bra a_2gr
                .byte CO+$03 
                .word l_tor     ; link to TOR (">R")
                .word z_2gr
                .byte "2>R"
.scope
a_2gr:          ; save the return address
                pla             ; LSB
                sta TMPADR
                pla             ; MSB
                sta TMPADR+1

                ; now we can move the data 
                lda 4,x         ; MSB
                pha
                lda 3,x         ; LSB
                pha

                ; now we can move the data 
                lda 2,x         ; MSB
                pha
                lda 1,x         ; LSB
                pha

                ; restore return address
                lda TMPADR+1   ; MSB
                pha
                lda TMPADR     ; LSB
                pha 

                inx
                inx
                inx
                inx

z_2gr:          rts
.scend
; ----------------------------------------------------------------------------
; 2R@ ( -- x1 x2 ) (R: x1 x2 -- x1 x2 ) 
; This is  R> R> 2DUP >R >R SWAP  but we can do this a lot faster in assembler 
; THis routine may not be natively compiled
l_tworfetch:    bra a_tworfetch
                .byte CO+$03 
                .word l_2gr    ; link to 2>R
                .word z_tworfetch
                .byte "2R@"

.scope
a_tworfetch:    ; make room on the stack 
                dex
                dex
                dex
                dex

                ; get four bytes off of Return Stack. This assumes that 
                ; we took a subroutine jump here so the first two entries
                ; are the return address
                txa
                tsx
                phx             ; 65c02 has no TXY, so do it the hard way
                ply 
                tax

                lda $0103,y     ; LSB of top entry
                sta 1,x
                lda $0104,y     ; MSB of top entry
                sta 2,x
                lda $0105,y     ; LSB of bottom entry
                sta 3,x
                lda $0106,y     ; MSB of top entry
                sta 4,x

z_tworfetch:    rts
.scend
; ----------------------------------------------------------------------------
; 2R> ( -- x1 x2 ) (R: x1 x2 -- ) 
; Pull two top entries from Return Stack. Is the same as R> R> SWAP
l_2rg:          bra a_2rg
                .byte CO+$03 
                .word l_tworfetch    ; link to 2R@
                .word z_2rg
                .byte "2R>"
.scope
a_2rg:          ; make room on stack 
                dex
                dex
                dex
                dex

                ; save the return address
                pla             ; LSB
                sta TMPADR
                pla             ; MSB
                sta TMPADR+1

                ; now we can access the data
                pla             ; LSB
                sta 1,x
                pla             ; MSB
                sta 2,x         

                ; now we can access the data
                pla             ; LSB
                sta 3,x
                pla             ; MSB
                sta 4,x         
 
                ; restore return address
                lda TMPADR+1   ; MSB
                pha
                lda TMPADR     ; LSB
                pha 
 
z_2rg:          rts             ; never reached
.scend
; -----------------------------------------------------------------------------
; QUESTION( c-addr -- ) ("?")
; Print contant of a variable 
l_quest:        bra a_quest
                .byte NC+$01 
                .word l_2rg    ; link to 2R>
                .word z_quest
                .byte "?"

.scope
a_quest:        jsr l_fetch
                jsr l_dot
z_quest:        rts
.scend
; -----------------------------------------------------------------------------
; FETCH  ( c-addr -- n ) ("@")
; Get one cell (16 bit) value from address. 
; TODO rewrite with Y as temporary storage?
l_fetch:        bra a_fetch
                .byte NC+$01 
                .word l_quest   ; link to QUESTION ("?")
                .word z_fetch
                .byte "@"

a_fetch:        lda 1,x        ; LSB
                sta TMPADR
                lda 2,x        ; MSB
                sta TMPADR+1
                
                lda (TMPADR)    ; LSB of address in memory
                pha 
                inc TMPADR      
                bne +
                inc TMPADR+1 

*               lda (TMPADR)    ; MSB of address in memory
                sta 2,x
                pla 
                sta 1,x

z_fetch:        rts
; -----------------------------------------------------------------------------
; STORE ( x addr -- ) ("!")
; Store value x at address
l_store:        bra a_store
                .byte NC+$01 
                .word l_fetch   ; link to FETCH ("@")
                .word z_store
                .byte "!"
.scope
a_store:        jsr l_swap

                lda 1,x        ; LSB
                sta (3,x)
                
                inc 3,x
                bne +
                inc 4,x

*               lda 2,x        ; MSB
                sta (3,x)

                inx             ; 2DROP
                inx
                inx
                inx
               
z_store:        rts
.scend
; -----------------------------------------------------------------------------
; 2DROP ( x x -- ) 
l_2drop:        bra a_2drop
                .byte NC+$05    ; compiles natively 
                .word l_store   ; link to STORE ("!")
                .word z_2drop
                .byte "2DROP"

a_2drop:        inx
                inx
                inx
                inx 

z_2drop:        rts
; -----------------------------------------------------------------------------
; DROP ( x -- ) 
; Pop one cell from the stack 
l_drop:         bra a_drop
                .byte NC+$04    ; compiles natively 
                .word l_2drop   ; link to 2DROP
                .word z_drop
                .byte "DROP"

a_drop:         inx
                inx

z_drop:         rts             
; -----------------------------------------------------------------------------
; WORDS ( -- ) 
; Print list of the words in the dictionary. This could also be realized with
; a combination of >NAME and TYPE, though this is probably marginally faster
; TODO rewrite so we don't break the line in the middle of the word
; ** THIS IS ALWAYS THE FIRST ENTRY IN THE DICTIONARY ** 
l_words:        bra a_words
                .byte $05 
                .word l_drop    ; link to DROP 
                .word z_words
                .byte "WORDS"
.scope
a_words:        ; start with last entry in dictionary
                lda DP
                sta TMPADR
                lda DP+1
                sta TMPADR+1

_loop:          lda #AscSP     ; SPACE 
                jsr f_putchr

                ldy #$02        ; location of Length Byte
                lda (TMPADR),y
                and #%00011111  ; mask everything but length 
                sta TMPCNT

                ldy #$07        ; start of Name String 
*               lda (TMPADR),y  
                jsr f_putchr

                dec TMPCNT
                beq _nextword
                
                iny 
                bra - 
 
                lda #AscSP      ; SPACE 
                jsr f_putchr
                
                ; get address of next word in list
_nextword:      ldy #$03        ; link starts at offset 3
                lda (TMPADR),y  ; LSB of link 
                pha
                iny
                lda (TMPADR),y  ; MSB
                sta TMPADR+1
                pla 
                sta TMPADR      ; keep LSB in A for check

                ; quit if we're at the end of the list 
                ora TMPADR+1
                bne _loop 

z_words:        rts
.scend
; =============================================================================
; HIGH-LEVEL COMMANDS
; =============================================================================
; These are more easily coded it Forth itself than assembler and are 
; compiled during boot. We save them as counted strings and feed them to 
; EVALUATE during boot. To make additions easier, we don't count the 
; number of entries, but use $0000 to mark the end of the table 

; TODO see if we need all of this or just can't put these all at the end
; of the file and have EVALUATE run through them
; TODO see if there isn't a way to get rid of these by making real 
; routines out of them
fhltbl:
        .word fh_if, fh_else, fh_until, fh_while, fh_rpt
        .word fh_is, fh_actionof 
        .word $0000

; All high-level command strings are uppercase and start with fh_ . Note 
; string size is in decimal. Max length of strings is $FE. The ";" can be
; included as part of the string without problems - don't let the color 
; coding in vim spook you. Uppercase is not required but recommended 
; for readability. COMPILE-ONLY is a non-standard word that sets the CO flag
; to force use in definitions only. The alternative is a ?COMPILE test. 

; See https://groups.google.com/forum/#!msg/comp.lang.forth/UMYrU1aRIW4/eV3qDk06Vs0J
; and http://blogs.msdn.com/b/ashleyf/archive/2011/02/04/if-else-then.aspx
; for discussions on IF ELSE THEN
; See http://www.forth200x.org/documents/html/implement.html for reference
; implementations of ANS Forth

; len -->  0    0   1    1    2    2    3    3    4    4    5    5    6    6    7
;          0    5   0    5    0    5    0    5    0    5    0    5    0    5    0
fh_if:    
.byte 55, ": IF POSTPONE 0BRANCH HERE 0 , ; IMMEDIATE COMPILE-ONLY"
fh_else:  
.byte 67, ": ELSE POSTPONE BRANCH HERE 0 , HERE ROT ! ; IMMEDIATE COMPILE-ONLY"
fh_until: 
.byte 51, ": UNTIL POSTPONE 0BRANCH , ; IMMEDIATE COMPILE-ONLY"
fh_while: 
.byte 63, ": WHILE POSTPONE 0BRANCH HERE 0 , SWAP ; IMMEDIATE COMPILE-ONLY"
fh_rpt:   
.byte 60, ": REPEAT POSTPONE AGAIN HERE SWAP ! ; IMMEDIATE COMPILE-ONLY"
fh_is:
.byte 75, ": IS STATE @ IF POSTPONE ['] POSTPONE DEFER! ELSE ' DEFER! THEN ; IMMEDIATE"
fh_actionof: 
.byte 82, ": ACTION-OF STATE @ IF POSTPONE ['] POSTPONE DEFER@ ELSE ' DEFER@ THEN ; IMMEDIATE"
; len -->  0    0   1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
;          0    5   0    5    0    5    0    5    0    5    0    5    0    5    0    5    0

; =============================================================================
; STRINGS
; =============================================================================
; Strings beginn with "fs" and are terminated by zero, because this is
; easier for the 65c02 to work with. Access is through an index byte passed
; to f_prtzerostr which gives us the string name we want
strtbl: .word fs_title, fs_version, fs_disclaim, fs_typebye     ; 00-03
        .word fs_prompt, fs_compile, fse_error, fse_stack       ; 04-07
        .word fse_channel, fse_divzero, fse_noname, fse_syntax  ; 08-0B
        .word fse_componly, fse_intonly, fse_empty, fs_xt       ; 0C-0F
        .word fs_f_IM, fs_f_CO, fs_f_NC, fs_dic_link            ; 10-13
        .word fs_datadump, fse_radix, fse_defer                 ; 14-17

; ----------------------------------------------------------------------------- 
; General Forth Strings (all start with fs_)
fs_title:      .byte "Tali Forth for the 65c02",0
fs_version:    .byte "Version BETA (10. Feb 2015)",0
fs_disclaim:   .byte "Tali Forth comes with absolutely NO WARRANTY",0
fs_typebye:    .byte "Type 'bye' to exit",0 
fs_prompt:     .byte " ok",0
fs_compile:    .byte " compiled",0

; --- The following strings are all for SEE
fs_xt:         .byte "xt: ",0
fs_f_IM:       .byte "immediate ",0
fs_f_CO:       .byte "compile-only ",0
fs_f_NC:       .byte "native-compile ",0
fs_dic_link:   .byte "links to:",0
fs_datadump:   .byte "Data Field dump:",0

; ----------------------------------------------------------------------------- 
; Warning and error strings (all start with fse_)
fse_error:     .byte ">>>Error<<<",0
fse_stack:     .byte "Stack over- or underflow",0
fse_channel:   .byte "Accessing unknown channel",0
fse_divzero:   .byte "Division by zero",0
fse_noname:    .byte "Parsing failure",0
fse_syntax:    .byte "Unknown word",0
fse_componly:  .byte "Interpreting a compile-only word",0
fse_intonly:   .byte "Not in interpret mode",0
fse_empty:     .byte " (empty)",0
fse_radix:     .byte "Digit larger than base",0
fse_defer:     .byte "DEFERed word not defined yet",0

; ----------------------------------------------------------------------------- 
; Helper strings. Leave alphastr as the last entry in the source code to 
; make it easier to see where it ends in ROM
alphastr:       .byte "0123456789ABCDEFGHIJKLMNOPQRSTUVWYZ"
; =============================================================================
; END
; =============================================================================
