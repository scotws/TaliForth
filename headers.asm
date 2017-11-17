; Dictionary Headers for Tali Forth for the 65c02
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 05. Dec 2016 (original for Liara Forth)
; This version: 14. Sep 2017

; Dictionary headers are kept separately from the code, which allows various
; programming tricks. We roughly follow the Gforth terminology: The Execution
; Token (xt) is the address of the first byte of a word's code that can be,
; uh, executed; the Name Token (nt) is a pointer to the beginning of the
; word's header in the Dictionary. In the code
; itself, we use "nt_<WORD>" for the nt and "xt_<WORD>" for the xt. 

; Headers are linked in 16 different lists depending on size, see the
; documentation for details. This is also the reason why there is 
; no information on the length of the word's string is coded here. 

; This gives us the following header structure:
;
;              8 bit     8 bit
;               LSB       MSB
; nt_word ->  +--------+--------+
;             | Next Header     | -> nt_next_word
;          +2 +-----------------+
;             | Start of Code   | -> xt_word 
;          +4 +-----------------+
;             | End of Code     | -> z_word
;          +6 +--------+--------+
;             | Status | String |
;             +--------+--------+
;             |        |        |
;             +--------+--------+
;             |        |  ...   | (name string does not end with a zero)
;          +n +--------+--------+

; This puts the Status Byte six bytes down from the nt of the word. 

;       CO - Compile Only
;       IM - Immediate Word
;       NN - Never Native Compile 
;       AN - Always Native Compile (may not be called by JSR)

; Note there are currently four bits unused. By default, all words can be
; natively compiled (compiled inline) or as a subroutine jump target; the system
; decides which variant to use based on a threshold the user can set. The NN
; flag forbids native compiling, the AN flag forces it.  

; In the individual lists, the most common word - the one we want to get to
; the fastest - comes last, that is, further down in the list. This means 
; that the first entry - the one first in the code - has a link of 0000,
; signaling that the list is done. 

; -------------------------------------------------------------------
; WORDS WITH LENGTH 01

nt_j:   
        .word 0000, xt_j, z_j
	.byte CO, 'j'

nt_i:
        .word nt_j, xt_i, z_i
        .byte CO, 'i'


wordlist_01:
	; TODO 

; -------------------------------------------------------------------
; WORDS WITH LENGTH 02
wordlist_02:


; -------------------------------------------------------------------
; WORDS WITH LENGTH 03
wordlist_03:

nt_bye: 
        .word 0000   ; next word in dictionary, 0000 signals end
        .word xt_bye ; start of code, the xt of this word
        .word z_bye  ; end of code (points to RTS)
	.byte 00     ; status byte
        .byte "bye"  ; word name, always lower case




; -------------------------------------------------------------------
; WORDS WITH LENGTH 04

nt_cold: 
        .word nt_bye, xt_cold, z_cold
	.byte 00, "cold"

nt_word:
         .word nt_cold, xt_word, z_word
	 .byte 00, "word"

nt_find: 
         .word nt_word, xt_find, z_find
	 .byte 00, "find"

wordlist_04:


; -------------------------------------------------------------------
; WORDS WITH LENGTH 05
wordlist_05:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 06
wordlist_06:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 07
wordlist_07:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 08
wordlist_08:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 09
wordlist_09:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 10
wordlist_10:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 11
wordlist_11:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 12
wordlist_12:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 13
wordlist_13:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 14
wordlist_14:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 15
wordlist_15:

; -------------------------------------------------------------------
; WORDS WITH LENGTH 16
wordlist_16:


