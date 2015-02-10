; -----------------------------------------------------------------------------
; MAIN FILE 
; Tali Forth for the 65c02 
; Scot W. Stevenson <scot.stevenson@gmail.com>
;
; First version 19. Jan 2014
; This version  24. Feb 2014
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; Used with the Ophis assembler and the py65mon simulator
; -----------------------------------------------------------------------------
; We assume for the moment that Forth and the Kernel will use 16 kb and start
; at $c000 of ROM. If we don't use this much space, change this to 8 kb 
; ($e000). Note that we can run into trouble with the py65 emulator in this 
; case because it hard-codes the input/output addresses to $f001
.org $c000

; =============================================================================
; FORTH CODE 
FORTH: 
.require "Tali-Forth.asm"

; =============================================================================
; KERNEL 
.require "Tali-Kernel.asm"

; =============================================================================
; LOAD ASSEMBLER MACROS
.require "macros.asm"

; =============================================================================
; INTERRUPT VECTORS
.advance $FFFA  ; fill with zeros so we get a complete ROM image. 

.word k_nmiv    ; NMI vector 
.word k_resetv  ; RESET vector
.word k_irqv    ; IRQ vector 

; =============================================================================
; END
