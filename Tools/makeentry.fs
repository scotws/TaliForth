\ TOOL: Create a dictionary entry header for Tali Forth
\ Scot W. Stevenson  <scot.stevenson@gmail.com>
\ gforth 0.7.2, License GPL (c) Use at your own risk
\ First version: 07. Feb 2014
\ This version:  09. Nov 2014

\ Takes the link name of a word and prints an empty header to the screen
\ so it can be accessed by copy and paste. From gforth, call
\     include makeentry.fs
\ followed by
\     makeentry <NAME>

\ IMPORTANT: The link address provided is to be copied to the previous 
\ entry in the dictionary, not this one. Remember to copy it manually.
   
16 constant INDENT

: soff ( u -- )  \ push in by 'indent' spaces 
   INDENT spaces ; 

: loffset ( u -- u )  \ take length of word and calculate the offset for the links
   INDENT swap -  
   3 - 
   spaces ;       \ for l_ and : stuff

: dashes ( -- )   \ print a line of dashes 
   76 0 do [char] - emit loop ; 

: str2upper ( addr u -- ) 
   0 do  dup i +  c@  toupper  emit loop drop ; 

: makeentry   ( "word"  -- )  \ keep (addr n) from PARSE-NAME on top of stack at all times
   decimal 
   cr 
   parse-name     \ puts addr n on the stack 
   dup >r         \ save length to R 
   
   ." ; " dashes cr
   ." ; " 2dup str2upper ."  ( -- )" cr 
   ." l_" 2dup type ." :"  dup loffset ." bra a_"  2dup type  cr 
   soff ." .byte $0"  hex  r@ u.  decimal  cr
   soff ." .word l_"  2dup type  4 spaces  ." ; link to "  
         2dup str2upper cr
   soff ." .word z_" 2dup type cr
   soff ." .byte "  $22 emit  2dup str2upper $22 emit cr cr 

   ." .scope" cr
   ." a_" 2dup type ." :"  dup loffset ." nop" cr cr
   ." z_" 2dup type ." :"  dup loffset ." rts" cr
   ." .scend" cr cr 

   r> drop 2drop ; 
