; *****************************************************************************
; *                                                                           * 
; * Copyright (C) 2016 Nicola Cimmino                                         *
; *                                                                           * 
; *   This program is free software: you can redistribute it and/or modify    *
; *   it under the terms of the GNU General Public License as published by    *
; *   the Free Software Foundation, either version 3 of the License, or       *
; *   (at your option) any later version.                                     *
; *                                                                           *
; *  This program is distributed in the hope that it will be useful,          *
; *   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
; *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
; *   GNU General Public License for more details.                            *
; *                                                                           *
; *   You should have received a copy of the GNU General Public License       *
; *   along with this program.  If not, see http://www.gnu.org/licenses/.     *
; *                                                                           *
; *                                                                           *
; *****************************************************************************

; * Below are tbe BASIC tokens for 10 SYS (49152)
; * We store them at the beginning of the BASIC RAM so when we can load
; * the program with autorun (LOAD "*",8,1) and save to type the SYS.
*=$801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $34, $39, $31, $35, $32
        BYTE    $29, $00, $00, $00

; *
; * Constants
; *
RASSTART = 59           ; Raster line where the raster interrupt starts
ZEROVAL  = 20           ; Zero page location set always to zero see §1 comments
TIMERA   = 21           ; Time base, roughly 1 per frame (50Hz)
TSCROLL  = 24           ; Current text pixel scroll 0-8
TSCSTART = 1624         ; First char of line 15 (1024+(15*40))
TSCOLST  = 55896        ; Color RAM for first char of line 15 (55296+(15*40))
; * This is the actual beginning of our assembly program.
*=$C000

        SEI             ; Prevent interrupts while we set things up
        JSR $FF81       ; Reset VIC, clear screen

        LDA #%00110101  ; Disable kernal and BASIC ROMs
        STA $01         ; we go bare metal.

        LDA #%01111111  ; Disable CIA-1/2 interrupts
        STA $DC0D
        STA $DD0D
        LDA #%00000001  ; Enable raster interrupt
        STA $D01A
        LSR $DC0D       ; Acknowledge CIA 
        LSR $D019       ; and video interrupts
        
        LDA #RASSTART   ; Set raster interrupt line
        STA $D012
        LDA #%01111111  ; Clear RST8 bit, the interrupt line is
        AND $D011       ; above raster line 255 
        STA $D011        

        LDA #<ISR       ; Setup the interrupt vector to our function  
        STA $FFFE       ; note that this vector is for ALL interrupts,
        LDA #>ISR       ; not only the VIC raster interrupt.
        STA $FFFF       ; Anyhow all others are disabled.

        LDA #0          ; Ensure we have a zero in a page zero location
        STA ZEROVAL     ; see other §1 comments for usage
        STA TIMERA      ; Initialize time base accumulators

        LDA #$80        ; Sprite 0 pointer to $2000
        STA $07F8
        LDA #%0000001   ; Enable sprite 0
        STA $D015
        STA $D017       ; Double height for sprite 0
        STA $D01D       ; Double width fo sprite 0

        LDA #160        ; Position the sprite in the middle of the bar
        STA $D000       ; break and just two lines after the start (so 
        LDA #RASSTART+2 ; the jitter offset lines are not affected by 
        STA $D001       ; shorter raster lines). Also the bar cannot
                        ; be taller than the sprite as the line timing
                        ; will changes as soon as out of the sprite area

        LDY #0          ; Prepare color RAM for the scroller text with
SETTCOL TYA             ; colors from the TSCOLST table
        AND #%00000111  ; Colors don't chamge during the scroll but since
        TAX             ; the text moves it will give the impression of 
        LDA SCRCOL,X    ; colors changes.
        STA TSCOLST,Y
        INY
        TYA 
        CMP #40         ; Stop after 40 chars as we reached the border.
        BNE SETTCOL

        CLI             ; Let interrupts come 

        ; This is our main loop. For now we don't do anything
        ; useful, just a mix of different length instructions
        ; so we maximise the raster interrupt jitter to simulate
        ; real code.

LOOP    LDA #1          ; 2 cycles
        LDA $01         ; 3 cycles
        LDA $0300       ; 4 cycles
        LDA ($04),Y     ; 5 cycles      
        LDA ($04,X)     ; 6 cycles 
        LSR $0300,X     ; 7 cycles
        JMP LOOP

        ; This is the first raster interrupt service routine.
        ; By the time we come here we can be anywhere on the
        ; desired line with a jitter of 7 cycles depending on
        ; the instruction executing when the interrupt appened.

ISR     PHA             ; Preserve A,X,Y on the stack
        TXA
        PHA
        TYA
        PHA

        TSX             ; We are about to let another interrupt happen
                        ; without calling RTI, so there will be one extra
                        ; return address on the stack. Save the good stack
                        ; pointer into X, we will restore it later before RTI
        
        INC $D012       ; Setup another raster interrupt for the next
        LDA #<ISR2      ; scan line   
        STA $FFFE       
        LDA #>ISR2      
        STA $FFFF
        
        LSR $D019       ; Acknoweledge video interrupts
        
        CLI

        NOP             ; Waste time waiting the interrupt to happen
        NOP             ; these are NOPs as they take 2 cycles the jitter
        NOP             ; of the next interrupt will be only 1 cycle.
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP             ; These are enough to complete a scan line.
        ; We will never come here as the interrupt will happen
        ; before and by resetting the stack pointer RTI will not
        ; return here either.
        
        ; This is the second raster interrupt routine. By the time
        ; we come here we have a jitter of just one cycle as we ensured
        ; this interrupt happens while executing NOPs (2 cycles).
        ; Next we ensure we spend the exact amount of cycles it takes 
        ; to draw a full scan line. The last BEQ *+2 we use does the 
        ; sync magic. See comments below. This is timed for PAL systems
        ; the delay loop needs to be changed for NTSC.

                        ; Interrupt servicing (during a NOP)          2/3 cycles                                               
ISR2    TXS             ; Restore the SP messed by the interrupt.       2 cycles   
        
        LDY #8          ; This loop, the interrput call the above TXS and 
        DEY             ; the below BIT and LDA take exactly one scan line 
        BNE *-1         ; minus 1 cycle.
                        ;                                              46 cycles 
        BIT $00         ;                                               3 cycles
        NOP
        NOP

        LDA $D012       ; Get current scan line                         4 cycles
        CMP $D012       ; Here we are either still on the same line     4 cycles
                        ; (with one cycle to go) or at the next line.
        BEQ *+2         ; If we are on same line branch (3 cycles)    3/2 cycles
                        ; else move on (2 cycles). Note that in both
                        ; cases we end up at the next instruction, but it
                        ; will take different time to get there so we 
                        ; offset the remaining 1 cycle jitter.
                        
        ; From here on we are stable.

        LDY #10         ; Push forward so the STA $D020/1 
        DEY             ; are in the horizontal sync area 
        BNE *-1         ; 
        BIT $00

        LDY #$FF        ; Y will be used to index table BARCOL, we start from FF
                        ; so when we INY below we roll to 00

BLOOP   INY             ; Next colour entry

        LDA $D012       ; We need to avoid bad lines, we set YSCROLL to
        CLC             ; (current raster line + 7)%8 so that the bad line
        ADC #7          ; is always the previous line.
        AND #%00000111  
        ORA #%00011000  
        STA $D011

        LDA ZEROVAL     ; We need zero in A in 3 cylcles so we load from page
                        ; zero from a location we set to zero. See §1 comments.
        STA $D021       ; Black vertical bar starts here...
        NOP             ; 
        LDA BARCOL,Y    ;                   
        STA $D021       ; ...and ends here
        BIT $00         ; Waste more cycles to fill up the raster line
        NOP

        LDA BARCOL,Y    ; Get the current bar colour                          
        STA $D020       ; and set it for border                        
        STA $D021       ; and background colour                         
                                
        AND #%10000000  ; Test bit-7 of the colour is set
        BNE BLEND       ; if set we are done with the bar
 
        JMP BLOOP       ; And repeat for next colour.
           
        ; We are done with the interrupt, we need to set up
        ; the next one and restore registers before leaving.

BLEND   LDA #RASSTART   ; Set raster interrupt to the start of the bar
        STA $D012

        CLC             ; Ensure the next interrupt will not happen on a
        ADC #7          ; bad line, set YSCROLL so the badline is one before
        AND #%00000111  ; the next interrupt (RASTART is in A at the start
        ORA #%00011000  ; of this block)
        STA $D011

        LDA #<ISR       ; Set the interrupt vector back to the first ISR        
        STA $FFFE       ; (the raster sync one)
        LDA #>ISR       
        STA $FFFF

        LSR $D019       ; Acknoweledge video interrupts
        
        ; This is a good time to take care of the time base.
        ; We increment TIMERA once per frame. This serves as time base for 
        ; animations, scrolls and music.

        INC TIMERA      ; Increment time base
        
        ; Here we move the sprite in the bar. This is a good place cause the
        ; sprite has just been drawn so we don't risk to tear it horizontally
        
        LDA TIMERA
        ROR
        ROR
        AND #%00011111  ; We use (TIMERA/4) % 32 as an index into SPOFF table
        TAX             ; which contains a sine going from 0 to 32 centered
        LDA SPOFF,X     ; on 16
        CLC
        ADC #144
        STA $D000       ; And we use it as sprite X+144
        
        ; We now prepare the scroller text

        LDA $D016       ; We decrease here the XSCROLL so at every frame we
        CLC             ; smoothly scroll one pixel to the left.
        ADC #7          ; As it stands now this will affect all the screen
        AND #%00000111  ; below the rasterbar.
        ORA #%11001000
        STA $D016

        AND #%00000111  ; Unless we scrolled all the way through the 8 pixels     
        CMP #%00000111  ; skip the next block which shifts the text one whole
        BNE SKIPTL      ; char left

        LDX TSCROLL     ; TSCROLL holds the current scroll position in chars
        INC TSCROLL     ; scroll left by one char
        LDY #0          ; Copy the string starting from the TSCROLL offset
TLOOP   LDA STEXT,X     ; into screen memory (TROLLSK)
        CMP #0          ; Unless it's a zero (@)
        BNE TROLLSK     ;
        LDA #0          ; in which case we restart from the beginning of the
        STA TSCROLL     ; string
        JMP SKIPTL
TROLLSK STA TSCSTART,Y  ; Store into screen memory starting from TSCSTART        
        INX
        INY
        TYA
        CMP #40         ; Stop after copying 40 chars as we reach the border
        BNE TLOOP

SKIPTL
        PLA             ; Restore Y,X,A from the stack
        TAY
        PLA
        TAX
        PLA

        RTI

; These are the bars colour codes. 
; We need to ensure this stuff is all within a page as we load
; it with an LDA absolute,X which takes one more cycle if the indexed
; value is on a different page of the absolute value.
*=$E000
BARCOL  BYTE 00,00,00,06,06,00,06,06,06,06,14
        BYTE 06,14,14,03,14,03,03,03
        BYTE 01,03,01,01,01,03,01,03
        BYTE 03,03,14,03,14,14,06,14
        BYTE 06,06,06,06,00,06,128

SCRCOL  BYTE 06,06,14,14,14,03,14,14

; Sprite offsets, this describes an harmonic motion for the piece
; at the center of the bar. A linear (constant speed) one could be
; done in software but this looks better.
SPOFF   BYTE 16,19,22,25,27,29,31,31
        BYTE 31,31,30,28,26,23,20,17
        BYTE 14,11,08,05,03,01,00,00
        BYTE 00,00,02,04,06,09,12,15

; Scroller text. We have 40 lead and tail spaces so things enter
; and exit the screen nicely. The char @ (screen code 0) signals the
; end of the string.
STEXT   TEXT '                                         '
        TEXT 'THIS IS A TEST SCROLL TEST, NOT MUCH TO SAY FOR NOW '
        TEXT 'THOUGH I WILL MAKE THIS A BIT LONGER SO IT IS EASIER TO '
        TEXT 'SEE WHAT IT REALLY LOOKS LIKE'
        TEXT '                                         @'

; Spite data
*=$2000 
SPRITE0 BYTE $00,$7F,$00,$01,$FF,$C0,$03,$FF
        BYTE $E0,$03,$E7,$E0,$07,$D9,$F0,$07
        BYTE $DF,$F0,$07,$D9,$F0,$03,$E7,$E0
        BYTE $03,$FF,$E0,$03,$FF,$E0,$02,$FF
        BYTE $A0,$01,$7F,$40,$01,$3E,$40,$00
        BYTE $9C,$80,$00,$9C,$80,$00,$49,$00
        BYTE $00,$49,$00,$00,$3E,$00,$00,$3E
        BYTE $00,$00,$3E,$00,$00,$1C,$00,$00




