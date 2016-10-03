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

        LDA $D018       ; Move character generator memory to $3000
        AND #%11110000  ; so we can display using our custom font.
        ORA #%00001100
        STA $D018

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

*=$3000
        BYTE    $3C,$66,$6E,$6E,$60,$6C,$36,$00
        BYTE    $00,$00,$3C,$67,$66,$66,$3B,$00
        BYTE    $58,$60,$6C,$76,$66,$66,$DC,$00
        BYTE    $00,$1C,$36,$60,$60,$62,$3C,$00
        BYTE    $18,$1C,$0E,$36,$66,$66,$3C,$00
        BYTE    $00,$3C,$66,$6C,$78,$62,$3C,$00
        BYTE    $0C,$10,$18,$3C,$18,$18,$18,$10
        BYTE    $00,$3C,$67,$66,$66,$3C,$46,$7C
        BYTE    $58,$60,$6C,$76,$66,$66,$6C,$0C
        BYTE    $00,$18,$00,$38,$18,$1A,$1C,$00
        BYTE    $00,$18,$00,$38,$18,$18,$18,$34
        BYTE    $5C,$30,$7C,$30,$30,$74,$18,$00
        BYTE    $2C,$18,$18,$18,$18,$38,$0C,$00
        BYTE    $00,$00,$DA,$7F,$6B,$6B,$6B,$00
        BYTE    $00,$00,$CE,$77,$66,$66,$77,$00
        BYTE    $00,$00,$7C,$66,$66,$E6,$38,$00
        BYTE    $00,$C0,$5C,$66,$E6,$7C,$60,$60
        BYTE    $00,$00,$3C,$67,$66,$3E,$06,$06
        BYTE    $00,$00,$6C,$34,$30,$70,$18,$00
        BYTE    $0C,$10,$38,$18,$18,$18,$18,$10
        BYTE    $08,$18,$3C,$18,$18,$38,$0C,$00
        BYTE    $00,$44,$EE,$66,$66,$E6,$3B,$00
        BYTE    $80,$CC,$76,$66,$66,$EC,$38,$00
        BYTE    $00,$8A,$DF,$6B,$6B,$6B,$76,$00
        BYTE    $00,$36,$1A,$18,$38,$6C,$60,$3C
        BYTE    $80,$CC,$76,$66,$66,$6C,$0C,$0C
        BYTE    $00,$3C,$46,$1C,$0F,$36,$36,$1C
        BYTE    $3C,$30,$30,$30,$30,$30,$3C,$00
        BYTE    $0C,$12,$30,$7C,$30,$62,$FC,$00
        BYTE    $3C,$0C,$0C,$0C,$0C,$0C,$3C,$00
        BYTE    $00,$18,$3C,$7E,$18,$18,$18,$18
        BYTE    $00,$10,$30,$7F,$7F,$30,$10,$00
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $18,$18,$18,$18,$00,$00,$18,$00
        BYTE    $36,$36,$6C,$00,$00,$00,$00,$00
        BYTE    $66,$66,$FF,$66,$FF,$66,$66,$00
        BYTE    $06,$3E,$60,$3E,$03,$76,$DC,$00
        BYTE    $62,$66,$0C,$18,$30,$66,$46,$00
        BYTE    $3C,$66,$3C,$38,$67,$66,$3F,$00
        BYTE    $0C,$0C,$18,$00,$00,$00,$00,$00
        BYTE    $0C,$18,$30,$30,$30,$18,$0C,$00
        BYTE    $30,$18,$0C,$0C,$0C,$18,$30,$00
        BYTE    $00,$66,$3C,$FF,$3C,$66,$00,$00
        BYTE    $00,$18,$18,$7E,$18,$18,$00,$00
        BYTE    $00,$00,$00,$00,$00,$18,$18,$30
        BYTE    $00,$00,$00,$7E,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$18,$18,$00
        BYTE    $00,$03,$06,$0C,$18,$30,$60,$00
        BYTE    $18,$36,$66,$66,$66,$6C,$18,$00
        BYTE    $18,$18,$38,$18,$18,$18,$3C,$00
        BYTE    $3C,$66,$66,$3C,$18,$32,$7E,$00
        BYTE    $7E,$66,$0C,$18,$46,$66,$3C,$00
        BYTE    $0C,$1A,$36,$66,$7F,$06,$0F,$00
        BYTE    $76,$5C,$40,$7C,$06,$66,$3C,$00
        BYTE    $1C,$36,$60,$7C,$66,$6C,$38,$00
        BYTE    $76,$5E,$06,$0C,$18,$30,$60,$00
        BYTE    $1C,$36,$66,$3C,$66,$6C,$38,$00
        BYTE    $1C,$36,$66,$3E,$06,$6C,$38,$00
        BYTE    $00,$00,$18,$00,$00,$18,$00,$00
        BYTE    $00,$00,$18,$00,$00,$18,$18,$30
        BYTE    $0E,$18,$30,$60,$30,$18,$0E,$00
        BYTE    $00,$00,$7E,$00,$7E,$00,$00,$00
        BYTE    $70,$18,$0C,$06,$0C,$18,$70,$00
        BYTE    $3C,$66,$66,$0C,$18,$00,$18,$00
        BYTE    $00,$00,$00,$FF,$FF,$00,$00,$00
        BYTE    $63,$B6,$36,$36,$66,$6B,$30,$00
        BYTE    $6C,$B6,$36,$3C,$36,$36,$6C,$00
        BYTE    $36,$68,$68,$68,$68,$66,$3C,$00
        BYTE    $7C,$B6,$33,$33,$63,$76,$DC,$00
        BYTE    $36,$68,$68,$6E,$68,$66,$3C,$00
        BYTE    $77,$DC,$18,$1E,$CC,$6C,$38,$00
        BYTE    $7B,$D6,$D6,$DF,$D3,$63,$3E,$00
        BYTE    $36,$3C,$60,$6E,$33,$B3,$E6,$1C
        BYTE    $76,$DC,$0C,$0C,$0C,$EC,$38,$00
        BYTE    $76,$DC,$0C,$0C,$0C,$6C,$6C,$38
        BYTE    $3E,$63,$7C,$64,$3E,$36,$E3,$00
        BYTE    $3E,$63,$66,$33,$30,$7B,$CE,$00
        BYTE    $DA,$FF,$6B,$6B,$6B,$6A,$DB,$00
        BYTE    $6E,$B3,$33,$36,$36,$36,$E3,$00
        BYTE    $7C,$C6,$C6,$66,$66,$66,$DC,$00
        BYTE    $6C,$B6,$B3,$B6,$33,$7E,$30,$30
        BYTE    $7C,$C6,$C6,$66,$66,$6F,$DD,$00
        BYTE    $6E,$B3,$B3,$3E,$36,$36,$E3,$00
        BYTE    $7B,$CE,$C0,$D6,$DB,$C3,$7E,$00
        BYTE    $7B,$DE,$58,$0C,$0C,$79,$DE,$00
        BYTE    $EE,$66,$66,$66,$66,$66,$BB,$00
        BYTE    $66,$B6,$B3,$B6,$33,$33,$CE,$00
        BYTE    $DB,$6A,$6B,$6B,$6B,$FF,$B6,$00
        BYTE    $76,$DB,$18,$7E,$18,$DB,$6E,$00
        BYTE    $6E,$DB,$5B,$1B,$33,$1B,$43,$3E
        BYTE    $3E,$43,$13,$1E,$13,$03,$66,$DC
        BYTE    $18,$18,$18,$FF,$FF,$18,$18,$18
        BYTE    $C0,$C0,$30,$30,$C0,$C0,$30,$30
        BYTE    $18,$18,$18,$18,$18,$18,$18,$18
        BYTE    $33,$33,$CC,$CC,$33,$33,$CC,$CC
        BYTE    $33,$99,$CC,$66,$33,$99,$CC,$66
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00
        BYTE    $F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0
        BYTE    $00,$00,$00,$00,$FF,$FF,$FF,$FF
        BYTE    $FF,$00,$00,$00,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$00,$00,$FF
        BYTE    $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0
        BYTE    $CC,$CC,$33,$33,$CC,$CC,$33,$33
        BYTE    $03,$03,$03,$03,$03,$03,$03,$03
        BYTE    $00,$00,$00,$00,$CC,$CC,$33,$33
        BYTE    $CC,$99,$33,$66,$CC,$99,$33,$66
        BYTE    $03,$03,$03,$03,$03,$03,$03,$03
        BYTE    $18,$18,$18,$1F,$1F,$18,$18,$18
        BYTE    $00,$00,$00,$00,$0F,$0F,$0F,$0F
        BYTE    $18,$18,$18,$1F,$1F,$00,$00,$00
        BYTE    $00,$00,$00,$F8,$F8,$18,$18,$18
        BYTE    $00,$00,$00,$00,$00,$00,$FF,$FF
        BYTE    $00,$00,$00,$1F,$1F,$18,$18,$18
        BYTE    $18,$18,$18,$FF,$FF,$00,$00,$00
        BYTE    $00,$00,$00,$FF,$FF,$18,$18,$18
        BYTE    $18,$18,$18,$F8,$F8,$18,$18,$18
        BYTE    $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0
        BYTE    $E0,$E0,$E0,$E0,$E0,$E0,$E0,$E0
        BYTE    $07,$07,$07,$07,$07,$07,$07,$07
        BYTE    $FF,$FF,$00,$00,$00,$00,$00,$00
        BYTE    $FF,$FF,$FF,$00,$00,$00,$00,$00
        BYTE    $00,$00,$00,$00,$00,$FF,$FF,$FF
        BYTE    $01,$03,$06,$6C,$78,$70,$60,$00
        BYTE    $00,$00,$00,$00,$F0,$F0,$F0,$F0
        BYTE    $0F,$0F,$0F,$0F,$00,$00,$00,$00
        BYTE    $18,$18,$18,$F8,$F8,$00,$00,$00
        BYTE    $F0,$F0,$F0,$F0,$00,$00,$00,$00
        BYTE    $F0,$F0,$F0,$F0,$0F,$0F,$0F,$0F
        BYTE    $C3,$99,$91,$91,$9F,$93,$C9,$FF
        BYTE    $FF,$FF,$C3,$98,$99,$99,$C4,$FF
        BYTE    $A7,$9F,$93,$89,$99,$99,$23,$FF
        BYTE    $FF,$E3,$C9,$9F,$9F,$9D,$C3,$FF
        BYTE    $E7,$E3,$F1,$C9,$99,$99,$C3,$FF
        BYTE    $FF,$C3,$99,$93,$87,$9D,$C3,$FF
        BYTE    $F3,$EF,$E7,$C3,$E7,$E7,$E7,$EF
        BYTE    $FF,$C3,$98,$99,$99,$C3,$B9,$83
        BYTE    $A7,$9F,$93,$89,$99,$99,$93,$F3
        BYTE    $FF,$E7,$FF,$C7,$E7,$E5,$E3,$FF
        BYTE    $FF,$E7,$FF,$C7,$E7,$E7,$E7,$CB
        BYTE    $A3,$CF,$83,$CF,$CF,$8B,$E7,$FF
        BYTE    $D3,$E7,$E7,$E7,$E7,$C7,$F3,$FF
        BYTE    $FF,$FF,$25,$80,$94,$94,$94,$FF
        BYTE    $FF,$FF,$31,$88,$99,$99,$88,$FF
        BYTE    $FF,$FF,$83,$99,$99,$19,$C7,$FF
        BYTE    $FF,$3F,$A3,$99,$19,$83,$9F,$9F
        BYTE    $FF,$FF,$C3,$98,$99,$C1,$F9,$F9
        BYTE    $FF,$FF,$93,$CB,$CF,$8F,$E7,$FF
        BYTE    $F3,$EF,$C7,$E7,$E7,$E7,$E7,$EF
        BYTE    $F7,$E7,$C3,$E7,$E7,$C7,$F3,$FF
        BYTE    $FF,$BB,$11,$99,$99,$19,$C4,$FF
        BYTE    $7F,$33,$89,$99,$99,$13,$C7,$FF
        BYTE    $FF,$75,$20,$94,$94,$94,$89,$FF
        BYTE    $FF,$C9,$E5,$E7,$C7,$93,$9F,$C3
        BYTE    $7F,$33,$89,$99,$99,$93,$F3,$F3
        BYTE    $FF,$C3,$B9,$E3,$F0,$C9,$C9,$E3
        BYTE    $C3,$CF,$CF,$CF,$CF,$CF,$C3,$FF
        BYTE    $F3,$ED,$CF,$83,$CF,$9D,$03,$FF
        BYTE    $C3,$F3,$F3,$F3,$F3,$F3,$C3,$FF
        BYTE    $FF,$E7,$C3,$81,$E7,$E7,$E7,$E7
        BYTE    $FF,$EF,$CF,$80,$80,$CF,$EF,$FF
        BYTE    $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        BYTE    $E7,$E7,$E7,$E7,$FF,$FF,$E7,$FF
        BYTE    $C9,$C9,$93,$FF,$FF,$FF,$FF,$FF
        BYTE    $99,$99,$00,$99,$00,$99,$99,$FF
        BYTE    $F9,$C1,$9F,$C1,$FC,$89,$23,$FF
        BYTE    $9D,$99,$F3,$E7,$CF,$99,$B9,$FF
        BYTE    $C3,$99,$C3,$C7,$98,$99,$C0,$FF
        BYTE    $F3,$F3,$E7,$FF,$FF,$FF,$FF,$FF
        BYTE    $F3,$E7,$CF,$CF,$CF,$E7,$F3,$FF
        BYTE    $CF,$E7,$F3,$F3,$F3,$E7,$CF,$FF
        BYTE    $FF,$99,$C3,$00,$C3,$99,$FF,$FF
        BYTE    $FF,$E7,$E7,$81,$E7,$E7,$FF,$FF
        BYTE    $FF,$FF,$FF,$FF,$FF,$E7,$E7,$CF
        BYTE    $FF,$FF,$FF,$81,$FF,$FF,$FF,$FF
        BYTE    $FF,$FF,$FF,$FF,$FF,$E7,$E7,$FF
        BYTE    $FF,$FC,$F9,$F3,$E7,$CF,$9F,$FF
        BYTE    $E7,$C9,$99,$99,$99,$93,$E7,$FF
        BYTE    $E7,$E7,$C7,$E7,$E7,$E7,$C3,$FF
        BYTE    $C3,$99,$99,$C3,$E7,$CD,$81,$FF
        BYTE    $81,$99,$F3,$E7,$B9,$99,$C3,$FF
        BYTE    $F3,$E5,$C9,$99,$80,$F9,$F0,$FF
        BYTE    $89,$A3,$BF,$83,$F9,$99,$C3,$FF
        BYTE    $E3,$C9,$9F,$83,$99,$93,$C7,$FF
        BYTE    $89,$A1,$F9,$F3,$E7,$CF,$9F,$FF
        BYTE    $E3,$C9,$99,$C3,$99,$93,$C7,$FF
        BYTE    $E3,$C9,$99,$C1,$F9,$93,$C7,$FF
        BYTE    $FF,$FF,$E7,$FF,$FF,$E7,$FF,$FF
        BYTE    $FF,$FF,$E7,$FF,$FF,$E7,$E7,$CF
        BYTE    $F1,$E7,$CF,$9F,$CF,$E7,$F1,$FF
        BYTE    $FF,$FF,$81,$FF,$81,$FF,$FF,$FF
        BYTE    $8F,$E7,$F3,$F9,$F3,$E7,$8F,$FF
        BYTE    $C3,$99,$99,$F3,$E7,$FF,$E7,$FF
        BYTE    $FF,$FF,$FF,$00,$00,$FF,$FF,$FF
        BYTE    $9C,$49,$C9,$C9,$99,$94,$CF,$FF
        BYTE    $93,$49,$C9,$C3,$C9,$C9,$93,$FF
        BYTE    $C9,$97,$97,$97,$97,$99,$C3,$FF
        BYTE    $83,$49,$CC,$CC,$9C,$89,$23,$FF
        BYTE    $C9,$97,$97,$91,$97,$99,$C3,$FF
        BYTE    $88,$23,$E7,$E1,$33,$93,$C7,$FF
        BYTE    $84,$29,$29,$20,$2C,$9C,$C1,$FF
        BYTE    $C9,$C3,$9F,$91,$CC,$4C,$19,$E3
        BYTE    $89,$23,$F3,$F3,$F3,$13,$C7,$FF
        BYTE    $89,$23,$F3,$F3,$F3,$93,$93,$C7
        BYTE    $C1,$9C,$83,$9B,$C1,$C9,$1C,$FF
        BYTE    $C1,$9C,$99,$CC,$CF,$84,$31,$FF
        BYTE    $25,$00,$94,$94,$94,$95,$24,$FF
        BYTE    $91,$4C,$CC,$C9,$C9,$C9,$1C,$FF
        BYTE    $83,$39,$39,$99,$99,$99,$23,$FF
        BYTE    $93,$49,$4C,$49,$CC,$81,$CF,$CF
        BYTE    $83,$39,$39,$99,$99,$90,$22,$FF
        BYTE    $91,$4C,$4C,$C1,$C9,$C9,$1C,$FF
        BYTE    $84,$31,$3F,$29,$24,$3C,$81,$FF
        BYTE    $84,$21,$A7,$F3,$F3,$86,$21,$FF
        BYTE    $11,$99,$99,$99,$99,$99,$44,$FF
        BYTE    $99,$49,$4C,$49,$CC,$CC,$31,$FF
        BYTE    $24,$95,$94,$94,$94,$00,$49,$FF
        BYTE    $89,$24,$E7,$81,$E7,$24,$91,$FF
        BYTE    $91,$24,$A4,$E4,$CC,$E4,$BC,$C1
        BYTE    $C1,$BC,$EC,$E1,$EC,$FC,$99,$23
        BYTE    $E7,$E7,$E7,$00,$00,$E7,$E7,$E7
        BYTE    $3F,$3F,$CF,$CF,$3F,$3F,$CF,$CF
        BYTE    $E7,$E7,$E7,$E7,$E7,$E7,$E7,$E7
        BYTE    $CC,$CC,$33,$33,$CC,$CC,$33,$33
        BYTE    $CC,$66,$33,$99,$CC,$66,$33,$99
        BYTE    $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        BYTE    $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F
        BYTE    $FF,$FF,$FF,$FF,$00,$00,$00,$00
        BYTE    $00,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        BYTE    $FF,$FF,$FF,$FF,$FF,$FF,$FF,$00
        BYTE    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
        BYTE    $33,$33,$CC,$CC,$33,$33,$CC,$CC
        BYTE    $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        BYTE    $FF,$FF,$FF,$FF,$33,$33,$CC,$CC
        BYTE    $33,$66,$CC,$99,$33,$66,$CC,$99
        BYTE    $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        BYTE    $E7,$E7,$E7,$E0,$E0,$E7,$E7,$E7
        BYTE    $FF,$FF,$FF,$FF,$F0,$F0,$F0,$F0
        BYTE    $E7,$E7,$E7,$E0,$E0,$FF,$FF,$FF
        BYTE    $FF,$FF,$FF,$07,$07,$E7,$E7,$E7
        BYTE    $FF,$FF,$FF,$FF,$FF,$FF,$00,$00
        BYTE    $FF,$FF,$FF,$E0,$E0,$E7,$E7,$E7
        BYTE    $E7,$E7,$E7,$00,$00,$FF,$FF,$FF
        BYTE    $FF,$FF,$FF,$00,$00,$E7,$E7,$E7
        BYTE    $E7,$E7,$E7,$07,$07,$E7,$E7,$E7
        BYTE    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
        BYTE    $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        BYTE    $F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8
        BYTE    $00,$00,$FF,$FF,$FF,$FF,$FF,$FF
        BYTE    $00,$00,$00,$FF,$FF,$FF,$FF,$FF
        BYTE    $FF,$FF,$FF,$FF,$FF,$00,$00,$00
        BYTE    $FE,$FC,$F9,$93,$87,$8F,$9F,$FF
        BYTE    $FF,$FF,$FF,$FF,$0F,$0F,$0F,$0F
        BYTE    $F0,$F0,$F0,$F0,$FF,$FF,$FF,$FF
        BYTE    $E7,$E7,$E7,$07,$07,$FF,$FF,$FF
        BYTE    $0F,$0F,$0F,$0F,$FF,$FF,$FF,$FF
        BYTE    $0F,$0F,$0F,$0F,$F0,$F0,$F0,$F0

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




