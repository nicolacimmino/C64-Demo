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
TMP1     = $000B        ; Page 0 location for temporary storage

; * This is the actual beginning of our assembly program.
*=$C000

start   SEI             ; Prevent interrupts while we set things up
        JSR $FF81       ; Reset VIC, clear screen
        LDA #%01111111  ; Disable CIA-1 interrupts
        STA $DC0D
        STA $DD0D
        LDA #%00000001  ; Enable raster interrupt
        STA $D01A
        LDA $DC0D       ; acknowledge CIA 
        LSR $D019       ; and video interrupts
        
        LDA #RASSTART   ; Set raster interrupt line
        STA $D012
        LDA #%01111111  ; Clear RST8 bit, the interrupt line is
        AND $D011       ; above raster line 255 
        STA $D011        

        LDA #%00110101  ; Disable kernal and BASIC ROMs
        STA $01         ; we go bare metal.

        LDA #<ISR       ; Setup the interrupt vector to our function  
        STA $FFFE       ; note that this vector is for ALL interrupts,
        LDA #>ISR       ; not only the VIC raster interrupt.
        STA $FFFF       ; Anyhow all others are disabled.

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
        
        LDA #%00000001
        STA $D019       ; Acknoweledge the interrupt  
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

        LDY #$FF        ; Y will be used to index table BARCOL, we start from FF
                        ; so when we INY below we roll to 00

BLOOP   INY             ; Next colour entry

        LDA BARCOL,Y    ; Get the current bar colour                          
        STA $D020       ; and set it for border                        
        STA $D021       ; and background colour                         
        
        LDA $D012       ; We need to avoid bad lines, we set YSCROLL to
        CLC             ; (current raster line + 7)%8 so that the bad line
        ADC #7          ; is always the previous line.
        AND #%00000111  
        ORA #%00011000  
        STA $D011

        BIT $00         ; Waste 3 cycles                
        LDA #0          ; Black vertical bar starts here...
        STA $D021
        NOP       
        LDA BARCOL,Y    ; ...and ends here                  
        STA $D021       ;  
        BIT $00         ; Waste more cycles to complete the raster line
        NOP       
        NOP
                        
        AND #%10000000  ; We still have the colour from BARCOL in A, test bit-7
        BNE BLEND       ; if set we are done with the bar
 
        JMP BLOOP       ; And repeat for next colour.
           
        ; We are done with the interrupt, we need to set up
        ; the next one and restore registers before leaving.

BLEND   LDA #%11111000  ; Restore YSCROLL to 0         
        AND $D011       
        STA $D011 

        LDA #RASSTART   ; Set raster interrupt to the start of the bar
        STA $D012
        LDA #<ISR        
        STA $FFFE       
        LDA #>ISR       
        STA $FFFF

        LDA #1
        STA $D019       ; Aknoweledge the interrupt        
        
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
BARCOL  BYTE 06,06,00,06,06,06,06,14
        BYTE 06,14,14,03,14,03,03,03
        BYTE 01,03,01,01,01,03,01,03
        BYTE 03,03,14,03,14,14,06,14
        BYTE 6,6,6,6,0,6,6,128
