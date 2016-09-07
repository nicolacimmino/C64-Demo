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

; * Below is tbe BASIC tokens for 10 SYS (49152)
; * We store them at the beginning of the BASIC RAM so when we can load
; * the program with autorun (LOAD "*",8,1) and save to type the SYS.
*=$801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $34, $39, $31, $35, $32
        BYTE    $29, $00, $00, $00

; *
; * Constants
; *
RASSTART = 80           ; Raster line where the raster interrupt starts

; * This is the actual beginning of our assembly program.
*=$C000

start   SEI             ; Prevent interrupts while we set things up
        JSR $FF81       ; Reset VIC, clear screen
        LDA #%01111111  ; Disable CIA-1 interrupts
        STA $DC0D
        LDA #$01        ; Enable raster interrupt
        STA $D01A
        LDA #RASSTART   ; Set raster interrupt line
        STA $D012
        LDA #%01111111  ; Clear RST8 bit, the interrupt line is
        AND $D011       ; above raster line 255 
        STA $D011        

        LDA #$35        ; Disable kernal and BASIC ROMs
        STA $01         ; we go bare metal.

        LDA #<irq       ; Setup the interrupt vector to our function  
        STA $FFFE       ; note that this vector is for ALL interrupts,
        LDA #>irq       ; not only the VIC raster interrupt.
        STA $FFFF       ; Anyhow all others are disabled.

        CLI             ; Let interrupts come 

        ; This is our main loop. For now we don't do anything
        ; useful, just a mix of different length instructions
        ; so we maximise the raster interrupt jitter to simulate
        ; real code.
loop    LDA #1          ; 2 cycles
        LDA $01         ; 3 cycles
        LDA $0400       ; 4 cycles
        LDA ($04),Y     ; 5 cycles      
        LDA ($04,X)     ; 6 cycles 
        LSR $0400,X     ; 7 cycles
        JMP loop

        ; This is the first raster interrupt service routine.
        ; By the time we come here we can be anywhere from the
        ; start of the desired line to +7 cycles depending on
        ; the instruction that was execuing when the interrupt
        ; occurred.
irq     PHA             ; Preserve A,X,Y on the stack
        TXA
        PHA
        TYA
        PHA
        
        INC $D012       ; Setup another raster interrupt for the next
        LDA #<irq2      ; scan line   
        STA $FFFE       
        LDA #>irq2      
        STA $FFFF

        TSX             ; We are about to let another interrupt happen
                        ; without calling RTI, so there will be one extra
                        ; return address on the stack. Save the good stack
                        ; pointer into X, we will restore it later before RTI
        
        ASL $D019       ; Clear the interrupt flag        
        CLI

        NOP             ; Waste time waiting the interrupt to happen
        NOP             ; these are NOPs as they take 2 cycles the jitter
        NOP             ; of the next interrupt will be only 1 cycle.
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP             ; These are enough to complete a scan line.

        ; This is the second raster interrupt routine. By the time
        ; we come here we have a jitter of just one cycle as we ensured
        ; this interrupt happens while executing NOPs (2 cycles).
        ; Next we ensure we spend the exact amount of cycles it takes 
        ; to draw a full scan line. The last instruction we use does the 
        ; sync magic. See comments below. This is timed for PAL systems
        ; the delay loop needs to be changed for NTSC.

irq2    TXS             ; Restore stack pointer as the interrupt messed it.

        LDX #$08        ; This loop, the above TXS and the below BIT and LDA
        DEX             ; take exactly one scan line minus 1 cycle.
        BNE *-1    
        BIT $00         ; Just a 3 cycles instruction
        
        LDA $d012       ; Get current scan line
        CMP $d012       ; Here we are either still on the same line
                        ; (with one cycle to go) or at the next line.
        BEQ *+2         ; If we are on the same line branch (3 cycles)
                        ; else move on (2 cycles). Note that in both
                        ; cases we end up at the next instruction, but it
                        ; will take different time to get there so we 
                        ; offset the remaining 1 cycle jitter.
        
        ; From here on we are stable.

        LDA #1          ; Just flick the color for few cycles
        STA $D020       ; to show the interrupt is stable
        STA $D021
        LDA #0          ; Back to separate border/background colors
        STA $D020       ; So we see where things are relative to our stroke
        LDA #2          
        STA $D021

        ; We are done with the interrupt, we need to set up
        ; the next one and restore registers before leaving.

        LDA #RASSTART   ; Set raster interrupt to the start of the bar
        STA $D012
        LDA #<irq        
        STA $FFFE       
        LDA #>irq       
        STA $FFFF

        ASL $D019       ; Clear the interrupt flag        
        LDA $DC0D
      
        PLA             ; Restore A,X,Y from the stack
        TAY
        PLA
        TAX
        PLA

        RTI

