; *****************************************************************************
; * Simple (unstable) rastebar example.                                       * 
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
BARPOS = 80             ; Raster line where the green bar starts
BACKCOL = 0             ; Background color

; * This is the actual beginning of our assembly program.
*=$C000

start   SEI             ; Prevent interrupts while we set things up
        JSR $FF81       ; Reset VIC, clear screen
        LDA #%01111111  ; Disable CIA-1 interrupts
        STA $DC0D
        LDA #$01        ; Enable raster interrupt
        STA $D01A
        LDA #BARPOS     ; Set raster interrupt to the start of the bar
        STA $D012
        LDA #%01111111  ; Clear RST8 bit, for now we don't work over
        AND $D011       ; raster line 255 
        STA $D011        

        LDA #$35        ; Disable kernal and BASIC ROMs
        STA $01

        LDA #<irq       ; Setup the interrupt vector to our function  
        STA $FFFE       ; note that this vector is for ALL interrupts,
        LDA #>irq       ; not only the VIC raster interrupt
        STA $FFFF

        CLI             ; Let interrupts come 
        JMP *

irq     PHA             ; Preserve A,X,Y on the stack
        TXA
        PHA
        TYA
        PHA
        
        LDA #$EE
delayA  ADC #1
        BNE delayA
        NOP
        NOP
        NOP

        LDX #0
waitbar LDA barcol,X
        STA $D020      
        STA $D021
        INX
        TXA
        AND #%00000111
        TAX
  
        LDA #$E8
delayB  ADC #1
        BNE delayB
        NOP

        LDA $D012       ; Wait until the raster scan reaches the end of the bar
        CMP #BARPOS+25
        BMI waitbar


        LDA #BACKCOL
        STA $D020
        STA $D021

        ASL $D019       ; Clear the interrupt flag
        
        LDA $DC0D
        
        PLA             ; Restore A,X,Y from the stack
        TAY
        PLA
        TAX
        PLA

        RTI

barcol  BYTE 9,5,13,1,1,13,5,9

