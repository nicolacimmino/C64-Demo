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

; * This is the actual beginning of our assembly program.
*=$C000

start   SEI             ; Prevent interrupts while we set things up
        JSR $FF81       ; Reset VIC, clear screen
        LDA #$01        ; Enable raster interrupt
        STA $D01A
        LDA #80         ; Set raster interrupt to line 80
        STA $D012
        LDA #%01111111  ; Clear RST8 bit, for now we don't work over
        AND $D011       ; raster line 255 
        STA $D011        
        LDA #<irq       ; Setup the interrupt vector to our function  
        STA $0314       ; note that this vector is for ALL interrupts,
        LDA #>irq       ; not only the VIC raster interrupt
        STA $0315
        CLI             ; Let interrupts come 
        RTS             ; We are done, back to BASIC prompt

irq     LDA $D019       ; Is this a raster interrupt?
        AND #01
        CMP #01
        BNE endint      ; No, move on
        STA $D019       ; Clear the interrupt flag

        LDA #2          ; Set border and background color to the
        STA $D020       ; desired bar color
        STA $D021
        
waitbar LDA $D012       ; Wait until the raster scan reaches line 185
        CMP #185
        BMI waitbar

        LDA #14         ; Reset the colors to C64 defaults
        STA $D020
        LDA #6
        STA $D021

endint  JMP $EA31       ; We are done run the default interrupt handler cleanup

