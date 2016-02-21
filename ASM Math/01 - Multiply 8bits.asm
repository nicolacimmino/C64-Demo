; *****************************************************************************
; * 8-bits multiplication.                                                    * 
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

; Result can be found at $2132 (decimal 8502)
*= $2134
mul8a   byte 0
mul8b   byte 0   
mul8res byte 0

; * This is the actual beginning of our assembly program.
*=$C000
 
        LDA #12         ; Operand a
        STA mul8a
        LDA #6          ; Operand b
        STA mul8b
        JSR mul8 
        RTS

mul8    LDA #0
        BEQ mul8st
mul8add CLC
        ADC mul8a
mul8cnt ASL mul8a
        BEQ mul8end
mul8st  ROR mul8b
        BCS mul8add
        BCC mul8cnt

mul8end STA mul8res
        RTS
