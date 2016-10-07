; *****************************************************************************
; *                                                                           *
; * COPYRIGHT (C) 2016 NICOLA CIMMINO                                         *
; *                                                                           *
; *   THIS PROGRAM IS FREE SOFTWARE: YOU CAN REDISTRIBUTE IT AND/OR MODIFY    *
; *   IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY    *
; *   THE FREE SOFTWARE FOUNDATION, EITHER VERSION 3 OF THE LICENSE, OR       *
; *   (AT YOUR OPTION) ANY LATER VERSION.                                     *
; *                                                                           *
; *  THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,          *
; *   BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          *
; *   MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  SEE THE           *
; *   GNU GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            *
; *                                                                           *
; *   YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE       *
; *   ALONG WITH THIS PROGRAM.  IF NOT, SEE HTTP://WWW.GNU.ORG/LICENSES/.     *
; *                                                                           *
; *                                                                           *
; *****************************************************************************

*=$CF00
TICK    WORD $0000
T1POS   BYTE $00

MUINIT  LDX #24
LP1     LDA #0
        STA $D400,X
        DEX
        BNE LP1

        LDA #%00001111
        STA $D418
        
        LDA #$F0
        STA $D406

        RTS

MUPLAY  INC TICK
        LDA TICK
        CMP #0
        BNE NOOVFL
        INC TICK+1
NOOVFL  LDA T1POS
        TAX
        LDA TRACK1,X
        CMP TICK
        BNE DONE
        INX        
        LDA TRACK1,X
        CMP TICK+1
        BNE DONE
        INX
    
        LDA TRACK1,X
        CMP #$FF
        BEQ RESM  
        
        LDY #0
MORE    LDA TRACK1,X
        STA $D400,Y
        INX
        INY
        CMP #6
        BNE MORE
        
        STX T1POS
        JMP DONE

RESM    LDA #0
        STA T1POS
        STA TICK
        STA TICK+1

DONE
        LDA T1POS
        STA $400

        RTS


; Track
; WORD TICK, AD/CR, PW, FREQ

TRACK1  WORD $0001, N_A4, $0000, $1122
        WORD $0002, N_A4, $0000, $1022
 
        WORD $0003, N_A4, $0000, $1122
        WORD $0004, N_A4, $0000, $1022
        
        WORD $0005, $00FF, $0000, $0000

N_A4 = $6D1E
