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
        TYA
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

TRACK1  WORD $0005, G_4, $0000, $2211
        WORD $0008, G_4, $0000, $2210
 
        WORD $0010, G_4, $0000, $2211
        WORD $0013, G_4, $0000, $2210
        
        WORD $0020, A_4, $0000, $2211
        WORD $0023, A_4, $0000, $2210
        
        WORD $0030, G_4, $0000, $2211
        WORD $0033, G_4, $0000, $2210
        
        WORD $0040, C_5, $0000, $2211
        WORD $0043, C_5, $0000, $2210
        
        WORD $0050, B_4, $0000, $2211
        WORD $0053, B_4, $0000, $2210
        
        WORD $0060, $FFFF, $0000, $0000

C_3 = $861
Cd_3 = $8E1
D_3 = $968
Dd_3 = $9F7
E_3 = $A8F
F_3 = $B30
Fd_3 = $BDA
G_3 = $C8F
Gd_3 = $D4E
A_3 = $E18
Ad_3 = $EEF
B_3 = $FD2
C_4 = $10C3
Cd_4 = $11C3
D_4 = $12D1
Dd_4 = $13EF
E_4 = $151F
F_4 = $1660
Fd_4 = $17B5
G_4 = $191E
Gd_4 = $1A9C
A_4 = $1C31
Ad_4 = $1DDF
B_4 = $1FA5
C_5 = $2187
Cd_5 = $2386
D_5 = $25A2
Dd_5 = $27DF
E_5 = $2A3E
F_5 = $2CC1
Fd_5 = $2F6B
G_5 = $323C
Gd_5 = $3539
A_5 = $3863
Ad_5 = $3BBE
B_5 = $3F4B
