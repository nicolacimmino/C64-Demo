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
TICK    WORD $0000      ; STORES THE CURRENT TICK, ONE TICK PER CALL TO MUPLAY
T1POS   BYTE $00        ; POINTER INSIDE TRACK1 TABLE

MUINIT  LDX  #24        ; CLEAR ALL SID REGISTERS
        LDA  #0
        STA  $D400,X
        DEX
        BNE  *-4

        LDA  #%00001111 ; SET VOLUME TO MAX
        STA  $D418

        LDA  #$FC       ; PRESET RELEASE TIME AND MAX SUSTAIN VOLUNE. THIS IS
        STA  $D406      ; BECAUSE TO OPTIMIZE WE USE ONLY AD IN THE TRACK DATA.

        RTS

MUPLAY  INC  TICK       ; NEXT TICK. THIS IS A 16 BITS VALUE, WE INCREMENT THE
        LDA  TICK       ; LSB FIRST AND, IF WE OVERFLOW TO 0, INCREMENT THE MSB
        CMP  #0
        BNE  *+5
        INC  TICK+1

        LDA  T1POS      ; FIRST WORD OF TRACK DATA IS THE POSITION WHERE THE
        TAX             ; NOTE SHOULD BE PLAYED. THIS IS A 16 BIT COMPARISON,
        LDA  TRACK1,X   ; IF WE ARE NOT AT THE WANTED TICK WE JUST EXIT.
        CMP  TICK
        BEQ  *+3
        RTS
        INX
        LDA  TRACK1,X
        CMP  TICK+1
        BEQ  *+3
        RTS
        INX

        LDA  TRACK1+1,X ; WE RECYCLE THE FREQUENCY WORD FOR SPECIAL COMMANDS
        CMP  #$FF       ; BY SETTING TO $FF THE HI PART. FOR NOW ONLY RESET
        BEQ  TRK1RES    ; TO THE START OF THE TRACK.

        LDY  #0         ; THE NEXT 6 BYTES ARE ALL THE REGISTERS OF THE VOICE
MORE    LDA  TRACK1,X   ; EXCEPT FOR FOR THE SR CONTROL REGISTER WHICH IS LEFT
        STA  $D400,Y    ; TO A FIXED VALUE (SEE MUINIT).
        INX             ; COPY THEM OVER.
        INY
        TYA
        CMP  #6
        BNE  MORE

        STX  T1POS      ; NEW OFFSET IN THE TRACK1 TABLE.

        RTS


TRK1RES LDA  #0         ; JUMP TO START COMMAND. RESET INDEX IN THE TRACK1 TABLE
        STA  T1POS      ; AND TICK AS WELL SINCE ALL TIMING DEPENDS ON THAT.
        STA  TICK
        STA  TICK+1

        RTS

; *****************************************************************************
; *                                                                           *
; * THIS TRACK IS PLAYED ON VOICE 1. EACH ENTRY IN THE TRACK IS MADE OF       *
; * 4 WORDS REPRESETING RESPECTIVELY:                                         *
; *                                                                           *
; *     TICK    THE TICK AT WHICH THIS SET OF SID REGISTERS SHOULD BE SET     *
; *     FREQ    THE FREQUENCY OF THE NOTE                                     *
; *     PWID    PULSE WIDTH CONTROL REGISTERS (LITTLE ENDIAN $D402 FIRST LSB) *
; *     ADCR    ATTACK/DECAY REGISTER (MSB) AND CONTROL REGISTER (LSB)        *
; *                                                                           *
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

        WORD $0060, $FF00, $0000, $0000
; *                                                                           *
; *****************************************************************************

; *****************************************************************************
; * CONSTANTS DEFINING THE SID FREQUENCY HI/LO VALUE FOR 3 OCTAVES.           *
; *                                                                           *
C_3     = $861
Cd_3    = $8E1
D_3     = $968
Dd_3    = $9F7
E_3     = $A8F
F_3     = $B30
Fd_3    = $BDA
G_3     = $C8F
Gd_3    = $D4E
A_3     = $E18
Ad_3    = $EEF
B_3     = $FD2
C_4     = $10C3
Cd_4    = $11C3
D_4     = $12D1
Dd_4    = $13EF
E_4     = $151F
F_4     = $1660
Fd_4    = $17B5
G_4     = $191E
Gd_4    = $1A9C
A_4     = $1C31
Ad_4    = $1DDF
B_4     = $1FA5
C_5     = $2187
Cd_5    = $2386
D_5     = $25A2
Dd_5    = $27DF
E_5     = $2A3E
F_5     = $2CC1
Fd_5    = $2F6B
G_5     = $323C
Gd_5    = $3539
A_5     = $3863
Ad_5    = $3BBE
B_5     = $3F4B
; *                                                                           *
; *****************************************************************************
