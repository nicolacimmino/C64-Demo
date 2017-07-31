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

                        ; THIS IS THE FIRST RASTER INTERRUPT SERVICE ROUTINE.
                        ; BY THE TIME WE COME HERE WE CAN BE ANYWHERE ON THE
                        ; DESIRED LINE WITH A JITTER OF 7 CYCLES DEPENDING ON
                        ; THE INSTRUCTION EXECUTING WHEN THE INTERRUPT APPENED.

*=$C400

ISR     PHA             ; PRESERVE A,X,Y ON THE STACK
        TXA
        PHA
        TYA
        PHA

        TSX             ; WE ARE ABOUT TO LET ANOTHER INTERRUPT HAPPEN
                        ; WITHOUT CALLING RTI, SO THERE WILL BE ONE EXTRA
                        ; RETURN ADDRESS ON THE STACK. SAVE THE GOOD STACK
                        ; POINTER INTO X, WE WILL RESTORE IT LATER BEFORE RTI

        INC  $D012      ; SETUP ANOTHER RASTER INTERRUPT FOR THE NEXT
        LDA  #<ISR2     ; SCAN LINE
        STA  $FFFE
        LDA  #>ISR2
        STA  $FFFF

        LSR  $D019      ; ACKNOWELEDGE VIDEO INTERRUPTS

        CLI

        NOP             ; WASTE TIME WAITING THE INTERRUPT TO HAPPEN
        NOP             ; THESE ARE NOPS AS THEY TAKE 2 CYCLES THE JITTER
        NOP             ; OF THE NEXT INTERRUPT WILL BE ONLY 1 CYCLE.
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
        NOP             ; THESE ARE ENOUGH TO COMPLETE A SCAN LINE.
        NOP             ; WE WILL  NEVER COME HERE AS  THE INTERRUPT WILL HAPPEN
        NOP             ; BEFORE AND BY RESETTING THE STACK POINTER RTI WILL NOT
        NOP             ; RETURN HERE EITHER.

                        ; THIS IS  THE SECOND  RASTER INTERRUPT  ROUTINE. BY THE 
                        ; TIME WE COME HERE WE HAVE  A JITTER OF  JUST ONE CYCLE 
                        ; AS WE ENSURED THIS  INTERRUPT  HAPPENS WHILE EXECUTING 
                        ; NOPS (2  CYCLES). NEXT  WE ENSURE  WE SPEND  THE EXACT 
                        ; AMOUNT  OF CYCLES  IT TAKES  TO DRAW A FULL SCAN LINE. 
                        ; THE LAST BEQ *+2 WE USE DOES THE SYNC MAGIC. SEE NOTES
                        ; BELOW. THIS  IS TIMED  FOR  PAL SYSTEMS THE DELAY LOOP 
                        ; NEEDS TO BE CHANGED FOR NTSC.

                        ; INTERRUPT SERVICING (DURING A NOP)          2/3 CYCLES
ISR2    TXS             ; RESTORE THE SP MESSED BY THE INTERRUPT.       2 CYCLES

        LDY  #8         ; THIS LOOP, THE INTERRPUT CALL THE ABOVE TXS AND
        DEY             ; THE BELOW BIT AND LDA TAKE EXACTLY ONE SCAN LINE
        BNE  *-1        ; MINUS 1 CYCLE.
                        ;                                              46 CYCLES
        BIT  $00        ;                                               3 CYCLES
        NOP
        NOP

        LDA  $D012      ; GET CURRENT SCAN LINE                         4 CYCLES
        CMP  $D012      ; HERE WE ARE EITHER STILL ON THE SAME LINE     4 CYCLES
                        ; (WITH ONE CYCLE TO GO) OR AT THE NEXT LINE.
        BEQ  *+2        ; IF WE ARE ON SAME LINE BRANCH (3 CYCLES)    3/2 CYCLES
                        ; ELSE MOVE ON (2 CYCLES). NOTE THAT IN BOTH
                        ; CASES WE END UP AT THE NEXT INSTRUCTION, BUT IT
                        ; WILL TAKE DIFFERENT TIME TO GET THERE SO WE
                        ; OFFSET THE REMAINING 1 CYCLE JITTER.

                        ; FROM HERE ON WE ARE STABLE.

        LDY  #10        ; PUSH FORWARD SO THE STA $D020/1
        DEY             ; ARE IN THE HORIZONTAL SYNC AREA
        BNE  *-1        ;
        BIT  $00

        LDY  #$FF       ; Y WILL BE USED TO INDEX TABLE BARCOL, WE START FROM FF
                        ; SO WHEN WE INY BELOW WE ROLL TO 00

BLOOP   INY             ; NEXT COLOUR ENTRY

        LDA  $D012      ; WE NEED TO AVOID BAD LINES, WE SET YSCROLL TO
        CLC             ; (CURRENT RASTER LINE + 7)%8 SO THAT THE BAD LINE
        ADC  #7         ; IS ALWAYS THE PREVIOUS LINE.
        AND  #%00000111
        ORA  #%00011000
        STA  $D011

        LDA  ZEROVAL    ; WE NEED ZERO IN A IN 3 CYLCLES SO WE LOAD FROM PAGE
                        ; ZERO FROM A LOCATION WE SET TO ZERO. SEE §1 COMMENTS.
        STA  $D021      ; BLACK VERTICAL BAR STARTS HERE...
        NOP             ;
        LDA  BARCOL,Y   ;
        STA  $D021      ; ...AND ENDS HERE
        BIT  $00        ; WASTE MORE CYCLES TO FILL UP THE RASTER LINE
        NOP

        LDA  BARCOL,Y   ; GET THE CURRENT BAR COLOUR
        STA  $D020      ; AND SET IT FOR BORDER
        STA  $D021      ; AND BACKGROUND COLOUR

        AND  #%10000000 ; TEST BIT-7 OF THE COLOUR IS SET
        BNE  BLEND      ; IF SET WE ARE DONE WITH THE BAR

        JMP  BLOOP      ; AND REPEAT FOR NEXT COLOUR.

                        ; WE ARE  DONE WITH THE INTERRUPT, WE NEED TO SET UP THE
                        ; NEXT ONE AND RESTORE REGISTERS BEFORE LEAVING.

BLEND   LDA  #RASSTART  ; SET RASTER INTERRUPT TO THE START OF THE BAR
        STA  $D012

        CLC             ; ENSURE NEXT  INTERRUPT  WILL NOT HAPPEN ON A BAD LINE,
        ADC  #7         ; SET YSCROLL SO THE BADLINE IS ONE LINE BEFORE THE NEXT 
        AND  #%00000111 ; INTERRUPT  (RASTART IS IN THE ACCUMULATOR AT THE START
        ORA  #%00011000 ; OF THIS BLOCK)
        STA  $D011

        LDA  #<ISR      ; SET THE INTERRUPT VECTOR BACK TO THE FIRST ISR
        STA  $FFFE      ; (THE RASTER SYNC ONE)
        LDA  #>ISR
        STA  $FFFF

        LSR  $D019      ; ACKNOWELEDGE VIDEO INTERRUPTS

        JSR  MUPLAY     ; ADVANCE THE TRACKER BY ONE TICK.

                        ; THIS IS A GOOD TIME TO TAKE CARE OF THE TIME BASE.
                        ; WE INCREMENT TIMERA ONCE PER FRAME.THIS SERVES AS TIME
                        ; BASE FOR ANIMATIONS AND SCROLLS.

        INC  TIMERA     ; INCREMENT TIME BASE

                        ; HERE WE MOVE  THE SPRITE  IN THE  BAR. THIS  IS A GOOD
                        ; PLACE CAUSE THE SPRITE HAS JUST BEEN DRAWN SO WE DON'T 
                        ; RISK TO TEAR IT HORIZONTALLY

        LDA  TIMERA
        ROR
        ROR
        AND  #%00011111 ; WE USE (TIMERA/4) % 32 AS AN INDEX INTO SPOFF TABLE
        TAX             ; WHICH CONTAINS A SINE GOING FROM 0 TO 32 CENTERED
        LDA  SPOFF,X    ; ON 16
        CLC
        ADC  #144
        STA  $D000      ; AND WE USE IT AS SPRITE X+144

        LDA  TIMERA     ; WE USER TIMERA/32 TO CHANGE THE LAMP SPRITE FROM AN ON
        AND  #%00010000 ; LAMP (YELLOW) TO OFF (BLACK)
        BEQ  SPBLK
        LDA  #$07
SPBLK   STA  $D026

                        ; WE NOW PREPARE THE SCROLLER TEXT

        LDA  $D016      ; WE DECREASE HERE THE XSCROLL SO AT EVERY FRAME WE
        CLC             ; SMOOTHLY SCROLL ONE PIXEL TO THE LEFT.
        ADC  #7         ; AS IT STANDS NOW THIS WILL AFFECT ALL THE SCREEN
        AND  #%00000111 ; BELOW THE RASTERBAR.
        ORA  #%11001000
        STA  $D016

        AND  #%00000111 ; UNLESS WE SCROLLED ALL THE WAY THROUGH THE 8 PIXELS
        CMP  #%00000111 ; SKIP THE NEXT BLOCK WHICH SHIFTS THE TEXT ONE WHOLE
        BNE  ENDINT     ; CHAR LEFT

        LDX  TSCROLL    ; TSCROLL HOLDS THE CURRENT SCROLL POSITION IN CHARS
        INC  TSCROLL    ; SCROLL LEFT BY ONE CHAR
        LDY  #0         ; COPY THE STRING STARTING FROM THE TSCROLL OFFSET
TLOOP   LDA  STEXT,X    ; INTO SCREEN MEMORY (TROLLSK)
        CMP  #0         ; UNLESS IT'S A ZERO (@)
        BNE  TROLLSK    ;
        LDA  #0         ; IN WHICH CASE WE RESTART FROM THE BEGINNING OF THE
        STA  TSCROLL    ; STRING
        JMP  ENDINT
TROLLSK STA  TSCSTART,Y ; STORE INTO SCREEN MEMORY STARTING FROM TSCSTART
        INX
        INY
        TYA
        CMP  #40        ; STOP AFTER COPYING 40 CHARS AS WE REACH THE BORDER
        BNE  TLOOP

ENDINT  PLA             ; RESTORE Y,X,A FROM THE STACK
        TAY
        PLA
        TAX
        PLA

        RTI

; THESE ARE THE BARS COLOUR CODES.
; WE NEED TO ENSURE THIS STUFF IS ALL WITHIN A PAGE AS WE LOAD
; IT WITH AN LDA ABSOLUTE,X WHICH TAKES ONE MORE CYCLE IF THE INDEXED
; VALUE IS ON A DIFFERENT PAGE OF THE ABSOLUTE VALUE.
*=$E000
BARCOL  BYTE 00,00,00,06,06,00,06,06,06,06,14
        BYTE 06,14,14,03,14,03,03,03
        BYTE 01,03,01,01,01,03,01,03
        BYTE 03,03,14,03,14,14,06,14
        BYTE 06,06,06,06,00,06,128

SCRCOL  BYTE 01,01,03,03,01,01,03,03

; SPRITE OFFSETS, THIS DESCRIBES AN HARMONIC MOTION FOR THE PIECE
; AT THE CENTER OF THE BAR. A LINEAR (CONSTANT SPEED) ONE COULD BE
; DONE IN SOFTWARE BUT THIS LOOKS BETTER.
SPOFF   BYTE 16,19,22,25,27,29,31,31
        BYTE 31,31,30,28,26,23,20,17
        BYTE 14,11,08,05,03,01,00,00
        BYTE 00,00,02,04,06,09,12,15

; SCROLLER TEXT. WE HAVE 40 LEAD AND TAIL SPACES SO THINGS ENTER
; AND EXIT THE SCREEN NICELY. THE CHAR @ (SCREEN CODE 0) SIGNALS THE
; END OF THE STRING.
STEXT   TEXT '                                         '
        TEXT 'THIS IS A TEST SCROLL TEST, NOT MUCH TO SAY FOR NOW '
        TEXT 'THOUGH I WILL MAKE THIS A BIT LONGER SO IT IS EASIER TO '
        TEXT 'SEE WHAT IT REALLY LOOKS LIKE'
        TEXT '                                         @'

*=$3000
FONT0   BYTE $00,$00,$00,$0C,$3F,$F0,$C0,$00
        BYTE $7C,$FE,$C6,$C6,$FE,$FE,$C6,$C6
        BYTE $FC,$FE,$C6,$FC,$C6,$C6,$FE,$FC
        BYTE $7E,$FE,$C0,$C0,$C0,$C0,$FE,$7E
        BYTE $FC,$FE,$C6,$C6,$C6,$C6,$FE,$FC
        BYTE $FE,$FE,$C0,$FC,$FC,$C0,$FE,$FE
        BYTE $FE,$FE,$C0,$FC,$FC,$C0,$C0,$C0
        BYTE $7C,$FE,$C0,$C0,$CE,$C6,$FE,$7C
        BYTE $C6,$C6,$C6,$FE,$FE,$C6,$C6,$C6
        BYTE $7C,$38,$38,$38,$38,$38,$38,$7C
        BYTE $FE,$FE,$0C,$0C,$CC,$CC,$FC,$78
        BYTE $CC,$DC,$F8,$F0,$F8,$DC,$CE,$C6
        BYTE $C0,$C0,$C0,$C0,$C0,$C0,$FE,$FE
        BYTE $C6,$EE,$FE,$FE,$D6,$C6,$C6,$C6
        BYTE $C6,$E6,$F6,$FE,$DE,$CE,$C6,$C6
        BYTE $7C,$FE,$C6,$C6,$C6,$C6,$FE,$7C
        BYTE $FC,$FE,$C6,$C6,$FE,$FC,$C0,$C0
        BYTE $7C,$FE,$C6,$C6,$D6,$CE,$FE,$7C
        BYTE $FC,$FE,$C6,$C6,$FC,$F8,$DC,$CE
        BYTE $7E,$FE,$C0,$FC,$7E,$06,$FE,$FC
        BYTE $FE,$FE,$38,$38,$38,$38,$38,$38
        BYTE $C6,$C6,$C6,$C6,$C6,$C6,$FE,$7C
        BYTE $C6,$C6,$C6,$C6,$6C,$6C,$38,$38
        BYTE $C6,$C6,$C6,$C6,$C6,$D6,$FE,$6C
        BYTE $C6,$C6,$EE,$7C,$38,$7C,$EE,$C6
        BYTE $C6,$C6,$6C,$7C,$38,$38,$38,$38
        BYTE $FE,$FE,$1C,$38,$70,$E0,$FE,$FE
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $7C,$FE,$C6,$C6,$C6,$C6,$FE,$7C
        BYTE $38,$78,$38,$38,$38,$38,$38,$7C
        BYTE $7C,$FE,$C6,$06,$7C,$C0,$FE,$FE
        BYTE $FE,$FE,$1C,$38,$1C,$C6,$FE,$7C
        BYTE $1C,$3C,$6C,$CC,$FE,$FE,$0C,$0C
        BYTE $FE,$FE,$C0,$FC,$06,$C6,$FE,$7C
        BYTE $7C,$FE,$C0,$FC,$C6,$C6,$FE,$7C
        BYTE $FE,$FE,$0C,$0C,$18,$18,$30,$30
        BYTE $7C,$C6,$C6,$7C,$C6,$C6,$FE,$7C
        BYTE $7C,$FE,$C6,$C6,$7E,$06,$FE,$7C

; SPITE DATA
*=$2000
;SPRITE0 BYTE $00,$7F,$00,$01,$FF,$C0,$03,$FF
;        BYTE $E0,$03,$E7,$E0,$07,$D9,$F0,$07
;        BYTE $DF,$F0,$07,$D9,$F0,$03,$E7,$E0
;        BYTE $03,$FF,$E0,$03,$FF,$E0,$02,$FF
;        BYTE $A0,$01,$7F,$40,$01,$3E,$40,$00
;        BYTE $9C,$80,$00,$9C,$80,$00,$49,$00
;        BYTE $00,$49,$00,$00,$3E,$00,$00,$3E
;        BYTE $00,$00,$3E,$00,$00,$1C,$00,$00

 
SPRITE0  BYTE $00,$AA,$00,$02,$FF,$80,$02,$FF
         BYTE $80,$0B,$FF,$E0,$0B,$FF,$E0,$0B
         BYTE $D7,$E0,$0B,$7D,$E0,$0B,$7D,$E0
         BYTE $0B,$7D,$E0,$02,$D7,$80,$02,$D7
         BYTE $80,$02,$D7,$80,$00,$AA,$00,$00
         BYTE $96,$00,$00,$9A,$00,$00,$A6,$00
         BYTE $00,$9A,$00,$00,$A6,$00,$00,$9A
         BYTE $00,$00,$A6,$00,$00,$28,$00,$00



