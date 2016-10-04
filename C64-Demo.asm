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

; * BELOW ARE TBE BASIC TOKENS FOR 10 SYS (49152)
; * WE STORE THEM AT THE BEGINNING OF THE BASIC RAM SO WHEN WE CAN LOAD
; * THE PROGRAM WITH AUTORUN (LOAD "*",8,1) AND SAVE TO TYPE THE SYS.
*=$801

        BYTE $0E, $08, $0A, $00, $9E, $20, $28, $34, $39, $31, $35, $32
        BYTE $29, $00, $00, $00

; *
; * CONSTANTS
; *
RASSTART = 59             ; RASTER LINE WHERE THE RASTER INTERRUPT STARTS
ZEROVAL = 20              ; ZERO PAGE LOCATION SET ALWAYS TO ZERO SEE §1 COMMENTS
TIMERA  = 21              ; TIME BASE, ROUGHLY 1 PER FRAME (50HZ)
TSCROLL = 24              ; CURRENT TEXT PIXEL SCROLL 0-8
TSCSTART = 1624           ; FIRST CHAR OF LINE 15 (1024+(15*40))
TSCOLST = 55896           ; COLOR RAM FOR FIRST CHAR OF LINE 15 (55296+(15*40))

; * THIS IS THE ACTUAL BEGINNING OF OUR ASSEMBLY PROGRAM.
*=$C000

START   SEI               ; PREVENT INTERRUPTS WHILE WE SET THINGS UP
        JSR  $FF81        ; RESET VIC, CLEAR SCREEN

        LDA  #%00110101   ; DISABLE KERNAL AND BASIC ROMS
        STA  $01          ; WE GO BARE METAL.

        LDA  #%01111111   ; DISABLE CIA-1/2 INTERRUPTS
        STA  $DC0D
        STA  $DD0D
        LDA  #%00000001   ; ENABLE RASTER INTERRUPT
        STA  $D01A
        LSR  $DC0D        ; ACKNOWLEDGE CIA
        LSR  $D019        ; AND VIDEO INTERRUPTS

        LDA  #RASSTART    ; SET RASTER INTERRUPT LINE
        STA  $D012
        LDA  #%01111111   ; CLEAR RST8 BIT, THE INTERRUPT LINE IS
        AND  $D011        ; ABOVE RASTER LINE 255
        STA  $D011

        LDA  #<ISR        ; SETUP THE INTERRUPT VECTOR TO OUR FUNCTION
        STA  $FFFE        ; NOTE THAT THIS VECTOR IS FOR ALL INTERRUPTS,
        LDA  #>ISR        ; NOT ONLY THE VIC RASTER INTERRUPT.
        STA  $FFFF        ; ANYHOW ALL OTHERS ARE DISABLED.

        LDA  #0           ; ENSURE WE HAVE A ZERO IN A PAGE ZERO LOCATION
        STA  ZEROVAL      ; SEE OTHER §1 COMMENTS FOR USAGE
        STA  TIMERA       ; INITIALIZE TIME BASE ACCUMULATORS

        LDA  #$80         ; SPRITE 0 POINTER TO $2000
        STA  $07F8
        LDA  #%0000001    ; ENABLE SPRITE 0
        STA  $D015
        STA  $D017        ; DOUBLE HEIGHT FOR SPRITE 0
        STA  $D01D        ; DOUBLE WIDTH FO SPRITE 0

        LDA  #160         ; POSITION THE SPRITE IN THE MIDDLE OF THE BAR
        STA  $D000        ; BREAK AND JUST TWO LINES AFTER THE START (SO
        LDA  #RASSTART+2  ; THE JITTER OFFSET LINES ARE NOT AFFECTED BY
        STA  $D001        ; SHORTER RASTER LINES). ALSO THE BAR CANNOT
                          ; BE TALLER THAN THE SPRITE AS THE LINE TIMING
                          ; WILL CHANGES AS SOON AS OUT OF THE SPRITE AREA

        LDY  #0           ; PREPARE COLOR RAM FOR THE SCROLLER TEXT WITH
SETTCOL TYA               ; COLORS FROM THE TSCOLST TABLE
        AND  #%00000111   ; COLORS DON'T CHAMGE DURING THE SCROLL BUT SINCE
        TAX               ; THE TEXT MOVES IT WILL GIVE THE IMPRESSION OF
        LDA  SCRCOL,X     ; COLORS CHANGES.
        STA  TSCOLST,Y
        INY
        TYA
        CMP  #40          ; STOP AFTER 40 CHARS AS WE REACHED THE BORDER.
        BNE  SETTCOL

        LDA  $D018        ; MOVE CHARACTER GENERATOR MEMORY TO $3000
        AND  #%11110000   ; SO WE CAN DISPLAY USING OUR CUSTOM FONT.
        ORA  #%00001100
        STA  $D018

        CLI               ; LET INTERRUPTS COME

                          ; THIS IS OUR MAIN LOOP. FOR NOW WE DON'T DO ANYTHING
                          ; USEFUL, JUST A MIX OF DIFFERENT LENGTH INSTRUCTIONS
                          ; SO WE MAXIMISE THE RASTER INTERRUPT JITTER TO SIMULATE
                          ; REAL CODE.

LOOP    LDA  #1           ; 2 CYCLES
        LDA  $01          ; 3 CYCLES
        LDA  $0300        ; 4 CYCLES
        LDA  ($04),Y      ; 5 CYCLES
        LDA  ($04,X)      ; 6 CYCLES
        LSR  $0300,X      ; 7 CYCLES
        JMP  LOOP

                          ; THIS IS THE FIRST RASTER INTERRUPT SERVICE ROUTINE.
                          ; BY THE TIME WE COME HERE WE CAN BE ANYWHERE ON THE
                          ; DESIRED LINE WITH A JITTER OF 7 CYCLES DEPENDING ON
                          ; THE INSTRUCTION EXECUTING WHEN THE INTERRUPT APPENED.

ISR     PHA               ; PRESERVE A,X,Y ON THE STACK
        TXA
        PHA
        TYA
        PHA

        TSX               ; WE ARE ABOUT TO LET ANOTHER INTERRUPT HAPPEN
                          ; WITHOUT CALLING RTI, SO THERE WILL BE ONE EXTRA
                          ; RETURN ADDRESS ON THE STACK. SAVE THE GOOD STACK
                          ; POINTER INTO X, WE WILL RESTORE IT LATER BEFORE RTI

        INC  $D012        ; SETUP ANOTHER RASTER INTERRUPT FOR THE NEXT
        LDA  #<ISR2       ; SCAN LINE
        STA  $FFFE
        LDA  #>ISR2
        STA  $FFFF

        LSR  $D019        ; ACKNOWELEDGE VIDEO INTERRUPTS

        CLI

        NOP               ; WASTE TIME WAITING THE INTERRUPT TO HAPPEN
        NOP               ; THESE ARE NOPS AS THEY TAKE 2 CYCLES THE JITTER
        NOP               ; OF THE NEXT INTERRUPT WILL BE ONLY 1 CYCLE.
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
        NOP               ; THESE ARE ENOUGH TO COMPLETE A SCAN LINE.
                          ; WE WILL NEVER COME HERE AS THE INTERRUPT WILL HAPPEN
                          ; BEFORE AND BY RESETTING THE STACK POINTER RTI WILL NOT
                          ; RETURN HERE EITHER.

                          ; THIS IS THE SECOND RASTER INTERRUPT ROUTINE. BY THE TIME
                          ; WE COME HERE WE HAVE A JITTER OF JUST ONE CYCLE AS WE ENSURED
                          ; THIS INTERRUPT HAPPENS WHILE EXECUTING NOPS (2 CYCLES).
                          ; NEXT WE ENSURE WE SPEND THE EXACT AMOUNT OF CYCLES IT TAKES
                          ; TO DRAW A FULL SCAN LINE. THE LAST BEQ *+2 WE USE DOES THE
                          ; SYNC MAGIC. SEE COMMENTS BELOW. THIS IS TIMED FOR PAL SYSTEMS
                          ; THE DELAY LOOP NEEDS TO BE CHANGED FOR NTSC.

                          ; INTERRUPT SERVICING (DURING A NOP)          2/3 CYCLES
ISR2    TXS               ; RESTORE THE SP MESSED BY THE INTERRUPT.       2 CYCLES

        LDY  #8           ; THIS LOOP, THE INTERRPUT CALL THE ABOVE TXS AND
        DEY               ; THE BELOW BIT AND LDA TAKE EXACTLY ONE SCAN LINE
        BNE  *-1          ; MINUS 1 CYCLE.
                          ;                                              46 CYCLES
        BIT  $00          ;                                               3 CYCLES
        NOP
        NOP

        LDA  $D012        ; GET CURRENT SCAN LINE                         4 CYCLES
        CMP  $D012        ; HERE WE ARE EITHER STILL ON THE SAME LINE     4 CYCLES
                          ; (WITH ONE CYCLE TO GO) OR AT THE NEXT LINE.
        BEQ  *+2          ; IF WE ARE ON SAME LINE BRANCH (3 CYCLES)    3/2 CYCLES
                          ; ELSE MOVE ON (2 CYCLES). NOTE THAT IN BOTH
                          ; CASES WE END UP AT THE NEXT INSTRUCTION, BUT IT
                          ; WILL TAKE DIFFERENT TIME TO GET THERE SO WE
                          ; OFFSET THE REMAINING 1 CYCLE JITTER.

                          ; FROM HERE ON WE ARE STABLE.

        LDY  #10          ; PUSH FORWARD SO THE STA $D020/1
        DEY               ; ARE IN THE HORIZONTAL SYNC AREA
        BNE  *-1          ;
        BIT  $00

        LDY  #$FF         ; Y WILL BE USED TO INDEX TABLE BARCOL, WE START FROM FF
                          ; SO WHEN WE INY BELOW WE ROLL TO 00

BLOOP   INY               ; NEXT COLOUR ENTRY

        LDA  $D012        ; WE NEED TO AVOID BAD LINES, WE SET YSCROLL TO
        CLC               ; (CURRENT RASTER LINE + 7)%8 SO THAT THE BAD LINE
        ADC  #7           ; IS ALWAYS THE PREVIOUS LINE.
        AND  #%00000111
        ORA  #%00011000
        STA  $D011

        LDA  ZEROVAL      ; WE NEED ZERO IN A IN 3 CYLCLES SO WE LOAD FROM PAGE
                          ; ZERO FROM A LOCATION WE SET TO ZERO. SEE §1 COMMENTS.
        STA  $D021        ; BLACK VERTICAL BAR STARTS HERE...
        NOP               ;
        LDA  BARCOL,Y     ;
        STA  $D021        ; ...AND ENDS HERE
        BIT  $00          ; WASTE MORE CYCLES TO FILL UP THE RASTER LINE
        NOP

        LDA  BARCOL,Y     ; GET THE CURRENT BAR COLOUR
        STA  $D020        ; AND SET IT FOR BORDER
        STA  $D021        ; AND BACKGROUND COLOUR

        AND  #%10000000   ; TEST BIT-7 OF THE COLOUR IS SET
        BNE  BLEND        ; IF SET WE ARE DONE WITH THE BAR

        JMP  BLOOP        ; AND REPEAT FOR NEXT COLOUR.

                          ; WE ARE DONE WITH THE INTERRUPT, WE NEED TO SET UP
                          ; THE NEXT ONE AND RESTORE REGISTERS BEFORE LEAVING.

BLEND   LDA  #RASSTART    ; SET RASTER INTERRUPT TO THE START OF THE BAR
        STA  $D012

        CLC               ; ENSURE THE NEXT INTERRUPT WILL NOT HAPPEN ON A
        ADC  #7           ; BAD LINE, SET YSCROLL SO THE BADLINE IS ONE BEFORE
        AND  #%00000111   ; THE NEXT INTERRUPT (RASTART IS IN A AT THE START
        ORA  #%00011000   ; OF THIS BLOCK)
        STA  $D011

        LDA  #<ISR        ; SET THE INTERRUPT VECTOR BACK TO THE FIRST ISR
        STA  $FFFE        ; (THE RASTER SYNC ONE)
        LDA  #>ISR
        STA  $FFFF

        LSR  $D019        ; ACKNOWELEDGE VIDEO INTERRUPTS

                          ; THIS IS A GOOD TIME TO TAKE CARE OF THE TIME BASE.
                          ; WE INCREMENT TIMERA ONCE PER FRAME. THIS SERVES AS TIME BASE FOR
                          ; ANIMATIONS, SCROLLS AND MUSIC.

        INC  TIMERA       ; INCREMENT TIME BASE

                          ; HERE WE MOVE THE SPRITE IN THE BAR. THIS IS A GOOD PLACE CAUSE THE
                          ; SPRITE HAS JUST BEEN DRAWN SO WE DON'T RISK TO TEAR IT HORIZONTALLY

        LDA  TIMERA
        ROR
        ROR
        AND  #%00011111   ; WE USE (TIMERA/4) % 32 AS AN INDEX INTO SPOFF TABLE
        TAX               ; WHICH CONTAINS A SINE GOING FROM 0 TO 32 CENTERED
        LDA  SPOFF,X      ; ON 16
        CLC
        ADC  #144
        STA  $D000        ; AND WE USE IT AS SPRITE X+144

                          ; WE NOW PREPARE THE SCROLLER TEXT

        LDA  $D016        ; WE DECREASE HERE THE XSCROLL SO AT EVERY FRAME WE
        CLC               ; SMOOTHLY SCROLL ONE PIXEL TO THE LEFT.
        ADC  #7           ; AS IT STANDS NOW THIS WILL AFFECT ALL THE SCREEN
        AND  #%00000111   ; BELOW THE RASTERBAR.
        ORA  #%11001000
        STA  $D016

        AND  #%00000111   ; UNLESS WE SCROLLED ALL THE WAY THROUGH THE 8 PIXELS
        CMP  #%00000111   ; SKIP THE NEXT BLOCK WHICH SHIFTS THE TEXT ONE WHOLE
        BNE  SKIPTL       ; CHAR LEFT

        LDX  TSCROLL      ; TSCROLL HOLDS THE CURRENT SCROLL POSITION IN CHARS
        INC  TSCROLL      ; SCROLL LEFT BY ONE CHAR
        LDY  #0           ; COPY THE STRING STARTING FROM THE TSCROLL OFFSET
TLOOP   LDA  STEXT,X      ; INTO SCREEN MEMORY (TROLLSK)
        CMP  #0           ; UNLESS IT'S A ZERO (@)
        BNE  TROLLSK      ;
        LDA  #0           ; IN WHICH CASE WE RESTART FROM THE BEGINNING OF THE
        STA  TSCROLL      ; STRING
        JMP  SKIPTL
TROLLSK STA  TSCSTART,Y   ; STORE INTO SCREEN MEMORY STARTING FROM TSCSTART
        INX
        INY
        TYA
        CMP  #40          ; STOP AFTER COPYING 40 CHARS AS WE REACH THE BORDER
        BNE  TLOOP

SKIPTL
        PLA               ; RESTORE Y,X,A FROM THE STACK
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
FONT0   BYTE $3C,$66,$6E,$6E,$60,$6C,$36,$00
        BYTE $00,$00,$3C,$67,$66,$66,$3B,$00
        BYTE $58,$60,$6C,$76,$66,$66,$DC,$00
        BYTE $00,$1C,$36,$60,$60,$62,$3C,$00
        BYTE $18,$1C,$0E,$36,$66,$66,$3C,$00
        BYTE $00,$3C,$66,$6C,$78,$62,$3C,$00
        BYTE $0C,$10,$18,$3C,$18,$18,$18,$10
        BYTE $00,$3C,$67,$66,$66,$3C,$46,$7C
        BYTE $58,$60,$6C,$76,$66,$66,$6C,$0C
        BYTE $00,$18,$00,$38,$18,$1A,$1C,$00
        BYTE $00,$18,$00,$38,$18,$18,$18,$34
        BYTE $5C,$30,$7C,$30,$30,$74,$18,$00
        BYTE $2C,$18,$18,$18,$18,$38,$0C,$00
        BYTE $00,$00,$DA,$7F,$6B,$6B,$6B,$00
        BYTE $00,$00,$CE,$77,$66,$66,$77,$00
        BYTE $00,$00,$7C,$66,$66,$E6,$38,$00
        BYTE $00,$C0,$5C,$66,$E6,$7C,$60,$60
        BYTE $00,$00,$3C,$67,$66,$3E,$06,$06
        BYTE $00,$00,$6C,$34,$30,$70,$18,$00
        BYTE $0C,$10,$38,$18,$18,$18,$18,$10
        BYTE $08,$18,$3C,$18,$18,$38,$0C,$00
        BYTE $00,$44,$EE,$66,$66,$E6,$3B,$00
        BYTE $80,$CC,$76,$66,$66,$EC,$38,$00
        BYTE $00,$8A,$DF,$6B,$6B,$6B,$76,$00
        BYTE $00,$36,$1A,$18,$38,$6C,$60,$3C
        BYTE $80,$CC,$76,$66,$66,$6C,$0C,$0C
        BYTE $00,$3C,$46,$1C,$0F,$36,$36,$1C
        BYTE $3C,$30,$30,$30,$30,$30,$3C,$00
        BYTE $0C,$12,$30,$7C,$30,$62,$FC,$00
        BYTE $3C,$0C,$0C,$0C,$0C,$0C,$3C,$00
        BYTE $00,$18,$3C,$7E,$18,$18,$18,$18
        BYTE $00,$10,$30,$7F,$7F,$30,$10,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00
        BYTE $18,$18,$18,$18,$00,$00,$18,$00
        BYTE $36,$36,$6C,$00,$00,$00,$00,$00
        BYTE $66,$66,$FF,$66,$FF,$66,$66,$00
        BYTE $06,$3E,$60,$3E,$03,$76,$DC,$00
        BYTE $62,$66,$0C,$18,$30,$66,$46,$00
        BYTE $3C,$66,$3C,$38,$67,$66,$3F,$00
        BYTE $0C,$0C,$18,$00,$00,$00,$00,$00
        BYTE $0C,$18,$30,$30,$30,$18,$0C,$00
        BYTE $30,$18,$0C,$0C,$0C,$18,$30,$00
        BYTE $00,$66,$3C,$FF,$3C,$66,$00,$00
        BYTE $00,$18,$18,$7E,$18,$18,$00,$00
        BYTE $00,$00,$00,$00,$00,$18,$18,$30
        BYTE $00,$00,$00,$7E,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$18,$18,$00
        BYTE $00,$03,$06,$0C,$18,$30,$60,$00
        BYTE $18,$36,$66,$66,$66,$6C,$18,$00
        BYTE $18,$18,$38,$18,$18,$18,$3C,$00
        BYTE $3C,$66,$66,$3C,$18,$32,$7E,$00
        BYTE $7E,$66,$0C,$18,$46,$66,$3C,$00
        BYTE $0C,$1A,$36,$66,$7F,$06,$0F,$00
        BYTE $76,$5C,$40,$7C,$06,$66,$3C,$00
        BYTE $1C,$36,$60,$7C,$66,$6C,$38,$00
        BYTE $76,$5E,$06,$0C,$18,$30,$60,$00
        BYTE $1C,$36,$66,$3C,$66,$6C,$38,$00
        BYTE $1C,$36,$66,$3E,$06,$6C,$38,$00
        BYTE $00,$00,$18,$00,$00,$18,$00,$00
        BYTE $00,$00,$18,$00,$00,$18,$18,$30
        BYTE $0E,$18,$30,$60,$30,$18,$0E,$00

; SPITE DATA
*=$2000
SPRITE0 BYTE $00,$7F,$00,$01,$FF,$C0,$03,$FF
        BYTE $E0,$03,$E7,$E0,$07,$D9,$F0,$07
        BYTE $DF,$F0,$07,$D9,$F0,$03,$E7,$E0
        BYTE $03,$FF,$E0,$03,$FF,$E0,$02,$FF
        BYTE $A0,$01,$7F,$40,$01,$3E,$40,$00
        BYTE $9C,$80,$00,$9C,$80,$00,$49,$00
        BYTE $00,$49,$00,$00,$3E,$00,$00,$3E
        BYTE $00,$00,$3E,$00,$00,$1C,$00,$00




