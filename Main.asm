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
RASSTART = 59            ; RASTER LINE WHERE THE RASTER INTERRUPT STARTS
ZEROVAL = 20             ; ZERO PAGE LOCATION SET ALWAYS TO ZERO SEE §1 COMMENTS
TIMERA  = 21             ; TIME BASE, ROUGHLY 1 PER FRAME (50HZ)
TSCROLL = 24             ; CURRENT TEXT PIXEL SCROLL 0-8
TSCSTART = 1024          ; FIRST CHAR OF LINE 15 (1024+(15*40))
TSCOLST = 55296          ; COLOR RAM FOR FIRST CHAR OF LINE 15 (55296+(15*40))

; * THIS IS THE ACTUAL BEGINNING OF OUR ASSEMBLY PROGRAM.
*=$C000

START   SEI             ; PREVENT INTERRUPTS WHILE WE SET THINGS UP
        JSR  $FF81      ; RESET VIC, CLEAR SCREEN

        LDA  #%00110101 ; DISABLE KERNAL AND BASIC ROMS
        STA  $01        ; WE GO BARE METAL.

        LDA  #%01111111 ; DISABLE CIA-1/2 INTERRUPTS
        STA  $DC0D
        STA  $DD0D
        LDA  #%00000001 ; ENABLE RASTER INTERRUPT
        STA  $D01A
        LDA  $DC0D      ; ACKNOWLEDGE CIA
        LSR  $D019      ; AND VIDEO INTERRUPTS

        LDA  #RASSTART  ; SET RASTER INTERRUPT LINE
        STA  $D012
        LDA  #%01111111 ; CLEAR RST8 BIT, THE INTERRUPT LINE IS
        AND  $D011      ; ABOVE RASTER LINE 255
        STA  $D011

        LDA  #<ISR      ; SETUP THE INTERRUPT VECTOR TO OUR FUNCTION
        STA  $FFFE      ; NOTE THAT THIS VECTOR IS FOR ALL INTERRUPTS,
        LDA  #>ISR      ; NOT ONLY THE VIC RASTER INTERRUPT.
        STA  $FFFF      ; ANYHOW ALL OTHERS ARE DISABLED.

        LDA  #0         ; ENSURE WE HAVE A ZERO IN A PAGE ZERO LOCATION
        STA  ZEROVAL    ; SEE OTHER §1 COMMENTS FOR USAGE
        STA  TIMERA     ; INITIALIZE TIME BASE ACCUMULATORS

        LDA  #$80       ; SPRITE 0 POINTER TO $2000
        STA  $07F8
        LDA  #%0000001  ; ENABLE SPRITE 0
        STA  $D015
        STA  $D017      ; DOUBLE HEIGHT FOR SPRITE 0
        STA  $D01D      ; DOUBLE WIDTH FO SPRITE 0

        LDA  #160       ; POSITION THE SPRITE IN THE MIDDLE OF THE BAR
        STA  $D000      ; BREAK AND JUST TWO LINES AFTER THE START (SO
        LDA  #RASSTART+2; THE JITTER OFFSET LINES ARE NOT AFFECTED BY
        STA  $D001      ; SHORTER RASTER LINES). ALSO THE BAR CANNOT
                        ; BE TALLER THAN THE SPRITE AS THE LINE TIMING
                        ; WILL CHANGES AS SOON AS OUT OF THE SPRITE AREA

        LDY  #0         ; PREPARE COLOR RAM FOR THE SCROLLER TEXT WITH
SETTCOL TYA             ; COLORS FROM THE TSCOLST TABLE
        AND  #%00000111 ; COLORS DON'T CHANGE DURING THE SCROLL BUT SINCE
        TAX             ; THE TEXT MOVES IT WILL GIVE THE IMPRESSION OF
        LDA  SCRCOL,X   ; COLORS CHANGES.
        STA  TSCOLST,Y
        INY
        TYA
        CMP  #40        ; STOP AFTER 40 CHARS AS WE REACHED THE BORDER.
        BNE  SETTCOL

        LDA  $D018      ; MOVE CHARACTER GENERATOR MEMORY TO $3000
        AND  #%11110000 ; SO WE CAN DISPLAY USING OUR CUSTOM FONT.
        ORA  #%00001100
        STA  $D018

        JSR  MUINIT

        CLI             ; LET INTERRUPTS COME

        ; THIS IS OUR MAIN LOOP. FOR NOW WE DON'T DO ANYTHING
        ; USEFUL, JUST A MIX OF DIFFERENT LENGTH INSTRUCTIONS
        ; SO WE MAXIMISE THE RASTER INTERRUPT JITTER TO SIMULATE
        ; REAL CODE.

LOOP    LDA  #1         ; 2 CYCLES
        LDA  $01        ; 3 CYCLES
        LDA  $0300      ; 4 CYCLES
        LDA  ($04),Y    ; 5 CYCLES
        LDA  ($04,X)    ; 6 CYCLES
        LSR  $0300,X    ; 7 CYCLES
        JMP  LOOP


