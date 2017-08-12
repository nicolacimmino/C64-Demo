; *****************************************************************************
; *                                                                           *
; * COPYRIGHT (C) 2017 NICOLA CIMMINO                                         *
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

; *****************************************************************************
; * BELOW ARE TBE BASIC TOKENS FOR 10 SYS49152                                *
; * WE STORE THEM AT THE BEGINNING OF THE BASIC RAM SO WHEN WE CAN LOAD       *
; * THE PROGRAM WITH AUTORUN (LOAD "*",8,1) AND SAVE TO TYPE THE SYS.         *

*=$801

        BYTE $0E, $08, $0A, $00, $9E, $34, $39, $31, $35, $32, $00, $00, $00
; *                                                                           *
; *****************************************************************************

INSTRP=$02 ; 2 bytes
STEPR=$04  ; still needed?
MUWAIT=$06 ; 2 bytes?? amount of ticks for the current wait
TICK=$08   ; 2 bytes current TICK
PHRP=$0A   ; 2 bytes pointer into the current PHRASE (abosulte address)

; *****************************************************************************
; * THIS IS THE ENTRY POINT INTO OUR PROGRAM. WE DO SOME SETUP AND THEN LET   *
; * THINGS ROLL FROM HERE.                                                    *

*=$C000

START   SEI             ; PREVENT INTERRUPTS WHILE WE SET THINGS UP.
        JSR  $FF81      ; RESET VIC, CLEAR SCREEN, THIS IS A KERNAL FUNCTION.

        LDA  #%00110101 ; DISABLE KERNAL AND BASIC ROMS WE GO BARE METAL.
        STA  $01        ; 

        LDA  #%01111111 ; DISABLE CIA-1/2 INTERRUPTS.
        STA  $DC0D      ;
        STA  $DD0D      ;
      
        LDA  #1         ; SET RASTER INTERRUPT FOR LINE 1. POSITION IS ACTUALLY
        STA  $D012      ; IRRELEVANT WE NEED A TIME BASE ONE CALL PER FRAME.
        LDA  #%01111111 ; CLEAR RST8 BIT, THE INTERRUPT LINE IS
        AND  $D011      ; ABOVE RASTER LINE 255.
        STA  $D011
       
        LDA  #<ISR      ; SET THE INTERRUPT VECTOR TO THE ISR ROUTINE.
        STA  $FFFE      ;
        LDA  #>ISR
        STA  $FFFF

        LSR  $D019      ; ACKNOWELEDGE VIDEO INTERRUPTS.
        LDA  #%00000001 ; ENABLE RASTER INTERRUPT.
        STA  $D01A      ;
        
        JSR MUINIT

        LDA  $DC0D      ; ACKNOWLEDGE CIA INTERRUPTS.
        
        CLI             ; LET INTERRUPTS COME.

        ; THIS IS OUR MAIN LOOP. NOTHING  USEFUL THE PAYER RUNS ONLY ONCE PER
        ; FRAME WHEN THE INTERRUPT HAPPENS.
        
        JMP  *

; *                                                                           *
; *****************************************************************************

MUINIT  LDA #0          ; Start from step 0
        STA STEPR
        STA STEPR+1
        
        LDA #<PHRASE
        STA PHRP        ; Phrase pointer, will come from loop
        LDA #>PHRASE
        STA PHRP+1

;        LDA  #<INSTR    ; Instrument 0 (this will really come from the track)
;        STA  INSTRP
;        LDA  #>INSTR
;        STA  INSTRP+1
;        
        LDX  #24        ; CLEAR ALL SID REGISTERS
        LDA  #0
        STA  $D400,X
        DEX
        BNE  *-4

        LDA #%00001111  ; Volume max
        STA $D418
        
        RTS
 
; *****************************************************************************
; * THIS IS THE RASTER INTERRUPT  SERVICE ROUTINE. IN A FULL APP THIS WOULD DO* 
; * SEVERAL THINGS, WE HERE ONLY PROCESS THE MUSIC STUFF.                     *

ISR     INC TICK        ; NEXT TICK (16 BIT INCREMENT)
        BNE *+4
        INC TICK+1

        LDY #0          ; PHRP IS POINTING TO THE CURRENT PHRASE ENTRY AND
        LDA TICK        ; THE FIRST TWO BYTES ARE THE TICK OF THE NEXT EVENT
        CMP (PHRP),Y    ; KEEP PLAYING THE CURRENT INSTRUMENT IF WE HAVE NOT
        BNE @PLAY       ; REACHED THE TICK YET
        INY 
        LDA TICK+1
        CMP (PHRP),Y
        BNE @PLAY

        LDY #2          ; Freq LO from track
        LDA (PHRP),Y
        STA $D400
        LDY #3          ; Freq HI from track
        LDA (PHRP),Y
        STA $D401
        
        LDA  #<INSTR    ; Instrument 0 (this will really come from the phrase)
        STA  INSTRP
        LDA  #>INSTR
        STA  INSTRP+1
       
        CLC
        LDA #6          ; On to next entry in the phrase
        ADC PHRP
        STA PHRP
        BNE @PLAY
        INC PHRP+1

@PLAY   LDY  #0         ; LOAD CURRRENT INSTRUMENT COMMAND
        LDA  (INSTRP),Y

        ROR             ; GET HIGH NIBBLE*2 IN X
        ROR             ;
        ROR             ;
        AND  #%00011110 ;
        TAX             ;

        LDA CMDTBL,X    ; TRANSFER MSB OF CMD ADDRESS
        STA @JSRINS+1

        LDA CMDTBL+1,X
        STA @JSRINS+2

        LDA  (INSTRP),Y ; PASS LOW NIBBLE OF COMMAND TO THE CALLED ROUTINE
        AND  #%00001111

@JSRINS JSR  *
        
        CLC             ; ROUTINE RETURNS IN A THE AMOUNT OF CMD MEMORY BYTES
        ADC INSTRP      ; CONSUMED. MOVE INSTRP
        STA INSTRP      ; TODO needs to be a 16bit addition

MUDONE  LSR  $D019      ; ACKNOWELEDGE VIDEO INTERRUPTS.

        RTI

; *                                                                           *
; *****************************************************************************

*=$F000

CMDTBL  WORD WIN
        WORD WAI
        WORD WVR
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD $0000
        WORD END

WIN     LDY  #1
        LDA  (INSTRP),Y        
        STA MUWAIT
        LDA #2
        RTS

WAI     LDA #0
        DEC MUWAIT
        BNE @DONE
        LDA #1
@DONE   RTS

WVR     TAX
        LDY  #1
        LDA  (INSTRP),Y
        STA  $D400,X

        LDA  #2         ; We consumed 2 bytes
        RTS


END     LDA  #0         ; We stay on the same instruction forever
        RTS
               
INSTR   BYTE $25, $11   ; WVR 5, $52            AD
        BYTE $26, $F1   ; SR
        BYTE $24, $11   ; WVR 4, %10000001      NOISE + GATE ON
        BYTE $00, $01
        BYTE $10
        BYTE $24, $00   ;

        BYTE $FF        ; END

        ; ticklo tickhi  FRELO FREHI INSTR, DUR
PHRASE  BYTE $01, $00, $D6, $1C, $00, $05
        BYTE $41, $00, $D6, $2C, $00, $05
        BYTE $81, $00, $D6, $1C, $00, $05
        BYTE $F1, $00, $D6, $2C, $00, $05

