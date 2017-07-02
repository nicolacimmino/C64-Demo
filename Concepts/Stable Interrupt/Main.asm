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
; * BELOW ARE TBE BASIC TOKENS FOR 10 SYS (49152)                             *
; * WE STORE THEM AT THE BEGINNING OF THE BASIC RAM SO WHEN WE CAN LOAD       *
; * THE PROGRAM WITH AUTORUN (LOAD "*",8,1) AND SAVE TO TYPE THE SYS.         *

*=$801

        BYTE $0E, $08, $0A, $00, $9E, $20, $28, $34, $39, $31, $35, $32
        BYTE $29, $00, $00, $00
; *                                                                           *
; *****************************************************************************

; *****************************************************************************
; * THIS IS THE ENTRY POINT INTO OUR PROGRAM. WE DO SOME SETUP AND THEN LET   *
; * THINGS ROLL FROM HERE.                                                    *

*=$C000

START   SEI             ; PREVENT INTERRUPTS WHILE WE SET THINGS UP.
        JSR  $FF81      ; RESET VIC, CLEAR SCREEN, THIS IS A KERNEL FUNCTION.

        LDA  #%00110101 ; DISABLE KERNAL AND BASIC ROMS WE GO BARE METAL.
        STA  $01        ; 

        LDA  #%01111111 ; DISABLE CIA-1/2 INTERRUPTS
        STA  $DC0D      ;
        STA  $DD0D      ;
      
        JSR  ISRSET     ; SETUP ISR AS RASTER INTERRUPT SERVICE ROUTINE

        LDA  #%00000001 ; ENABLE RASTER INTERRUPT
        STA  $D01A      ;

        LDA  $DC0D      ; ACKNOWLEDGE CIA INTERRUPTS
        
        CLI             ; LET INTERRUPTS COME

        ; THIS IS OUR MAIN LOOP. NOTHING  USEFUL, JUST A MIX OF DIFFERENT LENGTH 
        ; INSTRUCTIONS SO  WE MAXIMISE THE RASTER  INTERRUPT JITTER  TO SIMULATE
        ; REAL CODE.
        
LOOP    LDA  #1         ; 2 CYCLES
        LDA  $01        ; 3 CYCLES
        LDA  $0300      ; 4 CYCLES
        LDA  ($04),Y    ; 5 CYCLES
        LDA  ($04,X)    ; 6 CYCLES
        LSR  $0300,X    ; 7 CYCLES
        JMP  LOOP
; *                                                                           *
; *****************************************************************************

; *****************************************************************************
; * THIS IS THE FIRST RASTER INTERRUPT  SERVICE ROUTINE.  BY THE TIME WE COME * 
; * HERE WE  CAN BE  ANYWHERE ON  THE  DESIRED LINE WITH A JITTER OF 7 CYCLES * 
; * DEPENDING  ON THE  INSTRUCTION EXECUTING  WHEN THE INTERRUPT HAPPENED.    *

ISR     PHA             ; PRESERVE A,X,Y ON THE STACK
        TXA             ;
        PHA             ;
        TYA             ;
        PHA             ;
                
        TSX             ; WE  ARE ABOUT TO LET  ANOTHER INTERRUPT HAPPEN WITHOUT
                        ; CALLING RTI, SO THERE WILL BE ONE EXTRA RETURN ADDRESS 
                        ; ON  THE STACK. SAVE THE  GOOD STACK POINTER INTO X, WE 
                        ; WILL RESTORE IT LATER BEFORE RTI.
        
        INC  $D012      ; SETUP ANOTHER RASTER INTERRUPT FOR THE NEXT
        LDA  #<ISR2     ; SCAN LINE TO BE SERVED BY ISR2.
        STA  $FFFE      ;
        LDA  #>ISR2     ;
        STA  $FFFF      ;

        LSR  $D019      ; ACKNOWELEDGE VIDEO INTERRUPT AND ALLOW THE NEXT RASTER        
        CLI             ; INTERRUPT TO OCCOUR.

        LDY #5          ; WASTE TIME WAITING  THE INTERRUPT TO HAPPEN  IN ONE OF 
        DEY             ; THE NOPS BELOW, WHICH  TAKE 2 CYCLES, SO  THE NEXT ISR      
        BNE *-1         ; WILL BE CALLED WITH A JITTER OF JUST 1 CYCLE.

        NOP             
        NOP             
        NOP             
        NOP             
; *                                                                           *
; *****************************************************************************

; *****************************************************************************
; * THIS IS  THE  SECOND  RASTER INTERRUPT ROUTINE. BY THE  TIME WE COME HERE *
; * WE HAVE A JITTER OF JUST ONE CYCLE. NEXT  WE ENSURE  WE SPEND THE EXACT   *  
; * AMOUNT OF CYCLES IT TAKES TO DRAW A FULL SCAN LINE. THE LAST BEQ *+2 WE   *
; * USE DOES THE SYNC MAGIC. SEE NOTES BELOW. THIS  IS  TIMED FOR PAL SYSTEMS *
; * THE DELAY LOOP NEEDS TO BE CHANGED FOR NTSC.

                        ; INTERRUPT SERVICING (DURING A NOP)          7/8 CYCLES
ISR2    TXS             ; RESTORE THE SP MESSED BY THE INTERRUPT.       2 CYCLES

        LDX  #8         ; THE INTERRPUT SERVICING CALL AND THE CODE
        DEX             ; IN THIS BLOCK ARE TIMED TO LAST EXACTLY ONE
        BNE  *-1        ; LINE (63 CYCLES FOR PAL) SO THAT CMP $D012
                        ; HAPPENS EITHER ONE CYCLE BEFORE THE NEXT 
                        ; RASTER LINE OR JUST AT THE BEGINNING OF IT.
                        ;                                              46 CYCLES
        BIT $00         ;                                               3 CYLCES                                                     

        LDA  $D012      ; GET CURRENT SCAN LINE                         4 CYCLES
        CMP  $D012      ; HERE WE ARE EITHER STILL ON THE SAME LINE     4 CYCLES
                        ; (WITH ONE CYCLE TO GO) OR AT THE NEXT LINE.
        BEQ  *+2        ; IF WE ARE ON SAME LINE BRANCH (3 CYCLES)    3/2 CYCLES
                        ; ELSE MOVE ON (2 CYCLES). NOTE THAT IN BOTH
                        ; CASES WE END UP AT THE NEXT INSTRUCTION, BUT IT
                        ; WILL TAKE DIFFERENT TIME TO GET THERE SO WE
                        ; OFFSET THE REMAINING 1 CYCLE JITTER.

                        ; FROM HERE ON WE ARE STABLE.
                
        LDA  #1         ; CHANGE BORDER AND BACKGROUND  TO  WHITE FOR FEW CYCLES 
        STA  $D020      ; SO WE SHOW WHERE OUR CODE IS RUN ON THE SCREEN.
        STA  $D021      ; 

        NOP             ; JUST WAIT A WHILE TO MAKE THE BAR LONGER.
        NOP             ;
        NOP             ;
        NOP             ;
    
        LDA  #6         ; RESTORE BORDER AND BACKGROUND COLORS.
        STA  $D021      ;
        LDA  #14        ;
        STA  $D020      ;
      
        JSR ISRSET      ; SET RASTER INTERRUPT FOR THE NEXT SCREEN.

        PLA             ; RESTORE Y,X,A FROM THE STACK
        TAY             ;
        PLA             ;
        TAX             ;
        PLA             ;

        RTI
; *                                                                           *
; *****************************************************************************

; *****************************************************************************
; * SETUP A RASTER INTERRUPT TO BE SERVICED ON LINE 91 BY ISR                 *

ISRSET  LDA  #91        ; SET RASTER INTERRUPT FOR LINE 91
        STA  $D012
        LDA  #%01111111 ; CLEAR RST8 BIT, THE INTERRUPT LINE IS
        AND  $D011      ; ABOVE RASTER LINE 255
        STA  $D011
       
        LDA  #<ISR      ; SET THE INTERRUPT VECTOR BACK TO THE FIRST ISR
        STA  $FFFE      ; (THE RASTER SYNC ONE)
        LDA  #>ISR
        STA  $FFFF

        LSR  $D019      ; ACKNOWELEDGE VIDEO INTERRUPTS

        RTS
; *                                                                           *
; *****************************************************************************
