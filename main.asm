
; 10 SYS (49152)

*=$801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $34, $39, $31, $35, $32, $29, $00, $00, $00

*=$C000

start   SEI

        JSR $FF81       ; Reset Vic, clear screen
        LDA #$01        ; Enable raster interrupt
        STA $D01A

        LDA #30
        JSR setrstint
        
        ; Setup the interrupt vector
        LDA #<irq  
        STA $0314  
        LDA #>irq
        STA $0315

        CLI
        RTS     
        JMP *

irq     lda $d019                     ; clear source of interrupts
        and #01
        cmp #01
        bne endint
        sta $d019

        LDA $D012
        CMP #60
        BEQ barst
        LDA #30
        JSR setrstint
        LDA #4
        STA $D020
        STA $D021
        JMP endint

barst   LDA #50
        JSR setrstint
        LDA #2
        STA $D020
        STA $D021

endint  jmp $ea31

; **********************************************************************
; * Setup next raster interrupt line.
; * A: line LSB/2
; **********************************************************************
setrstint       ASL
                STA $D012       ; LSB of line to raster counter register
                BCC clearrst8   ; Was bit 7 set? No, clear RST8 bit
                LDA #%10000000  ; Yes, set RST8 bit
                ORA $D011
                STA $D011
                RTS

clearrst8       LDA #%01111111  ; Yes, clear RST8 bit
                AND $D011
                STA $D011   
                RTS
