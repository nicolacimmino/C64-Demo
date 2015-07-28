
; 10 SYS (49152)

*=$801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $34, $39, $31, $35, $32, $29, $00, $00, $00

*=$C000

start   SEI

        ;lda #$7f                      ; turn off the cia interrupts
        ;sta $dc0d
        JSR $FF81

        ; Enable raster interrupt
        LDA #$01
        STA $D01A

        ; Setup raster line, high bit in D011
        LDA #80  
        STA $D012
        LDA #$1B   
        STA $D011
          
        ; Setup the interrupt vector
        LDA #<irq  
        STA $0314  
        LDA #>irq
        STA $0315

        CLI
        JMP *

irq     lda $d019                     ; clear source of interrupts
        sta $d019

        LDA $D012
        CMP #80
        BEQ barst
        LDA #80
        STA $D012
        LDA #3
        STA $D020
        STA $D021
        JMP endint

barst   LDA #110
        STA $D012
        LDA #1
        STA $D020
        STA $D021

endint  jmp $ea31
     