
;#define LIBARGS ,

;
; iNES header
;

.segment "HEADER"
    .byte "NES"
    .byte $1A
    .byte $02                       ;ammount of 16k PRG-ROM banks
    .byte $01                       ;ammount of 8k CHR-ROM banks
    .byte $00                       ;mapper and mirroring
    .byte $00, $00, $00, $00       
    .byte $00, $00, $00, $00, $00 

.segment "ZEROPAGE"

pointerLo: .res 1
pointerHi: .res 1


;;;;; START OF CODE

.segment "CODE"
RESET:
	sei                            ;disable interrupts
    cld                            ;clear decimal mode

    ldx #%10000000                ;disable sound IRQ
    stx $4017
    ldx #$00
    stx $4010                    ;disable PCM

    ;initialize the stack register
    ldx #$FF
    txs                           ;transfer X to stack pointer


    ;clear PPU registers
    ldx #$00
    stx $2000
    stx $2001

   ;wait for vblank
:
    bit $2002
    bpl :-

    ;Clearing 2k memory
    txa
CLEARMEMORY:            ;$0000-$07FF 
    sta $0000, x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    lda #$ff
    sta $0200, x
    lda #$00

    inx
    cpx #$00
    bne CLEARMEMORY

    ;wait for vblank
:
    bit $2002
    bpl :-

    ;setting sprite range
    lda #$02
    sta $4014

    lda #$3f
    sta $2006
    lda #$00
    sta $2006

    ldx #$00
LOADPALETTES:
    lda PALETTEDATA, x
    sta $2007
    inx
    cpx #$20
    bne LOADPALETTES

    ldx #$00
LOADSPRITES:
    lda SPRITEDATA, x
    sta $0200, x
    inx
    cpx #$20
    bne LOADSPRITES

LOADBACKGROUND:
    lda $2002       ;read PPU status to reset high/low latch
    lda #$20
    sta $2006
    lda #$00
    sta $2006
    ldx #$00
LOADBACKGROUNDP1:  
    lda BACKGROUNDDATA, x
    sta $2007
    inx
    cpx #$00
    bne LOADBACKGROUNDP1
LOADBACKGROUNDP2:
    lda BACKGROUNDDATA+256,x
    sta $2007
    inx
    cpx #$00
    bne LOADBACKGROUNDP2
LOADBACKGROUNDP3:
    lda BACKGROUNDDATA+512,x
    sta $2007
    inx
    cpx #$00
    bne LOADBACKGROUNDP3
LOADBACKGROUNDP4:
    lda BACKGROUNDDATA+768,x
    sta $2007
    inx
    cpx #$00
    bne LOADBACKGROUNDP4

    ;load background palettesdata
    lda #$23     ;23c0  
    sta $2006
    lda #$c0
    sta $2006
    ldx #$00
LOADBACKGROUNDPALETTEDATA:
    lda BACKGROUNDPALETTEDATA, x
    sta $2007
    inx
    cpx #$40
    bne LOADBACKGROUNDPALETTEDATA
    

    ;Reset scroll
    lda #$00
    sta $2005
    sta $2005


;enable interrupts
    cli

    lda #%10010000
    sta $2000               ;when vblank occurs call NMI and use second sprite sheet for background

    lda #%00011110          ;show background and sprites
    sta $2001



INFLOOP:
	jmp INFLOOP	; endless loop

;;;;; INTERRUPT HANDLERS

NMIHandler:
	lda #$02            ;load sprite range
    	sta $4014
	rti
        
PALETTEDATA:
    	.byte $00, $0F, $00, $10, 	$00, $0A, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14 	;background palettes
	.byte $31, $0F, $15, $30, 	$00, $0F, $11, $30, 	$00, $0F, $30, $27, 	$00, $3C, $2C, $1C 	;sprite palettes

SPRITEDATA:
;Y, SPRITE NUM, attributes, X
;76543210
;||||||||
;||||||++- Palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ Priority (0: in front of background; 1: behind background)
;|+------- Flip sprite horizontally
;+-------- Flip sprite vertically
	.byte $40, $2, $00, $40
	.byte $40, $01, $00, $48
	.byte $48, $10, $00, $40
	.byte $48, $11, $00, $48

    ;sword
    .byte $50, $08, %00000001, $80
    .byte $50, $08, %01000001, $88
    .byte $58, $18, %00000001, $80
    .byte $58, $18, %01000001, $88


BACKGROUNDDATA:
	.include "damaNt.s"
        
BACKGROUNDPALETTEDATA:	;32 bytes
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
        .byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55

;
;;;;; CPU VECTORS

.segment "VECTORS"

.word NMIHandler
.word RESET

;
; CHR ROM
;
.segment "CHARS"
.incbin "damaSpr.chr"
.incbin "damaBg.chr"


;
; .etc
;
.segment "STARTUP"
.segment "SAMPLES"



