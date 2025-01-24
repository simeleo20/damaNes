
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


bpointer: .res 2

;bit:       7     6     5     4     3     2     1     0
;button:    A     B   select start  up   down  left right
buttons: .res 1

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
    cpx #$10
    bne LOADSPRITES

    ; Initialize world to point to world data
    LDA #<BACKGROUNDDATA
    STA bpointer
    LDA #>BACKGROUNDDATA
    STA bpointer+1

LOADBACKGROUND:
    ; setup address in PPU for nametable data
    BIT $2002
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006

    ldx #$04;Increment pointer 4 times to write 1024 bytes of data
    LDY #$00
LOADBACKGROUNDLOOP:                       ; loop to draw entire nametable
        LDA (bpointer),y
        STA $2007
        INY
        BNE LOADBACKGROUNDLOOP
        INC bpointer+1
        DEX
        BNE LOADBACKGROUNDLOOP

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
        
        jsr ReadController
        
        
        
        
	rti
        
; SUBRUTINE        
ReadController:

	lda #$01
  	sta $4016
  	lda #$00
 	sta $4016
  	ldx #$08

ReadControllerLoop:

  	lda $4016
  	lsr a           ; bit0 -> Carry
  	rol buttons     ; bit0 <- Carry
  	dex
  	bne ReadControllerLoop
  	rts



        
        
PALETTEDATA:
.incbin "palettes.dat"

SPRITEDATA:
;Y, SPRITE NUM, attributes, X
;76543210
;||||||||
;||||||++- Palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ Priority (0: in front of background; 1: behind background)
;|+------- Flip sprite horizontally
;+-------- Flip sprite vertically
	.byte $3f, $06, %00100010, $40
	.byte $3f, $06, %01100010, $48
	.byte $47, $06, %10100010, $40
	.byte $47, $06, %11100010, $48





BACKGROUNDDATA:
	.include "damaNt.s"
        
BACKGROUNDPALETTEDATA:	;32 bytes
	.byte $55, $55, $55, $55, $55, $55, $55, $55
        .byte $55, $00, $00, $00, $00, $00, $55, $55
	.byte $55, $00, $00, $00, $00, $00, $55, $55
        .byte $55, $00, $00, $00, $00, $00, $55, $55
	.byte $55, $55, $55, $55, $55, $55, $55, $55
        .byte $55, $55, $55, $55, $55, $55, $55, $55
        .byte $55, $55, $55, $55, $55, $55, $55, $55
        .byte $55, $55, $55, $55, $55, $55, $55, $55

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



