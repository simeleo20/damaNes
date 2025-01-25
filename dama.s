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
buttons1: 	.res 1
lastButtons1:	.res 1

BUTTON_A      = 1 << 7
BUTTON_B      = 1 << 6
BUTTON_SELECT = 1 << 5
BUTTON_START  = 1 << 4
BUTTON_UP     = 1 << 3
BUTTON_DOWN   = 1 << 2
BUTTON_LEFT   = 1 << 1
BUTTON_RIGHT  = 1 << 0

JOYPAD1 = $4016
JOYPAD2 = $4017

   ; designate a oam and drawing buffer
oam	   =	 $0200
;drawingbuf =	 $0300     	; buffer for PPU drawing

defaultStackPointer = $ff
defaultBufferPointer = $cf

stackPointer: 	.res 1
bufferPointer: 	.res 1

;buffer data 	[  size ] 1 stack top 
;	     	[address] 2
; 		[  data ] ( size ) 

; other variables
soft2000:	.res 2		; buffering $2000 writes PPUCTRL
soft2001:	.res 2		; buffering $2001 writes PPUMASK



needdma:	.res 1		; nonzero if NMI should perform sprite DMA
needdraw:	.res 1		; nonzero if NMI needs to do drawing from the buffer
needppureg:	.res 1		; nonzero if NMI should update $2000/$2001/$2005
sleeping:	.res 1		; nonzero if main thread is waiting for VBlank

xscroll:	.res 1		
yscroll:	.res 1	

gameState:	.res 1

MOVING_STATE = $01
SELECT_STATE = $02

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
    	ldx #defaultStackPointer
        stx stackPointer
    	txs                           ;transfer X to stack pointer
	
        lda #defaultBufferPointer
        sta bufferPointer

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
	lda #$01
        sta needdma

LOADBACKGROUND:
    	; setup address in PPU for nametable data
    	BIT $2002
    	LDA #$20
    	STA $2006
    	LDA #$00
    	STA $2006

    	ldx #$04;Increment pointer 4 times to write 1024 bytes of data
    	ldy #$00
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
	sta soft2000
        lda #%00011110          ;show background and sprites
        sta $2001
    	sta soft2001


        
PROVABUFFER:
	;	save stack pointer and swapping to buffer pointer
	tsx
        stx stackPointer
	ldx #defaultBufferPointer
        txs
        
	lda #$06
        pha
        lda #$08
        pha
        lda #<$2084
        pha
        lda #>$2084
        pha
        lda #$02
        pha
        lda #$01
        sta needdraw
        sta needppureg
        
;	rswap to stack pointer
	tsx
        stx bufferPointer
	ldx stackPointer
        txs
        
SETUP:
	lda #MOVING_STATE
	sta gameState
        
;-----------------loop----------------------

INFLOOP:
	lda buttons1
        sta lastButtons1
        jsr readController    
	
        lda gameState
        cmp #MOVING_STATE
        beq MOVINGSTATE
        cmp SELECT_STATE
        beq SELECTSTATE
        jmp INFLOOP
        
MOVINGSTATE:
        lda buttons1
        and #BUTTON_DOWN
        beq :+
        
        inc $0200
        
        
        
        :
        

        
SELECTSTATE:




SENDSPRITEDATA:


jmp INFLOOP	; endless loop

;-----------------------------------------

;--------------INTERRUPT HANDLERS-----------

NMIHandler:
	pha         ; back up registers (important)
     	txa
     	pha
     	tya
     	pha

        
        

	lda needdma
	beq :+
        	lda #0      ; do sprite DMA
         	sta $2003   ; conditional via the 'needdma' flag
         	lda #>oam
         	sta $4014

:	lda needdraw       ; do other PPU drawing (NT/Palette/whathaveyou)
        	beq :+             ;  conditional via the 'needdraw' flag
         	bit $2002        ; clear VBl flag, reset $2005/$2006 toggle
         	jsr doDrawing    ; draw the stuff from the drawing buffer
         	dec needdraw

:	lda needppureg
	beq :+
		lda soft2001   ; copy buffered $2000/$2001 (conditional via needppureg)
        	sta $2001
        	lda soft2000
         	sta $2000

         	bit $2002
         	lda xscroll    ; set X/Y scroll (conditional via needppureg)
         	sta $2005
         	lda yscroll
         	sta $2005

:	;jsr MusicEngine

       		lda #0         ; clear the sleeping flag so that WaitFrame will exit
       		sta sleeping   ;   note that you should not 'dec' here, as sleeping might
                ;   already be zero (will be the case during slowdown)

        
        
	pla            ; restore regs and exit
     	tay
     	pla
     	tax
     	pla
     	rti
        
        
        
; ----------------SUBRUTINE-----------------

readController:

	lda #$01
  	sta $4016
  	lda #$00
 	sta $4016
  	ldx #$08

ReadControllerLoop:

  	lda $4016
  	lsr a               ; bit0 -> Carry
  	rol buttons1     ; bit0 <- Carry
  	dex
  	bne ReadControllerLoop
  	rts


doDrawing:
;	save stack pointer and swapping to buffer pointer
	tsx
        stx stackPointer
	ldx bufferPointer
        txs

READBUFFER:
	; setup address in PPU for nametable data
        pla
        beq ENDREADB
        tax
        
    	bit $2002
    	pla
    	sta $2006
    	pla
    	sta $2006
	
READDATA:
        pla
        sta $2007
        dex
        bne READDATA
        jmp READBUFFER
        
ENDREADB:

;	reswap to stack pointer
	tsx
        stx bufferPointer
	ldx stackPointer
        txs

	rts
   ;--------------------------------------
   ; WaitFrame - waits for VBlank, returns after NMI handler is done

waitFrame:
     inc sleeping
     @loop:
       lda sleeping
       bne @loop
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



