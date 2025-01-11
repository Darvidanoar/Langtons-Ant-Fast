.segment "STARTUP"
;******************************************************************
; Langton's ant (https://en.wikipedia.org/wiki/Langton%27s_ant)
;
; Langton's ant is a two-dimensional Turing machine with a very simple 
; set of rules but complex emergent behavior.
;
; The ant moves in what appears to be fairly irregular pattern until
; aroung 10,000 moves when it starts building a 'highway' pattern
; consisting of 104 repeating moves.
;******************************************************************

.segment "ZEROPAGE"
;******************************************************************
; The KERNAL and BASIC reserve all the addresses from $0080-$00FF. 
; Locations $00 and $01 determine which banks of RAM and ROM are  
; visible in high memory, and locations $02 through $21 are the
; pseudoregisters used by some of the new KERNAL calls
; (r0 = $02+$03, r1 = $04+$05, etc)
; So we have $22 through $7f to do with as we please, which is 
; where .segment "ZEROPAGE" variables are stored.
;******************************************************************

.org $0022

; Zero Page
XPos:             .res 2   ; current X Co-ordinate of ant
YPos:             .res 2   ; current Y Co-ordinate of ant
PixelAddr:        .res 3   ; Pixel address of ant (used when converting x/y coords to a memory address)

; Global Variables
paint_color:      .res 1
CurrDir:          .res 1
CurrCellColour:   .res 1

.segment "INIT"
.segment "ONCE"
.segment "CODE"

.org $080D

   jmp start

.include "..\..\INC\x16.inc"

; VERA
VSYNC_BIT         = $01
DISPLAY_SCALE     = 64 ; 2X zoom

; PETSCII
SPACE             = $20
CHAR_O            = $4f
CLR               = $93
HOME              = $13
CHAR_Q            = $51
CHAR_G            = $47
CHAR_ENTER        = $0D

; Colors
BLACK             = 0
WHITE             = 1
RED               = 2


start:
   stz VERA_dc_video ; disable display

   ; scale display to 2x zoom (320x240)
   lda #DISPLAY_SCALE
   sta VERA_dc_hscale
   sta VERA_dc_vscale

   ; configure layer 0
   lda #$07 ; 8bpp bitmap  
   sta VERA_L0_config

   ; enable layer 0, output mode VGA
   lda #$11
   sta VERA_dc_video

   ; initialise
   jsr clear_screen

   ;Set Start Pos at middle of screen (160,120)
   lda #$a0
   sta XPos
   stz XPos+1
   lda #$78
   sta YPos
   stz YPos+1

   ; set the starting direction the ant in facing (north)
   lda #$00
   sta CurrDir

   ; get the colour of the cell under the ant
   jsr getCell
   sta CurrCellColour
   
   ; set the paint_colour to red and paint the ant
   lda #RED
   sta paint_color
   jsr paint_cell

main_loop:

   ; based on he rules, calculate the next direction and paint the current cell its new colour
   jsr getNextDir
   jsr paint_cell

   ; move the ant to its new cell
   jsr moveAnt

   ; set the paint_colour to red and paint the ant
   lda #RED
   sta paint_color
   jsr paint_cell

   ; a delay to slow the process down.  Comment this out if you want to run a full speed.
   ;jsr delay

   ; check if the user has pushed 'q' tpo quit
   jsr GETIN
   cmp #CHAR_Q
   beq @exit ; Q was pressed

   jmp main_loop

@exit:
   lda #SPACE
   lda #CLR
   jsr CHROUT
   rts
   

moveAnt:
   ; based on the Current Direction, move the ant to the next cell co-ordinates
   lda CurrDir
   bne @checkSouth
   ; North
   lda YPos
   sec
   sbc #$01
   sta YPos
   lda YPos+1
   sbc #$00
   sta YPos+1
   jsr checkXYWrap
   jsr getCell
   sta CurrCellColour
   bra @next
@checkSouth:
   lda CurrDir
   cmp #$02
   bne @checkEast
   ; South
   lda YPos
   clc
   adc #$01
   sta YPos
   lda YPos+1
   adc #$00
   sta YPos+1
   jsr checkXYWrap
   jsr getCell
   sta CurrCellColour
   bra @next
@checkEast:
   lda CurrDir
   cmp #$01
   bne @checkWest
   ; East
   lda XPos
   clc
   adc #$01
   sta XPos
   lda XPos+1
   adc #$00
   sta XPos+1
   jsr checkXYWrap
   jsr getCell
   sta CurrCellColour
   bra @next
@checkWest:
   ; West
   lda XPos
   sec
   sbc #$01
   sta XPos
   lda XPos+1
   sbc #$00
   sta XPos+1
   jsr checkXYWrap
   jsr getCell
   sta CurrCellColour
@next:
   rts


getNextDir:
   ; based on he rules, calculate the next direction
   ; NextDir Values:
   ; 0=North
   ; 1=East
   ; 2=South
   ; 3=West

   lda CurrCellColour
   bne @TurnCW
@TurnCCW:
   lda CurrDir
   dec
   and #%00000011
   sta CurrDir
   lda #WHITE
   sta paint_color
   bra @endNextDir
@TurnCW:
   lda CurrDir
   inc
   and #%00000011
   sta CurrDir
   lda #BLACK
   sta paint_color
@endNextDir:
   rts


paint_cell: 
; Input: XPos/YPos = text map coordinates
; Input: paint_color
   jsr getPixelAddr
   stz VERA_ctrl
   lda PixelAddr+2
   sta VERA_addr_bank
   lda PixelAddr+1
   sta VERA_addr_high ; Y
   lda PixelAddr
   sta VERA_addr_low ; 2*X + 1
   lda paint_color
   sta VERA_data0
   rts

clear_screen:
   lda #$10 ; Stride 1, Bank 0
   sta VERA_addr_bank
   stz VERA_addr_low
   stz VERA_addr_high
   ldy #$00
clear_screen_loopY0:   
   ldx #$00
clear_screen_loopX0:
   stz VERA_data0
   dex
   bne clear_screen_loopX0
   dey
   bne clear_screen_loopY0

   ldy #$2C
clear_screen_loopY1:   
   ldx #$00
clear_screen_loopX1:
   stz VERA_data0
   dex
   bne clear_screen_loopX1
   dey
   bne clear_screen_loopY1
   rts


getCell: 
   ; Input: XPos/YPos = text map coordinates
   ; Output: A = value of the tile
   jsr getPixelAddr
   stz VERA_ctrl
   lda PixelAddr+2 ; stride = 0, bank 
   sta VERA_addr_bank
   lda PixelAddr+1
   sta VERA_addr_high 
   lda PixelAddr
   sta VERA_addr_low 
   lda VERA_data0   
   rts

getPixelAddr:
   ; Input: XPos/YPos = text map coordinates
   ; Output: PixelAddr
   lda YPos
   cmp #$cd
   bcs @getBank1Pixel ; YPos >= $CD (205)
   ; multiply Ypos x 2
   asl
   bcs @getBank0BPixel
   tay
   ;Y should now point to the correct index row in the YBaseAddrTable
   stz PixelAddr+2
   lda YBaseAddrTableB0A,y
   sta PixelAddr+1
   iny
   lda YBaseAddrTableB0A,y
   sta PixelAddr
   jmp @addX
@getBank0BPixel:
   tay
   ;Y should now point to the correct index row in the YBaseAddrTable
   stz PixelAddr+2
   lda YBaseAddrTableB0B,y
   sta PixelAddr+1
   iny
   lda YBaseAddrTableB0B,y
   sta PixelAddr
   jmp @addX
@getBank1Pixel:
   lda YPos
   sec
   sbc #$cd
  ; multiply Ypos x 2
   asl
   tay
   ;Y should now point to the correct index row in the YBaseAddrTable
   lda #$01
   sta PixelAddr+2
   lda YBaseAddrTableB1,y
   sta PixelAddr+1
   iny
   lda YBaseAddrTableB1,y
   sta PixelAddr
@addX:
   clc
   lda PixelAddr
   adc XPos
   sta PixelAddr
   lda PixelAddr+1
   adc XPos+1
   sta PixelAddr+1
   lda PixelAddr+2
   adc #$00
   sta PixelAddr+2
   rts 

checkXYWrap:
   ; Input XPos/YPos = text map coordinates
   ; this subroutine does the screen wrap around
   lda XPos+1
   cmp #$FF ; has x gone past the left border?
   bne @checkXRight
   lda XPos
   cmp #$FF ; has x gone past the left border?
   bne @checkXRight
   lda #$01 ; set XPos to 319
   sta XPos+1
   lda #$3f
   sta XPos
@checkXRight:
   lda XPos+1
   cmp #$01 ; has x gone past the right border?
   bne @checkYTop
   lda XPos
   cmp #$40
   bne @checkYTop 
   lda #$00
   sta XPos+1
   sta XPos
@checkYTop:
   lda YPos+1
   cmp #$ff ; has y gone past the top border?
   bne @checkYBottom
   lda YPos
   cmp #$ff
   bne @checkYBottom
   stz YPos+1
   lda #$ef
   sta YPos
@checkYBottom:
   lda YPos
   cmp  #$f0 ; has y gone past the bottom border?
   bne @endcheckXYWrap
   stz YPos
@endcheckXYWrap:
   rts



delay:                  ; Standard issue delay loop
    lda #$00
delayloop_outer:
    pha
    lda #$00
delayloop_inner:
    inc
    bne delayloop_inner
    pla
    inc   
    bne delayloop_outer
    rts


; This lookup table is used by the getPixelAddr subroutine
; Each row represents the address of the left-most pixel of each row
; There are 204 rows
YBaseAddrTableB0A:
   .byte $00,$00
   .byte $01,$40
   .byte $02,$80
   .byte $03,$C0
   .byte $05,$00
   .byte $06,$40
   .byte $07,$80
   .byte $08,$C0
   .byte $0A,$00
   .byte $0B,$40
   .byte $0C,$80
   .byte $0D,$C0
   .byte $0F,$00
   .byte $10,$40
   .byte $11,$80
   .byte $12,$C0
   .byte $14,$00
   .byte $15,$40
   .byte $16,$80
   .byte $17,$C0
   .byte $19,$00
   .byte $1A,$40
   .byte $1B,$80
   .byte $1C,$C0
   .byte $1E,$00
   .byte $1F,$40
   .byte $20,$80
   .byte $21,$C0
   .byte $23,$00
   .byte $24,$40
   .byte $25,$80
   .byte $26,$C0
   .byte $28,$00
   .byte $29,$40
   .byte $2A,$80
   .byte $2B,$C0
   .byte $2D,$00
   .byte $2E,$40
   .byte $2F,$80
   .byte $30,$C0
   .byte $32,$00
   .byte $33,$40
   .byte $34,$80
   .byte $35,$C0
   .byte $37,$00
   .byte $38,$40
   .byte $39,$80
   .byte $3A,$C0
   .byte $3C,$00
   .byte $3D,$40
   .byte $3E,$80
   .byte $3F,$C0
   .byte $41,$00
   .byte $42,$40
   .byte $43,$80
   .byte $44,$C0
   .byte $46,$00
   .byte $47,$40
   .byte $48,$80
   .byte $49,$C0
   .byte $4B,$00
   .byte $4C,$40
   .byte $4D,$80
   .byte $4E,$C0
   .byte $50,$00
   .byte $51,$40
   .byte $52,$80
   .byte $53,$C0
   .byte $55,$00
   .byte $56,$40
   .byte $57,$80
   .byte $58,$C0
   .byte $5A,$00
   .byte $5B,$40
   .byte $5C,$80
   .byte $5D,$C0
   .byte $5F,$00
   .byte $60,$40
   .byte $61,$80
   .byte $62,$C0
   .byte $64,$00
   .byte $65,$40
   .byte $66,$80
   .byte $67,$C0
   .byte $69,$00
   .byte $6A,$40
   .byte $6B,$80
   .byte $6C,$C0
   .byte $6E,$00
   .byte $6F,$40
   .byte $70,$80
   .byte $71,$C0
   .byte $73,$00
   .byte $74,$40
   .byte $75,$80
   .byte $76,$C0
   .byte $78,$00
   .byte $79,$40
   .byte $7A,$80
   .byte $7B,$C0
   .byte $7D,$00
   .byte $7E,$40
   .byte $7F,$80
   .byte $80,$C0
   .byte $82,$00
   .byte $83,$40
   .byte $84,$80
   .byte $85,$C0
   .byte $87,$00
   .byte $88,$40
   .byte $89,$80
   .byte $8A,$C0
   .byte $8C,$00
   .byte $8D,$40
   .byte $8E,$80
   .byte $8F,$C0
   .byte $91,$00
   .byte $92,$40
   .byte $93,$80
   .byte $94,$C0
   .byte $96,$00
   .byte $97,$40
   .byte $98,$80
   .byte $99,$C0
   .byte $9B,$00
   .byte $9C,$40
   .byte $9D,$80
   .byte $9E,$C0

YBaseAddrTableB0B:
   .byte $A0,$00
   .byte $A1,$40
   .byte $A2,$80
   .byte $A3,$C0
   .byte $A5,$00
   .byte $A6,$40
   .byte $A7,$80
   .byte $A8,$C0
   .byte $AA,$00
   .byte $AB,$40
   .byte $AC,$80
   .byte $AD,$C0
   .byte $AF,$00
   .byte $B0,$40
   .byte $B1,$80
   .byte $B2,$C0
   .byte $B4,$00
   .byte $B5,$40
   .byte $B6,$80
   .byte $B7,$C0
   .byte $B9,$00
   .byte $BA,$40
   .byte $BB,$80
   .byte $BC,$C0
   .byte $BE,$00
   .byte $BF,$40
   .byte $C0,$80
   .byte $C1,$C0
   .byte $C3,$00
   .byte $C4,$40
   .byte $C5,$80
   .byte $C6,$C0
   .byte $C8,$00
   .byte $C9,$40
   .byte $CA,$80
   .byte $CB,$C0
   .byte $CD,$00
   .byte $CE,$40
   .byte $CF,$80
   .byte $D0,$C0
   .byte $D2,$00
   .byte $D3,$40
   .byte $D4,$80
   .byte $D5,$C0
   .byte $D7,$00
   .byte $D8,$40
   .byte $D9,$80
   .byte $DA,$C0
   .byte $DC,$00
   .byte $DD,$40
   .byte $DE,$80
   .byte $DF,$C0
   .byte $E1,$00
   .byte $E2,$40
   .byte $E3,$80
   .byte $E4,$C0
   .byte $E6,$00
   .byte $E7,$40
   .byte $E8,$80
   .byte $E9,$C0
   .byte $EB,$00
   .byte $EC,$40
   .byte $ED,$80
   .byte $EE,$C0
   .byte $F0,$00
   .byte $F1,$40
   .byte $F2,$80
   .byte $F3,$C0
   .byte $F5,$00
   .byte $F6,$40
   .byte $F7,$80
   .byte $F8,$C0
   .byte $FA,$00
   .byte $FB,$40
   .byte $FC,$80
   .byte $FD,$C0
   .byte $FF,$00

YBaseAddrTableB1:
   .byte $00,$40
   .byte $01,$80
   .byte $02,$C0
   .byte $04,$00
   .byte $05,$40
   .byte $06,$80
   .byte $07,$C0
   .byte $09,$00
   .byte $0A,$40
   .byte $0B,$80
   .byte $0C,$C0
   .byte $0E,$00
   .byte $0F,$40
   .byte $10,$80
   .byte $11,$C0
   .byte $13,$00
   .byte $14,$40
   .byte $15,$80
   .byte $16,$C0
   .byte $18,$00
   .byte $19,$40
   .byte $1A,$80
   .byte $1B,$C0
   .byte $1D,$00
   .byte $1E,$40
   .byte $1F,$80
   .byte $20,$C0
   .byte $22,$00
   .byte $23,$40
   .byte $24,$80
   .byte $25,$C0
   .byte $27,$00
   .byte $28,$40
   .byte $29,$80
   .byte $2A,$C0
