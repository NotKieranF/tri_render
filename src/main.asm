.INCLUDE	"main.h"
.INCLUDE	"nes.h"
.INCLUDE	"nmi.h"
.INCLUDE	"render.h"
.INCLUDE	"math.h"
.INCLUDE	"controller.h"
; Mark the kernal to be ran after initialization
.EXPORT		post_reset := main



; Constants
CAMERA_ROT_SPEED	= $0100
CAMERA_MOV_SPEED	= $0008



.SEGMENT	"SAVERAM"
pal_buffer:				.RES 32



.CODE
.PROC main

init_pal:
	LDA #$3F
	STA PPU::ADDR
	LDA #$00
	STA PPU::ADDR

	LDX #$00
:	LDA default_pal, X
	STA PPU::DATA
	INX
	CPX #$20
	BNE :-

init_pattern:
	LDA #$10
	STA PPU::ADDR
	LDA #$00
	STA PPU::ADDR

	LDX #$00
:	LDA balz, X
	STA PPU::DATA
	INX
	BNE :-

	LDA #PPU::RENDER_SP | PPU::RENDER_BG
	STA soft_ppumask
	LDA #PPU::DEFAULT_CTRL
	STA soft_ppuctrl
	STA PPU::CTRL


; Testing
.SCOPE
;	x0		:= $F0
;	y0		:= $F1
;	x1		:= $F2
;	y1		:= $F3
;
;	LDA #$80
;	STA x0
;	STA y0
;	LDA #$C0
;	STA x1
;	STA y1
;forever:
;	LDA buttons_held
;	BIT identity + BUTTON_LEFT
;	BEQ :+
;		DEC x1
;:	BIT identity + BUTTON_RIGHT
;	BEQ :+
;		INC x1
;:	BIT identity + BUTTON_UP
;	BEQ :+
;		DEC y1
;:	BIT identity + BUTTON_DOWN
;	BEQ :+
;		INC y1
;:	
;	LDA x0
;	STA $1C
;	LDA y0
;	STA $1D
;	LDA x1
;	STA $1E
;	LDA y1
;	STA $1F
;	STA $4445
;	JSR draw_line
;	STA $4445

; Test udiv_8x8bit_frac
;.PROC	test_udiv_8x8bit_frac
;	LDA #$00
;	LDX #$01
;	STA $FE
;	STX $FF
;:	LDA $FE
;	LDX $FF
;	JSR udiv_8x8bit_frac
;	STA $EE
;	STX $EF
;	STA $4444
;	INC $FE
;	BNE :-
;	INC $FF
;	BNE :-
;.ENDPROC

.PUSHSEG
.RODATA
.PROC	test_poly
.BYTE	$0B
.BYTE	$08
.BYTE	$80, $40
.BYTE	$C0, $80
.BYTE	$80, $C0
.BYTE	$40, $80
.BYTE	$20, $60
.ENDPROC

.BSS
test_poly_buffer:	.res 32

.POPSEG

	LDX #$00
:	LDA test_poly, X
	STA test_poly_buffer, X
	INX
	CPX #.SIZEOF(test_poly)
	BNE :-

	LDA #$02
	STA $FF

forever:
	LDX $FF
	LDA buttons_held
	AND #BUTTON_UP
	BEQ :+
		DEC test_poly_buffer + 1, X
:	LDA buttons_held
	AND #BUTTON_DOWN
	BEQ :+
		INC test_poly_buffer + 1, X
:	LDA buttons_held
	AND #BUTTON_LEFT
	BEQ :+
		DEC test_poly_buffer + 0, X
:	LDA buttons_held
	AND #BUTTON_RIGHT
	BEQ :+
		INC test_poly_buffer + 0, X
:	LDA buttons_down
	AND #BUTTON_A
	BEQ :++
		LDA $FF
		CLC
		ADC #$02
		CMP test_poly_buffer + 0
		BCC :+
			LDA #$02
	:	STA $FF
:	LDA buttons_down
	AND #BUTTON_B
	BEQ :++
		LDA $FF
		SEC
		SBC #$02
		CMP #$02
		BCS :+
			LDA test_poly_buffer + 0
			SBC #$00
	:	STA $FF
:	LDA buttons_down
	AND #BUTTON_SELECT
	BEQ :+
		INC test_poly_buffer + 1
:	LDA buttons_down
	AND #BUTTON_START
	BEQ :+
		DEC test_poly_buffer + 1
:

	; Clear partial tile allocations
	LDA #$01
	STA next_partial_pattern_index

	; Clear opaque tile allocations
	LDA #RENDER_MAX_TILES - 1
	STA next_opaque_pattern_index

	LDA #$00
	LDX #$00
:	STA opaque_tile_indices, X
	INX
	CPX #NUM_SHADES
	BNE :-

	; Clear nametable buffer
	LDA #$00
	LDX #$00
	@loop:
	.REPEAT	::SCREEN_HEIGHT_TILES, i
		STA nametable_buffer + i * ::SCREEN_WIDTH_TILES, X
	.ENDREP
	INX
	CPX #::SCREEN_WIDTH_TILES
	BNE @loop

	; Setup poly pointer
	LDA #<test_poly_buffer
	STA $00
	LDA #>test_poly_buffer
	STA $01

	; Rasterize polygon with performance highlighting
	LDA #PPU::RED_EMPHASIS | PPU::GRAYSCALE
	ORA soft_ppumask
	STA PPU::MASK
	JSR rasterize_poly
	LDA soft_ppumask
	STA PPU::MASK

	; Transfer gfx buffers
	LDA #$00
	STA soft_ppumask
	JSR wait_for_nmi

	; Transfer nametable buffer
asd:
	LDA #>$2000
	STA PPU::ADDR
	LDA #<$2000
	STA PPU::ADDR

	LDA #<(nametable_buffer + $100 - <(SCREEN_WIDTH_TILES * SCREEN_HEIGHT_TILES))
	STA $00 + 0
	LDA #>(nametable_buffer + $100 - <(SCREEN_WIDTH_TILES * SCREEN_HEIGHT_TILES))
	STA $00 + 1
	LDY #$100 - <(SCREEN_WIDTH_TILES * SCREEN_HEIGHT_TILES)
	LDX #>(SCREEN_WIDTH_TILES * SCREEN_HEIGHT_TILES - 1) + 1
@loop:
	LDA ($00), Y
	STA PPU::DATA
	INY
	BNE @loop
	INC $00 + 1
	DEX
	BNE @loop

	; Transfer pattern buffer
ad2:
	LDA #>$0000
	STA PPU::ADDR
	LDA #<$0000
	STA PPU::ADDR
	LDX #$00
@loop:
	.REPEAT	8, i
		LDA .ident(.sprintf("pattern_buffer_%d", i)), X
		STA PPU::DATA
	.ENDREP
	LDA #$00
	.REPEAT	8, i
		STA PPU::DATA
	.ENDREP
	INX
	CPX next_partial_pattern_index
	BNE @loop

	LDA #PPU::RENDER_SP | PPU::RENDER_BG
	STA soft_ppumask
	JSR wait_for_nmi

	JSR read_controller
	JMP forever
.ENDSCOPE

;	multiplier		:= $10
;	multiplicand	:= $12
;	product			:= $14
;
;test_mul_16x16bit_unsigned_hi:
;	JSR math_init
;	LDA #$00
;	STA multiplier + 0
;	STA multiplier + 1
;	STA multiplicand + 0
;	STA multiplicand + 1
;@outer_loop:
;	LDY multiplier + 0
;	LDA multiplier + 1
;	JSR set_mul_16x16bit_unsigned_hi16
;@inner_loop:
;	LDY multiplicand + 0
;	LDA multiplicand + 1
;	JSR mul_16x16bit_unsigned_hi16
;	STA product + 0
;	STY product + 1
;	STA $4444
;
;	INC multiplicand + 0
;	BNE @inner_loop
;	INC multiplicand + 1
;	BNE @inner_loop
;
;	LDA #<1013
;	CLC
;	ADC multiplier + 0
;	STA multiplier + 0
;	LDA #>1013
;	ADC multiplier + 1
;	STA multiplier + 1
;
;	LDA multiplier + 0
;	BNE @outer_loop
;	LDA multiplier + 1
;	BNE @outer_loop

;	multiplier		:= $10
;	multiplicand	:= $11
;	product			:= $13
;
;test_mul_8x16bit_signed_hi16:
;	JSR math_init
;	LDA #$00
;	STA multiplier
;	STA multiplicand + 0
;	STA multiplicand + 1
;@outer_loop:
;	LDA multiplier
;	JSR set_mul_8x16bit_signed_hi16
;@inner_loop:
;	LDY multiplicand + 0
;	LDA multiplicand + 1
;	JSR mul_8x16bit_signed_hi16
;	STA product + 0
;	STY product + 1
;	STA $4444
;
;	INC multiplicand + 0
;	BNE @inner_loop
;	INC multiplicand + 1
;	BNE @inner_loop
;
;	INC multiplier
;	BNE @outer_loop

;	dividend	:= $10
;	divisor		:= $11
;	quotient	:= $12
;test_div_7x7bit_unsigned_fractional:
;	LDA #$01
;	STA divisor
;	LDA #$00
;	STA dividend
;@loop:
;	LDA dividend
;	LDX divisor
;	JSR div_7x7bit_unsigned_fractional
;	STX quotient
;	STX $4444
;
;	INC dividend
;	LDA dividend
;	CMP divisor
;	BNE @loop
;
;	LDA #$00
;	STA dividend
;	INC divisor
;	LDA divisor
;	CMP #$80
;	BNE @loop


	JSR math_init
main_loop:
	JSR read_controller
	JSR move_camera

	STA $4445
;	LDA #PPU::RED_EMPHASIS | PPU::RENDER_SP
;	STA PPU::MASK
	JSR render_frame
;	LDA #PPU::RENDER_SP
;	STA PPU::MASK
	STA $4445

	JSR wait_for_nmi
	JMP main_loop
.ENDPROC


.PROC	move_camera
	LDA #BUTTON_SELECT
	AND buttons_held
	BEQ @translate

@rotate:
	LDA #BUTTON_UP
	AND buttons_held
	BEQ :+
	LDA camera_roll_lo
	CLC
	ADC #<CAMERA_ROT_SPEED
	STA camera_roll_lo
	LDA camera_roll_hi
	ADC #>CAMERA_ROT_SPEED
	STA camera_roll_hi

:	LDA #BUTTON_DOWN
	AND buttons_held
	BEQ :+
	LDA camera_roll_lo
	CLC
	SBC #<CAMERA_ROT_SPEED
	STA camera_roll_lo
	LDA camera_roll_hi
	SBC #>CAMERA_ROT_SPEED
	STA camera_roll_hi

:	LDA #BUTTON_RIGHT
	AND buttons_held
	BEQ :+
	LDA camera_pitch_lo
	CLC
	ADC #<CAMERA_ROT_SPEED
	STA camera_pitch_lo
	LDA camera_pitch_hi
	ADC #>CAMERA_ROT_SPEED
	STA camera_pitch_hi

:	LDA #BUTTON_LEFT
	AND buttons_held
	BEQ :+
	LDA camera_pitch_lo
	CLC
	SBC #<CAMERA_ROT_SPEED
	STA camera_pitch_lo
	LDA camera_pitch_hi
	SBC #>CAMERA_ROT_SPEED
	STA camera_pitch_hi

:	LDA #BUTTON_A
	AND buttons_held
	BEQ :+
	LDA camera_yaw_lo
	CLC
	ADC #<CAMERA_ROT_SPEED
	STA camera_yaw_lo
	LDA camera_yaw_hi
	ADC #>CAMERA_ROT_SPEED
	STA camera_yaw_hi

:	LDA #BUTTON_B
	AND buttons_held
	BEQ :+
	LDA camera_yaw_lo
	CLC
	SBC #<CAMERA_ROT_SPEED
	STA camera_yaw_lo
	LDA camera_yaw_hi
	SBC #>CAMERA_ROT_SPEED
	STA camera_yaw_hi

:	RTS

@translate:
	LDA #BUTTON_UP
	AND buttons_held
	BEQ :+
	LDA camera_pos_z_sub
	CLC
	ADC #<CAMERA_MOV_SPEED
	STA camera_pos_z_sub
	LDA camera_pos_z_lo
	ADC #>CAMERA_MOV_SPEED
	STA camera_pos_z_lo
	LDA camera_pos_z_hi
	ADC #^CAMERA_MOV_SPEED
	STA camera_pos_z_hi

:	LDA #BUTTON_DOWN
	AND buttons_held
	BEQ :+
	LDA camera_pos_z_sub
	SEC
	SBC #<CAMERA_MOV_SPEED
	STA camera_pos_z_sub
	LDA camera_pos_z_lo
	SBC #>CAMERA_MOV_SPEED
	STA camera_pos_z_lo
	LDA camera_pos_z_hi
	SBC #^CAMERA_MOV_SPEED
	STA camera_pos_z_hi

:	LDA #BUTTON_RIGHT
	AND buttons_held
	BEQ :+
	LDA camera_pos_x_sub
	CLC
	ADC #<CAMERA_MOV_SPEED
	STA camera_pos_x_sub
	LDA camera_pos_x_lo
	ADC #>CAMERA_MOV_SPEED
	STA camera_pos_x_lo
	LDA camera_pos_x_hi
	ADC #^CAMERA_MOV_SPEED
	STA camera_pos_x_hi

:	LDA #BUTTON_LEFT
	AND buttons_held
	BEQ :+
	LDA camera_pos_x_sub
	SEC
	SBC #<CAMERA_MOV_SPEED
	STA camera_pos_x_sub
	LDA camera_pos_x_lo
	SBC #>CAMERA_MOV_SPEED
	STA camera_pos_x_lo
	LDA camera_pos_x_hi
	SBC #^CAMERA_MOV_SPEED
	STA camera_pos_x_hi

:	LDA #BUTTON_A
	AND buttons_held
	BEQ :+
	LDA camera_pos_y_sub
	CLC
	ADC #<CAMERA_MOV_SPEED
	STA camera_pos_y_sub
	LDA camera_pos_y_lo
	ADC #>CAMERA_MOV_SPEED
	STA camera_pos_y_lo
	LDA camera_pos_y_hi
	ADC #^CAMERA_MOV_SPEED
	STA camera_pos_y_hi

:	LDA #BUTTON_B
	AND buttons_held
	BEQ :+
	LDA camera_pos_y_sub
	SEC
	SBC #<CAMERA_MOV_SPEED
	STA camera_pos_y_sub
	LDA camera_pos_y_lo
	SBC #>CAMERA_MOV_SPEED
	STA camera_pos_y_lo
	LDA camera_pos_y_hi
	SBC #^CAMERA_MOV_SPEED
	STA camera_pos_y_hi

:	RTS
.ENDPROC



;.PROC bresenham
;	x1		:= $00
;	y1		:= $01
;	dx		:= $02
;	dy		:= $03
;	error	:= $04
;
;	ptr		:= $05	; And $06
;
;; dx = abs(x1 - x0)
;	TYA
;	SEC
;	SBC x1
;	BPL :+
;	EOR #$FF
;	CLC
;	ADC #$01
;:	STA dx
;
;; dy = -abs(y1 - y0)
;	TXA
;	SEC
;	SBC y1
;	BMI :+
;	EOR #$FF
;	CLC
;	ADC #$01
;:	STA dy
;
;; error = dx + dy
;	LDA dx
;	CLC
;	ADC dy
;	STA error
;
;@loop:
;	JSR plot
;
;	; if x0 == x1 && y0 == y1 break
;	CPY x1
;	BNE :+
;	CPX y1
;	BEQ exit
;:
;	LDA error
;	ASL
;	CMP dy
;	BCC :+
;		CPY x1
;		BEQ :+
;		PHA
;		LDA error
;		CLC
;		ADC dy
;		STA error
;		PLA
;		INY
;:
;	CMP dx
;	BEQ :+
;	BCS :++
;:		CPX y1
;		BEQ :+
;		LDA error
;		CLC
;		ADC dx
;		STA error
;		INX
;:
;	JMP @loop
;
;exit:
;	RTS
;
;
;
;plot:
;	TXA							; Should be replaced by a lookup table
;	ASL
;	ASL
;	ASL
;	ASL
;	ASL
;	CLC
;	ADC #<name_buffer
;	STA ptr + 0
;
;	TXA
;	LSR
;	LSR
;	LSR
;	CLC
;	ADC #>name_buffer
;	STA ptr + 1
;
;	LDA #$FF
;	STA (ptr), Y
;	RTS
;.ENDPROC
;

;;
;.PROC	draw_tri
;	tri_x0		:= $00
;	tri_x1		:= $01
;	tri_x2		:= $02
;	tri_y0		:= $03
;	tri_y1		:= $04
;	tri_y2		:= $05
;
;	line_x1		:= $06
;	line_y1		:= $07
;	line_dx		:= $08
;	line_dy		:= $09
;	line_hdx	:= $0A
;	line_hdy	:= $0B
;	line_error	:= $0C	; And $0D
;
;; dx = abs(x0 - x1)
;	TYA
;	SEC
;	SBC line_x1
;	BCS :+
;	EOR #$FF
;	ADC #$01
;:	STA line_dx
;	ASL
;	STA line_hdx
;
;; dy = abs(y1 - y0)
;	TXA
;	SEC
;	SBC line_y1
;	BCS :+
;	EOR #$FF
;	ADC #$01
;:	STA line_dy
;	ASL
;	EOR #$FF
;	CLC
;	ADC #$01
;	STA line_hdy
;
;; error = dx - dy
;	LDA line_dx
;	SEC
;	SBC line_dy
;	STA line_error + 0
;	LDA #$00
;	BCS :+
;	LDA #$FF
;:	STA line_error + 1
;
;loop:
;; if x0 == x1 && y0 == y1 break
;	CPY line_x1
;	BNE :+
;	CPX line_y1
;	BEQ exit
;:
;
;; if error >= hdy
;	LDA error + 1
;	BPL :+
;	LDA error + 0
;	CMP hdy
;	:
;		; if x0 == x1 break
;		CPY line_x1
;		BEQ :-
;		; error = error - dy
;		LDA error + 0
;		SEC
;		SBC line_dy
;		STA error + 0
;		LDA error + 1
;		SBC #$00
;		STA error + 1
;		; x0 = x0 + sx
;		INY
;
;
;
;exit:
;
;
;.ENDPROC








; ////TEMP COMMENT

; The whole of rendering has a high potential for speed gains by making it smc
;.PROC	draw_tri_filled
;
;FILLED_TILES = $0F
;
;; RAM declarations
;.PUSHSEG
;.ZEROPAGE
;tri_x0:			.RES 1
;tri_x1:			.RES 1
;tri_x2:			.RES 1
;tri_y0:			.RES 1
;tri_y1:			.RES 1
;tri_y2:			.RES 1
;
;left_buffer:	.RES 8	; X positions of the leftmost pixels in each pixel row
;right_buffer:	.RES 8	; X positions of the rightmost pixels in each pixel row
;
;y_min:			.RES 1	; Y position of the topmost pixel in the current tile row
;y_max:			.RES 1	; Y position of the bottommost pixel in the current tile row
;x_min:			.RES 1	; X position of the leftmost pixel in the current tile row
;x_max:			.RES 1	; X position of the rightmost pixel in the current tile row
;
;tile_x:			.RES 1
;tile_y:			.RES 1
;
;tile_buffer:	.RES 8	; Holds the current tile mask
;row_buffer:		.RES 1	; Holds the current row of pixels within the current tile
;.POPSEG
;
;
;
;
;
;
;; Basic idea for triangle rendering algorithm
;;	For each triangle:
;
;;	For each row of tiles:
;;		Compute the tile coordinates of the leftmost and rightmost tiles
;;		Draw the leftmost tile using the contents of left buffer/right buffer
;;		Increment the x coordinate, and process the next tile. Repeat until we either reach the rightmost tile, or encounter a blank tile
;;		If we encountered a blank tile:
;;			Note its x position
;;			Draw the rightmost tile using the contents of left buffer/right buffer
;;			Decrement the x coordinate, and process the previous tile. Repeat until we encounter a blank tile. Note its x position
;;			Fill the solid tiles from the leftmost blank tile, to the rightmost blank tile
;;
;;	draw_row() {
;;
;;	}
;
;;	For each tile drawn:
;;		Determine if it's fully occluded, partially occluded, or not occluded
;;		If the tile is fully occluded:
;;			We can immediately exit
;;		Determine the mask for the current tile (implemented)
;;		If the tile is partially occluded:
;;			Determine the mask for the tile that is occluding
;;			Mask off the appropriate parts of the current tile
;;		Write the tile to the pattern table buffer
;;
;;	draw_tile() {
;;		target_name = name_buffer[tile_x][tile_y]
;;		if (target_name < SOLID_TILES)
;;			return;
;;
;;		for (y = y_min; y < y_max; y++) {
;;			if (left_buffer[y] < tile_x << 4) {
;;				row_buffer = 0;
;;			}
;;		}
;;		
;;		if (target_name == BLANK_TILE) {
;;			pattern_buffer[pattern_index << 4] = tile_mask;
;;			pattern_index++;
;;		} else {
;;			tile_mask &= pattern_buffer[target_name << 4]
;;			pattern_buffer[target_name << 4]
;;		}
;;	}
;
;
;
;
;; Test values
;	LDA #$00
;	STA left_buffer + 0
;	LDA #$01
;	STA left_buffer + 1
;	LDA #$02
;	STA left_buffer + 2
;	LDA #$03
;	STA left_buffer + 3
;	LDA #$00
;	STA left_buffer + 4
;	STA left_buffer + 5
;	STA left_buffer + 6
;	STA left_buffer + 7
;
;	LDA #$07 + 8
;	STA right_buffer + 0
;	STA right_buffer + 1
;	STA right_buffer + 2
;	STA right_buffer + 3
;	LDA #$06 + 8
;	STA right_buffer + 4
;	LDA #$05 + 8
;	STA right_buffer + 5
;	LDA #$04 + 8
;	STA right_buffer + 6
;	LDA #$03 + 8
;	STA right_buffer + 7
;
;	LDA #$00
;	STA top_pixel
;	LDA #$08
;	STA bottom_pixel
;	LDA #$08
;	STA left_pixel
;	STA right_pixel
;
;
;
;
;fill_row:
;	LDA left_pixel		; Get tile position of leftmost pixel
;	AND #%11111000
;	STA left_pixel
;
;	LDA right_pixel		; Get tile position of rightmost pixel
;	AND #%11111000
;	STA right_pixel
;
;
;draw_tile:
;	LDY tile_x			; Check tile existing tile at current position
;	LDX tile_y
;	LDA table_3, X
;	STA ptr + 0
;	LDA table_4, X
;	STA ptr + 1
;	LDA (ptr), Y
;	CMP #FILLED_TILES
;	BCC @exit
;@not_fully_occluded:
;	PHA					; Save existing tile to check for partial occlusion later
;
;	LDY top_pixel
;@loop:
;	LDA left_buffer, Y
;	SEC
;	SBC left_pixel
;	BCS :+
;		LDA #$00
;
;:	CMP #$08
;	BCC :+
;		LDA #$00
;		BEQ next
;
;:	TAX
;	LDA table_1, X
;	STA row_buffer
;
;	LDA right_buffer, Y
;	SEC
;	SBC left_pixel
;	BCS :+
;		LDA #$00
;		BEQ next
;
;:	CMP #$08
;	BCC :+
;		LDA #$07
;
;:	TAX
;	LDA table_2, X
;	AND row_buffer
;
;next:
;	STA tile_buffer, Y
;	INY
;	CPY bottom_pixel
;	BNE loop
;
;check_blank:
;	LDA #$FF
;	AND tile_buffer + 0
;	AND tile_buffer + 1
;	AND tile_buffer + 2
;	AND tile_buffer + 3
;	AND tile_buffer + 4
;	AND tile_buffer + 5
;	AND tile_buffer + 6
;	AND tile_buffer + 7
;	CMP #$FF
;	BEQ @blank
;
;	LDY #$07
;:	LDA tile_buffer, Y
;	STA pattern_buffer, Y
;	DEY
;	BPL :-
;
;@blank:
;
;	RTS
;
;; Tables
;.PUSHSEG
;.RODATA
;table_1:
;.BYTE	%11111111, %01111111, %00111111, %00011111
;.BYTE	%00001111, %00000111, %00000011, %00000001
;
;table_2:
;.BYTE	%10000000, %11000000, %11100000, %11110000
;.BYTE	%11111000, %11111100, %11111110, %11111111
;.POPSEG
;
;.ENDPROC



; Temp stuff
;.PROC	update_screen
;	ptr		:= $00	; And $01
;
;	LDA #PPU::DISABLE_RENDERING
;	STA PPU::MASK
;
;update_patterns:
;	LDA #>$0000
;	STA PPU::ADDR
;	LDA #<$0000
;	STA PPU::ADDR
;
;	LDA #<pattern_buffer
;	STA ptr + 0
;	LDA #>pattern_buffer
;	STA ptr + 1
;
;	LDY #<$2000
;	LDX #>$2000
;:	LDA (ptr), Y
;	STA PPU::DATA
;	INY
;	BNE :-
;	INC ptr + 1
;	DEX
;	BNE :-
;
;update_names:
;	LDA #>$2000
;	STA PPU::ADDR
;	LDA #<$2000
;	STA PPU::ADDR
;
;	LDA #<name_buffer
;	STA ptr + 0
;	LDA #>name_buffer
;	STA ptr + 1
;
;	LDY #<$0400
;	LDX #>$0400
;:	LDA (ptr), Y
;	STA PPU::DATA
;	INY
;	BNE :-
;	INC ptr + 1
;	DEX
;	BNE :-
;
;update_pals:
;	LDA #>$3F00
;	STA PPU::ADDR
;	LDA #<$3F00
;	STA PPU::ADDR
;
;	LDX #$00
;:	LDA pal_buffer, X
;	STA PPU::DATA
;	INX
;	CPX #$20
;	BNE :-
;
;exit:
;	LDA soft_ppumask
;	STA PPU::MASK
;
;	RTS
;.ENDPROC

default_pal:
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20

.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20

balz:
.INCBIN	"balz.chr"