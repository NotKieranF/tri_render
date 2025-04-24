.INCLUDE	"main.h"
.INCLUDE	"nes.h"
.INCLUDE	"nmi.h"
.INCLUDE	"render.h"
.INCLUDE	"math.h"
.INCLUDE	"controller.h"
; Mark the kernal to be ran after initialization
.EXPORT		post_reset := main





; Camera movement constants
CAMERA_ROT_SPEED	= $0100
CAMERA_MOV_SPEED	= $0008





.SEGMENT	"SAVERAM"
pal_buffer:				.RES 32
temp:					.RES 1





.CODE
.PROC main
	; Initialize nmi vector and rendering parameters
	LDA #<nmi_render
	STA soft_nmi_vector + 0
	LDA #>nmi_render
	STA soft_nmi_vector + 1

	LDA #PPU::MASK::RENDER_SP | PPU::MASK::RENDER_BG
	STA soft_ppumask
	LDA #PPU::CTRL::ENABLE_NMI | PPU::CTRL::BG_PATTERN_L | PPU::CTRL::SP_PATTERN_R
	STA soft_ppuctrl
	STA PPU::CTRL

	; Load initial palette
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

copy_polygon:
	LDX #$00
:	LDA test_poly, X
	STA display_list_polys, X
	INX
	CPX #test_poly_end - test_poly
	BNE :-
	LDA #<display_list_polys
	STA display_list_ptrs_lo + 0
	LDA #>display_list_polys
	STA display_list_ptrs_hi + 0


	LDA #$02
	STA temp

	JSR init_transfer_coroutine

	LDA #$00
.REPEAT 8, i
	STA .ident(.sprintf("pattern_buffer_%d", i))
.ENDREP

forever:
	LDX temp
	LDA buttons_held
	AND #BUTTON_UP
	BEQ :+
		DEC display_list_polys + 1, X
:	LDA buttons_held
	AND #BUTTON_DOWN
	BEQ :+
		INC display_list_polys + 1, X
:	LDA buttons_held
	AND #BUTTON_LEFT
	BEQ :+
		DEC display_list_polys + 0, X
:	LDA buttons_held
	AND #BUTTON_RIGHT
	BEQ :+
		INC display_list_polys + 0, X
:	LDA buttons_down
	AND #BUTTON_A
	BEQ :++
		LDA temp
		CLC
		ADC #$02
		CMP display_list_polys + 0
		BCC :+
			LDA #$02
	:	STA temp
:	LDA buttons_down
	AND #BUTTON_B
	BEQ :++
		LDA temp
		SEC
		SBC #$02
		CMP #$02
		BCS :+
			LDA display_list_polys + 0
			SBC #$00
	:	STA temp
:	LDA buttons_down
	AND #BUTTON_SELECT
	BEQ :+
		INC display_list_polys + 1
		LDA #$09
		CMP display_list_polys + 1
		BNE :+
			LDA #$00
			STA display_list_polys + 1
:	LDA buttons_down
	AND #BUTTON_START
	BEQ :+
		DEC display_list_polys + 1
		LDA #$FF
		CMP display_list_polys + 1
		BNE :+
			LDA #$08
			STA display_list_polys + 1
:

	; Setup poly pointer
	LDA #$01
	STA display_list_size

	JSR render_frame
	JSR read_controller
	JMP forever


	JSR math_init
main_loop:
	JSR read_controller
	JSR move_camera
	JSR render_frame
	JSR wait_for_nmi
	JMP main_loop
.ENDPROC

; Move camera based on controller state
;	Takes: Nothing
;	Returns: Nothing
;	Clobbers: A
.PROC	move_camera
	LDA #BUTTON_SELECT
	AND buttons_held
	BEQ @translate

; Face buttons and d-pad rotate camera when select is held
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

; Face buttons and d-pad translate camera when select is not held
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

test_poly:
.BYTE	$0B
.BYTE	$08
.BYTE	$80, $40
.BYTE	$C0, $80
.BYTE	$80, $C0
.BYTE	$40, $80
.BYTE	$20, $60
test_poly_end:

.RODATA
default_pal:
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20

.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20
.BYTE	$3F, $00, $10, $20