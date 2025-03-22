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





.CODE
.PROC main
	; Disable rendering, enable NMIs
	LDA #PPU::DISABLE_RENDERING
	STA soft_ppumask
	LDA #PPU::DEFAULT_CTRL
	STA soft_ppuctrl
	STA PPU::CTRL
	JSR wait_for_nmi

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

	; Load initial tile data
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

	; Enable sprite rendering
	LDA #PPU::RENDER_SP
	STA soft_ppumask

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

balz:
.INCBIN	"balz.chr"