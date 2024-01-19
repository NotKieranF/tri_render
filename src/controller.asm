.INCLUDE	"controller.h"
.INCLUDE	"nes.h"



.ZEROPAGE
buttons_held:		.RES 1
buttons_down:		.RES 1
buttons_up:			.RES 1



.CODE
.PROC	read_controller
	buttons_held_new	:= $00

strobe_controllers:
	LDA #$01
	STA APU::PORT_1				; Initiate strobe
	STA buttons_held_new		; Initialize buttons_held_new as a ring counter
	LSR							; A = 0
	STA APU::PORT_1				; Halt strobe

read_loop:
	LDA APU::PORT_1
	LSR
	ROL buttons_held_new
	BCC read_loop

update_controller_state:
	LDA buttons_held			; buttons_down = ~buttons_held & buttons_held_new; i.e. rising edge
	EOR #$FF
	AND buttons_held_new
	STA buttons_down

	LDA buttons_held_new		; buttons_up = ~buttons_held_new & buttons_held; i.e. falling edge
	EOR #$FF
	AND buttons_held
	STA buttons_up

	LDA buttons_held_new		; Copy new button state over previous frame's state
	STA buttons_held

	RTS
.ENDPROC