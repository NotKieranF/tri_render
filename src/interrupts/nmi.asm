; NMI handler
.INCLUDE	"nmi.h"
.INCLUDE	"nes.h"
.INCLUDE	"irq.h"
.INCLUDE	"render.h"
.INCLUDE	"vrc6.h"





.ZEROPAGE
soft_ppuctrl:		.RES 1	; Soft registers to be copied during vblank
soft_ppumask:		.RES 1
soft_scroll_x:		.RES 1
soft_scroll_y:		.RES 1
frame_done_flag:	.RES 1	; Indicates that a logical frame has been completed and graphics buffers are safe to empty
gfx_buffer_index:	.RES 1
oam_index:			.RES 1





.SEGMENT	"OAM"
oam:				.RES 256





.SEGMENT	"STACK"
gfx_buffer:			.RES 160





.CODE
; Actual NMI handler is mostly dummied out
.PROC	nmi
save_registers:
	PHA
	TXA
	PHA
	TYA
	PHA

clear_vblank_flag:
	BIT PPU::STATUS

;check_frame_done_flag:
;	LDA frame_done_flag
;	BEQ no_gfx_update

; hooby bleh
	LDA #$FF - 14
	STA VRC6::IRQ_LATCH
	LDA #%00000010
	STA VRC6::IRQ_CONTROL
	LDA #<shutdown_transfer_coroutine
	STA soft_irq_vector + 0
	LDA #>shutdown_transfer_coroutine
	STA soft_irq_vector + 1

	JSR startup_transfer_coroutine

write_registers:
	LDA soft_ppuctrl
	STA PPU::CTRL
	LDA soft_ppumask
	STA PPU::MASK
	LDA soft_scroll_x
	STA PPU::SCROLL
	LDA soft_scroll_y
	STA PPU::SCROLL
	LDA #>oam
	STA PPU::OAMDMA
	LDA #$00
	STA oam_index

;clear_frame_done_flag:
;	LDA #$00
;	STA frame_done_flag

; All timing sensitive updates have been performed, enable interrupts
no_gfx_update:
	CLI

restore_registers:
	PLA
	TAY
	PLA
	TAX
	PLA

	RTI
.ENDPROC

; Indicate that a logical frame is done, and wait for the next nmi before returning
;	Takes: Nothing
;	Returns: Nothing
;	Clobbers: A
.PROC	wait_for_nmi
	INC frame_done_flag
:	LDA frame_done_flag
	BNE :-
	RTS
.ENDPROC

; Clears OAM, putting all sprites offscreen
;	Takes: Nothing
;	Returns: Nothing
;	Clobbers: A, X
.PROC	clear_oam
	LDX #$3C
	LDA #$FF

:	STA oam + $00, X
	STA oam + $40, X
	STA oam + $80, X
	STA oam + $C0, X
	AXS #$04
	BPL :-

	RTS
.ENDPROC