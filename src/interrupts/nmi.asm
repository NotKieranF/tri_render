; NMI handler
.INCLUDE	"nmi.h"
.INCLUDE	"nes.h"
.INCLUDE	"irq.h"
.INCLUDE	"render.h"
.INCLUDE	"vrc6.h"





.ZEROPAGE
soft_nmi_vector:	.RES 2
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
	JMP (soft_nmi_vector)
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