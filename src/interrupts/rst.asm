; Reset handler
.INCLUDE	"rst.h"
.INCLUDE	"nes.h"
; Code to jump to after initialization. Should have a matching .EXPORT in another file
.IMPORT		post_reset



.SEGMENT	"RST_STUB"
.PROC	rst
	SEI							; Ignore IRQs
	LDA #$40
	STA $4017					; Disable APU IRQ
	LDX #$FF
	TXS							; Set up stack pointer
	INX							; X = 0
	STX PPU::CTRL				; Disable NMIs
	STX PPU::MASK				; Disable rendering
	STX $4010					; Disable DMC IRQs				

vblank_wait_1:
	BIT PPU::STATUS				; First wait for vblank
	BPL vblank_wait_1

clear_ram:
	LDA #$07					; Setup pointer to clear RAM. Clear page 7 first, working all the way down to page 0
	STA $00 + 1
	STX $00 + 0					; X = 0

	TXA							; A = X = 0
	TAY							; Y = A = 0
@loop:
	STA ($00), Y
	DEY
	BNE @loop
	DEC $00 + 1
	BPL @loop
	STA $00 + 1					; Hi byte of pointer is left as $FF, so needs to be cleared

vblank_wait_2:
	BIT PPU::STATUS				; Second wait for vblank, PPU is ready after this
	BPL vblank_wait_2

vblank_wait_3:
	BIT PPU::STATUS				; This shouldn't be necessary, but the rom is messing up in mesen without it /shrug
	BPL vblank_wait_3

	CLI							; Accept IRQs
	JMP post_reset
.ENDPROC