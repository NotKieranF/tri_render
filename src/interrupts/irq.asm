; IRQ handler
.INCLUDE	"irq.h"
.INCLUDE	"nes.h"
.INCLUDE	"nmi.h"





.ZEROPAGE
soft_irq_vector:			.RES 2	; Points to the current irq handler
dmc_irq_phase:				.RES 1	; Measured phase within current DMC period
vblank_time:				.RES 2	; Cycles remaining in extended vblank
; Transfer thread state
transfer_thread_a:			.RES 1
transfer_thread_x:			.RES 1
transfer_thread_y:			.RES 1
transfer_thread_pc:			.RES 2
transfer_thread_ps:			.RES 1
transfer_thread_ppu_ctrl:	.RES 1
transfer_thread_ppu_addr:	.RES 2





.CODE
.PROC   irq
	JMP (soft_irq_vector)
.ENDPROC

;
.PROC   irq_measure_phase
save_registers:
	PHA

	; Acknowledge incoming DMC IRQ
acknowledge_irq:
	LDA #$00
	STA APU::DMC::CTRL

	; Record current phase
	; X register contains cycle count in multiples of five
record_phase:
	STX dmc_irq_phase

restore_registers:
	PLA

	RTI
.ENDPROC

;
.PROC	irq_begin_extended_vblank
save_registers:
	PHA
	TXA
	PHA
	TYA
	PHA

delay:

begin_extended_vblank:
	LDA #$00
	STA PPU::MASK

	; Bleh
	LDA #<irq_shutdown_transfer_thread
	STA soft_irq_vector + 0
	LDA #>irq_shutdown_transfer_thread
	STA soft_irq_vector + 1

	; Restore transfer thread state
restore_transfer_thread:
	LDA transfer_thread_ppu_addr + 0
	STA PPU::ADDR
	LDA transfer_thread_ppu_addr + 1
	STA PPU::ADDR
	LDA transfer_thread_ppu_ctrl
	STA PPU::CTRL

	LDA transfer_thread_ps
	PHA
	LDA transfer_thread_pc + 0
	PHA
	LDA transfer_thread_pc + 1
	PHA

	LDA transfer_thread_a
	LDX transfer_thread_x
	LDY transfer_thread_y

	RTI
.ENDPROC

;
.PROC	irq_shutdown_transfer_thread
save_transfer_thread:
	STA transfer_thread_a
	STX transfer_thread_x
	STY transfer_thread_y

	PLA
	STA transfer_thread_ps
	PLA
	STA transfer_thread_pc + 0
	PLA
	STA transfer_thread_pc + 1

	; Bleh
	LDA #<irq_end_extended_vblank
	STA soft_irq_vector + 0
	LDA #>irq_end_extended_vblank
	STA soft_irq_vector + 1

restore_registers:
	PLA
	TAY
	PLA
	TAX
	PLA

	RTI
.ENDPROC

;
.PROC	irq_end_extended_vblank
save_registers:
	PHA
	TXA
	PHA
	TYA
	PHA

update_ppu_registers:
	LDA soft_ppuctrl
	STA PPU::CTRL
	LDA soft_scroll_x
	STA PPU::SCROLL
	LDA soft_scroll_y
	STA PPU::SCROLL

delay:

end_extended_vblank:
	LDA soft_ppumask
	STA PPU::MASK

	; Bleh
	LDA #<irq_begin_extended_vblank
	STA soft_irq_vector + 0
	LDA #>irq_begin_extended_vblank
	STA soft_irq_vector + 1

restore_registers:
	PLA
	TAY
	PLA
	TAX
	PLA

	RTI
.ENDPROC