; IRQ handler
.INCLUDE	"irq.h"
.INCLUDE	"vrc6.h"





.ZEROPAGE
soft_irq_vector:			.RES 2	; Points to the current irq handler





.CODE
; Acknowledge and service pending IRQ
.PROC   irq
	STA VRC6::IRQ_ACKNOWLEDGE
	JMP (soft_irq_vector)
.ENDPROC