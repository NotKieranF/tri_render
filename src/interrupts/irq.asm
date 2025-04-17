; IRQ handler
.INCLUDE	"irq.h"





.ZEROPAGE
soft_irq_vector:			.RES 2	; Points to the current irq handler





.CODE
; Acknowledge and service pending IRQ
.PROC   irq
	JMP (soft_irq_vector)
.ENDPROC