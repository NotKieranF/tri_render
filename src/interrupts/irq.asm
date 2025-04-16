; IRQ handler
.INCLUDE	"irq.h"
.INCLUDE	"nes.h"
.INCLUDE	"nmi.h"
.INCLUDE	"vrc6.h"





.ZEROPAGE
soft_irq_vector:			.RES 2	; Points to the current irq handler





.CODE
.PROC   irq
	STA VRC6::IRQ_ACKNOWLEDGE
	JMP (soft_irq_vector)
.ENDPROC