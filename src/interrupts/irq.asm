; IRQ handler
.INCLUDE	"irq.h"
.INCLUDE	"nes.h"



.ZEROPAGE
soft_irq_vector:		.RES 2	; Points to the current irq handler



.CODE
.PROC	irq
:	JMP :-

;	JMP (soft_irq_vector)
.ENDPROC