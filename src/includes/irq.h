.IFNDEF	IRQ_H
IRQ_H = 1

; 6502 irq vector
.GLOBAL		irq

; Software irq vector
.GLOBALZP	soft_irq_vector

; Bleh
.GLOBAL		irq_dummy, irq_shutdown_coroutine, irq_begin_extended_vblank, irq_end_extended_vblank

.ENDIF