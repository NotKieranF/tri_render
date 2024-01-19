.IFNDEF	IRQ_H
IRQ_H = 1

; 6502 irq vector
.GLOBAL		irq

; Software irq vector
.GLOBALZP	soft_irq_vector

.ENDIF