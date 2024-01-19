.INCLUDE	"rst.h"
.INCLUDE	"nmi.h"
.INCLUDE	"irq.h"



.SEGMENT "VECTORS"
.WORD	nmi
.WORD	rst
.WORD	irq