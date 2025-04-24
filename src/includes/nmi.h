.IFNDEF NMI_H
NMI_H = 1

; 6502 nmi vector
.GLOBAL		nmi

;
.GLOBALZP	soft_nmi_vector

; Indicate that a logical frame is done, and wait for the next nmi before returning
;	Takes: Nothing
;	Returns: Nothing
;	Clobbers: A
.GLOBAL		wait_for_nmi

; Clears OAM, putting all sprites offscreen
;	Takes: Nothing
;	Returns: Nothing
;	Clobbers: A, X
.GLOBAL		clear_oam

; Soft registers
.GLOBALZP	soft_ppuctrl, soft_ppumask, soft_scroll_x, soft_scroll_y

; Object Attribute Memory
.GLOBALZP	oam_index
.GLOBAL		oam

.ENDIF