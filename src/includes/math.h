.IFNDEF	MATH_H
MATH_H = 1

; Setup multiplier for fast mul inline
; Takes multiplier in A
.MACRO	SET_FAST_MUL
	STA fast_mul_sq1_lo_ptr + 0
	STA fast_mul_sq1_hi_ptr + 0
	EOR #$FF
	CLC
	ADC #$01
	STA fast_mul_sq2_lo_ptr + 0
	STA fast_mul_sq2_hi_ptr + 0
.ENDMAC

; Perform fast mul inline
; Takes multiplicand in Y
; Returns hi byte of product in A
; Stores lo byte of product at {product_lo} if specified, otherwise returns it in X
.MACRO	FAST_MUL product_lo
	LDA (fast_mul_sq1_lo_ptr), Y
	SEC
	SBC (fast_mul_sq2_lo_ptr), Y
	.IF .BLANK(product_lo)
		TAX
	.ELSE
		STA product_lo
	.ENDIF

	LDA (fast_mul_sq1_hi_ptr), Y
	SBC (fast_mul_sq2_hi_ptr), Y
.ENDMAC

; Perform fast mul inline and return only the hi bits
;	Takes: Signed multiplicand in Y
;	Returns: Hi byte of product in A
;	Clobbers: A
.MACRO	FAST_MUL_HI
	LDA (fast_mul_sq1_hi_ptr), Y
	SEC
	SBC (fast_mul_sq2_hi_ptr), Y
.ENDMAC

; Fastmul pointers. Shouldn't be touched directly, only exported for inlining purposes
.GLOBALZP	fast_mul_sq1_lo_ptr, fast_mul_sq1_hi_ptr, fast_mul_sq2_lo_ptr, fast_mul_sq2_hi_ptr
.GLOBALZP	mul_sq1_lo_ptr_0, mul_sq1_hi_ptr_0, mul_sq2_lo_ptr_0, mul_sq2_hi_ptr_0

; Multiplication routines
.GLOBAL		set_mul_16x8bit_signed, mul_16x8bit_signed, set_mul_16x16bit_unsigned_hi16, mul_16x16bit_unsigned_hi16, set_mul_8x16bit_signed_hi16, mul_8x16bit_signed_hi16

; Division routines
.GLOBAL		udiv_8x8bit_frac

; Reciprocal routine
.GLOBAL		reciprocal_16bit_unsigned

; Initializes various routines
.GLOBAL		math_init

; Scaled trig tables (whole, half, quarter, eighth)
.GLOBAL		sin, hsin, qsin, esin, cos, hcos, qcos, ecos

; Identity table, useful for inter-register operations
.GLOBAL		identity

; 16-bit signed reciprocal for all 8-bit signed numbers
.GLOBAL		reciprocal_16bit_signed_lo, reciprocal_16bit_signed_hi

; Lo 8 bits of 65536 / (i + 256)
.GLOBAL		reciprocal_lo

.ENDIF