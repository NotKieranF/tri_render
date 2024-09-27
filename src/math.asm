.INCLUDE	"math.h"
; Should probably modify fast_mul to use the range -85..85 and return values in the same range. 
;	- Would be far more efficient for matrix and vector math that needs to remain in the range -1.0..1.0
;	- Would be 32% more precise than the current solution of using the range -64..64
;	- Would cut down zp usage by 50% if we accept the minor precision loss (~0.5% avg) of not computing the low byte, potentially making it feasible to keep object 
;	  transformation matrices in zp while rendering (72 bytes vs 36), saving ~198 cycles per vertex
;	- Maybe a good idea to reduce the range to something like -80..80 so that there's a buffer before things overflow
;		- Even better idea, have lookup values outside that index range clamp to the output range of -80..80

; Should be possible to perform matrix multiplication/dot products without using intermediate storage
;	- I.e. LDY $aa
;		   LDA (fast_mul_sq1), Y
;		   SEC
;		   SBC (fast_mul_sq2), Y
;		   LDY $bb
;		   CLC
;		   ADC (fast_mul_sq1), Y
;		   SEC
;		   SBC (fast_mul_sq2), Y
;		   ...
;	- Order really shouldn't even matter here, as the carry doesn't affect any of the lower order bits
;	- Would be nice to elide some of the SECs and CLCs as they take ~20% of the cycles, but that doesn't seem likely. (Maybe the errors between ADC and SBC cancel out?)
;	- Maybe do adds before subs, only setting or clearing the carry for the first operation

; Orthonormalization for orientation matrix:
;	- Based on: https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process but with the final step replaced with a cross product to avoid unecessary dot products and normalizations
;	- u' = u / |u|
;	- v' = v / |v| - dot(v / |v|, u') * u'
;	- w' = cross(u', w')

; Might make sense to have a few different lookup tables for fast_mul to speed up various operations
;	- Output range -85..85 for operations that need to stay within fast_mul ranges
;	- Output range -127..127 for operations that are increasing in precision and should be scaled between -1.0..1.0

; Temp stuff
.IF 0

; Sets up fastmul pointers for a 3x3 matrix in matrix_a
;	Takes: Matrix to setup in matrix
;	Returns: 
;	Clobbers: A, X, Y
.PROC	set_fast_matmul_3x3
	matrix				:= $00	; Through $08

	LDX #.SIZEOF(MATRIX) - 1
	LDY #(.SIZEOF(MATRIX) - 1) - 1

loop:
	LDA matrix, X
	STA fast_matmul_xx_sq1_ptr, Y
	EOR #$FF
	CLC
	ADC #$01
	STA fast_matmul_xx_sq2_ptr, Y

	DEX
	DEY
	DEY
	BPL loop

	RTS
.ENDPROC

; Multiplies matrix in matrix_a by matri
;	Takes: Left hand matrix in matrix_a
;	Returns: Matrix product in matrix_a
;	Clobbers: A, X, Y
.PROC	fast_matmul_3x3_3x3_signed

.ENDPROC

; Compute the matrix mul
.PROC	fast_matmul_3x3_3x1_signed
	LDY input_x
	LDA (fast_matmul_xx_sq1_ptr), Y
	SEC
	SBC (fast_matmul_xx_sq2_ptr), Y

	LDY input_y
	CLC
	ADC (fast_matmul_xy_sq1_ptr), Y
	SEC
	SBC (fast_matmul_xy_sq2_ptr), Y

	LDY input_z
	CLC
	ADC (fast_matmul_xz_sq1_ptr), Y
	SEC
	SBC (fast_matmul_xz_sq2_ptr), Y
	STA output_x

	LDY input_x
	LDA (fast_matmul_yx_sq1_ptr), Y
	SEC
	SBC (fast_matmul_yx_sq2_ptr), Y

	LDY input_y
	CLC
	ADC (fast_matmul_yy_sq1_ptr), Y
	SEC
	SBC (fast_matmul_yy_sq2_ptr), Y

	LDY input_z
	CLC
	ADC (fast_matmul_yz_sq1_ptr), Y
	SEC
	SBC (fast_matmul_yz_sq2_ptr), Y
	STA output_y

	LDY input_x
	LDA (fast_matmul_zx_sq1_ptr), Y
	SEC
	SBC (fast_matmul_zx_sq2_ptr), Y

	LDY input_y
	CLC
	ADC (fast_matmul_zy_sq1_ptr), Y
	SEC
	SBC (fast_matmul_zy_sq2_ptr), Y

	LDY input_z
	CLC
	ADC (fast_matmul_zz_sq1_ptr), Y
	SEC
	SBC (fast_matmul_zz_sq2_ptr), Y
	STA output_z

	RTS
.ENDPROC

.ENDIF


.ZEROPAGE
; Cached 3x3 matrix multiply pointers
fast_matmul_xx_sq1_ptr:		.RES 2
fast_matmul_xx_sq2_ptr:		.RES 2
fast_matmul_xy_sq1_ptr:		.RES 2
fast_matmul_xy_sq2_ptr:		.RES 2
fast_matmul_xz_sq1_ptr:		.RES 2
fast_matmul_xz_sq2_ptr:		.RES 2
fast_matmul_yx_sq1_ptr:		.RES 2
fast_matmul_yx_sq2_ptr:		.RES 2
fast_matmul_yy_sq1_ptr:		.RES 2
fast_matmul_yy_sq2_ptr:		.RES 2
fast_matmul_yz_sq1_ptr:		.RES 2
fast_matmul_yz_sq2_ptr:		.RES 2
fast_matmul_zx_sq1_ptr:		.RES 2
fast_matmul_zx_sq2_ptr:		.RES 2
fast_matmul_zy_sq1_ptr:		.RES 2
fast_matmul_zy_sq2_ptr:		.RES 2
fast_matmul_zz_sq1_ptr:		.RES 2
fast_matmul_zz_sq2_ptr:		.RES 2

fast_mul_sq1_lo_ptr:		.RES 2
fast_mul_sq1_hi_ptr:		.RES 2
fast_mul_sq2_lo_ptr:		.RES 2
fast_mul_sq2_hi_ptr:		.RES 2

mul_sq1_lo_ptr_0:			.RES 2
mul_sq1_lo_ptr_1:			.RES 2
mul_sq1_lo_ptr_2:			.RES 2
mul_sq1_lo_ptr_3:			.RES 2

mul_sq1_hi_ptr_0:			.RES 2
mul_sq1_hi_ptr_1:			.RES 2
mul_sq1_hi_ptr_2:			.RES 2
mul_sq1_hi_ptr_3:			.RES 2

mul_sq2_lo_ptr_0:			.RES 2
mul_sq2_lo_ptr_1:			.RES 2
mul_sq2_lo_ptr_2:			.RES 2
mul_sq2_lo_ptr_3:			.RES 2

mul_sq2_hi_ptr_0:			.RES 2
mul_sq2_hi_ptr_1:			.RES 2
mul_sq2_hi_ptr_2:			.RES 2
mul_sq2_hi_ptr_3:			.RES 2



.BSS
multiplier:					.RES 2
multiplicand:				.RES 2
product:					.RES 4
quotient:					.RES 1
divisor:					.RES 1










.CODE
;
.PROC	math_init
init_mul:
	LDX #$08
:	DEX
	LDA #>square_1_lo
	STA mul_sq1_lo_ptr_0, X
	LDA #>square_1_hi
	STA mul_sq1_hi_ptr_0, X
	LDA #>square_2_lo
	STA mul_sq2_lo_ptr_0, X
	LDA #>square_2_hi
	STA mul_sq2_hi_ptr_0, X
	DEX
	BNE :-

init_fast_mul:
	LDA #>fast_square_lo
	STA fast_mul_sq1_lo_ptr + 1
	STA fast_mul_sq2_lo_ptr + 1
	LDA #>fast_square_hi
	STA fast_mul_sq1_hi_ptr + 1
	STA fast_mul_sq2_hi_ptr + 1

	RTS
.ENDPROC

; Setup the multiplier for a 16x8-bit signed multiplication
;	Takes: Signed 16-bit multiplier in AY
;	Returns: Nothing
;	Clobbers: A, Y
.PROC	set_mul_16x8bit_signed
	STA multiplier + 1
	STA mul_sq1_lo_ptr_1 + 0
	STA mul_sq1_hi_ptr_1 + 0
	EOR #$FF
	STA mul_sq2_lo_ptr_1 + 0
	STA mul_sq2_hi_ptr_1 + 0

	TYA
	STA multiplier + 0
	STA mul_sq1_lo_ptr_0 + 0
	STA mul_sq1_hi_ptr_0 + 0
	EOR #$FF
	STA mul_sq2_lo_ptr_0 + 0
	STA mul_sq2_hi_ptr_0 + 0

	RTS
.ENDPROC

; Performs a signed 16x8-bit signed multiplication based on a multiplicand in Y and a multiplier that has been set up by set_mul_16x8bit_signed previously
;	Takes: 8-bit signed multiplicand in Y
;	Returns: 24-bit signed product in AYX
;	Clobbers: A, X, Y
;	~113 cycles on avg
.PROC	mul_16x8bit_signed
	STY multiplicand + 0

	LDA (mul_sq1_lo_ptr_0), Y
	SEC
	SBC (mul_sq2_lo_ptr_0), Y
	TAX

	LDA (mul_sq1_hi_ptr_0), Y
	SBC (mul_sq2_hi_ptr_0), Y
	STA product + 1

	LDA (mul_sq1_lo_ptr_1), Y
	SEC
	SBC (mul_sq2_lo_ptr_1), Y
	STA product + 0

	LDA (mul_sq1_hi_ptr_1), Y
	SBC (mul_sq2_hi_ptr_1), Y
	TAY

	LDA product + 0
	CLC
	ADC product + 1
	BCC :+
		INY
	:

	BIT multiplicand + 0
	BPL :+
		SEC
		SBC multiplier + 0
		STA product + 1
		TYA
		SBC multiplier + 1
		JMP :++
	:
	STA product + 1
	TYA

	:
	BIT multiplier + 1
	BPL :+
		SEC
		SBC multiplicand + 0
	:

	LDY product + 1

	RTS
.ENDPROC

; Setup the multiplier for a 16x16-bit unsigned multiplication
;	Takes: 16-bit unsigned multiplier in YA
;	Returns: Nothing
;	Clobbers: A, Y
.PROC	set_mul_16x16bit_unsigned_hi16
	STA mul_sq1_hi_ptr_1 + 0
	EOR #$FF
	STA mul_sq2_hi_ptr_1 + 0

	TYA
	STA mul_sq1_hi_ptr_0 + 0
	STA mul_sq1_hi_ptr_2 + 0
	STA mul_sq1_lo_ptr_0 + 0
	EOR #$FF
	STA mul_sq2_hi_ptr_0 + 0
	STA mul_sq2_hi_ptr_2 + 0
	STA mul_sq2_lo_ptr_0 + 0

	RTS
.ENDPROC

; 140 total
;	a.b * c.d = a * c + a * d / 256 + b * c / 256
;
;	Takes: 16-bit unsigned multiplicand in AY
;	Returns: 16-bit unsigned product in YA
;	Clobbers: A, X, Y
.PROC	mul_16x16bit_unsigned_hi16
	STA multiplicand + 1

	; b * c / 256
	LDA (mul_sq1_hi_ptr_0), Y
	SEC
	SBC (mul_sq2_hi_ptr_0), Y
	STA product + 0

	; a * d / 256
	LDY multiplicand + 1
	LDA (mul_sq1_hi_ptr_1), Y
	SEC
	SBC (mul_sq2_hi_ptr_1), Y
	STA product + 1

	; a * c
	LDA (mul_sq1_lo_ptr_0), Y
	SEC
	SBC (mul_sq2_lo_ptr_0), Y
	TAX

	LDA (mul_sq1_hi_ptr_2), Y
	SBC (mul_sq2_hi_ptr_2), Y
	TAY

	TXA
	CLC
	ADC product + 0
	BCC :+
		INY
		CLC
:	ADC product + 1
	BCC :+
		INY
:	RTS
.ENDPROC


; Gets the unsigned 0.16-bit fixed point reciprocal of a given unsigned 16.0-bit fixed number
;	Takes: 16-bit unsigned input in XA
;	Returns: 16-bit unsigned reciprocal in YA
;	Clobbers: A, X, Y, $1F
;	~17 cycles just right case
;	~47 cycles too large best case
;	~161 cycles too large worst case
;	~44 cycles too small best case
;	~177 cycles too small worst case
.PROC	reciprocal_16bit_unsigned
	input_hi	:= $1F
	output_hi	:= $1F

; Hi byte of input must be $01 for reciprocal table
check_hi_byte:
	LDY #$00
	CPX #$01
	BEQ just_right
	BCS too_large
	CMP #$00
	BEQ zero

; If the hi byte is smaller than $01, the input must be shifted left until the hi byte is $01
; Then the resulting value from the table must be shifted left just as many times
too_small:
	STY output_hi

:	INY
	ASL
	BCC :-

	TAX
	LDA reciprocal_lo, X

:	ASL
	ROL output_hi
	DEY
	BNE :-

	LDY output_hi

	RTS

; If the hi byte is larger than $01, the input must be shifted right until the hi byte is $01
; Then the resulting value from the table must be shifted right just as many times
too_large:
	STX input_hi
	LSR input_hi

:	INY
	ROR
	LSR input_hi			; Hi byte is shifted until it's $00, but lo byte isn't actually shifted during the final iteration
	BNE :-

	TAX
	LDA reciprocal_lo, X

:	LSR
	DEY
	BNE :-

	LDY #$00

	RTS

; If both lo byte and hi byte are $00, then return a reciprocal of $FFFF
zero:
	LDA #$FF
	DEY
	RTS

just_right:
	TAX
	LDA reciprocal_lo, X

	RTS
.ENDPROC

;	Takes: Unsigned 8-bit multiplier in A
;	Returns: Nothing
;	Clobbers: A
.PROC	set_mul_8x16bit_signed_hi16
		STA mul_sq1_lo_ptr_0 + 0
		STA mul_sq1_hi_ptr_0 + 0
		EOR #$FF
		STA mul_sq2_lo_ptr_0 + 0
		STA mul_sq2_hi_ptr_0 + 0

		RTS
.ENDPROC

;	Takes: Signed 16-bit multiplicand in AY
;	Returns: Hi 16 bits of signed 24-bit product in YA
;	Clobbers: A, X, Y
.PROC	mul_8x16bit_signed_hi16
	STA multiplicand + 1

	LDA (mul_sq1_hi_ptr_0), Y
	SEC
	SBC (mul_sq2_hi_ptr_0), Y
	TAX

	LDY multiplicand + 1

	LDA (mul_sq1_lo_ptr_0), Y
	SEC
	SBC (mul_sq2_lo_ptr_0), Y
	STA product + 0

	LDA (mul_sq1_hi_ptr_0), Y
	SBC (mul_sq2_hi_ptr_0), Y

	BIT multiplicand + 1
	BPL :+
		SEC
		SBC mul_sq1_lo_ptr_0 + 0
:	STA product + 1

	TXA
	CLC
	ADC product + 0
	BCC :+
		INC product + 1
:	STA product + 0

	LDA product + 0
	LDY product + 1

	RTS
.ENDPROC

; Computes A / X
;	Takes: 7-bit unsigned dividend in A, 7-bit unsigned divisor in X
;	Returns: 7-bit unsigned fractional quotient in X, 8-bit unsigned remainder in A
;	Clobbers: A, X, $1E - $1F
.PROC	div_7x7bit_unsigned_fractional
	STX divisor
	LDX #%00000010		; Initialize quotient as a ring counter
	STX quotient
@loop:
	ASL
	CMP divisor
	BCC :+
	SBC divisor
:	ROL quotient
	BCC @loop
	LDX quotient

	RTS
.ENDPROC

; Computes A / X where A < X
;	Takes: 8-bit unsigned dividend in A, 8-bit unsigned divisor in X
;	Returns: 8-bit unsigned fractional quotient in X, 8-bit unsigned remainder in A
;	Clobbers: A, X, $1E - $1F
.PROC	udiv_8x8bit_frac
	STX divisor
	LDX #%00000001		; Initialize quotient as a ring counter
	STX quotient
@loop:
	ASL
	BCS :+
	CMP divisor
	BCC :++
:	SBC divisor
	SEC
:	ROL quotient
	BCC @loop
	LDX quotient

	RTS
.ENDPROC






.RODATA
.ALIGN	256
; Sine and cosine tables at various amplitudes
.INCLUDE	"trig_tables.inc"

; And identity table, useful for doing inter-register operations
identity:
.REPEAT	256, i
	.BYTE	i
.ENDREP

;
.ALIGN	256
square_1_lo:
.REPEAT	512, i
	.LOBYTES	(i * i) / 4
.ENDREP
square_1_hi:
.REPEAT	512, i
	.HIBYTES	(i * i) / 4
.ENDREP

;
square_2_lo:
.REPEAT	512, i
	.LOBYTES	((i - 255) * (i - 255)) / 4
.ENDREP
square_2_hi:
.REPEAT	512, i
	.HIBYTES	((i - 255) * (i - 255)) / 4
.ENDREP

; Square tables for fastmul
fast_square_lo:
.REPEAT 160, i
	.LOBYTES	(i * i); / 4
.ENDREP
.REPEAT	192, i
	.LOBYTES	((i - 96) * (i - 96)); / 4
.ENDREP
.REPEAT 160, i
	.LOBYTES	((i - 160) * (i - 160)); / 4
.ENDREP
fast_square_hi:
.REPEAT 160, i
	.HIBYTES	(i * i); / 4
.ENDREP
.REPEAT	192, i
	.HIBYTES	((i - 96) * (i - 96)); / 4
.ENDREP
.REPEAT 160, i
	.HIBYTES	((i - 160) * (i - 160)); / 4
.ENDREP

;
reciprocal_lo:
.REPEAT	256, i
	.LOBYTES	65535 / (i + 256)
.ENDREP

; 16-bit signed reciprocal for all 8-bit signed numbers
reciprocal_16bit_signed_lo:
.REPEAT	256, i
	.IF .NOT(i - 128 = 0)
		.LOBYTES	32767 / (i - 128)
	.ELSE
		.LOBYTES	32767
	.ENDIF
.ENDREP

; 16-bit signed reciprocal for all 8-bit signed numbers
reciprocal_16bit_signed_hi:
.REPEAT	256, i
	.IF .NOT(i - 128 = 0)
		.HIBYTES	32767 / (i - 128)
	.ELSE
		.HIBYTES	32767
	.ENDIF
.ENDREP


; Ideas:
; Logarithm based multiplication/division
; Probably slower than just taking the reciprocal and multiplying
; Same principle as reciprocal, just thrice