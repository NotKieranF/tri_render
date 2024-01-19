.INCLUDE	"float.h"

; Adapted from https://codebase64.org/doku.php?id=base:floating_point_routines_for_the_6502


.ZEROPAGE
float_1:			.TAG FLOAT
float_2:			.TAG FLOAT



.CODE

.PROC	fadd_16
	addend		:= float_1
	augend		:= float_2
	sum			:= float_1

; Check if signs are different
	LDA addend + FLOAT_16::EXPONENT
	SEC
	SBC augend + FLOAT_16::EXPONENT
	BEQ exponents_equal
	TAX
	BCC 

addend_greater:
	CPX #.SIZEOF(FLOAT_16::MANTISSA) * 8
	BCS return_addend

	LDA augend + FLOAT_16::MANTISSA
:	CMP #$80
	ROR
	DEX
	BNE :-
	STA augend + FLOAT_16::MANTISSA
	BEQ exponents_equal

augend_greater:
	CPX #<-.SIZEOF(FLOAT_16::MANTISSA) * 8
	BCC return_augend

	LDA addend + FLOAT_16::MANTISSA
:	CMP #$80
	ROR
	INX
	BNE :-
	STA addend + FLOAT_16::MANTISSA

exponents_equal:
	LDA addend + FLOAT_16::MANTISSA
	CLC
	ADC augend + FLOAT_16::MANTISSA
	BVC normalize
	ROR
	INC sum + FLOAT_16::EXPONENT

normalize:
	STA sum + FLOAT_16::MANTISSA
	DEC sum + FLOAT_16::EXPONENT
	ASL
	EOR sum + FLOAT_16::MANTISSA
	BMI normalize
	INC sum + FLOAT_16::EXPONENT

exit:
	RTS



;	fadd_16 () {
;		expdif = addend.exp - augend.exp
;		if (expdif != 0) {
;			
;		}
;			
;
;
;
;	}



	LDA addend + FLOAT_16::SIGN
	EOR augend + FLOAT_16::SIGN
	BEQ @same_sign

@different_signs:
	LDA addend + FLOAT_16::MANTISSA
	BMI :+
	JSR fswap_16
:	LDA augend + FLOAT_16::MANTISSA
	AND #%01111111
	STA augend + FLOAT_16::MANTISSA
	JMP fsub_16

@same_sign:
	LDA addend + FLOAT_16::EXPONENT
	SEC
	SBC augend + FLOAT_16::EXPONENT
	TAX
	BEQ operands_equal
	BCC

addend_greater:
	CPX #$08
	BCS exit
	LDA augend + FLOAT_16::MANTISSA
@loop:
	ASL
	DEX
	BNE @loop
	BEQ operands_equal

augend_greater:
	CPX #<-$08
	BCC fswap_16

operands_equal:
	LDA addend + FLOAT_16::MANTISSA
	CLC
	ADC augend + FLOAT_16::MANTISSA
	BVC :+
	ROR
:	STA sum + FLOAT_16::MANTISSA

exit:
	RTS
.ENDPROC

;
.PROC	fsub_16
	JSR fcomp_16
	JMP fadd_16
.ENDPROC

.PROC	fdiv_16
	JSR frecip_16
.ENDPROC

;
;	fmul_16 () {
;		product.exp = multiplier.exp + multiplicand.exp
;
;		product.mant = multiplier.mant * multiplicand.mant
;		if (abs(product.mant) > 2) {
;			product.exp += 1
;			product.mant /= 2
;		}
;
;	}
.PROC	fmul_16
	multiplier		:= float_1
	multiplicand	:= float_2
	product			:= float_1

	LDA multiplier + FLOAT_16::EXPONENT
	CLC
	ADC multiplicand + FLOAT_16::EXPONENT
	STA product + FLOAT_16::EXPONENT

	LDA multiplier + FLOAT_16::MANTISSA
	STA $00
	STA $00
	EOR #$FF
	STA $00
	STA $00

	LDY multiplicand + FLOAT_16::MANTISSA
	LDA ($00), Y
	SEC
	SBC ($00), Y
	TAX

	LDA ($00), Y
	SBC ($00), Y
	BIT multiplier + FLOAT_16::MANTISSA
	BPL :+
		SEC
		SBC multiplicand + FLOAT_16::MANTISSA
:	BIT multiplicand + FLOAT_16::MANTISSA
	BPL :+
		SEC
		SBC multiplier + FLOAT_16::MANTISSA
:	STA product + FLOAT_16::MANTISSA
	CPX #$80
	ROL
	EOR product + FLOAT_16::MANTISSA
	BPL :+
		INC product + FLOAT_16::EXPONENT
		BEQ overflow
		ROR
:	STA product + FLOAT_16::MANTISSA

	RTS

overflow:
	BRK
.ENDPROC



;	float fadd_16 (float addend, float augend) {
;		if (addend.sign == 0 && augend.sign == 1) {
;			augend.sign = 0
;			return fsub_16(addend, augend)
;		}
;
;		if (addend.sign == 1 && augend.sign == 0) {
;			fswap_16()
;			augend.sign = 0
;			return fsub_16(addend, augend)
;		}
;
;		expdiff = addend.exp - augend.exp
;		if (expdiff > 0) {
;			if (expdiff > 8)
;				return addend
;			for (int i = 0; i < expdiff; i++)
;				augend >>= 1
;		}
;
;		if (expdiff < 0) {
;			if (expdiff < -8)
;				return augend
;			for (int i = 0; i > expdiff; i--)
;				addend >>= 1
;		}
;
;		sum.mant = addend.mant + augend.mant
;		
;	}
;


;	Sign/mag		2's compliment
;	(1)10000000 -> (1)10000000
;	(1)10000001 -> (1)01111111
;	(1)11111111 -> (1)00000001