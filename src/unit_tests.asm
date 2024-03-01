.IFDEF __UNIT_TEST__
;	multiplicand := $10
;	multiplier := $11
;	product := $13
;
;test_mul_16x8bit_signed:
;	JSR math_init
;	LDA #$00
;	STA multiplicand + 0
;	STA multiplier + 0
;	STA multiplier + 1
;@outer_loop:
;	LDA multiplier + 0
;	LDX multiplier + 1
;	JSR set_mul_16x8bit_signed
;@inner_loop:
;	LDY multiplicand
;	JSR mul_16x8bit_signed
;	STA product + 2
;	STY product + 1
;	STX product + 0
;	STA $4444
;	INC multiplicand
;	BNE @inner_loop
;
;	INC multiplier + 0
;	BNE @outer_loop
;	INC multiplier + 1
;	BNE @outer_loop


;	multiplicand := $10
;	multiplier := $11
;	product := $13
;
;test_fastmul:
;	JSR math_init
;	LDA #$C0
;	STA multiplicand + 0
;	STA multiplier + 0
;@outer_loop:
;	LDA multiplier + 0
;	SET_FAST_MUL
;@inner_loop:
;	LDY multiplicand
;	FAST_MUL product + 0
;	STA product + 1
;	STA $4444
;	INC multiplicand
;	LDA #$40
;	CMP multiplicand
;	BNE @inner_loop
;	LDA #$C0
;	STA multiplicand
;
;	INC multiplier
;	LDA #$40
;	CMP multiplier
;	BNE @outer_loop

;	reciprocal_input := $10
;	reciprocal_output := $12
;
;test_reciprocal:
;	JSR math_init
;	LDA #$00
;	STA reciprocal_input + 0
;	STA reciprocal_input + 1
;@loop:
;	LDA reciprocal_input + 0
;	LDX reciprocal_input + 1
;	JSR reciprocal_16bit_unsigned
;	STA reciprocal_output + 0
;	STY reciprocal_output + 1
;	STA $4444
;
;	INC reciprocal_input + 0
;	BNE @loop
;
;	INC reciprocal_input + 1
;	BNE @loop

;	multiplier		:= $10
;	multiplicand	:= $12
;	product			:= $14
;
;test_mul_16x16bit_unsigned_hi:
;	JSR math_init
;	LDA #$00
;	STA multiplier + 0
;	STA multiplier + 1
;	STA multiplicand + 0
;	STA multiplicand + 1
;@outer_loop:
;	LDY multiplier + 0
;	LDA multiplier + 1
;	JSR set_mul_16x16bit_unsigned_hi16
;@inner_loop:
;	LDY multiplicand + 0
;	LDA multiplicand + 1
;	JSR mul_16x16bit_unsigned_hi16
;	STA product + 0
;	STY product + 1
;	STA $4444
;
;	INC multiplicand + 0
;	BNE @inner_loop
;	INC multiplicand + 1
;	BNE @inner_loop
;
;	LDA #<1013
;	CLC
;	ADC multiplier + 0
;	STA multiplier + 0
;	LDA #>1013
;	ADC multiplier + 1
;	STA multiplier + 1
;
;	LDA multiplier + 0
;	BNE @outer_loop
;	LDA multiplier + 1
;	BNE @outer_loop

;	multiplier		:= $10
;	multiplicand	:= $11
;	product			:= $13
;
;test_mul_8x16bit_signed_hi16:
;	JSR math_init
;	LDA #$00
;	STA multiplier
;	STA multiplicand + 0
;	STA multiplicand + 1
;@outer_loop:
;	LDA multiplier
;	JSR set_mul_8x16bit_signed_hi16
;@inner_loop:
;	LDY multiplicand + 0
;	LDA multiplicand + 1
;	JSR mul_8x16bit_signed_hi16
;	STA product + 0
;	STY product + 1
;	STA $4444
;
;	INC multiplicand + 0
;	BNE @inner_loop
;	INC multiplicand + 1
;	BNE @inner_loop
;
;	INC multiplier
;	BNE @outer_loop

;	dividend	:= $10
;	divisor		:= $11
;	quotient	:= $12
;test_div_7x7bit_unsigned_fractional:
;	LDA #$01
;	STA divisor
;	LDA #$00
;	STA dividend
;@loop:
;	LDA dividend
;	LDX divisor
;	JSR div_7x7bit_unsigned_fractional
;	STX quotient
;	STX $4444
;
;	INC dividend
;	LDA dividend
;	CMP divisor
;	BNE @loop
;
;	LDA #$00
;	STA dividend
;	INC divisor
;	LDA divisor
;	CMP #$80
;	BNE @loop
.ENDIF