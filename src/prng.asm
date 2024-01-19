; Kieran Firkin
; Various Linear Congruential Generator routines for producing pseudorandom numbers. (https://en.wikipedia.org/wiki/Linear_congruential_generator)
; These happen to be fast, but only the highest order bit of the output achieves the full period, and the multiplier of 257 is suboptimal.
.INCLUDE	"prng.h"
increment = 1



.ZEROPAGE
seed:		.RES 4



.CODE
.PROC	lcg_8_bit
	LDA seed + 0
	ASL
	ASL
	CLC
	ADC seed + 0
	CLC
	ADC #increment
	STA seed + 0
	RTS
.ENDPROC


.PROC	lcg_16_bit
	LDA seed + 0
	CLC
	ADC #1
	TAX

	LDA seed + 1
	ADC seed + 0
	STA seed + 1
	STX seed + 0
	RTS
.ENDPROC


.PROC	lcg_24_bit
	LDA seed + 0
	CLC
	ADC #increment
	TAX

	LDA seed + 1
	ADC seed + 0
	TAY

	LDA seed + 2
	ADC seed + 1
	STA seed + 2
	STY seed + 1
	STX seed + 0
	RTS
.ENDPROC

.PROC	lcg_32_bit
	LDA seed + 0
	CLC
	ADC #increment
	TAX

	LDA seed + 1
	ADC seed + 0
	TAY
	STX seed + 0

	LDA seed + 2
	ADC seed + 1
	TAX
	STY seed + 1

	LDA seed + 3
	ADC seed + 2
	STA seed + 3
	STX seed + 2
	RTS
.ENDPROC