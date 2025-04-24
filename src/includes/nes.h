.IFNDEF	NES_H
NES_H = 1

.SCOPE	PPU
	; Registers
	CTRL				:= $2000
	MASK				:= $2001
	STATUS				:= $2002
	OAMADDR				:= $2003
	OAMDATA				:= $2004
	SCROLL				:= $2005
	ADDR				:= $2006
	DATA				:= $2007
	OAMDMA				:= $4014

	; PPU::CTRL flags
	.SCOPE	CTRL
		DISABLE_NMI		= 0 << 7
		ENABLE_NMI		= 1 << 7
		SMALL_SPRITES	= 0 << 5
		LARGE_SPRITES	= 1 << 5
		BG_PATTERN_L	= 0 << 4
		BG_PATTERN_R	= 1 << 4
		SP_PATTERN_L	= 0 << 3
		SP_PATTERN_R	= 1 << 3
		INC_1			= 0 << 2
		INC_32			= 1 << 2
	.ENDSCOPE

	; PPU::MASK flags
	.SCOPE	MASK
		B_EMPHASIS		= 1 << 7
		G_EMPHASIS		= 1 << 6
		R_EMPHASIS		= 1 << 5
		CLIP_SP			= 1 << 4
		CLIP_BG			= 1 << 3
		RENDER_SP		= CLIP_SP | 1 << 2
		RENDER_BG		= CLIP_BG | 1 << 1
		GRAYSCALE		= 1 << 0
	.ENDSCOPE

	; PPU::STATUS flags
	.SCOPE	STATUS
		VBLANK			= 1 << 7
		SPRITE_0		= 1 << 6
		SPRITE_V		= 1 << 5
	.ENDSCOPE
.ENDSCOPE

;
.STRUCT	OAM
	Y_POS				.RES 1
	TILE				.RES 1
	ATTR				.RES 1
	X_POS				.RES 1
.ENDSTRUCT

.SCOPE	APU
	; Registers
	PORT_1				:= $4016
	PORT_2				:= $4017
	STATUS				:= $4015
	COUNTER				:= $4017

	; First pulse channel
	.SCOPE	SQ1
		; Registers
		CTRL			:= $4000
		SWEEP			:= $4001
		TIMER_LO		:= $4002
		TIMER_HI		:= $4003

		; APU::SQ1::CTRL flags
		.SCOPE	CTRL
			DUTY_MASK	= %11
			DUTY_SHIFT	= 6
		.ENDSCOPE
	.ENDSCOPE

	; Second pulse channel
	.SCOPE	SQ2
		; Registers
		CTRL			:= $4004
		SWEEP			:= $4005
		TIMER_LO		:= $4006
		TIMER_HI		:= $4007
	.ENDSCOPE
	
	; Triangle channel
	.SCOPE	TRI
		; Registers
		COUNTER			:= $4008
		TIMER_LO		:= $400A
		TIMER_HI		:= $400B
	.ENDSCOPE

	; Noise channel
	.SCOPE	NOI
		; Registers
		CTRL			:= $400C
		PERIOD			:= $400E
		LENGTH			:= $400F
	.ENDSCOPE

	; Delta modulation channel
	.SCOPE	DMC
		; Registers
		CTRL			:= $4010
		LOAD			:= $4011
		ADDR			:= $4012
		LENGTH			:= $4013

		; Flags
		IRQ_ENABLE		= %10000000
		LOOP			= %01000000

		; CPU cycles between output level changes per rate. NTSC
		RATE_0			= 428
		RATE_1			= 380
		RATE_2			= 340
		RATE_3			= 320
		RATE_4			= 286
		RATE_5			= 254
		RATE_6			= 226
		RATE_7			= 214
		RATE_8			= 190
		RATE_9			= 160
		RATE_A			= 142
		RATE_B			= 128
		RATE_C			= 106
		RATE_D			= 84
		RATE_E			= 72
		RATE_F			= 54
	.ENDSCOPE

.ENDSCOPE

.ENDIF