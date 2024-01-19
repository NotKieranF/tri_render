.IFNDEF	CONTROLLER_H
CONTROLLER_H = 1

; Read standard controller
; Trashes A, $00
.GLOBAL		read_controller

; Current button state, buttons pressed on this frame, and buttons released on this frame, respectively
.GLOBALZP	buttons_held, buttons_down, buttons_up

; Standard controller defines
BUTTON_A			= 1 << 7
BUTTON_B			= 1 << 6
BUTTON_SELECT		= 1 << 5
BUTTON_START		= 1 << 4
BUTTON_UP			= 1 << 3
BUTTON_DOWN			= 1 << 2
BUTTON_LEFT			= 1 << 1
BUTTON_RIGHT		= 1 << 0

; Helpful button combos
BUTTON_FACE			= BUTTON_A | BUTTON_B
BUTTON_DIRECTION	= BUTTON_UP | BUTTON_DOWN | BUTTON_LEFT | BUTTON_RIGHT

.ENDIF