; Kieran Firkin
; TODO: Credits (poly render nesdev thread, c-hacking math, elite website)
.INCLUDE	"nes.h"
.INCLUDE	"render.h"
.INCLUDE	"nmi.h"
.INCLUDE	"math.h"
.INCLUDE	"objects.h"

; Ideas:
;	* Clip against near plane while iterating through vertices when rendering each polygon, instead of rendering to a buffer first
;	* Cache projected vertex positions
;	* Don't clip against near plane at all, instead let frustum culling do that for us
;		- Pros: Faster, don't need transformed_vertex_cache, only projected_vertex_cache, pop-in is okay for small objects
;		- Cons: Pop-in would be unacceptable for larger terrain objects
;	* Don't clip against *anything*
;		- Pros: Much faster, don't need transformed_vertex_cache
;		- Cons: Pop-in would be terrible
;	* Clip against the view frustum entirely within cameraspace, instead of splitting between cameraspace and screenspace
;		- Relevant math:	https://stackoverflow.com/questions/5666222/3d-line-plane-intersection
;							Seems like 2 dot-products, 1 scalar product, and a few vector additions. Seems slower probably, and hard to maintain only 8 bits of precision
;		- Pros: Potentially faster? Can do work entirely within 8 bits of precision, instead of 16 (subtract mesh origin from clipping plane)
;				Can probably trivially accept and reject entire polygons if they lie a certain distance away from the clipping plane
;		- Cons: Have to work in 3 dimensions instead of 2, clipping against non-orthogonal planes; it becomes harder to utilize projected_vertex_cache

.ZEROPAGE
display_list_indices:			.RES 32
display_list_size:				.RES 1

; Camera parameters
camera_pos_sub:
camera_pos_x_sub:				.RES NUM_CAMERAS
camera_pos_y_sub:				.RES NUM_CAMERAS
camera_pos_z_sub:				.RES NUM_CAMERAS
camera_pos_lo:
camera_pos_x_lo:				.RES NUM_CAMERAS
camera_pos_y_lo:				.RES NUM_CAMERAS
camera_pos_z_lo:				.RES NUM_CAMERAS
camera_pos_hi:
camera_pos_x_hi:				.RES NUM_CAMERAS
camera_pos_y_hi:				.RES NUM_CAMERAS
camera_pos_z_hi:				.RES NUM_CAMERAS
camera_pitch_lo:				.RES NUM_CAMERAS
camera_pitch_hi:				.RES NUM_CAMERAS
camera_yaw_lo:					.RES NUM_CAMERAS
camera_yaw_hi:					.RES NUM_CAMERAS
camera_roll_lo:					.RES NUM_CAMERAS
camera_roll_hi:					.RES NUM_CAMERAS

; Rotation matrices
camera_matrix:					.REPEAT NUM_CAMERAS 
              						.TAG ROT_MATRIX 
              					.ENDREPEAT
object_matrix:					.TAG ROT_MATRIX
combined_matrix:				.TAG ROT_MATRIX

; Buffer for the current working tile
tile_buffer:					.RES 16
tile_buffer_alpha:				.RES 8						; 1 is transparent, 0 is opaque





.BSS
display_list_polys:				.RES 256
transformed_vertex_cache_x:		.RES MESH_MAX_VERTICES
transformed_vertex_cache_y:		.RES MESH_MAX_VERTICES
transformed_vertex_cache_z:		.RES MESH_MAX_VERTICES

projected_vertex_cache_x_lo:	.RES MESH_MAX_VERTICES
projected_vertex_cache_x_hi:	.RES MESH_MAX_VERTICES
projected_vertex_cache_y_lo:	.RES MESH_MAX_VERTICES
projected_vertex_cache_y_hi:	.RES MESH_MAX_VERTICES

poly_buffer_x_lo:				.RES POLY_MAX_VERTICES
poly_buffer_y_lo:				.RES POLY_MAX_VERTICES
poly_buffer_x_hi:				.RES POLY_MAX_VERTICES
poly_buffer_y_hi:				.RES POLY_MAX_VERTICES
poly_buffer_next:				.RES POLY_MAX_VERTICES
poly_buffer_first:				.RES 1
poly_buffer_last:				.RES 1





.SEGMENT	"SAVERAM"
; Pattern table buffers
pattern_buffer:					.RES RENDER_MAX_TILES * 16
pattern_buffer_alpha:			.RES RENDER_MAX_TILES * 8	; 1 is transparent, 0 is opaque
allocated_patterns:				.RES 1

; Nametable buffer
name_buffer:					.RES SCREEN_WIDTH_TILES * SCREEN_HEIGHT_TILES





.CODE
.PROC	render_frame
init:
	; Effectively clear display list
	LDA #$00
	STA display_list_size

	; Generate camera rotation matrix
	LDA camera_roll_hi
	LDX camera_yaw_hi
	LDY camera_pitch_hi
	JSR setup_rot_matrix
	LDX #.SIZEOF(ROT_MATRIX)
:	LDA object_matrix - 1, X
	STA camera_matrix - 1, X
	DEX
	BNE :-

	JSR clear_oam
	LDX #$00
	JSR render_object

;	LDA #$01
;	STA camera_pos_z_lo
;	STA camera_roll_hi
;	INC camera_pitch_hi

;	JSR sort_display_list
;	JSR draw_display_list

	RTS
.ENDPROC

;
;	Takes: Signed 16-bit cameraspace vertex coordinates in vertex_x, vertex_y, and vertex_z
;	Returns: Signed 16-bit projected vertex coordinates in vertex_x and vertex_y
;	Clobbers: A, X, Y, $1A - $1F
.PROC	project_point
	vertex_x		:= $1A	; And $1B
	vertex_y		:= $1C	; And $1D
	vertex_z		:= $1E	; And $1F
	log_z			:= $1E

check_z:
	LDY #$00
	LDX vertex_z + 0
	LDA vertex_z + 1
	CMP #$01
	BEQ get_reciprocal

; Shift vertex_z right until it's in the range 256-511, i.e. until the hi byte is $01, and record the number of shifts in Y
shift_z:
	TXA
	LSR vertex_z + 1
:	INY
	ROR
	LSR vertex_z + 1	; Hi byte is shifted until it's $00, but we only need the lo byte and it's not shifted on the final iteration
	BNE :-
	TAX

; Get the reciprocal of the shifted vertex_z, vertex_x and vertex_y are multiplied by this instead of dividing by vertex_z
; Also store the number of shifts
get_reciprocal:
	STY log_z
	LDA reciprocal_lo, X

; Setup 8-bit multiplier
set_mul:
	STA mul_sq1_lo_ptr_0 + 0
	STA mul_sq1_hi_ptr_0 + 0
	EOR #$FF
	STA mul_sq2_lo_ptr_0 + 0
	STA mul_sq2_hi_ptr_0 + 0

; Get the hi 16 bits of the product between vertex_x and the reciprocal
mul_x:
	LDY vertex_x + 0
	LDA (mul_sq1_hi_ptr_0), Y
	SEC
	SBC (mul_sq2_hi_ptr_0), Y
	TAX

	LDY vertex_x + 1
	LDA (mul_sq1_lo_ptr_0), Y
	SEC
	SBC (mul_sq2_lo_ptr_0), Y
	STA vertex_x + 0

	LDA (mul_sq1_hi_ptr_0), Y
	SBC (mul_sq2_hi_ptr_0), Y

	; If vertex_x was negative, we need to subtract the reciprocal from the hi byte
	BIT vertex_x + 1
	BPL :+
		SEC
		SBC mul_sq1_lo_ptr_0 + 0
:	STA vertex_x + 1

	TXA
	CLC
	ADC vertex_x + 0
	BCC :+
		INC vertex_x + 1
:	STA vertex_x + 0

; Get the hi 16 bits of the product between vertex_y and the reciprocal
mul_y:
	LDY vertex_y + 0
	LDA (mul_sq1_hi_ptr_0), Y
	SEC
	SBC (mul_sq2_hi_ptr_0), Y
	TAX

	LDY vertex_y + 1
	LDA (mul_sq1_lo_ptr_0), Y
	SEC
	SBC (mul_sq2_lo_ptr_0), Y
	STA vertex_y + 0

	LDA (mul_sq1_hi_ptr_0), Y
	SBC (mul_sq2_hi_ptr_0), Y

	; If vertex_y was negative, we need to subtract the reciprocal from the hi byte
	BIT vertex_y + 1
	BPL :+
		SEC
		SBC mul_sq1_lo_ptr_0 + 0
:	STA vertex_y + 1

	TXA
	CLC
	ADC vertex_y + 0
	BCC :+
		INC vertex_y + 1
:	STA vertex_y + 0

; Shift output right the same number of times vertex_z was shifted to undo the implicit multiplication
shift_output:
	LDY log_z
	BEQ exit

	LDA vertex_x + 1
:	CMP #$80
	ROR
	ROR vertex_x + 0
	DEY
	BNE :-
	STA vertex_x + 1

	LDY log_z
	LDA vertex_y + 1
:	CMP #$80
	ROR
	ROR vertex_y + 0
	DEY
	BNE :-
	STA vertex_y + 1

exit:
	RTS
.ENDPROC

; Multiplies a point provided in
;	Takes: 7-bit signed input vector in $1A, $1B, $1C (x, y, z)
;	Returns: 7-bit signed output vector in $1D, $1E, $1F (x, y, z)
;	Clobbers: A, Y, $1A - $1F
.PROC	transform_point_combined_matrix
	input_x			:= $1A
	output_x		:= $1B
	input_y			:= $1C
	output_y		:= $1D
	input_z			:= $1E
	output_z		:= $1F

column_x:
	LDA input_x
	SET_FAST_MUL

	@row_x:
		LDY camera_matrix + ROT_MATRIX::XX
		FAST_MUL_HI
		STA output_x

	@row_y:
		LDY camera_matrix + ROT_MATRIX::XY
		FAST_MUL_HI
		STA output_y

	@row_z:
		LDY camera_matrix + ROT_MATRIX::XZ
		FAST_MUL_HI
		STA output_z

column_y:
	LDA input_y
	SET_FAST_MUL

	@row_x:
		LDY camera_matrix + ROT_MATRIX::YX
		FAST_MUL_HI
		CLC
		ADC output_x
		STA output_x

	@row_y:
		LDY camera_matrix + ROT_MATRIX::YY
		FAST_MUL_HI
		CLC
		ADC output_y
		STA output_y

	@row_z:
		LDY camera_matrix + ROT_MATRIX::YZ
		FAST_MUL_HI
		CLC
		ADC output_z
		STA output_z

column_z:
	LDA input_z
	SET_FAST_MUL

	@row_x:
		LDY camera_matrix + ROT_MATRIX::ZX
		FAST_MUL_HI
		CLC
		ADC output_x
		STA output_x

	@row_y:
		LDY camera_matrix + ROT_MATRIX::ZY
		FAST_MUL_HI
		CLC
		ADC output_y
		STA output_y

	@row_z:
		LDY camera_matrix + ROT_MATRIX::ZZ
		FAST_MUL_HI
		CLC
		ADC output_z
		STA output_z

exit:
	RTS
.ENDPROC

;
;
;
;
.PROC	interpolate

.ENDPROC

;
;	Takes:
;	Returns:
;	Clobbers:
.PROC	clip_poly

clip_left:

clip_right:

clip_top:

clip_bottom:

	RTS
.ENDPROC

;
.PROC	render_object
	object_pos_sub			:= $00	; And $01, $02. Stored in X, Y, Z order
	object_pos_lo			:= $03	; And $04, $05. Stored in X, Y, Z order
	object_pos_hi			:= $06	; And $07, $08. Stored in X, Y, Z order
	mesh_rel_pos_lo			:= $00	; And $01, $02. Stored in X, Y, Z order
	mesh_rel_pos_hi			:= $03	; And $04, $05. Stored in X, Y, Z order
	mesh_cameraspace_x		:= $06	; And $07
	mesh_cameraspace_y		:= $08	; And $09
	mesh_cameraspace_z		:= $0A	; And $0B
	loop_count				:= $0C
	mesh_ptr				:= $0D	; And $0E. Points to mesh data
	mesh_num_polys			:= $0F
	mesh_vertices_x_ptr		:= $00	; And $01. Points to list of vertex X components
	mesh_vertices_y_ptr		:= $02	; And $03. Points to list of vertex Y components
	mesh_vertices_z_ptr		:= $04	; And $05. Points to list of vertex Z components
	mesh_poly_ptr			:= $10	; And $11. Points to list of polygons
;	poly_num_vertices		:= $15	

	LDA object_pos_x_sub, X
	STA object_pos_sub + 0
	LDA object_pos_y_sub, X
	STA object_pos_sub + 1
	LDA object_pos_z_sub, X
	STA object_pos_sub + 2
	LDA object_pos_x_lo, X
	STA object_pos_lo + 0
	LDA object_pos_y_lo, X
	STA object_pos_lo + 1
	LDA object_pos_z_lo, X
	STA object_pos_lo + 2
	LDA object_pos_x_hi, X
	STA object_pos_hi + 0
	LDA object_pos_y_hi, X
	STA object_pos_hi + 1
	LDA object_pos_z_hi, X
	STA object_pos_hi + 2

; Get mesh's position relative to the camera, and cull meshes that are farther than 32767 units away from the camera
; mesh_rel_pos = object_pos - camera_pos
; if (|mesh_rel_pos| > 32767) then return
compute_rel_pos:
	LDX #$02
@loop:
	LDA object_pos_sub, X
	SEC
	SBC camera_pos_sub, X
	STA mesh_rel_pos_lo, X

	LDA object_pos_lo, X
	SBC camera_pos_lo, X
	STA mesh_rel_pos_hi, X

	LDA object_pos_hi, X
	SBC camera_pos_hi, X
;	STA mesh_rel_pos_hi, X

	BEQ @keep_mesh					; Check if mesh is greater than 32767 units away from the camera in a given axis
	CMP #$FF
	BEQ @keep_mesh
@cull_mesh:
	RTS
@keep_mesh:
	DEX
	BPL @loop

; Compute mesh's cameraspace position by multiplying its relative position vector by the current camera's rotation matrix
; mesh_cameraspace_x = mesh_rel_x * camera_matrix.xx + mesh_rel_y * camera_matrix.yx + mesh_rel_z * camera_matrix.zx
; mesh_cameraspace_y = mesh_rel_x * camera_matrix.xy + mesh_rel_y * camera_matrix.yy + mesh_rel_z * camera_matrix.zy
; mesh_cameraspace_z = mesh_rel_x * camera_matrix.xz + mesh_rel_y * camera_matrix.yz + mesh_rel_z * camera_matrix.zz
compute_cameraspace_pos:
	; Clear cameraspace position
	LDA #$00
	LDX #3 * 2 - 1
:	STA mesh_cameraspace_x, X
	DEX
	BPL :-

	LDX #$02
	STX loop_count
@loop:
	LDY mesh_rel_pos_lo, X
	LDA mesh_rel_pos_hi, X
	JSR set_mul_16x8bit_signed
@row_x:
	LDX loop_count
	LDY camera_matrix + ROT_MATRIX::XX, X
	JSR mul_16x8bit_signed
	TAX
	CLC
	TYA
	ADC mesh_cameraspace_x + 0
	STA mesh_cameraspace_x + 0
	TXA
	ADC mesh_cameraspace_x + 1
	STA mesh_cameraspace_x + 1

@row_y:
	LDX loop_count
	LDY camera_matrix + ROT_MATRIX::XY, X
	JSR mul_16x8bit_signed
	TAX
	CLC
	TYA
	ADC mesh_cameraspace_y + 0
	STA mesh_cameraspace_y + 0
	TXA
	ADC mesh_cameraspace_y + 1
	STA mesh_cameraspace_y + 1

@row_z:
	LDX loop_count
	LDY camera_matrix + ROT_MATRIX::XZ, X
	JSR mul_16x8bit_signed
	TAX
	CLC
	TYA
	ADC mesh_cameraspace_z + 0
	STA mesh_cameraspace_z + 0
	TXA
	ADC mesh_cameraspace_z + 1
	STA mesh_cameraspace_z + 1

@check_loop:
	DEC loop_count
	LDX loop_count
	BPL @loop

; Cull the current mesh if it's sufficiently outside the view frustum
frustum_cull:

; At this point we're committed to rendering this mesh, so we need to extract the mesh data
get_mesh_data:
	LDA #<cube
	STA mesh_ptr + 0
	LDA #>cube
	STA mesh_ptr + 1

	LDY #$00
@get_num_polys:
	LDA (mesh_ptr), Y
	STA mesh_num_polys

@get_vertices_x_ptr:
	INY
	LDA (mesh_ptr), Y
	STA mesh_vertices_x_ptr + 0
	INY
	LDA (mesh_ptr), Y
	STA mesh_vertices_x_ptr + 1

@get_vertices_y_ptr:
	INY
	LDA (mesh_ptr), Y
	STA mesh_vertices_y_ptr + 0
	INY
	LDA (mesh_ptr), Y
	STA mesh_vertices_y_ptr + 1

@get_vertices_z_ptr:
	INY
	LDA (mesh_ptr), Y
	STA mesh_vertices_z_ptr + 0
	INY
	LDA (mesh_ptr), Y
	STA mesh_vertices_z_ptr + 1

@get_poly_ptr:
	INY
	LDA (mesh_ptr), Y
	STA mesh_poly_ptr + 0
	INY
	LDA (mesh_ptr), Y
	STA mesh_poly_ptr + 1
	
; Set the X component of all entries in the vertex cache to an invalid value ($80) to indicate that they haven't been populated yet
; NOTE: Partially unroll this loop maybe?
clear_vertex_caches:
	LDX #MESH_MAX_VERTICES - 1
	LDA #$80
:	STA transformed_vertex_cache_x, X
	STA projected_vertex_cache_x_hi, X
	DEX
	BPL :-

;
draw_polygons:
	JSR render_poly
	DEC mesh_num_polys
	BNE draw_polygons


draw_origin:
	LDA mesh_cameraspace_x + 0
	STA project_point::vertex_x + 0
	LDA mesh_cameraspace_x + 1
	STA project_point::vertex_x + 1

	LDA mesh_cameraspace_y + 0
	STA project_point::vertex_y + 0
	LDA mesh_cameraspace_y + 1
	STA project_point::vertex_y + 1

	LDA mesh_cameraspace_z + 0
	STA project_point::vertex_z + 0
	LDA mesh_cameraspace_z + 1
	BMI @skip
	BEQ @skip
	STA project_point::vertex_z + 1

	JSR project_point

	LDA project_point::vertex_x + 0
	CLC
	ADC #<(SCREEN_WIDTH_PX / 2)
	TAX
	LDA project_point::vertex_x + 1
	ADC #>(SCREEN_WIDTH_PX / 2)
	BNE @skip

	LDA project_point::vertex_y + 0
	CLC
	ADC #<(SCREEN_HEIGHT_PX / 2)
	TAY
	LDA project_point::vertex_y + 1
	ADC #>(SCREEN_HEIGHT_PX / 2)
	BNE @skip
	TYA

	JSR plot_point

@skip:

	RTS
.ENDPROC

;
;	Takes:
;	Returns:
;	Clobbers:

	;	render_poly(poly_ptr) {
	;		poly_num_vertices = *(poly_ptr++)
	;
	;		// Need to pull out one iteration of the loop to setup parity
	;		for (int i = 1; i < poly_num_vertices; i++) {
	;			prev_parity = cur_parity
	;			// If the current vertex i in the projected cache, then we know it must be inside the near plane
	;			if (projected_vertex_cache_x_hi[i] == 0x80) {
	;				cur_parity = 1;
	;				if (cur_parity != prev_parity) {
	;
	;				}
	;
	;				poly_buffer_x_lo[j] = projected_vertex_cache_x_lo[i]
	;				poly_buffer_x_hi[j] = projected_vertex_cache_x_hi[i]
	;				poly_buffer_y_lo[j] = projected_vertex_cache_y_lo[i]
	;				poly_buffer_y_hi[j] = projected_vertex_cache_y_hi[i]
	;				j++
	;
	;			// Else if the current vertex is in the transformed cache, then we know it must be outside the near plane
	;			} else if {transformed_vertex_cache_x == 0x80) {
	;				cur_parity = 0;
	;				if (cur_parity != prev_parity) {
	;
	;				}
	;
	;			// Otherwise, we just haven't encountered the current vertex
	;			} else {
	;				transform_point_combined_matrix()
	;
	;
	;			}
	;		}
	;
	;	}


;	Interpolating between two points for crossing
;	xdiff = x2 - x1
;	ydiff = y2 - y1
;	slope = ydiff / xdiff
;	y1 + slope * (x - x1)
;
.PROC	render_poly
	prev_vertex		:= $14
	cur_vertex		:= $15
	poly_index		:= $16
	num_vertices	:= $17
	cur_parity		:= $18
	prev_parity		:= $19
	x_diff			:= $1A
	interpolated_x	:= $1B	; And $1C
	y_diff			:= $1D
	interpolated_y	:= $1E	; And $1F

	INSIDE			= $01
	OUTSIDE			= $00

;
init:
	LDY #$00
	STY poly_index
	STY poly_buffer_first
	STY poly_buffer_last

	LDA (render_object::mesh_poly_ptr), Y
	INC poly_index
	STA num_vertices

	LDA #$80								; Ensure that the first loop isn't marked as passing across the clipping plane, no matter what side it begins on
	STA cur_parity

loop:
	LDA cur_parity							; Check for equality to determine if we've crossed the clipping plane instead of inequality
	EOR #$01								; This is so we can more easily incorporate the first iteration by setting the hi bit so that cur_parity and prev_parity are never equal
	STA prev_parity

	LDA cur_vertex
	STA prev_vertex

	LDA #OUTSIDE							; Preload cur_parity assuming that we're outside the clipping plane, this is so the code paths can converge later on
	STA cur_parity

@get_vertex_index:
	LDY poly_index
	LAX (render_object::mesh_poly_ptr), Y
	STX cur_vertex
	INC poly_index
	LDA #$80

; If the current vertex is present in the projected cache, then we know that it's within the near clipping plane
@check_projected_cache:
	CMP projected_vertex_cache_x_hi, X
	BNE inside_near_plane

; Otherwise if the current vertex is present in the transformed cache, then we know that it's outside the near clipping plane
@check_transformed_cache:
	CMP transformed_vertex_cache_x, X
	BNE outside_near_plane

; Finally, if a vertex is not present in either cache, then we have not encountered it yet, and must transform and check manually
new_vertex:
	TXA
	TAY
	LDA (render_object::mesh_vertices_x_ptr), Y
	STA transform_point_combined_matrix::input_x
	LDA (render_object::mesh_vertices_y_ptr), Y
	STA transform_point_combined_matrix::input_y
	LDA (render_object::mesh_vertices_z_ptr), Y
	STA transform_point_combined_matrix::input_z

	; X is preserved
	JSR transform_point_combined_matrix

	; Cache transformed result and pre-emptively setup project_point inputs
@cache_x:
	LDY #$00
	LDA transform_point_combined_matrix::output_x
	BPL :+												; Sign extend result
		DEY
:	STA transformed_vertex_cache_x, X
	CLC
	ADC render_object::mesh_cameraspace_x + 0
	STA project_point::vertex_x + 0
	TYA
	ADC render_object::mesh_cameraspace_x + 1
	STA project_point::vertex_x + 1

@cache_y:
	LDY #$00
	LDA transform_point_combined_matrix::output_y
	BPL :+
		DEY
:	STA transformed_vertex_cache_y, X
	CLC
	ADC render_object::mesh_cameraspace_y + 0
	STA project_point::vertex_y + 0
	TYA
	ADC render_object::mesh_cameraspace_y + 1
	STA project_point::vertex_y + 1

@cache_z:
	LDY #$00
	LDA transform_point_combined_matrix::output_z
	BPL :+
		DEY
:	STA transformed_vertex_cache_z, X
	CLC
	ADC render_object::mesh_cameraspace_z + 0
	STA project_point::vertex_z + 0
	TYA
	ADC render_object::mesh_cameraspace_z + 1
	STA project_point::vertex_z + 1

	; Skip projection if hi byte is $00 or $80-$FF, i.e. if the point is behind the near plane
	BMI outside_near_plane
	BEQ outside_near_plane

; Otherwise project vertex into screenspace and cache the results
project_vertex:
	JSR project_point
	LDX cur_vertex

@cache_x:
	LDA project_point::vertex_x + 0
	CLC									; Offset x position by half the screen width so that on-screen coordinates range between [0, SCREEN_WIDTH_PX)
	ADC #<(SCREEN_WIDTH_PX / 2)
	STA projected_vertex_cache_x_lo, X
	LDA project_point::vertex_x + 1
	ADC #>(SCREEN_HEIGHT_PX / 2)
	STA projected_vertex_cache_x_hi, X

@cache_y:
	LDA project_point::vertex_y + 0
	CLC									; Offset y position by half the screen height so that on-screen coordiantes range between [0, SCREEN_HEIGHT_PX)
	ADC #<(SCREEN_HEIGHT_PX / 2)
	STA projected_vertex_cache_y_lo, X
	LDA project_point::vertex_y + 1
	ADC #>(SCREEN_HEIGHT_PX / 2)
	STA projected_vertex_cache_y_hi, X

; Set cur_parity to indicate that we're on the inside of the clipping plane, then fall through to check if we've crossed the clipping plane
inside_near_plane:
	INC cur_parity

; If we've crossed the clipping plane, then we need to interpolate between the existing vertices to add a new vertex between them
; Otherwise, we skip forward to check if we need to copy the current vertex over to the output
outside_near_plane:
	LDA cur_parity
	EOR prev_parity
	BEQ :+
		JMP @check_parity
	:

; Need to clean up code here
@interpolate:
	LDX cur_vertex
	LDY prev_vertex

	; x_diff = transformed_vertex_cache_x[prev_vertex] - transformed_vertex_cache_x[cur_vertex]
	LDA transformed_vertex_cache_x, Y
	SEC
	SBC transformed_vertex_cache_x, X
	STA x_diff

	; y_diff = transformed_vertex_cache_y[prev_vertex] - transformed_vertex_cache_y[cur_vertex]
	LDA transformed_vertex_cache_y, Y
	SEC
	SBC transformed_vertex_cache_y, X
	STA y_diff

	; z_diff = transformed_vertex_cache_z[prev_vertex] - transformed_vertex_cache_z[cur_vertex]
	; z_recip = reciprocal_16bit_signed[z_diff]
	LDA transformed_vertex_cache_z, Y
	SEC
	SBC transformed_vertex_cache_z, X
	TAX
	LDY reciprocal_16bit_signed_lo, X
	LDA reciprocal_16bit_signed_hi, X
	JSR set_mul_16x8bit_signed

	; clip_diff = NEAR_PLANE - mesh_cameraspace_z - transformed_vertex_cache_z[prev_vertex]
	; weight = z_recip * clip_diff / 256
	LDY prev_vertex
	LDA #<NEAR_PLANE
	SEC
	SBC render_object::mesh_cameraspace_z + 0
	SEC
	SBC transformed_vertex_cache_z, Y
	TAY
	JSR mul_16x8bit_signed
	JSR set_mul_16x8bit_signed

	; interpolated_x = x_diff * weight / 256
	LDY x_diff
	JSR mul_16x8bit_signed
;	STY interpolated_x + 0
;	STA interpolated_x + 1
	STA interpolated_x + 0						; Sign extend A
	CMP #$80
	LDA #$FF
	ADC #$00
	EOR #$FF
	STA interpolated_x + 1

	CPY #$80
	ROL interpolated_x + 0
	ROL interpolated_x + 1

	; interpolated_y = y_diff * weight / 256
	LDY y_diff
	JSR mul_16x8bit_signed
;	STY interpolated_y + 0
;	STA interpolated_y + 1
	STA interpolated_y + 0						; Sign extend A
	CMP #$80
	LDA #$FF
	ADC #$00
	EOR #$FF
	STA interpolated_y + 1

	CPY #$80
	ROL interpolated_y + 0
	ROL interpolated_y + 1

	; interpolated_x += SCREEN_WIDTH_PX / 2
	LDA #<(SCREEN_WIDTH_PX / 2)
	CLC
	ADC interpolated_x + 0
	STA interpolated_x + 0
	LDA #>(SCREEN_WIDTH_PX / 2)
	ADC interpolated_x + 1
	STA interpolated_x + 1

	; interpolated_y += SCREEN_HEIGHT_PX / 2
	LDA #<(SCREEN_HEIGHT_PX / 2)
	CLC
	ADC interpolated_y + 0
	STA interpolated_y + 0
	LDA #>(SCREEN_HEIGHT_PX / 2)
	ADC interpolated_y + 1
	STA interpolated_y + 1

	LDY prev_vertex
	; interpolated_x += transformed_vertex_cache_x[prev_vertex]
	LDX #$00
	LDA transformed_vertex_cache_x, Y
	BPL :+
		DEX
:	CLC
	ADC interpolated_x + 0
	STA interpolated_x + 0
	TXA
	ADC interpolated_x + 1
	STA interpolated_x + 1

	; interpolated_y += transformed_vertex_cache_y[prev_vertex]
	LDX #$00
	LDA transformed_vertex_cache_y, Y
	BPL :+
		DEX
:	CLC
	ADC interpolated_y + 0
	STA interpolated_y + 0
	TXA
	ADC interpolated_y + 1
	STA interpolated_y + 1

	LDX poly_buffer_last
	; poly_buffer_x[poly_buffer_last] = interpolated_x + mesh_cameraspace_x
	LDA render_object::mesh_cameraspace_x + 0
	CLC
	ADC interpolated_x + 0
	STA poly_buffer_x_lo, X
	LDA render_object::mesh_cameraspace_x + 1
	ADC interpolated_x + 1
	STA poly_buffer_x_hi, X

	; poly_buffer_y[poly_buffer_last] = interpolated_y + mesh_cameraspace_y
	LDA render_object::mesh_cameraspace_y + 0
	CLC
	ADC interpolated_y + 0
	STA poly_buffer_y_lo, X
	LDA render_object::mesh_cameraspace_y + 1
	ADC interpolated_y + 1
	STA poly_buffer_y_hi, X

	; poly_buffer_next[poly_buffer_last - 1] = poly_buffer_last
	; poly_buffer_last++
	TXA
	STA poly_buffer_next - 1, X
	STA $4448
	INC poly_buffer_last


;
;	xDiff = s16.0
;	yDiff = s16.0
;	zDiff = s16.0
;	clipDiff = s16.0
;	

; cameraspace_z = mesh_cameraspace_z + transformed_space_z
; (0x0100 - (mesh_cameraspace_z + transformed_space_z))
; (0x0100 - mesh_cameraspace_z) - transformed_space_z


;	Better version
;	Requires one 16x8 hi16 multiplication, and two 16x8 mid 8 multiplications with repeated 16bit factors
;	interpolate(int i, int j) {
;		x_diff = transformed_vertex_cache_x[i] - transformed_vertex_cache_x[j]
;		y_diff = transformed_vertex_cache_y[i] - transformed_vertex_cache_y[j]
;		z_diff = transformed_vertex_cache_z[i] - transformed_vertex_cache_z[j]
;		clip_diff = 0x0100 - mesh_cameraspace_z - transformed_vertex_cache_z[j]
;
;		z_recip = reciprocal_signed_16bit(z_diff)
;		interp_factor = mul_16x8bit_signed_hi16(z_recip, clip_diff)
;
;		x_pos = transformed_vertex_cache_x[j] + mul_16x8bit_signed_mid8(x_diff, interp_factor) + mesh_cameraspace_x
;		y_pos = transformed_vertex_cache_y[j] + mul_16x8bit_signed_mid8(x_diff, interp_factor) + mesh_cameraspace_z
;
;	}

;	z_recip = 0.16 signed (i.e. 32767 ~= 1.0)
;	interp_factor = 8.0 * 0.16 = 8.16 -> 8.8 signed


@check_parity:
	LDA cur_parity
	BEQ check_loop

; If the current vertex is on the inside of the clipping plane, we need to copy it to the output
copy_vertex:
	LDX cur_vertex
	LDY poly_buffer_last
	INC poly_buffer_last

@copy_x:
	LDA projected_vertex_cache_x_lo, X
	STA poly_buffer_x_lo, Y
	LDA projected_vertex_cache_x_hi, X
	STA poly_buffer_x_hi, Y

@copy_y:
	LDA projected_vertex_cache_y_lo, X
	STA poly_buffer_y_lo, Y
	LDA projected_vertex_cache_y_hi, X
	STA poly_buffer_y_hi, Y

@set_next:
	TYA
	STA poly_buffer_next - 1, Y				; This is concerning, but should be fine as long as poly_buffer_next follows the rest of the poly_buffer arrays

check_loop:
	DEC num_vertices
	BEQ :+
		JMP loop
:

; Increment mesh_poly_ptr, close the polygon, and exit
exit:
	LDA poly_index
	CLC
	ADC render_object::mesh_poly_ptr + 0
	STA render_object::mesh_poly_ptr + 0

	LDY poly_buffer_last
	LDA #$00
	STA poly_buffer_next - 1, Y

	; assert(A == $00)
	ADC render_object::mesh_poly_ptr + 1
	STA render_object::mesh_poly_ptr + 1

temp_test:
	LDA #$00
@loop:
	TAY
	PHA
	LDA poly_buffer_x_hi, Y
	BNE :+
	LDA poly_buffer_y_hi, Y
	BNE :+
	LDX poly_buffer_x_lo, Y
	LDA poly_buffer_y_lo, Y
	JSR plot_point

:	PLA
	TAY
	LDA poly_buffer_next, Y
	BNE @loop

	RTS
.ENDPROC

; Sets up a rotation matrix based on Tait-Bryan angles provided
;	Takes: Yaw in X, pitch in Y, roll in A
;	Returns: Rotation matrix in object_matrix
;	Clobbers: A, X, Y, $1B - $1F
;	~411 cycles
.PROC	setup_rot_matrix
yaw			:= $1B		; Tait-Bryan angles
pitch		:= $1C
roll		:= $1D

y_plus_p	:= $1E		; Common angle calculations
y_minus_p	:= $1F

	STX yaw
	STY pitch
	STA roll

; xx = qcos(y-p) + qcos(y+p)
; xy = qsin(y+p) + qsin(y-p)
compute_xx_xy:
	LDA yaw
	SEC
	SBC pitch
	STA y_minus_p
	TAY

	LDA yaw
	CLC
	ADC pitch
	STA y_plus_p
	TAX

	LDA qcos, Y
	CLC
	ADC qcos, X
	STA object_matrix + ROT_MATRIX::XX

	LDA qsin, X
	CLC
	ADC qsin, Y
	STA object_matrix + ROT_MATRIX::YX

; zx = -hsin(p)
compute_zx:
	LDA pitch
	CLC
	ADC #$80
	TAX

	LDA hsin, X
	STA object_matrix + ROT_MATRIX::ZX

; zy = qsin(p+r) - qsin(p-r)
; zz = qcos(p-r) + qcos(p+r)
compute_zy_zz:
	LDA pitch
	CLC
	ADC roll
	TAY

	LDA pitch
	SEC
	SBC roll
	TAX

	LDA qsin, Y
	SEC
	SBC qsin, X
	STA object_matrix + ROT_MATRIX::ZY

	LDA qcos, X
	CLC
	ADC qcos, Y
	STA object_matrix + ROT_MATRIX::ZZ

; xy = ecos(y-p-r) - ecos(y+p-r) + qsin(y-r)
; yz = ecos(y-p-r) - ecos(y+p-r) + qsin(y-r)
compute_xy_yz:
	LDA y_minus_p
	SEC
	SBC roll
	TAY

	LDA y_plus_p
	SEC
	SBC roll
	TAX
	LDA ecos, Y
	SEC
	SBC ecos, X
	TAY

	LDA yaw
	SEC
	SBC roll
	TAX
	TYA
	CLC
	ADC qsin, X
	STA object_matrix + ROT_MATRIX::XY
	STA object_matrix + ROT_MATRIX::YZ

; xy = ecos(y-p+r) - ecos(y+p+r) - qsin(y+r) - xy
; yz = ecos(y-p+r) - ecos(y+p+r) - qsin(y+r) + xy
	LDA y_minus_p
	CLC
	ADC roll
	TAY

	LDA y_plus_p
	CLC
	ADC roll
	TAX
	LDA ecos, Y
	SEC
	SBC ecos, X
	TAY

	LDA yaw
	CLC
	ADC roll
	TAX
	TYA
	SEC
	SBC qsin, X
	TAY
	SEC
	SBC object_matrix + ROT_MATRIX::XY
	STA object_matrix + ROT_MATRIX::XY

	TYA
	CLC
	ADC object_matrix + ROT_MATRIX::YZ
	STA object_matrix + ROT_MATRIX::YZ

; xz = esin(y-p+r) - esin(y+p+r) + qcos(y+r)
; yy = esin(y-p+r) - esin(y+p+r) + qcos(y+r)
compute_xz_yy:
	LDA y_minus_p
	CLC
	ADC roll
	TAY

	LDA y_plus_p
	CLC
	ADC roll
	TAX
	LDA esin, Y
	SEC
	SBC esin, X
	TAY

	LDA yaw
	CLC
	ADC roll
	TAX
	TYA
	CLC
	ADC qcos, X
	STA object_matrix + ROT_MATRIX::XZ
	STA object_matrix + ROT_MATRIX::YY

; xz = esin(y+p-r) - esin(y-p-r) + qcos(y-r) - xz
; yy = esin(y+p-r) - esin(y-p-r) + qcos(y-r) + yy
	LDA y_plus_p
	SEC
	SBC roll
	TAY

	LDA y_minus_p
	SEC
	SBC roll
	TAX
	LDA esin, Y
	SEC
	SBC esin, X
	TAY

	LDA yaw
	SEC
	SBC roll
	TAX
	TYA
	CLC
	ADC qcos, X
	TAY
	SEC
	SBC object_matrix + ROT_MATRIX::XZ
	STA object_matrix + ROT_MATRIX::XZ

	TYA
	CLC
	ADC object_matrix + ROT_MATRIX::YY
	STA object_matrix + ROT_MATRIX::YY

	STA $4400
	RTS

; xy = ecos(y+p+r) + ecos(y+p-r) - qsin(y-r)
; xz = ecos(y+p+r) + ecos(y+p-r) - qsin(y-r)
compute_xy_xz:
	LDA y_plus_p
	CLC
	ADC roll
	TAY

	LDA y_plus_p
	SEC
	SBC roll
	TAX
	LDA ecos, Y
	CLC
	ADC ecos, X
	TAY

	LDA yaw
	SEC
	SBC roll
	TAX
	TYA
	SEC
	SBC qsin, X
	STA object_matrix + ROT_MATRIX::XY
	STA object_matrix + ROT_MATRIX::XZ

; xy = ecos(y-p+r) + ecos(y-p-r) - qsin(y+r) + xy
; xz = ecos(y-p+r) + ecos(y-p-r) - qsin(y+r) - xz
	LDA y_minus_p
	CLC
	ADC roll
	TAY

	LDA y_minus_p
	SEC
	SBC roll
	TAX
	LDA ecos, Y
	CLC
	ADC ecos, X
	TAY

	LDA yaw
	CLC
	ADC roll
	TAX
	TYA
	SEC
	SBC qsin, X
	TAY
	CLC
	ADC object_matrix + ROT_MATRIX::XY
	STA object_matrix + ROT_MATRIX::XY

	TYA
	SEC
	SBC object_matrix + ROT_MATRIX::XZ
	STA object_matrix + ROT_MATRIX::XZ

; yy = esin(y-p+r) - esin(y+p+r) + qcos(y+r)
; yz = esin(y-p+r) - esin(y+p+r) + qcos(y+r)
compute_yy_yz:
	LDA y_minus_p
	CLC
	ADC roll
	TAY

	LDA y_plus_p
	CLC
	ADC roll
	TAX
	LDA esin, Y
	SEC
	SBC esin, X
	TAY

	LDA yaw
	CLC
	ADC roll
	TAX
	TYA
	CLC
	ADC qcos, X
	STA object_matrix + ROT_MATRIX::YY
	STA object_matrix + ROT_MATRIX::YZ

; yy = esin(y+p-r) - esin(y-p-r) + qcos(y-r) + yy
; yz = esin(y+p-r) - esin(y-p-r) + qcos(y-r) - yz
	LDA y_plus_p
	SEC
	SBC	roll
	TAY

	LDA y_minus_p
	SEC
	SBC roll
	TAX
	LDA esin, Y
	SEC
	SBC esin, X
	TAY

	LDA yaw
	SEC
	SBC roll
	TAX
	TYA
	CLC
	ADC qcos, X
	TAY
	CLC
	ADC object_matrix + ROT_MATRIX::YY
	STA object_matrix + ROT_MATRIX::YY

	TYA
	SEC
	SBC object_matrix + ROT_MATRIX::YZ
	STA object_matrix + ROT_MATRIX::YZ

	STA $4400
	RTS
.ENDPROC

; Premultiplies object_matrix by camera_matrix and stores the result in combined_matrix
;	Could do with its own multiplication routine, as its precision requirements are unique
;	Could proooobably massage it to loop 9 times instead of 3 to reduce code size, but matrix math is melting my brain so not today. 
;	Plus it would increase run time by a fair bit, 3 extra zeropage loads per iteration-ish I think.
;	Clobbers: A, X, Y, $1F
.PROC	matrix_multiply

loop:
	LDA camera_matrix + ROT_MATRIX::XX, X
	SET_FAST_MUL

	LDY object_matrix + ROT_MATRIX::XX
	FAST_MUL_HI
	STA combined_matrix + ROT_MATRIX::XX

	LDY object_matrix + ROT_MATRIX::XY
	FAST_MUL_HI
	STA combined_matrix + ROT_MATRIX::XY

	LDY object_matrix + ROT_MATRIX::XZ
	FAST_MUL_HI
	STA combined_matrix + ROT_MATRIX::XZ

	LDA camera_matrix + ROT_MATRIX::XY, X
	SET_FAST_MUL

	LDY object_matrix + ROT_MATRIX::YX
	FAST_MUL_HI
	CLC
	ADC combined_matrix + ROT_MATRIX::XX
	STA combined_matrix + ROT_MATRIX::XX

	LDY object_matrix + ROT_MATRIX::YY
	FAST_MUL_HI
	CLC
	ADC combined_matrix + ROT_MATRIX::XY
	STA combined_matrix + ROT_MATRIX::XY

	LDY object_matrix + ROT_MATRIX::YZ
	FAST_MUL_HI
	CLC
	ADC combined_matrix + ROT_MATRIX::XZ
	STA combined_matrix + ROT_MATRIX::XZ

	LDA camera_matrix + ROT_MATRIX::XZ, X
	SET_FAST_MUL

	LDY object_matrix + ROT_MATRIX::ZX
	FAST_MUL_HI
	CLC
	ADC combined_matrix + ROT_MATRIX::XX
	STA combined_matrix + ROT_MATRIX::XX

	LDY object_matrix + ROT_MATRIX::ZY
	FAST_MUL_HI
	CLC
	ADC combined_matrix + ROT_MATRIX::XY
	STA combined_matrix + ROT_MATRIX::XY

	LDY object_matrix + ROT_MATRIX::ZZ
	FAST_MUL_HI
	CLC
	ADC combined_matrix + ROT_MATRIX::XZ
	STA combined_matrix + ROT_MATRIX::XZ

	INX
	CPX #.SIZEOF(ROT_MATRIX) / 3
	BNE :+
	JMP loop
:

	RTS
.ENDPROC

; Sorts display_list_indices from nearest to farthest
;	Requires display_list_indices to be in zp
;	Clobbers: A, X, Y, $1F
.PROC	sort_display_list
	outer_loop_count	:= $1F

	LDA display_list_size
	STA outer_loop_count
outer_loop:
	LDX #$01
inner_loop:
	LDY display_list_indices - 0, X
	LDA display_list_polys + SCREEN_POLY::DEPTH_HI, Y
	LDY display_list_indices - 1, X
	CMP display_list_polys + SCREEN_POLY::DEPTH_HI, Y
	BCC check_inner
	BNE swap_indices
	LDA display_list_polys + SCREEN_POLY::DEPTH_LO, Y
	LDY display_list_indices - 0, X
	CMP display_list_polys + SCREEN_POLY::DEPTH_LO, Y
	BCS check_inner

swap_indices:
	LDA display_list_indices - 0, X
	STA display_list_indices - 1, X
	STY display_list_indices - 0, X

check_inner:
	INX
	CPX outer_loop_count
	BNE inner_loop

check_outer:
	LDA #$01
	DCP outer_loop_count
	BNE outer_loop

exit:
	RTS
.ENDPROC

;
.PROC	draw_display_list
	cur_poly		:= $00
	cur_vertex		:= $01
	num_vertices	:= $02

	LDX #$00
outer_loop:
	LDY display_list_indices, X
	LDA display_list_polys + SCREEN_POLY::ATTR, Y
	STA num_vertices
	STX cur_poly
inner_loop:
	LDA display_list_polys + SCREEN_POLY::X_POS, Y
	TAX
	LDA display_list_polys + SCREEN_POLY::Y_POS, Y
	STY cur_vertex
	JSR plot_point

	LDY cur_vertex
	INY
	INY
	DEC num_vertices
	BNE inner_loop

	DEC display_list_size
	BNE outer_loop

	RTS
.ENDPROC

;
;	Takes:
;	Returns:
;	Clobbers: A, X, Y, $00 - $1F
.IF 0
.PROC	rasterize_poly
	left_edges		:= $00 ; Through $07
	right_edges		:= $08 ; Through $0F
	topmost_px		:= $1A
	bottommost_px	:= $1B
	leftmost_px		:= $1C
	rightmost_px	:= $1D
	poly_ptr		:= $1E	; And $1F

;	rasterizePoly(Poly* polyPtr) {
;		int bresTargetL;
;		int bresTargetR;
;		int bresErrorL;
;		int bresErrorR;
;		int bresSlopeL;
;		int bresSlopeR;
;		int bresRoutineL;
;		int bresRoutineR;
;
;		// Initlialization
;		int color = *(polyPtr++);
;		int targetVertexL = 2;
;		int targetVertexR = *(polyPtr++);
;		int bresCurrentXL = *(polyPtr + 0);
;		int bresCurrentYL = *(polyPtr + 1);
;		int bresCurrentXR = *(polyPtr + 0);
;		int bresCurrentYR = *(polyPtr + 1);
;
;
;		
;	}
;
;	initBresL() {
;		bresTargetXL = *(polyPtr + targetVertexL++);
;		bresTargetYL = *(polyPtr + targetVertexL++);
;
;		xDif = bresTargetXL - bresCurrentXL;
;		yDif = bresTargetYL - bresCurrentYL;
;		if (xDif < 0) {
;			xDif = abs(xDif)
;			if (xDif > yDif) {
;				//
;				bresSlopeL = yDif / xDif;
;				bresRoutineL = 0;
;			} else {
;				bresSlopeL = xDif / yDif;
;				bresRoutineL = 1;
;			}
;		} else {
;			if (xDif > yDif) {
;				bresSlopeL = yDif / xDif;
;				bresRoutineL = 2;
;			} else {
;				bresSlopeL = xDif / yDif;
;				bresRoutineL = 3;
;			}
;		}
;
;		bresErrorL = 0;
;
;		return;
;	}
;
;	initBresR() {
;		bresTargetYR = *(polyPtr + targetVertexR--);
;		bresTargetXR = *(polyPtr + targetVertexR--); 
;	}
;
;	stepBresL() {
;
;	}

	LDY #$00
	LDA (poly_ptr), Y
	STA right_target_vertex

	INY
	LDA (poly_ptr), Y
	STA left_current_x
	STA right_current_x

	INY
	LDA (poly_ptr), Y
	STA left_current_y
	STA right_current_y

	INY
	STY left_target_vertex
	LDA (poly_ptr), Y
	STA left_target_x

	INY
	LDA (poly_ptr), Y
	STA left_target_y

	LDY right_target_vertex
	LDA (poly_ptr), Y
	STA right_target_y

	DEY
	LDA (poly_ptr), Y

read_vertex_left:
	LDY left_target_vertex
	LDA (poly_ptr), Y
	STA left_target_x
	SEC
	SBC left_current_x
	BCS @pos
@neg:
	EOR #$FF
;	CLC
	ADC #$01
@pos:
	STA dx

	INY
	LDA (poly_ptr), Y
	STA left_target_y
	SEC
	SBC left_current_y
	CMP dx
	BCS @pos
@neg:
	INX
@pos:

	INY
	STY left_target_vertex

read_vertex_right:
	LDY right_target_vertex
	LDA (poly_ptr), Y
	STA right_target_y

	DEY
	LDA (poly_ptr), Y
	STA right_target_x

	DEY
	STY right_target_vertex











	LDY #$00
loop:
	LDX left_edges, Y
	LDA left_table, X
	LDX right_edges, Y
	AND right_table, X
	STA tile_buffer, Y

	LDA left_edges, Y
	SEC
	SBC #$08
	BCS :+
		LDA #$00
:	STA left_edges, Y

	LDA right_edges, Y
	SEC
	SBC #$08
	BCS :+
		LDA #$00
:	STA right_edges, Y

	INY
	CPY #$08
	BNE loop



	LDY #$00
loop:
	LDA left_edges, Y
	TAX
	CPX #$08
	BCC :+
		LDX #$08
		CLC
:	SBC #$08 - 1
	BCS :+
		LDA #$00
:	STA left_edges, Y
	LDA left_table, X
	PHA

	LDA right_edges, Y
	TAX
	CPX #$08
	BCC :+
		LDX #$08
		CLC
:	SBC #$08 - 1
	BCS :+
		LDA #$00
:	STA right_edges, Y
	PLA
	AND right_table, X
	STA tile_buffer, Y

	INY
	CPY #$08
	BNE loop



	LDY #$00
loop:
	LDA left_edges, Y
	SEC
	SBC min_px
	BCS :+
		LDX #$00
		BEQ :+++
:	CMP #$08
	BCC :+
		LDA #$08
:	TAX
:	LDA left_table, X
	PHA

	LDA right_edges, Y
	SEC
	SBC min_px
	BCS :+
		LDX #$00
		BEQ :+++
:	CMP #$08
	BCC :+
		LDA #$08
:	TAX
:	PLA
	AND right_table, X
	STA tile_buffer, Y

	INY
	CPY #$08
	BNE loop

;
	LDY #$00
loop:
	LDA left_edges, Y
	CMP max_px
	BCC :+
		LDX #$08
		BNE :+++
:	SBC min_px
	BCS :+
		LDA #$00
:	TAX
	LDA left_table, X
	PHA

; ...

	INY
	CPY #$08
	BNE loop

	LDY #$00
loop:
	LDX left_edges, Y
	CPX #$08
	BCC :+
		LDX #$08
:	LDA left_table, X

	LDX right_edges, Y
	CPX #$08
	BCC :+
		LDX #$08
:	AND right_table, X
	STA tile_buffer, Y

	INY
	CPY #$08
	BNE loop

; Best, pretty sure
; Functional
; 70 cycles max, 62 cycles min, plus no cost outside of loop
	LDY #$00
	CLD
loop:
	LAX left_edges, Y
	CPX #$08
	BCC :+
		LDX #$08
		CLC
:	SBC #$08 - 1
	BCS :+
		LDA #$00
:	STA left_edges, Y
	LDA left_table, X
	PHA

	LAX right_edges, Y
	CPX #$08
	BCC :+
		LDX #$08
		CLC
:	SBC #$08 - 1
	BCS :+
		LDA #$00
:	STA right_edges, Y
	PLA
	AND right_edges, X
	CMP #$FF
	BNE :+
		SED
:	STA tile_buffer, Y

	INY
	CPY #$08
	BNE loop

; separate px and tile pos?
; 32/44 cycles best case, 79 cycles worst case
; left_encountered should hold $FF before we encounter the left edge, and $00 after
; right_encountered should hold $00 before we encounter the right edge, and $FF after
; carry is clear if tile is completely filled, set if not
; 0's are opaque
	CLC
	LDX #$07
loop:
	LDA left_encountered, X
	BEQ :+
	DEC left_edges_tile, X
	BNE :+
		INC left_encountered, X
		LDY left_edges_px, X
		LDA left_table, Y
:	STA temp

	LDA right_encountered, X
	BNE :+
	DEC right_edges_tile, X
	BNE :+
		DEC right_encountered, X
		LDY right_edges_px, X
		LDA right_table, Y
:	ORA temp

	BEQ :+
		SEC
:	STA tile_buffer_alpha, X

	DEX
	BPL loop

; Maybe better modified version
; Uses D flag instead of C or V because it needs to use abs, Y indexing for RMW instructions. Fortunately the side effects are negligable in this case
	SED
	LDY #$07
loop:
	LDA left_encountered, Y
	BEQ :+
	DCP left_edges_tile, Y
	BNE :+
		ISC left_encountered, Y
		LDX left_edges_px, Y
		LDA left_table, X
:	STA temp

	LDA right_encountered, Y
	BNE :+
	DCP right_edges_tile, Y
	BNE :+
		DCP right_encountered, Y
		LDX right_edges_px, Y
		LDA right_table, X
:	ORA temp

; 63 extra cycles
	STA temp
	EOR #$FF
	AND (pattern_buffer_alpha_ptr), Y
	TAX
	BEQ :+
		CLD
:	LDA temp
	AND (pattern_buffer_alpha_ptr), Y
	STA (pattern_buffer_alpha_ptr), Y

	TXA
	AND (color_lo), Y
	ORA (pattern_buffer_lo_ptr), Y
	STA (pattern_buffer_lo_ptr), Y

	TXA
	AND (color_hi), Y
	ORA (pattern_buffer_hi_ptr), Y
	STA (pattern_buffer_hi_ptr), Y

	DEY
	BPL loop

; Modified version that incorporates more than just generating the tile mask
; Might not be as effective
	CLC
	LDX #$07
loop:
	LDA left_encountered, X
	BEQ :+
	DEC left_edges_tile, X
	BNE :+
		INC left_encountered, X
		LDY left_edges_px, X
		LDA left_table, Y
:	STA temp

	LDA right_encountered, X
	BNE :+
	DEC right_edges_tile, X
	BNE :+
		DEC right_encountered, X
		LDY right_edges_px, X
		LDA right_table, Y
:	ORA temp

	BNE :+
		SEC
:	STX temp
	LDY temp
	ORA (tile_alpha), Y
	EOR #$FF
	STA temp
	AND (tile_color_lo_plane), Y
	ORA (tile_lo_plane), Y
	STA (tile_lo_plane), Y

	LDA temp
	AND (tile_color_hi_plane), Y
	ORA (tile_hi_plane), Y
	STA (tile_hi_plane), Y

	LDA temp
	ORA (tile_alpha), Y
	STA (tile_alpha), Y

	DEX
	BPL loop




;	rasterizePoly (ScreenPoly* polyPtr) {
;		int topmostPx;
;		int bottommostPx:
;		int leftmostPx;
;		int rightmostPx;
;		int leftEdges[8];
;		int rightEdges[8];
;
;		for (int xTile = leftmostPx / 8; xTile < rightmostPx / 8; xTile++) {
;			for (int yPx = 0; yPx < 8; yPx++) {
;				tile[xTile][yTile][yPx] = leftEdges[yPx] & rightEdges[yPx]
;				leftEdges[yPx] -= 8;
;				rightEdges[yPx] -= 8;
;			}
;		}
;	}

.PUSHSEG
.RODATA
left_table:
	.BYTE	%11111111, %01111111, %00111111, %00011111
	.BYTE	%00001111, %00000111, %00000011, %00000001, %00000000
right_table:
	.BYTE	%00000000, %10000000, %11000000, %11100000
	.BYTE	%11110000, %11111000, %11111100, %11111110, %11111111
.POPSEG
.ENDPROC
.ENDIF

.IF 0
; Plots a tile to the pattern table buffer, updating the nametable buffer if needed
;	May be more efficient to inline this later, if it's only called from rasteriez_poly
;	Takes: Tile x pos in Y, tile Y pos in X, tile data in tile_buffer and tile_buffer_alpha
;	Returns: Nothing, modifies pattern_buffer, pattern_buffer_alpha, allocated_patterns, and name_buffer directly
;	Clobbers: A, X, Y, $1E - $1F
.PROC	plot_tile
	name_buffer_ptr		:= $1E	; And $1F

get_tile_id:
	LDA screen_lo, X
	CLC
	ADC #<name_buffer
	STA name_buffer_ptr + 0

	LDA screen_hi, X
	ADC #>name_buffer
	STA name_buffer_ptr + 1

	LAX (name_buffer_ptr), Y
	BEQ blank
	JMP not_blank

; If the tile at (x, y) is fully transparent, then we must allocate a new tile, update the tile at (x, y), and overwrite the contents of the newly allocated tile
blank:
	INC allocated_patterns
	BEQ tile_overflow
	LAX allocated_patterns
	STA (name_buffer_ptr), Y
	.REPEAT	8, i
	; lo_plane
		LDA tile_buffer + i + 0
		STA pattern_buffer + i * RENDER_MAX_TILES + 0, X

	; hi_plane
		LDA tile_buffer + i + 8
		STA pattern_buffer + i * RENDER_MAX_TILES + 8, X

	; alpha
		LDA tile_buffer_alpha + i
		STA pattern_buffer_alpha + i * RENDER_MAX_TILES, X
	.ENDREP
	RTS

; Maybe have error reporting here?
tile_overflow:
	RTS

; If the tile at (x, y) is only partially transparent, then we must update the existing tile by masking off occluded pixels in the tile buffer using the 
; existing tile's alpha, and ORing the result with the existing tile. As the alpha buffer is inverted, it must be ANDed instead.
not_blank:
	.REPEAT	8, i
	; lo_plane
		LDA tile_buffer + i + 0
		AND pattern_buffer_alpha + i * RENDER_MAX_TILES, X
		ORA pattern_buffer + i * RENDER_MAX_TILES + 0, X
		STA pattern_buffer + i * RENDER_MAX_TILES + 0, X

	; hi_plane
		LDA tile_buffer + i + 8
		AND pattern_buffer_alpha + i * RENDER_MAX_TILES, X
		ORA pattern_buffer + i * RENDER_MAX_TILES + 8, X
		STA pattern_buffer + i * RENDER_MAX_TILES + 8, X

	; alpha
		LDA tile_buffer_alpha + i
		AND pattern_buffer_alpha + i * RENDER_MAX_TILES, X
		STA pattern_buffer_alpha + i * RENDER_MAX_TILES, X
	.ENDREP
	RTS
.ENDPROC
.ENDIF

; Plots a point on the screen
;	Takes: X position in A, and Y position in Y
;	Clobbers: A, X, Y
.PROC	plot_point
	LDY oam_index

	STA oam + OAM::Y_POS, Y
	TXA
	STA oam + OAM::X_POS, Y
	LDA #$00
	STA oam + OAM::ATTR, Y
	LDA #$05
	STA oam + OAM::TILE, Y

	INY
	INY
	INY
	INY

	STY oam_index

	RTS
.ENDPROC





.RODATA
; Need to rename these, they're multiplication tables for indexing into the screen buffer
screen_lo:
.REPEAT SCREEN_HEIGHT_TILES, i
	.LOBYTES	SCREEN_WIDTH_TILES * i
.ENDREP
screen_hi:
.REPEAT	SCREEN_HEIGHT_TILES, i
	.HIBYTES	SCREEN_WIDTH_TILES * i
.ENDREP

; Cube model
.PROC	cube
	.BYTE	$06					; Number of polygons
	.WORD	cube_vertices_x
	.WORD	cube_vertices_y
	.WORD	cube_vertices_z
	.WORD	cube_polys

	CUBE_SCALE		= $3F

	cube_vertices_x:
	.LOBYTES	+CUBE_SCALE, +CUBE_SCALE, -CUBE_SCALE, -CUBE_SCALE, +CUBE_SCALE, +CUBE_SCALE, -CUBE_SCALE, -CUBE_SCALE
	cube_vertices_y:
	.LOBYTES	+CUBE_SCALE, -CUBE_SCALE, -CUBE_SCALE, +CUBE_SCALE, +CUBE_SCALE, -CUBE_SCALE, -CUBE_SCALE, +CUBE_SCALE
	cube_vertices_z:
	.LOBYTES	+CUBE_SCALE, +CUBE_SCALE, +CUBE_SCALE, +CUBE_SCALE, -CUBE_SCALE, -CUBE_SCALE, -CUBE_SCALE, -CUBE_SCALE

	cube_polys:
	.BYTE	$04
	.BYTE	$00, $01, $02, $03
	.BYTE	$04
	.BYTE	$04, $05, $06, $07
	.BYTE	$04
	.BYTE	$00, $01, $05, $04
	.BYTE	$04
	.BYTE	$02, $03, $07, $06
	.BYTE	$04
	.BYTE	$01, $02, $06, $05
	.BYTE	$04
	.BYTE	$00, $03, $07, $04
.ENDPROC