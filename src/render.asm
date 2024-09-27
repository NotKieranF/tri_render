; (c) Kieran Firkin
; TODO: Credits (poly render nesdev thread, c-hacking math, elite website), Proper License
.INCLUDE	"nes.h"
.INCLUDE	"render.h"
.INCLUDE	"nmi.h"
.INCLUDE	"math.h"
.INCLUDE	"objects.h"

; Overview of rendering pipeline:
;	* 

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
;	* Rotation/orientation matrices give scaling effectively for free
;		- Mesh scale can be applied at two different locations, either:
;			- After the mesh and camera orientation matrices have been composed, which maintains as much precision for as long as possible, but requires an extra nine
;			  multiplications by three factors per mesh per camera per frame
;			- After orthonormalization, which only requires nine multiplications by three factors per orthonormilization step (probably once per frame), but reduces
;			  the precision of the mesh orientation matrix, potentially causing unacceptable innacuracies during mesh rotations and furthur orthonormalization steps
;			- Probably a third middling option where scaling is applied just before the mesh and camera orientation matrices are composed, reducing precision slightly
;			  but also reducing the workload from a per mesh per camera per frame operation to just a per mesh per frame operation
;		- Camera scale can be useful for PAR image correction, probably a good idea to have NTSC (1.143:1), PAL (1.386:1), 16x9 (1.524?:1), and custom options
;		- Camera scale could also be useful for FOV effects by scaling depth. Will probably require a generalized frustum culling algorithm, compared to the optimized 90* version
;		- Orientation matrices could be non-orthogonal for skewing effects, however this is probably not as useful or easy to implement as scaling
;	* Storing orientation as a matrix a la elite is probably a better idea compared to using euler angles
;		- Applying relative pitch and roll isn't too bad, should probably use https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula, though a pretty big 
;		  improvement can be made by using the fact that we have convenient orthonormal vectors.
;			- For rotating by angle a about the z axis: x' = x * cos(a) + y * sin(a), y' = y * cos(a) - x * sin(a). The subtraction for the latter equation is to 
;			  compensate for the fact that cross(x, z) = y but cross(y, z) = -x. All told, each rotation should take 4 scalar multiplications and 2 vector additions
;	* When doing back face culling, we need to compute the dot product between the surface normal and the line-of-sight vector between the camera and a point that is 
;	  coplanar to the face.
;		- Elite does something clever, where the surface normal is scaled in such a way that it also represents a point coplanar to the face, allowing them to 
;		  accept/reject faces without pushing more than one vector through the transformation matrix. This has two caveats: surface normals are no longer normalized, 
;		  and so lighting effects would require renormalization; and this technique imposes certain geometric limitations on models.
;		- An alternative option is to store a normalized surface normal, and use the face barycenter as the point on the surface. This solves the renormalization 
;		  issue, and is equally performant when a face is accepted - as the barycenter needs to be transformed regardless for sorting purposes - but results in an 
;		  extra transformation when a face is rejected.
;		- Another, better alternative is to instead use the first vertex in the face as the point on the surface. This performs just as well when a face is accepted, 
;		  but allows for the result to be cached and used again when a face is rejected. By structuring the mesh in such a way to minimize the number of distinct 
;		  first vertices, we can reduce the total performance hit.
;			- In particular, we should structure the mesh so that faces that share a first vertex are stored consecutively. The entire facing vector can then be
;			  cached for the current vertex block, requiring only four bytes: three for the vector, and one for the vertex index corresponding to the current vertex
;			  block.
;	* Maybe use LU factorization for applying transformation matrix to points? - No
;	* Probably need to compute inverse of camera matrix
;	* Can easily extend fast reciprocal used in projection to work with values less than 256 by adding an additional lookup table specifically for them
;		- Not clear how easily those values could be actually projected though
;	* For rendering distance stars as points, we want to be able to cull them as soon as possible as converting them into cameraspace coordinates is likely the most 
;	  taxing part of their rendering. To do this, we can determine which octant the view vector of the camera is in, and immediately discard all stars in the opposing
;	  octant.
;		- This can probably be extended by splitting each octant into sections based on the relative magnitudes of the x, y, and z coordinates, allowing us to reject
;		  all areas which are not adjacent to the one housing the view vector.
;		- Flickering stars every frame may also be a viable technique to reduce computational load. May not look good at lower frame rates.
;		- Might be a good idea to scratch the octant comparison nonsense, and just take the dot product between the view vector and a pseudo-normalized vector from 
;		  the camera to the star?
;		- Could use some sort of hierarchical queue system to keep track of active and non-active stars
;			- Active queue of stars that are checked and (probably) rendered every frame. Stars that aren't rendered get demoted to the inactive queue
;			- Inactive queue of stars where each frame one (or more) are checked to see if they should be rendered and promoted to the active queue (visible),
;			  promoted to the semi-active queue (dot product with view vector is positive), or recycled (dot product with view vector is negative).
;			- Semi-active queue of stars where each frame one (or more) are checked to see if they should be rendered and promoted to the active queue (visible),
;			  demoted to the inactive queue (dot product with view vector is negative), or recycled (dot product with view vector is positive).
;			- This could also probably be applied to culling more than just stars? Though it's unlikely that enough objects will be able to be concurrently loaded to
;			  make this worthwhile, not to mention it's not guaranteed that all objects will be rendered on the first frame they're visible
;	* For projecting vertices into extended screen space, it should actually be possible to move the near clipping plane -- from Z = 256 to Z = 2 -- without risking
;	  overflowing the 16-bit extended screen space coordinates, due to frustum culling at the mesh level. The most extreme extended screen space position for a given
;	  vertex after frustum culling with a visibility radius of 80 * sqrt(2) is represented by (Z + 80 * sqrt(2) + 80) * 256 / Z for a given Z value -- which evaluates to
;	  24977 at Z = 2, just within the -32768 - 32768 range, and decreases asymptotically at higher Z values.
;		- Z = 3 is the cutoff for a visibility radius of 80 * sqrt(2) * sqrt(2) and a maximum vertex offset of 80 * sqrt(2), i.e. if we allow all possible vertex
;		  positions within a meshes coordinate system, instead of restricting vertices to the sphere that is circumscribed by the meshes coordinate system.
;		- In terms of implementation details, it would probably be best to first check for Z < 256, and then jump to a routine that uses a dedicated reciprocal lookup
;		  table and performs a full 16.0-by-0.16-bit multiplication with 16.0 bit result, instead of the optimized 16.0-by-0.8-bit multiplication that the main path
;		  performs.
;	* For normalizing vectors, sqrt15 on https://github.com/TobyLobster/sqrt_test?tab=readme-ov-file is particularly fast, at a modest cost of 512 bytes. Additionally,
;	  a similar technique can be directly applied directly to the inverse square root function: we first shift the input left two bits at a time until the hi byte is
;	  within the range $40-$FF. We then use this hi byte as an index into an inverse square root table to produce an intermediate answer. Finally, we shift the
;	  intermediate answer to the left as many times as we shifted the input, one bit at a time. This results in a final answer with < 5% error for input values greater
;	  than ~80.
;	* For remappable control schemes: keep an array of button combos in ram, with each index representing some remappable action. Action code can then check whether
;	  the button combo in ram matches the current controller state.
;		- Will probably be necessary to have two arrays, one for positive checks, one for negative checks
;		- Will probably have to do some post-processing on controller mappings; e.g. if L+A maps to action one, and A maps to action two, then action two shouldn't 
;		  activate when L+A are pressed--i.e. ~L+A maps to action two--but if A only maps to action two, then L shouldn't matter
;			- For each action map, iterate through all other action maps. If the current action map & the other action map = 0, then there are no conflicts and we 
;			  can move on. Otherwise we XOR that intermediate value with the other action map. If that result is zero, then the other action map is a subeset of the 
;			  current and we can also move on, Otherwise, we need to add the resulting buttons to our negative mapping.
;			- In other words:
;				LDX #$00
;			@outer:
;				LDY #$00
;			@inner:
;				LDA positive_action_map, X
;				AND positive_action_map, Y
;				BEQ :+
;					EOR positive_action_map, Y
;					BEQ :+
;						ORA negative_action_map, X
;						STA negative_action_map, X
;			:	INY
;				CPY #.sizeof(ACTIONS)
;				BNE @inner
;				INX
;				CPX #.sizeof(ACTIONS)
;				BNE @outer
;		- For actions that work based on buttons_held, one must check that all buttons are held, i.e. (buttons_held & ACTION_MASK) ^ ACTION_MASK = 0
;		- For actions that work based on buttons_down, one must check that all buttons are held, and that at least one button has been pressed this frame,
;		  i.e. ((buttons_held & ACTION_MASK) ^ ACTION_MASK = 0) & (buttons_down & ACTION_MASK != 0)
;		- For actions that work based on buttons_up, one must check that at least one button has been released this frame, and that the rest are held,
;		  i.e. (((buttons_held | buttons_up) & ACTION_MASK) ^ ACTION_MASK = 0) & (buttons_up & ACTION_MASK != 0)
;	* Maybe a good idea to ditch Bresenham's algorithm in favor of something which only steps along the Y axis?
;		- If it's not more efficient in general, then we can have it only for very shallow slopes

; Mesh format outline:
;	* Broken up generally into vertex list, face list, and line list, plus misc info like radius.
;	* Vertex list is comprised of three parallel arrays, one for each vector component.
;	* Face list is comprised of a face count value, followed by a sequence of face structs.
;		- Face structs start with a normal vector, followed by vertex count and color info, followed by a sequence of vertex indices, followed by a barycenter.
;	* Line list is comprised of a line count value, followed by a sequence of line structs.
;		- Line structs start with vertex count and color info, followed by a sequence of vertex indices.

; Temp note: Keep a count of the number of cycles we have left in extended vblank. Subtract the length of each prospective chunk of work (tile upload, nametable row 
; upload, etc.) before executing. If result doesn't underflow, then store back modified value and execute chunk. Otherwise, this chunk cannot complete in the allotted
; time and so we must start the cleanup process. Cleanup process involves waiting out the remaining extended vblank time before setting scroll and enabling rendering.
; Note, it should only be necessary to wait to align with the next scanline boundary if we set the scroll properly. This is helpful to avoid wasting time on frames
; with light vram transfer load.

; Thread outline:
;	Main upload thread:
;		* Kicked off by DPCM IRQ, takes place after UI upload thread
;		* Responsible for uploading main graphics buffer contents to PPU
;		- Check if buffers have been populated
;			- If not, return immediately
;			- Otherwise...
;		- Upload nametable buffer in its entirety
;		- Upload opaque patterns
;		- Indicate that the nametable and opaque pattern buffers have been consumed and so are safe to populate
;		- Upload non-opaque patterns top-to-bottom, decrementing a counter to indicate to which tiles have been consumed and are safe to populate
;		- Swap nametables and pattern tables PPU side to display new screen
;
;	Main thread:
;		* Kicked off by RESET
;		- Step game state
;		- Build display list of clipped screen-space polygons and lines
;		- Wait for main upload thread to indicate that it has consumed the nametable and opaque pattern buffers
;			- Shouldn't need to wait on this condition all too often. If waiting here becomes an issue, it'd be possible to incorporate small chunks of work inbetween
;			  checks (e.g. moving objects based on their speed, stepping enemy behaviours)
;		- Rasterize display list, checking the main upload thread tile counter each time we allocate a new tile
;
;	UI thread:
;		* Kicked off by DPCM IRQ, takes place after main upload thread
;
;	UI upload thread:
;		* Kicked off by DPCM IRQ, takes place first
;
;	Music thread:
;		* Kicked off by DPCM IRQ, takes place after UI thread
;
;	Frame counter thread:
;		* Kicked off by NMI
;		* Needs to take a constant time

RASTERIZE_ROWS = 1

.ZEROPAGE
display_list_indices:			.RES 32
display_list_size:				.RES 1
left_edges:						.RES 8 * RASTERIZE_ROWS	; Buffer containing the leftmost pixels of a tile row to be rasterized
right_edges:					.RES 8 * RASTERIZE_ROWS	; Buffer containing the rightmost pixels of a tile row to be rasterized

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

allocated_partial_patterns:		.RES 1
allocated_opaque_patterns:		.RES 1





.SEGMENT	"SAVERAM"
.ALIGN	256
; Pattern table buffers
pattern_buffer:					.RES RENDER_MAX_TILES * 16
pattern_buffer_alpha:			.RES RENDER_MAX_TILES * 8	; 1 is transparent, 0 is opaque

; Nametable buffer
nametable_buffer:				.RES SCREEN_WIDTH_TILES * SCREEN_HEIGHT_TILES





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
	topmost_px					:= $1A
	bottommost_px				:= $1B
	leftmost_px					:= $1C
	rightmost_px				:= $1D
	fill_color_lo_ptr:			:= $12	; And $13
	fill_color_hi_ptr:			:= $14	; And $15
	name_buffer_ptr				:= $16	; And $17
	pattern_buffer_alpha_ptr:	:= $18	; And $19
	pattern_buffer_lo_ptr:		:= $1A	; And $1B
	pattern_buffer_hi_ptr:		:= $1C	; And $1D
	poly_ptr					:= $1E	; And $1F

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
; Also includes prelude nametable lookup
get_tile_id:
	LDA screen_lo, X
	CLC
	ADC #<name_buffer
	STA name_buffer_ptr + 0

	LDA screen_hi, X
	ADC #>name_buffer
	STA name_buffer_ptr + 1

	LDA (name_buffer_ptr), Y
	CMP #BLANK_TILE
	BEQ not_occluded
	BCC fully_occluded

partially_occluded:
	INC allocated_patterns
	LDA allocated_patterns
	STA (name_buffer_ptr), Y

; pattern_buffer_alpha_ptr = pattern_buffer_alpha + tile * 8
; pattern_buffer_lo_ptr = pattern_buffer + tile * 16 + 0
; pattern_buffer_hi_ptr = pattern_buffer + tile * 16 + 8

	LDA table_lo, X
	STA pattern_buffer_lo_ptr + 0
	CLC
	ADC #$08
	STA pattern_buffer_hi_ptr + 0
	LDA table_hi, X
	STA pattern_buffer_lo_ptr + 1
	ADC #$00
	STA pattern_buffer_hi_ptr + 1



not_occluded:
	SED
	LDY #$07
@loop:
	LDA left_encountered, Y
	BEQ :+
	DCP left_edges_tile, Y
	BNE :+
		ISC left_encountered, Y
		LDX left_edges_px
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

@a:
	STA (pattern_buffer_alpha_ptr), Y
	EOR #$FF
	BEQ :+
		CLD
:	TAX
	AND (color_lo), Y
	STA (pattern_buffer_lo_ptr), Y

	TXA
	AND (color_hi), Y
	STA (pattern_buffer_hi_ptr), Y

	DEY
	BPL @loop





	SED
	LDY #$07
loop:
	LDA left_encountered, Y
	BEQ :+
	; This isn't safe --- I think it is actually
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

; Non-updating version. 35 cycles
	STA (pattern_buffer_alpha_ptr), Y
	EOR #$FF
	BEQ :+
		CLD
:	TAX
	AND (color_lo), Y
	STA (pattern_buffer_lo_ptr), Y

	TXA
	AND (color_hi), Y
	STA (pattern_buffer_hi_ptr), Y

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

;
;	Takes: Pointer to polygon struct in $00 - $01, with vertices stored in clockwise order, sorted such that the topmost vertex is first, preferring the leftmost 
;		   vertex if ambiguous.
;	Returns: Nothing
;	Clobbers: A, X, Y, $00 - $1F
.PROC	rasterize_poly
	TILE_MASK					= %11111000
	PIXEL_MASK					= %00000111

	poly_ptr					:= $00	; And $01
	left_index					:= $02
	right_index					:= $03
	poly_color					:= $04
	poly_size					:= $05
	leftmost_pixel				:= $06
	rightmost_pixel				:= $07
	tile_row					:= $08

	bres_current_x_l			:= $09
	bres_current_x_r			:= $0A
	bres_current_y_l			:= $0B
	bres_current_y_r			:= $0C
	bres_target_x_l				:= $0D
	bres_target_x_r				:= $0E
	bres_target_y_l				:= $0F
	bres_target_y_r				:= $10
	bres_target_y_relative_l	:= $11
	bres_target_y_relative_r	:= $12
	bres_routine_ptr_l			:= $13	; And $14
	bres_routine_ptr_r			:= $15	; And $16
	bres_slope_l				:= $17
	bres_slope_r				:= $18
	bres_residual_l				:= $19
	bres_residual_r				:= $1A

	LDY #$00
get_size:
	LDA (poly_ptr), Y
	STA poly_size
	STA left_index

get_color:
	INY
	LDA (poly_ptr), Y
	STA poly_color

get_x:
	INY
	LDA (poly_ptr), Y
	STA bres_current_x_l
	STA bres_current_x_r

get_y:
	INY
	LAX (poly_ptr), Y
	STY right_index
	STA bres_target_y_l
	STA bres_target_y_r
	ORA #TILE_MASK				; current_y = target_y % 8 - 8
	STA bres_current_y_l
	STA bres_current_y_r
	AND #PIXEL_MASK
	TAY							; Y now holds the number of blank rows above the tompost vertex
	TXA
	LSR
	LSR
	LSR
	STA tile_row

prefill_edges:
	LDA #$00					; Loop through unused entries of left_edges and right_edges and initialize them to opposite sides of the screen
	LDX #$FF					; This ensures that upon rasterization, there are no pixels between left_edges[i] and right_edges[i] in an unused row i
@loop:
	DEY
	BMI @exit
	STA right_edges, Y			; right_edges should be initialized to the left side of the screen, i.e. $00
	STX left_edges, Y			; left_edges should be initialized to the right side of the screen, i.e. $FF
	BNE @loop
@exit:	
	STA rightmost_pixel			; Similarly, rightmost_pixel and leftmost_pixel need to be initialized to opposite sides of the screen
	STX leftmost_pixel			; This ensures they are immediately reloaded when we start stepping along the left and right edges

read_initial_vertices:
	JSR read_vertex_right_initial
	BCS exit					; If first right vertex is at the same level as the topmost vertex, all points are colinear
:	JSR read_vertex_left_initial		; Keep reading vertices to the left until we find one that's below the current. We are guaranteed to find one as we've already 
	BCS :-						; established the first right vertex is below the topmost vertex

loop:
	JSR rasterize_tile_row
	INC tile_row

	; Initialize leftmost_pixel and rightmost_pixel to ensure they're overwritten
	LDX #$00
	STX rightmost_pixel
	DEX							; X = $FF
	STX leftmost_pixel

	JSR step_bres_right
	BCS read_final_left_vertex	; If we've reached the bottom on the right side, i.e. C = 1, we must read step along the left side one last time before rasterization
	JSR step_bres_left
	BCS postfill_edges			;
	JMP loop

read_final_left_vertex:
	JSR step_bres_left

postfill_edges:
	LDX bres_current_y_r
	CPX bres_current_y_l
	BCC :+
		LDX bres_current_y_l
:	LDA #$00
	LDY #$FF
@loop:
	STA z:right_edges + $08, X
	STY z:left_edges + $08, X	
	INX
	BNE @loop
@exit:

final_row_rasterization:
	JSR rasterize_tile_row

exit:
	RTS

	; Cleans up after we've stepped through a whole tile
	.PROC	bres_exit_left
		STY bres_current_x_l
		STA bres_residual_l
		LDA #<-($08 * RASTERIZE_ROWS)	; Reset index back to -8 for the next row
		STA bres_current_y_l

		CPY leftmost_pixel				; The most extreme point in a tile row can only ever fall on the first line, final line, or a line with a vertex
		BCC :+							; This check handles the last line
			STY leftmost_pixel
			CLC							; Indicate that we've completed a row in full with C = 0
	:	RTS
	.ENDPROC

	; TODO: Clear carries before slope additions aren't strictly necessary as we can be sure of the carry state beforehand

	; Bresenham routines for left traversal
	.PROC	bres_routine_left_xp
	:	CPY bres_target_x_l
		BEQ read_vertex_left
		INY
		CLC
		ADC bres_slope_l
		BCC :-
			STY left_edges + $08 * RASTERIZE_ROWS, X
			INX
			BNE :-
			BEQ bres_exit_left
	.ENDPROC

	.PROC	bres_routine_left_xn
	:	CPY bres_target_x_l
		BEQ read_vertex_left
		DEY
		CLC
		ADC bres_slope_l
		BCC :-
			STY left_edges + $08 * RASTERIZE_ROWS, X
			INX
			BNE :-
			BEQ bres_exit_left
	.ENDPROC

	.PROC	bres_routine_left_yp
	@loop:
		CPX bres_target_y_relative_l
		BEQ read_vertex_left
		STY left_edges + $08 * RASTERIZE_ROWS, X
		CLC
		ADC bres_slope_l
		BCC :+
			INY
	:	INX
		BNE @loop
		BEQ bres_exit_left
	.ENDPROC

	.PROC	bres_routine_left_yn
	@loop:
		CPX bres_target_y_relative_l
		BEQ read_vertex_left
		STY left_edges + $08 * RASTERIZE_ROWS, X
		CLC
		ADC bres_slope_l
		BCC :+
			DEY
	:	INX
		BNE @loop
		BEQ bres_exit_left
	.ENDPROC

	; Reads a vertex from poly_ptr at left_index, and sets up left bresenham properties based the current position and the values read
	; Note: Assumes that bres_target_x_l and bres_target_y_l represent the current position. Assumes that left index points to the final byte of the vertex, and 
	;		leaves it pointing at the final byte of the next vertex in counterclockwise order
	;	Takes:
	;	Returns:
	;	Clobbers: A, X, Y, $1F
	.PROC	read_vertex_left
		routine_index		:= $1F
		dy					:= $1E

		STX bres_current_y_l
		STY bres_current_x_l

	::read_vertex_left_initial = *
		LDY left_index
	@get_y:
		LAX (poly_ptr), Y
		DEY
		SEC
		SBC bres_target_y_l				; Compute dy = target - current. N.B. this is the opposite of normal
		BCC bottom						; If the target is at or above the current, i.e. dy <= 0, we've reached the bottom of the polygon and need to exit
		BEQ bottom
		STA dy
		CLC
		ADC bres_current_y_l
		CLC
		ADC #$08
		STA bres_target_y_relative_l	; Reduced y position is relative to the current y position
		STX bres_target_y_l

	@get_x:
		LAX (poly_ptr), Y
		DEY
		SEC
		SBC bres_current_x_l			; Compute dx = target - current. N.B. this is the opposite of normal
		BCS :+
			EOR #$FF					; Compute |dx| if dx < 0
		;	CLC							; assert(pscarry == 0)
			ADC #$01
			CLC
	:	ROL routine_index				; C = 1 if dx > 0, i.e. a negative traversal along the x axis
		STX bres_target_x_l
		STY left_index

	@check_steepness:
		LDX dy							; Going in here: A = |dx|, X = |dy|
		CMP dy
		BCC :+							; Swap A and X if dx > dy
			TAX
			LDA dy
	:	ROL routine_index				; C = 1 if dx < dy, i.e. the y axis is the major axis

	@get_slope:
		JSR udiv_8x8bit_frac
		STX bres_slope_l

	@set_residual:
		LDA #$80
		STA bres_residual_l

	@get_routine:
		LDA routine_index				; Bit 0 indicates the major axis, bit 1 indicates the traversal direction along the x axis
		AND #%00000011					; Only the two lower order bits contain index information
		TAX
		LDA left_routine_table_lo, X
		STA bres_routine_ptr_l + 0
		LDA left_routine_table_hi, X
		STA bres_routine_ptr_l + 1
		JMP step_bres_left				; This may be able to be removed after a slight refactor, with execution falling straight through to step_bres_left

	bottom:
		SEC								; Indicate that we have not completed a row in full with C = 1
		RTS								; Return to the original caller instead of continuing to step along the perimeter
	.ENDPROC

	; Initiates a step along the left edge
	;	Takes:
	;	Returns: C = 0 if a full row was processed, C = 1 if the bottom of the polygon was reached
	;	Clobbers: A, X, Y, $1F
	.PROC	step_bres_left
		LAX bres_target_y_relative_l	; Subtract 8 from the relative target position as we've moved down a row since last iteration
		AXS #$08 * RASTERIZE_ROWS		; This needs to happen before we step along a line due to the fact that we compare a negative index against the target
		STX bres_target_y_relative_l

		LDX bres_current_y_l			; Load saved bresenham parameters
		LDY bres_current_x_l
		LDA bres_residual_l

		CPY leftmost_pixel				; The most extreme point in a tile row can only ever fall on the first line, final line, or a line with a vertex
		BCC :+							; This check handles the first line, as well as lines with vertices, as this routine is called by read_vertex_left
			STY leftmost_pixel

	:	JMP (bres_routine_ptr_l)
	.ENDPROC

	; Cleans up after we've stepped through a whole tile
	.PROC	bres_exit_right
		STY bres_current_x_r
		STA bres_residual_r
		LDA #<-($08 * RASTERIZE_ROWS)	; Reset index back to -8 for the next row
		STA bres_current_y_r

		CPY rightmost_pixel				; The most extreme point in a tile row can only ever fall on the first line, final line, or a line with a vertex
		BCC :+							; This check handles the last line
			STY rightmost_pixel
			CLC							; Indicate that we've completed a row in full with C = 0
	:	RTS
	.ENDPROC

	; Bresenham routines for right traversal
	.PROC	bres_routine_right_xp
	:	CPY bres_target_x_r
		BEQ read_vertex_right
		INY
		CLC
		ADC bres_slope_r
		BCC :-
			STY right_edges + $08 * RASTERIZE_ROWS, X
			INX
			BNE :-
			BEQ bres_exit_right
	.ENDPROC

	.PROC	bres_routine_right_xn
	:	CPY bres_target_x_r
		BEQ read_vertex_right
		DEY
		CLC
		ADC bres_slope_r
		BCC :-
			STY right_edges + $08 * RASTERIZE_ROWS, X
			INX
			BNE :-
			BEQ bres_exit_right
	.ENDPROC

	.PROC	bres_routine_right_yp
	@loop:
		CPX bres_target_y_relative_r
		BEQ read_vertex_right
		STY right_edges + $08 * RASTERIZE_ROWS, X
		CLC
		ADC bres_slope_r
		BCC :+
			INY
	:	INX
		BNE @loop
		BEQ bres_exit_right
	.ENDPROC

	.PROC	bres_routine_right_yn
	@loop:
		CPX bres_target_y_relative_r
		BEQ read_vertex_right
		STY right_edges + $08 * RASTERIZE_ROWS, X
		CLC
		ADC bres_slope_r
		BCC :+
			DEY
	:	INX
		BNE @loop
		BEQ bres_exit_right
	.ENDPROC

	; Reads a vertex from poly_ptr at right_index, and sets up right bresenham properties based the current position and the values read
	; Note: Assumes that bres_target_x_r and bres_target_y_r represent the current position. Assumes that right index points to the last byte of the previous vertex, 
	;		and leaves it pointing at the last byte of the current vertex
	;	Takes:
	;	Returns:
	;	Clobbers: A, X, Y, $1E - $1F
	.PROC	read_vertex_right
		routine_index		:= $1F
		dx					:= $1E

		STX bres_current_y_r
		STY bres_current_x_r

	::read_vertex_right_initial = *
		LDY right_index
	@get_x:
		INY
		LAX (poly_ptr), Y
		SEC
		SBC bres_current_x_r			; Compute dx = target - current
		BCS :+
			EOR #$FF					; Compute |dx| if dx < 0
		;	CLC							; assert(pscarry == 0)
			ADC #$01
			CLC
	:	ROL routine_index				; C = 1 if dx > 0, i.e. a negative traversal along the x axis
		STA dx
		STX bres_target_x_r

	@get_y:
		INY
		LAX (poly_ptr), Y
		SEC
		SBC bres_target_y_r				; Compute dy = target - current. N.B. this is the opposite of normal
		BCC bottom						; If the target is at or above the current, i.e. dy <= 0, we've reached the bottom of the polygon and need to exit
		BEQ bottom
		PHA
		CLC
		ADC bres_current_y_r
		CLC
		ADC #$08
		STA bres_target_y_relative_r	; Reduced y position is relative to the current y position
		STX bres_target_y_r
		STY right_index
		PLA

	@check_steepness:
		LDX dx							; Going in here: A = |dy|, X = |dx|
		CMP dx
		BCC :+							; Swap A and X if dy > dx
			TAX
			LDA dx
	:	ROL routine_index				; C = 1 if dy < dx, i.e. the x axis is the major axis

	@get_slope:
		JSR udiv_8x8bit_frac
		STX bres_slope_r

	@set_residual:
		LDA #$80
		STA bres_residual_r

	@get_routine:
		LDA routine_index				; Bit 0 indicates the major axis, bit 1 indicates the traversal direction along the x axis
		AND #%00000011					; Only the two lower order bits contain index information
		TAX
		LDA right_routine_table_lo, X
		STA bres_routine_ptr_r + 0
		LDA right_routine_table_hi, X
		STA bres_routine_ptr_r + 1
		JMP step_bres_right				; This may be able to be removed after a slight refactor, with execution falling straight through to step_bres_right

	bottom:
		SEC								; Indicate that we have not completed a row in full with C = 1
		RTS								; Return to the original caller instead of continuing to step along the perimeter
	.ENDPROC

	; Initiates a step along the right edge
	;	Takes:
	;	Returns: C = 0 if a full row was processed, C = 1 if the bottom of the polygon was reached
	;	Clobbers: A, X, Y, $1E - $1F
	.PROC	step_bres_right
		LAX bres_target_y_relative_r	; Subtract 8 from the relative target position as we've moved down a row since last iteration
		AXS #$08 * RASTERIZE_ROWS		; This needs to happen before we step along a line due to the fact that we compare a negative index against the target
		STX bres_target_y_relative_r

		LDX bres_current_y_r			; Load saved bresenham parameters
		LDY bres_current_x_r
		LDA bres_residual_r

		CPY rightmost_pixel				; The most extreme point in a tile row can only ever fall on the first line, final line, or a line with a vertex
		BCC :+							; This check handles the first line, as well as lines with vertices, as this routine is called by read_vertex_left
			STY rightmost_pixel

	:	JMP (bres_routine_ptr_r)
	.ENDPROC

	;
	;	Takes:
	;	Returns:
	;	Clobbers: A, X, Y, $1E - $1F
	.PROC	rasterize_tile_row
		STA $4448
		RTS


		leftmost_tile		:= leftmost_pixel
		rightmost_tile		:= rightmost_pixel
		nametable_ptr		:= $1E	; And $1F

	set_leftmost_tile:
		LDA leftmost_pixel
		LSR
		LSR
		LSR
		STA leftmost_tile

	set_rightmost_tile:
		LDA rightmost_pixel
		LSR
		LSR
		LSR
		STA rightmost_tile

	set_nametable_ptr:
		LDX tile_row
		LDA screen_lo, X
		STA nametable_ptr + 0
		LDA screen_hi, X
		CLC
		ADC #>nametable_buffer
		STA nametable_ptr + 1

	loop:
		LDY leftmost_tile
		INC leftmost_tile
		LDA (nametable_ptr), Y
		BEQ @allocate_new
		CMP allocated_opaque_patterns
		BCC loop
	@modify_existing:

	@allocate_new:
	
	.ENDPROC

; Rasterizes an entire tile row based on the contents of left_edges and right_edges
;
;
;rasterize_tile_row:
;	LDA #$80
;	ADC #$80			; assert(psoverflow == 1)
;	LDY #$07
;@loop:
;	LDA left_edges, Y
;	LDX #%11111000
;	AXS #$00
;	BEQ :+				; Invert this condition to increase typical case speed by 2 cycles?
;		LDA #$FF
;		BCC @next
;		LDA #$00
;		BEQ :++
;:	AND #%00000111
;	TAX
;	LDA lmask_table, X
;:	STA temp
;
;	LDA right_edges, Y
;	LDX #%11111000
;	AXS #$00
;	BEQ :+
;		LDA #$FF		; Maybe pull this out into its own thing, after the BCS? Same w/ above. Saves 4 cycles typically
;		BCS @next
;		LDA #$00
;		BEQ :++
;:	AND #%00000111
;	TAX
;	LDA rmask_table, X
;:	ORA temp
;
;@next:
;	BEQ :+
;		CLV
;:	STA alpha_buffer, Y
;	DEY
;	BPL @loop
;	BVC rasterize_tile_row

	.PUSHSEG
		.RODATA
		left_routine_table_lo:
		.LOBYTES	bres_routine_left_yn, bres_routine_left_xn
		.LOBYTES	bres_routine_left_yp, bres_routine_left_xp

		left_routine_table_hi:
		.HIBYTES	bres_routine_left_yn, bres_routine_left_xn
		.HIBYTES	bres_routine_left_yp, bres_routine_left_xp

		right_routine_table_lo:
		.LOBYTES	bres_routine_right_xn, bres_routine_right_yn
		.LOBYTES	bres_routine_right_xp, bres_routine_right_yp

		right_routine_table_hi:
		.HIBYTES	bres_routine_right_xn, bres_routine_right_yn
		.HIBYTES	bres_routine_right_xp, bres_routine_right_yp
	.POPSEG
.ENDPROC

; Draws a line from (x0, y0) to (x1, y1) using bresenham's algorithm
;	Takes: x0, y0, x1, y1 in $1C - $1F
;	Returns: Nothing
;	Clobbers: A, X, Y, $19 - $1F
.PROC	draw_line
	x0			:= $1C
	y0			:= $1D
	x1			:= $1E
	y1			:= $1F
	slope		:= $19
	ptr			:= $1A	; And $1B

compute_dx:
	LDY #$00			; Preload bresenham routine ID for positive dx, positive dy, dx > dy
	LDA x1
	SEC
	SBC x0
	BEQ bres_vert		; If dx = 0, we have a vertical line
	BCS :+
		EOR #$FF		; Negate dx if it's negative
		ADC #$01		; assert(pscarry == 0)
		LDY #$04		; Load bresenham routine ID for negative dx, positive dy, dx > dy
:	TAX					; x = |dx|

compute_dy:
	LDA y1
	SEC
	SBC y0
	BEQ bres_horiz		; If dy = 0, we have a horizontal line
	BCS :+
		EOR #$FF		; Negate dy if it's negative
		ADC #$01		; assert(pscarry == 0)
		INY				; Increment bresenham routine ID by two to go from positive dy -> negative dy
		INY
:						; a = |dy|

compute_slope:
	CMP identity, X
	BCC :+
		STX slope		; Swap dx and dy if dx < dy
		TAX
		LDA slope
		INY				; Increment bresenham routine ID by one to go from dx > dy -> dx < dy
:	JSR udiv_8x8bit_frac
	STX slope

get_routine:
	LDA bres_routine_table_lo, Y
	STA ptr + 0
	LDA bres_routine_table_hi, Y
	STA ptr + 1

	LDX x0
	LDY y0
	LDA #$80
	CLC
	JMP (ptr)

; Draw a horizontal line
bres_horiz:
	LDX x0
	LDY y0
	CPX x1
	BCS @neg_loop

@pos_loop:
	STA $444F
	INX
	CPX x1
	BNE @pos_loop
	RTS

@neg_loop:
	STA $444F
	DEX
	CPX x1
	BNE @neg_loop
	RTS

; Draw a vertical line
bres_vert:
	LDX x0
	LDY y0
	CPY y1
	BCS @neg_loop

@pos_loop:
	STA $444F
	INY
	CPY y1
	BNE @pos_loop
	RTS

@neg_loop:
	STA $444F
	DEY
	CPY y1
	BNE @neg_loop
	RTS

; Bresenham routines for the 8 different octants. Not sure if this mess of assembler directives is better than writing the individual routines out or not...
.REPEAT	8, i
	.PROC	.IDENT(.SPRINTF("bres_routine_%d", i))
	@loop:
		STA $444F
		ADC slope			; assert(pscarry == 0)
		BCC @no_increment_minor

	@increment_minor:
		; Move along the minor axis
		.IF(i & %011 = %000)
			INY
		.ELSEIF(i & %011 = %010)
			DEY
		.ELSEIF(i & %101 = %001)
			INX
		.ELSEIF(i & %101 = %101)
			DEX
		.ENDIF
		CLC

	@no_increment_minor:
		; Move along the major axis
		.IF(i & %101 = %000)
			INX
		.ELSEIF(i & %101 = %100)
			DEX
		.ELSEIF(i & %011 = %001)
			INY
		.ELSEIF(i & %011 = %011)
			DEY
		.ENDIF
		;
		.IF(i & %001 = %000)
			CPX x1
		.ELSEIF(i & %001 = %001)
			CPY y1
		.ENDIF
		BNE @loop
		RTS
	.ENDPROC
.ENDREP

bres_routine_table_lo:
.LOBYTES	bres_routine_0, bres_routine_1, bres_routine_2, bres_routine_3
.LOBYTES	bres_routine_4, bres_routine_5, bres_routine_6, bres_routine_7

bres_routine_table_hi:
.HIBYTES	bres_routine_0, bres_routine_1, bres_routine_2, bres_routine_3
.HIBYTES	bres_routine_4, bres_routine_5, bres_routine_6, bres_routine_7

.ENDPROC

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