.IFNDEF	RENDER_H
RENDER_H = 1

; Maximum number of tiles that the rasterizing engine can allocate
RENDER_MAX_TILES			= 128

; Maximum number of vertices that a polygon can have
RENDER_MAX_VERTICES			= 10

; Size of the display list in bytes
RENDER_DISPLAY_LIST_SIZE	= 256

; Maximum number of polygons that can fit in the display list
RENDER_MAX_POLYS			= RENDER_DISPLAY_LIST_SIZE / 256

; Maximum number of vertices in a given mesh
MESH_MAX_VERTICES			= 32

; Maximum number of vertices that can be in a given polygon, accounting for vertices that may be added during the clipping process (+1 per clipping plane)
POLY_MAX_VERTICES			= 16

; Maximum number of cameras
NUM_CAMERAS					= 1

; Screen size in tiles
SCREEN_WIDTH_TILES			= 32
SCREEN_WIDTH_PX				= SCREEN_WIDTH_TILES * 8
SCREEN_HEIGHT_TILES			= 30
SCREEN_HEIGHT_PX			= SCREEN_HEIGHT_TILES * 8

; Near clipping plane. Some aspects of rendering depend on this being $0100
NEAR_PLANE					= $0100

; Mesh data structure
.STRUCT	MESH
	RADIUS				.RES 1
	ATTR				.RES 1
	VERTICES_X_PTR		.RES 2
	VERTICES_Y_PTR		.RES 2
	VERTICES_Z_PTR		.RES 2
	POLY_VERTEX_PTR		.RES 2
	POLY_NORMAL_PTR		.RES 2
.ENDSTRUCT

;
.STRUCT	SCREEN_POLY
	DEPTH_LO			.RES 1
	DEPTH_HI			.RES 1
	ATTR				.RES 1
	X_POS				.RES 1
	Y_POS				.RES 1
.ENDSTRUCT

;
.STRUCT	ROT_MATRIX
	XX					.RES 1
	YX					.RES 1
	ZX					.RES 1
	XY					.RES 1
	YY					.RES 1
	ZY					.RES 1
	XZ					.RES 1
	YZ					.RES 1
	ZZ					.RES 1
.ENDSTRUCT

; Routines
.GLOBAL		plot_point
.GLOBAL		render_frame

;
.GLOBALZP	camera_pitch_hi, camera_pitch_lo, camera_yaw_hi, camera_yaw_lo, camera_roll_hi, camera_roll_lo
.GLOBALZP	camera_pos_x_sub, camera_pos_x_lo, camera_pos_x_hi, camera_pos_y_sub, camera_pos_y_lo, camera_pos_y_hi, camera_pos_z_sub, camera_pos_z_lo, camera_pos_z_hi

.ENDIF