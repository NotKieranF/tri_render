#include <GL/glut.h>
#include <stdio.h>
#include <math.h>
#include "render.h"
#include "mesh.c"

static Point3D cameraPos = {.x = 0, .y = 0, .z = 0};
static Rot3D cameraRot = {.yaw = 0, .pitch = 0, .roll = 0};
static Object objects[MAX_OBJECTS];
static int displayListSize;
static int displayListIndices[RENDER_MAX_POLYS];
static ScreenPoly displayListPolys[RENDER_MAX_POLYS];
static int frameRate = 20;

// Maximum bits of precision needed in projection multiplication
static int maxPrec = 0;



void handleInput(unsigned char key, int x, int y) {
	switch (key) {
		case 'w':
			cameraPos.z += CAMERA_SPEED * cos(cameraRot.yaw * PI / 128);
			cameraPos.x += CAMERA_SPEED * sin(cameraRot.yaw * PI / 128);
			break;
		case 's':
			cameraPos.z -= CAMERA_SPEED * cos(cameraRot.yaw * PI / 128);
			cameraPos.x -= CAMERA_SPEED * sin(cameraRot.yaw * PI / 128);
			break;
		case 'd':
			cameraPos.x += CAMERA_SPEED * cos(cameraRot.yaw * PI / 128);
			cameraPos.z -= CAMERA_SPEED * sin(cameraRot.yaw * PI / 128);
			break;
		case 'a':
			cameraPos.x -= CAMERA_SPEED * cos(cameraRot.yaw * PI / 128);
			cameraPos.z += CAMERA_SPEED * sin(cameraRot.yaw * PI / 128);
			break;
		case 'q':
			cameraPos.y += CAMERA_SPEED;
			break;
		case 'e':
			cameraPos.y -= CAMERA_SPEED;
			break;
		case 'y':
			cameraRot.pitch -= CAMERA_ROT_SPEED;
			break;
		case 'h':
			cameraRot.pitch += CAMERA_ROT_SPEED;
			break;
		case 'g':
			cameraRot.yaw -= CAMERA_ROT_SPEED;
			break;
		case 'j':
			cameraRot.yaw += CAMERA_ROT_SPEED;
			break;
		case 't':
			cameraRot.roll -= CAMERA_ROT_SPEED;
			break;
		case 'u':
			cameraRot.roll += CAMERA_ROT_SPEED;
			break;
		case ',':
			frameRate -= 1;
			break;
		case '.':
			frameRate += 1;
			break;
		case 'l':
			printf("rot: (%d, %d, %d)\n", cameraRot.yaw, cameraRot.pitch, cameraRot.roll);
			printf("pos: (%d, %d, %d)\n", cameraPos.x, cameraPos.y, cameraPos.z);
			printf("frame rate: %d\n", frameRate);
			fflush(stdout);
			break;
		case 'r':
			cameraPos = (Point3D) {.x = 0, .y = 0, .z = 0};
			cameraRot = (Rot3D) {.yaw = 0, .pitch = 0, .roll = 0};
	}
}

//
void setupRotMatrix(double rotMatrix[3][3], Rot3D rot) {
	double r = rot.roll * PI / 128;
	double y = rot.yaw * PI / 128;
	double p = rot.pitch * PI / 128;

	rotMatrix[0][0] = cos(r) * cos(y);
	rotMatrix[1][0] = cos(r) * sin(y) * sin(p) - sin(r) * cos(p);
	rotMatrix[2][0] = cos(r) * sin(y) * cos(p) + sin(r) * sin(p);

	rotMatrix[0][1] = sin(r) * cos(y);
	rotMatrix[1][1] = sin(r) * sin(y) * sin(p) + cos(r) * cos(p);
	rotMatrix[2][1] = sin(r) * sin(y) * cos(p) - cos(r) * sin(p);

	rotMatrix[0][2] = -sin(y);
	rotMatrix[1][2] = cos(y) * sin(p);
	rotMatrix[2][2] = cos(y) * cos(p);

/*	Full matrix
	rotMatrix[0][0] = cos(r);
	rotMatrix[1][0] = -sin(r);
	rotMatrix[2][0] = 0;

	rotMatrix[0][1] = cos(p) * sin(r);
	rotMatrix[1][1] = cos(p) * cos(r);
	rotMatrix[2][1] = -sin(p);

	rotMatrix[0][2] = sin(p) * sin(r);
	rotMatrix[1][2] = sin(p) * cos(r);
	rotMatrix[2][2] = cos(p);

	cos(r)				-sin(r)				0
	cos(p) * sin(r)		cos(p) * cos(r)		-sin(p)
	sin(p) * sin(r)		sin(p) * cos(r)		cos(p)

 Yaw matrix
	cos(y)	0		sin(y)
	0		1		0
	-sin(y)	0		cos(y)

 Pitch matrix
	1		0		0
	0		cos(p)	-sin(p)
	0		sin(p)	cos(p)

 Roll matrix
	cos(r)	-sin(r)	0
	sin(r)	cos(r)	0
	0		0		1
*/

	return;
}

/*	ASSEMBLY VERSION
	; Might be more efficient to combine this routine with the routine to generate the object matrix
	; If optimizing for multiple cameras, probably want to loop through and do
	; ~1089 cycles

	.PROC	matrix_multiply
		LDA left_matrix_xx
		SET_FAST_MUL

		LDY right_matrix_xx
		FAST_MUL
		CPX #$80
		ROL
		STA product_matrix_xx

		LDY right_matrix_xy
		FAST_MUL
		CPX #$80
		ROL
		STA product_matrix_xy

		LDY right_matrix_xz
		FAST_MUL
		CPX #$80
		ROL
		STA product_matrix_xz

	??:
		LDA left_matrix_xy
		SET_FAST_MUL

		LDY right_matrix_yx
		FAST_MUL
		CPX #$80
		ROL
		CLC
		ADC product_matrix_xx
		STA product_matrix_xx

		LDY right_matrix_yy
		FAST_MUL
		CPX #$80
		ROL
		CLC
		ADC product_matrix_xy
		STA product_matrix_xy

		LDY right_matrix_yz
		FAST_MUL
		CPX #$80
		ROL
		CLC
		ADC product_matrix_xz
		STA product_matrix_xz

		;... etcetera

	.ENDPROC
*/
void matrixMultiply(double leftMatrix[3][3], double rightMatrix[3][3], double productMatrix[3][3]) {
	productMatrix[0][0] = leftMatrix[0][0] * rightMatrix[0][0] + leftMatrix[0][1] * rightMatrix[1][0] + leftMatrix[0][2] * rightMatrix[2][0];
	productMatrix[1][0] = leftMatrix[1][0] * rightMatrix[0][0] + leftMatrix[1][1] * rightMatrix[1][0] + leftMatrix[1][2] * rightMatrix[2][0];
	productMatrix[2][0] = leftMatrix[2][0] * rightMatrix[0][0] + leftMatrix[2][1] * rightMatrix[1][0] + leftMatrix[2][2] * rightMatrix[2][0];

	productMatrix[0][1] = leftMatrix[0][0] * rightMatrix[0][1] + leftMatrix[0][1] * rightMatrix[1][1] + leftMatrix[0][2] * rightMatrix[2][1];
	productMatrix[1][1] = leftMatrix[1][0] * rightMatrix[0][1] + leftMatrix[1][1] * rightMatrix[1][1] + leftMatrix[1][2] * rightMatrix[2][1];
	productMatrix[2][1] = leftMatrix[2][0] * rightMatrix[0][1] + leftMatrix[2][1] * rightMatrix[1][1] + leftMatrix[2][2] * rightMatrix[2][1];

	productMatrix[0][2] = leftMatrix[0][0] * rightMatrix[0][2] + leftMatrix[0][1] * rightMatrix[1][2] + leftMatrix[0][2] * rightMatrix[2][2];
	productMatrix[1][2] = leftMatrix[1][0] * rightMatrix[0][2] + leftMatrix[1][1] * rightMatrix[1][2] + leftMatrix[1][2] * rightMatrix[2][2];
	productMatrix[2][2] = leftMatrix[2][0] * rightMatrix[0][2] + leftMatrix[2][1] * rightMatrix[1][2] + leftMatrix[2][2] * rightMatrix[2][2];

	return;
}

Point3D rotatePoint(double rotMatrix[3][3], Point3D point) {
	Point3D rotatedPoint;

	rotatedPoint.x = (point.x * (int) (rotMatrix[0][0] * 64) + point.y * (int) (rotMatrix[0][1] * 64) + point.z * (int) (rotMatrix[0][2] * 64)) / 64;
	rotatedPoint.y = (point.x * (int) (rotMatrix[1][0] * 64) + point.y * (int) (rotMatrix[1][1] * 64) + point.z * (int) (rotMatrix[1][2] * 64)) / 64;
	rotatedPoint.z = (point.x * (int) (rotMatrix[2][0] * 64) + point.y * (int) (rotMatrix[2][1] * 64) + point.z * (int) (rotMatrix[2][2] * 64)) / 64;

	return rotatedPoint;
}

Point3D translatePoint(Point3D point, Point3D translation) {
	point.x += translation.x;
	point.y += translation.y;
	point.z += translation.z;

	return point;
}

Point3D normalize(Point3D point) {
	int magnitude = sqrt(pow(point.x, 2) + pow(point.y, 2) + pow(point.z, 2));
	magnitude /= 32;

	point.x /= magnitude;
	point.y /= magnitude;
	point.z /= magnitude;

	return point;
}

//
void rasterizePoly(ScreenPoly poly) {
	glBegin(GL_POLYGON);
		// Set fill color
		glColor3ub(poly.color.r, poly.color.g, poly.color.b);

		// Draw vertices
		for (int i = 0; i < poly.numVertices; i++)
			glVertex2i(poly.vertices[i].x, poly.vertices[i].y);
	glEnd();

	return;
}

// Cull a point against the view frustum (90 degree FOV). Returns 1 if outside view frustum, 0 if inside
int frustumCull(Point3D cameraspacePos, int radius) {
	// Cull against near plane
	if (cameraspacePos.z + radius < NEAR_PLANE)
		return 1;

	// Cull against far plane
	if (cameraspacePos.z - radius > FAR_PLANE)
		return 1;

	// Cull against left face
	if (cameraspacePos.x + radius < -cameraspacePos.z)
		return 1;

	// Cull against right face
	if (cameraspacePos.x - radius > cameraspacePos.z)
		return 1;

	// Cull against top face
	if (cameraspacePos.y + radius < -cameraspacePos.z)
		return 1;

	// Cull against bottom face
	if (cameraspacePos.y - radius > cameraspacePos.z)
		return 1;

	// Point is within all faces of the view frustum
	return 0;
}

// 
int dotProduct(Point3D a, Point3D b) {
	return a.x * b.x + a.y * b.y + a.z * b.z;
}

//
int backFaceCull(double rotMatrix[3][3], Point3D normalVector, Point3D sightVector) {
	Point3D rotatedNormal = rotatePoint(rotMatrix, normalVector);
	return dotProduct(rotatedNormal, sightVector) / 64;
}

unsigned int depthFunction(unsigned int depth) {
	depth = sqrt(depth);
	unsigned int lg = log(depth) / log(2);
	return lg * 256 + depth / pow(2, lg - 8);
}

Point2D projectionFunction(Point3D inputVertex) {
	Point2D outputVertex;

//	int reciprocalZ = inputVertex.z ? 65536 / inputVertex.z : 65536;
//	outputVertex.x = (inputVertex.x * reciprocalZ) / 256;
//	outputVertex.y = (inputVertex.y * reciprocalZ) / 256;

	int logZ = fmax(log(inputVertex.z) / log(2) - 8, 0);
	int msbZ = inputVertex.z / pow(2, logZ);
	int reciprocalZ = msbZ ? 65536 / msbZ : 65536;

	outputVertex.x = (inputVertex.x * reciprocalZ) / (256 * pow(2, logZ));
	outputVertex.y = (inputVertex.y * reciprocalZ) / (256 * pow(2, logZ));

	int curPrec = fmax(log(abs((inputVertex.x * reciprocalZ) / 256)) / log(2), log(abs((inputVertex.y * reciprocalZ) / 256)) / log(2)) + 1;
	if (curPrec > maxPrec)
		maxPrec = curPrec;

//	outputVertex.x = inputVertex.z ? inputVertex.x * SCREEN_WIDTH / inputVertex.z : 0;
//	outputVertex.y = inputVertex.z ? inputVertex.y * SCREEN_WIDTH / inputVertex.z : 0;

//	printf("X: %d, Y: %d, Z: %d\n", inputVertex.x, inputVertex.y, inputVertex.z);
//	printf("Z: %d, logZ: %d, msbZ: %d, reciprocalZ: %d\n", inputVertex.z, logZ, msbZ, reciprocalZ);
//	printf("X': %d, Y': %d, Max bits: %d\n", inputVertex.x * reciprocalZ, inputVertex.y * reciprocalZ, maxPrec);
//	fflush(stdout);

//	if (reciprocalZ > 255) {
//		exit(0);
//	}

	return outputVertex;

//	Shift zpos right until it fits within 9 bits (hi bit = 1), counting the iterations
//	Lookup reciprocal of zpos
//	Multiply xpos and ypos by reciprocal to get 24bit result
//	Then shift 24bit result right by the same amount as needed for original zpos

/*
	~202 on a good day?
	~658 on a bad day?
	.PROC	project_vertex
		logz		:= $00
		output_x	:= $01	; And $02, $03
		output_y	:= $04	; And $05, $06

		LDY #$00
		LDX zpos_lo
		LDA zpos_hi
		CMP #$01
		BEQ lookup_recip

	shift_z:
		TXA
		LSR zpos_hi
	:	INY
		ROR
		LSR zpos_hi
		BNE :-
		TAX

	lookup_recip:
		STY logz
		LDA reciprocal, X

	set_mul:
		STA mul_sq1_lo_ptr + 0
		STA mul_sq1_hi_ptr + 0
		EOR #$FF
		STA mul_sq2_lo_ptr + 0
		STA mul_sq2_hi_ptr + 0

	mul_x:
		LDY input_x + 0

		LDA (mul_sq1_hi_ptr), Y
		SEC
		SBC (mul_sq2_hi_ptr), Y
		TAX

		LDY input_x + 1

		LDA (mul_sq1_lo_ptr), Y
		SEC
		SBC (mul_sq2_lo_ptr), Y
		STA output_x + 0

		LDA (mul_sq1_hi_ptr), Y
		SBC (mul_sq2_hi_ptr), Y

		BIT input_x + 1
		BPL :+
			SEC
			SBC mul_sq1_lo_ptr + 1
	:	STA output_x + 1

		TXA
		CLC
		ADC output_x + 0
		BCC :+
			INC output_x + 1
	:

	mul_y:

	shift_output:
		LDY logz
		BEQ exit
		LDA output_x + 1
	:	CMP #$80
		ROR
		ROR output_x + 0
		DEY
		BNE :-
		STA output_x + 1

		LDY logz
		LDA output_y + 1
	:	CMP #$80
		ROR
		ROR output_y + 0
		DEY
		BNE :-
		STA output_y + 1

	exit
		RTS

	.ENDPROC
*/
}

//
ScreenPoly drawPoly(double rotMatrix[3][3], Point3D cameraspacePos, MeshPoly poly, Point3D *vertexCache, Point3D *vertices) {
	ScreenPoly projectedPoly = {.numVertices = poly.numVertices, .color = poly.color};
	//	projectedPoly.color.r = projectedPoly.color.r * angle / 64;
	//	projectedPoly.color.g = projectedPoly.color.g * angle / 64;
	//	projectedPoly.color.b = projectedPoly.color.b * angle / 64;

	// Iterate through each vertex of the poly
	for (int i = 0; i < poly.numVertices; i++) {
		int vertexIndex = poly.vertexIndices[i];

		// Check if the vertex is already cached
		Point3D transformedVertex;
		if (vertexCache[vertexIndex].x != 0x8000) {
			transformedVertex = vertexCache[vertexIndex];
		} else {
			Point3D curVertex = vertices[vertexIndex];
			transformedVertex = translatePoint(rotatePoint(rotMatrix, curVertex), cameraspacePos);
			vertexCache[vertexIndex] = transformedVertex;
		}

		// Project vertex onto the screen
//		printf("Cameraspace: X: %d, Y: %d, Z: %d\n", cameraspacePos.x, cameraspacePos.y, cameraspacePos.z);
		Point2D projectedVertex = projectionFunction(transformedVertex);

//		if (abs(transformedVertex.x * reciprocalZ / 256) > 65536 | abs(transformedVertex.y * reciprocalZ / 256) > 65536) {
//			printf("Actual: X: %d, Y: %d, Z: %d\n", transformedVertex.x, transformedVertex.y, transformedVertex.z);
//			printf("Screen: X: %d, Y: %d, Z: %d\n", projectedVertex.x, projectedVertex.y, transformedVertex.z);
//			fflush(stdout);
//		}

		projectedPoly.vertices[i] = projectedVertex;
	}

	// Transform the centroid to compute the depth of the polygon
	Point3D transformedCentroid = translatePoint(rotatePoint(rotMatrix, poly.centroid), cameraspacePos);
	projectedPoly.depth = pow(transformedCentroid.z, 2) + pow(transformedCentroid.x, 2) + pow(transformedCentroid.y, 2);

	return projectedPoly;
}

//
/*
	.PROC	draw_object
		STX current_object
	compute_rel_x:
		LDA object_x_sub, X
		SEC
		SBC camera_x_sub
		STA object_rel_x_sub

		LDA object_x_lo, X
		SBC camera_x_lo
		STA object_rel_x_lo

		LDA object_x_hi, X			; Don't bother drawing objects that are out of range
		SBC camera_x_hi
		BNE exit

	compute_rel_y:
		LDA object_y_sub, X
		SEC
		SBC camera_y_sub
		STA object_rel_y_sub

		LDA object_y_lo, X
		SBC camera_y_lo
		STA object_rel_y_lo

		LDA object_y_hi, X
		SBC camera_y_hi
		BNE exit

	compute_rel_z:
		LDA object_z_sub, X
		SEC
		SBC camera_z_sub
		STA object_rel_z_sub

		LDA object_z_lo, X
		SBC camera_z_lo
		STA object_rel_z_lo

		LDA object_z_hi, X
		SBC camera_z_hi
		BNE exit

	compute_cameraspace_pos:
		LDA o

	compute_object_rotation_matrix:
		LDX current_object

		LDA object_yaw_hi, X
		CLC
		ADC camera_yaw_hi
		STA setup_rot_matrix::yaw
	
		LDA object_pitch_hi, X
		CLC
		ADC camera_pitch_hi
		STA setup_rot_matrix::pitch

		LDA object_roll_hi, X
		CLC
		ADC camera_roll_hi
		STA setup_rot_matrix::roll

		JSR setup_rot_matrix

	setup_vertex_cache:
		LDA #$80
		LDX #POLY_MAX_VERTICES
	:	STA vertex_cache_x_hi, X
		DEX
		BPL :-


	.ENDPROC
*/
void drawObject(Object object, double cameraMatrix[3][3]) {
	Point3D objectRelPos = {.x = object.pos.x - cameraPos.x, .y = object.pos.y - cameraPos.y, .z = object.pos.z - cameraPos.z};
//	objectRelPos.x /= 256;
//	objectRelPos.y /= 256;
//	objectRelPos.z /= 256;
	Point3D objectPosCameraspace = rotatePoint(cameraMatrix, objectRelPos);


	// Check if object is within view frustum
	if (frustumCull(objectPosCameraspace, 10))
		return;

	// Compute line-of-sight vector from camera to object origin
	Point3D sightVector = normalize(objectPosCameraspace);
	//	printf("sight: (%d, %d, %d)\n", sightVector.x, sightVector.y, sightVector.z);
	//	fflush(stdout);

	// Compute object rotation matrix
	double objectMatrix[3][3];
	setupRotMatrix(objectMatrix, object.rot);
	double combinedMatrix[3][3];
	matrixMultiply(cameraMatrix, objectMatrix, combinedMatrix);

	// Setup vertex cache
	Point3D vertexCache[MESH_MAX_VERTICES];
	for (int i = 0; i < MESH_MAX_VERTICES; i++)
		vertexCache[i] = (Point3D) {.x = 0x8000};

	// Iterate through the object's mesh and draw each polygon
	for (int i = 0; i < object.mesh.numPolys; i++) {
		// Check if polygon is visible
		int angle = -backFaceCull(combinedMatrix, object.mesh.polys[i].normal, sightVector); 
		if (angle < 0)
			continue;

		// Add transformed and projected polygon to display list
		displayListIndices[displayListSize] = displayListSize;
		displayListPolys[displayListSize] = drawPoly(combinedMatrix, objectPosCameraspace, object.mesh.polys[i], vertexCache, object.mesh.vertices);
		displayListSize++;
	}
}

// Sort polygons from nearest to farthest
/* Asm version
	;	Requires display_list_indices to be in zp
	;	Clobbers A, X, Y
	.PROC	sort_display_list
	outer_loop:
		LDX #$01
	inner_loop:
		LDY display_list_indices - 0, X
		LDA display_list_polys + 0, Y
		LDY display_list_indices - 1, X
		CMP display_list_polys + 0, Y
		BCC check_inner
		BNE swap_indices
		LDA display_list_polys + 1, Y
		LDY display_list_indices - 0, X
		CMP display_list_polys + 1, Y
		BCS check_inner

	swap_indices:
		LDA display_list_indices - 0, X
		STA display_list_indices - 1, X
		STY display_list_indices - 0, X

	check_inner:
		INX
		CPX display_list_size
		BNE inner_loop

	check_outer:
		LDA #$01
		DCP display_list_size
		BNE outer_loop:

	exit:
		RTS
	.ENDPROC
*/
void sortPolys() {
	for (int i = 0; i < displayListSize - 1; i++) {
		for (int j = 0; j < displayListSize - 1; j++) {
			ScreenPoly current = displayListPolys[displayListIndices[j]];
			ScreenPoly next = displayListPolys[displayListIndices[j + 1]];
			if (current.depth < next.depth) {
				int temp = displayListIndices[j];
				displayListIndices[j] = displayListIndices[j + 1];
				displayListIndices[j + 1] = temp;
			}
		}
	}
}

//
void display(int value) {
	// Compute camera rotation matrix
	double cameraMatrix[3][3];
	setupRotMatrix(cameraMatrix, cameraRot);

	glClear(GL_COLOR_BUFFER_BIT);


	displayListSize = 0;
	drawObject(cube, cameraMatrix);
	newCube.rot.roll += 20 / frameRate;
	cube.rot.yaw += 20 / frameRate;

	drawObject(newCube, cameraMatrix);
	sortPolys();
	for (int i = 0; i < displayListSize; i++) {
		ScreenPoly curPoly = displayListPolys[displayListIndices[i]];
		rasterizePoly(curPoly);
//		printf("index: %d, color: #%02X%02X%02X, vertices: %d, depth: %d\n", i, curPoly.color.r, curPoly.color.g, curPoly.color.b, curPoly.numVertices, curPoly.depth);
//		fflush(stdout);
	}

	glFlush();

	glutTimerFunc(1000 / frameRate, display, 0);
	return;
}

// Initialize OpenGL stuff and register callbacks
int main(int argc, char **argv) {
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE);

	glutInitWindowSize(SCREEN_WIDTH, SCREEN_HEIGHT);
	glutCreateWindow("Test Window :)");
	glutDisplayFunc(display);
	glutKeyboardFunc(handleInput);
	glClearColor(0.0, 0.0, 0.0, 0.0);
	gluOrtho2D(-SCREEN_WIDTH / 2, SCREEN_WIDTH / 2, -SCREEN_HEIGHT / 2, SCREEN_HEIGHT / 2);

	glutMainLoop();
}


/* ASM Compute 16-bit square
; Lo byte in x, hi byte in y
; 90-93 cycles
; Could be 84-87 if hi bytes are returned in A and X

result:		.RES 4

.PROC	square_16bit
	STX square_lo_ptr + 0
	STX square_hi_ptr + 0

	LDA square_lo, X
	STA result + 0

	LDA square_hi, X
	CLC
	ADC (square_lo_ptr), Y
	PHP
	SEC
	SBC square_lo, Y
	PHP
	SEC
	SBC square_lo, X
	STA result + 1

	LDA square_lo, Y
	SBC square_hi, X
	LDX square_hi, Y
	BCS :+
	DEX
:	PLP 
	SBC square_hi, Y
	BCS :+
	DEX
:	PLP
	ADC (square_hi_ptr), Y
	STA result + 2
	BCC :+
	INX
:	STX result + 3

	RTS

.ENDPROC


	LDX x_dist + 0
	LDY x_dist + 1
	JSR square_16bit
	LDA result + 0
	STA dist + 0
	LDA result + 1
	STA dist + 1
	LDA result + 2
	STA dist + 2
	LDA result + 3
	STA dist + 3

	LDY barycenter_x
	MUL_XX
	STX x_dist + 0
	STA x_dist + 1

	LDY barycenter_y
	MUL_XY
	TAY
	TXA
	CLC
	ADC x_dist + 0
	STA x_dist + 0
	TYA
	ADC x_dist + 1
	STA x_dist + 1


*/




/*	QUATERNION
	q * p1 * q'

	(a + bi + cj + dk) * (0 + xi + yj + zk) * (a - bi - cj - dk)

	(axi + bxii + cxji + dxki + ayj + byij + cyjj + dykj + azk + bzik + czjk + dzkk) * (a - bi - cj - dk)

	(axi - bx - cxk + dxj + ayj + byk - cy - dyi + azk - bzj + czi - dz) * (a - bi - cj - dk)

	(aaxi - abx - acxk + adxj + aayj + abyk - acy - adyi + aazk - abzj + aczi - adz)
	 - (abxii - bbxi - bcxki + bdxji + abyji + bbyki - bcyi - bdyii + abzki - bbzji + bczii - bdzi)
	 - (acxij - bcxj - ccxkj + cdxjj + acyjj + bcykj - ccyj - cdyij + aczkj - bczjj + cczij - cdzj)
	 - (adxik - bdxk - cdxkk + ddxjk + adyjk + bdykk - cdyk - ddyik + adzkk - bdzjk + cdzik - ddzk)

	(aaxi - abx - acxk + adxj + aayj + abyk - acy - adyi + aazk - abzj + aczi - adz)
	 - (-abx - bbxi - bcxj - bdxk - abyk + bbyj - bcyi + bdy + abzj + bbzk - bcz - bdzi)
	 - (+acxk - bcxj + ccxi - cdx - acy - bcyi - ccyj - cdyk - aczi + bcz + cczk - cdzj)
	 - (-adxj - bdxk + cdx + ddxi + adyi - bdy - cdyk + ddyj - adz - bdzi - cdzj - ddzk)

	(aaxi - abx - acxk + adxj + aayj + abyk - acy - adyi + aazk - abzj + aczi - adz)
	 + (+abx + bbxi + bcxj + bdxk + abyk - bbyj + bcyi - bdy - abzj - bbzk + bcz + bdzi)
	 + (-acxk + bcxj - ccxi + cdx + acy + bcyi + ccyj + cdyk + aczi - bcz - cczk + cdzj)
	 + (+adxj + bdxk - cdx - ddxi - adyi + bdy + cdyk - ddyj + adz + bdzi + cdzj + ddzk)

	a  = 0
	bi = aaxi + bbxi - ccxi - ddxi - 2adyi + 2aczi + 2bcyi + 2bdzi
	cj = aayj - bbyj + ccyj - ddyj + 2adxj - 2abzj + 2bcxj + 2cdzj
	dk = aazk - bbzk - cczk + ddzk + 2abyk - 2acxk + 2bdxk + 2cdyk
*/

/* Fun ASM tidbit
bcd_add:
	CLC
@loop:
	LDA (addend), Y
	ADC (augend), Y
	CMP #10
	BCC :+
	SBC #10
:	STA (addend), Y
	INY
	CPY max
	BNE @loop
	RTS

bcd_sub:
	SEC
@loop:
	LDA (addend), Y
	SBC (augend), Y
	BCS :+
	ADC #10
:	STA (addend), Y
	INY
	CPY max
	BNE @loop
	RTS
*/