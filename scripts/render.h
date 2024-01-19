#ifndef RENDER_H
#define RENDER_H

#define SCREEN_WIDTH 256
#define SCREEN_HEIGHT 240
#define MAX_OBJECTS 1
#define CAMERA_SPEED 8
#define CAMERA_ROT_SPEED 1
#define RENDER_MAX_POLYS 32
#define MESH_MAX_VERTICES 128
#define POLY_MAX_VERTICES 10
#define NEAR_PLANE 1
#define FAR_PLANE 65536
#define PI 3.14159

typedef struct Point3D {
	int x;
	int y;
	int z;
} Point3D;

typedef struct Point2D {
	int x;
	int y;
} Point2D;

typedef struct Rot3D {
	unsigned char yaw;
	unsigned char pitch;
	unsigned char roll;
} Rot3D;

typedef struct Color {
	unsigned char r;
	unsigned char g;
	unsigned char b;
} Color;

typedef struct ScreenPoly {
	Color color;
	int numVertices;
	Point2D vertices[POLY_MAX_VERTICES];
	unsigned int depth;
} ScreenPoly;

typedef struct MeshPoly {
	Color color;
	int numVertices;
	int* vertexIndices;
	Point3D normal;
	Point3D centroid;
} MeshPoly;

typedef struct Mesh {
	Point3D* vertices;
	int numPolys;
	MeshPoly* polys;
} Mesh;

typedef struct Object {
	Point3D pos;
	Rot3D rot;
	Mesh mesh;
} Object;

#endif