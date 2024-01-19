// Cube
#define cubeScale 10

static Point3D cubeVertices[8] = {(Point3D) {.x = 1 * cubeScale, .y = 1 * cubeScale, .z = 1 * cubeScale}, 
                                  (Point3D) {.x = 1 * cubeScale, .y = -1 * cubeScale, .z = 1 * cubeScale}, 
                                  (Point3D) {.x = -1 * cubeScale, .y = -1 * cubeScale, .z = 1 * cubeScale}, 
                                  (Point3D) {.x = -1 * cubeScale, .y = 1 * cubeScale, .z = 1 * cubeScale},
                                  (Point3D) {.x = 1 * cubeScale, .y = 1 * cubeScale, .z = -1 * cubeScale}, 
                                  (Point3D) {.x = 1 * cubeScale, .y = -1 * cubeScale, .z = -1 * cubeScale}, 
                                  (Point3D) {.x = -1 * cubeScale, .y = -1 * cubeScale, .z = -1 * cubeScale}, 
                                  (Point3D) {.x = -1 * cubeScale, .y = 1 * cubeScale, .z = -1 * cubeScale}};

static int cubeFace0[4] = {0, 1, 2, 3};
static int cubeFace1[4] = {4, 5, 6, 7};
static int cubeFace2[4] = {0, 1, 5, 4};
static int cubeFace3[4] = {2, 3, 7, 6};
static int cubeFace4[4] = {1, 2, 6, 5};
static int cubeFace5[4] = {0, 3, 7, 4};

static MeshPoly cubePolys[6] = {{.color = (Color) {.r = 0xFF, .g = 0xFF, .b = 0x00}, .numVertices = 4, .vertexIndices = cubeFace0, .normal = (Point3D) {.x = 0, .y = 0, .z = 1}, .centroid = (Point3D) {.x = 0, .y = 0, .z = 1 * cubeScale}},
                                {.color = (Color) {.r = 0xFF, .g = 0x00, .b = 0xFF}, .numVertices = 4, .vertexIndices = cubeFace1, .normal = (Point3D) {.x = 0, .y = 0, .z = -1}, .centroid = (Point3D) {.x = 0, .y = 0, .z = -1 * cubeScale}},
                                {.color = (Color) {.r = 0x00, .g = 0xFF, .b = 0xFF}, .numVertices = 4, .vertexIndices = cubeFace2, .normal = (Point3D) {.x = 1, .y = 0, .z = 0}, .centroid = (Point3D) {.x = 1 * cubeScale, .y = 0, .z = 0}},
                                {.color = (Color) {.r = 0xFF, .g = 0x00, .b = 0x00}, .numVertices = 4, .vertexIndices = cubeFace3, .normal = (Point3D) {.x = -1, .y = 0, .z = 0}, .centroid = (Point3D) {.x = -1 * cubeScale, .y = 0, .z = 0}},
                                {.color = (Color) {.r = 0x00, .g = 0x00, .b = 0xFF}, .numVertices = 4, .vertexIndices = cubeFace4, .normal = (Point3D) {.x = 0, .y = -1, .z = 0}, .centroid = (Point3D) {.x = 0, .y = -1 * cubeScale, .z = 0}},
                                {.color = (Color) {.r = 0x00, .g = 0xFF, .b = 0x00}, .numVertices = 4, .vertexIndices = cubeFace5, .normal = (Point3D) {.x = 0, .y = 1, .z = 0}, .centroid = (Point3D) {.x = 0, .y = 1 * cubeScale, .z = 0}}};

static Object cube = {.pos = (Point3D) {.x = 0, .y = 0, .z = 256}, .rot = (Rot3D) {.yaw = 0, .pitch = 0, .roll = 0}, .mesh = (Mesh) {.vertices = cubeVertices, .numPolys = 6, .polys = cubePolys}};

static Object newCube = {.pos = (Point3D) {.x = 0, .y = 256, .z = 256}, .rot = (Rot3D) {.yaw = 0, .pitch = 50, .roll = 0}, .mesh = (Mesh) {.vertices = cubeVertices, .numPolys = 6, .polys = cubePolys}};