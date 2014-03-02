typedef struct {
	float x, y;
} Point_t;
bool less(Point_t a, Point_t b) {
	return a.x < b.x && a.y < b.y;
}
...
int sizeA = 32;
Point_t * A = malloc(sizeA);
A[0] = {0, 0};
A[1] = {0, 1};
A[2] = {1, 1};
A[3] = {1, 0};
bool g = less(A[0], A[1]);
if (g) {
	less(A[1], A[2]);
} else {
	less(A[2], A[3]);
}