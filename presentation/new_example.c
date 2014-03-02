typedef struct {
	float x, y;
} Point_t;
bool lessX(Point_t * a, Point_t * b) {
	return a->x < b->x;
}
...
Point_t A[1] = {{0, 0}};
Point_t B = {0, 1};
Point_t * pC;
bool g = lessX(&A[0], &B);
if (g) {
	pC = &A[0];
} else {
	pC = &B;
}
bool k = lessX(&B, &A[0]);
