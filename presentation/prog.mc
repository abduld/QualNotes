typedef struct {
  double x, y;
} Point_t;
bool less(Point_t a, Point_t b) {
  return (a.x < b.x) || (a.x == b.x && a.y < b.y);
}
void swap(Point_t * a, Point_t * b) {
  Point_t tmp = *a;
  *a = *b;
  *b = tmp;
}
void bubbleSort(Point_t *pts, int len) {
  bool swapped = true;
  while (swapped) {
    int ii = 1;
    swapped = false;
    while (ii < len) {
      Point_t * prev = &pts[ii - 1];
      Point_t * curr = &pts[ii];
      if (less(*prev, *curr)) {
        swap(prev, curr);
        swapped = true;
      }
      ii++;
    }
  }
}
int main(void) {
  lenA = 4;
  lenB = 4;
  A = malloc(lenA);
  A = malloc(lenA);
  A = {{0,0}, {0,1}, {1,1}, {1,0}};
  B = {{1,0}, {1,1}, {0,1}, {0,0}};
  bubbleSort(A, lenA);
  bubbleSort(B, lenB);
  return 0;
}
