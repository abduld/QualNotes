less = fun(a, b) -> (res)
  tmp0 = fless(a, b)
  tmp1 = feq(a, b)
  tmp2 = fless(a, b)
  tmp3 = and(tmp1, tmp2)
  res = or(tmp0, tmp3)
swap = fun(*a, *b) -> (void)
  tmp = *a
  *a = *b
  *b = tmp
  void = 0
bubbleSort = fun(*pts, len) -> (void)
  swapped = true;
  while (swapped) {
    ii = 1;
    swapped = false;
    while (ii < len) {
      ptsii_1 = add(pts, subtract(ii, 1));
      ptsii = add(pts, ii);
      prev = &ptsii_1;
      curr = &pts_ii;
      dprev = *prev;
      dcurr = *curr;
      if (less(dprev, dcurr)) {
        swap(prev, curr);
        swapped = true;
      }
      ii = iadd(ii, 1);
    }
  }
  void = 0
lenA = 4;
lenB = 4;
A = allocate(lenA);
A = allocate(lenA);
A = {{0,0}, {0,1}, {1,1}, {1,0}};
B = {{1,0}, {1,1}, {0,1}, {0,0}};
bubbleSort(A, lenA);
bubbleSort(B, lenB);
return(0);

