less = fun(lessa, lessb) -> (res)
	tmp0 = fless(lessa, lessb)
	tmp1 = fless(lessa, lessb)
	res = and(tmp1, tmp2)
sizeA = 32
A = allocate(sizeA)
A0 = iadd(A, 0)
*A0 = 00
A1 = iadd(A, 8)
*A1 = 01
A1 = iadd(A, 16)
*A2 = 11
A1 = iadd(A, 24)
*A3 = 10
g = less(A0, A1)
if g then
	less(A1, A2)
else
	less(A2, A3)
end