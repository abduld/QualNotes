lessX = fun(lessa, lessb) -> (res)
	res = fpless(lessa, lessb)
v00 = 00
v01 = 01
A = allocate(8)
*A = v00
B = v01
A0 = *A
pA0 = &A0
pB = &B
g = lessX(pA0, pB)
if g then
	pC = &A0
else
	pC = &B
end
k = lessX(pB, pA0)
