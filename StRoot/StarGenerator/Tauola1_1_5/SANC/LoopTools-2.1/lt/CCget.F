* CCget.F
* the three-point tensor coefficients for complex parameters
* this file is part of LoopTools
* last modified 28 Jun 04 th

#include "lt.h"


	integer function CCget(p1, p2, p1p2, m1, m2, m3)
	implicit none
	double complex p1, p2, p1p2, m1, m2, m3

	integer cachelookup
	external cachelookup, CCijk

	double complex para(6)

	double precision CCcache(2)
	common /CCbase/ CCcache

* The following data statement initializes two *pointers* to NULL
* (see cache.c).  This is sneaky but ok because IEEE says 0D0 is
* represented as 8 0-bytes.
	data CCcache /0, 0/

	para(1) = p1
	para(2) = p2
	para(3) = p1p2
	para(4) = m1
	para(5) = m2
	para(6) = m3

	CCget = cachelookup(para, CCcache, CCijk, 12, 13)
	end

************************************************************************

	subroutine CCijk(P, C)
	implicit none
	double complex P(6)
	double complex C(13)

	double complex a0i(2), b0p, b1p
	common /bsave/ a0i, b0p, b1p

	double complex CB11, CC0
	external CB11, CC0

	double complex M11, M12, M22
	common /Cmatrix2/ M11, M12, M22

	double complex det2, f1, f2
	double complex b1123, b023, b123
	double complex b1113, b013, b113
	double complex b1112, b012, b112
	double complex c1

	M12 = .5D0*(P(2) - P(3) - P(1))
	det2 = 2*(P(1)*P(3) - M12*M12)
	M12 = M12/det2
	M11 = P(3)/det2
	M22 = P(1)/det2
	f1 = P(4) - P(5) + P(1)
	f2 = P(4) - P(6) + P(3)

	b1123 = CB11(P(2), P(6), P(5))
	b023 = b0p
	b123 = b1p
	b1113 = CB11(P(3), P(4), P(6))
	b013 = b0p
	b113 = b1p
	b1112 = CB11(P(1), P(4), P(5))
	b012 = b0p
	b112 = b1p

	C(cc0) = CC0(P(1), P(2), P(3), P(4), P(5), P(6))
	call Crotate2(C(cc1), C(cc2),
     &    b013 - b023 - f1*C(cc0),
     &    b012 - b023 - f2*C(cc0))

	C(cc00) = .5D0*(P(4)*C(cc0) +
     &    .5D0*(f1*C(cc1) + f2*C(cc2) + b023)) + .25D0
	call Crotate2(C(cc11), C(cc12),
     &    -f1*C(cc1) - b123 - 2*C(cc00),
     &    -f2*C(cc1) - b123 + b112)
	b023 = b023 + b123
	call Crotate2(c1, C(cc22),
     &    b023 + b113 - f1*C(cc2),
     &    b023 - f2*C(cc2) - 2*C(cc00))
	C(cc12) = .5D0*(C(cc12) + c1)

	C(cc001) = (P(4)*C(cc1) + .5D0*(f1*C(cc11) +
     &    f2*C(cc12) + b123))/3D0 - 1/18D0
	C(cc002) = (P(4)*C(cc2) + .5D0*(f1*C(cc12) +
     &    f2*C(cc22) - b023))/3D0 - 1/18D0
	call Crotate2(C(cc111), C(cc112),
     &    -b1123 - f1*C(cc11) - 4*C(cc001),
     &    -b1123 - f2*C(cc11) + b1112)
	b1123 = b1123 + b023 + b123
	call Crotate2(C(cc122), C(cc222),
     &    -b1123 - f1*C(cc22) + b1113,
     &    -b1123 - f2*C(cc22) - 4*C(cc002))
	end

************************************************************************

	subroutine Crotate2(out1, out2, in1, in2)
	implicit none
	double complex out1, out2, in1, in2

	double complex M11, M12, M22
	common /Cmatrix2/ M11, M12, M22

	out1 = M11*in1 + M12*in2
	out2 = M12*in1 + M22*in2
	end

