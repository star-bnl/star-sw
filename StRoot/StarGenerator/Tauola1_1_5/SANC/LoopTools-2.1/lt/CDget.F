* CDget.F
* the four-point tensor coefficients for complex parameters
* this file is part of LoopTools
* last modified 28 Jun 04 th

#include "lt.h"


	integer function CDget(p1, p2, p3, p4, p1p2, p2p3,
     &    m1, m2, m3, m4)
	implicit none
	double complex p1, p2, p3, p4, p1p2, p2p3
	double complex m1, m2, m3, m4

	integer cachelookup
	external cachelookup, CDijkl

	double complex para(10)

	double precision CDcache(2)
	common /CDbase/ CDcache

* The following data statement initializes two *pointers* to NULL
* (see cache.c).  This is sneaky but ok because IEEE says 0D0 is
* represented as 8 0-bytes.
	data CDcache /0, 0/

	para(1) = p1
	para(2) = p2
	para(3) = p3
	para(4) = p4
	para(5) = p1p2
	para(6) = p2p3
	para(7) = m1
	para(8) = m2
	para(9) = m3
	para(10) = m4

	CDget = cachelookup(para, CDcache, CDijkl, 20, 46)
	end

************************************************************************

	subroutine CDijkl(P, D)
	implicit none
	double complex P(10)
	double complex D(46)

	integer CCget
	double complex CD0
	external CCget, CD0

	double complex CCcache(1)
	common /CCbase/ CCcache

	double complex M11, M12, M13, M22, M23, M33
	common /Cmatrix3/ M11, M12, M13, M22, M23, M33

	double complex f1, f2, f3, det3
	double complex d1, d2, d3
	double complex c1, c2, c3, c4, c5
	integer C234, C134, C124, C123

	f1 = .5D0*(P(1) + P(5) - P(2))
	f2 = .5D0*(P(1) + P(4) - P(6))
	f3 = .5D0*(P(5) + P(4) - P(3))
	M22 = P(1)*P(4) - f2*f2
	M23 = f1*f2 - P(1)*f3
	det3 = 2*(P(5)*M22 - P(4)*f1*f1 + f3*(f1*f2 + M23))
	M11 = (P(4)*P(5) - f3*f3)/det3
	M12 = (f2*f3 - P(4)*f1)/det3
	M13 = (f1*f3 - P(5)*f2)/det3
	M22 = M22/det3
	M23 = M23/det3
	M33 = (P(1)*P(5) - f1*f1)/det3

	f1 = P(1) + P(7) - P(8)
	f2 = P(5) + P(7) - P(9)
	f3 = P(4) + P(7) - P(10)

	C234 = CCget(P(2), P(3), P(6), P(8), P(9), P(10))
	C134 = CCget(P(5), P(3), P(4), P(7), P(9), P(10))
	C124 = CCget(P(1), P(6), P(4), P(7), P(8), P(10))
	C123 = CCget(P(1), P(2), P(5), P(7), P(8), P(9))

	c1 = CCcache(C234 + cc0) + CCcache(C234 + cc1) +
     &    CCcache(C234 + cc2)
	c2 = CCcache(C234 + cc12) + CCcache(C234 + cc22) +
     &    CCcache(C234 + cc2)
	c3 = 2*(CCcache(C234 + cc1) + c2) + CCcache(C234 + cc11) -
     &    CCcache(C234 + cc22) + CCcache(C234 + cc0)
	c4 = CCcache(C234 + cc1) + CCcache(C234 + cc11) +
     &    CCcache(C234 + cc12)
	c5 = CCcache(C234 + cc111) + CCcache(C234 + cc112) +
     &    CCcache(C234 + cc11)

	D(dd0) = CD0(P(1), P(2), P(3), P(4), P(5), P(6),
     &    P(7), P(8), P(9), P(10))
	call Crotate3(D(dd1), D(dd2), D(dd3),
     &    CCcache(C134 + cc0) - CCcache(C234 + cc0) - f1*D(dd0),
     &    CCcache(C124 + cc0) - CCcache(C234 + cc0) - f2*D(dd0),
     &    CCcache(C123 + cc0) - CCcache(C234 + cc0) - f3*D(dd0))

	D(dd00) = P(7)*D(dd0) + .5D0*(CCcache(C234 + cc0) +
     &    D(dd1)*f1 + D(dd2)*f2 + D(dd3)*f3)
	call Crotate3(D(dd11), d1, d2,
     &    c1 - f1*D(dd1) - 2*D(dd00),
     &    CCcache(C124 + cc1) + c1 - f2*D(dd1),
     &    CCcache(C123 + cc1) + c1 - f3*D(dd1))
	call Crotate3(D(dd12), D(dd22), d3,
     &    CCcache(C134 + cc1) - CCcache(C234 + cc1) - f1*D(dd2),
     &    -(CCcache(C234 + cc1) + f2*D(dd2)) - 2*D(dd00),
     &    CCcache(C123 + cc2) - CCcache(C234 + cc1) - f3*D(dd2))
	call Crotate3(D(dd13), D(dd23), D(dd33),
     &    CCcache(C134 + cc2) - CCcache(C234 + cc2) - f1*D(dd3),
     &    CCcache(C124 + cc2) - CCcache(C234 + cc2) - f2*D(dd3),
     &    -(CCcache(C234 + cc2) + f3*D(dd3)) - 2*D(dd00))
	D(dd12) = .5D0*(D(dd12) + d1)
	D(dd13) = .5D0*(D(dd13) + d2)
	D(dd23) = .5D0*(D(dd23) + d3)

	call Crotate3(D(dd001), D(dd002), D(dd003),
     &    CCcache(C134 + cc00) - CCcache(C234 + cc00) - f1*D(dd00),
     &    CCcache(C124 + cc00) - CCcache(C234 + cc00) - f2*D(dd00),
     &    CCcache(C123 + cc00) - CCcache(C234 + cc00) - f3*D(dd00))
	call Crotate3(D(dd111), D(dd112), D(dd113),
     &    -f1*D(dd11) - c3 - 4*D(dd001),
     &    CCcache(C124 + cc11) - f2*D(dd11) - c3,
     &    CCcache(C123 + cc11) - f3*D(dd11) - c3)
	call Crotate3(D(dd122), D(dd222), D(dd223),
     &    CCcache(C134 + cc11) - CCcache(C234 + cc11) - f1*D(dd22),
     &    -(CCcache(C234 + cc11) + f2*D(dd22)) - 4*D(dd002),
     &    CCcache(C123 + cc22) - CCcache(C234 + cc11) - f3*D(dd22))
	call Crotate3(D(dd133), D(dd233), D(dd333),
     &    CCcache(C134 + cc22) - CCcache(C234 + cc22) - f1*D(dd33),
     &    CCcache(C124 + cc22) - CCcache(C234 + cc22) - f2*D(dd33),
     &    -(CCcache(C234 + cc22) + f3*D(dd33)) - 4*D(dd003))
	call Crotate3(d1, D(dd123), d2,
     &    c2 - f1*D(dd13) - 2*D(dd003),
     &    CCcache(C124 + cc12) + c2 - f2*D(dd13),
     &    c2 - f3*D(dd13) - 2*D(dd001))
	D(dd113) = .5D0*(D(dd113) + d1)
	D(dd133) = .5D0*(D(dd133) + d2)

	D(dd0000) = (P(7)*D(dd00) + .5D0*(CCcache(C234 + cc00) +
     &    f1*D(dd001) + f2*D(dd002) + f3*D(dd003)))/3D0 + 1/36D0
	D(dd0011) = (P(7)*D(dd11) + .5D0*(c3 + f1*D(dd111) +
     &    f2*D(dd112) + f3*D(dd113)))/3D0
	D(dd0012) = (P(7)*D(dd12) - .5D0*(c4 - f1*D(dd112) -
     &    f2*D(dd122) - f3*D(dd123)))/3D0
	D(dd0013) = (P(7)*D(dd13) - .5D0*(c2 - f1*D(dd113) -
     &    f2*D(dd123) - f3*D(dd133)))/3D0
	D(dd0022) = (P(7)*D(dd22) + .5D0*(CCcache(C234 + cc11)+
     &    f1*D(dd122) + f2*D(dd222) + f3*D(dd223)))/3D0
	D(dd0023) = (P(7)*D(dd23) + .5D0*(CCcache(C234 + cc12)+
     &    f1*D(dd123) + f2*D(dd223) + f3*D(dd233)))/3D0
	D(dd0033) = (P(7)*D(dd33) + .5D0*(CCcache(C234 + cc22)+
     &    f1*D(dd133) + f2*D(dd233) + f3*D(dd333)))/3D0

	c3 = c2 + CCcache(C234 + cc112) + CCcache(C234 + cc122)
	c2 = c2 + c3 - CCcache(C234 + cc2) + CCcache(C234 + cc122) +
     &    CCcache(C234 + cc222)
	c4 = c4 + c1 + c2 + c3 + c4 + c5 + CCcache(C234 + cc12)

	call Crotate3(D(dd1111), D(dd1112), D(dd1113),
     &    c4 - f1*D(dd111) - 6*D(dd0011),
     &    CCcache(C124 + cc111) + c4 - f2*D(dd111),
     &    CCcache(C123 + cc111) + c4 - f3*D(dd111))
	call Crotate3(d1, D(dd1123), D(dd1133),
     &    -(c2 + f1*D(dd113)) - 4*D(dd0013),
     &    CCcache(C124 + cc112) - c2 - f2*D(dd113),
     &    -(c2 + f3*D(dd113)) - 2*D(dd0011))
	call Crotate3(D(dd1122), d2, D(dd1223),
     &    c5 - f1*D(dd122) - 2*D(dd0022),
     &    c5 - f2*D(dd122) - 4*D(dd0012),
     &    CCcache(C123 + cc122) + c5 - f3*D(dd122))
	call Crotate3(D(dd1222), D(dd2222), D(dd2223),
     &    CCcache(C134 + cc111) - CCcache(C234 + cc111) - f1*D(dd222),
     &    -(CCcache(C234 + cc111) + f2*D(dd222)) - 6*D(dd0022),
     &    CCcache(C123 + cc222) - CCcache(C234 + cc111) - f3*D(dd222))
	call Crotate3(D(dd1233), D(dd2233), d3,
     &    CCcache(C134 + cc122) - CCcache(C234 + cc122) - f1*D(dd233),
     &    -(CCcache(C234 + cc122) + f2*D(dd233)) - 2*D(dd0033),
     &    -(CCcache(C234 + cc122) + f3*D(dd233)) - 4*D(dd0023))
	call Crotate3(D(dd1333), D(dd2333), D(dd3333),
     &    CCcache(C134 + cc222) - CCcache(C234 + cc222) - f1*D(dd333),
     &    CCcache(C124 + cc222) - CCcache(C234 + cc222) - f2*D(dd333),
     &    -(CCcache(C234 + cc222) + f3*D(dd333)) - 6*D(dd0033))
	D(dd1113) = .5D0*(D(dd1113) + d1)
	D(dd1222) = .5D0*(D(dd1222) + d2)
	D(dd2333) = .5D0*(D(dd2333) + d3)
	end

************************************************************************

	subroutine Crotate3(out1, out2, out3, in1, in2, in3)
	implicit none
	double complex out1, out2, out3, in1, in2, in3

	double complex M11, M12, M13, M22, M23, M33
	common /Cmatrix3/ M11, M12, M13, M22, M23, M33

	out1 = M11*in1 + M12*in2 + M13*in3
	out2 = M12*in1 + M22*in2 + M23*in3
	out3 = M13*in1 + M23*in2 + M33*in3
	end

