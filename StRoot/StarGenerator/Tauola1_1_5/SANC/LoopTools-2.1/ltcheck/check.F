* check.F
* set the maximum relative deviation allowed between ff and ad results
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "ltcheck.h"


	block data
	double precision maxdev
	common /ffcheck/ maxdev
	data maxdev /1D-12/
	end

************************************************************************

	subroutine setmaxdev(newmaxdev)
	implicit none
	double precision newmaxdev

	double precision maxdev
	common /ffcheck/ maxdev

	maxdev = newmaxdev
	end

************************************************************************

	subroutine checkA(result, func, name, m)
	implicit none
	double complex result, func
	external func
	character name*(*)
	double precision m

	double precision maxdev
	common /ffcheck/ maxdev

	double complex check

	check = func(m)
	if( abs(result - check)/abs(result) .gt. maxdev ) then
	  print *, "deviation in ", name
	  print *, "  m = ", m
	  print *, "FF's result: ", result
	  print *, "check:       ", check
	endif
	end

************************************************************************

	subroutine checkB(result, func, name, p, m1, m2)
	implicit none
	double complex result, func
	external func
	character name*(*)
	double precision p, m1, m2

	double precision maxdev
	common /ffcheck/ maxdev

	double complex check

	check = func(p, m1, m2)
	if( abs(result - check)/abs(result) .gt. maxdev ) then
	  print *, "deviation in ", name
	  print *, "  p  = ", p
	  print *, "  m1 = ", m1
	  print *, "  m2 = ", m2
	  print *, "FF's result: ", result
	  print *, "check:       ", check
	endif
	end

************************************************************************

	subroutine checkC(result, func, name,
     &    p1, p2, p1p2, m1, m2, m3)
	implicit none
	double complex result, func
	external func
	character name*(*)
	double precision p1, p2, p1p2, m1, m2, m3

	double precision maxdev
	common /ffcheck/ maxdev

	double complex check

	check = func(p1, p2, p1p2, m1, m2, m3)
	if( abs(result - check)/abs(result) .gt. maxdev ) then
	  print *, "deviation in ", name
	  print *, "  p1   = ", p1
	  print *, "  p2   = ", p2
	  print *, "  p1p2 = ", p1p2
	  print *, "  m1   = ", m1
	  print *, "  m2   = ", m2
	  print *, "  m3   = ", m3
	  print *, "FF's result: ", result
	  print *, "check:       ", check
	endif
	end

************************************************************************

	subroutine checkD(result, func, name,
     &    p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	double complex result, func
	external func
	character name*(*)
	double precision p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4

	double precision maxdev
	common /ffcheck/ maxdev

	double complex check

	check = func(p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4)
	if( abs(result - check)/abs(result) .gt. maxdev ) then
	  print *, "deviation in ", name
	  print *, "  p1   = ", p1
	  print *, "  p2   = ", p2
	  print *, "  p3   = ", p3
	  print *, "  p4   = ", p4
	  print *, "  p1p2 = ", p1p2
	  print *, "  p2p3 = ", p2p3
	  print *, "  m1   = ", m1
	  print *, "  m2   = ", m2
	  print *, "  m3   = ", m3
	  print *, "  m4   = ", m4
	  print *, "FF's result: ", result
	  print *, "check:       ", check
	endif
	end

