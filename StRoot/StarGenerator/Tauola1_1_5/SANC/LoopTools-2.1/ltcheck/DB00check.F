* DB00check.F
* the derivative of B00 with respect to p^2
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "DB00.F"


	double complex function DB00check(p, m1, m2)
	implicit none
	double precision p, m1, m2

	double complex B1check, DB0check, DB1check
	external B1check, DB0check, DB1check

	DB00check = 1/6D0*(2*m1*DB0check(p, m1, m2) +
     &    B1check(p, m1, m2) +
     &    (p + m1 - m2)*DB1check(p, m1, m2)) - 1/18D0
	end

