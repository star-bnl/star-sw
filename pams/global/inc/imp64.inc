*
* $Id: imp64.inc,v 1.1 2000/05/10 20:56:47 lbarnby Exp $
*
* $Log: imp64.inc,v $
* Revision 1.1  2000/05/10 20:56:47  lbarnby
* Additional include files for primary verex confidence level calculation module
*
* Revision 1.1.1.1  1996/04/01 15:02:59  mclareni
* Mathlib gen
*
*
* imp64.inc
*
#if !defined(CERNLIB_DOUBLE)
      IMPLICIT REAL (A-H,O-Z)

#elif (defined(CERNLIB_F90))&&(defined(CERNLIB_DOUBLE))
      IMPLICIT REAL(2) (A-H,O-Z)

#elif 1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

#endif
