*
* $Id: grndm.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
*
* $Log: grndm.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.1  2001/02/27 10:15:40  nevski
*  first working release
*
* Revision 1.3  2000/01/06 22:09:17  nevski
* unlimited track/vertex version
*
* Revision 1.2  1998/07/04 20:25:51  nevski
* CERNLIB_LINUX flag introduced
*
* Revision 1.2  1998/03/02 15:33:35  japost
*    A significant error on Linux is corrected:
*
*     Two lines of grndm were not defined for any system that was not listed.
*   In such a system (and Linux was one) the behavior of grndm was incorrect.
*
*     A default behavior is now defined for all systems. Any system that has
*   a problem with this behavior (using integer division) must be added to
*   the #ifdef together with IBMALL etc.
*
* Revision 1.1.1.1  1995/10/24 10:21:42  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.23  by  S.Giani
*-- Author :
      SUBROUTINE GRNDMO(RVEC,LEN)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       To generate a vector RVECV of LEN random numbers         *
C.    *         Copy of the CERN Library routine RANECU                *
C.    *                                                                *
C.    *    ==>Called by : <USER>, many GEANT routines                  *
C.    *       Author    F.Carminati *********                          *
C.    * Modifications:                                                 *
C.    * PN,1999: randomise small numbers (<1.e-4)                      *
C.    ******************************************************************
C.
      DIMENSION RVEC(*)
#if defined(CERNLIB_CRAY)
      REAL ISEED, ISEED1, ISEED2, K, C1, C2, IZ
#endif
#if !defined(CERNLIB_CRAY)
*     DOUBLE PRECISION ISEED, ISEED1, ISEED2, K, C1, C2, IZ
#endif
      PARAMETER (TWOM12=1./4096., TWOM24=TWOM12*TWOM12)
      PARAMETER (C1=1./53668., C2=1./52774.)
      PARAMETER (MAXSEQ=215)
      COMMON / GCRNDM / JSEQ(2), ISEED(2,MAXSEQ)
C
      ISEQ   = JSEQ(1)
      ISEED1 = ISEED(1,ISEQ)
      ISEED2 = ISEED(2,ISEQ)
      DO 100 I= 1, LEN

        K = INT(ISEED1*C1)
        ISEED1 = 40014*(ISEED1 - K*53668) - K*12211
        IF (ISEED1 .LT. 0) ISEED1=ISEED1+2147483563
C
        K = INT(ISEED2*C2)
        ISEED2 = 40692*(ISEED2 - K*52774) - K* 3791
        IF (ISEED2 .LT. 0) ISEED2=ISEED2+2147483399
C
        IZ = ISEED1 - ISEED2
        IF (IZ .LE. 0)  IZ = IZ + 2147483562
C
        Z  = IZ
        IF (Z .LE. 4096) Z = Z + ISEED2*4.6566128E-10

        IF (Z .LE. 0) then
            print *,' ********** GRNDM  ERROR ! ********** '
            print *,' C1,C2,ISEQ,ISEED1,ISEED2 = ',
     >                C1,C2,ISEQ,ISEED1,ISEED2
            print *,' ********** GRNDM  ERROR ! ********** '
            Z = Z + 0.9999991
        endif

        RVEC(I) = Z * 4.6566128E-10

  100 CONTINUE
      ISEED(1,ISEQ) = ISEED1
      ISEED(2,ISEQ) = ISEED2
      END
