* $Id: agrndm.F,v 1.1.1.1 2004/01/12 23:49:38 potekhin Exp $
* $Log: agrndm.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
*
* Revision 1.1  2001/02/27 10:14:59  nevski
*  first working release
*
******************************************************************************
      SUBROUTINE aGRNDM (RVEC,LEN)
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


