* $Id: utcfit.F,v 1.2 1998/01/27 00:35:54 fisyak Exp $
* $Log: utcfit.F,v $
* Revision 1.2  1998/01/27 00:35:54  fisyak
* Split sources
*
      INTEGER FUNCTION UTCFIT(XCIR,YCIR,WCIR,NCIR,
     >                           SGN,R0,ACENT,BCENT,CHISQ)
C>-----------------------------------------------------------------
C
C UTCFIT - circle fit
C
C DESCRIPTION:
C Subroutine that fits circle parameters using algorithm
C described by Chernov and Oskov in Computer Physics
C Communications.
C 
C INPUT ARGUMENTS:
C OUTPUT ARGUMENTS:
C AUTHOR:
C<----------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
        IMPLICIT DOUBLE PRECISION (A-H , O-Z)
        INTEGER ITRY, NTRY, NCIR, IERR
C
        REAL    SGN,R0,ACENT,BCENT,CHISQ
        REAL    XCIR(*),YCIR(*),WCIR(*)
*
        DOUBLE PRECISION  LAMDA,KAPPA,KAPPA1
C
        COMMON /CIRCOM/ XAV   ,YAV   ,WSUM  ,RSCALE,COSROT,SINROT,
     +                XXAV  ,XYAV  ,YYAV  ,XRRAV ,YRRAV ,RRRRAV,
     +                ALPHA ,BETA  ,KAPPA
*
        DATA DBL1 / 1.0D0/,DBL2 / 2.0D0/, DBL4/ 4.0D0/, DBL64/6.4D1/
C
        DATA NTRY  / 5/
C
C-->  FIRST CALCULATE THE H MATRIX AND OTHER THINGS
C
        UTCFIT = 0
        IERR = 0
*
        CALL UTCMAT(XCIR,YCIR,WCIR,NCIR)
C
C-->  USE THESE TO GET THE COEFFICIENTS OF THE 4-TH ORDER POLYNIMIAL
C-->  DON'T PANIC - THE THIRD ORDER TERM IS ZERO !
C
        XRRXRR= XRRAV  * XRRAV
        YRRYRR= YRRAV  * YRRAV
        RRRRM1= RRRRAV - DBL1
        XXYY  = XXAV   * YYAV
C
        C0  =          RRRRM1*XXYY - XRRXRR*YYAV - YRRYRR*XXAV
        C1  =        - RRRRM1      + XRRXRR      + YRRYRR   - DBL4*XXYY
        C2  =   DBL4 + RRRRM1                               - DBL4*XXYY
        C4  = - DBL4
C
C-->  COEFFICIENTS OF THE DERIVATIVE - USED IN NEWTON-RAPHSON ITERATIONS
C
        C2D =   DBL2 * C2
        C4D =   DBL4 * C4
C
C-->  0'TH VALUE OF LAMDA - LINEAR INTERPOLATION BETWEEN P(0) & P(YYAV)
C
CCCC  LAMDA = YYAV * C0 / (C0 + YRRSQ * (XXAV-YYAV))
        LAMDA = 0.0D0
        DLAMDA= 0.0D0
C
        CHISCL= WSUM * RSCALE**2
        DLAMAX= 0.001 / CHISCL
C
        DO 30 ITRY = 1,NTRY
C
            P = C0 + LAMDA * (C1 + LAMDA * (C2 + LAMDA * LAMDA * C4 ))
            PD = (C1 + LAMDA * (C2D + LAMDA * LAMDA * C4D))
C
            DLAMDA= - P / PD
            LAMDA = LAMDA + DLAMDA
C
C
            IF (DABS(DLAMDA).LT.DLAMAX) GO TO 35
C
   30   CONTINUE
C
        ITRY = NTRY
C
   35   CONTINUE
C
        CHISQ = CHISCL * LAMDA
C
        DCHISQ= CHISCL * DLAMDA
C
C-->  NOW CALCULATE THE MATRIX ELEMENTS FOR ALPHA, BETA & KAPPA
C
        H11   = XXAV  -     LAMDA
        H14   = XRRAV
        H22   = YYAV  -     LAMDA
        H24   = YRRAV
        H34   = DBL1   + DBL2*LAMDA
        IF(H11.EQ.0.0D0.OR.H22.EQ.0.0D0) THEN
           UTCFIT = 1
C          write(6,*) 'Problems in the UTCFIT'
           RETURN
        ENDIF
C
C
        ROOTSQ= (H14/H11)**2 + DBL4*H34
C
        IF (DABS(H22).GE.DABS(H24)) THEN
            RATIO = H24/H22
            ROOTSQ=        RATIO**2  + ROOTSQ
            KAPPA = DBL1 / DSQRT(ROOTSQ)
            BETA  = - RATIO * KAPPA
        ELSE
            RATIO = H22/H24
            ROOTSQ= DBL1 + (RATIO**2) * ROOTSQ
            BETA  = DBL1 / DSQRT(ROOTSQ)
            IF (H24   .GT. 0.0) BETA  = - BETA
            KAPPA = - RATIO * BETA
        ENDIF
C
*
        ALPHA = - (H14/H11) * KAPPA
C
c-->  transform these into the lab coordinate system
c-->  first get kappa and back to real dimensions
c
        KAPPA1 = KAPPA / RSCALE
        DBR0  =   0.5D0 / KAPPA1
C
c-->  next rotate alpha and beta and scale
C
        ALPHAR=     (COSROT * ALPHA - SINROT * BETA)*DBR0
        BETAR =     (SINROT * ALPHA + COSROT * BETA)*DBR0
C
c-->  then translate by (xav,yav)
C
        ACENT=-( ALPHAR - XAV)
        BCENT=-( BETAR  - YAV)
        R0   = DBR0
C
        IF(YRRAV .GT. 0) SGN = -1.0
        IF(YRRAV .LT. 0) SGN =  1.0
*
        RETURN
        END
