* $Id: utcmat.F,v 1.2 1998/01/27 00:35:54 fisyak Exp $
* $Log: utcmat.F,v $
* Revision 1.2  1998/01/27 00:35:54  fisyak
* Split sources
*
      SUBROUTINE UTCMAT(xcir,ycir,wcir,ncir)
C
C>----------------------------------------------------------------------
C
C UTCMAT - auxiliary routine for the circle fitting
C
C<----------------------------------------------------------------------

C
C      calculates weighted averages needed  *
C       for circle fitting, shifts, rotates and scales coordi  *
C       nate system so the whole procedure is simpler          *
C
      IMPLICIT DOUBLE PRECISION   (A-H , O-Z)
      DOUBLE PRECISION   KAPPA
      INTEGER NCIR,I
*
      COMMON /CIRCOM/ XAV   ,YAV   ,WSUM  ,RSCALE,COSROT,SINROT,
     >                XXAV  ,XYAV  ,YYAV  ,XRRAV ,YRRAV ,RRRRAV,
     >                ALPHA ,BETA  ,KAPPA
      real xcir(*),ycir(*),wcir(*)
C
C-->  CIRCLH CALCULATES AVERAGES USED IN THE H MATRIX OF THE CIRCLE FIT
C
C-->  H IS CALCULATED IN THE COORDINATE SYSTEM WHERE :
C
C-->  <X>    = 0
C-->  <Y>    = 0
C-->  <XY>   = 0
C-->  <R**2> = 1
C
C-->  IN THIS COORDINATE SYSTEM ONLY 5 ELEMENTS OF H ARE NEITHER 0 OR 1
C
C-->  THEY ARE : <X**2>, <Y**2>, <X R**2>, <Y R**2> & <R**4>
C
C-->  <X>  AND <Y>  BEFORE TRANSLATION ARE NEEDED TO GET BACK TO THE LAB
C
C-->  COSROT & SINROT ARE NEEDED TO GET BACK TO THE LAB
C
C-->  SIGN(Q) CAN BE DETERMINED WITH THE SIGNS CHOSEN FOR CROT & SINROT
C
C-->  WSUM   IS REQUIRED TO SCALE THE CHISQUARE
C-->  RSCALE IS REQUIRED TO SCALE KAPPA and CHISQUARE
C
C-->  FIRST CALCULATE <X> AND <Y> IN THE ORIGINAL CORRDINATE SYSTEM
C
        WSUM  = 0.0D0
C
        XAV   = 0.0D0
        YAV   = 0.0D0
C
        DO 10 I = 1,NCIR
C
            WI = 0.0D0
            XI = 0.0D0
            YI = 0.0D0
            WI = WCIR(I)
            XI = XCIR(I)
            YI = YCIR(I)
C
            WSUM = WSUM + WI
C
            XAV = XAV + WI * XI
            YAV = YAV + WI * YI
C
   10   CONTINUE
C
        XAV   = XAV    / WSUM
        YAV   = YAV    / WSUM
C
C-->  CALCULATE <X**2>, <XY>, AND <Y**2> WITH <X> = 0, & <Y> = 0
C
        XXAV  = 0.0D0
        XYAV  = 0.0D0
        YYAV  = 0.0D0
C
        DO 20 I = 1,NCIR
C
            WI = 0.0D0
            XI = 0.0D0
            YI = 0.0D0
            WI = WCIR(I)
            XI = XCIR(I)
            YI = YCIR(I)
C
            XI = XI - XAV
            YI = YI - YAV
C
            WIXI = WI * XI
            WIYI = WI * YI
C
            XXAV = XXAV + WIXI*XI
            XYAV = XYAV + WIXI*YI
            YYAV = YYAV + WIYI*YI
C
   20   CONTINUE
C
        XXAV  = XXAV   / WSUM
        XYAV  = XYAV   / WSUM
        YYAV  = YYAV   / WSUM
C
C-->  ROTATE COORDINATES SO THAT <XY> = 0
C
C-->  SIGN(C**2 - S**2) = SIGN(XXAV - YYAV) >
C-->  &                                     > ==> NEW : (XXAV-YYAV) > 0
C-->  SIGN(S) = SIGN(XYAV)                  >
C
        A     = DABS( XXAV - YYAV )
        B     = 4.0 * XYAV * XYAV
C
        ASQPB = A**2 + B
        RASQPB= DSQRT(ASQPB)
C
        SPLUS  = 1.0D0 + A / RASQPB
        SMINUS = B / (ASQPB * SPLUS)
C
        SPLUS = DSQRT(0.5 * SPLUS )
        SMINUS= DSQRT(0.5 * SMINUS)
C
C-->  FIRST REQUIRE : SIGN(C**2 - S**2) = SIGN(XXAV - YYAV)
C
        IF (XXAV .LE.YYAV ) THEN
           COSROT= SMINUS
           SINROT= SPLUS
        ELSE
           COSROT= SPLUS
           SINROT= SMINUS
        ENDIF
C
C-->  REQUIRE : SIGN(S) = SIGN(XYAV) * SIGN(C) (ASSUMING SIGN(C) > 0)
C
        IF (XYAV  .LT. 0.0D0) SINROT = - SINROT
C
C-->  WE NOW HAVE THE SMALLEST ANGLE THAT GUARANTEES <X**2> > <Y**2>
*
C-->  TO GET THE SIGN OF THE CHARGE RIGHT, THE NEW X-AXIS MUST POINT
C-->  OUTWARD FROM THE ORGIN.  WE ARE FREE TO CHANGE SIGNS OF BOTH
C-->  COSROT AND SINROT SIMULTANEOUSLY TO ACCOMPLISH THIS.
C
C-->  CHOOSE SIGN OF C WISELY TO BE ABLE TO GET THE SIGN OF THE CHARGE
C
	IF((COSROT*XAV + SINROT*YAV).GT. 0.0D0) GO TO 35
C
	COSROT= - COSROT
	SINROT= - SINROT
C
   35	CONTINUE
c
C
C-->  NOW GET <R**2> AND RSCALE= SQRT(<R**2>)
C
        RRAV  = XXAV  + YYAV
        RSCALE= DSQRT(RRAV)
C
        XXAV  = 0.0D0
        YYAV  = 0.0D0
        XRRAV = 0.0D0
        YRRAV = 0.0D0
        RRRRAV= 0.0D0
        XYAV  = 0.0D0
C
        DO 40 I = 1,NCIR
C
            WI = 0.0D0
            XOLD = 0.0D0
            YOLD = 0.0D0
            WI = WCIR(I)
            XOLD = XCIR(I)
            YOLD = YCIR(I)
C
            XOLD = XOLD - XAV
            YOLD = YOLD - YAV
C
C-->  ROTATE SO THAT <XY> = 0 & DIVIDE BY RSCALE SO THAT <R**2> = 1
C
            XI = ( COSROT * XOLD + SINROT * YOLD ) / RSCALE
            YI = ( - SINROT * XOLD + COSROT * YOLD ) / RSCALE
C
            XIXI = XI * XI
            YIYI = YI * YI
            RIRI = XIXI + YIYI
C
            WIRIRI= WI * RIRI
C
            XYAV = XYAV + WI * XI * YI
            XXAV = XXAV + WI * XIXI
            YYAV = YYAV + WI * YIYI
C
            XRRAV = XRRAV + WIRIRI * XI
            YRRAV = YRRAV + WIRIRI * YI
            RRRRAV= RRRRAV + WIRIRI * RIRI
C
   40   CONTINUE
C
C-->  DIVIDE BY WSUM TO MAKE AVERAGES
C
        XXAV  = XXAV   / WSUM
        YYAV  = YYAV   / WSUM
        XRRAV = XRRAV  / WSUM
        YRRAV = YRRAV  / WSUM
        RRRRAV= RRRRAV / WSUM
        XYAV  = XYAV   / WSUM
C
        RETURN
        END
