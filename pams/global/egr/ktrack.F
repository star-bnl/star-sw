      SUBROUTINE KTINFI(PARR,PARF,COVF)
************************************************************************
***                                                                  ***
***   Subroutine KTINFI initialise filter                            ***
***                                                                  ***
***   Purpose:                                                       ***
***      - set IPOS to 0                                             ***
***      - initialise fitted values at IPOS=0                        ***
***                                                                  ***
***   Input:                                                         ***
***      - PARR ... refernce track                                   ***
***      - PARF ... filtered parameter                               ***
***      - COVF ... filtered cov. matrix                             ***
***                                                                  ***
***   Remarks:                                                       ***
***      - filtered cov. matrix may have "infinite" values,          ***
***        if first measurement has less than 5 dimensions.          ***
***                                                                  ***
***      - for correct evaluation of ChiSquare you have to           ***
***        call KTMEAS afterwards                                    ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              PARR( 5), PARF( 5), COVF(15)

*     first filterd value

      DO 1000 I = 1, 5
         PARREF(I,0) = PARR(I)
         PARFIL(I,0) = PARF(I) - PARR(I)
 1000 CONTINUE

      DO 1010 I = 1, 15
         COVFIL(I,0) = COVF(I)
 1010 CONTINUE

      IPOS = 0

      END
      SUBROUTINE KTPRED(PARR,PARP,DPAR,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTPRED prediction without multiple scattering       ***
***                                                                  ***
***   Purpose:                                                       ***
***      - move IPOS to IPOS + 1                                     ***
***      - propagate last filtered cov. matrix                       ***
***                                                                  ***
***   Input:                                                         ***
***      - PARR reference parameter                                  ***
***      - DPAR derivative of the track model                        ***
***                                                                  ***
***   Output:                                                        ***
***      - PARP predicted parameter                                  ***
***      - IRET                                                      ***
***         0  ... o.k.                                              ***
***         1  ... IPOS > MAXPAR                                     ***
***         2  ... matrix inversion failed                           ***
***                (predicted cov. matrix is not positve definit)    ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              PARR(5), PARP(5), DPAR(5,5)

*     move to next IPOS

      IPOS = IPOS + 1
      IF (IPOS .GT. MAXPAR)                                    GOTO 9001

      IDMEAS(IPOS) = 0

*     store pred. & ref. parameter vec & derivative matrix

      DO 1000 I = 1, 5
         PARREF(I,IPOS) = PARR(I)
         DO 1100 J = 1, 5
            DERPAR(J,I,IPOS) = DPAR(J,I)
 1100    CONTINUE
 1000 CONTINUE

      DO 1050 I = 1, 15
         COVMUS(I,IPOS) = 0.D0
 1050 CONTINUE

*     calculate prediction

      CALL KMFDV5(DERPAR(1,1,IPOS),PARFIL(1,IPOS-1),PARPRE(1,IPOS))

      DO 1200 I = 1, 5
         PARP(I) = PARPRE(I,IPOS) + PARREF(I,IPOS)
 1200 CONTINUE

*     propagete filt cov mat from IPOS-1 to pred cov mat IPOS

      CALL KMFDC5(DERPAR(1,1,IPOS),COVFIL(1,IPOS-1),COVPRE(1,IPOS))

*     calculate inverse pred. cov. matrix (used later)

      CALL KMINC5(COVPRE(1,IPOS),COIPRE(1,IPOS),IERR)
      IF (IERR .NE. 0)                                         GOTO 9002

      IRET = 0
      RETURN

 9001 IRET = 1
      RETURN

 9002 IRET = 2
      RETURN

      END
      SUBROUTINE KTPREM(PARR,PARP,DPAR,COVMS,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTPREM prediction with multiple scattering          ***
***                                                                  ***
***   Purpose:                                                       ***
***      - move IPOS to IPOS + 1                                     ***
***      - propagate last filtered cov. matrix                       ***
***                                                                  ***
***   Input:                                                         ***
***      - PARR reference parameter                                  ***
***      - DPAR derivative of the track model                        ***
***      - COVMS multiple scattering                                 ***
***                                                                  ***
***   Output:                                                        ***
***      - PARP predicted parameter                                  ***
***      - IRET                                                      ***
***         0  ... o.k.                                              ***
***         1  ... IPOS > MAXPAR                                     ***
***         2  ... matrix inversion failed                           ***
***                (predicted cov. matrix is not positve definit)    ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              PARR(5), PARP(5), DPAR(5,5), COVMS(15)

*     move to next IPOS

      IPOS = IPOS + 1
      IF (IPOS .GT. MAXPAR)                                    GOTO 9001

      IDMEAS(IPOS) = 0

*     store pred. & ref. parameter vec & derivative matrix

      DO 1000 I = 1, 5
         PARREF(I,IPOS) = PARR(I)
         DO 1100 J = 1, 5
            DERPAR(J,I,IPOS) = DPAR(J,I)
 1100    CONTINUE
 1000 CONTINUE

      DO 1200 I = 1, 15
         COVMUS(I,IPOS) = COVMS(I)
 1200 CONTINUE

*     calculate prediction

      CALL KMFDV5(DERPAR(1,1,IPOS),PARFIL(1,IPOS-1),PARPRE(1,IPOS))

      DO 1300 I = 1, 5
         PARP(I) = PARPRE(I,IPOS) + PARREF(I,IPOS)
 1300 CONTINUE

*     propagete filt cov mat from IPOS-1 to pred cov mat IPOS

      CALL KMFDC5(DERPAR(1,1,IPOS),COVFIL(1,IPOS-1),COVPRE(1,IPOS))

*     add multiple scattering

      DO 1400 I = 1, 15
         COVPRE(I,IPOS) = COVPRE(I,IPOS) + COVMUS(I,IPOS)
 1400 CONTINUE

*     calculate inverse pred. cov. matrix (used later)

      CALL KMINC5(COVPRE(1,IPOS),COIPRE(1,IPOS),IERR)
      IF (IERR .NE. 0)                                         GOTO 9002

      IRET = 0
      RETURN

 9001 IRET = 1
      RETURN

 9002 IRET = 2
      RETURN

      END
      SUBROUTINE KTMEAS(IDIM,VMEA,CMEA,HTMEA,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTMEAS store measurement                            ***
***                                                                  ***
***   Purpose:                                                       ***
***      - storing of a measurement                                  ***
***                                                                  ***
***   Input:                                                         ***
***      - IDIM ... Dimension of Measurement (1,2 supported)         ***
***      - VMEA ... measurment vector                                ***
***      - CMEA ... cov. matrix of measurement vector                ***
***      - HTMEA... transposed projection matrix                     ***
***                                                                  ***
***   Output:                                                        ***
***      - IRET                                                      ***
***         0 ... o.k.                                               ***
***         1 ... cov. matrix of measurement not pos. definit        ***
***         2 ... IDIM > MAXMDV                                      ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              VMEA(*), CMEA(*), HTMEA(5,*)
      DIMENSION         VREF(MAXMDV)

*     store measurement

      IF (IDIM .GT. MAXMDV)                                    GOTO 9001
      IDMEAS(IPOS)   = IDIM

      DO 1000 I = 1, IDIM * (IDIM + 1) / 2
         COVMEA(I,IPOS) = CMEA(I)
 1000 CONTINUE

      DO 1020 I = 1, IDIM
         DO 1010 J = 1, 5
            HTMEAS(J,I,IPOS) = HTMEA(J,I)
 1010    CONTINUE
 1020 CONTINUE

      CALL KMHTV5(IDMEAS(IPOS),HTMEAS(1,1,IPOS),PARREF(1,IPOS),VREF)
      DO 1030 I = 1, IDIM
         VECMEA(I,IPOS) = VMEA(I) - VREF(I)
 1030 CONTINUE

      CALL KMINCI(IDMEAS(IPOS),COVMEA(1,IPOS),COIMEA(1,IPOS),IERR)
      IF (IERR .NE. 0)                                         GOTO 9002

      IRET = 0
      RETURN

 9001 IRET = 1
      RETURN

 9002 IRET = 2
      RETURN

      END
      SUBROUTINE KTFILT(PARF,COVF,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTFILT filter step                                  ***
***                                                                  ***
***   Purpose:                                                       ***
***      - filtering of one-dimensional measurement vector           ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              PARF(5), COVF(15)

      DIMENSION         T(15), P(5), Q(5), V(MAXMDV)

      IF (IDMEAS(IPOS).EQ.0) THEN
         CALL KTFOUT(PARF(1),COVF(1))
         IRET = 0
         RETURN
      ENDIF

*     calculate filtered cov. matrix

      CALL KMHTCI(IDMEAS(IPOS),HTMEAS(1,1,IPOS),COIMEA(1,IPOS),T)

      DO 1000 I = 1, 15
         T(I) =  COIPRE(I,IPOS) +  T(I)
 1000 CONTINUE

      CALL KMINC5(T,COVFIL(1,IPOS),IERR)
      IF (IERR .NE. 0)                                         GOTO 9001

*     calculate filtered parameter vector

      CALL KMCIVI(5,COIPRE(1,IPOS),PARPRE(1,IPOS),P)

      CALL KMCIVI(IDMEAS(IPOS),COIMEA(1,IPOS),VECMEA(1,IPOS),V)
      CALL KMHTVI(IDMEAS(IPOS),HTMEAS(1,1,IPOS),V,Q)

      DO 2000 I = 1, 5
         P(I) = P(I) + Q(I)
 2000 CONTINUE

      CALL KMCIVI(5,COVFIL(1,IPOS),P,PARFIL(1,IPOS))


      DO 2100 I = 1, 5
         PARF(I) = PARFIL(I,IPOS) + PARREF(I,IPOS)
 2100 CONTINUE

      DO 2200 I = 1, 15
         COVF(I) = COVFIL(I,IPOS)
 2200 CONTINUE

      IRET = 0
      RETURN

 9001 IRET = 1
      RETURN

      END
      SUBROUTINE KTFCH2(RES,PUL,CH2,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTFCH2 calculate filtered chi-square update         ***
***                                                                  ***
***   Purpose:                                                       ***
***      - calculate filtered chi-square update                      ***
***                                                                  ***
***   Output:                                                        ***
***      - RES ...  residual vector                                  ***
***      - PUL ...  pull quantity                                    ***
***      - CH2 ...  chi-square update                                ***
***      - IRET ...                                                  ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              RES(*), PUL(*), CH2
      DIMENSION         DRES(MAXMDV), DPUL(MAXMDV)

      CALL KTCCH2(IDMEAS(IPOS),VECMEA(1,IPOS),COVMEA(1,IPOS),
     +            HTMEAS(1,1,IPOS),PARFIL(1,IPOS),COVFIL(1,IPOS),
     +            DRES,DPUL,DCH2,IRET)

      DO 1000 I = 1, IDMEAS(IPOS)
         RES(I) = DRES(I)
         PUL(I) = DPUL(I)
 1000 CONTINUE
      CH2 = DCH2
      RETURN

      END
      SUBROUTINE KTFOUT(PARF,COVF)
************************************************************************
***                                                                  ***
***   Subroutine KTFOUT remove an outlier during filtering           ***
***                                                                  ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL               PARF(5), COVF(15)

      DO 1000 I = 1, 5
         PARFIL(I,IPOS) = PARPRE(I,IPOS)
         PARF  (I)      = PARFIL(I,IPOS) + PARREF(I,IPOS)
 1000 CONTINUE

      DO 1100 I = 1, 15
         COVFIL(I,IPOS) = COVPRE(I,IPOS)
         COVF  (I)      = COVFIL(I,IPOS)
 1100 CONTINUE

      RETURN

      END
      SUBROUTINE KTINSM(PSMO,CSMO)
************************************************************************
***                                                                  ***
***   Subroutine KTINSM init smoothing                               ***
***                                                                  ***
***   Purpose:                                                       ***
***      - Init Smoothing at IPOS                                    ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL               PSMO(5), CSMO(15)

*     last filtered value is first smoothed

      DO 1000 I = 1, 5
         PARSMO(I,IPOS) = PARFIL(I,IPOS)
         PSMO(I)        = PARSMO(I,IPOS) + PARREF(I,IPOS)
 1000 CONTINUE

      DO 1010 I = 1, 15
         COVSMO(I,IPOS) = COVFIL(I,IPOS)
         CSMO(I)        = COVSMO(I,IPOS)
 1010 CONTINUE
      RETURN

      END
      SUBROUTINE KTSMOO(PSMO,CSMO,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTSMOO smoother step                                ***
***                                                                  ***
***   Purpose:                                                       ***
***      - Smoothing at IPOS                                         ***
***      - IPOS = IPOS - 1                                           ***
***                                                                  ***
***   Output:                                                        ***
***      - PSMO .... smoothed parameter vector                       ***
***      - CSMO .... smoothed covariance matrix                      ***
***      - IRET                                                      ***
***         0 .... o.k.                                              ***
***         1 .... IPOS = 0                                          ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              PSMO(5), CSMO(15)

      DIMENSION         A(5,5), C(15), C1(15), P(5), Q(5)

      IPOS = IPOS - 1
      IF (IPOS .LT. 0)                                         GOTO 9001

*     calculate smoother gain matrix

      CALL KMSMOO(COVFIL(1,IPOS),DERPAR(1,1,IPOS+1),COIPRE(1,IPOS+1),A)

*     calculate smoothed parameter vector

      DO 1000 I = 1, 5
         P(I) = PARSMO(I,IPOS+1) - PARPRE(I,IPOS+1)
 1000 CONTINUE

      CALL KMA5V5(A,P,Q)

      DO 1100 I = 1, 5
         PARSMO(I,IPOS) = PARFIL(I,IPOS) + Q(I)
         PSMO(I)        = PARSMO(I,IPOS) + PARREF(I,IPOS)
 1100 CONTINUE

*     calculate smoothed covariance matrix


      DO 2000 I = 1, 15
         C(I) = COVSMO(I,IPOS+1) - COVPRE(I,IPOS+1)
 2000 CONTINUE

      CALL KMA5C5(A,C,C1)

      DO 3000 I = 1, 15
         COVSMO(I,IPOS) = COVFIL(I,IPOS) + C1(I)
         CSMO(I)        = COVSMO(I,IPOS)
 3000 CONTINUE

      IRET = 0
      RETURN

 9001 IRET = 1
      RETURN

      END
      SUBROUTINE KTSCH2(RES,PUL,CH2,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTSCH2 calculate smoothed chi-square update         ***
***                                                                  ***
***   Purpose:                                                       ***
***      - calculate smoothed chi-square update                      ***
***                                                                  ***
***   Output:                                                        ***
***      - RES ...  residual vector                                  ***
***      - PUL ...  pull quantities                                  ***
***      - CH2 ...  chi-square update                                ***
***      - IRET ...                                                  ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              RES(*), PUL(*), CH2
      DIMENSION         DRES(MAXMDV), DPUL(MAXMDV)

      CALL KTCCH2(IDMEAS(IPOS),VECMEA(1,IPOS),COVMEA(1,IPOS),
     +            HTMEAS(1,1,IPOS),PARSMO(1,IPOS),COVSMO(1,IPOS),
     +            DRES,DPUL,DCH2,IRET)

      DO 1000 I = 1, IDMEAS(IPOS)
         RES(I) = DRES(I)
         PUL(I) = DPUL(I)
 1000 CONTINUE
      CH2 = DCH2
      RETURN

      END
      SUBROUTINE KTSOUT(PARS,COVS,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTSOUT remove an outlier during smoothing           ***
***                                                                  ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              PARS(5), COVS(15)

      DIMENSION         R(15), T(15), P(5), Q(5), V(MAXMDV)

*     calculate filtered cov. matrix

      CALL KMHTCI(IDMEAS(IPOS),HTMEAS(1,1,IPOS),COIMEA(1,IPOS),T)

      CALL KMINC5(COVSMO(1,IPOS),R,IERR)
      IF (IERR .NE. 0)                                         GOTO 9001

      DO 1000 I = 1, 15
         T(I) =  R(I) -  T(I)
 1000 CONTINUE

      CALL KMINC5(T,COVSMO(1,IPOS),IERR)
      IF (IERR .NE. 0)                                         GOTO 9001

*     calculate filtered parameter vector

      CALL KMCIVI(5,R,PARSMO(1,IPOS),P)

      CALL KMCIVI(IDMEAS(IPOS),COIMEA(1,IPOS),VECMEA(1,IPOS),V)
      CALL KMHTVI(IDMEAS(IPOS),HTMEAS(1,1,IPOS),V,Q)

      DO 2000 I = 1, 5
         P(I) = P(I) - Q(I)
 2000 CONTINUE

      CALL KMCIVI(5,COVSMO(1,IPOS),P,PARSMO(1,IPOS))


      DO 2100 I = 1, 5
         PARS(I) = PARSMO(I,IPOS) + PARREF(I,IPOS)
 2100 CONTINUE

      DO 2200 I = 1, 15
         COVS(I) = COVSMO(I,IPOS)
 2200 CONTINUE

      IRET = 0
      RETURN

 9001 IRET = 1
      RETURN

      END
      SUBROUTINE KTINRF(PARF,COVF)
************************************************************************
***                                                                  ***
***   Subroutine KTINRF initilise refitting                          ***
***                                                                  ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              PARF(5), COVF(15)

      DO 1000 I = 1, 5
         PARF(I) = PARFIL(I,0) + PARREF(I,0)
 1000 CONTINUE

      DO 1100 I = 1, 15
         COVF(I) = COVFIL(I,0)
 1100 CONTINUE

      IPOS = 0

      RETURN

      END
      SUBROUTINE KTREPR(PARP,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTREPR prediction during refitting                  ***
***                                                                  ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      REAL              PARP(5), COVP(15)

      IPOS = IPOS + 1
      IF (IPOS .GT. MAXPAR)                                    GOTO 9001

      CALL KMFDV5(DERPAR(1,1,IPOS),PARFIL(1,IPOS-1),PARPRE(1,IPOS))

      DO 1000 I = 1, 5
         PARP(I)        = PARPRE(I,IPOS) + PARREF(I,IPOS)
 1000 CONTINUE

*     propagete filt cov mat from IPOS-1 to pred cov mat IPOS

      CALL KMFDC5(DERPAR(1,1,IPOS),COVFIL(1,IPOS-1),COVPRE(1,IPOS))

*     add multiple scattering

      DO 1100 I = 1, 15
         COVPRE(I,IPOS) = COVPRE(I,IPOS) + COVMUS(I,IPOS)
 1100 CONTINUE

*     calculate inverse pred. cov. matrix (used later)

      CALL KMINC5(COVPRE(1,IPOS),COIPRE(1,IPOS),IERR)
      IF (IERR .NE. 0)                                         GOTO 9002

      DO 1200 I = 1, 15
         COVP(I) = COVPRE(I,IPOS)
 1200 CONTINUE

      IRET = 0
      RETURN

 9001 CONTINUE
      IRET = 1
      RETURN

 9002 CONTINUE
      IRET = 2
      RETURN

      END
      SUBROUTINE KTCCH2(ID,VMEA,CMEA,HT,PAR,COV,RES,PUL,CH2,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTCCH2 calculate chi-square update                  ***
***                                                                  ***
***   Purpose:                                                       ***
***      - calculate smoothed chi-square update                      ***
***                                                                  ***
***   Output:                                                        ***
***      - RES ...  residual vector                                  ***
***      - CH2 ...  chi-square update                                ***
***      - IRET ... Return Code                                      ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************


      DIMENSION          VMEA(*), CMEA(*), HT(5,*), PAR(5), COV(15)
      DIMENSION          RES(*), PUL(*)

      DIMENSION          COVRES(MAXMDC), COIRES(MAXMDC)

      CALL KMHTV5(ID,HT,PAR,RES)
      DO 1000 I = 1, ID
         RES(I) = VMEA(I) - RES(I)
 1000 CONTINUE

      CALL KMHTC5(ID,HT,COV,COVRES)
      DO 1100 I = 1, ID * (ID + 1) / 2
         COVRES(I) = CMEA(I) - COVRES(I)
 1100 CONTINUE

      CALL KMINCI(ID,COVRES,COIRES,IERR)
      IF (IERR .NE. 0)                                         GOTO 9001
      CALL KMVCVI(ID,RES,COIRES,CH2)
      IF (CH2 .LT. 0.)                                         GOTO 9002
      IF (ID .EQ. 1) THEN
         IF ( COVRES(1) .LT. 0.)                               GOTO 9003
         PUL(1) = RES(1) / SQRT( COVRES(1) )
      ELSEIF (ID .EQ. 2) THEN
         IF ( COVRES(1) .LT. 0.)                               GOTO 9003
         IF ( COVRES(3) .LT. 0.)                               GOTO 9003
         PUL(1) = RES(1) / SQRT( COVRES(1) )
         PUL(2) = RES(2) / SQRT( COVRES(3) )
      ELSE
         WRITE(6,'('' Hard luck. Not supported yet.'')')
      ENDIF

 2000 CONTINUE

      IRET = 0
      RETURN

 9001 IRET = 1
      RETURN

 9002 IRET = 2
      RETURN

 9003 IRET = 3
      RETURN

      END
      SUBROUTINE KTWPRE(LUN)
************************************************************************
***                                                                  ***
***   Subroutine KTWPRE write predicted values                       ***
***                                                                  ***
***   Purpose:                                                       ***
***      - write predicted parameter vector and covariance matrix    ***
***                                                                  ***
***   Input:                                                         ***
***      - LUN ...  I/O Unit                                         ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      DIMENSION         P(5)

      DO 1000 I = 1, 5
         P(I) = PARREF(I,IPOS) + PARPRE(I,IPOS)
 1000 CONTINUE

      CALL KTWPAR(LUN,'Predicted',P,COVPRE(1,IPOS),IPOS)
      RETURN

      END
      SUBROUTINE KTWFIL(LUN)
************************************************************************
***                                                                  ***
***   Subroutine KTWFIL write filtered  values                       ***
***                                                                  ***
***   Purpose:                                                       ***
***      - write filtered parameter vector and covariance matrix     ***
***                                                                  ***
***   Input:                                                         ***
***      - LUN ...  I/O Unit                                         ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      DIMENSION            P(5)

      DO 1000 I = 1, 5
         P(I) = PARREF(I,IPOS) + PARFIL(I,IPOS)
 1000 CONTINUE

      CALL KTWPAR(LUN,'Filtered',P,COVFIL(1,IPOS),IPOS)

      END
      SUBROUTINE KTWSMO(LUN)
************************************************************************
***                                                                  ***
***   Subroutine KTWSMO write smoothed  values                       ***
***                                                                  ***
***   Purpose:                                                       ***
***      - write smoothed parameter vector and covariance matrix     ***
***                                                                  ***
***   Input:                                                         ***
***      - LUN ...  I/O Unit                                         ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      DIMENSION         P(5)

      DO 1000 I = 1, 5
         P(I) = PARREF(I,IPOS) + PARSMO(I,IPOS)
 1000 CONTINUE

      CALL KTWPAR(LUN,'Smoothed',P,COVSMO(1,IPOS),IPOS)

      END
      SUBROUTINE KTWPAR(LUN,TEXT,PAR,COV,IPOS)
************************************************************************
***                                                                  ***
***   Subroutine KTWPAR write parameter & covariance matrix          ***
***                                                                  ***
***   Purpose: Output of Parameters for Trace                        ***
***                                                                  ***
***                                                                  ***
***   Input:                                                         ***
***     - LUN  ... I/O-Unit                                          ***
***     - TEXT ... description                                       ***
***     - PAR  ... parameter vector                                  ***
***     - COV  ... covariance matrix                                 ***
***                                                                  ***
***                                                   Dietrich Liko  ***
*************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION         PAR(5),COV(15)
      CHARACTER*(*)     TEXT

      REAL              C(5,5),EV(5),WORK(5)

      K=0
      DO 1000 I = 1, 5
         DO 1010 J = I, 5
            K = K + 1
            C(I,J) = COV(K)
            C(J,I) = COV(K)
 1010    CONTINUE
 1000 CONTINUE

      WRITE(LUN,'(1X,A,'' (IPOS = '',I3,'')''/)') TEXT, IPOS
      WRITE(LUN,'('' Par: '',5(F12.8,1X)/)') (PAR(I),I=1,5)
      WRITE(LUN,'(5('' Cov: '',5(F12.8,1X),/))') ((C(I,J),J=1,5),I=1,5)

CCC A.C.S.      CALL EISRS2(5,5,C,EV,IERR,WORK)

      WRITE(LUN,'('' Ev : '',5(F12.8,1X),/)') (EV(I),I=1,5)
      RETURN

      END
      SUBROUTINE KTWDER(LUN)
************************************************************************
***                                                                  ***
***   Subroutine KTWDER write derivative matrix                      ***
***                                                                  ***
***   Input:                                                         ***
***     - LUN ... I/O Unit                                           ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      WRITE(LUN,'('' Derivative''/)')
      WRITE(LUN,'(5(''      '',5(F12.8,1X)/))')
     +          ((DERPAR(I,J,IPOS),J=1,5),I=1,5),0.,0.,0.,0.,1.
      RETURN

      END
      SUBROUTINE KTWREF(LUN)
************************************************************************
***                                                                  ***
***   Subroutine KTWREF write reference track                        ***
***                                                                  ***
***   Input:                                                         ***
***     - LUN ... I/O Unit                                           ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*     Kalman Track Fit Constants                                       *
*                                                                      *
************************************************************************

      PARAMETER        (MAXPAR  = 100,
     +                  MAXMDV  = 2)

      PARAMETER        (MAXMDC = MAXMDV * ( MAXMDV + 1 ) / 2 )

************************************************************************
*                                                                      *
*     MAXPAR  ... max. nr. of parameter vector per track               *
*     MAXMDV  ... max. dimension of measurement vector                 *
*     MAXMDC  ... max. dimension of cov. matrix of meas. vector        *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*     Kalman Track Fit Variables                                       *
*                                                                      *
************************************************************************

      COMMON           /SCRTCH/IPOS,IDUMMY,
     +                         PARREF( 5,0:MAXPAR), PARPRE( 5,0:MAXPAR),
     +                         COVPRE(15,0:MAXPAR), COIPRE(15,0:MAXPAR),
     +                         PARFIL( 5,0:MAXPAR), COVFIL(15,0:MAXPAR),
     +                         PARSMO( 5,0:MAXPAR), COVSMO(15,0:MAXPAR),
     +                         DERPAR(5,5,MAXPAR),  COVMUS(15,MAXPAR),
     +                         VECMEA(MAXMDV,0:MAXPAR),
     +                         COVMEA(MAXMDC,0:MAXPAR),
     +                         COIMEA(MAXMDC,0:MAXPAR),
     +                         HTMEAS(5,MAXMDV,0:MAXPAR),
     +                         IDMEAS(0:MAXPAR)
************************************************************************
*                                                                      *
*     IPOS        .... position                                        *
*     PARREF      .... parameter vectors of reference track            *
*     PARPRE      .... parameter vectors of prediction                 *
*     COVPRE      .... covariance matrix of prediction                 *
*     COIPRE      .... inverse covariance matrix of prediction         *
*     PARFIL      .... parameter vector of filter step                 *
*     COVFIL      .... covariance matrix of filter step                *
*     PARSMO      .... parameter vector of smoother step               *
*     COVSMO      .... covariance matrix of smoother step              *
*     IDMEAS      .... dimension of measurement vector                 *
*     DERPAR      .... derivative matrix of paramter vector            *
*     VECMEA      .... measurement vector                              *
*     COVMEA      .... cov. matrix of mesurement vector                *
*     COIMEA      .... inv. cov. matrix (weight matrix) of measurement *
*     HTMEAS      .... transposed projection matrix of mesurement      *
*                                                                      *
************************************************************************

      WRITE(LUN,'('' Ref: '',5(F12.8,1X)/)') (PARREF(I,IPOS),I=1,5)
      RETURN

      END
      SUBROUTINE KTCHMI(A,B,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KTCHMI checks Matrix Invesion                       ***
***                                                                  ***
***   Input:                                                         ***
***     - A ... cov. matrix                                          ***
***     - C ... inverted cov. matrix                                 ***
***                                                                  ***
***   Output:                                                        ***
***     - IRET                                                       ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      PARAMETER        (EPS = 1.E-3)

      REAL              A(15), B(15), C(5,5)

      CALL KMC5C5(A,B,C)

      DO 1010 I = 1, 5
         DO 1000 J = 1, 5
            IF (I .EQ. J) THEN
               IF ( ABS( C(I,J) - 1.0 ) .GT. EPS )             GOTO 9001
            ELSE
               IF ( ABS( C(I,J) ) .GT. EPS )                   GOTO 9001
            ENDIF
 1000    CONTINUE
 1010 CONTINUE

      IRET = 0
      RETURN

 9001 IRET = 1
      WRITE(6,'('' KTCHMI - Error in matrix inversion detected.'')')
      RETURN

      END
      SUBROUTINE KMINCI(ID,A,B,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KMINCI B := 1 / A    (dim ID)                       ***
***                                                                  ***
***   Input:                                                         ***
***      A    .... cov. matrix                                       ***
***                                                                  ***
***   Output:                                                        ***
***      B    .... cov. matrix                                       ***
***      IRET                                                        ***
***        0 .... o.k.                                               ***
***        1 ...  Inversion failed                                   ***
***                                                                  ***
***   Remark:                                                        ***
***      For the moment only ID<=2 is supported                      ***
***      No check is made if ID is correct!                          ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
      PARAMETER        (EPS5 = 1.D-30,
     +                  EPS3 = 1.D-30,
     +                  EPS2 = 1.D-30,
     +                  EPS1 = 1.D-30)

      DIMENSION         A(*), B(*)

      GOTO (1000, 2000) ID

 1000 CONTINUE
      IF (A(1) .LT. EPS1)                                      GOTO 9001
      B(1) = 1. / A(1)
      IRET = 0
      RETURN

 2000 CONTINUE


      DET = A(1) * A(3) - A(2) * A(2)
      IF (ABS(DET) .LT. EPS2)                                  GOTO 9001
      B(1) = A(3) / DET
      B(2) =-A(2) / DET
      B(3) = A(1) / DET
      IRET = 0

      RETURN

 9001 CONTINUE
      IRET = 1

      END
      SUBROUTINE KMINC5(A,B,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KMINC5 B := 1 / A    (dim 5)                        ***
***                                                                  ***
***   Purpose:                                                       ***
***     - calculates the inverse of A giving B                       ***
***     - uses Cholesky decomposition                                ***
***                                                                  ***
***   Input:                                                         ***
***     - A           pos. def. and sym. matrix                      ***
***                                                                  ***
***   Output:                                                        ***
***     - B           inv. matrix(also pos. def. and sym.)           ***
***     - IRET        Error Matrix Inversion                         ***
***        0 ...      no error                                       ***
***        1 ...      matrix not pos. definit                        ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************
*                                                                      *
*     Reference:                                                       *
*     Mueller/Jordan : Formelsammlung zur Numerischen Mathematik       *
*                                                                      *
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
      PARAMETER        (EPS5 = 1.D-30,
     +                  EPS3 = 1.D-30,
     +                  EPS2 = 1.D-30,
     +                  EPS1 = 1.D-30)

      DIMENSION         A(15), B(15)

      S11 = A( 1)
      IF ( S11 .LT. EPS5 )                                     GOTO 9001
      S11 = SQRT( S11 )
      S12 = ( A( 2) ) / S11
      S13 = ( A( 3) ) / S11
      S14 = ( A( 4) ) / S11
      S15 = ( A( 5) ) / S11
      S22 = A( 6) - S12 * S12
      IF ( S22 .LT. EPS5 )                                     GOTO 9001
      S22 = SQRT( S22 )
      S23 = ( A( 7) - S12 * S13 ) / S22
      S24 = ( A( 8) - S12 * S14 ) / S22
      S25 = ( A( 9) - S12 * S15 ) / S22
      S33 = A(10) - S13 * S13 - S23 * S23
      IF ( S33 .LT. EPS5 )                                     GOTO 9001
      S33 = SQRT( S33 )
      S34 = ( A(11) - S13 * S14 - S23 * S24 ) / S33
      S35 = ( A(12) - S13 * S15 - S23 * S25 ) / S33
      S44 = A(13) - S14 * S14 - S24 * S24 - S34 * S34
      IF ( S44 .LT. EPS5 )                                     GOTO 9001
      S44 = SQRT( S44 )
      S45 = ( A(14) - S14 * S15 - S24 * S25 - S34 * S35 ) / S44
      S55 = A(15) - S15 * S15 - S25 * S25 - S35 * S35 - S45 * S45
      IF ( S55 .LT. EPS5 )                                     GOTO 9001
      S55 = SQRT( S55 )
      T11 = 1.0 / S11
      T22 = 1.0 / S22
      T33 = 1.0 / S33
      T44 = 1.0 / S44
      T55 = 1.0 / S55
      B(15) = ( T55 ) * T55
      B(14) = - ( S45 * B(15) ) * T44
      B(12) = - ( S34 * B(14) + S35 * B(15) ) * T33
      B( 9) = - ( S23 * B(12) + S24 * B(14) + S25 * B(15) ) * T22
      B( 5) = - ( S12 * B( 9) + S13 * B(12) + S14 * B(14) + S15 *
     +        B(15) ) * T11
      B(13) = ( T44 - S45 * B(14) ) * T44
      B(11) = - ( S34 * B(13) + S35 * B(14) ) * T33
      B( 8) = - ( S23 * B(11) + S24 * B(13) + S25 * B(14) ) * T22
      B( 4) = - ( S12 * B( 8) + S13 * B(11) + S14 * B(13) + S15 *
     +        B(14) ) * T11
      B(10) = ( T33 - S34 * B(11) - S35 * B(12) ) * T33
      B( 7) = - ( S23 * B(10) + S24 * B(11) + S25 * B(12) ) * T22
      B( 3) = - ( S12 * B( 7) + S13 * B(10) + S14 * B(11) + S15 *
     +        B(12) ) * T11
      B( 6) = ( T22 - S23 * B( 7) - S24 * B( 8) - S25 * B( 9) ) * T22
      B( 2) = - ( S12 * B( 6) + S13 * B( 7) + S14 * B( 8) + S15 *
     +        B( 9) ) * T11
      B( 1) = ( T11 - S12 * B( 2) - S13 * B( 3) - S14 * B( 4) - S15 *
     +        B( 5) ) * T11

      IRET = 0
      RETURN

 9001 IRET = 1

      END
      SUBROUTINE KMINC3(A,B,IRET)
************************************************************************
***                                                                  ***
***   Subroutine KMINC3 B := 1 / A    (dim 3)                        ***
***                                                                  ***
***   Purpose:                                                       ***
***     - calculates the inverse of A giving B                       ***
***     - uses Cholesky decomposition                                ***
***                                                                  ***
***   Input:                                                         ***
***     - A           pos. def. and sym. matrix                      ***
***                                                                  ***
***   Output:                                                        ***
***     - B           inv. matrix(also pos. def. and sym.)           ***
***     - IRET        Error Matrix Inversion                         ***
***        0 ...      no error                                       ***
***        1 ...      matrix not pos. definit                        ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************
*                                                                      *
*     Reference:                                                       *
*     Mueller/Jordan : Formelsammlung zur Numerischen Mathematik       *
*                                                                      *
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
      PARAMETER        (EPS5 = 1.D-30,
     +                  EPS3 = 1.D-30,
     +                  EPS2 = 1.D-30,
     +                  EPS1 = 1.D-30)

      DIMENSION         A(6), B(6)

      S11 = A(1)
      IF ( S11 .LT. EPS3 )                                     GOTO 9001
      S11 = SQRT( S11 )
      S12 = ( A(2) ) / S11
      S13 = ( A(3) ) / S11
      S22 = A(4) - S12 * S12
      IF ( S22 .LT. EPS3 )                                     GOTO 9001
      S22 = SQRT( S22 )
      S23 = ( A(5) - S12 * S13 ) / S22
      S33 = A(6) - S13 * S13 - S23 * S23
      IF ( S33 .LT. EPS3 )                                     GOTO 9001
      S33 = SQRT( S33 )
      T11 = 1.0 / S11
      T22 = 1.0 / S22
      T33 = 1.0 / S33
      B(6) = ( T33 ) * T33
      B(5) = - ( S23 * B(6) ) * T22
      B(3) = - ( S12 * B(5) + S13 * B(6) ) * T11
      B(4) = ( T22 - S23 * B(5) ) * T22
      B(2) = - ( S12 * B(4) + S13 * B(5) ) * T11
      B(1) = ( T11 - S12 * B(2) - S13 * B(3) ) * T11

      IRET = 0
      RETURN

 9001 IRET = 1

      END
      SUBROUTINE KMFDC5(F,A,B)
************************************************************************
***                                                                  ***
***   Subroutine KMFDC5 B := F * A * T(F)                            ***
***                                                                  ***
***   Input:                                                         ***
***      F  ... derivative of track model                            ***
***      A  ... cov. matrix                                          ***
***                                                                  ***
***   Output:                                                        ***
***      B  ... cov. matrix                                          ***
***                                                                  ***
***                                                   Dietrich Liko  ***
*** Modified to 5x5 by A.C. Saulys                                   ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION          F(5,5), A(15), B(15)

      T11 = F(1,1) * A( 1) + F(1,2) * A( 2) +
     +      F(1,3) * A( 3) + F(1,4) * A( 4) + F(1,5) * A( 5)
      T12 = F(1,1) * A( 2) + F(1,2) * A( 6) +
     +      F(1,3) * A( 7) + F(1,4) * A( 8) + F(1,5) * A( 9)
      T13 = F(1,1) * A( 3) + F(1,2) * A( 7) +
     +      F(1,3) * A(10) + F(1,4) * A(11) + F(1,5) * A(12)
      T14 = F(1,1) * A( 4) + F(1,2) * A( 8) +
     +      F(1,3) * A(11) + F(1,4) * A(13) + F(1,5) * A(14)
      T15 = F(1,1) * A( 5) + F(1,2) * A( 9) +
     +      F(1,3) * A(12) + F(1,4) * A(14) + F(1,5) * A(15)
      T21 = F(2,1) * A( 1) + F(2,2) * A( 2) +
     +      F(2,3) * A( 3) + F(2,4) * A( 4) + F(2,5) * A( 5)
      T22 = F(2,1) * A( 2) + F(2,2) * A( 6) +
     +      F(2,3) * A( 7) + F(2,4) * A( 8) + F(2,5) * A( 9)
      T23 = F(2,1) * A( 3) + F(2,2) * A( 7) +
     +      F(2,3) * A(10) + F(2,4) * A(11) + F(2,5) * A(12)
      T24 = F(2,1) * A( 4) + F(2,2) * A( 8) +
     +      F(2,3) * A(11) + F(2,4) * A(13) + F(2,5) * A(14)
      T25 = F(2,1) * A( 5) + F(2,2) * A( 9) +
     +      F(2,3) * A(12) + F(2,4) * A(14) + F(2,5) * A(15)
      T31 = F(3,1) * A( 1) + F(3,2) * A( 2) +
     +      F(3,3) * A( 3) + F(3,4) * A( 4) + F(3,5) * A( 5)
      T32 = F(3,1) * A( 2) + F(3,2) * A( 6) +
     +      F(3,3) * A( 7) + F(3,4) * A( 8) + F(3,5) * A( 9)
      T33 = F(3,1) * A( 3) + F(3,2) * A( 7) +
     +      F(3,3) * A(10) + F(3,4) * A(11) + F(3,5) * A(12)
      T34 = F(3,1) * A( 4) + F(3,2) * A( 8) +
     +      F(3,3) * A(11) + F(3,4) * A(13) + F(3,5) * A(14)
      T35 = F(3,1) * A( 5) + F(3,2) * A( 9) +
     +      F(3,3) * A(12) + F(3,4) * A(14) + F(3,5) * A(15)
      T41 = F(4,1) * A( 1) + F(4,2) * A( 2) +
     +      F(4,3) * A( 3) + F(4,4) * A( 4) + F(4,5) * A( 5)
      T42 = F(4,1) * A( 2) + F(4,2) * A( 6) +
     +      F(4,3) * A( 7) + F(4,4) * A( 8) + F(4,5) * A( 9)
      T43 = F(4,1) * A( 3) + F(4,2) * A( 7) +
     +      F(4,3) * A(10) + F(4,4) * A(11) + F(4,5) * A(12)
      T44 = F(4,1) * A( 4) + F(4,2) * A( 8) +
     +      F(4,3) * A(11) + F(4,4) * A(13) + F(4,5) * A(14)
      T45 = F(4,1) * A( 5) + F(4,2) * A( 9) +
     +      F(4,3) * A(12) + F(4,4) * A(14) + F(4,5) * A(15)
      T51 = F(5,1) * A( 1) + F(5,2) * A( 2) +
     +      F(5,3) * A( 3) + F(5,4) * A( 4) + F(5,5) * A( 5)
      T52 = F(5,1) * A( 2) + F(5,2) * A( 6) +
     +      F(5,3) * A( 7) + F(5,4) * A( 8) + F(5,5) * A( 9)
      T53 = F(5,1) * A( 3) + F(5,2) * A( 7) +
     +      F(5,3) * A(10) + F(5,4) * A(11) + F(5,5) * A(12)
      T54 = F(5,1) * A( 4) + F(5,2) * A( 8) +
     +      F(5,3) * A(11) + F(5,4) * A(13) + F(5,5) * A(14)
      T55 = F(5,1) * A( 5) + F(5,2) * A( 9) +
     +      F(5,3) * A(12) + F(5,4) * A(14) + F(5,5) * A(15)

      B( 1) = T11 * F(1,1) + T12 * F(1,2) + T13 * F(1,3) +
     +        T14 * F(1,4) + T15 * F(1,5)
      B( 2) = T11 * F(2,1) + T12 * F(2,2) + T13 * F(2,3) +
     +        T14 * F(2,4) + T15 * F(2,5)
      B( 3) = T11 * F(3,1) + T12 * F(3,2) + T13 * F(3,3) +
     +        T14 * F(3,4) + T15 * F(3,5)
      B( 4) = T11 * F(4,1) + T12 * F(4,2) + T13 * F(4,3) +
     +        T14 * F(4,4) + T15 * F(4,5)
      B( 5) = T11 * F(5,1) + T12 * F(5,2) + T13 * F(5,3) +
     +        T14 * F(5,4) + T15 * F(5,5)

      B( 6) = T21 * F(2,1) + T22 * F(2,2) + T23 * F(2,3) +
     +        T24 * F(2,4) + T25 * F(2,5)
      B( 7) = T21 * F(3,1) + T22 * F(3,2) + T23 * F(3,3) +
     +        T24 * F(3,4) + T25 * F(3,5)
      B( 8) = T21 * F(4,1) + T22 * F(4,2) + T23 * F(4,3) +
     +        T24 * F(4,4) + T25 * F(4,5)
      B( 9) = T21 * F(5,1) + T22 * F(5,2) + T23 * F(5,3) +
     +        T24 * F(5,4) + T25 * F(5,5)

      B(10) = T31 * F(3,1) + T32 * F(3,2) + T33 * F(3,3) +
     +        T34 * F(3,4) + T35 * F(3,5)
      B(11) = T31 * F(4,1) + T32 * F(4,2) + T33 * F(4,3) +
     +        T34 * F(4,4) + T35 * F(4,5)
      B(12) = T31 * F(5,1) + T32 * F(5,2) + T33 * F(5,3) +
     +        T34 * F(5,4) + T35 * F(5,5)

      B(13) = T41 * F(4,1) + T42 * F(4,2) + T43 * F(4,3) +
     +        T44 * F(4,4) + T45 * F(4,5)
      B(14) = T41 * F(5,1) + T42 * F(5,2) + T43 * F(5,3) +
     +        T44 * F(5,4) + T45 * F(5,5)

      B(15) = T51 * F(5,1) + T52 * F(5,2) + T53 * F(5,3) +
     +        T54 * F(5,4) + T55 * F(5,5)

      END
      SUBROUTINE KMCIVI(ID,C,A,B)
************************************************************************
***                                                                  ***
***   Subroutine KMCIVI  B := C * A                                  ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION         C(*), A(*), B(*)

      IF (ID .EQ. 1) THEN
         B(1) = C(1) * A(1)
      ELSEIF (ID .EQ. 2) THEN
         B(1) = C(1) * A(1) + C(2) * A(2)
         B(2) = C(2) * A(1) + C(3) * A(2)
      ELSEIF (ID .EQ. 3) THEN
         B(1) = C(1) * A(1) + C(2) * A(2) + C(3) * A(3)
         B(2) = C(2) * A(1) + C(4) * A(2) + C(5) * A(3)
         B(3) = C(3) * A(1) + C(5) * A(2) + C(6) * A(3)
      ELSEIF (ID .EQ. 4) THEN
         B(1) = C( 1) * A(1) + C( 2) * A(2) + C( 3) * A(3) +
     +          C( 4) * A(4)
         B(2) = C( 2) * A(1) + C( 5) * A(2) + C( 6) * A(3) +
     +          C( 7) * A(4)
         B(3) = C( 3) * A(1) + C( 6) * A(2) + C( 8) * A(3) +
     +          C( 9) * A(4)
         B(4) = C( 4) * A(1) + C( 7) * A(2) + C( 9) * A(3) +
     +          C( 10)* A(4)
      ELSEIF (ID .EQ. 5) THEN
         B(1) = C( 1) * A(1) + C( 2) * A(2) + C( 3) * A(3) +
     +          C( 4) * A(4) + C( 5) * A(5)
         B(2) = C( 2) * A(1) + C( 6) * A(2) + C( 7) * A(3) +
     +          C( 8) * A(4) + C( 9) * A(5)
         B(3) = C( 3) * A(1) + C( 7) * A(2) + C(10) * A(3) +
     +          C(11) * A(4) + C(12) * A(5)
         B(4) = C( 4) * A(1) + C( 8) * A(2) + C(11) * A(3) +
     +          C(13) * A(4) + C(14) * A(5)
         B(5) = C( 5) * A(1) + C( 9) * A(2) + C(12) * A(3) +
     +          C(14) * A(4) + C(15) * A(5)
      ELSE
         WRITE(*,'('' KMCIVI - ID>5 not supported'')')
      ENDIF

      END
      SUBROUTINE KMVCVI(ID,A,C,B)
************************************************************************
***                                                                  ***
***   Subroutine KMVCVI  B := A * C * A                              ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION         C(*), A(*)

      IF (ID .EQ. 1) THEN
         B = A(1) * C(1) * A(1)
      ELSEIF (ID .EQ. 2) THEN
         B = A(1) * ( C(1) * A(1) + C(2) * A(2) ) +
     +       A(2) * ( C(2) * A(1) + C(3) * A(2) )
      ENDIF

      END
      SUBROUTINE KMHTV5(ID,HT,A,B)
************************************************************************
***                                                                  ***
***   Subroutine KMHTV5  B := T(HT) * A                              ***
***                                                                  ***
***   Input:                                                         ***
***     ID .... dimension of measurement vector                      ***
***     HT .... transposed projection matrix                         ***
***     A  .... parameter vector                                     ***
***                                                                  ***
***   Output:                                                        ***
***     B  .... vector of dimenson ID                                ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION         HT(5,*), A(5), B(5)

      DO 1000 I = 1, ID
         B(I) = HT(1,I) * A(1) + HT(2,I) * A(2) + HT(3,I) * A(3) +
     +          HT(4,I) * A(4) + HT(5,I) * A(5)
 1000 CONTINUE

      END
      SUBROUTINE KMHTC5(ID,HT,A,B)
************************************************************************
***                                                                  ***
***   Subroutine KMHTC5  B := HT * A * T( HT )                       ***
***                                                                  ***
***   Input:                                                         ***
***     ID .... dimension of measurement vector                      ***
***     HT .... transposed projection matrix                         ***
***     A  .... cov. matrix (15)                                     ***
***                                                                  ***
***   Output:                                                        ***
***     B  .... cov. matrix (ID * (ID + 1)/2)                        ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
      PARAMETER        (MAXMDV = 2)


      DIMENSION         HT(5,*), A(15), B(*) , T(5,MAXMDV)

      DO 1000 I = 1, ID
         T(1,I) = HT(1,I) * A( 1) + HT(2,I) * A( 2) + HT(3,I) * A( 3) +
     +            HT(4,I) * A( 4) + HT(5,I) * A( 5)
         T(2,I) = HT(1,I) * A( 2) + HT(2,I) * A( 6) + HT(3,I) * A( 7) +
     +            HT(4,I) * A( 8) + HT(5,I) * A( 9)
         T(3,I) = HT(1,I) * A( 3) + HT(2,I) * A( 7) + HT(3,I) * A(10) +
     +            HT(4,I) * A(11) + HT(5,I) * A(12)
         T(4,I) = HT(1,I) * A( 4) + HT(2,I) * A( 8) + HT(3,I) * A(11) +
     +            HT(4,I) * A(13) + HT(5,I) * A(14)
         T(5,I) = HT(1,I) * A( 5) + HT(2,I) * A( 9) + HT(3,I) * A(12) +
     +            HT(4,I) * A(14) + HT(5,I) * A(15)
 1000 CONTINUE

      K = 1
      DO 2010 I = 1, ID
         DO 2000 J = I, ID
            B(K) = HT(1,I) * T(1,J) + HT(2,I) * T(2,J) +
     +             HT(3,I) * T(3,J) + HT(4,I) * T(4,J) +
     +             HT(5,I) * T(5,J)
            K = K + 1
 2000    CONTINUE
 2010 CONTINUE

      END
      SUBROUTINE KMHTCI(ID,HT,A,B)
************************************************************************
***                                                                  ***
***   Subroutine KMHTCI  B := HT * A * T( HT )                       ***
***                                                                  ***
***   Input:                                                         ***
***     ID .... dimension of measurement vector                      ***
***     HT .... transposed projection matrix                         ***
***     A  .... cov. matrix (ID * (ID +1) / 2)                       ***
***                                                                  ***
***   Output:                                                        ***
***     B  .... cov. matrix (15)                                     ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
      PARAMETER        (MAXMDV = 2)


      DIMENSION         HT(5,*), A(*), B(15), T(MAXMDV,5)

      IF (ID .EQ. 1) THEN
         DO 1000 I = 1, 5
            T(1,I) = HT(I,1) * A(1)
 1000    CONTINUE
         K = 1
         DO 1020 I = 1, 5
            DO 1010 J = I, 5
               B(K) = HT(I,1) * T(1,J)
               K = K + 1
 1010       CONTINUE
 1020    CONTINUE
      ELSEIF (ID .EQ. 2) THEN
         DO 2000 I = 1, 5
            T(1,I) = HT(I,1) * A(1) + HT(I,2) * A(2)
            T(2,I) = HT(I,1) * A(2) + HT(I,2) * A(3)
 2000    CONTINUE
         K = 1
         DO 2020 I = 1, 5
            DO 2010 J = I, 5
               B(K) = HT(I,1) * T(1,J)  + HT(I,2) * T(2,J)
               K = K + 1
 2010       CONTINUE
 2020    CONTINUE
      ENDIF

      END
      SUBROUTINE KMHTVI(ID,HT,A,B)
************************************************************************
***                                                                  ***
***   Subroutine KMHTVI  B := T(HT) * A   (dim n)                    ***
***                                                                  ***
***   Input:                                                         ***
***     ID .... dimension of measurement vector                      ***
***     HT .... transposed projection matrix                         ***
***     A  .... cov. matrix (ID * (ID + 1) / 2)                      ***
***                                                                  ***
***   Output:                                                        ***
***     B  .... cov. matrix (15)                                     ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION         HT(5,*), A(*), B(5)

      DO 1000 I = 1, 5
         B(I) = 0.
 1000 CONTINUE

      DO 1020 I= 1, ID
         DO 1010 J = 1, 5
            B(J) = B(J) + HT(J,I) * A(I)
 1010    CONTINUE
 1020 CONTINUE

      END
      SUBROUTINE KMSMOO(C1,F,C2,A)
************************************************************************
***                                                                  ***
***   Subroutine KMSMOO  A := C1 * T(F) * C2                         ***
***                                                                  ***
***   Input:                                                         ***
***     C1 .... cov. matrix                                          ***
***     HT .... derivative of track model                            ***
***     C2 .... cov. matrix                                          ***
***                                                                  ***
***   Output:                                                        ***
***     A  .... matrix                                               ***
***                                                                  ***
***                                                   Dietrich Liko  ***
*** Modified to 5x5 bt A.C. Saulys                                   ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION         C1(15), F(5,5), C2(15), A(5,5)

      T11 = C1( 1) * F(1,1) + C1( 2) * F(1,2) +
     +      C1( 3) * F(1,3) + C1( 4) * F(1,4) + C1( 5) * F(1,5)
      T12 = C1( 1) * F(2,1) + C1( 2) * F(2,2) +
     +      C1( 3) * F(2,3) + C1( 4) * F(2,4) + C1( 5) * F(2,5)
      T13 = C1( 1) * F(3,1) + C1( 2) * F(3,2) +
     +      C1( 3) * F(3,3) + C1( 4) * F(3,4) + C1( 5) * F(3,5)
      T14 = C1( 1) * F(4,1) + C1( 2) * F(4,2) +
     +      C1( 3) * F(4,3) + C1( 4) * F(4,4) + C1( 5) * F(4,5)
      T15 = C1( 1) * F(5,1) + C1( 2) * F(5,2) +
     +      C1( 3) * F(5,3) + C1( 4) * F(5,4) + C1( 5) * F(5,5)

      T21 = C1( 2) * F(1,1) + C1( 6) * F(1,2) +
     +      C1( 7) * F(1,3) + C1( 8) * F(1,4) + C1( 9) * F(1,5)
      T22 = C1( 2) * F(2,1) + C1( 6) * F(2,2) +
     +      C1( 7) * F(2,3) + C1( 8) * F(2,4) + C1( 9) * F(2,5)
      T23 = C1( 2) * F(3,1) + C1( 6) * F(3,2) +
     +      C1( 7) * F(3,3) + C1( 8) * F(3,4) + C1( 9) * F(3,5)
      T24 = C1( 2) * F(4,1) + C1( 6) * F(4,2) +
     +      C1( 7) * F(4,3) + C1( 8) * F(4,4) + C1( 9) * F(4,5)
      T25 = C1( 2) * F(5,1) + C1( 6) * F(5,2) +
     +      C1( 7) * F(5,3) + C1( 8) * F(5,4) + C1( 9) * F(5,5)

      T31 = C1( 3) * F(1,1) + C1( 7) * F(1,2) +
     +      C1(10) * F(1,3) + C1(11) * F(1,4) + C1(12) * F(1,5)
      T32 = C1( 3) * F(2,1) + C1( 7) * F(2,2) +
     +      C1(10) * F(2,3) + C1(11) * F(2,4) + C1(12) * F(2,5)
      T33 = C1( 3) * F(3,1) + C1( 7) * F(3,2) +
     +      C1(10) * F(3,3) + C1(11) * F(3,4) + C1(12) * F(3,5)
      T34 = C1( 3) * F(4,1) + C1( 7) * F(4,2) +
     +      C1(10) * F(4,3) + C1(11) * F(4,4) + C1(12) * F(4,5)
      T35 = C1( 3) * F(5,1) + C1( 7) * F(5,2) +
     +      C1(10) * F(5,3) + C1(11) * F(5,4) + C1(12) * F(5,5)

      T41 = C1( 4) * F(1,1) + C1( 8) * F(1,2) +
     +      C1(11) * F(1,3) + C1(13) * F(1,4) + C1(14) * F(1,5)
      T42 = C1( 4) * F(2,1) + C1( 8) * F(2,2) +
     +      C1(11) * F(2,3) + C1(13) * F(2,4) + C1(14) * F(2,5)
      T43 = C1( 4) * F(3,1) + C1( 8) * F(3,2) +
     +      C1(11) * F(3,3) + C1(13) * F(3,4) + C1(14) * F(3,5)
      T44 = C1( 4) * F(4,1) + C1( 8) * F(4,2) +
     +      C1(11) * F(4,3) + C1(13) * F(4,4) + C1(14) * F(4,5)
      T45 = C1( 4) * F(5,1) + C1( 8) * F(5,2) +
     +      C1(11) * F(5,3) + C1(13) * F(5,4) + C1(14) * F(5,5)

      T51 = C1( 5) * F(1,1) + C1( 9) * F(1,2) +
     +      C1(12) * F(1,3) + C1(14) * F(1,4) + C1(15) * F(1,5)
      T52 = C1( 5) * F(2,1) + C1( 9) * F(2,2) +
     +      C1(12) * F(2,3) + C1(14) * F(2,4) + C1(15) * F(2,5)
      T53 = C1( 5) * F(3,1) + C1( 9) * F(3,2) +
     +      C1(12) * F(3,3) + C1(14) * F(3,4) + C1(15) * F(3,5)
      T54 = C1( 5) * F(4,1) + C1( 9) * F(4,2) +
     +      C1(12) * F(4,3) + C1(14) * F(4,4) + C1(15) * F(4,5)
      T55 = C1( 5) * F(5,1) + C1( 9) * F(5,2) +
     +      C1(12) * F(5,3) + C1(14) * F(5,4) + C1(15) * F(5,5)

      A(1,1) = T11 * C2( 1) + T12 * C2( 2) +
     +         T13 * C2( 3) + T14 * C2( 4) + T15 * C2( 5)
      A(1,2) = T11 * C2( 2) + T12 * C2( 6) +
     +         T13 * C2( 7) + T14 * C2( 8) + T15 * C2( 9)
      A(1,3) = T11 * C2( 3) + T12 * C2( 7) +
     +         T13 * C2(10) + T14 * C2(11) + T15 * C2(12)
      A(1,4) = T11 * C2( 4) + T12 * C2( 8) +
     +         T13 * C2(11) + T14 * C2(13) + T15 * C2(14)
      A(1,5) = T11 * C2( 5) + T12 * C2( 9) +
     +         T13 * C2(12) + T14 * C2(14) + T15 * C2(15)
      A(2,1) = T21 * C2( 1) + T22 * C2( 2) +
     +         T23 * C2( 3) + T24 * C2( 4) + T25 * C2( 5)
      A(2,2) = T21 * C2( 2) + T22 * C2( 6) +
     +         T23 * C2( 7) + T24 * C2( 8) + T25 * C2( 9)
      A(2,3) = T21 * C2( 3) + T22 * C2( 7) +
     +         T23 * C2(10) + T24 * C2(11) + T25 * C2(12)
      A(2,4) = T21 * C2( 4) + T22 * C2( 8) +
     +         T23 * C2(11) + T24 * C2(13) + T25 * C2(14)
      A(2,5) = T21 * C2( 5) + T22 * C2( 9) +
     +         T23 * C2(12) + T24 * C2(14) + T25 * C2(15)
      A(3,1) = T31 * C2( 1) + T32 * C2( 2) +
     +         T33 * C2( 3) + T34 * C2( 4) + T35 * C2( 5)
      A(3,2) = T31 * C2( 2) + T32 * C2( 6) +
     +         T33 * C2( 7) + T34 * C2( 8) + T35 * C2( 9)
      A(3,3) = T31 * C2( 3) + T32 * C2( 7) +
     +         T33 * C2(10) + T34 * C2(11) + T35 * C2(12)
      A(3,4) = T31 * C2( 4) + T32 * C2( 8) +
     +         T33 * C2(11) + T34 * C2(13) + T35 * C2(14)
      A(3,5) = T31 * C2( 5) + T32 * C2( 9) +
     +         T33 * C2(12) + T34 * C2(14) + T35 * C2(15)
      A(4,1) = T41 * C2( 1) + T42 * C2( 2) +
     +         T43 * C2( 3) + T44 * C2( 4) + T45 * C2( 5)
      A(4,2) = T41 * C2( 2) + T42 * C2( 6) +
     +         T43 * C2( 7) + T44 * C2( 8) + T45 * C2( 9)
      A(4,3) = T41 * C2( 3) + T42 * C2( 7) +
     +         T43 * C2(10) + T44 * C2(11) + T45 * C2(12)
      A(4,4) = T41 * C2( 4) + T42 * C2( 8) +
     +         T43 * C2(11) + T44 * C2(13) + T45 * C2(14)
      A(4,5) = T41 * C2( 5) + T42 * C2( 9) +
     +         T43 * C2(12) + T44 * C2(14) + T45 * C2(15)
      A(5,1) = T51 * C2( 1) + T52 * C2( 2) +
     +         T53 * C2( 3) + T54 * C2( 4) + T55 * C2( 5)
      A(5,2) = T51 * C2( 2) + T52 * C2( 6) +
     +         T53 * C2( 7) + T54 * C2( 8) + T55 * C2( 9)
      A(5,3) = T51 * C2( 3) + T52 * C2( 7) +
     +         T53 * C2(10) + T54 * C2(11) + T55 * C2(12)
      A(5,4) = T51 * C2( 4) + T52 * C2( 8) +
     +         T53 * C2(11) + T54 * C2(13) + T55 * C2(14)
      A(5,5) = T51 * C2( 5) + T52 * C2( 9) +
     +         T53 * C2(12) + T54 * C2(14) + T55 * C2(15)

      END
      SUBROUTINE KMA5V5(A,B,C)
************************************************************************
***                                                                  ***
***   Subroutine KMA5V5  C := A * B                                  ***
***                                                                  ***
***   Input:                                                         ***
***     A  .... matrix                                               ***
***     B  .... vector                                               ***
***                                                                  ***
***   Output:                                                        ***
***     C  .... vector                                               ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION          A(5,5), B(5), C(5)

      DO 1010 I = 1, 5
         C(I) = 0.
         DO 1000 J = 1, 5
            C(I) = C(I) + A(I,J) * B(J)
 1000    CONTINUE
 1010 CONTINUE

      END
      SUBROUTINE KMA5C5(A,B,C)
************************************************************************
***                                                                  ***
***   Subroutine KMA5C5  C := A * B * T(A)                           ***
***                                                                  ***
***   Input:                                                         ***
***     A  .... matrix                                               ***
***     B  .... cov. matrix                                          ***
***                                                                  ***
***   Output:                                                        ***
***     C  .... vector                                               ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION          A(5,5), B(15), C(15)

      S11 = A(1,1) * B( 1) + A(1,2) * B( 2) + A(1,3) * B( 3) +
     +      A(1,4) * B( 4) + A(1,5) * B( 5)
      S12 = A(1,1) * B( 2) + A(1,2) * B( 6) + A(1,3) * B( 7) +
     +      A(1,4) * B( 8) + A(1,5) * B( 9)
      S13 = A(1,1) * B( 3) + A(1,2) * B( 7) + A(1,3) * B(10) +
     +      A(1,4) * B(11) + A(1,5) * B(12)
      S14 = A(1,1) * B( 4) + A(1,2) * B( 8) + A(1,3) * B(11) +
     +      A(1,4) * B(13) + A(1,5) * B(14)
      S15 = A(1,1) * B( 5) + A(1,2) * B( 9) + A(1,3) * B(12) +
     +      A(1,4) * B(14) + A(1,5) * B(15)
      S21 = A(2,1) * B( 1) + A(2,2) * B( 2) + A(2,3) * B( 3) +
     +      A(2,4) * B( 4) + A(2,5) * B( 5)
      S22 = A(2,1) * B( 2) + A(2,2) * B( 6) + A(2,3) * B( 7) +
     +      A(2,4) * B( 8) + A(2,5) * B( 9)
      S23 = A(2,1) * B( 3) + A(2,2) * B( 7) + A(2,3) * B(10) +
     +      A(2,4) * B(11) + A(2,5) * B(12)
      S24 = A(2,1) * B( 4) + A(2,2) * B( 8) + A(2,3) * B(11) +
     +      A(2,4) * B(13) + A(2,5) * B(14)
      S25 = A(2,1) * B( 5) + A(2,2) * B( 9) + A(2,3) * B(12) +
     +      A(2,4) * B(14) + A(2,5) * B(15)
      S31 = A(3,1) * B( 1) + A(3,2) * B( 2) + A(3,3) * B( 3) +
     +      A(3,4) * B( 4) + A(3,5) * B( 5)
      S32 = A(3,1) * B( 2) + A(3,2) * B( 6) + A(3,3) * B( 7) +
     +      A(3,4) * B( 8) + A(3,5) * B( 9)
      S33 = A(3,1) * B( 3) + A(3,2) * B( 7) + A(3,3) * B(10) +
     +      A(3,4) * B(11) + A(3,5) * B(12)
      S34 = A(3,1) * B( 4) + A(3,2) * B( 8) + A(3,3) * B(11) +
     +      A(3,4) * B(13) + A(3,5) * B(14)
      S35 = A(3,1) * B( 5) + A(3,2) * B( 9) + A(3,3) * B(12) +
     +      A(3,4) * B(14) + A(3,5) * B(15)
      S41 = A(4,1) * B( 1) + A(4,2) * B( 2) + A(4,3) * B( 3) +
     +      A(4,4) * B( 4) + A(4,5) * B( 5)
      S42 = A(4,1) * B( 2) + A(4,2) * B( 6) + A(4,3) * B( 7) +
     +      A(4,4) * B( 8) + A(4,5) * B( 9)
      S43 = A(4,1) * B( 3) + A(4,2) * B( 7) + A(4,3) * B(10) +
     +      A(4,4) * B(11) + A(4,5) * B(12)
      S44 = A(4,1) * B( 4) + A(4,2) * B( 8) + A(4,3) * B(11) +
     +      A(4,4) * B(13) + A(4,5) * B(14)
      S45 = A(4,1) * B( 5) + A(4,2) * B( 9) + A(4,3) * B(12) +
     +      A(4,4) * B(14) + A(4,5) * B(15)
      S51 = A(5,1) * B( 1) + A(5,2) * B( 2) + A(5,3) * B( 3) +
     +      A(5,4) * B( 4) + A(5,5) * B( 5)
      S52 = A(5,1) * B( 2) + A(5,2) * B( 6) + A(5,3) * B( 7) +
     +      A(5,4) * B( 8) + A(5,5) * B( 9)
      S53 = A(5,1) * B( 3) + A(5,2) * B( 7) + A(5,3) * B(10) +
     +      A(5,4) * B(11) + A(5,5) * B(12)
      S54 = A(5,1) * B( 4) + A(5,2) * B( 8) + A(5,3) * B(11) +
     +      A(5,4) * B(13) + A(5,5) * B(14)
      S55 = A(5,1) * B( 5) + A(5,2) * B( 9) + A(5,3) * B(12) +
     +      A(5,4) * B(14) + A(5,5) * B(15)


      C( 1) =  S11 * A(1,1) + S12 * A(1,2) + S13 * A(1,3) +
     +         S14 * A(1,4) + S15 * A(1,5)
      C( 2) =  S11 * A(2,1) + S12 * A(2,2) + S13 * A(2,3) +
     +         S14 * A(2,4) + S15 * A(2,5)
      C( 3) =  S11 * A(3,1) + S12 * A(3,2) + S13 * A(3,3) +
     +         S14 * A(3,4) + S15 * A(3,5)
      C( 4) =  S11 * A(4,1) + S12 * A(4,2) + S13 * A(4,3) +
     +         S14 * A(4,4) + S15 * A(4,5)
      C( 5) =  S11 * A(5,1) + S12 * A(5,2) + S13 * A(5,3) +
     +         S14 * A(5,4) + S15 * A(5,5)
      C( 6) =  S21 * A(2,1) + S22 * A(2,2) + S23 * A(2,3) +
     +         S24 * A(2,4) + S25 * A(2,5)
      C( 7) =  S21 * A(3,1) + S22 * A(3,2) + S23 * A(3,3) +
     +         S24 * A(3,4) + S25 * A(3,5)
      C( 8) =  S21 * A(4,1) + S22 * A(4,2) + S23 * A(4,3) +
     +         S24 * A(4,4) + S25 * A(4,5)
      C( 9) =  S21 * A(5,1) + S22 * A(5,2) + S23 * A(5,3) +
     +         S24 * A(5,4) + S25 * A(5,5)
      C(10) =  S31 * A(3,1) + S32 * A(3,2) + S33 * A(3,3) +
     +         S34 * A(3,4) + S35 * A(3,5)
      C(11) =  S31 * A(4,1) + S32 * A(4,2) + S33 * A(4,3) +
     +         S34 * A(4,4) + S35 * A(4,5)
      C(12) =  S31 * A(5,1) + S32 * A(5,2) + S33 * A(5,3) +
     +         S34 * A(5,4) + S35 * A(5,5)
      C(13) =  S41 * A(4,1) + S42 * A(4,2) + S43 * A(4,3) +
     +         S44 * A(4,4) + S45 * A(4,5)
      C(14) =  S41 * A(5,1) + S42 * A(5,2) + S43 * A(5,3) +
     +         S44 * A(5,4) + S45 * A(5,5)
      C(15) =  S51 * A(5,1) + S52 * A(5,2) + S53 * A(5,3) +
     +         S54 * A(5,4) + S55 * A(5,5)

      END
      SUBROUTINE KMC5C5(A,B,C)
************************************************************************
***                                                                  ***
***   Subroutine KMC5C5  C := A * B                                  ***
***                                                                  ***
***   Input:                                                         ***
***     A  .... cov. matrix                                          ***
***     B  .... cov. matrix                                          ***
***                                                                  ***
***   Output:                                                        ***
***     C  .... matrix                                               ***
***                                                                  ***
***                                                   Dietrich Liko  ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION        A(15),B(15),C(5,5)

      C(1,1) = A( 1) * B( 1) + A( 2) * B( 2) + A( 3) * B( 3) + A( 4) *
     +         B( 4) + A( 5) * B( 5)
      C(1,2) = A( 1) * B( 2) + A( 2) * B( 6) + A( 3) * B( 7) + A( 4) *
     +         B( 8) + A( 5) * B( 9)
      C(1,3) = A( 1) * B( 3) + A( 2) * B( 7) + A( 3) * B(10) + A( 4) *
     +         B(11) + A( 5) * B(12)
      C(1,4) = A( 1) * B( 4) + A( 2) * B( 8) + A( 3) * B(11) + A( 4) *
     +         B(13) + A( 5) * B(14)
      C(1,5) = A( 1) * B( 5) + A( 2) * B( 9) + A( 3) * B(12) + A( 4) *
     +         B(14) + A( 5) * B(15)
      C(2,1) = A( 2) * B( 1) + A( 6) * B( 2) + A( 7) * B( 3) + A( 8) *
     +         B( 4) + A( 9) * B( 5)
      C(2,2) = A( 2) * B( 2) + A( 6) * B( 6) + A( 7) * B( 7) + A( 8) *
     +         B( 8) + A( 9) * B( 9)
      C(2,3) = A( 2) * B( 3) + A( 6) * B( 7) + A( 7) * B(10) + A( 8) *
     +         B(11) + A( 9) * B(12)
      C(2,4) = A( 2) * B( 4) + A( 6) * B( 8) + A( 7) * B(11) + A( 8) *
     +         B(13) + A( 9) * B(14)
      C(2,5) = A( 2) * B( 5) + A( 6) * B( 9) + A( 7) * B(12) + A( 8) *
     +         B(14) + A( 9) * B(15)
      C(3,1) = A( 3) * B( 1) + A( 7) * B( 2) + A(10) * B( 3) + A(11) *
     +         B( 4) + A(12) * B( 5)
      C(3,2) = A( 3) * B( 2) + A( 7) * B( 6) + A(10) * B( 7) + A(11) *
     +         B( 8) + A(12) * B( 9)
      C(3,3) = A( 3) * B( 3) + A( 7) * B( 7) + A(10) * B(10) + A(11) *
     +         B(11) + A(12) * B(12)
      C(3,4) = A( 3) * B( 4) + A( 7) * B( 8) + A(10) * B(11) + A(11) *
     +         B(13) + A(12) * B(14)
      C(3,5) = A( 3) * B( 5) + A( 7) * B( 9) + A(10) * B(12) + A(11) *
     +         B(14) + A(12) * B(15)
      C(4,1) = A( 4) * B( 1) + A( 8) * B( 2) + A(11) * B( 3) + A(13) *
     +         B( 4) + A(14) * B( 5)
      C(4,2) = A( 4) * B( 2) + A( 8) * B( 6) + A(11) * B( 7) + A(13) *
     +         B( 8) + A(14) * B( 9)
      C(4,3) = A( 4) * B( 3) + A( 8) * B( 7) + A(11) * B(10) + A(13) *
     +         B(11) + A(14) * B(12)
      C(4,4) = A( 4) * B( 4) + A( 8) * B( 8) + A(11) * B(11) + A(13) *
     +         B(13) + A(14) * B(14)
      C(4,5) = A( 4) * B( 5) + A( 8) * B( 9) + A(11) * B(12) + A(13) *
     +         B(14) + A(14) * B(15)
      C(5,1) = A( 5) * B( 1) + A( 9) * B( 2) + A(12) * B( 3) + A(14) *
     +         B( 4) + A(15) * B( 5)
      C(5,2) = A( 5) * B( 2) + A( 9) * B( 6) + A(12) * B( 7) + A(14) *
     +         B( 8) + A(15) * B( 9)
      C(5,3) = A( 5) * B( 3) + A( 9) * B( 7) + A(12) * B(10) + A(14) *
     +         B(11) + A(15) * B(12)
      C(5,4) = A( 5) * B( 4) + A( 9) * B( 8) + A(12) * B(11) + A(14) *
     +         B(13) + A(15) * B(14)
      C(5,5) = A( 5) * B( 5) + A( 9) * B( 9) + A(12) * B(12) + A(14) *
     +         B(14) + A(15) * B(15)

      END
      SUBROUTINE KMFDV5(F,A,B)
************************************************************************
***                                                                  ***
***   Subroutine KMFDV5  B := F * A                                  ***
***                                                                  ***
***   Input:                                                         ***
***     F  .... derivative matrix                                    ***
***     A  .... parameter vector                                     ***
***                                                                  ***
***   Output:                                                        ***
***     B  .... parameter vector                                     ***
***                                                                  ***
***                                                   Dietrich Liko  ***
*** Changed to 5x5 by A. C. Saulys                                    ***
************************************************************************

      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)

      DIMENSION          F(5,5), A(5), B(5)

      DO 1100 I = 1, 5
         B(I) = 0.
         DO 1000 J = 1, 5
            B(I) = B(I) + F(I,J) * A(J)
 1000    CONTINUE
 1100 CONTINUE

CCC      B(5) = A(5)

      END
