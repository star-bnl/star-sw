      FUNCTION XSPENZ(X)
*     ---------------
* Hans Kuijf, 1988
* XSPENZ(X) calcs the complex spence-function, through mapping on
* the area where there is a quickly convergent series.
      REAL*8 PI
      COMPLEX*16 X, SPENC, XSPENZ
      PI=4D0*DATAN(1D0)
* Map the x on the unit circle.
* But so that x is not in the neighbourhood of (1,0)
* ABS(Z)=-CDLOG(1D0-X) is always smaller than 1.10
* But (1.10)^19/(19!)*bernoulli(19)=2.7D-15
      IF (CDABS(1D0-X).LT.1D-13) THEN
        XSPENZ=PI*PI/6D0
      ELSE IF (CDABS(1D0-X).LT.0.5D0) THEN
        XSPENZ=PI*PI/6D0-CDLOG(1D0-X)*CDLOG(X)-SPENC(1D0-X)
      ELSE IF (CDABS(X).GT.1D0) THEN
        XSPENZ=-PI*PI/6D0-0.5D0*CDLOG(-X)*CDLOG(-X)-SPENC(1D0/X)
      ELSE
        XSPENZ = SPENC(X)
      END IF
      END
 
      FUNCTION SPENC(X)
      COMPLEX*16 X,SUM,Z,Z2,SPENC
      Z=-CDLOG(1D0-X)
      Z2=Z*Z
* Horner's rule for the powers z^3 through z^19
      SUM=43867D0/798D0
      SUM=SUM*Z2/342D0-3617D0/510D0
      SUM=SUM*Z2/272D0+7D0/6D0
      SUM=SUM*Z2/210D0-691D0/2730D0
      SUM=SUM*Z2/156D0+5D0/66D0
      SUM=SUM*Z2/110D0-1D0/30D0
      SUM=SUM*Z2/ 72D0+1D0/42D0
      SUM=SUM*Z2/ 42D0-1D0/30D0
      SUM=SUM*Z2/ 20D0+1D0/6D0
* The first three terms of the power series
      SUM=Z2*Z*SUM/6D0-0.25D0*Z2+Z
      SPENC=SUM
      END
*
        FUNCTION CSPEN(Z)
C       FUNCTION XSPENZ(Z)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SPENCE-FUNKTION KOMPLEX, FREI NACH HOLLIK                     C
C---------------------------------------------------------------------C
C       20.07.83    UE 20.08.85                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       COMPLEX*16 XSPENZ,W,SUM,Z,U
        COMPLEX*16 CSPEN ,W,SUM,Z,U
        REAL*8 RZ,AZ,A1
        REAL*8 B(9)
      DATA B/
     1   0.1666666666666666666666666667D0,
     2  -0.0333333333333333333333333333D0,
     3   0.0238095238095238095238095238D0,
     4  -0.0333333333333333333333333333D0,
     5   0.0757575757575757575757575758D0,
     6  -0.2531135531135531135531135531D0,
     7   1.1666666666666666666666666667D0,
     8  -7.09215686274509804D0         ,
     9  54.97117794486215539D0         /
C     BEACHTE:                 B(N)=B2N
C     B(1)=1./6.
C     B(2)=-1./30.
C     B(3)=1./42.
C     B(4)=-1./30.
C     B(5)=5./66.
C     B(6)=-691./2730.
C     B(7)=7./6.
C     B(8)=-3617./510.
C     B(9)=43867./798.
C     B(10)=-174611./330.
C     B(11)=854513./138.
C     PI=3.1415926535897932384
C     PI*PI/6.=1.6449..., PI*PI/3=3.28986...
C
C---> GENAUIGKEIT
c      II = 9
      ii = 8
      RZ=DREAL(Z)
      AZ=CDABS(Z)
      A1=CDABS(1D0-Z)
      IF((DABS(RZ-1D0).LE.1D-15).AND.(dabs(DIMAG(Z)).le.1D-15))GOTO 40
      IF(RZ.GT.5D-1) GOTO 20
      IF(AZ.GT.1D0) GOTO 10
      W=-CDLOG(1D0-Z)
      SUM=W-0.25D0*W*W
      U=W
      DO 1 K=1,II
      U=U*W*W/DFLOAT(2*K*(2*K+1))
      SUM=SUM+U*B(K)
 1    CONTINUE
      CSPEN=SUM
      RETURN
10    W=-CDLOG(1D0-1D0/Z)
      SUM=W-0.25D0*W*W
      U=W
      DO 11 K=1,II
      U=U*W*W/DFLOAT(2*K*(2*K+1))
      SUM=SUM+U*B(K)
11    CONTINUE
      CSPEN=-SUM-1.64493406684822643D0-.5D0*CDLOG(-Z)**2
      RETURN
20    IF(A1.GT.1D0) GOTO 30
      W=-CDLOG(Z)
      SUM=W-0.25D0*W*W
      U=W
      DO 21 K=1,II
      U=U*W*W/DFLOAT(2*K*(2*K+1))
      SUM=SUM+U*B(K)
21    CONTINUE
      CSPEN=-SUM+1.64493406684822643D0-CDLOG(Z)*CDLOG(1D0-Z)
      RETURN
30    W=CDLOG(1D0-1D0/Z)
      SUM=W-0.25D0*W*W
      U=W
      DO 31 K=1,II
      U=U*W*W/DFLOAT(2*K*(2*K+1))
      SUM=SUM+U*B(K)
31    CONTINUE
      CSPEN=SUM+3.28986813369645287D0
     *               +.5D0*CDLOG(Z-1D0)**2-CDLOG(Z)*CDLOG(1D0-Z)
50    CONTINUE
      RETURN
40    CSPEN=DCMPLX(1.64493406684822643D0,0D0)
        RETURN
        END
