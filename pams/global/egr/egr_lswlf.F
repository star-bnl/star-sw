C ---------------------------------------------------------------------
C
C EGR_LSWLF - a least squares straight line fitting program
C
C DESCRIPTION:
C Performs weighted least squares fit to line (Y  =  A*X  +  B )
C using linear regression.
C
C INPUT ARGUMENTS:
C  X(N),Y(N),W(N) - Input values and weights
C  N              - Number of pairs of data points,  INTEGER
C  X              - Array of data points in X-axis,  MUST BE REAL
C  Y              - Array of data points in Y-axis,  MUST BE REAL
C  W              - Array of weights for data points,MUST BE REAL
C
C OUTPUT ARGUMENTS:
C  B              - Y intercept of best fitted straight line, REAL
C  SIGB           - Standard deviation of B,                  REAL
C  A              - Slope of fitted straight line,            REAL
C  SIGA           - Standard deviation of A,                  REAL
C  CHISQ          - Chi-square                                REAL
C  EGR_LSWLF          - = 0  return without error
C                   = -1 got some fitting problems
C
C AUTHOR:
C Jawluen Tang, Physics department, UT-Austin
C
C USAGE:
C IRET=EGR_LSWLF(X,Y,W,N,A,B,CHISQ,SIGA,SIGB)
C
C CREATION DATE:
C 10-AUGUST-1993
C Last modified,    Jawluen Tang, August 10,1993
C<---------------------------------------------------------------
      INTEGER*4 FUNCTION EGR_LSWLF(X,Y,W,N,A,B,CHISQ,SIGA,SIGB)
 
      IMPLICIT NONE
      DOUBLE PRECISION  SUM,SX,SY,SXX,SXY,SYY,DET
      REAL       a,b,varsq,siga,sigb,chi,chisq
      REAL  X(*),Y(*),W(*)
      INTEGER I,N
      INTEGER imsg
      DATA imsg /0/
      CHARACTER*132 errmsg
 
      CHI=99999999.0
C________________________________________________________________
C     N must be > 2
C________________________________________________________________
      if(n.le.2) then
         write(errmsg,'(A45)'),
     +        'EGR_LSWLF-W: Not enough points for line fit'
         call message(errmsg,1,imsg)
           EGR_LSWLF=-1
           return
       endif
 
C     initialization
 
        SUM=0.D0
        SX =0.D0
        SY =0.D0
        SXX=0.D0
        SXY=0.D0
        SYY=0.D0
C__________________________________________________________________
C     find sum , sumx ,sumy, sumxx, sumxy
C__________________________________________________________________
      do i=1,n
            SUM=SUM + W(I)
            SX =SX  + W(I)*X(I)
            SY =SY  + W(I)*Y(I)
            SXX=SXX + W(I)*X(I)*X(I)
            SXY=SXY + W(I)*X(I)*Y(I)
            SYY=SYY + W(I)*Y(I)*Y(I)
      end do
 
      DET= SUM*SXX -SX*SX
      IF(ABS(DET).LT.1.D-20) then
           EGR_LSWLF=-1
           GO TO 30
      endif
C
C     compute the best fitted parameters A,B
C
      A = (SUM*SXY -  SX*SY)/DET
      B = (SY*SXX  - SXY*SX)/DET
 
C     calculate chi-square
 
      CHI=0.0
      DO  I=1,N
            CHI=CHI+ W(I)*(Y(I)-A*X(I)-B)**2
      end do
 
 
C     calculate estimated variance
 
      varsq=chi/(float(n)-2.)
 
 
C     calculate covariance matrix
 
      siga=sqrt(varsq*sxx/det)
      sigb=sqrt(varsq*sum/det)
C        sigab=-sx/det
 
 30       CONTINUE
      CHISQ=CHI
      EGR_LSWLF = 0
      RETURN
      END
