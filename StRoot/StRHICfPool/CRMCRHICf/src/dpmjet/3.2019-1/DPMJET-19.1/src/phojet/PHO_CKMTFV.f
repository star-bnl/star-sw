
      REAL FUNCTION PHO_CKMTFV(X,Fvl)
      IMPLICIT NONE
      REAL aloga , alpha , axi , axj , axk , beta , bxi , bxj , bxk , 
     &     det , fi , fj , fk , Fvl , X , xgrid
      INTEGER i , j , k , nx
C**********************************************************************
C
C     LOGARITHMIC INTERPOLATOR - WATCH OUT FOR NEGATIVE
C     FUNCTIONS AND/OR X VALUES OUTSIDE THE RANGE 0 TO 1.
C     NOTE: DIMENSION OF FVL IS OVERWRITTEN BY VALUE USED
C     IN MAIN ROUTINE.
C
C**********************************************************************
      SAVE 
 
      DIMENSION Fvl(25) , xgrid(25)
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      DATA nx , xgrid/25 , .001 , .002 , .004 , .008 , .016 , .032 , 
     &     .064 , .1 , .15 , .2 , .25 , .3 , .35 , .4 , .45 , .5 , .55 , 
     &     .6 , .65 , .7 , .75 , .8 , .85 , .9 , .95/
 
      PHO_CKMTFV = 0.
      DO i = 1 , nx
         IF ( X.LT.xgrid(i) ) GOTO 100
      END DO
 100  i = i - 1
      IF ( i.EQ.0 ) THEN
         i = i + 1
      ELSE IF ( i.GT.23 ) THEN
         i = 23
      END IF
      j = i + 1
      k = j + 1
      axi = LOG(xgrid(i))
      bxi = LOG(1.-xgrid(i))
      axj = LOG(xgrid(j))
      bxj = LOG(1.-xgrid(j))
      axk = LOG(xgrid(k))
      bxk = LOG(1.-xgrid(k))
      fi = LOG(ABS(Fvl(i))+1.E-15)
      fj = LOG(ABS(Fvl(j))+1.E-16)
      fk = LOG(ABS(Fvl(k))+1.E-17)
      det = axi*(bxj-bxk) + axj*(bxk-bxi) + axk*(bxi-bxj)
      aloga = (fi*(axj*bxk-axk*bxj)+fj*(axk*bxi-axi*bxk)
     &        +fk*(axi*bxj-axj*bxi))/det
      alpha = (fi*(bxj-bxk)+fj*(bxk-bxi)+fk*(bxi-bxj))/det
      beta = (fi*(axk-axj)+fj*(axi-axk)+fk*(axj-axi))/det
      IF ( ABS(alpha).GT.99. .OR. ABS(beta).GT.99. .OR. ABS(aloga)
     &     .GT.99. ) RETURN
C      IF(ALPHA.GT.50..OR.BETA.GT.50.) THEN
C         WRITE(LO,2001) X,FVL
C 2001    FORMAT(8E12.4)
C         WRITE(LO,2001) ALPHA,BETA,ALOGA,DET
C      ENDIF
      PHO_CKMTFV = EXP(aloga)*X**alpha*(1.-X)**beta
 
      END FUNCTION
