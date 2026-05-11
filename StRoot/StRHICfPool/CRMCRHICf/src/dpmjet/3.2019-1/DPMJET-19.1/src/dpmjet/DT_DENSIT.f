
      DOUBLE PRECISION FUNCTION DT_DENSIT(Na,R,Ra)
 
      IMPLICIT NONE
      DOUBLE PRECISION fnorm , ONE , pdif , PI , R , r0 , r1 , Ra , 
     &                 TINY10 , TWO , TWOPI , ZERO
      INTEGER Na
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0)
      PARAMETER (TWOPI=6.283185307179586476925286766559D+00,
     &           PI=TWOPI/TWO)
 
      DIMENSION r0(18) , fnorm(18)
      DATA r0/ZERO , ZERO , ZERO , ZERO , 2.12D0 , 2.56D0 , 2.41D0 , 
     &     2.46D0 , 2.52D0 , 2.45D0 , 2.37D0 , 2.46D0 , 2.44D0 , 
     &     2.54D0 , 2.58D0 , 2.72D0 , 2.66D0 , 2.79D0/
      DATA fnorm/.1000D+01 , .1000D+01 , .1000D+01 , .1000D+01 , 
     &     .1000D+01 , .1000D+01 , .1000D+01 , .1000D+01 , .1000D+01 , 
     &     .1000D+01 , .1012D+01 , .1039D+01 , .1075D+01 , .1118D+01 , 
     &     .1164D+01 , .1214D+01 , .1265D+01 , .1318D+01/
      DATA pdif/0.545D0/
 
      DT_DENSIT = ZERO
C shell model
      IF ( Na.LE.4 ) THEN
         STOP 'DT_DENSIT-0'
      ELSE IF ( (Na.GT.4) .AND. (Na.LE.18) ) THEN
         r1 = r0(Na)/SQRT(2.5D0-4.0D0/DBLE(Na))
         DT_DENSIT = (ONE+(DBLE(Na)-4.0D0)/6.0D0*(R/r1)**2)
     &               *EXP(-(R/r1)**2)/fnorm(Na)
C Woods-Saxon
      ELSE IF ( Na.GT.18 ) THEN
         DT_DENSIT = ONE/(ONE+EXP((R-Ra)/pdif))
      END IF
 
      END FUNCTION
