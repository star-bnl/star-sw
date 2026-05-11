
      DOUBLE PRECISION FUNCTION DT_RNCLUS(N)
 
C***********************************************************************
C Nuclear radius for nucleus with mass number N.                       *
C This version dated 26.9.00  is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION ONE , ONETHI , radnuc , RNUCLE , THREE
      SAVE 
 
      PARAMETER (ONE=1.0D0,THREE=3.0D0,ONETHI=ONE/THREE)
 
C nucleon radius
      PARAMETER (RNUCLE=1.12D0)
 
C nuclear radii for selected nuclei
      DIMENSION radnuc(18)
      DATA radnuc/8*0.0D0 , 2.52D0 , 2.45D0 , 2.37D0 , 2.45D0 , 2.44D0 , 
     &     2.55D0 , 2.58D0 , 2.71D0 , 2.66D0 , 2.71D0/
 
      IF ( N.GT.18 ) THEN
         DT_RNCLUS = RNUCLE*DBLE(N)**ONETHI
      ELSE IF ( radnuc(N).GT.0.0D0 ) THEN
         DT_RNCLUS = radnuc(N)
      ELSE
         DT_RNCLUS = RNUCLE*DBLE(N)**ONETHI
      END IF
 
      END FUNCTION
