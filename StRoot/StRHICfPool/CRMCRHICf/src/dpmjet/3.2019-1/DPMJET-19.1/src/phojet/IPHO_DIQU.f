
      INTEGER FUNCTION IPHO_DIQU(Iq1,Iq2)
C***********************************************************************
C
C     selection of diquark number (PDG convention)
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Iq1 , Iq2
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
C  external functions
      DOUBLE PRECISION DT_RNDM
 
C  local variables
      INTEGER i0 , i1 , i2
      DOUBLE PRECISION dum
 
      i1 = ABS(Iq1)
      i2 = ABS(Iq2)
 
      IF ( i1.EQ.i2 ) THEN
         i0 = i1*1100 + 3
      ELSE
         i0 = MAX(i1,i2)*1000 + MIN(i1,i2)*100
         IF ( DT_RNDM(dum).GT.PARmdl(135) ) THEN
            i0 = i0 + 1
         ELSE
            i0 = i0 + 3
         END IF
      END IF
 
      IPHO_DIQU = SIGN(i0,Iq1)
 
      END FUNCTION
