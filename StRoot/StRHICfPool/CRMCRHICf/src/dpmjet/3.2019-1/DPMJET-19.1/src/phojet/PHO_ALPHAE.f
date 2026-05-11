
      DOUBLE PRECISION FUNCTION PHO_ALPHAE(Q2)
C**********************************************************************
C
C     calculation of ALPHA_em
C
C     input:    Q2      scale in GeV**2
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      DOUBLE PRECISION Q2
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
      DOUBLE PRECISION PYALEM
 
      PHO_ALPHAE = 1.D0/137.D0
 
 
 
      IF ( IPAmdl(120).EQ.1 ) PHO_ALPHAE = PYALEM(Q2)
 
      END FUNCTION
