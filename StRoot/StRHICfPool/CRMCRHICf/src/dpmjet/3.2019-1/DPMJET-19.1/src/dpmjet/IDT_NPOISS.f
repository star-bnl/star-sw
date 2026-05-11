
      INTEGER FUNCTION IDT_NPOISS(Avn)
 
C***********************************************************************
C Sample according to Poisson distribution with Poisson parameter AVN. *
C The original version written by J. Ranft.                            *
C This version dated 11.1.95 is written by S. Roesler.                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION a , Avn , DT_RNDM , expavn
      INTEGER k
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      expavn = EXP(-Avn)
      k = 1
      a = 1.0D0
 
 100  a = DT_RNDM(a)*a
      IF ( a.GE.expavn ) THEN
         k = k + 1
         GOTO 100
      END IF
      IDT_NPOISS = k - 1
 
      END FUNCTION
