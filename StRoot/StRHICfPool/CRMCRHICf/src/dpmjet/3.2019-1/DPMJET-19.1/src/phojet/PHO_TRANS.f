
      SUBROUTINE PHO_TRANS(Xo,Yo,Zo,Cde,Sde,Cfe,Sfe,X,Y,Z)
C**********************************************************************
C
C  rotation of coordinate frame (1) de rotation around y axis
C                               (2) fe rotation around z axis
C  (inverse rotation to PHO_TRANI)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Cde , Cfe , Sde , Sfe , X , Xo , Y , Yo , Z , Zo
      SAVE 
 
      X = Cde*Cfe*Xo - Sfe*Yo + Sde*Cfe*Zo
      Y = Cde*Sfe*Xo + Cfe*Yo + Sde*Sfe*Zo
      Z = -Sde*Xo + Cde*Zo
 
      END SUBROUTINE
