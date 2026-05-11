
      SUBROUTINE PHO_TRANI(Xo,Yo,Zo,Cde,Sde,Cfe,Sfe,X,Y,Z)
C**********************************************************************
C
C  rotation of coordinate frame (1) -fe rotation around z axis
C                               (2) -de rotation around y axis
C  (inverse rotation to PHO_TRANS)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Cde , Cfe , Sde , Sfe , X , Xo , Y , Yo , Z , Zo
      SAVE 
 
      X = Cde*Cfe*Xo + Cde*Sfe*Yo - Sde*Zo
      Y = -Sfe*Xo + Cfe*Yo
      Z = Sde*Cfe*Xo + Sde*Sfe*Yo + Cde*Zo
 
      END SUBROUTINE
