
      SUBROUTINE DT_MYTRAN(Imode,Xo,Yo,Zo,Cde,Sde,Cfe,Sfe,X,Y,Z)
 
C***********************************************************************
C This subroutine rotates the coordinate frame                         *
C    a) theta  around y                                                *
C    b) phi    around z      if IMODE = 1                              *
C                                                                      *
C     x'          cos(ph) -sin(ph) 0      cos(th)  0  sin(th)   x      *
C     y' = A B =  sin(ph) cos(ph)  0  .   0        1        0   y      *
C     z'          0       0        1     -sin(th)  0  cos(th)   z      *
C                                                                      *
C and vice versa if IMODE = 0.                                         *
C This version dated 5.4.94 is based on the original version DTRAN     *
C by J. Ranft and is written by S. Roesler.                            *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Cde , Cfe , Sde , Sfe , X , Xo , Y , Yo , Z , Zo
      INTEGER Imode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      IF ( Imode.EQ.1 ) THEN
         X = Cde*Cfe*Xo - Sfe*Yo + Sde*Cfe*Zo
         Y = Cde*Sfe*Xo + Cfe*Yo + Sde*Sfe*Zo
         Z = -Sde*Xo + Cde*Zo
      ELSE
         X = Cde*Cfe*Xo + Cde*Sfe*Yo - Sde*Zo
         Y = -Sfe*Xo + Cfe*Yo
         Z = Sde*Cfe*Xo + Sde*Sfe*Yo + Cde*Zo
      END IF
      END SUBROUTINE
