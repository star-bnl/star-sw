
      SUBROUTINE DT_STTRAN(Xo,Yo,Zo,Cde,Sde,Sfe,Cfe,X,Y,Z)
 
      IMPLICIT NONE
      DOUBLE PRECISION a , anglsq , Cde , Cfe , Sde , Sfe , X , xi , 
     &                 Xo , Y , yi , Yo , Z , zi , Zo
      SAVE 
      DATA anglsq/1.D-30/
C***********************************************************************
C     VERSION BY                     J. RANFT                          *
C                                    LEIPZIG                           *
C                                                                      *
C     THIS IS A SUBROUTINE OF FLUKA TO GIVE NEW DIRECTION COSINES      *
C                                                                      *
C     INPUT VARIABLES:                                                 *
C        XO,YO,ZO = ORIGINAL DIRECTION COSINES                         *
C        CDE,SDE  = COSINE AND SINE OF THE POLAR (THETA)               *
C                   ANGLE OF "SCATTERING"                              *
C        SDE      = SINE OF THE POLAR (THETA) ANGLE OF "SCATTERING"    *
C        SFE,CFE  = SINE AND COSINE OF THE AZIMUTHAL (PHI) ANGLE       *
C                   OF "SCATTERING"                                    *
C                                                                      *
C     OUTPUT VARIABLES:                                                *
C        X,Y,Z     = NEW DIRECTION COSINES                             *
C                                                                      *
C     ROTATION OF COORDINATE SYSTEM (SEE CERN 64-47 )                  *
C***********************************************************************
C
C
C  Changed by A. Ferrari
C
C     IF (ABS(XO)-0.0001D0) 1,1,2
C   1 IF (ABS(YO)-0.0001D0) 3,3,2
C   3 CONTINUE
      a = Xo**2 + Yo**2
      IF ( a.LT.anglsq ) THEN
         X = Sde*Cfe
         Y = Sde*Sfe
         Z = Cde*Zo
      ELSE
         xi = Sde*Cfe
         yi = Sde*Sfe
         zi = Cde
         a = SQRT(a)
         X = -Yo*xi/a - Zo*Xo*yi/a + Xo*zi
         Y = Xo*xi/a - Zo*Yo*yi/a + Yo*zi
         Z = a*yi + Zo*zi
      END IF
 
      END SUBROUTINE
