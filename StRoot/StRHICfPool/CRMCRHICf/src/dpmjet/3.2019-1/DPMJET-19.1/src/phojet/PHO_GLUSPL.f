
      DOUBLE PRECISION FUNCTION PHO_GLUSPL(Zmin)
C*********************************************************************
C
C     calculate quark - antiquark light cone momentum fractions
C     according to Altarelli-Parisi g->q aq splitting function
C     (symmetric z interval assumed)
C
C     input: ZMIN    minimal Z value allowed,
C                    1-ZMIN maximal Z value allowed
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ALEXP , DEPS , DT_RNDM , xi , zmax , Zmin , 
     &                 zminl , zz
      SAVE 
 
      PARAMETER (ALEXP=0.3333333333D0,DEPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
      IF ( Zmin.GE.0.5D0 ) THEN
         IF ( IDEb(69).GT.2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,E12.4)')
     &            'PHO_GLUSPL: ZMIN>=0.5' , Zmin
         END IF
         zz = 0.D0
         GOTO 100
      ELSE IF ( Zmin.LE.0.D0 ) THEN
         IF ( IDEb(69).GT.2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,E12.4)')
     &            'PHO_GLUSPL: ZMIN<=0' , Zmin
         END IF
         zminl = DEPS
      ELSE
         zminl = Zmin
      END IF
 
      zmax = 1.D0 - zminl
      xi = DT_RNDM(zmax)
      zz = ((1.D0-xi)*zminl**3+xi*zmax**3)**ALEXP
      IF ( DT_RNDM(zz).LT.0.5D0 ) zz = 1.D0 - zz
 
 100  IF ( IDEb(69).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2E12.4)')
     &         'PHO_GLUSPL: ZMIN,Z ' , Zmin , zz
      END IF
      PHO_GLUSPL = zz
      END FUNCTION
