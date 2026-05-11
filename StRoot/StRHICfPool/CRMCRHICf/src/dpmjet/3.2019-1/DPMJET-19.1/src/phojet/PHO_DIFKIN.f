
      SUBROUTINE PHO_DIFKIN(Xmp1,Xmp2,Tt,Pmom1,Pmom2,Irej)
C**********************************************************************
C
C     calculation of diffractive kinematics
C
C     input:    XMP1         mass of outgoing particle system 1 (GeV)
C               XMP2         mass of outgoing particle system 2 (GeV)
C               TT           momentum transfer    (GeV**2, negative)
C
C     output:   PMOM1(5)     four momentum of outgoing system 1
C               PMOM2(5)     four momentum of outgoing system 2
C               IREJ         0    kinematics consistent
C                            1    kinematics inconsistent
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , DT_RNDM , e1f , e1i , e2f , EPS , pcm2 , 
     &                 pcmp2 , PHO_XLAM , plong , ptran , ss , Tt , xi , 
     &                 xm1 , xm2 , Xmp1 , xmp12 , Xmp2 , xmp22
      INTEGER Irej
      SAVE 
 
      PARAMETER (EPS=1.D-10,DEPS=0.001)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  c.m. kinematics of diffraction
      INCLUDE 'inc/podcms'
C  some constants
      INCLUDE 'inc/pocons'
 
      DOUBLE PRECISION Pmom1 , Pmom2
      DIMENSION Pmom1(5) , Pmom2(5)
 
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(49).GT.10 )
     &      WRITE (LO,'(1X,A,/5X,5E12.4)')
     &      'PHO_DIFKIN: Ecmd,Pcmd,Mini-1,Mini-2,TT:' , ECMd , PCMd , 
     &     Xmp1 , Xmp2 , Tt
 
C  general kinematic constraints
      Irej = 1
      IF ( ECMd.LE.1.1D0*(Xmp1+Xmp2) ) RETURN
 
C  new squared cms momentum
      xmp12 = Xmp1**2
      xmp22 = Xmp2**2
      ss = ECMd**2
      pcm2 = PCMd**2
      pcmp2 = PHO_XLAM(ss,xmp12,xmp22)**2/(4.D0*ss)
 
C  new longitudinal/transverse momentum
      e1i = SQRT(pcm2+PMAssd(1)**2)
      e1f = SQRT(pcmp2+xmp12)
      e2f = SQRT(pcmp2+xmp22)
      plong = (Tt+pcm2+pcmp2-(e1i-e1f)**2)/(2.D0*PCMd)
      ptran = pcmp2 - plong**2
 
C  check consistency of kinematics
      IF ( ptran.LT.0.D0 ) THEN
         IF ( IDEb(49).GE.1 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I10)') 'PHO_DIFKIN: ' , 
     &           'inconsistent kinematics in event call: ' , KEVent
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/5X,1p,6E11.3)')
     &            'PHO_DIFKIN: XMP1,XMP2,TT,PCMP,PLONG,PTRANS' , Xmp1 , 
     &           Xmp2 , Tt , SQRT(pcmp2) , plong , 
     &           SIGN(SQRT(ABS(ptran)),ptran)
         END IF
         Irej = 1
         RETURN
      ELSE
         ptran = SQRT(ptran)
      END IF
      xi = PI2*DT_RNDM(ptran)
 
C  outgoing momenta in cm. system
      Pmom1(4) = e1f
      Pmom1(1) = ptran*COS(xi)
      Pmom1(2) = ptran*SIN(xi)
      Pmom1(3) = plong
      Pmom1(5) = Xmp1
 
      Pmom2(4) = e2f
      Pmom2(1) = -Pmom1(1)
      Pmom2(2) = -Pmom1(2)
      Pmom2(3) = -plong
      Pmom2(5) = Xmp2
      Irej = 0
 
C  debug output / precision check
      IF ( IDEb(49).GE.0 ) THEN
C  check kinematics
         xm1 = (Pmom1(4)-Pmom1(3))*(Pmom1(4)+Pmom1(3)) - Pmom1(1)
     &         **2 - Pmom1(2)**2
         xm1 = SIGN(SQRT(ABS(xm1)),xm1)
         xm2 = (Pmom2(4)-Pmom2(3))*(Pmom2(4)+Pmom2(3)) - Pmom2(1)
     &         **2 - Pmom2(2)**2
         xm2 = SIGN(SQRT(ABS(xm2)),xm2)
         IF ( (ABS(xm1-Xmp1).GT.DEPS) .OR. (ABS(xm1-Xmp1).GT.DEPS) )
     &        THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/5X,4E11.4)')
     &            'PHO_DIFKIN: ' , 
     &           'inconsistent masses: MINI-1,MOUT-1,MINI-2,MOUT-2' , 
     &           Xmp1 , xm1 , Xmp2 , xm2
            CALL PHO_PREVNT(-1)
         END IF
C  output
         IF ( IDEb(49).GT.10 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,5E11.3,/1X,A,5E11.3)')
     &            'PHO_DIFKIN: P1' , Pmom1 , '                 P2' , 
     &           Pmom2
         END IF
      END IF
 
      END SUBROUTINE
