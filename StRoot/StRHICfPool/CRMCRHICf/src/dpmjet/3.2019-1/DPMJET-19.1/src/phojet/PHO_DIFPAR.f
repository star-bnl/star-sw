
      SUBROUTINE PHO_DIFPAR(Imoth1,Imoth2,Igenm,Ifl1,Ifl2,Ipar,P1,P2,
     &                      Iposh1,Iposh2,Imode,Irej)
C***********************************************************************
C
C     perform string construction for diffraction dissociation
C
C     input:     IMOTH1,2     index of mother particles in POEVT1
C                IGENM        production process of mother particles
C                IFL1,IFL2    particle numbers
C                             (IDPDG,IDBAM for quasi-elas. hadron)
C                IPAR         0  quasi-elasic scattering
C                             1  single string configuration
C                             2  two string configuration
C                P1           massive 4 momentum of first
C                P1(6)        virtuality/squ.mass of particle (GeV**2)
C                P1(7)        virtuality of Pomeron (neg, GeV**2)
C                P2           massive 4 momentum of second particle
C                IMODE        1   diffraction dissociation
C                             2   double-pomeron scattering
C
C     output:    IPOSH1,2     index of the particles in /POEVT1/
C                IREJ         0  successful string construction
C                             1  no string construction possible
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , EPS , P1 , P2 , pch1
      INTEGER i , ic1 , ic2 , Ifl1 , Ifl2 , igen , Igenm , iltr1 , 
     &        Imode , Imoth1 , Imoth2 , Ipar , Iposh1 , Iposh2 , Irej , 
     &        k
      SAVE 
 
      DIMENSION P1(7) , P2(7)
 
      PARAMETER (EPS=1.D-7,DEPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  c.m. kinematics of diffraction
      INCLUDE 'inc/podcms'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DIMENSION pch1(2,4)
      DATA ic1/0/
      DATA ic2/0/
 
      Irej = 0
      iltr1 = NHEp + 1
      igen = Igenm
      IF ( Igenm.LE.-10 ) igen = 0
 
C  elastic part
      IF ( Ipar.EQ.0 ) THEN
         IF ( (Ifl1.EQ.92) .OR. (Ifl1.EQ.91) ) THEN
            IF ( igen.EQ.0 ) igen = 3
C  pi+/pi- isotropic background
            CALL PHO_REGPAR(1,Ifl1,Ifl2,Imoth1,Imoth2,P1(1),P1(2),P1(3),
     &                      P1(4),0,igen,0,0,Iposh1,1)
            CALL PHO_SDECAY(Iposh1,0,-2)
         ELSE
            IF ( igen.EQ.0 ) THEN
               igen = 2
               IF ( Ifl1.NE.IDHep(Imoth1) ) igen = 3
            END IF
C  registration of particle or resonance
            CALL PHO_REGPAR(1,Ifl1,Ifl2,Imoth1,Imoth2,P1(1),P1(2),P1(3),
     &                      P1(4),0,igen,0,0,Iposh1,1)
         END IF
 
C  diffraction dissociation
      ELSE IF ( (Ipar.GE.1) .AND. (Ipar.LE.18) ) THEN
C  calculation of resulting particle momenta
         IF ( Imoth1.EQ.NPOsd(1) ) THEN
            k = 2
         ELSE
            k = 1
         END IF
         DO i = 1 , 4
            pch1(2,i) = PDCms(i,k) - P2(i)
            pch1(1,i) = P1(i) - pch1(2,i)
         END DO
 
C  registration
         IF ( Imode.LT.2 ) THEN
            IF ( igen.EQ.0 ) igen = -Igenm/10 + 4
            CALL PHO_REGPAR(1,Ifl1,Ifl2,Imoth1,Imoth2,pch1(1,1),
     &                      pch1(1,2),pch1(1,3),pch1(1,4),-1,igen,ic1,
     &                      ic2,Iposh1,1)
         ELSE
            IF ( igen.EQ.0 ) igen = 4
         END IF
         CALL PHO_REGPAR(1,990,0,Imoth2,Imoth1,pch1(2,1),pch1(2,2),
     &                   pch1(2,3),pch1(2,4),-1,igen,ic1,ic2,Iposh2,1)
 
C  invalid IPAR
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I6)')
     &         'PHO_DIFPAR:ERROR: invalid IPAR:' , Ipar
         CALL PHO_ABORT
      END IF
 
C  back transformation
      CALL PHO_LTRHEP(iltr1,NHEp,CODd,SIDd,COFd,SIFd,GAMbed(4),GAMbed(1)
     &                ,GAMbed(2),GAMbed(3))
 
      END SUBROUTINE
