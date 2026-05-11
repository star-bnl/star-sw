
      SUBROUTINE PHO_DIFSLP(Idf1,Idf2,Ivec1,Ivec2,Xm1,Xm2,Xmx,Tt,Slwght,
     &                      Irej)
C**********************************************************************
C
C     sampling of T  (Mandelstam variable) distribution within
C     certain limits TMIN, TMAX
C
C     input:    IDF1,2     type of diffractive vertex
C                           0   elastic/quasi-elastic scattering
C                           1   diffraction dissociation
C               IVEC1,2    vector meson IDs in case of quasi-elastic
C                          scattering, otherwise 0
C               XM1        mass of diffractive system 1 (GeV)
C               XM2        mass of diffractive system 2 (GeV)
C               XMX        max. mass of diffractive system (GeV)
C
C     output:   TT         squared momentum transfer ( < 0, GeV**2)
C               SLWGHT     weight to allow for mass-dependent slope
C               IREJ       0  successful sampling
C                          1  masses too big for given T range
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , EPS , fac , pcm2 , pcmp2 , PHO_XLAM , 
     &                 sb1 , sb2 , sc1 , sc2 , slmin , slope , Slwght , 
     &                 ss , tmax , tmaxa , tmaxe , tmaxp , tmin , tmina
      DOUBLE PRECISION tmine , tminp , Tt , xi , Xm1 , xm12 , Xm2 , 
     &                 xm22 , xma1 , xma2 , xmp12 , xmp22 , Xmx , xmx1 , 
     &                 xmx2
      INTEGER Idf1 , Idf2 , Irej , Ivec1 , Ivec2
      SAVE 
 
      PARAMETER (EPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  c.m. kinematics of diffraction
      INCLUDE 'inc/podcms'
C  cross sections
      INCLUDE 'inc/pocsec'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  parameters of the "simple" Vector Dominance Model
      INCLUDE 'inc/posvdm'
C  some constants
      INCLUDE 'inc/pocons'
 
      Irej = 0
      xm12 = Xm1**2
      xm22 = Xm2**2
      ss = ECMd**2
C
C  range of momentum transfer t
      tmin = -PARmdl(68)
      tmax = -PARmdl(69)
C  determine min. abs(t) necessary to produce masses
      pcm2 = PCMd**2
      pcmp2 = PHO_XLAM(ss,xm12,xm22)**2/(4.D0*ss)
      IF ( pcmp2.LE.0.D0 ) THEN
         Irej = 1
         Tt = 0.D0
         RETURN
      END IF
      tminp = PMAssd(1)**2 + xm12 + 2.D0*PCMd*SQRT(pcmp2)
     &        - 2.D0*SQRT((PMAssd(1)**2+pcm2)*(xm12+pcmp2))
C
      IF ( tminp.LT.tmax ) THEN
         IF ( IDEb(44).GE.3 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/5X,5E12.3)')
     &            'PHO_DIFSLP:REJECTION: ' , 
     &           'too large Tmin (XM1/2,TMIN,TMAX,TMINP)' , Xm1 , Xm2 , 
     &           tmin , tmax , tminp
         END IF
         IFAil(32) = IFAil(32) + 1
         Irej = 1
         Tt = 0.D0
         RETURN
      END IF
      tmina = MIN(tmin,tminp)
C
C  calculation of slope (mass-dependent parametrization)
      IF ( Idf1+Idf2.GT.0 ) THEN
C  diffraction dissociation
         xmp12 = Xm1**2 + PVIrtd(1)
         xmp22 = Xm2**2 + PVIrtd(2)
         xmx1 = SQRT(xmp12)
         xmx2 = SQRT(xmp22)
         CALL PHO_SCALES(PMAssd(1),PMAssd(2),xmx1,xmx2,sc1,sc2,sb1,sb2)
         fac = 4.D0*(PMAssd(1)*PMAssd(2))**2
         slope = DBLE(Idf1+Idf2)
     &           *B0Ppp + 2.D0*(B0Pom(1)*sb1+B0Pom(2)*sb2+
     &           ALPomp*LOG(ss*fac/((PMAssd(1)**2+xmp12)
     &           *(PMAssd(2)**2+xmp22))+PARmdl(47)))
         slope = MAX(slope,1.D0)
C
         xma1 = Xmx
         xma2 = Xmx
         IF ( Idf1.EQ.0 ) THEN
            xma1 = Xm1
C  Typo corrected according to Anatoli:
C       ELSE IF(IDF1.EQ.0) THEN
         ELSE IF ( Idf2.EQ.0 ) THEN
            xma2 = Xm2
         END IF
         xmp12 = xma1**2 + PVIrtd(1)
         xmp22 = xma2**2 + PVIrtd(2)
         xmx1 = SQRT(xmp12)
         xmx2 = SQRT(xmp22)
         CALL PHO_SCALES(PMAssd(1),PMAssd(2),xmx1,xmx2,sc1,sc2,sb1,sb2)
         slmin = DBLE(Idf1+Idf2)
     &           *B0Ppp + 2.D0*(B0Pom(1)*sb1+B0Pom(2)*sb2+
     &           ALPomp*LOG(ss*fac/((PMAssd(1)**2+xmp12)
     &           *(PMAssd(2)**2+xmp22))+PARmdl(47)))
         slmin = MAX(slmin,1.D0)
C  elastic/quasi-elastic scattering
      ELSE IF ( ISWmdl(13).EQ.0 ) THEN
C  external slope values
         IF ( LPRi.GT.4 ) WRITE (LO,*)
     &         'PHO_DIFSLP:ERROR: this option is not installed !'
         CALL PHO_ABORT
      ELSE IF ( ISWmdl(13).EQ.1 ) THEN
C  model slopes
         IF ( Ivec1*Ivec2.EQ.0 ) THEN
            slope = SLOel
         ELSE
            slope = SLOvm(Ivec1,Ivec2)
         END IF
         slmin = slope
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &         'SASDSDT:ERROR: invalid ISWMDL(13)' , ISWmdl(13)
         CALL PHO_ABORT
      END IF
C
C  determine max. abs(t) to avoid underflows
      tmaxp = -25.D0/slope
      tmaxa = MAX(tmax,tmaxp)
C
      IF ( tmina.LT.tmaxa ) THEN
         IF ( IDEb(44).GE.3 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/5X,5E12.3)')
     &            'PHO_DIFSLP:REJECTION: ' , 
     &           'too small Tmax (XM1/2,TMINA,TMAXA,SLOPE)' , Xm1 , 
     &           Xm2 , tmina , tmaxa , slope
         END IF
         IFAil(32) = IFAil(32) + 1
         Irej = 1
         Tt = 0.D0
         RETURN
      END IF
C
C  sampling from corrected range of T
      tmine = EXP(slmin*tmina)
      tmaxe = EXP(slmin*tmaxa)
      xi = (tmine-tmaxe)*DT_RNDM(slmin) + tmaxe
      Tt = LOG(xi)/slmin
      Slwght = EXP((slope-slmin)*Tt)
C
C  debug output
      IF ( IDEb(44).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,
     &        '(1X,A,1P,E12.3/5X,A,2I2,2X,2I2,2E10.2,/5X,A,5E10.2)')
     &         'PHO_DIFSLP: sampled momentum transfer:' , Tt , 
     &        'IDF1/2,IVEC1/2,XM1/2:' , Idf1 , Idf2 , Ivec1 , Ivec2 , 
     &        Xm1 , Xm2 , 'Tmi,Tmx,SLMIN,SLOPE,WGHT:' , tminp , tmaxp , 
     &        slmin , slope , Slwght
      END IF
      END SUBROUTINE
