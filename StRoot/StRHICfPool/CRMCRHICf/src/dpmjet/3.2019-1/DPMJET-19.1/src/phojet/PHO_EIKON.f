
      SUBROUTINE PHO_EIKON(Ip,Ifhard,B)
C*********************************************************************
C
C     calculation of unitarized amplitudes
C
C     input: IP               particle combination
C            IFHARD           -1  ignore previously calculated Born
C                                 cross sections
C                             0   calculate hard Born cross sections or
C                                 take them from interpolation table
C                                 (if available)
C                             1   take hard cross sections from /POSBRN/
C            B                impact parameter (mb**(1/2))
C                   /POSBRN/  input cross sections
C                   /GLOCMS/  cm energy
C                   /POPREG/  soft and hard parameters
C
C     output: /POINT4/
C             AMPEL           purely elastic amplitude
C             AMPVM           quasi-elastically vectormeson prod.
C             AMLMSD(2)       amplitudes of low mass sing. diffr.
C             AMHMSD(2)       amplitudes of high mass sing. diffr.
C             AMLMDD          amplitude of low mass double diffr.
C             AMHMDD          amplitude of high mass double diffr.
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION B , DEPS , elast , EXPMAX , FIVE , pvold , 
     &                 rmass1 , rmass2 , THOUS , xmass1 , xmass2 , 
     &                 xmpom , xmvdm
      INTEGER i , idxmold , Ifhard , IFIVE , IFOUR , Ip , ipold , ISIX , 
     &        ITHREE , ITWO
      SAVE 
 
      PARAMETER (ITWO=2,ITHREE=3,IFOUR=4,IFIVE=5,ISIX=6,FIVE=5.D0,
     &           THOUS=1.D3,EXPMAX=70.D0,DEPS=1.D-20)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  complex Born graph amplitudes used for unitarization
      INCLUDE 'inc/point4'
C  cross sections
      INCLUDE 'inc/pocsec'
C  Born graph cross sections and slopes
      INCLUDE 'inc/posbrn'
C  scaled cross sections and slopes
      INCLUDE 'inc/pozbrn'
C  Born graph cross sections after applying diffraction model
      INCLUDE 'inc/point1'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  unitarized amplitudes for different diffraction channels
      INCLUDE 'inc/point5'
 
      COMPLEX*16 czero , cone , b24 , auxp , auxr , auxh , auxd , 
     &           auxt1 , auxt2 , auxl , ampr , ampo , ampp , ampq
 
      DIMENSION pvold(2)
 
      DATA elast/0.D0/
      DATA ipold/ - 1/
      DATA idxmold/ - 1/
      DATA pvold/ - 1.D0 , -1.D0/
      DATA xmpom/0.766D0/
      DATA xmvdm/0.766D0/
 
C  calculation of scaled cross sections and slopes
 
C  test for redundant calculation
      IF ( (ECM.NE.elast) .OR. (Ifhard.EQ.-1) .OR. 
     &     (PVIrt(1).NE.pvold(1)) .OR. (PVIrt(2).NE.pvold(2)) .OR. 
     &     (Ip.NE.ipold) .OR. (IDXmpar.NE.idxmold) ) THEN
C  effective particle masses, VDM assumption
         xmass1 = PMAss(1)
         xmass2 = PMAss(2)
         rmass1 = RMAss(1)
         rmass2 = RMAss(2)
         IF ( IFPap(1).EQ.22 ) THEN
            xmass1 = xmvdm
         ELSE IF ( IFPap(1).EQ.990 ) THEN
            xmass1 = xmpom
         END IF
         IF ( IFPap(2).EQ.22 ) THEN
            xmass2 = xmvdm
         ELSE IF ( IFPap(2).EQ.990 ) THEN
            xmass2 = xmpom
         END IF
C  different particle combinations
         IF ( Ip.EQ.3 ) THEN
            xmass1 = xmass2
            rmass1 = rmass2
         ELSE IF ( Ip.EQ.4 ) THEN
            xmass1 = xmpom
            rmass1 = xmass1
         END IF
         IF ( Ip.GT.1 ) THEN
            xmass2 = xmpom
            rmass2 = xmass2
         END IF
C  update pomeron CM system
         PMAssp(1) = xmass1
         PMAssp(2) = xmass2
         ECMp = ECM
 
         czero = DCMPLX(0.D0,0.D0)
         cone = DCMPLX(1.D0,0.D0)
         elast = ECM
         pvold(1) = PVIrt(1)
         pvold(2) = PVIrt(2)
         ipold = Ip
         idxmold = IDXmpar
 
C  purely elastic scattering
         CALL PHO_BORNCS(Ip,Ifhard,xmass1,xmass2,xmass1,xmass2)
         ZXP(1,1) = ZIGp
         BXP(1,1) = BPOm
         ZXR(1,1) = ZIGr
         BXR(1,1) = BREg
         ZXH(1,1) = ZIGhr
         BXH(1,1) = BHAr
         ZXD(1,1) = ZIGhd
         BXD(1,1) = BHAd
         ZXT1a(1,1) = ZIGt1(1)
         BXT1a(1,1) = BTR1(1)
         ZXT1b(1,1) = ZIGt1(2)
         BXT1b(1,1) = BTR1(2)
         ZXT2a(1,1) = ZIGt2(1)
         BXT2a(1,1) = BTR2(1)
         ZXT2b(1,1) = ZIGt2(2)
         BXT2b(1,1) = BTR2(2)
         ZXL(1,1) = ZIGl
         BXL(1,1) = BLOo
         ZXDpe(1,1) = ZIGdp(1)
         BXDpe(1,1) = BDP(1)
         ZXDpa(1,1) = ZIGdp(2)
         BXDpa(1,1) = BDP(2)
         ZXDpb(1,1) = ZIGdp(3)
         BXDpb(1,1) = BDP(3)
         ZXDpd(1,1) = ZIGdp(4)
         BXDpd(1,1) = BDP(4)
         SBOpom(1) = SIGp
         SBOreg(1) = SIGr
         SBOhar(1) = SIGhr
         SBOhad(1) = SIGhd
         SBOtr1(1,1) = SIGt1(1)
         SBOtr1(1,2) = SIGt1(2)
         SBOtr2(1,1) = SIGt2(1)
         SBOtr2(1,2) = SIGt2(2)
         SBOlpo(1) = SIGl
         SBOdpo(1,1) = SIGdp(1)
         SBOdpo(1,2) = SIGdp(2)
         SBOdpo(1,3) = SIGdp(3)
         SBOdpo(1,4) = SIGdp(4)
 
C  low mass single diffractive scattering 1
         CALL PHO_BORNCS(Ip,1,xmass1,xmass2,rmass1,xmass2)
         ZXP(1,2) = ZIGp
         BXP(1,2) = BPOm
         ZXR(1,2) = ZIGr
         BXR(1,2) = BREg
         ZXH(1,2) = ZIGhr
         BXH(1,2) = BHAr
         ZXD(1,2) = ZIGhd
         BXD(1,2) = BHAd
         ZXT1a(1,2) = ZIGt1(1)
         BXT1a(1,2) = BTR1(1)
         ZXT1b(1,2) = ZIGt1(2)
         BXT1b(1,2) = BTR1(2)
         ZXT2a(1,2) = ZIGt2(1)
         BXT2a(1,2) = BTR2(1)
         ZXT2b(1,2) = ZIGt2(2)
         BXT2b(1,2) = BTR2(2)
         ZXL(1,2) = ZIGl
         BXL(1,2) = BLOo
         ZXDpe(1,2) = ZIGdp(1)
         BXDpe(1,2) = BDP(1)
         ZXDpa(1,2) = ZIGdp(2)
         BXDpa(1,2) = BDP(2)
         ZXDpb(1,2) = ZIGdp(3)
         BXDpb(1,2) = BDP(3)
         ZXDpd(1,2) = ZIGdp(4)
         BXDpd(1,2) = BDP(4)
         SBOpom(2) = SIGp
         SBOreg(2) = SIGr
         SBOhar(2) = SIGhr
         SBOhad(2) = 0.D0
         SBOtr1(2,1) = SIGt1(1)
         SBOtr1(2,2) = SIGt1(2)
         SBOtr2(2,1) = SIGt2(1)
         SBOtr2(2,2) = SIGt2(2)
         SBOlpo(2) = SIGl
         SBOdpo(2,1) = SIGdp(1)
         SBOdpo(2,2) = SIGdp(2)
         SBOdpo(2,3) = SIGdp(3)
         SBOdpo(2,4) = SIGdp(4)
 
C  low mass single diffractive scattering 2
         CALL PHO_BORNCS(Ip,1,xmass1,xmass2,xmass1,rmass2)
         ZXP(1,3) = ZIGp
         BXP(1,3) = BPOm
         ZXR(1,3) = ZIGr
         BXR(1,3) = BREg
         ZXH(1,3) = ZIGhr
         BXH(1,3) = BHAr
         ZXD(1,3) = ZIGhd
         BXD(1,3) = BHAd
         ZXT1a(1,3) = ZIGt1(1)
         BXT1a(1,3) = BTR1(1)
         ZXT1b(1,3) = ZIGt1(2)
         BXT1b(1,3) = BTR1(2)
         ZXT2a(1,3) = ZIGt2(1)
         BXT2a(1,3) = BTR2(1)
         ZXT2b(1,3) = ZIGt2(2)
         BXT2b(1,3) = BTR2(2)
         ZXL(1,3) = ZIGl
         BXL(1,3) = BLOo
         ZXDpe(1,3) = ZIGdp(1)
         BXDpe(1,3) = BDP(1)
         ZXDpa(1,3) = ZIGdp(2)
         BXDpa(1,3) = BDP(2)
         ZXDpb(1,3) = ZIGdp(3)
         BXDpb(1,3) = BDP(3)
         ZXDpd(1,3) = ZIGdp(4)
         BXDpd(1,3) = BDP(4)
         SBOpom(3) = SIGp
         SBOreg(3) = SIGr
         SBOhar(3) = SIGhr
         SBOhad(3) = 0.D0
         SBOtr1(3,1) = SIGt1(1)
         SBOtr1(3,2) = SIGt1(2)
         SBOtr2(3,1) = SIGt2(1)
         SBOtr2(3,2) = SIGt2(2)
         SBOlpo(3) = SIGl
         SBOdpo(3,1) = SIGdp(1)
         SBOdpo(3,2) = SIGdp(2)
         SBOdpo(3,3) = SIGdp(3)
         SBOdpo(3,4) = SIGdp(4)
 
C  low mass double diffractive scattering
         CALL PHO_BORNCS(Ip,1,xmass1,xmass2,rmass1,rmass2)
         ZXP(1,4) = ZIGp
         BXP(1,4) = BPOm
         ZXR(1,4) = ZIGr
         BXR(1,4) = BREg
         ZXH(1,4) = ZIGhr
         BXH(1,4) = BHAr
         ZXD(1,4) = ZIGhd
         BXD(1,4) = BHAd
         ZXT1a(1,4) = ZIGt1(1)
         BXT1a(1,4) = BTR1(1)
         ZXT1b(1,4) = ZIGt1(2)
         BXT1b(1,4) = BTR1(2)
         ZXT2a(1,4) = ZIGt2(1)
         BXT2a(1,4) = BTR2(1)
         ZXT2b(1,4) = ZIGt2(2)
         BXT2b(1,4) = BTR2(2)
         ZXL(1,4) = ZIGl
         BXL(1,4) = BLOo
         ZXDpe(1,4) = ZIGdp(1)
         BXDpe(1,4) = BDP(1)
         ZXDpa(1,4) = ZIGdp(2)
         BXDpa(1,4) = BDP(2)
         ZXDpb(1,4) = ZIGdp(3)
         BXDpb(1,4) = BDP(3)
         ZXDpd(1,4) = ZIGdp(4)
         BXDpd(1,4) = BDP(4)
         SBOpom(4) = SIGp
         SBOreg(4) = SIGr
         SBOhar(4) = SIGhr
         SBOhad(4) = 0.D0
         SBOtr1(4,1) = SIGt1(1)
         SBOtr1(4,2) = SIGt1(2)
         SBOtr2(4,1) = SIGt2(1)
         SBOtr2(4,2) = SIGt2(2)
         SBOlpo(4) = SIGl
         SBOdpo(4,1) = SIGdp(1)
         SBOdpo(4,2) = SIGdp(2)
         SBOdpo(4,3) = SIGdp(3)
         SBOdpo(4,4) = SIGdp(4)
 
C  calculate Born graph cross sections
         SBOpom(0) = 0.D0
         SBOreg(0) = 0.D0
         SBOhar(0) = 0.D0
         SBOhad(0) = 0.D0
         SBOtr1(0,1) = 0.D0
         SBOtr1(0,2) = 0.D0
         SBOtr2(0,1) = 0.D0
         SBOtr2(0,2) = 0.D0
         SBOlpo(0) = 0.D0
         SBOdpo(0,1) = 0.D0
         SBOdpo(0,2) = 0.D0
         SBOdpo(0,3) = 0.D0
         SBOdpo(0,4) = 0.D0
         DO i = 1 , 4
            SBOpom(0) = SBOpom(0) + ELAfac(i)*SBOpom(i)
            SBOreg(0) = SBOreg(0) + ELAfac(i)*SBOreg(i)
            SBOhar(0) = SBOhar(0) + ELAfac(i)*SBOhar(i)
            SBOhad(0) = SBOhad(0) + ELAfac(i)*SBOhad(i)
            SBOtr1(0,1) = SBOtr1(0,1) + ELAfac(i)*SBOtr1(i,1)
            SBOtr1(0,2) = SBOtr1(0,2) + ELAfac(i)*SBOtr1(i,2)
            SBOtr2(0,1) = SBOtr2(0,1) + ELAfac(i)*SBOtr2(i,1)
            SBOtr2(0,2) = SBOtr2(0,2) + ELAfac(i)*SBOtr2(i,2)
            SBOlpo(0) = SBOlpo(0) + ELAfac(i)*SBOlpo(i)
            SBOdpo(0,1) = SBOdpo(0,1) + ELAfac(i)*SBOdpo(i,1)
            SBOdpo(0,2) = SBOdpo(0,2) + ELAfac(i)*SBOdpo(i,2)
            SBOdpo(0,3) = SBOdpo(0,3) + ELAfac(i)*SBOdpo(i,3)
            SBOdpo(0,4) = SBOdpo(0,4) + ELAfac(i)*SBOdpo(i,4)
         END DO
 
         SIGpom = SBOpom(0)
         SIGreg = SBOreg(0)
         SIGtr1(1) = SBOtr1(0,1)
         SIGtr1(2) = SBOtr1(0,2)
         SIGtr2(1) = SBOtr2(0,1)
         SIGtr2(2) = SBOtr2(0,2)
         SIGloo = SBOlpo(0)
         SIGdpo(1) = SBOdpo(0,1)
         SIGdpo(2) = SBOdpo(0,2)
         SIGdpo(3) = SBOdpo(0,3)
         SIGdpo(4) = SBOdpo(0,4)
         SIGhar = SBOhar(0)
         SIGdir = SBOhad(0)
      END IF
 
      b24 = DCMPLX(B**2,0.D0)/4.D0
 
      AMPel = czero
      ampr = czero
      ampo = czero
      ampp = czero
      ampq = czero
      AMLmsd(1) = czero
      AMLmsd(2) = czero
      AMHmsd(1) = czero
      AMHmsd(2) = czero
      AMLmdd = czero
      AMHmdd = czero
 
C  different models
 
      IF ( ISWmdl(1).LT.3 ) THEN
C  pomeron
         auxp = ZXP(1,1)*EXP(-b24/BXP(1,1))
C  reggeon
         auxr = ZXR(1,1)*EXP(-b24/BXR(1,1))
C  hard resolved processes
         auxh = ZXH(1,1)*EXP(-b24/BXH(1,1))
C  hard direct processes
         auxd = ZXD(1,1)*EXP(-b24/BXD(1,1))
C  triple-Pomeron: baryon high mass diffraction
         auxt1 = ZXT1a(1,1)*EXP(-b24/BXT1a(1,1)) + ZXT1b(1,1)
     &           *EXP(-b24/BXT1b(1,1))
C  triple-Pomeron: photon/meson high mass diffraction
         auxt2 = ZXT2a(1,1)*EXP(-b24/BXT2a(1,1)) + ZXT2b(1,1)
     &           *EXP(-b24/BXT2b(1,1))
C  loop-Pomeron
         auxl = ZXL(1,1)*EXP(-b24/BXL(1,1))
      END IF
 
      IF ( ISWmdl(1).EQ.0 ) THEN
         AMPel = 0.5D0*((VDMq2f(1)+VDMq2f(2)+VDMq2f(3)+VDMq2f(4))
     &           *(cone-EXP(-auxr-auxp-auxh+auxt1+auxt2+auxl))
     &           +(cone-(VDMq2f(1)-VDMq2f(2)-VDMq2f(3)-VDMq2f(4)))*auxd)
         ampr = 0.5D0*SQRT(VDMq2f(1))
     &          *(cone-EXP(-auxr-auxp-auxh+auxt1+auxt2+auxl))
         ampo = 0.5D0*SQRT(VDMq2f(2))
     &          *(cone-EXP(-auxr-auxp-auxh+auxt1+auxt2+auxl))
         ampp = 0.5D0*SQRT(VDMq2f(3))
     &          *(cone-EXP(-auxr-auxp-auxh+auxt1+auxt2+auxl))
         ampq = 0.5D0*SQRT(VDMq2f(4))
     &          *(cone-EXP(-auxr-auxp-auxh+auxt1+auxt2+auxl))
 
      ELSE IF ( ISWmdl(1).EQ.1 ) THEN
         ampr = 0.5D0*SQRT(VDMq2f(1))
     &          *(cone-EXP(-auxr-auxp-auxh*VDMq2f(1)))
         ampo = 0.5D0*SQRT(VDMq2f(2))
     &          *(cone-EXP(-3.D0*auxr-auxp-auxh*VDMq2f(2)))
         ampp = 0.5D0*SQRT(VDMq2f(3))*(cone-EXP(-auxp-auxh*VDMq2f(3)))
         ampq = 0.5D0*SQRT(VDMq2f(4))
     &          *(cone-EXP(-auxr-auxp-auxh*VDMq2f(4)))
         AMPel = SQRT(VDMq2f(1))*ampr + SQRT(VDMq2f(2))
     &           *ampo + SQRT(VDMq2f(3))*ampp + SQRT(VDMq2f(4))
     &           *ampq + auxd/2.D0
 
C  simple analytic two channel model (version A)
      ELSE IF ( ISWmdl(1).EQ.3 ) THEN
         CALL PHO_CHAN2A(B)
 
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I2)')
     &         'EIKON: ERROR: unsupported model ISWMDL(1) ' , ISWmdl(1)
         STOP
      END IF
 
      END SUBROUTINE
