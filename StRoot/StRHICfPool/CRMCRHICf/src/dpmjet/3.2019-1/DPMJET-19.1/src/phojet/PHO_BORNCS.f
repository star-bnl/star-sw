
      SUBROUTINE PHO_BORNCS(Ip,Ifhard,Xm1,Xm2,Xm3,Xm4)
C*********************************************************************
C
C     calculation of Born graph cross sections and slopes
C
C     input: IP               particle combination
C            IFHARD           -1 calculate hard Born graph cross section
C                             0  take hard Born graph cross section
C                                from interpolation table if available
C                             1  assume that correct hard cross
C                                sections are already stored in /POSBRN/
C            XM1,XM2,XM3,XM4  masses of external lines
C                   /GLOCMS/  energy and PT cut-off
C                   /POPREG/  soft and hard parameters
C                   /POSBRN/  input cross sections
C                   /POZBRN/  scaled input values
C                    IFHARD   0  calculate hard input cross sections
C                             1  assume hard input cross sections exist
C
C     output: ZPOM            scaled pomeron cross section
C             ZIGR            scaled reggeon cross section
C             ZIGHR           scaled hard resolved cross section
C             ZIGHD           scaled hard direct cross section
C             ZIGT1           scaled triple-Pomeron cross section
C             ZIGT2           scaled triple-Pomeron cross section
C             ZIGL            scaled loop-Pomeron cross section
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alharp , b0pom1 , b0pom2 , b0reg1 , b0reg2 , 
     &                 bl4 , bp1 , bp2 , btr , btx , deltap , DEPS , 
     &                 dspt , DT_SANO , EPS , FIVE , gp1 , gp2 , gr1 , 
     &                 gr2
      INTEGER i , Ifhard , IFIVE , IFOUR , Ip , ITHREE , ITWO , m
      DOUBLE PRECISION rmass1 , rmass2 , scalb1 , scalb2 , scale , 
     &                 scale1 , scale2 , scb1 , scb2 , scg1 , scg2 , 
     &                 sd , sigtr , sigtx , THOUS , virt1 , virt2 , 
     &                 Xm1 , Xm2 , Xm3
      DOUBLE PRECISION Xm4 , xmpom , xmr2
      SAVE 
 
      PARAMETER (ITWO=2,ITHREE=3,IFOUR=4,IFIVE=5,FIVE=5.D0,THOUS=1.D3,
     &           EPS=0.01D0,DEPS=1.D-30)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
C  event debugging information
      INCLUDE 'inc/podebg'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  names of hard scattering processes
      INCLUDE 'inc/pohpro'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  interpolation tables for hard cross section and MC selection weights
#ifndef FOR_CORSIKA
      INCLUDE 'inc/pohtab'
#else
      INCLUDE 'inc/pohtab50'
#endif
C  Born graph cross sections and slopes
      INCLUDE 'inc/posbrn'
C  scaled cross sections and slopes
      INCLUDE 'inc/pozbrn'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  data needed for soft-pt calculation
      INCLUDE 'inc/point3'
 
      COMPLEX*16 czero , bp4 , br4 , bhr4 , bhd4 , bt14 , bt24 , bd4 , 
     &           sp , sr , ss , bpom1 , bpom2 , breg1 , breg2 , b0hard
      DIMENSION scb1(4) , scb2(4) , scg1(4) , scg2(4)
      DIMENSION bt14(2) , bt24(2) , bd4(4)
      DIMENSION dspt(0:MAX_PRO_2)
 
      DATA xmpom/0.766D0/
      DATA czero/(0.D0,0.D0)/
 
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(48).GE.10 )
     &      WRITE (LO,'(/1X,A,I3,4E16.9,I3)')
     &      'PHO_BORNCS: IP,M1..M4,IFHARD' , Ip , Xm1 , Xm2 , Xm3 , 
     &     Xm4 , Ifhard
C  scales
      CALL PHO_SCALES(Xm1,Xm2,Xm3,Xm4,scale1,scale2,scalb1,scalb2)
C
C  calculate hard input cross sections (output in mb)
      IF ( Ifhard.NE.1 ) THEN
         IF ( (Ifhard.EQ.0) .AND. (HECm_tab(1,Ip,IDXmpar).GT.1.D0) )
     &        THEN
C  double-log interpolation
            CALL PHO_HARINT(Ip,ECMp,0.D0,0.D0,0,MAX_PRO_2,3,4,1)
            DO m = 0 , MAX_PRO_2
               DSIgh(m) = HSIg(m,IDXmpar)
               dspt(m) = HDPt(m,IDXmpar)
            END DO
         ELSE
C  new calculation
            CALL PHO_HARINT(Ip,ECMp,0.D0,0.D0,0,-2,0,0,1)
            CALL PHO_HARXTO(ECMp,PTCut(Ip),PTCut(Ip),DSIgh,dspt)
         END IF
C
C  save values to calculate soft pt distribution
         IF ( Ip.EQ.1 ) THEN
            VDMq2f(1) = VDMfac(1)
            VDMq2f(2) = VDMfac(2)
            VDMq2f(3) = VDMfac(3)
            VDMq2f(4) = VDMfac(4)
         ELSE IF ( Ip.EQ.2 ) THEN
            VDMq2f(1) = VDMfac(1)
            VDMq2f(2) = VDMfac(2)
            VDMq2f(3) = 1.D0
            VDMq2f(4) = 0.D0
         ELSE IF ( Ip.EQ.3 ) THEN
            VDMq2f(1) = VDMfac(3)
            VDMq2f(2) = VDMfac(4)
            VDMq2f(3) = 1.D0
            VDMq2f(4) = 0.D0
         ELSE
            VDMq2f(1) = 1.D0
            VDMq2f(2) = 0.D0
            VDMq2f(3) = 1.D0
            VDMq2f(4) = 0.D0
         END IF
C  VDM factors
         AMPfac(1) = SQRT(VDMq2f(1)*VDMq2f(3))
         AMPfac(2) = SQRT(VDMq2f(2)*VDMq2f(3))
         AMPfac(3) = SQRT(VDMq2f(1)*VDMq2f(4))
         AMPfac(4) = SQRT(VDMq2f(2)*VDMq2f(4))
         ELAfac(1) = VDMq2f(1)*VDMq2f(3) + VDMq2f(2)*VDMq2f(3)
     &               + VDMq2f(1)*VDMq2f(4) + VDMq2f(2)*VDMq2f(4)
         ELAfac(2) = 2.D0*(AMPfac(1)*AMPfac(2)+AMPfac(3)*AMPfac(4))
         ELAfac(3) = 2.D0*(AMPfac(1)*AMPfac(3)+AMPfac(2)*AMPfac(4))
         ELAfac(4) = 4.D0*AMPfac(1)*AMPfac(4)
         VFAc = ELAfac(1) + PHIsup(1)*PHIsup(2)*ELAfac(4) + PHIsup(1)
     &          *ELAfac(2) + PHIsup(2)*ELAfac(3)
         DSIghp = dspt(9)/VFAc
         SIGh = DSIgh(9)/VFAc
C  extract real part
         IF ( IPAmdl(1).EQ.0 ) THEN
            DO i = 0 , MAX_PRO_2
               DSIgh(i) = DCMPLX(DREAL(DSIgh(i)),0.D0)
            END DO
         END IF
C  write out results
         IF ( IDEb(48).GE.15 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,1P,2E11.3)')
     &            'PHO_BORNCS: QCD-PM cross sections (mb)' , ECMp , 
     &           PTCut(Ip)
            DO i = 0 , MAX_PRO_2
               IF ( LPRi.GT.4 ) WRITE (LO,'(10X,A,2E20.10)') PROc(i) , 
     &              DSIgh(i)
            END DO
         END IF
      END IF
 
C  DPMJET interface: subtract anomalous part
      IF ( (Ip.EQ.1) .AND. (IPAmdl(13).GT.0) ) DSIgh(9) = DSIgh(9)
     &     - DCMPLX(DT_SANO(ECMp),0.D0)
 
      scale = ABS(DSIgh(15))
      IF ( scale.LT.DEPS ) THEN
         SIGhd = czero
      ELSE
         SIGhd = DSIgh(15)
      END IF
      scale = ABS(DSIgh(9))
      IF ( scale.LT.DEPS ) THEN
         SIGhr = czero
      ELSE
         SIGhr = DSIgh(9)*scale1*scale2/VFAc
      END IF
 
C  calculate soft input cross sections (output in mb)
      ss = DCMPLX(ECMp**2-PMAssp(1)**2-PMAssp(2)**2+0.01D0,0.D0)
      IF ( IPAmdl(1).EQ.1 ) THEN
C  pomeron signature
         sp = ss*DCMPLX(0.D0,-1.D0)
C  reggeon signature
         sr = ss*DCMPLX(0.D0,1.D0)
      ELSE
         sp = ss
         sr = ss
      END IF
C  coupling constants (mb**1/2)
C  particle dependent slopes (GeV**-2)
      IF ( Ip.EQ.1 ) THEN
         gp1 = GP(1)
         gp2 = GP(2)
         gr1 = GR(1)
         gr2 = GR(2)
         b0pom1 = B0Pom(1)
         b0pom2 = B0Pom(2)
         b0reg1 = B0Reg(1)
         b0reg2 = B0Reg(2)
         b0hard = B0Har
         rmass1 = RMAss(1)
         rmass2 = RMAss(2)
      ELSE IF ( Ip.EQ.2 ) THEN
         gp1 = GP(1)
         gp2 = PARmdl(77)
         gr1 = GR(1)
         gr2 = PARmdl(77)*GPPr/GPPp
         b0pom1 = B0Pom(1)
         b0pom2 = B0Ppp
         b0reg1 = B0Reg(1)
         b0reg2 = B0Ppr
         b0hard = b0pom1 + b0pom2
         rmass1 = RMAss(1)
         rmass2 = xmpom
      ELSE IF ( Ip.EQ.3 ) THEN
         gp1 = GP(2)
         gp2 = PARmdl(77)
         gr1 = GR(2)
         gr2 = PARmdl(77)*GPPr/GPPp
         b0pom1 = B0Pom(2)
         b0pom2 = B0Ppp
         b0reg1 = B0Reg(2)
         b0reg2 = B0Ppr
         b0hard = b0pom1 + b0pom2
         rmass1 = RMAss(2)
         rmass2 = xmpom
      ELSE IF ( Ip.EQ.4 ) THEN
         gp1 = PARmdl(77)
         gp2 = gp1
         gr1 = PARmdl(77)*GPPr/GPPp
         gr2 = gr1
         b0pom1 = B0Ppp
         b0pom2 = B0Ppp
         b0reg1 = B0Ppr
         b0reg2 = B0Ppr
         b0hard = b0pom1 + b0pom2
         rmass1 = xmpom
         rmass2 = xmpom
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I7)')
     &         'PHO_BORNCS:ERROR:invalid IP' , Ip
         CALL PHO_ABORT
      END IF
      gp1 = gp1*scale1
      gp2 = gp2*scale2
      gr1 = gr1*scale1
      gr2 = gr2*scale2
C  input slope parameters (GeV**-2)
      bpom1 = b0pom1*scalb1
      bpom2 = b0pom2*scalb2
      breg1 = b0reg1*scalb1
      breg2 = b0reg2*scalb2
C  effective slopes
      xmr2 = (2.D0*MIN(Xm1,Xm3)*MIN(Xm2,Xm4))**2
      scale = ss*xmr2/((Xm1**2+Xm3**2)*(Xm2**2+Xm4**2)) + 2.D0
      BPOm = bpom1 + bpom2 + ALPomp*LOG(scale)
      BREg = breg1 + breg2 + ALRegp*LOG(scale)
      IF ( IPAmdl(9).EQ.0 ) THEN
         BHAr = b0hard
         BHAd = b0hard
      ELSE IF ( IPAmdl(9).EQ.1 ) THEN
         BHAr = b0hard*(scalb1+scalb2)/2.D0
         BHAd = BHAr
      ELSE IF ( IPAmdl(9).EQ.2 ) THEN
         BHAr = bpom1 + bpom2
         BHAd = BHAr
      ELSE IF ( IPAmdl(9).EQ.3 ) THEN
         alharp = PARmdl(260)*ALPomp
         BHAr = b0hard + alharp*LOG(scale)
         BHAd = BHAr
      ELSE
         BHAr = BPOm
         BHAd = BPOm
      END IF
C  input cross section pomeron
      SIGp = gp1*gp2*EXP((ALPom-1.D0)*LOG(sp))
      SIGr = gr1*gr2*EXP((ALReg-1.D0)*LOG(sr))
C  save value to calculate soft pt distribution
      SIGs = (SIGr+SIGp)/(scale1*scale2)
 
C  higher order graphs
      virt1 = PVIrtp(1)
      virt2 = PVIrtp(2)
C  bare/renormalized intercept for enhanced graphs
      IF ( IPAmdl(8).EQ.0 ) THEN
         deltap = ALPom - 1.D0
      ELSE
         deltap = PARmdl(48) - 1.D0
      END IF
      sd = ECMp**2
      bp1 = 2.D0*bpom1
      bp2 = 2.D0*bpom2
C  input cross section high-mass double diffraction
      CALL PHO_LOOREG(sd,gp1,bp1,gp2,bp2,deltap,ALPomp,GPPp,B0Ppp,virt1,
     &                virt2,sigtr,btr)
      SIGl = DCMPLX(sigtr,0.D0)
      BLOo = DCMPLX(btr,0.D0)
C
C  input cross section high mass diffraction particle 1
C  first possibility
      CALL PHO_SCALES(Xm1,Xm2,Xm3,PMAssp(2),scg1(1),scg2(1),scb1(1),
     &                scb2(1))
      CALL PHO_SCALES(Xm1,PMAssp(2),Xm3,Xm4,scg1(2),scg2(2),scb1(2),
     &                scb2(2))
      scalb1 = (scb1(1)+scb1(2))/2.D0
      scalb2 = (scb2(1)+scb2(2))/2.D0
      bp1 = 2.D0*bpom1*scalb1
      bp2 = 2.D0*bpom2*scalb2
C  input cross section high mass diffraction
      CALL PHO_TRIREG(sd,gp1,bp1,gp2,bp2,deltap,ALPomp,GPPp,B0Ppp,virt1,
     &                sigtr,btr)
      SIGt1(1) = scg1(1)*scg2(1)*scg2(2)*DCMPLX(sigtr,0.D0)
      BTR1(1) = DCMPLX(btr,0.D0)
C  second possibility:  high-low mass double diffraction
      CALL PHO_SCALES(Xm1,Xm2,Xm3,rmass2,scg1(1),scg2(1),scb1(1),scb2(1)
     &                )
      CALL PHO_SCALES(Xm1,rmass2,Xm3,Xm4,scg1(2),scg2(2),scb1(2),scb2(2)
     &                )
      scalb1 = (scb1(1)+scb1(2))/2.D0
      scalb2 = (scb2(1)+scb2(2))/2.D0
      bp1 = 2.D0*bpom1*scalb1
      bp2 = 2.D0*bpom2*scalb2
C  input cross section high mass diffraction
      CALL PHO_TRIREG(sd,gp1,bp1,gp2,bp2,deltap,ALPomp,GPPp,B0Ppp,virt1,
     &                sigtr,btr)
      SIGt1(2) = scg1(1)*scg2(1)*scg2(2)*DCMPLX(sigtr,0.D0)
      BTR1(2) = DCMPLX(btr,0.D0)
C
C  input cross section high mass diffraction particle 2
C  first possibility
      CALL PHO_SCALES(Xm1,Xm2,PMAssp(1),Xm4,scg1(1),scg2(1),scb1(1),
     &                scb2(1))
      CALL PHO_SCALES(PMAssp(1),Xm2,Xm3,Xm4,scg1(2),scg2(2),scb1(2),
     &                scb2(2))
      scalb1 = (scb1(1)+scb1(2))/2.D0
      scalb2 = (scb2(1)+scb2(2))/2.D0
      bp1 = 2.D0*bpom1*scalb1
      bp2 = 2.D0*bpom2*scalb2
C  input cross section high mass diffraction
      CALL PHO_TRIREG(sd,gp2,bp2,gp1,bp1,deltap,ALPomp,GPPp,B0Ppp,virt2,
     &                sigtr,btr)
      SIGt2(1) = scg1(1)*scg1(2)*scg2(1)*DCMPLX(sigtr,0.D0)
      BTR2(1) = DCMPLX(btr,0.D0)
C  second possibility:  high-low mass double diffraction
      CALL PHO_SCALES(Xm1,Xm2,rmass1,Xm4,scg1(1),scg2(1),scb1(1),scb2(1)
     &                )
      CALL PHO_SCALES(rmass1,Xm2,Xm3,Xm4,scg1(2),scg2(2),scb1(2),scb2(2)
     &                )
      scalb1 = (scb1(1)+scb1(2))/2.D0
      scalb2 = (scb2(1)+scb2(2))/2.D0
      bp1 = 2.D0*bpom1*scalb1
      bp2 = 2.D0*bpom2*scalb2
C  input cross section high mass diffraction
      CALL PHO_TRIREG(sd,gp2,bp2,gp1,bp1,deltap,ALPomp,GPPp,B0Ppp,virt2,
     &                sigtr,btr)
      SIGt2(2) = scg1(1)*scg1(2)*scg2(1)*DCMPLX(sigtr,0.D0)
      BTR2(2) = DCMPLX(btr,0.D0)
C
C  input cross section for loop-pomeron
C  first possibility
      CALL PHO_SCALES(Xm1,Xm2,PMAssp(1),Xm4,scg1(1),scg2(1),scb1(1),
     &                scb2(1))
      CALL PHO_SCALES(PMAssp(1),Xm2,Xm3,Xm4,scg1(2),scg2(2),scb1(2),
     &                scb2(2))
      CALL PHO_SCALES(Xm1,Xm2,Xm3,PMAssp(2),scg1(3),scg2(3),scb1(3),
     &                scb2(3))
      CALL PHO_SCALES(Xm1,PMAssp(2),Xm3,Xm4,scg1(4),scg2(4),scb1(4),
     &                scb2(4))
      scalb1 = (scb1(1)+scb1(2)+scb1(3)+scb1(4))/4.D0
      scalb2 = (scb2(1)+scb2(2)+scb2(3)+scb2(4))/4.D0
      bp1 = bpom1*scalb1
      bp2 = bpom2*scalb2
      CALL PHO_TRXPOM(sd,gp2,bp2,gp1,bp1,deltap,ALPomp,GPPp,B0Ppp,sigtx,
     &                btx)
      SIGdp(1) = scg1(1)*scg1(2)*scg2(3)*scg2(4)*DCMPLX(sigtx,0.D0)
      BDP(1) = DCMPLX(btx,0.D0)
C  second possibility
      CALL PHO_SCALES(Xm1,Xm2,rmass1,Xm4,scg1(1),scg2(1),scb1(1),scb2(1)
     &                )
      CALL PHO_SCALES(rmass1,Xm2,Xm3,Xm4,scg1(2),scg2(2),scb1(2),scb2(2)
     &                )
      CALL PHO_SCALES(Xm1,Xm2,Xm3,PMAssp(2),scg1(3),scg2(3),scb1(3),
     &                scb2(3))
      CALL PHO_SCALES(Xm1,PMAssp(2),Xm3,Xm4,scg1(4),scg2(4),scb1(4),
     &                scb2(4))
      scalb1 = (scb1(1)+scb1(2)+scb1(3)+scb1(4))/4.D0
      scalb2 = (scb2(1)+scb2(2)+scb2(3)+scb2(4))/4.D0
      bp1 = bpom1*scalb1
      bp2 = bpom2*scalb2
      CALL PHO_TRXPOM(sd,gp2,bp2,gp1,bp1,deltap,ALPomp,GPPp,B0Ppp,sigtx,
     &                btx)
      SIGdp(2) = scg1(1)*scg1(2)*scg2(3)*scg2(4)*DCMPLX(sigtx,0.D0)
      BDP(2) = DCMPLX(btx,0.D0)
C  third possibility
      CALL PHO_SCALES(Xm1,Xm2,PMAssp(1),Xm4,scg1(1),scg2(1),scb1(1),
     &                scb2(1))
      CALL PHO_SCALES(PMAssp(1),Xm2,Xm3,Xm4,scg1(2),scg2(2),scb1(2),
     &                scb2(2))
      CALL PHO_SCALES(Xm1,Xm2,Xm3,rmass2,scg1(3),scg2(3),scb1(3),scb2(3)
     &                )
      CALL PHO_SCALES(Xm1,rmass2,Xm3,Xm4,scg1(4),scg2(4),scb1(4),scb2(4)
     &                )
      scalb1 = (scb1(1)+scb1(2)+scb1(3)+scb1(4))/4.D0
      scalb2 = (scb2(1)+scb2(2)+scb2(3)+scb2(4))/4.D0
      bp1 = bpom1*scalb1
      bp2 = bpom2*scalb2
      CALL PHO_TRXPOM(sd,gp2,bp2,gp1,bp1,deltap,ALPomp,GPPp,B0Ppp,sigtx,
     &                btx)
      SIGdp(3) = scg1(1)*scg1(2)*scg2(3)*scg2(4)*DCMPLX(sigtx,0.D0)
      BDP(3) = DCMPLX(btx,0.D0)
C  fourth possibility
      CALL PHO_SCALES(Xm1,Xm2,rmass1,Xm4,scg1(1),scg2(1),scb1(1),scb2(1)
     &                )
      CALL PHO_SCALES(rmass1,Xm2,Xm3,Xm4,scg1(2),scg2(2),scb1(2),scb2(2)
     &                )
      CALL PHO_SCALES(Xm1,Xm2,Xm3,rmass2,scg1(3),scg2(3),scb1(3),scb2(3)
     &                )
      CALL PHO_SCALES(Xm1,rmass2,Xm3,Xm4,scg1(4),scg2(4),scb1(4),scb2(4)
     &                )
      scalb1 = (scb1(1)+scb1(2)+scb1(3)+scb1(4))/4.D0
      scalb2 = (scb2(1)+scb2(2)+scb2(3)+scb2(4))/4.D0
      bp1 = bpom1*scalb1
      bp2 = bpom2*scalb2
      CALL PHO_TRXPOM(sd,gp2,bp2,gp1,bp1,deltap,ALPomp,GPPp,B0Ppp,sigtx,
     &                btx)
      SIGdp(4) = scg1(1)*scg1(2)*scg2(3)*scg2(4)*DCMPLX(sigtx,0.D0)
      BDP(4) = DCMPLX(btx,0.D0)
C
C  input cross section for YY-iterated triple-pomeron
C     .....
C
C  write out input cross sections
      IF ( IDEb(48).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &         'Born graph input cross sections and slopes' , 
     &        '------------------------------------------'
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3E16.9)')
     &         'energy                  ' , ECMp , PVIrtp
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,4E16.9)')
     &         'external masses 1,2,3,4 ' , Xm1 , Xm2 , Xm3 , Xm4
         IF ( LPRi.GT.4 ) WRITE (LO,'(A)')
     &         ' input cross sections (millibarn):'
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '           SIGR     ' , SIGr
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        ' (soft)    SIGP     ' , SIGp
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        ' (hard)    SIGHR    ' , SIGhr
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '           SIGHD    ' , SIGhd
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)')
     &        '           SIGT1    ' , SIGt1
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)')
     &        '           SIGT2    ' , SIGt2
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '           SIGL     ' , SIGl
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)')
     &        '         SIGDP(1-2) ' , SIGdp(1) , SIGdp(2)
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)')
     &        '         SIGDP(3-4) ' , SIGdp(3) , SIGdp(4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(A)') ' input slopes (GeV**-2)'
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '           BREG     ' , BREg
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '            BREG1   ' , breg1
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '            BREG2   ' , breg2
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '           BPOM     ' , BPOm
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '            BPOM1   ' , bpom1
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '            BPOM2   ' , bpom2
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '           BHAR     ' , BHAr
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '           BHAD     ' , BHAd
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,E16.9)')
     &        '           B0PPP    ' , B0Ppp
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)')
     &        '           BTR1     ' , BTR1
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)')
     &        '           BTR2     ' , BTR2
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)')
     &        '           BLOO     ' , BLOo
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)')
     &        '           BDP(1-2) ' , BDP(1) , BDP(2)
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)')
     &        '           BDP(3-4) ' , BDP(3) , BDP(4)
      END IF
C
      BPOm = BPOm*GEV2mb
      BREg = BREg*GEV2mb
      BHAr = BHAr*GEV2mb
      BHAd = BHAd*GEV2mb
      BTR1(1) = BTR1(1)*GEV2mb
      BTR1(2) = BTR1(2)*GEV2mb
      BTR2(1) = BTR2(1)*GEV2mb
      BTR2(2) = BTR2(2)*GEV2mb
      BLOo = BLOo*GEV2mb
C
      bp4 = BPOm*4.D0
      br4 = BREg*4.D0
      bhr4 = BHAr*4.D0
      bhd4 = BHAd*4.D0
      bt14(1) = BTR1(1)*4.D0
      bt14(2) = BTR1(2)*4.D0
      bt24(1) = BTR2(1)*4.D0
      bt24(2) = BTR2(2)*4.D0
      bl4 = BLOo*4.D0
C
      ZIGp = SIGp/(PI2*bp4)
      ZIGr = SIGr/(PI2*br4)
      ZIGhr = SIGhr/(PI2*bhr4)
      ZIGhd = SIGhd/(PI2*bhd4)
      ZIGt1(1) = SIGt1(1)/(PI2*bt14(1))
      ZIGt1(2) = SIGt1(2)/(PI2*bt14(2))
      ZIGt2(1) = SIGt2(1)/(PI2*bt24(1))
      ZIGt2(2) = SIGt2(2)/(PI2*bt24(2))
      ZIGl = SIGl/(PI2*bl4)
      DO i = 1 , 4
         BDP(i) = BDP(i)*GEV2mb
         bd4(i) = BDP(i)*4.D0
         ZIGdp(i) = SIGdp(i)/(PI2*bd4(i))
      END DO
C
      IF ( IDEb(48).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(A)') ' normalized input values:'
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '           ZIGR ' , 
     &        ZIGr
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '           BREG ' , 
     &        BREg
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '           ZIGP ' , 
     &        ZIGp
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '           BPOM ' , 
     &        BPOm
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '          ZIGHR ' , 
     &        ZIGhr
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '           BHAR ' , 
     &        BHAr
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '          ZIGHD ' , 
     &        ZIGhd
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '           BHAD ' , 
     &        BHAd
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)') '          ZIGT1 ' , 
     &        ZIGt1
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)') '          ZIGT2 ' , 
     &        ZIGt2
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,2E16.9)') '           ZIGL ' , 
     &        ZIGl
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)') '     ZIGDP(1-2) ' , 
     &        ZIGdp(1) , ZIGdp(2)
         IF ( LPRi.GT.4 ) WRITE (LO,'(A,4E16.9)') '     ZIGDP(3-4) ' , 
     &        ZIGdp(3) , ZIGdp(4)
      END IF
      END SUBROUTINE
