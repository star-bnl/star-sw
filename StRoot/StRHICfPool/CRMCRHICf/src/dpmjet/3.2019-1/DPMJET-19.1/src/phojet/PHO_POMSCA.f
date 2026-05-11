
      SUBROUTINE PHO_POMSCA(Ii,Mspom,Mhpom,Msreg,Ival1,Ival2,Mspar1,
     &                      Mspar2,Mhpar1,Mhpar2,Irej)
C**********************************************************************
C
C     parton orientated formulation of soft and hard inelastic events
C
C
C     input:    II        particle combiantion (1..4)
C               MSPOM     number of soft pomerons
C               MHPOM     number of semihard pomerons
C               MSREG     number of soft reggeons
C
C     output:   IVAL1,2   0 no valence quark engaged
C                         otherwise:  position of valence quark engaged
C                         neg.number: gluon connected to valence quark
C                                     by color flow
C               MSPAR1,2  number of realized soft partons
C               MHPAR1,2  number of realized hard partons
C               IREJ      1 failure
C                         0 success
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ahs , alnhs , DT_RNDM , emin , p1 , p2 , pd1 , 
     &                 ptmax , ptmx , q2h , sah , sas , TINY , valpro , 
     &                 wg1 , wgx , xhmax1 , xhmax2 , xisr1 , xisr2
      DOUBLE PRECISION xmax1 , xmax2 , xmaxh1 , xmaxh2 , xmaxp1 , 
     &                 xmaxp2 , xmaxx1 , xmaxx2 , xscut , xss1 , xss2 , 
     &                 xsss1 , xsss2 , xtmp1 , xtmp2 , xx , z1difs , 
     &                 z2difs
      INTEGER i , i1 , i2 , i3 , i4 , ifl1 , ifl2 , ihard , Ii , inmax , 
     &        iptm , iqua1 , iqua2 , Irej , itry , Ival1 , Ival2 , 
     &        ivg1 , ivg2 , ivglu1
      INTEGER ivglu2 , ivq1 , ivq2 , j , k , l , line , mhard , mhcha , 
     &        Mhpar1 , Mhpar2 , Mhpom , mscha , msdiff , msg1 , msg2 , 
     &        msm1 , msm2 , Mspar1 , Mspar2
      INTEGER Mspom , Msreg , n , nn , ntry
      SAVE 
 
      PARAMETER (TINY=1.D-30)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  general process information
      INCLUDE 'inc/poprcs'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  light-cone x fractions and c.m. momenta of soft cut string ends
      INCLUDE 'inc/posoft'
C  hard scattering data
      INCLUDE 'inc/pohslt'
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      DIMENSION p1(4) , p2(4) , pd1(-6:6)
 
      IF ( LPRi.GT.4 .AND. IDEb(24).GT.20 ) WRITE (LO,'(1X,A,3I5)')
     &      'PHO_POMSCA: MSPOM,MHPOM,MSREG' , Mspom , Mhpom , Msreg
 
      itry = 0
      ntry = 10
      Irej = 0
      inmax = 10
      mhard = Mhpom
 
C  phase space limitation (single hard valence-valence quark scattering)
      IF ( Mhpom.GT.0 ) THEN
         emin = 2.D0*PTWant + 0.2D0
         IF ( ECMp.LT.emin ) THEN
            IF ( LPRi.GT.4 .AND. IDEb(24).GE.1 )
     &            WRITE (LO,'(1X,A,1P,3E10.3)') 'PHO_POMSCA: ' , 
     &           'kin. rejection (1) (Ecm,Ptcut,Emin)' , ECMp , PTWant , 
     &           emin
            Irej = 50
            IFAil(6) = IFAil(6) + 1
            RETURN
         END IF
      END IF
 
      sas = PARmdl(160+Ii)/ECMp
      sah = 2.D0*PTWant/ECMp
      AS = sas**2
      AH = sah**2
 
C  save energy for leading particle effect
      xmaxp1 = 1.D0
      IF ( (IPAmdl(13).GT.0) .AND. (IHFls(1).NE.0) ) xmaxp1 = 1.D0 - 
     &     PARmdl(165)*XPSub
      xmaxp2 = 1.D0
      IF ( (IPAmdl(13).GT.0) .AND. (IHFls(2).NE.0) ) xmaxp2 = 1.D0 - 
     &     PARmdl(165)*XTSub
 
C
C  main loop to select hard and soft parton kinematics
C -----------------------------------------------------
      IFAil(31) = IFAil(31) + mhard
 100  Irej = 0
      ihard = 0
      LSC1hd = 0
      itry = itry + 1
      IF ( itry.GT.1 ) IFAil(5) = IFAil(5) + 1
      IF ( itry.GE.ntry ) THEN
         Irej = 1
         GOTO 500
      END IF
      line = 0
      LSCahd = 0
      IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) .AND. (IPRoce.EQ.1) )
     &     THEN
         xss1 = MAX(0.D0,1.D0-XPSub)
         xss2 = MAX(0.D0,1.D0-XTSub)
      ELSE
         xss1 = 0.D0
         xss2 = 0.D0
      END IF
 
C  partons needed to construct soft/hard interactions
 200  Mspar1 = 2*Mspom + Msreg + Mhpom
      Mspar2 = Mspar1
      Mhpar1 = Mhpom
      Mhpar2 = Mhpom
 
C  number of strings
      mscha = 2*Mspom + Msreg
      mhcha = 2*Mhpom
 
      KSOft = mscha
      KHArd = mhcha
 
C  check actual phase space limit
      xx = sas*DBLE(mscha) + sah*DBLE(mhcha)/2.D0
      IF ( xx.GE.1.D0 ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(24).GE.3 )
     &         WRITE (LO,'(1X,2A,/1X,4I3,1P4E12.4)')
     &         'PHO_POMSCA: internal kin. rejection ' , 
     &        '(MSpom,MHpom,MSchain,MHchain,Ecm,AS,AH,XX):' , Mspom , 
     &        Mhpom , mscha , mhcha , ECMp , AS , AH , xx
         IF ( Mspom+Msreg+Mhpom.GT.1 ) THEN
            IF ( Msreg.GT.0 ) THEN
               Msreg = Msreg - 1
            ELSE IF ( Mspom.GT.0 ) THEN
               Mspom = Mspom - 1
            ELSE IF ( Mhpom.GT.1 ) THEN
               Mhpom = Mhpom - 1
            END IF
            GOTO 200
         END IF
         IF ( LPRi.GT.4 .AND. IDEb(24).GE.1 )
     &         WRITE (LO,'(1X,A,1P2E10.3)')
     &         'PHO_POMSCA: kin. rejection (2) (Ecm,Ptcut)' , ECMp , 
     &        PTWant
         Irej = 50
         IFAil(6) = IFAil(6) + 1
         RETURN
      END IF
 
      xmaxx1 = MAX(TINY,1.D0-MIN(Mspar1,1)*AS-MIN(Mhpar1,1)*AH)
      xmaxx2 = MAX(TINY,1.D0-MIN(Mspar2,1)*AS-MIN(Mhpar2,1)*AH)
 
C  very low energy phase space restriction
      IF ( mhard.GT.0 ) THEN
         IF ( (xmaxx1*xmaxx2.LE.AH) ) THEN
            IF ( LPRi.GT.4 .AND. IDEb(24).GE.1 )
     &            WRITE (LO,'(1X,A,1P2E10.3)')
     &            'PHO_POMSCA: kin. rejection (3) (Ecm,Ptcut)' , ECMp , 
     &           PTWant
            Irej = 50
            IFAil(6) = IFAil(6) + 1
            RETURN
         END IF
      END IF
 
      AS = MAX(AS,PSOmin/PCMp)
      ALNs = LOG(AS)
      ALNh = LOG(AH)
      Z1Max = LOG(xmaxx1)
      Z2Max = LOG(xmaxx2)
      Z1Dif = Z1Max + Z2Max - ALNh
      Z2Dif = Z1Dif
      ptmax = 0.D0
C
C  select hard parton momenta
C ------------------- begin of inner loop -------------------
      IF ( IPOix3.EQ.0 ) IPOwgc(4+Ii) = 0
 
      IF ( mhard.GT.MSCAHD ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I3)') 'PHO_POMSCA: ' , 
     &        'no space left in /POHSLT/ (MHARD,MSCAHD):' , mhard , 
     &        MSCAHD
         Irej = 1
         RETURN
      END IF
 
      DO nn = 1 , mhard
C
C  generate one resolved hard scattering
C
C  high-pt option
         IF ( (nn.EQ.1) .AND. (Ii.EQ.1) .AND. (HSWcut(4+Ii).GT.PTWant) )
     &        THEN
            CALL PHO_HARINT(-1,ECMp,PVIrtp(1),PVIrtp(2),-1,MAX_PRO_2,1,
     &                      4,Mspom+Mhpom)
            xscut = HSIg(9,IDXmpar)
            ahs = AH
            alnhs = ALNh
            z1difs = Z1Dif
            z2difs = Z2Dif
            AH = (2.D0*PTWant/ECMp)**2
            ALNh = LOG(AH)
            Z1Dif = Z1Max + Z2Max - ALNh
            Z2Dif = Z1Dif
            IF ( (Z1Dif.LE.0.01D0) .OR. (Z2Dif.LE.0.01D0) ) THEN
               IF ( LPRi.GT.4 .AND. IDEb(24).GE.1 )
     &               WRITE (LO,'(1X,2A,/1X,1P4E12.3)')
     &               'PHO_POMSCA: kin.rejection, high-pt option ' , 
     &              '(Z1/2max,ALNH,Z1dif):' , Z1Max , Z2Max , ALNh , 
     &              Z1Dif
               Irej = 5
               RETURN
            END IF
            CALL PHO_HARSCA(2,Ii)
            CALL PHO_HARINT(1,ECMp,PVIrtp(1),PVIrtp(2),-1,MAX_PRO_2,1,4,
     &                      Mspom+Mhpom)
            AH = ahs
            ALNh = alnhs
            Z1Dif = z1difs
            Z2Dif = z2difs
            IPOwgc(4+Ii) = IPOwgc(4+Ii) + 1
            HSWght(4+Ii) = xscut/HSIg(9,IDXmpar)*DBLE(mhard)
C  minimum bias option
         ELSE
            CALL PHO_HARSCA(2,Ii)
         END IF
 
C  fill /POHSLT/
         LSIdx(nn) = nn
         LSCahd = nn
         XHD(nn,1) = X1
         XHD(nn,2) = X2
         X0Hd(nn,1) = X1
         X0Hd(nn,2) = X2
         VHD(nn) = V
         ETAhd(nn,1) = ETAc
         ETAhd(nn,2) = ETAd
         PTHd(nn) = PT
         NPRohd(nn) = MSPr
         Q2Sca(nn,1) = QQPd
         Q2Sca(nn,2) = QQPd
         PDFva(nn,1) = PDF1(IA)
         PDFva(nn,2) = PDF2(IB)
         NINhd(nn,1) = IA
         NINhd(nn,2) = IB
         N0Inhd(nn,1) = IA
         N0Inhd(nn,2) = IB
         NIVal(nn,1) = IV1
         NIVal(nn,2) = IV2
         N0Ival(nn,1) = IV1
         N0Ival(nn,2) = IV2
         NOUthd(nn,1) = IC
         NOUthd(nn,2) = ID
         NBRahd(nn,1) = IDPdg1
         NBRahd(nn,2) = IDPdg2
         i3 = 8*(nn-1)
         i4 = 8*(nn-1) + 4
         DO i = 1 , 4
            PPH(i3+i,1) = PHI1(i)
            PPH(i3+i,2) = PHI2(i)
            PPH(i4+i,1) = PHO1(i)
            PPH(i4+i,2) = PHO2(i)
         END DO
 
      END DO
 
C  sort according to pt-hat
      DO nn = 1 , mhard
         ptmx = PTHd(LSIdx(nn))
         iptm = nn
         DO i = nn + 1 , mhard
            IF ( PTHd(LSIdx(i)).GT.ptmx ) THEN
               iptm = i
               ptmx = PTHd(LSIdx(i))
            END IF
         END DO
         IF ( iptm.NE.nn ) CALL PHO_SWAPI(LSIdx(nn),LSIdx(iptm))
      END DO
      iptm = LSIdx(1)
 
C  copy partons, generate ISR
      DO l = 1 , mhard
         nn = LSIdx(l)
         xsss1 = xss1 + XHD(nn,1)
         xsss2 = xss2 + XHD(nn,2)
C  debug output
         IF ( LPRi.GT.4 .AND. IDEb(24).GE.10 )
     &         WRITE (LO,'(1X,A,3I4,1P,3E11.3)')
     &         'PHO_POMSCA: NR,LSIDX,MSPR,X1,X2,PT' , l , nn , 
     &        NPRohd(nn) , XHD(nn,1) , XHD(nn,2) , PTHd(nn)
C  check phase space
         IF ( (xsss1.GT.xmaxx1) .OR. (xsss2.GT.xmaxx2) .OR. 
     &        ((1.D0-xsss1)*(1.D0-xsss2).LT.AS) ) THEN
            IF ( ihard.EQ.0 ) THEN
               IF ( ISWmdl(2).NE.1 ) GOTO 100
               Mhpom = 0
               Mspom = 1
               Msreg = 0
            END IF
            GOTO 300
         END IF
 
C  reweight according to photon virtuality
         IF ( IPAmdl(115).GE.1 ) THEN
            QQPd = Q2Sca(nn,1)
            wgx = 1.D0
            IF ( IDPdg1.EQ.22 ) THEN
               IF ( IPAmdl(115).EQ.1 ) THEN
                  IF ( QQPd.LT.PVIrtp(1)+PARmdl(144) ) THEN
                     wg1 = 0.D0
                  ELSE
                     wg1 = LOG(QQPd/(PVIrtp(1)+PARmdl(144)))
     &                     /LOG(QQPd/PARmdl(144))
                  END IF
                  IF ( NINhd(nn,1).EQ.0 ) wg1 = wg1*wg1
               ELSE IF ( IPAmdl(115).EQ.2 ) THEN
                  CALL PHO_PDF(1,X0Hd(nn,1),QQPd,PVIrtp(1),pd1)
                  wg1 = pd1(NINhd(nn,1))/PDFva(nn,1)
               END IF
               wgx = wg1
            END IF
            QQPd = Q2Sca(nn,2)
            IF ( IDPdg2.EQ.22 ) THEN
               IF ( IPAmdl(115).EQ.1 ) THEN
                  IF ( QQPd.LT.PVIrtp(2)+PARmdl(144) ) THEN
                     wg1 = 0.D0
                  ELSE
                     wg1 = LOG(QQPd/(PVIrtp(2)+PARmdl(144)))
     &                     /LOG(QQPd/PARmdl(144))
                  END IF
                  IF ( NINhd(nn,2).EQ.0 ) wg1 = wg1*wg1
               ELSE IF ( IPAmdl(115).EQ.2 ) THEN
                  CALL PHO_PDF(2,X0Hd(nn,2),QQPd,PVIrtp(2),pd1)
                  wg1 = pd1(NINhd(nn,2))/PDFva(nn,2)
               END IF
               wgx = wgx*wg1
            END IF
 
            IF ( IDEb(24).GE.25 .AND. LPRi.GT.4 )
     &            WRITE (LO,'(1X,2A,/5X,I10,I3,1P6E10.3)')
     &            'PHO_POMSCA: ' , 
     &           ' re-weight with (EVE, MSPR, X1/2, Q2, PV1/2, W1/W2)' , 
     &           KEVent , MSPr , X0Hd(nn,1) , X0Hd(nn,2) , QQPd , 
     &           PVIrtp , wgx
 
            IF ( wgx.LT.DT_RNDM(wgx) ) THEN
               IF ( l.NE.1 ) GOTO 300
               Irej = 50
               RETURN
            END IF
 
            IF ( LPRi.GT.4 .AND. wgx.GT.1.D0 )
     &            WRITE (LO,'(1X,2A,/5X,I10,I3,1P6E10.3)')
     &            'PHO_POMSCA: ' , 
     &           'weight >1 (EVE, MSPR, X1/2, Q2, PV1/2, W1/W2)' , 
     &           KEVent , MSPr , X0Hd(nn,1) , X0Hd(nn,2) , QQPd , 
     &           PVIrtp , wgx
 
         END IF
 
C  generate ISR
         IF ( (ISWmdl(8).GE.2) .AND. ((IPAmdl(101).NE.1) .OR. (l.EQ.1))
     &        ) THEN
            IF ( IPAmdl(109).EQ.1 ) THEN
               q2h = PARmdl(93)*PTHd(nn)**2
            ELSE
               q2h = -PARmdl(93)*VHD(nn)*XHD(nn,1)*XHD(nn,2)*ECMp*ECMp
            END IF
            xhmax1 = 1.D0 - xsss1 - mscha*AS + XHD(nn,1)
            xhmax2 = 1.D0 - xsss2 - mscha*AS + XHD(nn,2)
            i3 = 8*nn - 4
            DO j = 1 , 4
               p1(j) = PPH(i3+j,1)
               p2(j) = PPH(i3+j,2)
            END DO
            IF ( IDEb(24).GE.10 .AND. LPRi.GT.4 )
     &            WRITE (LO,'(1X,A,/5X,2I3,1P,3E12.4)')
     &            'PHO_POMSCA: generate ISR for (L,NN,X1,X2,Q2H)' , l , 
     &           nn , XHD(nn,1) , XHD(nn,2) , q2h
            j = nn
            IF ( l.EQ.1 ) j = -nn
            CALL PHO_HARISR(j,p1,p2,NOUthd(nn,1),NOUthd(nn,2),
     &                      N0Inhd(nn,1),N0Inhd(nn,2),N0Ival(nn,1),
     &                      N0Ival(nn,2),q2h,X0Hd(nn,1),X0Hd(nn,2),
     &                      xhmax1,xhmax2,ifl1,ifl2,NIVal(nn,1),
     &                      NIVal(nn,2),xisr1,xisr2,Irej)
            xsss1 = xsss1 + xisr1 - XHD(nn,1)
            xsss2 = xsss2 + xisr2 - XHD(nn,2)
            NINhd(nn,1) = ifl1
            NINhd(nn,2) = ifl2
            XHD(nn,1) = xisr1
            XHD(nn,2) = xisr2
         END IF
 
C  check phase space
         IF ( (xsss1.GT.xmaxx1) .OR. (xsss2.GT.xmaxx2) .OR. 
     &        ((1.D0-xsss1)*(1.D0-xsss2).LT.AS) ) THEN
            IF ( ihard.EQ.0 ) THEN
               IF ( ISWmdl(2).NE.1 ) GOTO 100
               Mhpom = 0
               Mspom = 1
               Msreg = 0
            END IF
            GOTO 300
         END IF
 
C  leave energy for leading particle effect
         IF ( (ihard.GT.0) .AND. 
     &        ((xsss1.GT.xmaxp1) .OR. (xsss2.GT.xmaxp2)) ) GOTO 300
 
C  hard scattering accepted
         ihard = ihard + 1
         xss1 = xsss1
         xss2 = xsss2
         IFAil(31) = IFAil(31) - 1
 
      END DO
 
C ------------------- end of inner (hard) loop -------------------
 
 300  Mhpom = ihard
      Mhpar1 = ihard
      Mhpar2 = ihard
 
C  count valences involved in hard scattering
      Ival1 = 0
      Ival2 = 0
      DO l = 1 , ihard
         nn = LSIdx(l)
         IF ( (NIVal(nn,1).NE.0) .AND. (Ival1.EQ.0) ) Ival1 = nn
         IF ( (NIVal(nn,2).NE.0) .AND. (Ival2.EQ.0) ) Ival2 = nn
      END DO
 
      iqua1 = 0
      iqua2 = 0
      ivglu1 = 0
      ivglu2 = 0
      DO l = 1 , ihard
         nn = LSIdx(l)
 
C  photon, pomeron valences
         IF ( (Ival1.EQ.0) .AND. (NINhd(nn,1).NE.0) ) THEN
            IF ( (IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990) ) THEN
               NIVal(nn,1) = 1
               Ival1 = nn
            END IF
         END IF
         IF ( (Ival2.EQ.0) .AND. (NINhd(nn,2).NE.0) ) THEN
            IF ( (IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990) ) THEN
               NIVal(nn,2) = 1
               Ival2 = nn
            END IF
         END IF
 
C  total number of quarks
         IF ( NINhd(nn,1).NE.0 ) THEN
            iqua1 = iqua1 + 1
         ELSE IF ( ivglu1.EQ.0 ) THEN
            ivglu1 = nn
         END IF
         IF ( NINhd(nn,2).NE.0 ) THEN
            iqua2 = iqua2 + 1
         ELSE IF ( ivglu2.EQ.0 ) THEN
            ivglu2 = nn
         END IF
      END DO
 
C  gluons emitted by valence quarks
      valpro = 1.D0
      IF ( Ii.EQ.1 ) valpro = VALprg(1)
      ivq1 = 1
      ivg1 = 0
      Ival1 = MAX(Ival1,0)
      IF ( Ival1.EQ.0 ) THEN
         ivq1 = 0
         IF ( (ivglu1.NE.0) .AND. (DT_RNDM(xss1).LT.valpro) ) THEN
            Ival1 = -ivglu1
            ivg1 = 1
         END IF
      END IF
      valpro = 1.D0
      IF ( Ii.EQ.1 ) valpro = VALprg(2)
      ivq2 = 1
      ivg2 = 0
      Ival2 = MAX(Ival2,0)
      IF ( Ival2.EQ.0 ) THEN
         ivq2 = 0
         IF ( (ivglu2.NE.0) .AND. (DT_RNDM(xss2).LT.valpro) ) THEN
            Ival2 = -ivglu2
            ivg2 = 1
         END IF
      END IF
      Mspom = MAX(0,Mspom-iqua1-iqua2)
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(24).GE.5 ) WRITE (LO,'(1X,A,6I4)')
     &      'PHO_POMSCA: IVAL1/2,IQUA1/2,IVGLU1/2' , Ival1 , Ival2 , 
     &     iqua1 , iqua2 , ivglu1 , ivglu2
 
C  select soft X values
C  number of soft/remnant quarks
 400  IF ( Mspom.EQ.0 ) THEN
         IF ( IPAmdl(18).EQ.0 ) THEN
            Mspar1 = 2 + 2*Mhpom + Msreg - iqua1 - 2*ivq1 - 2*ivg1
            Mspar2 = 2 + 2*Mhpom + Msreg - iqua2 - 2*ivq2 - 2*ivg2
         ELSE
            Mspar1 = 2 + Msreg + iqua1 - 2*ivq1
            Mspar2 = 2 + Msreg + iqua2 - 2*ivq2
         END IF
      ELSE IF ( IPAmdl(18).EQ.0 ) THEN
         Mspar1 = 2*Mspom + Msreg + 2*Mhpom - iqua1
         Mspar2 = 2*Mspom + Msreg + 2*Mhpom - iqua2
      ELSE
         Mspar1 = 2*Mspom + Msreg + iqua1 + 2*ivg1
         Mspar2 = 2*Mspom + Msreg + iqua2 + 2*ivg2
      END IF
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(24).GE.15 ) WRITE (LO,'(1X,A,9I3)')
     &      'PHO_POMSCA: MSP,MSR,MHP,IVQ1/2,IVG1/2,MSPAR1/2' , Mspom , 
     &     Msreg , Mhpom , ivq1 , ivq2 , ivg1 , ivg2 , Mspar1 , Mspar2
 
      xmax1 = 1.D0 - MAX(Mspar1-1,0)*AS - xss1
      xmax2 = 1.D0 - MAX(Mspar2-1,0)*AS - xss2
      i1 = ivq1
      i2 = ivq2
      IF ( Ival1.LE.0 ) i1 = 0
      IF ( Ival2.LE.0 ) i2 = 0
      IF ( (ivq1+ivg1)*(ivq2+ivg2).NE.0 ) THEN
         msdiff = 2*Mspom
      ELSE
         msdiff = 2*MAX(0,Mspom-1)
      END IF
      msg1 = Mspar1
      msg2 = Mspar2
      msm1 = Mspar1 - msdiff
      msm2 = Mspar2 - msdiff
      xmaxh1 = MIN(xmax1,PARmdl(44))
      xmaxh2 = MIN(xmax2,PARmdl(44))
      CALL PHO_SOFTXX(NPOsp(1),NPOsp(2),msg1,msg2,i1,i2,msm1,msm2,xss1,
     &                xss2,xmaxh1,xmaxh2,XS1,XS2,Irej)
 
C  correct for proper simulation of high pt tail
      IF ( Irej.NE.0 ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(48).GE.2 ) WRITE (LO,'(1X,A,4I4)')
     &         'PHO_STDPAR: rejection (PHO_SOFTXX): MSPOM,MHPOM,I1,I2' , 
     &        Mspom , Mhpom , i1 , i2
         IF ( Mspom*Mhpom.GT.0 ) THEN
            Mspom = Mspom - 1
            GOTO 400
         ELSE IF ( Mspom.GT.1 ) THEN
            Mspom = Mspom - 1
            GOTO 400
         ELSE IF ( Mhpom.GT.1 ) THEN
            ihard = ihard - 1
            IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) .AND. 
     &           (IPRoce.EQ.1) ) THEN
               xss1 = MAX(0.D0,1.D0-XPSub)
               xss2 = MAX(0.D0,1.D0-XTSub)
            ELSE
               xss1 = 0.D0
               xss2 = 0.D0
            END IF
            DO k = 1 , ihard
               i = LSIdx(k)
               xss1 = xss1 + XHD(i,1)
               xss2 = xss2 + XHD(i,2)
            END DO
            GOTO 300
         END IF
         Irej = 4
         GOTO 500
      END IF
C  accepted
      Mspom = Mspom - (Mspar1-msg1)/2
      Mspar1 = msg1
      Mspar2 = msg2
C  ------------ kinematics sampled ---------------
C  debug output
      IF ( IDEb(24).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_POMSCA: soft x values, ITRY' , itry
         DO i = 2 , MAX(Mspar1,Mspar2)
            IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,2E12.3)') i , XS1(i) , 
     &           XS2(i)
         END DO
      END IF
      IF ( (1.D0-xss1)*(1.D0-xss2).LT.AS ) GOTO 100
 
C  end of loop
      XS1(1) = 1.D0 - xss1
      XS2(1) = 1.D0 - xss2
 
C  process counting
      DO n = 1 , LSCahd
         MH_acc_1(NPRohd(n),Ii,IDXmpar) = MH_acc_1(NPRohd(n),Ii,IDXmpar)
     &      + 1
      END DO
 
C  soft particle momenta
 
      IF ( MAX(Mspar1,Mspar2).GT.MAXSOF ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,3I4)')
     &         'PHO_POMSCA: no space left in ' , 
     &        '/POSOFT/ (MSPAR1/2,MAXSOF):' , Mspar1 , Mspar2 , MAXSOF
         Irej = 1
         RETURN
      END IF
 
      DO i = 1 , Mspar1
         PSOft1(1,i) = 0.D0
         PSOft1(2,i) = 0.D0
         PSOft1(3,i) = XS1(i)*ECMp/2.D0
         PSOft1(4,i) = XS1(i)*ECMp/2.D0
      END DO
      DO i = 1 , Mspar2
         PSOft2(1,i) = 0.D0
         PSOft2(2,i) = 0.D0
         PSOft2(3,i) = -XS2(i)*ECMp/2.D0
         PSOft2(4,i) = XS2(i)*ECMp/2.D0
      END DO
 
      KSOft = MAX(Mspar1,Mspar2)
      KHArd = MAX(Mhpar1,Mhpar2)
      KSPom = Mspom
      KSReg = Msreg
      KHPom = Mhpom
 
C  debug output
      IF ( IDEb(24).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I3,2I5)')
     &         'PHO_POMSCA: accepted IVAL1,IVAL2,ITRY,NTRY' , Ival1 , 
     &        Ival2 , itry , ntry
         IF ( Mspar1+Mspar2.GT.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)')
     &            'soft x particle1   particle2:'
            xtmp1 = 0.D0
            xtmp2 = 0.D0
            DO i = 1 , MAX(Mspar1,Mspar2)
               IF ( i.LE.MIN(Mspar1,Mspar2) ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,2E13.4)') i , 
     &                 XS1(i) , XS2(i)
                  xtmp1 = xtmp1 + XS1(i)
                  xtmp2 = xtmp2 + XS2(i)
               ELSE IF ( i.LE.Mspar1 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,2E13.4)') i , 
     &                 XS1(i) , 0.D0
                  xtmp1 = xtmp1 + XS1(i)
               ELSE IF ( i.LE.Mspar2 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,2E13.4)') i , 
     &                 0.D0 , XS2(i)
                  xtmp2 = xtmp2 + XS2(i)
               END IF
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2E13.4)')
     &            'sum X1/2 (soft):' , xtmp1 , xtmp2
         END IF
         IF ( Mhpar1.GT.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)') 
     &        'NR  IDX  MSPR hard X / hard X ISR / flavor particle 1,2:'
            DO k = 1 , Mhpar1
               i = LSIdx(k)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,3I3,4E12.3,2I3)') k , i , 
     &              NPRohd(i) , X0Hd(i,1) , X0Hd(i,2) , XHD(i,1) , 
     &              XHD(i,2) , NINhd(i,1) , NINhd(i,2)
               xtmp1 = xtmp1 + XHD(i,1)
               xtmp2 = xtmp2 + XHD(i,2)
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2E13.4)')
     &            'sum X1/2 (soft+hard):' , xtmp1 , xtmp2
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)')
     &            'hard momenta  particle1:'
            DO k = 1 , Mhpar1
               i = LSIdx(k)
               i3 = 8*i - 4
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,2I3,1P,4E12.3,I5)') k , 
     &              i , (PPH(i3+l,1),l=1,4) , NOUthd(i,1)
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)')
     &            'hard momenta  particle2:'
            DO k = 1 , Mhpar2
               i = LSIdx(k)
               i3 = 8*i - 4
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,2I3,1P,4E12.3,I5)') k , 
     &              i , (PPH(i3+l,2),l=1,4) , NOUthd(i,2)
            END DO
         END IF
      END IF
      RETURN
 
C  event rejected, print debug information
 500  IFAil(4) = IFAil(4) + 1
      IF ( IDEb(24).GE.2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/,10X,7I5)')
     &        'PHO_POMSCA: ' , 
     &        'rejection (MSPOM,MHPOM,IHARD,MHARD,ITRY,NTRY,IREJ)' , 
     &        Mspom , Mhpom , ihard , mhard , itry , ntry , Irej
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4,1P,2E12.4)')
     &         'IP,Ecm,PTcut:' , Ii , ECMp , PTWant
         IF ( IDEb(24).GE.5 ) THEN
            CALL PHO_PREVNT(0)
         ELSE
            CALL PHO_PREVNT(-1)
         END IF
      END IF
 
      END SUBROUTINE
