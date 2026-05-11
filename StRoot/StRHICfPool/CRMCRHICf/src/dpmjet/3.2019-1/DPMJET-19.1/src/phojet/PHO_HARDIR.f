
      SUBROUTINE PHO_HARDIR(Ii,Ival1,Ival2,Mspar1,Mspar2,Mhpar1,Mhpar2,
     &                      Irej)
C**********************************************************************
C
C     parton orientated formulation of direct scattering processes
C
C     input:
C
C     output:   II        particle combination (1..4)
C               IVAL1,2   0 no valence quarks engaged
C                         1 valence quarks engaged
C               MSPAR1,2  number of realized soft partons
C               MHPAR1,2  number of realized hard partons
C               IREJ      1 failure
C                         0 success
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , p1 , p2 , pd1 , q2h , TINY , wgx , 
     &                 xhmax1 , xhmax2 , xisr1 , xisr2 , xmax , xmaxh , 
     &                 xmaxx , xss1 , xss2
      INTEGER i , ifl1 , ifl2 , Ii , Irej , itry , Ival1 , Ival2 , j , 
     &        k , line , Mhpar1 , Mhpar2 , Mspar1 , Mspar2 , ntry
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  light-cone x fractions and c.m. momenta of soft cut string ends
      INCLUDE 'inc/posoft'
C  hard scattering data
      INCLUDE 'inc/pohslt'
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      DIMENSION p1(4) , p2(4) , pd1(-6:6)
 
      PARAMETER (TINY=1.D-10)
 
      itry = 0
      ntry = 10
      LSC1hd = 0
      LSIdx(1) = 1
 
C  check phase space
      IF ( ECMp.LT.(2.D0*PTWant+0.1D0) ) THEN
         IFAil(18) = IFAil(18) + 1
         Irej = 50
         RETURN
      END IF
 
      AS = (PARmdl(160+Ii)/ECMp)**2
      AH = (2.D0*PTWant/ECMp)**2
 
      ALNs = LOG(AS)
      ALNh = LOG(AH)
 
      xmax = MAX(TINY,1.D0-AS)
      Z1Max = LOG(xmax)
      Z1Dif = Z1Max - ALNh
C
C  main loop to select hard and soft parton kinematics
C -----------------------------------------------------
 100  Irej = 0
      itry = itry + 1
      LSC1hd = LSC1hd + 1
      IF ( itry.GT.1 ) THEN
         IFAil(17) = IFAil(17) + 1
         IF ( itry.GE.ntry ) THEN
            Irej = 1
 
            IFAil(16) = IFAil(16) + 1
            IF ( IDEb(25).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I5)')
     &               'PHO_HARDIR: rejection (ITRY,NTRY,IREJ)' , itry , 
     &              ntry , Irej
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.4)')
     &               'available energy:' , ECMp
               IF ( IDEb(25).GE.5 ) THEN
                  CALL PHO_PREVNT(0)
               ELSE
                  CALL PHO_PREVNT(-1)
               END IF
            END IF
            GOTO 99999
         END IF
      END IF
      line = 0
      LSCahd = 0
      xss1 = 0.D0
      xss2 = 0.D0
      Mspar1 = 0
      Mspar2 = 0
 
C  select hard V,X
      CALL PHO_HARSCA(1,Ii)
      xss1 = xss1 + X1
      xss2 = xss2 + X2
C  debug output
      IF ( IDEb(25).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2E12.4,2I5)')
     &         'PHO_HARDIR: AS,XMAX,process ID,ITRY' , AS , xmax , 
     &        MSPr , itry
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,4E12.4)')
     &         'HARD X1,2  SUM X1,2' , X1 , X2 , xss1 , xss2
      END IF
 
      IF ( MSPr.LE.11 ) THEN
         IF ( (xss2.GT.xmax) .OR. ((1.D0-xss2).LT.AS) ) GOTO 100
      ELSE IF ( MSPr.LE.13 ) THEN
         IF ( (xss1.GT.xmax) .OR. ((1.D0-xss1).LT.AS) ) GOTO 100
      END IF
 
C  fill /POHSLT/
      LSCahd = 1
      LSIdx(1) = 1
      XHD(1,1) = X1
      XHD(1,2) = X2
      X0Hd(1,1) = X1
      X0Hd(1,2) = X2
      VHD(1) = V
      ETAhd(1,1) = ETAc
      ETAhd(1,2) = ETAd
      PTHd(1) = PT
      Q2Sca(1,1) = QQPd
      Q2Sca(1,2) = QQPd
      NPRohd(1) = MSPr
      NBRahd(1,1) = IDPdg1
      NBRahd(1,2) = IDPdg2
      DO i = 1 , 4
         PPH(i,1) = PHI1(i)
         PPH(i,2) = PHI2(i)
         PPH(4+i,1) = PHO1(i)
         PPH(4+i,2) = PHO2(i)
      END DO
C  valence quarks
      Ival1 = IV1
      Ival2 = IV2
      PDFva(1,1) = 0.D0
      PDFva(1,2) = 0.D0
C  parton flavours
      IF ( MSPr.LE.11 ) THEN
         NINhd(1,1) = IDPdg1
         NINhd(1,2) = IB
         PDFva(1,2) = PDF2(IB)
         KHDir = 1
      ELSE IF ( MSPr.LE.13 ) THEN
         NINhd(1,1) = IA
         PDFva(1,1) = PDF1(IA)
         NINhd(1,2) = IDPdg2
         KHDir = 2
      ELSE
         NINhd(1,1) = IDPdg1
         NINhd(1,2) = IDPdg2
         KHDir = 3
      END IF
      N0Inhd(1,1) = NINhd(1,1)
      N0Inhd(1,2) = NINhd(1,2)
      N0Ival(1,1) = Ival1
      N0Ival(1,2) = Ival2
      NOUthd(1,1) = IC
      NOUthd(1,2) = ID
 
C  reweight according to photon virtuality
      IF ( MSPr.NE.14 ) THEN
         IF ( IPAmdl(115).GE.1 ) THEN
            wgx = 1.D0
            IF ( ((MSPr.EQ.10) .OR. (MSPr.EQ.11)) .AND. (IDPdg2.EQ.22) )
     &           THEN
               QQPd = Q2Sca(1,2)
               IF ( IPAmdl(115).EQ.1 ) THEN
                  IF ( QQPd.LT.(PVIrtp(2)+PARmdl(144)) ) THEN
                     wgx = 0.D0
                  ELSE
                     wgx = LOG(QQPd/(PVIrtp(2)+PARmdl(144)))
     &                     /LOG(QQPd/PARmdl(144))
                  END IF
                  IF ( NINhd(1,2).EQ.0 ) wgx = wgx*wgx
               ELSE IF ( IPAmdl(115).EQ.2 ) THEN
                  CALL PHO_PDF(2,X2,QQPd,PVIrtp(2),pd1)
                  wgx = pd1(IB)/PDFva(1,2)
               END IF
            ELSE IF ( ((MSPr.EQ.12) .OR. (MSPr.EQ.13)) .AND. 
     &                (IDPdg1.EQ.22) ) THEN
               QQPd = Q2Sca(1,1)
               IF ( IPAmdl(115).EQ.1 ) THEN
                  IF ( QQPd.LT.(PVIrtp(1)+PARmdl(144)) ) THEN
                     wgx = 0.D0
                  ELSE
                     wgx = LOG(QQPd/(PVIrtp(1)+PARmdl(144)))
     &                     /LOG(QQPd/PARmdl(144))
                  END IF
                  IF ( NINhd(1,1).EQ.0 ) wgx = wgx*wgx
               ELSE IF ( IPAmdl(115).EQ.2 ) THEN
                  CALL PHO_PDF(1,X1,QQPd,PVIrtp(1),pd1)
                  wgx = pd1(IA)/PDFva(1,1)
               END IF
            END IF
 
            IF ( IDEb(25).GE.25 .AND. LPRi.GT.4 )
     &            WRITE (LO,'(1X,2A,/5X,I10,I3,1P6E10.3)')
     &            'PHO_HARDIR: ' , 
     &           're-weight with (EVE, MSPR, X1/2, Q2, PV1/2, W1/W2)' , 
     &           KEVent , MSPr , X1 , X2 , QQPd , PVIrtp , wgx
 
            IF ( wgx.LT.DT_RNDM(wgx) ) THEN
               Irej = 50
               RETURN
            END IF
 
            IF ( wgx.GT.1.01D0 .AND. LPRi.GT.4 )
     &            WRITE (LO,'(1X,2A,/5X,I10,I3,1P6E10.3)')
     &            'PHO_HARDIR: ' , 
     &           're-weight >1 (EVE, MSPR, X1/2, Q2, PV1/2, W1/W2)' , 
     &           KEVent , MSPr , X1 , X2 , QQPd , PVIrtp , wgx
 
         END IF
      END IF
 
C  generate ISR
      IF ( (MSPr.NE.14) .AND. (ISWmdl(8).GE.2) ) THEN
         IF ( IPAmdl(109).EQ.1 ) THEN
            q2h = PARmdl(93)*PT**2
         ELSE
            q2h = -PARmdl(93)*VHD(1)*XHD(1,1)*XHD(1,2)*ECMp*ECMp
         END IF
         xhmax1 = 1.D0 - xss1 - AS + XHD(1,1)
         xhmax2 = 1.D0 - xss2 - AS + XHD(1,2)
         DO j = 1 , 4
            p1(j) = PPH(4+j,1)
            p2(j) = PPH(4+j,2)
         END DO
         CALL PHO_HARISR(-1,p1,p2,NOUthd(1,1),NOUthd(1,2),N0Inhd(1,1),
     &                   N0Inhd(1,2),N0Ival(1,1),N0Ival(1,2),q2h,
     &                   X0Hd(1,1),X0Hd(1,2),xhmax1,xhmax2,ifl1,ifl2,
     &                   Ival1,Ival2,xisr1,xisr2,Irej)
         xss1 = xss1 + xisr1 - XHD(1,1)
         xss2 = xss2 + xisr2 - XHD(1,2)
         NINhd(1,1) = ifl1
         NINhd(1,2) = ifl2
         XHD(1,1) = xisr1
         XHD(1,2) = xisr2
      ELSE
         ifl1 = NINhd(1,1)
         ifl2 = NINhd(1,2)
      END IF
      NIVal(1,1) = Ival1
      NIVal(1,2) = Ival2
 
C  add photon/hadron remnant
 
C  incoming gluon
      IF ( ifl2.EQ.0 ) THEN
         xmaxx = 1.D0 - xss2 - AS
         xmaxh = MIN(xmaxx,PARmdl(44))
         CALL PHO_HADSP2(IDBam2,xss2,xmaxh,XS2,Irej)
         Ival2 = 1
         Mspar1 = 0
         Mspar2 = 2
         Mhpar1 = 1
         Mhpar2 = 1
      ELSE IF ( ifl1.EQ.0 ) THEN
         xmaxx = 1.D0 - xss1 - AS
         xmaxh = MIN(xmaxx,PARmdl(44))
         CALL PHO_HADSP2(IDBam1,xss1,xmaxh,XS1,Irej)
         Ival1 = 1
         Mspar1 = 2
         Mspar2 = 0
         Mhpar1 = 1
         Mhpar2 = 1
 
C  incoming quark
      ELSE IF ( ABS(ifl2).LE.12 ) THEN
         IF ( Ival2.EQ.1 ) THEN
            XS2(1) = 1.D0 - xss2
            Mspar1 = 0
            Mspar2 = 1
            Mhpar1 = 1
            Mhpar2 = 1
         ELSE
            xmaxx = 1.D0 - xss2 - AS
            xmaxh = MIN(xmaxx,PARmdl(44))
            CALL PHO_HADSP3(IDBam2,xss2,xmaxh,XS2,Irej)
            Mspar1 = 0
            Mspar2 = 3
            Mhpar1 = 1
            Mhpar2 = 1
         END IF
      ELSE IF ( ABS(ifl1).LE.12 ) THEN
         IF ( Ival1.EQ.1 ) THEN
            XS1(1) = 1.D0 - xss1
            Mspar1 = 1
            Mspar2 = 0
            Mhpar1 = 1
            Mhpar2 = 1
         ELSE
            xmaxx = 1.D0 - xss1 - AS
            xmaxh = MIN(xmaxx,PARmdl(44))
            CALL PHO_HADSP3(IDBam1,xss1,xmaxh,XS1,Irej)
            Mspar1 = 3
            Mspar2 = 0
            Mhpar1 = 1
            Mhpar2 = 1
         END IF
 
C  double direct process
      ELSE IF ( MSPr.EQ.14 ) THEN
         Mspar1 = 0
         Mspar2 = 0
         Mhpar1 = 1
         Mhpar2 = 1
 
C  unknown process
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3/)')
     &         'PHO_HARDIR:ERROR: unsupported hard process (MSPR)' , 
     &        MSPr
         CALL PHO_ABORT
      END IF
 
      IF ( Irej.NE.0 ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(25).GE.3 ) WRITE (LO,'(1X,A,3I5)')
     &         'PHO_HARDIR: int. rejection (MSPR,ITRY,NTRY)' , MSPr , 
     &        itry , ntry
         GOTO 100
      END IF
 
C  soft particle momenta
      IF ( Mspar1.GT.0 ) THEN
         DO i = 1 , Mspar1
            PSOft1(1,i) = 0.D0
            PSOft1(2,i) = 0.D0
            PSOft1(3,i) = XS1(i)*ECMp/2.D0
            PSOft1(4,i) = XS1(i)*ECMp/2.D0
         END DO
      END IF
      IF ( Mspar2.GT.0 ) THEN
         DO i = 1 , Mspar2
            PSOft2(1,i) = 0.D0
            PSOft2(2,i) = 0.D0
            PSOft2(3,i) = -XS2(i)*ECMp/2.D0
            PSOft2(4,i) = XS2(i)*ECMp/2.D0
         END DO
      END IF
C  process counting
      MH_acc_1(MSPr,Ii,IDXmpar) = MH_acc_1(MSPr,Ii,IDXmpar) + 1
      KSOft = MAX(Mspar1,Mspar2)
      KHArd = MAX(Mhpar1,Mhpar2)
C  debug output
      IF ( IDEb(25).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I3,3I5)')
     &         'PHO_HARDIR: accepted IVAL1,IVAL2,MSPR,ITRY,NTRY' , 
     &        Ival1 , Ival2 , MSPr , itry , ntry
         IF ( Mspar1.GT.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4)')
     &           'soft x particle 1:' , Mspar1
            DO i = 1 , Mspar1
               IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,E12.3)') i , XS1(i)
            END DO
         END IF
         IF ( Mspar2.GT.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4)')
     &           'soft x particle 2:' , Mspar2
            DO i = 1 , Mspar2
               IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,E12.3)') i , XS2(i)
            END DO
         END IF
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4)')
     &         'ini.hard X/flavor particle 1:' , Mhpar1
         IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,E12.3,I8)') 1 , XHD(1,1) , 
     &        NINhd(1,1)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4)')
     &         'fin.hard momenta  particle 1:' , Mhpar1
         IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,4E12.3,I8)') 1 , 
     &        (PPH(k,1),k=5,8) , NOUthd(1,1)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4)')
     &         'ini.hard X/flavour particle 2:' , Mhpar2
         IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,E12.3,I8)') 1 , XHD(1,2) , 
     &        NINhd(1,2)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I4)')
     &         'fin.hard momenta  particle 2:' , Mhpar2
         IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I3,4E12.3,I8)') 1 , 
     &        (PPH(k,2),k=5,8) , NOUthd(1,2)
      END IF
 
99999 END SUBROUTINE
