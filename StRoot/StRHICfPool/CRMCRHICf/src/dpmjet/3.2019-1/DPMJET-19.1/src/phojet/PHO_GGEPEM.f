
      SUBROUTINE PHO_GGEPEM(Nevent,Ee1,Ee2)
C**********************************************************************
C
C     interface to call PHOJET (variable energy run) for
C     gamma-gamma collisions on e+e- collider
C
C     fully differential equivalent (improved) photon approximation
C     to get photon flux
C
C     input:     EE1     LAB system energy of electron/positron 1
C                EE2     LAB system energy of electron/positron 2
C                NEVENT  >0  number of events to generate
C                        -1   initialization
C                        -2   final call (cross section calculation)
C            from /LEPCUT/:
C                YMIN1   lower limit of Y1
C                        (energy fraction taken by photon from electron)
C                YMAX1   upper limit of Y1
C                Q2MIN1  lower limit of photon virtuality
C                Q2MAX1  upper limit of photon virtuality
C                THMIN1  lower limit of scattered electron
C                THMAX1  upper limit of scattered electron
C                YMIN2   lower limit of Y2
C                        (energy fraction taken by photon from electron)
C                YMAX2   upper limit of Y2
C                Q2MIN2  lower limit of photon virtuality
C                Q2MAX2  upper limit of photon virtuality
C                THMIN2  lower limit of scattered electron
C                THMAX2  upper limit of scattered electron
C
C     output:    after final call with NEVENT=-2
C                EE1     e+ e- cross section (mb)
C                EE2     gamma-gamma cross section (mb)
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      DOUBLE PRECISION Ee1 , Ee2
      INTEGER Nevent
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
C  photon flux kinematics and cuts
      INCLUDE 'inc/pofcut'
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
C  external functions
      DOUBLE PRECISION DT_RNDM
 
C  local variables
      DOUBLE PRECISION an1max , an1min , an2max , an2min , ay1 , ay2 , 
     &                 ays1 , ays2 , cof1 , cof2 , cpfthe , day1 , 
     &                 day2 , delly , ditry , ditrw , ecfrac , ecmax2 , 
     &                 ecmin2 , egam , elem , elem2 , ffl , fft , 
     &                 fluxl , fluxt , flxapp , flxqpm , ggecm2 , p1 , 
     &                 p2 , pp , pt , pt2 , q21av2 , q21ave , q21max , 
     &                 q21min , q22av2 , q22ave , q22max , q22min , 
     &                 q2log1 , q2log2 , q2low1 , q2low2 , q2p1 , q2p2 , 
     &                 sif1 , sif2 , sigcur , sigmax , thmac1 , thmac2 , 
     &                 thmic1 , thmic2 , weight , wg , wgfx , wgh , 
     &                 wghapp , wghl , wghqpm , wgmax , wgy , x1del , 
     &                 x1max , x1min , x2del , x2max , x2min , y1 , y2 , 
     &                 yeff1 , yeff2 , ymi , yy1max , yy1min , yy2max , 
     &                 yy2min
 
      INTEGER i , iheac1 , iheac2 , ihetry , irej , itrw_low , 
     &        itrw_high , itry_low , itry_high , k , max_tab , niter , 
     &        itg1 , itg2
 
      DIMENSION p1(4) , p2(4) , ihetry(4) , iheac1(4) , iheac2(4)
      INTEGER IPHO_PDG2ID
 
C  initialization of event generation
 
      IF ( Nevent.EQ.-1 ) THEN
 
         DO i = 1 , 4
            ihetry(i) = 0
            iheac1(i) = 0
            iheac2(i) = 0
         END DO
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A)')
     &         'PHO_GGEPEM: initialization'
 
C  electron data
         elem = 0.512D-03
         elem2 = elem**2
         AMSrc(1) = elem
         AMSrc(2) = elem
C  lepton numbers
         IDPsrc(1) = 11
         IDPsrc(2) = -11
         IDBsrc(1) = IPHO_PDG2ID(11)
         IDBsrc(2) = IPHO_PDG2ID(-11)
 
C  check/update kinematic limitations
 
         ymi = MIN(YMAx1,1.D0-elem/Ee1)
         IF ( ymi.LT.YMAx1 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &            'PHO_GGEPEM: Ymax1 decreased (old/new)' , YMAx1 , ymi
            YMAx1 = ymi
         END IF
         ymi = MIN(YMAx2,1.D0-elem/Ee2)
         IF ( ymi.LT.YMAx2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &            'PHO_GGEPEM: Ymax2 decreased (old/new)' , YMAx2 , ymi
            YMAx2 = ymi
         END IF
 
         ymi = ECMin**2/(4.D0*Ee1*Ee2*YMAx2)
         IF ( YMIn1.LT.ymi ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &            'PHO_GGEPEM: Ymin1 increased (old/new)' , YMIn1 , ymi
            YMIn1 = ymi
         ELSE IF ( YMIn1.GT.ymi ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,E12.5,A,E12.5)')
     &            'PHO_GGEPEM:' , 'ECM-CUT corresponds to YMIN1 of' , 
     &           ymi , '  INSTEAD OF' , YMIn1
         END IF
         ymi = ECMin**2/(4.D0*Ee1*Ee2*YMAx1)
         IF ( YMIn2.LT.ymi ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &            'PHO_GGEPEM: Ymin2 increased (old/new)' , YMIn2 , ymi
            YMIn2 = ymi
         ELSE IF ( YMIn2.GT.ymi ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,E12.5,A,E12.5)')
     &            'PHO_GGEPEM:' , 'ECM-CUT corresponds to YMIN2 of' , 
     &           ymi , '  INSTEAD OF' , YMIn2
         END IF
 
C  store COS of angular tagging range
         thmic1 = COS(MAX(0.D0,THMin1))
         thmac1 = COS(MIN(THMax1,PI))
         thmic2 = COS(MAX(0.D0,THMin2))
         thmac2 = COS(MIN(THMax2,PI))
 
         x1max = LOG(YMAx1)
         x1min = LOG(YMIn1)
         x1del = x1max - x1min
         x2max = LOG(YMAx2)
         x2min = LOG(YMIn2)
         x2del = x2max - x2min
 
C  debug: integrated photon flux
 
         IF ( IDEb(30).GE.1 ) THEN
            max_tab = 50
            fluxt = 0.D0
            fluxl = 0.D0
            delly = LOG(YMAx1/YMIn1)/DBLE(max_tab-1)
            IF ( LPRi.GT.4 .AND. IDEb(30).GE.2 ) WRITE (LO,'(1X,2A,I5)')
     &            'PHO_GGEPEM: ' , 
     &           'table of photon flux (trans/long side 1)' , max_tab
            DO i = 1 , max_tab
               y1 = EXP(x1min+delly*DBLE(i-1))
               IF ( (1.D0-y1).GT.1.D-8 ) THEN
                  q2low1 = MAX(Q2Min1,elem2*y1*y1/(1.D0-y1))
               ELSE
                  q2low1 = 2.D0*Q2Max1
               END IF
               IF ( q2low1.LT.Q2Max1 ) THEN
                  fft = ((1.D0+(1.D0-y1)**2)/y1*LOG(Q2Max1/q2low1)
     &                  -2.D0*elem2*y1*(1.D0/q2low1-1.D0/Q2Max1))
     &                  /(2.D0*PI*137.D0)
                  ffl = 2.D0*(1.D0-y1)*LOG(Q2Max1/q2low1)
     &                  /(2.D0*PI*137.D0)
               ELSE
                  fft = 0.D0
                  ffl = 0.D0
               END IF
               fluxt = fluxt + y1*ffl
               fluxl = fluxl + y1*fft
               IF ( LPRi.GT.4 .AND. IDEb(30).GE.2 )
     &               WRITE (LO,'(5X,1P3E14.4)') y1 , fft , ffl
            END DO
            fluxt = fluxt*delly
            fluxl = fluxl*delly
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,1P2E12.4)')
     &            'PHO_GGEPEM: ' , 
     &           'integrated flux (trans/long side 1):' , fluxt , fluxl
         END IF
 
C  maximum weight
 
         q2low1 = MAX(Q2Min1,elem2*YMIn1**2/(1.D0-YMIn1))
         q2low2 = MAX(Q2Min2,elem2*YMIn2**2/(1.D0-YMIn2))
         y1 = YMIn1
         y2 = YMIn2
         IF ( ISWmdl(10).GE.2 ) THEN
C  long. and transversely polarized photons
            wgmax = ((1.D0+(1.D0-y1)**2+2.D0*(1.D0-y1))
     &              *LOG(Q2Max1/q2low1)
     &              -2.D0*elem2*y1*(1.D0/q2low1-1.D0/Q2Max1)*y1)
     &              *((1.D0+(1.D0-y2)**2+2.D0*(1.D0-y2))
     &              *LOG(Q2Max2/q2low2)
     &              -2.D0*elem2*y2*(1.D0/q2low2-1.D0/Q2Max2)*y2)
         ELSE
C  transversely polarized photons only
            wgmax = ((1.D0+(1.D0-y1)**2)*LOG(Q2Max1/q2low1)
     &              -2.D0*elem2*y1*(1.D0/q2low1-1.D0/Q2Max1)*y1)
     &              *((1.D0+(1.D0-y2)**2)*LOG(Q2Max2/q2low2)
     &              -2.D0*elem2*y2*(1.D0/q2low2-1.D0/Q2Max2)*y2)
         END IF
 
C  initialize gamma-gamma event generator
 
C  photon 1
         egam = YMAx1*Ee1
         p1(1) = 0.D0
         p1(2) = 0.D0
         p1(3) = SQRT(egam**2-q2low1)
         p1(4) = egam
C  photon 2
         egam = YMAx2*Ee2
         p2(1) = 0.D0
         p2(2) = 0.D0
         p2(3) = -SQRT(egam**2-q2low2)
         p2(4) = egam
C  sum of helicities
         IGHel(1) = -1
         IGHel(2) = -1
 
C  set min. energy for interpolation tables
         PARmdl(19) = MIN(PARmdl(19),ECMin)
 
C  initialize event gneration
         CALL PHO_SETPAR(1,22,0,0.D0)
         CALL PHO_SETPAR(2,22,0,0.D0)
         CALL PHO_EVENT(-1,p1,p2,sigmax,irej)
         CALL PHO_PHIST(-1,sigmax)
         CALL PHO_LHIST(-1,sigmax)
 
C  generation of events, flux calculation
 
         ecmin2 = ECMin**2
         ecmax2 = ECMax**2
         ecfrac = ecmin2/(4.D0*Ee1*Ee2)
         ay1 = 0.D0
         ay2 = 0.D0
         ays1 = 0.D0
         ays2 = 0.D0
         q21min = 1.D30
         q22min = 1.D30
         q21max = 0.D0
         q22max = 0.D0
         q21ave = 0.D0
         q22ave = 0.D0
         q21av2 = 0.D0
         q22av2 = 0.D0
         an1min = 1.D30
         an2min = 1.D30
         an1max = 0.D0
         an2max = 0.D0
         yy1min = 1.D30
         yy2min = 1.D30
         yy1max = 0.D0
         yy2max = 0.D0
         niter = 0
         itry_low = 0
         itry_high = 0
         itrw_low = 0
         itrw_high = 0
 
C  generate NEVENT events (might be just 1 per call)
 
      ELSE IF ( Nevent.GT.0 ) THEN
 
         niter = niter + Nevent
 
         DO i = 1 , Nevent
 
C  sample y1, y2
 20         itry_low = itry_low + 1
            IF ( itry_low.EQ.1000000 ) THEN
               itry_low = 0
               itry_high = itry_high + 1
            END IF
 
 40         itrw_low = itrw_low + 1
            IF ( itrw_low.EQ.1000000 ) THEN
               itrw_low = 0
               itrw_high = itrw_high + 1
            END IF
 
            y1 = EXP(x1del*DT_RNDM(ay1)+x1min)
            y2 = EXP(x2del*DT_RNDM(ay2)+x2min)
            IF ( y1*y2.LT.ecfrac ) GOTO 40
            IF ( ISWmdl(10).GE.2 ) THEN
               yeff1 = 1.D0 + (1.D0-y1)**2 + 2.D0*(1.D0-y1)
               yeff2 = 1.D0 + (1.D0-y2)**2 + 2.D0*(1.D0-y2)
            ELSE
               yeff1 = 1.D0 + (1.D0-y1)**2
               yeff2 = 1.D0 + (1.D0-y2)**2
            END IF
 
            q2low1 = MAX(Q2Min1,elem2*y1**2/(1.D0-y1))
            q2low2 = MAX(Q2Min2,elem2*y2**2/(1.D0-y2))
            q2log1 = LOG(Q2Max1/q2low1)
            q2log2 = LOG(Q2Max2/q2low2)
            wgh = (yeff1*q2log1-2.D0*elem2*y1*(1.D0/q2low1-1.D0/Q2Max1)
     &            *y1)
     &            *(yeff2*q2log2-2.D0*elem2*y2*(1.D0/q2low2-1.D0/Q2Max2)
     &            *y2)
            IF ( wgmax.LT.wgh ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,4E12.5)')
     &               'PHO_GGEPEM: inconsistent weight:' , y1 , y2 , 
     &              wgmax , wgh
            END IF
            IF ( DT_RNDM(ays1)*wgmax.GT.wgh ) GOTO 40
 
C  limit on Ecm_gg (app. cut, precise cut applied later)
            ggecm2 = 4.D0*y1*y2*Ee1*Ee2
            IF ( ggecm2.LT.ecmin2 ) GOTO 40
 
C  sample Q2
            IF ( IPAmdl(174).EQ.1 ) THEN
 50            q2p1 = q2low1*EXP(q2log1*DT_RNDM(y1))
               weight = (yeff1-2.D0*(1.D0-y1)*q2low1/q2p1)/yeff1
               IF ( weight.LT.DT_RNDM(q2p1) ) GOTO 50
            ELSE
               q2p1 = q2low1
            END IF
 
            IF ( IPAmdl(174).EQ.1 ) THEN
 60            q2p2 = q2low2*EXP(q2log2*DT_RNDM(y2))
               weight = (yeff2-2.D0*(1.D0-y2)*q2low2/q2p2)/yeff2
               IF ( weight.LT.DT_RNDM(q2p2) ) GOTO 60
            ELSE
               q2p2 = q2low2
            END IF
 
            GYY(1) = y1
            GQ2(1) = q2p1
            GYY(2) = y2
            GQ2(2) = q2p2
 
C  incoming electron 1
            PINi(1,1) = 0.D0
            PINi(2,1) = 0.D0
            PINi(3,1) = Ee1*(1.D0-0.5D0*elem2/Ee1**2)
            PINi(4,1) = Ee1
            PINi(5,1) = elem
C  photon 1
            pp = (2.D0*Ee1**2*y1+q2p1)/(2.D0*PINi(3,1))
            pt2 = (Ee1**2*(q2p1*(1.D0-y1)-elem2*y1**2)-0.25D0*q2p1**2-
     &            q2p1*elem2)/PINi(3,1)**2
            IF ( pt2.LT.0.D0 ) GOTO 40
            pt = SQRT(pt2)
            CALL PHO_SFECFE(sif1,cof1)
            p1(1) = cof1*pt
            p1(2) = sif1*pt
            p1(3) = pp
            p1(4) = Ee1*y1
C  outgoing electron 1
            PFIn(1,1) = -p1(1)
            PFIn(2,1) = -p1(2)
            PFIn(3,1) = PINi(3,1) - p1(3)
            PFIn(4,1) = PINi(4,1) - p1(4)
            PFIn(5,1) = elem
C  incoming electron 2
            PINi(1,2) = 0.D0
            PINi(2,2) = 0.D0
            PINi(3,2) = -Ee2*(1.D0-0.5D0*elem2/Ee2**2)
            PINi(4,2) = Ee2
            PINi(5,2) = 0.D0
C  photon 2
            pp = (2.D0*Ee2**2*y2+q2p2)/(2.D0*PINi(3,2))
            pt2 = (Ee2**2*(q2p2*(1.D0-y2)-elem2*y2**2)-0.25D0*q2p2**2-
     &            q2p2*elem2)/PINi(3,2)**2
            IF ( pt2.LT.0.D0 ) GOTO 40
            pt = SQRT(pt2)
            CALL PHO_SFECFE(sif2,cof2)
            p2(1) = cof2*pt
            p2(2) = sif2*pt
            p2(3) = pp
            p2(4) = Ee2*y2
C  outgoing electron 2
            PFIn(1,2) = -p2(1)
            PFIn(2,2) = -p2(2)
            PFIn(3,2) = PINi(3,2) - p2(3)
            PFIn(4,2) = PINi(4,2) - p2(4)
            PFIn(5,2) = elem
 
C  precise ECMS cut
 
            ggecm2 = (p1(4)+p2(4))**2 - (p1(1)+p2(1))**2 - (p1(2)+p2(2))
     &               **2 - (p1(3)+p2(3))**2
            IF ( (ggecm2.LT.ecmin2) .OR. (ggecm2.GT.ecmax2) ) GOTO 40
            GGEcm = SQRT(ggecm2)
 
C  beam lepton detector acceptance
 
C  lepton tagger 1
            cpfthe = PFIn(3,1)/PFIn(4,1)
            itg1 = 0
            IF ( PFIn(4,1).GE.EEMin1 ) THEN
               IF ( (cpfthe.LE.thmic1) .AND. (cpfthe.GE.thmac1) )
     &              itg1 = 1
            END IF
 
C  lepton tagger 2
            cpfthe = PFIn(3,2)/PFIn(4,2)
            itg2 = 0
            IF ( PFIn(4,2).GE.EEMin2 ) THEN
               IF ( (cpfthe.LE.thmic2) .AND. (cpfthe.GE.thmac2) )
     &              itg2 = 1
            END IF
 
C  beam lepton taggers
 
C  anti-tag
            IF ( (ITAg1.EQ.-1) .AND. (itg1.NE.0) ) GOTO 40
            IF ( (ITAg2.EQ.-1) .AND. (itg2.NE.0) ) GOTO 40
C  tag
            IF ( (ITAg1.EQ.1) .AND. (itg1.EQ.0) ) GOTO 40
            IF ( (ITAg2.EQ.1) .AND. (itg2.EQ.0) ) GOTO 40
C  single-tag inclusive
            IF ( (ITAg1.EQ.0) .AND. (ITAg2.EQ.0) .AND. (itg1+itg2.EQ.0)
     &           ) GOTO 40
C  single-tag/anti-tag
            IF ( (ITAg1.EQ.2) .AND. (ITAg2.EQ.2) .AND. (itg1+itg2.NE.1)
     &           ) GOTO 40
 
            PGAm(1,1) = p1(1)
            PGAm(2,1) = p1(2)
            PGAm(3,1) = p1(3)
            PGAm(4,1) = p1(4)
            PGAm(5,1) = -SQRT(q2p1)
            PGAm(1,2) = p2(1)
            PGAm(2,2) = p2(2)
            PGAm(3,2) = p2(3)
            PGAm(4,2) = p2(4)
            PGAm(5,2) = -SQRT(q2p2)
 
C  photon helicities
            IF ( ISWmdl(10).GE.2 ) THEN
               wgh = yeff1 - 2.D0*elem2*y1**2/q2p1
               wghl = 2.D0*(1-y1)
               IF ( DT_RNDM(y1).GT.wghl/wgh ) THEN
                  IGHel(1) = 1
               ELSE
                  IGHel(1) = 0
               END IF
               wgh = yeff2 - 2.D0*elem2*y2**2/q2p2
               wghl = 2.D0*(1-y2)
               IF ( DT_RNDM(y2).GT.wghl/wgh ) THEN
                  IGHel(2) = 1
               ELSE
                  IGHel(2) = 0
               END IF
               k = 2*IGHel(1) + IGHel(2) + 1
               ihetry(k) = ihetry(k) + 1
            ELSE
               IGHel(1) = -1
               IGHel(2) = -1
            END IF
 
C  user cuts
            CALL PHO_PRESEL(5,irej)
            IF ( irej.NE.0 ) GOTO 40
 
            wgfx = 1.D0
C  reweight according to LO photon emission diagrams (Budnev et al.)
            IF ( IPAmdl(116).GE.1 ) THEN
               CALL PHO_WGEPEM(flxapp,flxqpm,0)
               wgfx = flxqpm/flxapp
               IF ( wgfx.GT.1.D0 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,/,5x,1p,5e11.4)')
     &                  ' PHO_GGEPEM: flux weight > 1 (y1/2,Q21/2,W)' , 
     &                 y1 , y2 , q2p1 , q2p2 , GGEcm
               END IF
            END IF
 
C  event generation
C         IVWGHT(1) = 1
C         EVWGHT(1) = MAX(WGFX,1.D0)
            CALL PHO_EVENT(1,p1,p2,sigcur,irej)
            IF ( irej.NE.0 ) GOTO 20
            IF ( ISWmdl(10).GE.2 ) THEN
               k = 2*IGHel(1) + IGHel(2) + 1
               iheac1(k) = iheac1(k) + 1
            END IF
 
C  reweight according to QPM model (e+e- collider only)
            IF ( (KHDir.GT.0) .AND. (IPAmdl(116).GE.2) .AND. 
     &           (ISWmdl(10).GE.2) ) THEN
               CALL PHO_WGEPEM(wghapp,wghqpm,1)
               wg = wghqpm/wghapp/MAX(1.D0,wgfx)
               IF ( DT_RNDM(wg).GT.wg ) GOTO 20
            ELSE IF ( IPAmdl(116).GE.1 ) THEN
               IF ( DT_RNDM(wg).GT.wgfx ) GOTO 20
            END IF
 
C  polar angle
            PFThe(1) = ACOS(PFIn(3,1)/PFIn(4,1))
            PFThe(2) = ACOS(PFIn(3,2)/PFIn(4,2))
C  azimuthal angle
            PFPhi(1) = ATAN2(cof1,sif1)
            PFPhi(2) = ATAN2(cof2,sif2)
 
C  statistics
            ay1 = ay1 + y1
            ays1 = ays1 + y1*y1
            ay2 = ay2 + y2
            ays2 = ays2 + y2*y2
            q21min = MIN(q21min,q2p1)
            q22min = MIN(q22min,q2p2)
            q21max = MAX(q21max,q2p1)
            q22max = MAX(q22max,q2p2)
            an1min = MIN(an1min,PFThe(1))
            an2min = MIN(an2min,PFThe(2))
            an1max = MAX(an1max,PFThe(1))
            an2max = MAX(an2max,PFThe(2))
            yy1min = MIN(yy1min,y1)
            yy2min = MIN(yy2min,y2)
            yy1max = MAX(yy1max,y1)
            yy2max = MAX(yy2max,y2)
            q21ave = q21ave + q2p1
            q22ave = q22ave + q2p2
            q21av2 = q21av2 + q2p1*q2p1
            q22av2 = q22av2 + q2p2*q2p2
            IF ( ISWmdl(10).GE.2 ) THEN
               k = 2*IGHel(1) + IGHel(2) + 1
               iheac2(k) = iheac2(k) + 1
            END IF
 
C  external histograms
            CALL PHO_PHIST(1,HSWght(0))
            CALL PHO_LHIST(1,HSWght(0))
         END DO
 
C  final cross section calculation and event generation summary
 
      ELSE IF ( Nevent.EQ.-2 ) THEN
 
C       EVWGHT(1) = 1.D0
C       IVWGHT(1) = 0
         ditry = DBLE(itry_high)*1.D+6 + DBLE(itry_low)
         ditrw = DBLE(itrw_high)*1.D+6 + DBLE(itrw_low)
         wgy = wgmax*ditry/ditrw/(137.D0*2.D0*PI)**2
         wgy = wgy*LOG(YMAx1/YMIn1)*LOG(YMAx2/YMIn2)
         ay1 = ay1/DBLE(niter)
         ays1 = ays1/DBLE(niter)
         day1 = SQRT((ays1-ay1**2)/DBLE(niter))
         ay2 = ay2/DBLE(niter)
         ays2 = ays2/DBLE(niter)
         day2 = SQRT((ays2-ay2**2)/DBLE(niter))
         q21ave = q21ave/DBLE(niter)
         q21av2 = q21av2/DBLE(niter)
         q21av2 = SQRT((q21av2-q21ave**2)/DBLE(niter))
         q22ave = q22ave/DBLE(niter)
         q22av2 = q22av2/DBLE(niter)
         q22av2 = SQRT((q22av2-q22ave**2)/DBLE(niter))
         weight = wgy*sigmax*DBLE(niter)/ditry
         Ee1 = weight
         Ee2 = sigmax*DBLE(niter)/ditry
 
C  output of statistics, histograms
         IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/1X,A,1PE12.3,A,/1X,A)') 
     &       '========================================================='
     &       , ' *****   simulated cross section: ' , weight , 
     &       ' mb  *****' , 
     &       '========================================================='
         IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,I10,1p,2e14.6)')
     &         'PHO_GGEPEM:summary: NITER,ITRY,ITRW' , niter , ditry , 
     &        ditrw
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'effective weight (FLUX,TOTAL)' , wgy , weight
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'average Y1,DY1               ' , ay1 , day1
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'average Y2,DY2               ' , ay2 , day2
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'sampled Y range photon 1     ' , yy1min , yy1max
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'sampled Y range photon 2     ' , yy2min , yy2max
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'average Q2,DQ2 photon 1      ' , q21ave , q21av2
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'sampled Q2 range photon 1    ' , q21min , q21max
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'average Q2,DQ2  photon 2     ' , q22ave , q22av2
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'sampled Q2 range photon 2    ' , q22min , q22max
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &         'sampled THETA range electron1' , an1min , an1max
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P4E12.4)')
     &         'sampled THETA range electron2' , an2min , an2max , 
     &        PI - an2max , PI - an2min
 
         IF ( ISWmdl(10).GE.2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,3(/1X,A,4I12))') 
     &      'Helicity decomposition:    0 0      0 1      1 0       1 1'
     &      , 'tried:        ' , ihetry , 'accepted (1): ' , iheac1 , 
     &      'accepted (2): ' , iheac2
         END IF
 
         CALL PHO_EVENT(-2,p1,p2,weight,irej)
         IF ( niter.GT.1 ) THEN
            CALL PHO_PHIST(-2,weight)
            CALL PHO_LHIST(-2,weight)
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &            'PHO_GGEPEM: no output of histograms' , niter
         END IF
 
      END IF
 
      END SUBROUTINE
