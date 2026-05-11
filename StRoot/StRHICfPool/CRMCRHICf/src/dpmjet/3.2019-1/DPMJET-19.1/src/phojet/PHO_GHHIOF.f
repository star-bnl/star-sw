
      SUBROUTINE PHO_GHHIOF(Nevent,Een,Na,Nz)
C**********************************************************************
C
C     interface to call PHOJET (variable energy run) for
C     gamma-hadron collisions in heavy ion collisions
C     (form factor approach)
C
C     input:     EEN     LAB system energy per nucleon
C                NA      atomic number of ion/hadron
C                NZ      charge number of ion/hadron
C                NEVENT  number of events to generate
C            from /LEPCUT/:
C                YMIN1,2 lower limit of Y
C                        (energy fraction taken by photon from hadron)
C                YMAX1,2 upper cutoff for Y, necessary to avoid
C                        underflows
C                Q2MIN1,2 minimum Q**2 of photons (should be set to 0)
C                Q2MAX1,2 maximum Q**2 of photons (if necessary,
C                        corrected according size of hadron)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alpha , amp , amp2 , ay1 , ay2 , ays1 , ays2 , 
     &                 cof , day1 , day2 , delly , DT_RNDM , e1y , ee , 
     &                 Een , egam , fac12 , ff , ff2 , flux
      DOUBLE PRECISION fm2gev , gamma , ghdiff , hima2 , himass , 
     &                 hiradi , p1 , p2 , PI , q21av2 , q21ave , 
     &                 q21max , q21min , q22av2 , q22ave , q22max , 
     &                 q22min , q2e , q2log1 , q2log2
      DOUBLE PRECISION q2low1 , q2low2 , q2maxh , q2p1 , q2p2 , sif , 
     &                 sigcur , sigmax , weight , wgh , wgmax , wgmax1 , 
     &                 wgmax2 , wgy , wgy1 , wgy2 , x1del , x1max , 
     &                 x1min , x2del
      DOUBLE PRECISION x2max , x2min , xdel , xf , xmax , xmin , y1 , 
     &                 y2 , yeff , ymax , ymi , ymin , yq2 , yy1max , 
     &                 yy1min , yy2max , yy2min
      INTEGER i , irej , iside , itrw , itry , k , max_tab , Na , 
     &        Nevent , niter , niters , Nz
      SAVE 
 
      PARAMETER (PI=3.141592653589793238462643383279D+00)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  event debugging information
      INCLUDE 'inc/podebg'
C  photon flux kinematics and cuts
      INCLUDE 'inc/pofcut'
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      DIMENSION p1(4) , p2(4)
      DIMENSION niters(2) , itrw(2)
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))') 
     &                       'PHO_GHHIOF: gamma-hadron event generation'
     &                       , 
     &                       '-----------------------------------------'
C  hadron size and mass
      fm2gev = 5.07D0
      himass = DBLE(Na)*0.938D0
      hima2 = himass**2
      hiradi = 1.2D0*fm2gev*DBLE(Na)**0.333
      alpha = DBLE(Nz**2)/137.D0
      amp = 0.938D0
      amp2 = amp**2
C  correct Q2MAX1,2 according to hadron size
      q2maxh = 2.D0/hiradi**2
      Q2Max1 = MIN(Q2Max1,q2maxh)
      Q2Max2 = MIN(Q2Max2,q2maxh)
      IF ( Q2Max1.LT.1.D-20 ) Q2Max1 = q2maxh
      IF ( Q2Max2.LT.1.D-20 ) Q2Max2 = q2maxh
C  total hadron / heavy ion energy
      ee = Een*DBLE(Na)
      gamma = ee/himass
C  setup /POFSRC/
      GAMsrc(1) = gamma
      GAMsrc(2) = gamma
      RADsrc(1) = hiradi
      RADsrc(2) = hiradi
      AMSrc(1) = himass
      AMSrc(2) = himass
C  check cuts on photon-hadron mass
      IF ( (0.765D0+PARmdl(46)).GT.(PARmdl(45)*ECMin) ) THEN
         ymi = ECMin
         ECMin = PARmdl(46)/PARmdl(45) + 0.1D0
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &         'PHO_GHHIOF: ecmin corrected to (old/new)' , ymi , ECMin
      END IF
C  check kinematic limitations
      ymi = ECMin**2/(4.D0*ee*Een)
      IF ( YMIn1.LT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &         'PHO_GHHIOF: ymin1 increased to (old/new)' , YMIn1 , ymi
         YMIn1 = ymi
      ELSE IF ( YMIn1.GT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,E12.5,A,E12.5)')
     &         'PHO_GHHIOF:' , 'ECM-CUT CORRESPONDS TO YMIN1 OF' , ymi , 
     &        '  INSTEAD OF' , YMIn1
      END IF
      IF ( YMIn2.LT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &         'PHO_GHHIOF: ymin2 increased to (old/new)' , YMIn2 , ymi
         YMIn2 = ymi
      ELSE IF ( YMIn2.GT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,E12.5,A,E12.5)')
     &         'PHO_GHHIOF:' , 'ECM-CUT CORRESPONDS TO YMIN2 OF' , ymi , 
     &        '  INSTEAD OF' , YMIn2
      END IF
C  kinematic limitation
      q2low1 = MAX(Q2Min1,hima2*YMIn1**2/(1.D0-YMIn1))
      q2low2 = MAX(Q2Min2,hima2*YMIn2**2/(1.D0-YMIn2))
C  debug output
      IF ( LPRi.GT.4 ) WRITE (LO,'(/6X,A,2I4)')
     &                         'MASS NUMBER, CHARGE NUMBER  ' , Na , Nz
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,E12.5)')
     &                         'HADRON MASS (GeV)           ' , himass
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,E12.5)')
     &                         'HADRON RADIUS (GeV**-1)     ' , hiradi
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'Q**2 RANGE PHOTON 1 (GEV**2)' , q2low1 , 
     &                        Q2Max1
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'Q**2 RANGE PHOTON 2 (GEV**2)' , q2low2 , 
     &                        Q2Max2
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'Y RANGE PHOTON 1            ' , YMIn1 , 
     &                        YMAx1
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'Y RANGE PHOTON 2            ' , YMIn2 , 
     &                        YMAx2
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'SQRT(S) PER NUCLEON, TOTAL  ' , 
     &                        2.D0*Een , 2.D0*ee
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'INV.MASS PHOTON-HADRON      ' , ECMin , 
     &                        ECMax
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,E12.5)')
     &                         'MIN. INV.MASS PHOTON-POMERON' , 
     &                        PARmdl(175)
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,I10)')
     &                         'EVENTS TO PROCESS           ' , Nevent
      IF ( q2low1.GE.Q2Max1 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &         'PHO_GHHIOF:ERROR:INCONSISTENT Q**2 RANGE 1' , q2low1 , 
     &        Q2Max1
         CALL PHO_ABORT
      END IF
      IF ( q2low2.GE.Q2Max2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &         'PHO_GHHIOF:ERROR:INCONSISTENT Q**2 RANGE 2' , q2low2 , 
     &        Q2Max2
         CALL PHO_ABORT
      END IF
C  hadron numbers set to 0
      IDPsrc(1) = 0
      IDPsrc(2) = 0
      IDBsrc(1) = 0
      IDBsrc(2) = 0
C
      max_tab = 100
      ymax = YMAx1
      ymin = YMIn1
      xmax = LOG(ymax)
      xmin = LOG(ymin)
      xdel = xmax - xmin
      delly = LOG(ymax/ymin)/DBLE(max_tab-1)
      DO i = 1 , max_tab
         y1 = EXP(xmin+delly*DBLE(i-1))
         q2low1 = MAX(Q2Min1,hima2*y1*y1/(1.D0-y1))
         IF ( q2low1.GE.Q2Max1 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &            'PHO_GHHIOF: ymax1 changed from/to' , YMAx1 , y1
            YMAx1 = MIN(y1,YMAx1)
            GOTO 100
         END IF
      END DO
 100  ymax = YMAx2
      ymin = YMIn2
      xmax = LOG(ymax)
      xmin = LOG(ymin)
      xdel = xmax - xmin
      delly = LOG(ymax/ymin)/DBLE(max_tab-1)
      DO i = 1 , max_tab
         y1 = EXP(xmin+delly*DBLE(i-1))
         q2low2 = MAX(Q2Min2,hima2*y1*y1/(1.D0-y1))
         IF ( q2low2.GE.Q2Max2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &            'PHO_GHHIOF: ymax2 changed from/to' , YMAx2 , y1
            YMAx2 = MIN(y1,YMAx2)
            GOTO 200
         END IF
      END DO
C
 200  x1max = LOG(YMAx1)
      x1min = LOG(YMIn1)
      x1del = x1max - x1min
      x2max = LOG(YMAx2)
      x2min = LOG(YMIn2)
      x2del = x2max - x2min
      delly = LOG(YMAx1/YMIn1)/DBLE(max_tab-1)
      flux = 0.D0
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(/1X,A,I5)')
     &      'PHO_GHHIOF: table of raw photon flux (side 1)' , max_tab
      DO i = 1 , max_tab
         y1 = EXP(x1min+delly*DBLE(i-1))
         q2low1 = MAX(Q2Min1,hima2*y1*y1/(1.D0-y1))
         ff = ((1.D0+(1.D0-y1)**2)/y1*LOG(Q2Max1/q2low1)
     &        -2.D0*hima2*y1*(1.D0/q2low1-1.D0/Q2Max1))*alpha/(2.D0*PI)
         flux = flux + y1*ff
         IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(5X,2E15.4)')
     &        y1 , ff
      END DO
      flux = flux*delly
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(1X,A,E12.4)')
     &      'PHO_GHHIOF: integrated flux (one side):' , flux
C
C  photon
      egam = MAX(YMAx1,YMAx2)*ee
      p1(1) = 0.D0
      p1(2) = 0.D0
      p1(3) = egam
      p1(4) = egam
C  hadron
      p2(1) = 0.D0
      p2(2) = 0.D0
      p2(3) = -SQRT(Een**2-amp2)
      p2(4) = Een
      CALL PHO_SETPAR(1,22,0,0.D0)
      CALL PHO_SETPAR(2,2212,0,0.D0)
      CALL PHO_EVENT(-1,p1,p2,sigmax,irej)
C
      q2low1 = MAX(Q2Min1,hima2*YMIn1**2/(1.D0-YMIn1))
      q2low2 = MAX(Q2Min2,hima2*YMIn2**2/(1.D0-YMIn2))
      y1 = YMIn1
      y2 = YMIn2
      wgmax1 = (1.D0+(1.D0-y1)**2)*LOG(Q2Max1/q2low1)
     &         - 2.D0*hima2*y1*(1.D0/q2low1-1.D0/Q2Max1)*y1
      wgmax2 = (1.D0+(1.D0-y2)**2)*LOG(Q2Max2/q2low2)
     &         - 2.D0*hima2*y2*(1.D0/q2low2-1.D0/Q2Max2)*y2
C
      IF ( IPAmdl(175).EQ.1 ) wgmax2 = 0.D0
      IF ( IPAmdl(175).EQ.2 ) wgmax1 = 0.D0
C
      fac12 = wgmax1*LOG(YMAx1/YMIn1)
     &        /(wgmax1*LOG(YMAx1/YMIn1)+wgmax2*LOG(YMAx2/YMIn2))
C
      CALL PHO_PHIST(-1,sigmax)
      CALL PHO_LHIST(-1,sigmax)
C
C  generation of events, flux calculation
 
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
      yy1min = 1.D30
      yy2min = 1.D30
      yy1max = 0.D0
      yy2max = 0.D0
      niter = Nevent
      niters(1) = 0
      niters(2) = 0
      itry = 0
      itrw(1) = 0
      itrw(2) = 0
      DO i = 1 , niter
C  sample y1, y2
 250     itry = itry + 1
C
C  select side of photon emission
 300     IF ( DT_RNDM(ay1).LT.fac12 ) THEN
            itrw(1) = itrw(1) + 1
C  select Y1
            y1 = EXP(x1del*DT_RNDM(ay1)+x1min)
            q2low1 = MAX(Q2Min1,hima2*y1**2/(1.D0-y1))
            IF ( q2low1.GE.Q2Max1 ) GOTO 300
            q2log1 = LOG(Q2Max1/q2low1)
            wgh = (1.D0+(1.D0-y1)**2)
     &            *q2log1 - 2.D0*hima2*y1*(1.D0/q2low1-1.D0/Q2Max1)*y1
            IF ( LPRi.GT.4 .AND. wgmax1.LT.wgh )
     &            WRITE (LO,'(1X,A,3E12.5)')
     &            'PHO_GHHIOF:WEIGHT ERROR (1):' , y1 , wgmax1 , wgh
            IF ( DT_RNDM(ays1)*wgmax1.GT.wgh ) GOTO 300
C  sample Q2
            IF ( IPAmdl(174).EQ.1 ) THEN
               yeff = 1.D0 + (1.D0-y1)**2
 310           q2p1 = q2low1*EXP(q2log1*DT_RNDM(y1))
               weight = (yeff-2.D0*(1.D0-y1)*q2low1/q2p1)/yeff
               IF ( weight.LT.DT_RNDM(q2p1) ) GOTO 310
            ELSE
               q2p1 = q2low1
            END IF
C  impact parameter
            GAImp(1) = 1.D0/SQRT(q2p1)
C  form factor (squared)
            ff2 = 1.D0
            IF ( GAImp(1).LT.2.D0*hiradi ) ff2 = 0.D0
            IF ( DT_RNDM(q2p1).GE.ff2 ) GOTO 300
C  photon data
            GYY(1) = y1
            GQ2(1) = q2p1
 
C
C  incoming hadron 1
            PINi(1,1) = 0.D0
            PINi(2,1) = 0.D0
            PINi(3,1) = SQRT(ee**2-amp2)
            PINi(4,1) = ee
            PINi(5,1) = amp
C  outgoing hadron 1
            yq2 = SQRT((1.D0-y1)*q2p1)
            q2e = q2p1/(4.D0*ee)
            e1y = ee*(1.D0-y1)
            CALL PHO_SFECFE(sif,cof)
            PFIn(1,1) = yq2*cof
            PFIn(2,1) = yq2*sif
            PFIn(3,1) = e1y - q2e
            PFIn(4,1) = e1y + q2e
            PFIn(5,1) = 0.D0
            PFPhi(1) = ATAN2(cof,sif)
            PFThe(1) = ACOS((e1y-q2e)/(q2e+e1y))
C  incoming hadron 2
            PINi(1,2) = 0.D0
            PINi(2,2) = 0.D0
            PINi(3,2) = -SQRT(ee**2-amp2)
            PINi(4,2) = ee
            PINi(5,2) = amp
C  scattering photon
            p1(1) = -PFIn(1,1)
            p1(2) = -PFIn(2,1)
            p1(3) = PINi(3,1) - PFIn(3,1)
            p1(4) = PINi(4,1) - PFIn(4,1)
C  scattering hadron
            p2(1) = 0.D0
            p2(2) = 0.D0
            p2(3) = -SQRT(Een**2-amp2)
            p2(4) = Een
            iside = 1
C
         ELSE
C
            itrw(2) = itrw(2) + 1
C  select Y2
            y2 = EXP(x2del*DT_RNDM(ay2)+x2min)
            q2low2 = MAX(Q2Min2,hima2*y2**2/(1.D0-y2))
            IF ( q2low2.GE.Q2Max2 ) GOTO 300
            q2log2 = LOG(Q2Max2/q2low2)
            wgh = (1.D0+(1.D0-y2)**2)
     &            *q2log2 - 2.D0*hima2*y2*(1.D0/q2low2-1.D0/Q2Max2)*y2
            IF ( LPRi.GT.4 .AND. wgmax2.LT.wgh )
     &            WRITE (LO,'(1X,A,3E12.5)')
     &            'PHO_GHHIOF:WEIGHT ERROR (2):' , y2 , wgmax2 , wgh
            IF ( DT_RNDM(ays1)*wgmax2.GT.wgh ) GOTO 300
C  sample Q2
            IF ( IPAmdl(174).EQ.1 ) THEN
               yeff = 1.D0 + (1.D0-y2)**2
 320           q2p2 = q2low2*EXP(q2log2*DT_RNDM(y2))
               weight = (yeff-2.D0*(1.D0-y2)*q2low2/q2p2)/yeff
               IF ( weight.LT.DT_RNDM(q2p2) ) GOTO 320
            ELSE
               q2p2 = q2low2
            END IF
C  impact parameter
            GAImp(2) = 1.D0/SQRT(q2p2)
C  form factor (squared)
            ff2 = 1.D0
            IF ( GAImp(2).LT.2.D0*hiradi ) ff2 = 0.D0
            IF ( DT_RNDM(q2p2).GE.ff2 ) GOTO 300
C  photon data
            GYY(2) = y2
            GQ2(2) = q2p2
 
C
C  incoming hadron 1
            PINi(1,1) = 0.D0
            PINi(2,1) = 0.D0
            PINi(3,1) = SQRT(ee**2-amp2)
            PINi(4,1) = ee
            PINi(5,1) = amp
C  incoming hadron 2
            PINi(1,2) = 0.D0
            PINi(2,2) = 0.D0
            PINi(3,2) = -SQRT(ee**2-amp2)
            PINi(4,2) = ee
            PINi(5,2) = amp
C  outgoing hadron 2
            yq2 = SQRT((1.D0-y2)*q2p2)
            q2e = q2p2/(4.D0*ee)
            e1y = ee*(1.D0-y2)
            CALL PHO_SFECFE(sif,cof)
            PFIn(1,2) = yq2*cof
            PFIn(2,2) = yq2*sif
            PFIn(3,2) = -e1y + q2e
            PFIn(4,2) = e1y + q2e
            PFIn(5,2) = 0.D0
            PFPhi(2) = ATAN2(cof,sif)
            PFThe(2) = ACOS((q2e-e1y)/(q2e+e1y))
C  scattering hadron
            p2(1) = 0.D0
            p2(2) = 0.D0
            p2(3) = SQRT(Een**2-amp2)
            p2(4) = Een
C  scattering photon
            p1(1) = -PFIn(1,2)
            p1(2) = -PFIn(2,2)
            p1(3) = PINi(3,2) - PFIn(3,2)
            p1(4) = PINi(4,2) - PFIn(4,2)
            iside = 2
         END IF
C  ECMS cut
         GGEcm = (p1(4)+p2(4))**2 - (p1(1)+p2(1))**2 - (p1(2)+p2(2))
     &           **2 - (p1(3)+p2(3))**2
         IF ( GGEcm.LT.0.1D0 ) GOTO 300
         GGEcm = SQRT(GGEcm)
         IF ( (GGEcm.LT.ECMin) .OR. (GGEcm.GT.ECMax) ) GOTO 300
C
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
         CALL PHO_PRESEL(5,irej)
C  photon helicities
         IGHel(1) = 1
         IGHel(2) = 1
C  user cuts
         IF ( irej.NE.0 ) GOTO 300
C  event generation
         CALL PHO_EVENT(1,p1,p2,sigcur,irej)
         IF ( irej.EQ.0 ) THEN
C  cut on diffractive mass
            DO k = 1 , NHEp
               IF ( ISThep(k).EQ.30 ) THEN
                  ghdiff = PHEp(1,k)
                  IF ( ghdiff.GE.PARmdl(175) ) GOTO 350
                  GOTO 250
               END IF
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A)')
     &            'PHO_GHHIOF: no diffractive entry found'
            CALL PHO_PREVNT(-1)
         END IF
         GOTO 250
C  remove quasi-elastically scattered hadron
 350     DO k = 1 , NHEp
            IF ( (ISThep(k).EQ.1) .AND. (IDHep(k).EQ.2212) ) THEN
               xf = ABS(PHEp(3,k)/Een)
C           ISTHEP(K) = 2
               IF ( xf.LT.PARmdl(72) ) GOTO 250
               GOTO 400
            END IF
         END DO
C
C  statistics
 
 400     niters(iside) = niters(iside) + 1
         IF ( iside.EQ.1 ) THEN
 
            ay1 = ay1 + y1
            ays1 = ays1 + y1*y1
            q21ave = q21ave + q2p1
            q21av2 = q21av2 + q2p1*q2p1
            q21min = MIN(q21min,q2p1)
            q21max = MAX(q21max,q2p1)
            yy1min = MIN(yy1min,y1)
            yy1max = MAX(yy1max,y1)
         ELSE
 
            ay2 = ay2 + y2
            ays2 = ays2 + y2*y2
            q22ave = q22ave + q2p2
            q22av2 = q22av2 + q2p2*q2p2
            q22min = MIN(q22min,q2p2)
            q22max = MAX(q22max,q2p2)
            yy2min = MIN(yy2min,y2)
            yy2max = MAX(yy2max,y2)
         END IF
C  histograms
         CALL PHO_PHIST(1,HSWght(0))
         CALL PHO_LHIST(1,HSWght(0))
      END DO
C
      wgmax = wgmax1*LOG(YMAx1/YMIn1)*fac12
      wgy1 = wgmax*DBLE(itry)/DBLE(MAX(itrw(1),1))*alpha/(2.D0*PI)
      wgmax = wgmax2*LOG(YMAx2/YMIn2)*(1.D0-fac12)
      wgy2 = wgmax*DBLE(itry)/DBLE(MAX(itrw(2),1))*alpha/(2.D0*PI)
      ay1 = ay1/DBLE(MAX(niters(1),1))
      ays1 = ays1/DBLE(MAX(niters(1),1))
      day1 = SQRT((ays1-ay1**2)/DBLE(MAX(niters(1),1)))
      ay2 = ay2/DBLE(MAX(niters(2),1))
      ays2 = ays2/DBLE(MAX(niters(2),1))
      day2 = SQRT((ays2-ay2**2)/DBLE(MAX(niters(2),1)))
      q21ave = q21ave/DBLE(MAX(niters(1),1))
      q21av2 = q21av2/DBLE(MAX(niters(1),1))
      q21av2 = SQRT((q21av2-q21ave**2)/DBLE(MAX(niters(1),1)))
      q22ave = q22ave/DBLE(MAX(niters(2),1))
      q22av2 = q22av2/DBLE(MAX(niters(2),1))
      q22av2 = SQRT((q22av2-q22ave**2)/DBLE(MAX(niters(2),1)))
      wgmax = wgmax1*LOG(YMAx1/YMIn1) + wgmax2*LOG(YMAx2/YMIn2)
      wgy = wgmax*DBLE(itry)/DBLE(itrw(1)+itrw(2))*alpha/(2.D0*PI)
      weight = wgy*sigmax*DBLE(niter)/DBLE(itry)
C  output of statistics, histograms
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/1X,A,1PE12.3,A,/1X,A)') 
     &       '========================================================='
     &       , ' *****   simulated cross section: ' , weight , 
     &       ' mb  *****' , 
     &       '========================================================='
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/3X,6I12)') 
     &     'PHO_GHHIOF:SUMMARY:  NITER,   NITERS1/2,   ITRY,    ITRW1,2'
     &     , niter , niters , itry , itrw
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'EFFECTIVE WEIGHT (FLUX,TOTAL)' , wgy , 
     &                        weight
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'AVERAGE Y1,DY1               ' , ay1 , 
     &                        day1
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'AVERAGE Y2,DY2               ' , ay2 , 
     &                        day2
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'SAMPLED Y RANGE PHOTON 1     ' , 
     &                        yy1min , yy1max
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'SAMPLED Y RANGE PHOTON 2     ' , 
     &                        yy2min , yy2max
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'AVERAGE Q2,DQ2 PHOTON 1      ' , 
     &                        q21ave , q21av2
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'SAMPLED Q2 RANGE PHOTON 1    ' , 
     &                        q21min , q21max
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'AVERAGE Q2,DQ2  PHOTON 2     ' , 
     &                        q22ave , q22av2
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'SAMPLED Q2 RANGE PHOTON 2    ' , 
     &                        q22min , q22max
C
      CALL PHO_EVENT(-2,p1,p2,weight,irej)
      IF ( niter.GT.1 ) THEN
         CALL PHO_PHIST(-2,weight)
         CALL PHO_LHIST(-2,weight)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &         'PHO_GHHIOF:NO OUTPUT OF HISTOGRAMS' , niter
      END IF
 
      END SUBROUTINE
