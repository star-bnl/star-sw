
      SUBROUTINE PHO_GHHIAS(Nevent,Eep,Een,Na,Nz)
C**********************************************************************
C
C     interface to call PHOJET (variable energy run) for
C     gamma-hadron collisions in heavy ion - hadron
C     collisions (form factor approach)
C
C     input:     EEP     LAB system energy of proton (GeV)
C                EEN     LAB system energy per nucleon (GeV)
C                NA      atomic number of ion/hadron
C                NZ      charge number of ion/hadron
C                NEVENT  number of events to generate
C            from /LEPCUT/:
C                YMIN2   lower limit of Y
C                        (energy fraction taken by photon from hadron)
C                YMAX2   upper cutoff for Y, necessary to avoid
C                        underflows
C                Q2MIN2  minimum Q**2 of photons (should be set to 0)
C                Q2MAX2  maximum Q**2 of photons (if necessary,
C                        corrected according size of hadron)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alpha , amp , amp2 , ay1 , ay2 , ays1 , ays2 , 
     &                 cof , day2 , delly , DT_RNDM , e1y , ee , Een , 
     &                 Eep , egam , ff , ff2 , flux , fm2gev
      DOUBLE PRECISION gamma , ghdiff , hima2 , himass , hiradi , p1 , 
     &                 p2 , PI , q22av2 , q22ave , q22max , q22min , 
     &                 q2e , q2log2 , q2low2 , q2maxh , q2p1 , q2p2 , 
     &                 sif , sigcur
      DOUBLE PRECISION sigmax , weight , wgh , wgmax , wgmax2 , wgy , 
     &                 wgy2 , x2del , x2max , x2min , xdel , xf , xmax , 
     &                 xmin , y1 , y2 , yeff , ymax , ymi , ymin
      DOUBLE PRECISION yq2 , yy2max , yy2min
      INTEGER i , irej , iside , itrw , itry , k , max_tab , Na , 
     &        Nevent , niter , niters , Nz, Nprint
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

      IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))') 
     &                       'PHO_GHHIAS: hadron-gamma event generation'
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
C  correct Q2MAX2 according to hadron size
      q2maxh = 2.D0/hiradi**2
      Q2Max2 = MIN(Q2Max2,q2maxh)
      IF ( Q2Max2.LT.1.D-20 ) Q2Max2 = q2maxh
C  total hadron / heavy ion energy
      ee = Een*DBLE(Na)
      gamma = ee/himass
C  setup /POFSRC/
      GAMsrc(2) = gamma
      RADsrc(2) = hiradi
      AMSrc(2) = himass
C  check kinematic limitations
      ymi = ECMin**2/(4.D0*ee*Eep)
      IF ( YMIn2.LT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &         'PHO_GHHIAS: ymin2 increased to (old/new)' , YMIn2 , ymi
         YMIn2 = ymi
      ELSE IF ( YMIn2.GT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,E12.5,A,E12.5)')
     &         'PHO_GHHIAS:' , 'ECM-CUT CORRESPONDS TO YMIN2 OF' , ymi , 
     &        '  INSTEAD OF' , YMIn2
      END IF
C  kinematic limitation
      q2low2 = MAX(Q2Min2,hima2*YMIn2**2/(1.D0-YMIn2))
C  debug output
      IF ( LPRi.GT.4 ) WRITE (LO,'(/6X,A,2I4)')
     &                         'MASS NUMBER, CHARGE NUMBER  ' , Na , Nz
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,E12.5)')
     &                         'HEAVY ION MASS (GeV)        ' , himass
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,E12.5)')
     &                         'HEAVY ION  RADIUS (GeV**-1) ' , hiradi
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'Q**2 RANGE PHOTON 2 (GEV**2)' , q2low2 , 
     &                        Q2Max2
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'Y RANGE PHOTON 2            ' , YMIn2 , 
     &                        YMAx2
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'SQRT(S) PER NUCLEON, TOTAL  ' , 
     &                        2.D0*SQRT(Een*Eep) , 2.D0*SQRT(ee*Eep)
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'INV.MASS HADRON-PHOTON      ' , ECMin , 
     &                        ECMax
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,I10)')
     &                         'EVENTS TO PROCESS           ' , Nevent
      IF ( q2low2.GE.Q2Max2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &         'PHO_GHHIAS:ERROR:inconsistent Q**2 range 2' , q2low2 , 
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
      ymax = YMAx2
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
     &            'PHO_GHHIAS: ymax2 changed from/to' , YMAx2 , y1
            YMAx2 = MIN(y1,YMAx2)
            GOTO 100
         END IF
      END DO
C
 100  x2max = LOG(YMAx2)
      x2min = LOG(YMIn2)
      x2del = x2max - x2min
      delly = LOG(YMAx2/YMIn2)/DBLE(max_tab-1)
      flux = 0.D0
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(/1X,A,I5)')
     &      'PHO_GHHIAS: table of raw photon flux (side 2)' , max_tab
      DO i = 1 , max_tab
         y2 = EXP(x2min+delly*DBLE(i-1))
         q2low2 = MAX(Q2Min2,hima2*y2*y2/(1.D0-y2))
         ff = ((1.D0+(1.D0-y2)**2)/y2*LOG(Q2Max2/q2low2)
     &        -2.D0*hima2*y2*(1.D0/q2low2-1.D0/Q2Max2))*alpha/(2.D0*PI)
         flux = flux + y2*ff
         IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(5X,2E15.4)')
     &        y2 , ff
      END DO
      flux = flux*delly
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(1X,A,E12.4)')
     &      'PHO_GHHIAS: integrated flux:' , flux
C
C  hadron
      p1(1) = 0.D0
      p1(2) = 0.D0
      p1(3) = -SQRT(Eep**2-amp2)
      p1(4) = Eep
C  photon
      egam = YMAx2*ee
      p2(1) = 0.D0
      p2(2) = 0.D0
      p2(3) = egam
      p2(4) = egam
      CALL PHO_SETPAR(1,2212,0,0.D0)
      CALL PHO_SETPAR(2,22,0,0.D0)
      CALL PHO_EVENT(-1,p1,p2,sigmax,irej)
C
      q2low2 = MAX(Q2Min2,hima2*YMIn2**2/(1.D0-YMIn2))
      y2 = YMIn2
      wgmax2 = (1.D0+(1.D0-y2)**2)*LOG(Q2Max2/q2low2)
     &         - 2.D0*hima2*y2*(1.D0/q2low2-1.D0/Q2Max2)*y2
C
      CALL PHO_PHIST(-1,sigmax)
      CALL PHO_LHIST(-1,sigmax)
C
C  generation of events, flux calculation
 
      ay1 = 0.D0
      ay2 = 0.D0
      ays1 = 0.D0
      ays2 = 0.D0
      q22min = 1.D30
      q22max = 0.D0
      q22ave = 0.D0
      q22av2 = 0.D0
      yy2min = 1.D30
      yy2max = 0.D0
      niter = Nevent
      niters = 0
      itry = 0
      itrw = 0
      DO i = 1 , niter
C  sample photon flux
 150     itry = itry + 1
C
 200     itrw = itrw + 1
C  select Y2
         y2 = EXP(x2del*DT_RNDM(ay2)+x2min)
         q2low2 = MAX(Q2Min2,hima2*y2**2/(1.D0-y2))
         IF ( q2low2.GE.Q2Max2 ) GOTO 200
         q2log2 = LOG(Q2Max2/q2low2)
         wgh = (1.D0+(1.D0-y2)**2)
     &         *q2log2 - 2.D0*hima2*y2*(1.D0/q2low2-1.D0/Q2Max2)*y2
         IF ( LPRi.GT.4 .AND. wgmax2.LT.wgh ) WRITE (LO,'(1X,A,3E12.5)')
     &         'PHO_GHHIAS:WEIGHT ERROR (2):' , y2 , wgmax2 , wgh
         IF ( DT_RNDM(ays1)*wgmax2.GT.wgh ) GOTO 200
C  sample Q2
         IF ( IPAmdl(174).EQ.1 ) THEN
            yeff = 1.D0 + (1.D0-y2)**2
 220        q2p2 = q2low2*EXP(q2log2*DT_RNDM(y2))
            weight = (yeff-2.D0*(1.D0-y2)*q2low2/q2p2)/yeff
            IF ( weight.LT.DT_RNDM(q2p2) ) GOTO 220
         ELSE
            q2p2 = q2low2
         END IF
C  impact parameter
         GAImp(2) = 1.D0/SQRT(q2p2)
C  form factor (squared)
         ff2 = 1.D0
         IF ( GAImp(2).LT.2.D0*hiradi ) ff2 = 0.D0
         IF ( DT_RNDM(q2p2).GE.ff2 ) GOTO 200
C  photon data
         GYY(2) = y2
         GQ2(2) = q2p2
 
C
C  incoming hadron 1
         PINi(1,1) = 0.D0
         PINi(2,1) = 0.D0
         PINi(3,1) = SQRT(Eep**2-amp2)
         PINi(4,1) = Eep
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
         p1(1) = 0.D0
         p1(2) = 0.D0
         p1(3) = SQRT(Eep**2-amp2)
         p1(4) = Eep
         q2p1 = amp2
C  scattering photon
         p2(1) = -PFIn(1,2)
         p2(2) = -PFIn(2,2)
         p2(3) = PINi(3,2) - PFIn(3,2)
         p2(4) = PINi(4,2) - PFIn(4,2)
         iside = 2
C
C  ECMS cut
         GGEcm = (p1(4)+p2(4))**2 - (p1(1)+p2(1))**2 - (p1(2)+p2(2))
     &           **2 - (p1(3)+p2(3))**2
         IF ( GGEcm.LT.0.1D0 ) GOTO 200
         GGEcm = SQRT(GGEcm)
         IF ( (GGEcm.LT.ECMin) .OR. (GGEcm.GT.ECMax) ) GOTO 200
C
         PGAm(1,1) = p1(1)
         PGAm(2,1) = p1(2)
         PGAm(3,1) = p1(3)
         PGAm(4,1) = p1(4)
         PGAm(5,1) = amp
         PGAm(1,2) = p2(1)
         PGAm(2,2) = p2(2)
         PGAm(3,2) = p2(3)
         PGAm(4,2) = p2(4)
         PGAm(5,2) = -SQRT(q2p2)
C  photon helicities
         IGHel(2) = 1
C  user cuts
         CALL PHO_PRESEL(5,irej)
         IF ( irej.NE.0 ) GOTO 200
C  event generation
         CALL PHO_EVENT(1,p1,p2,sigcur,irej)
         IF (Nprint.LE.10) THEN
            CALL PHO_PREVNT(2)
            Nprint = Nprint + 1
         END IF
         
         IF ( irej.EQ.0 ) THEN
C  cut on diffractive mass
            DO k = 1 , NHEp
               IF ( ISThep(k).EQ.30 ) THEN
                  ghdiff = PHEp(1,k)
                  IF ( ghdiff.GE.PARmdl(175) ) GOTO 250
                  GOTO 150
               END IF
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A)')
     &            'PHO_GHHIAS: no diffractive entry found'
            CALL PHO_PREVNT(-1)
         END IF
         GOTO 150
C  remove quasi-elastically scattered hadron
 250     DO k = 1 , NHEp
            IF ( (ISThep(k).EQ.1) .AND. (IDHep(k).EQ.2212) ) THEN
               xf = ABS(PHEp(3,k)/Een)
C           ISTHEP(K) = 2
               IF ( xf.LT.PARmdl(72) ) GOTO 150
               GOTO 300
            END IF
         END DO
C
C  statistics
 
 300     niters = niters + 1
 
         ay2 = ay2 + y2
         ays2 = ays2 + y2*y2
         q22ave = q22ave + q2p2
         q22av2 = q22av2 + q2p2*q2p2
         q22min = MIN(q22min,q2p2)
         q22max = MAX(q22max,q2p2)
         yy2min = MIN(yy2min,y2)
         yy2max = MAX(yy2max,y2)
C  histograms
         CALL PHO_PHIST(1,HSWght(0))
         CALL PHO_LHIST(1,HSWght(0))
      END DO
C
      wgmax = wgmax2*LOG(YMAx2/YMIn2)
      wgy2 = wgmax*DBLE(itry)/DBLE(MAX(itrw,1))*alpha/(2.D0*PI)
      ay2 = ay2/DBLE(MAX(niters,1))
      ays2 = ays2/DBLE(MAX(niters,1))
      day2 = SQRT((ays2-ay2**2)/DBLE(MAX(niters,1)))
      q22ave = q22ave/DBLE(MAX(niters,1))
      q22av2 = q22av2/DBLE(MAX(niters,1))
      q22av2 = SQRT((q22av2-q22ave**2)/DBLE(MAX(niters,1)))
      wgmax = wgmax2*LOG(YMAx2/YMIn2)
      wgy = wgmax*DBLE(itry)/DBLE(itrw)*alpha/(2.D0*PI)
      weight = wgy*sigmax*DBLE(niter)/DBLE(itry)
C  output of statistics, histograms
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/1X,A,1PE12.3,A,/1X,A)') 
     &       '========================================================='
     &       , ' *****   simulated cross section: ' , weight , 
     &       ' mb  *****' , 
     &       '========================================================='
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/3X,4I12)') 
     &        'PHO_GHHIAS:SUMMARY:  NITER,    NITERS,    ITRY,     ITRW'
     &        , niter , niters , itry , itrw
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'EFFECTIVE WEIGHT (FLUX,TOTAL)' , wgy , 
     &                        weight
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'AVERAGE Y2,DY2               ' , ay2 , 
     &                        day2
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'SAMPLED Y RANGE PHOTON 2     ' , 
     &                        yy2min , yy2max
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
     &         'PHO_GHHIAS: no output of histograms' , niter
      END IF
 
      END SUBROUTINE
