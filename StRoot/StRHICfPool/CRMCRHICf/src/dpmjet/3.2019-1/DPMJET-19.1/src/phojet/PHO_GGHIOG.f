
      SUBROUTINE PHO_GGHIOG(Nevent,Een,Na,Nz)
C**********************************************************************
C
C     interface to call PHOJET (variable energy run) for
C     gamma-gamma collisions via heavy ions (geometrical approach)
C
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
C
C      currently implemented approximation similar to:
C
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alpha , ay1 , ay2 , ays1 , ays2 , day1 , day2 , 
     &                 delly , DEPS , DT_RNDM , e1y , ee , Een , egam , 
     &                 fcorr , ff , ffmax , flux , flx , fm2gev
      DOUBLE PRECISION gamma , hima2 , himass , hiradi , p1 , p2 , 
     &                 PHO_GGFLCL , PHO_GGFLCR , PI , sigcur , sigmax , 
     &                 tabcu , tabyl , weight , wg , wgx , wgy , xi , 
     &                 xi1 , xi2
      DOUBLE PRECISION y , y1 , y2 , ymax , ymi , ymin , yy1max , 
     &                 yy1min , yy2max , yy2min
      INTEGER i , irej , itrw , itry , k , MAX_TAB , Na , Nevent , 
     &        niter , Nz
      SAVE 
 
      PARAMETER (DEPS=1.D-20,PI=3.141592653589793238462643383279D+00)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  photon flux kinematics and cuts
      INCLUDE 'inc/pofcut'
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      PARAMETER (MAX_TAB=100)
      DIMENSION p1(4) , p2(4) , tabcu(0:MAX_TAB) , tabyl(0:MAX_TAB)
 
C
      IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))') 
     &                        'PHO_GGHIOG: gamma-gamma event generation'
     &                        , 
     &                        '---------------------------------------'
C  hadron size and mass
      fm2gev = 5.07D0
      himass = DBLE(Na)*0.938D0
      hima2 = himass**2
      hiradi = 1.2D0*fm2gev*DBLE(Na)**0.333
      alpha = DBLE(Nz**2)/137.D0
C  total hadron / heavy ion energy
      ee = Een*DBLE(Na)
      gamma = ee/himass
C  setup /POFSRC/
      GAMsrc(1) = gamma
      GAMsrc(2) = gamma
      RADsrc(1) = hiradi
      RADsrc(2) = hiradi
      AMSrc(1) = himass
      AMSrc(1) = himass
C  kinematic limitations
      ymi = (ECMin/(2.D0*ee))**2
      IF ( YMIn1.LT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &         'PHO_GGHIOG: ymin1 increased to (old/new)' , YMIn1 , ymi
         YMIn1 = ymi
      ELSE IF ( YMIn1.GT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,E12.5,A,E12.5)')
     &         'PHO_GGHIOG:' , 'ECM-CUT CORRESPONDS TO YMIN1 OF' , ymi , 
     &        '  INSTEAD OF' , YMIn1
      END IF
      IF ( YMIn2.LT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.5)')
     &         'PHO_GGHIOG: ymin2 increased to (old/new)' , YMIn2 , ymi
         YMIn2 = ymi
      ELSE IF ( YMIn2.GT.ymi ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,E12.5,A,E12.5)')
     &         'PHO_GGHIOG:' , 'ECM-CUT CORRESPONDS TO YMIN2 OF' , ymi , 
     &        '  INSTEAD OF' , YMIn2
      END IF
C  debug output
      IF ( LPRi.GT.4 ) WRITE (LO,'(/6X,A,2I4)')
     &                         'MASS NUMBER, CHARGE NUMBER  ' , Na , Nz
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,E12.5)')
     &                         'HADRON MASS (GeV)           ' , himass
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,E12.5)')
     &                         'HADRON RADIUS (GeV**-1)     ' , hiradi
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,E12.5)')
     &                         'LORENTZ GAMMA               ' , gamma
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'Y RANGE PHOTON 1            ' , YMIn1 , 
     &                        YMAx1
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'Y RANGE PHOTON 2            ' , YMIn2 , 
     &                        YMAx2
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,2E12.5)')
     &                         'SQRT(S) PER NUCLEON, TOTAL  ' , 
     &                        2.D0*Een , 2.D0*ee
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,I10)')
     &                         'EVENTS TO PROCESS           ' , Nevent
C  hadron numbers set to 0
      IDPsrc(1) = 0
      IDBsrc(1) = 0
      IDPsrc(2) = 0
      IDBsrc(2) = 0
C  table of flux function, log interpolation
      ymin = YMIn1
      ymax = YMAx1
      ymax = MIN(ymax,0.9999999D0)
      delly = LOG(ymax/ymin)/DBLE(MAX_TAB-1)
      tabyl(0) = LOG(ymin)
      ffmax = 0.D0
      DO i = 1 , MAX_TAB
         y = EXP(tabyl(0)+delly*DBLE(i-1))
         wg = ee*y
         xi = wg*hiradi/gamma
         ff = alpha*PHO_GGFLCL(xi)/y
         ffmax = MAX(ff,ffmax)
         IF ( ff.LT.1.D-10*ffmax ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &            'PHO_GGHIOG: ymax1 changed from/to' , YMAx1 , y
            YMAx1 = MIN(y,YMAx1)
            GOTO 100
         END IF
      END DO
 100  ymin = YMIn2
      ymax = YMAx2
      ymax = MIN(ymax,0.9999999D0)
      delly = LOG(ymax/ymin)/DBLE(MAX_TAB-1)
      tabyl(0) = LOG(ymin)
      ffmax = 0.D0
      DO i = 1 , MAX_TAB
         y = EXP(tabyl(0)+delly*DBLE(i-1))
         wg = ee*y
         xi = wg*hiradi/gamma
         ff = alpha*PHO_GGFLCL(xi)/y
         ffmax = MAX(ff,ffmax)
         IF ( ff.LT.1.D-10*ffmax ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &            'PHO_GGHIOG: ymax2 changed from/to' , YMAx2 , y
            YMAx2 = MIN(y,YMAx2)
            GOTO 200
         END IF
      END DO
 200  ymi = (ECMin/(2.D0*ee))**2/YMAx2
      IF ( ymi.GT.YMIn1 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &         'PHO_GGHIOG: ymin1 changed from/to' , YMIn1 , ymi
         YMIn1 = ymi
      END IF
      YMAx1 = MIN(ymax,YMAx1)
      ymi = (ECMin/(2.D0*ee))**2/YMAx1
      IF ( ymi.GT.YMIn2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &         'PHO_GGHIOG: ymin2 changed from/to' , YMIn2 , ymi
         YMIn2 = ymi
      END IF
C
      ymin = YMIn1
      ymax = YMAx1
      delly = LOG(ymax/ymin)/DBLE(MAX_TAB-1)
      tabcu(0) = 0.D0
      tabyl(0) = LOG(ymin)
      flux = 0.D0
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(/1X,A,I5)')
     &      'PHO_GGHIOG: table of raw photon flux (side 1)' , MAX_TAB
      DO i = 1 , MAX_TAB
         y = EXP(tabyl(0)+delly*DBLE(i-1))
         wg = ee*y
         xi = wg*hiradi/gamma
         ff = alpha*PHO_GGFLCL(xi)/y
         ffmax = MAX(ff,ffmax)
         tabcu(i) = tabcu(i-1) + ff*y
         tabyl(i) = LOG(y)
         flux = flux + y*ff
         IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(5X,2E15.4)')
     &        y , ff
      END DO
      flux = flux*delly
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(1X,A,E12.4)')
     &      'PHO_GGHIOG: integrated flux (one side):' , flux
C
C  initialization
C  photon 1
      egam = ymax*ee
      p1(1) = 0.D0
      p1(2) = 0.D0
      p1(3) = egam
      p1(4) = egam
C  photon 2
      egam = ymax*ee
      p2(1) = 0.D0
      p2(2) = 0.D0
      p2(3) = -egam
      p2(4) = egam
      CALL PHO_SETPAR(1,22,0,0.D0)
      CALL PHO_SETPAR(2,22,0,0.D0)
      CALL PHO_EVENT(-1,p1,p2,sigmax,irej)
      CALL PHO_PHIST(-1,sigmax)
      CALL PHO_LHIST(-1,sigmax)
C
C  generation of events
 
      ay1 = 0.D0
      ay2 = 0.D0
      ays1 = 0.D0
      ays2 = 0.D0
      yy1min = 1.D30
      yy2min = 1.D30
      yy1max = 0.D0
      yy2max = 0.D0
      niter = Nevent
      itry = 0
      itrw = 0
      DO i = 1 , niter
 250     itry = itry + 1
 300     itrw = itrw + 1
         xi = DT_RNDM(ay1)*tabcu(MAX_TAB)
         DO k = 1 , MAX_TAB
            IF ( tabcu(k).GE.xi ) THEN
               y1 = delly/(tabcu(k)-tabcu(k-1))*(xi-tabcu(k-1))
     &              + tabyl(k-1)
               y1 = EXP(y1)
               GOTO 350
            END IF
         END DO
         y1 = YMAx1
 350     xi = DT_RNDM(ay2)*tabcu(MAX_TAB)
         DO k = 1 , MAX_TAB
            IF ( tabcu(k).GE.xi ) THEN
               y2 = delly/(tabcu(k)-tabcu(k-1))*(xi-tabcu(k-1))
     &              + tabyl(k-1)
               y2 = EXP(y2)
               GOTO 400
            END IF
         END DO
         y2 = YMAx2
C  setup kinematics
 
 400     GYY(1) = y1
         GQ2(1) = 0.D0
         GYY(2) = y2
         GQ2(2) = 0.D0
C  incoming electron 1
         PINi(1,1) = 0.D0
         PINi(2,1) = 0.D0
         PINi(3,1) = ee
         PINi(4,1) = ee
         PINi(5,1) = 0.D0
C  outgoing electron 1
         e1y = ee*(1.D0-y1)
         PFIn(1,1) = 0.D0
         PFIn(2,1) = 0.D0
         PFIn(3,1) = e1y
         PFIn(4,1) = e1y
         PFIn(5,1) = 0.D0
C  photon 1
         p1(1) = -PFIn(1,1)
         p1(2) = -PFIn(2,1)
         p1(3) = PINi(3,1) - PFIn(3,1)
         p1(4) = PINi(4,1) - PFIn(4,1)
C  incoming electron 2
         PINi(1,2) = 0.D0
         PINi(2,2) = 0.D0
         PINi(3,2) = -ee
         PINi(4,2) = ee
         PINi(5,2) = 0.D0
C  outgoing electron 2
         e1y = ee*(1.D0-y2)
         PFIn(1,2) = 0.D0
         PFIn(2,2) = 0.D0
         PFIn(3,2) = -e1y
         PFIn(4,2) = e1y
         PFIn(5,2) = 0.D0
C  photon 2
         p2(1) = -PFIn(1,2)
         p2(2) = -PFIn(2,2)
         p2(3) = PINi(3,2) - PFIn(3,2)
         p2(4) = PINi(4,2) - PFIn(4,2)
C  ECMS cut
         GGEcm = (p1(4)+p2(4))**2 - (p1(3)+p2(3))**2
         IF ( GGEcm.LT.0.1D0 ) GOTO 300
         GGEcm = SQRT(GGEcm)
         IF ( (GGEcm.LT.ECMin) .OR. (GGEcm.GT.ECMax) ) GOTO 300
         PGAm(1,1) = p1(1)
         PGAm(2,1) = p1(2)
         PGAm(3,1) = p1(3)
         PGAm(4,1) = p1(4)
         PGAm(5,1) = 0.D0
         PGAm(1,2) = p2(1)
         PGAm(2,2) = p2(2)
         PGAm(3,2) = p2(3)
         PGAm(4,2) = p2(4)
         PGAm(5,2) = 0.D0
C  impact parameter constraints
         xi1 = p1(4)*hiradi/gamma
         xi2 = p2(4)*hiradi/gamma
         flx = PHO_GGFLCL(xi1)*PHO_GGFLCL(xi2)
         fcorr = PHO_GGFLCR(hiradi)
         wgx = (flx-fcorr)/flx
         IF ( DT_RNDM(y2).GT.wgx ) GOTO 300
C  photon helicities
         IGHel(1) = 1
         IGHel(2) = 1
C  cut given by user
         CALL PHO_PRESEL(5,irej)
         IF ( irej.NE.0 ) GOTO 300
C  event generation
         CALL PHO_EVENT(1,p1,p2,sigcur,irej)
         IF ( irej.NE.0 ) GOTO 250
 
C  statistics
         ay1 = ay1 + y1
         ays1 = ays1 + y1*y1
         ay2 = ay2 + y2
         ays2 = ays2 + y2*y2
         yy1min = MIN(yy1min,y1)
         yy2min = MIN(yy2min,y2)
         yy1max = MAX(yy1max,y1)
         yy2max = MAX(yy2max,y2)
C  histograms
         CALL PHO_PHIST(1,HSWght(0))
         CALL PHO_LHIST(1,HSWght(0))
      END DO
C
      wgy = flux**2*DBLE(itry)/DBLE(itrw)
      ay1 = ay1/DBLE(niter)
      ays1 = ays1/DBLE(niter)
      day1 = SQRT((ays1-ay1**2)/DBLE(niter))
      ay2 = ay2/DBLE(niter)
      ays2 = ays2/DBLE(niter)
      day2 = SQRT((ays2-ay2**2)/DBLE(niter))
      weight = wgy*sigmax*DBLE(niter)/DBLE(itry)
C  output of statistics, histograms
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/1X,A,1PE12.3,A,/1X,A)') 
     &       '========================================================='
     &       , ' *****   simulated cross section: ' , weight , 
     &       ' mb  *****' , 
     &       '========================================================='
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,3I12)')
     &                         'PHO_GGHIOG:SUMMARY:NITER,ITRY,ITRW' , 
     &                        niter , itry , itrw
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
 
C
      CALL PHO_EVENT(-2,p1,p2,weight,irej)
      IF ( niter.GT.1 ) THEN
         CALL PHO_PHIST(-2,weight)
         CALL PHO_LHIST(-2,weight)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &         'PHO_GGHIOG:NO OUTPUT OF HISTOGRAMS' , niter
      END IF
 
      END SUBROUTINE
