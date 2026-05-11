
      SUBROUTINE PHO_GGBEAM(Nevent,Ee,Ypsi,Sigx,Sigy,Sigz,Aeb)
C**********************************************************************
C
C     interface to call PHOJET (variable energy run) for
C     gamma-gamma collisions via beamstrahlung
C
C     input:     EE      LAB system energy of electron/positron
C                YPSI    beamstrahlung parameter
C                SIGX,Y  transverse bunch dimensions
C                SIGZ    longitudinal bunch dimension
C                AEB     number of electrons/positrons in a bunch
C                NEVENT  number of events to generate
C            from /LEPCUT/:
C                YMIN1   lower limit of Y
C                        (energy fraction taken by photon from electron)
C                YMAX1   upper cutoff for Y, necessary to avoid
C                        underflows
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Aeb , akap , angam , ay1 , ay2 , ays1 , ays2 , 
     &                 cof , day1 , day2 , delly , DEPS , DT_RNDM , 
     &                 e1y , Ee , ee1 , ee2 , egam , elem , ff
      DOUBLE PRECISION flux , gaot , gg , ggecml , ot , p1 , p2 , PI , 
     &                 q2e , q2p1 , q2p2 , re , sif , sigcur , sigmax , 
     &                 Sigx , Sigy , Sigz , tabcu , tabyl
      DOUBLE PRECISION tt , weight , wgy , ww , xi , y , y1 , y2 , 
     &                 ymax , ymin , Ypsi , yq2
      INTEGER i , irej , itrw , itry , k , MAX_TAB , Nevent , niter
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
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,I10)')
     &                         'PHO_GGBEAM: events to process' , Nevent
C  electron data
      re = 2.818D-12
      elem = 0.512D-03
      IDPsrc(1) = 0
      IDBsrc(1) = 0
      IDPsrc(2) = 0
      IDBsrc(2) = 0
C  table of flux function, log interpolation
      IF ( Ypsi.LE.0.D0 )
     &     Ypsi = 5.D0*re**2*Ee*Aeb*137.D0/(6.D0*Sigz*(Sigx+Sigy)*elem)
      IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,E12.4)')
     &                         'PHO_GGBEAM: beamstrahlung parameter:' , 
     &                        Ypsi
      IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &                         'PHO_GGBEAM: sigma-z,ne-bunch:' , Sigz , 
     &                        Aeb
      tt = 2.D0/3.D0
      ot = 1.D0/3.D0
C     GAOT  = DGAMMA(OT)
      gaot = 2.6789385347D0
      akap = tt/Ypsi
      ww = 1.D0/(6.D0*SQRT(akap))
      angam = 5.D0*Sigz*elem/(137.D0**2*2.D0*re*Ee)
     &        *Ypsi/SQRT(1.D0+Ypsi**tt)
 
      ymin = YMIn1
      ymax = MIN(YMAx1,0.9D0)
      tabcu(0) = 0.D0
      tabyl(0) = LOG(ymin)
      delly = LOG(ymax/ymin)/DBLE(MAX_TAB-1)
      flux = 0.D0
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(1X,A,I5)')
     &      'PHO_GGBEAM: table of photon flux' , MAX_TAB
      DO i = 1 , MAX_TAB
         y = EXP(tabyl(0)+delly*DBLE(i-1))
         gg = 1.D0 - 0.5D0*(1-y)
     &        **tt*(1.D0-y+(1.D0+y)*SQRT(1.D0+Ypsi**tt))
         ff = akap**ot/gaot/y**tt/(1.D0-y)**ot*EXP(-akap*y/(1.D0-y))
     &        *((1.D0-ww)/gg*(1.D0-(1.D0-EXP(-angam*gg))/(angam*gg))
     &        +ww*(1.D0-(1.D0-EXP(-angam))/angam))
         tabcu(i) = tabcu(i-1) + ff*y
         tabyl(i) = LOG(y)
         flux = flux + y*ff
         IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(5X,2E15.4)')
     &        y , ff
      END DO
      flux = flux*delly
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(1X,A,E12.4)')
     &      'PHO_GGBEAM: integrated flux (one side):' , flux
 
      ee1 = Ee
      ee2 = Ee
C  photon 1
      egam = ymax*Ee
      p1(1) = 0.D0
      p1(2) = 0.D0
      p1(3) = egam
      p1(4) = egam
C  photon 2
      egam = ymax*Ee
      p2(1) = 0.D0
      p2(2) = 0.D0
      p2(3) = -egam
      p2(4) = egam
      CALL PHO_SETPAR(1,22,0,0.D0)
      CALL PHO_SETPAR(2,22,0,0.D0)
      CALL PHO_EVENT(-1,p1,p2,sigmax,irej)
      CALL PHO_PHIST(-1,sigmax)
      CALL PHO_LHIST(-1,sigmax)
 
C  generation of events
 
      ay1 = 0.D0
      ay2 = 0.D0
      ays1 = 0.D0
      ays2 = 0.D0
      niter = Nevent
      itry = 0
      itrw = 0
      DO i = 1 , niter
 50      itry = itry + 1
 100     itrw = itrw + 1
         xi = DT_RNDM(ay1)*tabcu(MAX_TAB)
         DO k = 1 , MAX_TAB
            IF ( tabcu(k).GE.xi ) THEN
               y1 = delly/(tabcu(k)-tabcu(k-1))*(xi-tabcu(k-1))
     &              + tabyl(k-1)
               y1 = EXP(y1)
               GOTO 150
            END IF
         END DO
         y1 = ymax
 150     xi = DT_RNDM(ay2)*tabcu(MAX_TAB)
         DO k = 1 , MAX_TAB
            IF ( tabcu(k).GE.xi ) THEN
               y2 = delly/(tabcu(k)-tabcu(k-1))*(xi-tabcu(k-1))
     &              + tabyl(k-1)
               y2 = EXP(y2)
               GOTO 200
            END IF
         END DO
         y2 = ymax
 
 200     q2p1 = 0.D0
         q2p2 = 0.D0
         GYY(1) = y1
         GQ2(1) = q2p1
         GYY(2) = y2
         GQ2(2) = q2p2
C  incoming electron 1
         PINi(1,1) = 0.D0
         PINi(2,1) = 0.D0
         PINi(3,1) = ee1
         PINi(4,1) = ee1
         PINi(5,1) = 0.D0
C  outgoing electron 1
         yq2 = SQRT((1.D0-y1)*q2p2)
         q2e = q2p1/(4.D0*ee1)
         e1y = ee1*(1.D0-y1)
         CALL PHO_SFECFE(sif,cof)
         PFIn(1,1) = yq2*cof
         PFIn(2,1) = yq2*sif
         PFIn(3,1) = e1y - q2e
         PFIn(4,1) = e1y + q2e
         PFIn(5,1) = 0.D0
C  photon 1
         p1(1) = -PFIn(1,1)
         p1(2) = -PFIn(2,1)
         p1(3) = PINi(3,1) - PFIn(3,1)
         p1(4) = PINi(4,1) - PFIn(4,1)
C  incoming electron 2
         PINi(1,2) = 0.D0
         PINi(2,2) = 0.D0
         PINi(3,2) = -ee2
         PINi(4,2) = ee2
         PINi(5,2) = 0.D0
C  outgoing electron 2
         yq2 = SQRT((1.D0-y2)*q2p2)
         q2e = q2p2/(4.D0*ee2)
         e1y = ee2*(1.D0-y2)
         CALL PHO_SFECFE(sif,cof)
         PFIn(1,2) = yq2*cof
         PFIn(2,2) = yq2*sif
         PFIn(3,2) = -e1y + q2e
         PFIn(4,2) = e1y + q2e
         PFIn(5,2) = 0.D0
C  photon 2
         p2(1) = -PFIn(1,2)
         p2(2) = -PFIn(2,2)
         p2(3) = PINi(3,2) - PFIn(3,2)
         p2(4) = PINi(4,2) - PFIn(4,2)
C  ECMS cut
         GGEcm = (p1(4)+p2(4))**2 - (p1(1)+p2(1))**2 - (p1(2)+p2(2))
     &           **2 - (p1(3)+p2(3))**2
         IF ( GGEcm.LT.0.1D0 ) GOTO 100
         GGEcm = SQRT(GGEcm)
         IF ( (GGEcm.LT.ECMin) .OR. (GGEcm.GT.ECMax) ) GOTO 100
C
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
C  photon helicities
         IGHel(1) = 1
         IGHel(2) = 1
C  cut given by user
         CALL PHO_PRESEL(5,irej)
         IF ( irej.NE.0 ) GOTO 100
C  event generation
         CALL PHO_EVENT(1,p1,p2,sigcur,irej)
         IF ( irej.NE.0 ) GOTO 50
         ggecml = LOG(GGEcm)
 
C  statistics
         ay1 = ay1 + y1
         ays1 = ays1 + y1*y1
         ay2 = ay2 + y2
         ays2 = ays2 + y2*y2
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
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,2I10)')
     &                         'PHO_GGBEAM:SUMMARY:NITER,ITRY' , niter , 
     &                        itry
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'EFFECTIVE WEIGHT (FLUX,TOTAL)' , wgy , 
     &                        weight
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2F10.5)')
     &                         'PHO_GGBEAM:AVERAGE Y1,DY1 ' , ay1 , day1
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2F10.5)')
     &                         'PHO_GGBEAM:AVERAGE Y2,DY2 ' , ay2 , day2
C
      CALL PHO_EVENT(-2,p1,p2,weight,irej)
      IF ( niter.GT.1 ) THEN
         CALL PHO_PHIST(-2,weight)
         CALL PHO_LHIST(-2,weight)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &         'PHO_GGBEAM:NO OUTPUT OF HISTOGRAMS' , niter
      END IF
 
      END SUBROUTINE
