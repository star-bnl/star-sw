
      SUBROUTINE PHO_GPHERA(Nevent,Ee1,Ee2)
C**********************************************************************
C
C     interface to call PHOJET (variable energy run) with
C     HERA kinematics, photon as particle 2
C
C     equivalent photon approximation to get photon flux
C
C     input:     NEVENT  number of events to generate
C                EE1     proton energy (LAB system)
C                EE2     electron energy (LAB system)
C             from /POFCUT/:
C                YMIN2    lower limit of Y
C                        (energy fraction taken by photon from electron)
C                YMAX2    upper limit of Y
C                Q2MIN2   lower limit of photon virtuality
C                Q2MAX2   upper limit of photon virtuality
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION an2max , an2min , ay , ay2 , cof , day , delly , 
     &                 DEPS , DT_RNDM , e1y , ecmax2 , ecmin2 , Ee1 , 
     &                 Ee2 , egam , elem , elem2 , ffl , fft , fluxl
      DOUBLE PRECISION fluxt , p1 , p2 , PHO_PMASS , PI , prom , prom2 , 
     &                 q2 , q22av2 , q22ave , q22max , q22min , q2e , 
     &                 q2log , q2low , q2max , q2min , sif , sigcur , 
     &                 sigmax
      DOUBLE PRECISION weight , wgh , wghl , wgmax , wgy , xidel , 
     &                 ximax , ximin , y , yeff , yq2 , yy , yy2max , 
     &                 yy2min
      INTEGER i , IPHO_PDG2ID , irej , itrw , itry , max_tab , Nevent , 
     &        niter
      SAVE 
 
      PARAMETER (DEPS=1.D-10,PI=3.141592653589793238462643383279D+00)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  photon flux kinematics and cuts
      INCLUDE 'inc/pofcut'
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      DIMENSION p1(4) , p2(4)
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,I10)')
     &                         'PHO_GPHERA: events to process' , Nevent
C  assign particle momenta according to HERA kinematics
C  proton data
      prom = PHO_PMASS(2212,1)
      prom2 = prom**2
      IDPsrc(1) = 0
      IDBsrc(1) = 0
C  electron data
      elem = 0.512D-03
      elem2 = elem**2
      AMSrc(2) = elem
      IDPsrc(2) = 11
      IDBsrc(2) = IPHO_PDG2ID(11)
C
      q2min = Q2Min2
      q2max = Q2Max2
C
      ximax = LOG(YMAx2)
      ximin = LOG(YMIn2)
      xidel = ximax - ximin
C
      IF ( q2min.GT.elem2*YMIn2**2/(1.D0-YMIn2) .AND. LPRi.GT.4 )
     &     WRITE (LO,'(/1X,A,1P2E11.4)')
     &             'PHO_GPHERA: lower Q2 cutoff larger than kin. limit:'
     &            , q2min , elem2*YMIn2**2/(1.D0-YMIn2)
C
      max_tab = 50
      delly = LOG(YMAx2/YMIn2)/DBLE(max_tab-1)
      fluxt = 0.D0
      fluxl = 0.D0
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(1X,A,I5)')
     &      'PHO_GPHERA: table of photon flux (trans/long)' , max_tab
      DO i = 1 , max_tab
         y = EXP(ximin+delly*DBLE(i-1))
         q2low = MAX(q2min,elem2*y**2/(1.D0-y))
         fft = ((1.D0+(1.D0-y)**2)/y*LOG(q2max/q2low)
     &         -2.D0*elem2*y*(1.D0/q2low-1.D0/q2max))/(2.D0*PI*137.D0)
         ffl = 2.D0*(1.D0-y)/y*LOG(q2max/q2low)/(2.D0*PI*137.D0)
         fluxt = fluxt + y*fft
         fluxl = fluxl + y*ffl
         IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(5X,1P3E14.4)')
     &        y , fft , ffl
      END DO
      fluxt = fluxt*delly
      fluxl = fluxl*delly
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &      'PHO_GPHERA: integrated flux (trans./long.):' , fluxt , 
     &     fluxl
C
      ay = 0.D0
      ay2 = 0.D0
      yy = YMIn2
      q2low = MAX(q2min,elem2*yy**2/(1.D0-yy))
      wgmax = (1.D0+(1.D0-yy)**2)*LOG(q2max/q2low)
     &        - 2.D0*elem2*yy*(1.D0/q2low-1.D0/q2max)*yy
      IF ( ISWmdl(10).GE.2 ) wgmax = wgmax + 2.D0*(1.D0-yy)
     &     *LOG(q2max/q2low)
C
C  initialization of PHOJET at upper energy limit
C  proton momentum
      p1(1) = 0.D0
      p1(2) = 0.D0
      p1(3) = SQRT(Ee1**2-prom2+DEPS)
      p1(4) = Ee1
C  photon momentum
      egam = YMAx2*Ee2
      p2(1) = 0.D0
      p2(2) = 0.D0
      p2(3) = -egam
      p2(4) = egam
C  sum of both photon polarizations
      IGHel(2) = -1
C
      CALL PHO_SETPAR(1,2212,0,0.D0)
      CALL PHO_SETPAR(2,22,0,0.D0)
      CALL PHO_EVENT(-1,p1,p2,sigmax,irej)
      CALL PHO_PHIST(-1,sigmax)
      CALL PHO_LHIST(-1,sigmax)
C
C  generation of events, flux calculation
 
      ecmin2 = ECMin**2
      ecmax2 = ECMax**2
      ay = 0.D0
      ay2 = 0.D0
      q22min = 1.D30
      q22ave = 0.D0
      q22av2 = 0.D0
      q22max = 0.D0
      an2min = 1.D30
      an2max = 0.D0
      yy2min = 1.D30
      yy2max = 0.D0
      niter = Nevent
      itry = 0
      itrw = 0
      DO i = 1 , niter
C  sample y
 50      itry = itry + 1
 100     itrw = itrw + 1
         yy = EXP(xidel*DT_RNDM(ay)+ximin)
         IF ( ISWmdl(10).GE.2 ) THEN
            yeff = 1.D0 + (1.D0-yy)**2 + 2.D0*(1.D0-yy)
         ELSE
            yeff = 1.D0 + (1.D0-yy)**2
         END IF
         q2low = MAX(q2min,elem2*yy**2/(1.D0-yy))
         q2log = LOG(q2max/q2low)
         wgh = yeff*q2log - 2.D0*elem2*yy**2*(1.D0/q2low-1.D0/q2max)
         IF ( wgmax.LT.wgh ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3E12.5)')
     &            'PHO_GPHERA: inconsistent weight:' , yy , wgmax , wgh
         END IF
         IF ( DT_RNDM(ay2)*wgmax.GT.wgh ) GOTO 100
C  sample Q2
         IF ( IPAmdl(174).EQ.1 ) THEN
 120        q2 = q2low*EXP(q2log*DT_RNDM(yy))
            weight = (yeff-2.D0*elem2*yy**2/q2)/yeff
            IF ( weight.LT.DT_RNDM(q2) ) GOTO 120
         ELSE
            q2 = q2low
         END IF
C
 
C  incoming electron
         PINi(1,2) = 0.D0
         PINi(2,2) = 0.D0
         PINi(3,2) = -Ee2
         PINi(4,2) = Ee2
         PINi(5,2) = 0.D0
C  outgoing electron
         yq2 = SQRT((1.D0-yy)*q2)
         q2e = q2/(4.D0*Ee2)
         e1y = Ee2*(1.D0-yy)
         CALL PHO_SFECFE(sif,cof)
         PFIn(1,2) = yq2*cof
         PFIn(2,2) = yq2*sif
         PFIn(3,2) = -e1y + q2e
         PFIn(4,2) = e1y + q2e
         PFIn(5,2) = 0.D0
C  set /POFSRC/
         GYY(2) = yy
         GQ2(2) = q2
C  polar angle
         PFThe(2) = ACOS(PFIn(3,2)/PFIn(4,2))
C  electron tagger
         IF ( PFIn(4,2).GT.EEMin2 ) THEN
            IF ( (PFThe(2).LT.THMin2) .OR. (PFThe(2).GT.THMax2) )
     &           GOTO 100
         END IF
C  azimuthal angle
         PFPhi(2) = ATAN2(cof,sif)
C  photon momentum
         p2(1) = -PFIn(1,2)
         p2(2) = -PFIn(2,2)
         p2(3) = PINi(3,2) - PFIn(3,2)
         p2(4) = PINi(4,2) - PFIn(4,2)
C  proton momentum
         p1(1) = 0.D0
         p1(2) = 0.D0
         p1(3) = SQRT(Ee1**2-prom2)
         p1(4) = Ee1
C  ECMS cut
         GGEcm = (p1(4)+p2(4))**2 - (p1(1)+p2(1))**2 - (p1(2)+p2(2))
     &           **2 - (p1(3)+p2(3))**2
         IF ( (GGEcm.LT.ecmin2) .OR. (GGEcm.GT.ecmax2) ) GOTO 100
         GGEcm = SQRT(GGEcm)
C
         PGAm(1,2) = p2(1)
         PGAm(2,2) = p2(2)
         PGAm(3,2) = p2(3)
         PGAm(4,2) = p2(4)
         PGAm(5,2) = -SQRT(q2)
C  photon helicity
         IF ( ISWmdl(10).GE.2 ) THEN
            wgh = yeff - 2.D0*elem2*yy**2/q2
            wghl = 2.D0*(1-yy)
            IF ( DT_RNDM(yy).GE.wghl/wgh ) THEN
               IGHel(2) = 1
            ELSE
               IGHel(2) = 0
            END IF
         ELSE
            IGHel(2) = -1
         END IF
C  user cuts
         CALL PHO_PRESEL(5,irej)
         IF ( irej.NE.0 ) GOTO 100
C  event generation
         CALL PHO_EVENT(1,p1,p2,sigcur,irej)
         IF ( irej.NE.0 ) GOTO 50
 
C  statistics
         ay = ay + yy
         ay2 = ay2 + yy*yy
         yy2min = MIN(yy2min,yy)
         yy2max = MAX(yy2max,yy)
         q22min = MIN(q22min,q2)
         q22max = MAX(q22max,q2)
         q22ave = q22ave + q2
         q22av2 = q22av2 + q2*q2
         an2min = MIN(an2min,PFThe(2))
         an2max = MAX(an2max,PFThe(2))
C  histograms
         CALL PHO_PHIST(1,HSWght(0))
         CALL PHO_LHIST(1,HSWght(0))
      END DO
C
      wgy = wgmax*DBLE(itry)/DBLE(itrw)/(137.D0*2.D0*PI)
      wgy = wgy*LOG(YMAx2/YMIn2)
      ay = ay/DBLE(niter)
      ay2 = ay2/DBLE(niter)
      day = SQRT((ay2-ay**2)/DBLE(niter))
      q22ave = q22ave/DBLE(niter)
      q22av2 = q22av2/DBLE(niter)
      q22av2 = SQRT((q22av2-q22ave**2)/DBLE(niter))
      weight = wgy*sigmax*DBLE(niter)/DBLE(itry)
C  output of histograms
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/1X,A,1PE12.3,A,/1X,A)') 
     &       '========================================================='
     &       , ' *****   simulated cross section: ' , weight , 
     &       ' mb  *****' , 
     &       '========================================================='
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,3I10)')
     &                         'PHO_GPHERA:SUMMARY:NITER,ITRY,ITRW' , 
     &                        niter , itry , itrw
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'EFFECTIVE WEIGHT (FLUX,TOTAL)' , wgy , 
     &                        weight
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'AVERAGE Y,DY                 ' , ay , 
     &                        day
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'SAMPLED Y RANGE PHOTON       ' , 
     &                        yy2min , yy2max
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'AVERAGE Q2,DQ2               ' , 
     &                        q22ave , q22av2
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'SAMPLED Q2 RANGE PHOTON      ' , 
     &                        q22min , q22max
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P4E12.4)')
     &                         'SAMPLED THETA RANGE ELECTRON ' , 
     &                        an2min , an2max , PI - an2max , 
     &                        PI - an2min
C
      CALL PHO_EVENT(-2,p1,p2,weight,irej)
      IF ( niter.GT.1 ) THEN
         CALL PHO_PHIST(-2,weight)
         CALL PHO_LHIST(-2,weight)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &         'PHO_GPHERA:NO OUTPUT OF HISTOGRAMS' , niter
      END IF
 
      END SUBROUTINE
