
      SUBROUTINE PHO_GGBLSR(Nevent,Ee1,Ee2,Pl_lam_1,Pl_lam_2,X_1,X_2,
     &                      Rho,A)
C***********************************************************************
C
C     interface to call PHOJET (variable energy run) for
C     gamma-gamma collisions via laser backscattering
C
C     input:     EE1         lab. system energy of electron/positron 1
C                EE2         lab. system energy of electron/positron 2
C                NEVENT      number of events to generate
C                Pl_lam_1/2  product of electron and photon pol.
C                X_1/2       standard X parameter
C                rho         ratio of distance to conversion point and
C                            transverse beam size
C                A           ellipticity of electon beam
C
C                (see Ginzburg & Kotkin hep-ph/9905462)
C
C            from /LEPCUT/:
C                YMIN1   lower limit of Y1
C                        (energy fraction taken by photon from electron)
C                YMAX1   upper limit of Y1
C                YMIN2   lower limit of Y2
C                        (energy fraction taken by photon from electron)
C                YMAX2   upper limit of Y2
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION A , a2 , ay1 , ay2 , ays1 , ays2 , cof , day1 , 
     &                 day2 , dely1 , dely2 , DT_RNDM , dum , e1y , 
     &                 Ee1 , Ee2 , egam , fac , f_inp_1 , f_inp_2
      DOUBLE PRECISION f_int_1 , f_int_2 , g_1 , g_2 , p1 , p2 , phi_1 , 
     &                 phi_2 , PHO_EXPBESSI0 , PI , pi2 , Pl_lam_1 , 
     &                 Pl_lam_2 , q2e , q2p1 , q2p2 , r1 , r2 , Rho , 
     &                 sif
      DOUBLE PRECISION sigcur , sigmax , v , weight , wght , wgrid , 
     &                 wgy , xgrid , X_1 , X_2 , x_inp_1 , x_inp_2 , 
     &                 x_out_1 , x_out_2 , y1 , y2 , yq2
      INTEGER i , i1 , i2 , irej , itrw , itry , max_tab , Nevent , 
     &        nint , niter , N_DIM
      SAVE 
 
      PARAMETER (PI=3.141592653589793238462643383279D+00)
 
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
 
      PARAMETER (N_DIM=100)
      DIMENSION x_inp_1(N_DIM) , f_inp_1(N_DIM) , f_int_1(N_DIM) , 
     &          x_inp_2(N_DIM) , f_inp_2(N_DIM) , f_int_2(N_DIM) , 
     &          xgrid(96) , wgrid(96)
 
      DIMENSION p1(4) , p2(4)
 
      pi2 = 2.D0*PI
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,I10)')
     &                         'PHO_GGBLSR: events to process' , Nevent
 
      YMAx1 = MIN(X_1/(1.D0+X_1),YMAx1)
      YMAx2 = MIN(X_2/(1.D0+X_2),YMAx2)
      IF ( (YMIn1.GT.YMAx1) .OR. (YMIn2.GT.YMAx2) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,2E12.4)')
     &         'PHO_GGBLSR:ERROR: ' , 'invalid Ymin1,Ymin2' , YMIn1 , 
     &        YMIn2
         RETURN
      END IF
      IDPsrc(1) = 0
      IDBsrc(1) = 0
      IDPsrc(2) = 0
      IDBsrc(2) = 0
 
C  initialize sampling
 
      max_tab = 50
      dely1 = (YMAx1-YMIn1)/DBLE(max_tab-1)
      dely2 = (YMAx2-YMIn2)/DBLE(max_tab-1)
 
      IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 ) WRITE (LO,'(1X,A,I5)')
     &      'PHO_GGBLSR: table of photon flux ' , max_tab
 
      DO i = 1 , max_tab
 
         y1 = YMIn1 + dely1*DBLE(i-1)
         r1 = y1/(X_1*(1.D0-y1))
         x_inp_1(i) = y1
         f_inp_1(i) = 1.D0/(1.D0-y1) - y1 + (2.D0*r1-1.D0)
     &                **2 - Pl_lam_1*X_1*r1*(2.D0*r1-1.D0)*(2.D0-y1)
 
         y2 = YMIn2 + dely2*DBLE(i-1)
         r2 = y2/(X_2*(1.D0-y2))
         x_inp_2(i) = y2
         f_inp_2(i) = 1.D0/(1.D0-y2) - y2 + (2.D0*r2-1.D0)
     &                **2 - Pl_lam_2*X_2*r2*(2.D0*r2-1.D0)*(2.D0-y2)
 
         IF ( LPRi.GT.4 .AND. IDEb(30).GE.1 )
     &         WRITE (LO,'(5X,1p,2E13.4,5x,2E13.4)') y1 , f_inp_1(i) , 
     &        y2 , f_inp_2(i)
 
      END DO
 
      CALL PHO_SAMP1D(-1,x_inp_1,f_inp_1,f_int_1,max_tab,x_out_1)
      CALL PHO_SAMP1D(-1,x_inp_2,f_inp_2,f_int_2,max_tab,x_out_2)
 
C  initialize event generator
 
C  photon 1
      egam = YMAx1*Ee1
      p1(1) = 0.D0
      p1(2) = 0.D0
      p1(3) = egam
      p1(4) = egam
C  photon 2
      egam = YMAx2*Ee2
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
 
         CALL PHO_SAMP1D(1,x_inp_1,f_inp_1,f_int_1,max_tab,x_out_1)
         CALL PHO_SAMP1D(1,x_inp_2,f_inp_2,f_int_2,max_tab,x_out_2)
 
         g_1 = SQRT(MAX(0.D0,X_1/(x_out_1+1.D-6)-X_1-1.D0))
         g_2 = SQRT(MAX(0.D0,X_2/(x_out_2+1.D-6)-X_2-1.D0))
         IF ( ABS(1.D0-A).LT.1.D-3 ) THEN
            v = Rho**2/4.D0*g_1*g_2
            wght = EXP(-Rho**2/8.D0*(g_1-g_2)**2)*PHO_EXPBESSI0(v)
         ELSE
            nint = 16
            CALL PHO_GAUSET(0.D0,pi2,nint,xgrid,wgrid)
            a2 = A**2
            fac = Rho**2/(4.D0*(1.D0+a2))
            wght = 0.D0
            DO i1 = 1 , nint
               phi_1 = xgrid(i1)
               DO i2 = 1 , nint
                  phi_2 = xgrid(i2)
                  wght = wght + 
     &                   EXP(-fac*(a2*(g_1*COS(phi_1)+g_2*COS(phi_2))
     &                   **2+(g_1*SIN(phi_1)+g_2*SIN(phi_2))**2))
     &                   *wgrid(i1)*wgrid(i2)
               END DO
            END DO
            wght = wght/pi2**2
         END IF
 
         IF ( wght.GT.1.D0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,5E11.4)')
     &            'PHO_GGBLSR:WEIGHT ERROR:' , y1 , y2 , wght
         END IF
         IF ( DT_RNDM(dum).GT.wght ) GOTO 100
 
         y1 = x_out_1
         y2 = x_out_2
 
         q2p1 = 0.D0
         q2p2 = 0.D0
         GYY(1) = y1
         GQ2(1) = q2p1
         GYY(2) = y2
         GQ2(2) = q2p2
C  incoming electron 1
         PINi(1,1) = 0.D0
         PINi(2,1) = 0.D0
         PINi(3,1) = Ee1
         PINi(4,1) = Ee1
         PINi(5,1) = 0.D0
C  outgoing electron 1
         yq2 = SQRT((1.D0-y1)*q2p2)
         q2e = q2p1/(4.D0*Ee1)
         e1y = Ee1*(1.D0-y1)
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
         PINi(3,2) = -Ee2
         PINi(4,2) = Ee2
         PINi(5,2) = 0.D0
C  outgoing electron 2
         yq2 = SQRT((1.D0-y2)*q2p2)
         q2e = q2p2/(4.D0*Ee2)
         e1y = Ee2*(1.D0-y2)
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
 
C  statistics
         ay1 = ay1 + y1
         ays1 = ays1 + y1*y1
         ay2 = ay2 + y2
         ays2 = ays2 + y2*y2
C  histograms
         CALL PHO_PHIST(1,HSWght(0))
         CALL PHO_LHIST(1,HSWght(0))
      END DO
 
      wgy = DBLE(itry)/DBLE(itrw)
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
      IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,3I10)')
     &                         'PHO_GGBLSR:SUMMARY:NITER,ITRY,ITRW' , 
     &                        niter , itry , itrw
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P2E12.4)')
     &                         'EFFECTIVE WEIGHT (FLUX,TOTAL)' , wgy , 
     &                        weight
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2F10.5)')
     &                         'PHO_GGBLSR:AVERAGE Y1,DY1 ' , ay1 , day1
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2F10.5)')
     &                         'PHO_GGBLSR:AVERAGE Y2,DY2 ' , ay2 , day2
 
      CALL PHO_EVENT(-2,p1,p2,weight,irej)
      IF ( niter.GT.1 ) THEN
         CALL PHO_PHIST(-2,weight)
         CALL PHO_LHIST(-2,weight)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &         'PHO_GGBLSR:NO OUTPUT OF HISTOGRAMS' , niter
      END IF
 
      END SUBROUTINE
