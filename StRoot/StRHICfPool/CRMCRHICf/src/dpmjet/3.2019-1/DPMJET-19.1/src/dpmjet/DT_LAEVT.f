
      SUBROUTINE DT_LAEVT(Nevts,Epn,Npmass,Npchar,Ntmass,Ntchar,Idp,
     &                    Iglau)
 
C***********************************************************************
C Interface to run DPMJET for lepton-nucleus interactions.             *
C Kinematics is sampled using the equivalent photon approximation      *
C Based on GPHERA-routine by R. Engel.                                 *
C This version dated 23.03.96 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ady , ALPHEM , amlpt , amlpt2 , anorf , aymax , 
     &                 aymin , ayrge , bgta , cod , cof , DT_RNDM , 
     &                 dum , e1y , ecmgn , ecmln , ecmtmp , egnmax , 
     &                 egnmin , egnxx
      DOUBLE PRECISION Epn , etotgn , etotln , ff1 , ff2 , ONE , pgtot , 
     &                 PI , pltot , ppn , ppt , ptotgn , ptotln , px , 
     &                 py , pz , q2e , q2log , q2low , q2tmp
      DOUBLE PRECISION rat , sid , sif , sigmax , sigxx , stot , stotx , 
     &                 theta , THREE , TINY10 , TINY4 , TWO , TWOPI , 
     &                 weight , wgh , wghmax , wgy , xbj , xblow , xdumb
      DOUBLE PRECISION y , yeff , yq2 , yy , yytmp , ZERO
      INTEGER i , i1 , i2 , Idp , idppdg , IDT_IPDGHA , ievt , Iglau , 
     &        ihfle0 , ihfle1 , ihfle2 , ihflq0 , ihflq1 , ihflq2 , 
     &        ihflu0 , ihflu1 , ihflu2 , ihflx0 , ihflx1 , ihflx2
      INTEGER ihfly0 , ihfly1 , ihfly2 , irej , itrw , itry , kkmat , 
     &        maxtab , nc0 , nc1 , Nevts , nmsg , Npchar , Npmass , 
     &        Ntchar , Ntmass
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY4=1.0D-4,ZERO=0.0D0,ONE=1.0D0,
     &           TWO=2.0D0,THREE=3.0D0)
      PARAMETER (TWOPI=6.283185307179586454D+00,PI=TWOPI/TWO,
     &           ALPHEM=ONE/137.0D0)
 
C     CHARACTER*72 HEADER
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C event history
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C kinematical cuts for lepton-nucleus interactions
      INCLUDE 'inc/dtlcut'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C kinematics at lepton-gamma vertex
      INCLUDE 'inc/dtlgvx'
C flags for activated histograms
      INCLUDE 'inc/dthis3'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C event flag
      INCLUDE 'inc/dtevno'
 
      DIMENSION xdumb(40) , bgta(4)
 
C LEPTO
 
 
      IF ( MCGene.EQ.3 ) STOP ' This version does not contain LEPTO !'
 
      kkmat = 1
      nmsg = MAX(Nevts/10,1)
 
C mass of incident lepton
      amlpt = AAM(Idp)
      amlpt2 = amlpt**2
      idppdg = IDT_IPDGHA(Idp)
 
C consistency of kinematical limits
      Q2Min = MAX(Q2Min,TINY10)
      Q2Max = MAX(Q2Max,TINY10)
      YMIn = MIN(MAX(YMIn,TINY10),0.999D0)
      YMAx = MIN(MAX(YMAx,TINY10),0.999D0)
 
C total energy of the lepton-nucleon system
      ptotln = SQRT((PLEpt0(1)+PNUcl(1))**2+(PLEpt0(2)+PNUcl(2))
     &         **2+(PLEpt0(3)+PNUcl(3))**2)
      etotln = PLEpt0(4) + PNUcl(4)
      ecmln = SQRT((etotln-ptotln)*(etotln+ptotln))
      ECMax = MIN(ECMax,ecmln)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99010) ECMin , ECMax , YMIn , YMAx , 
     &                        Q2Min , Q2Max , EGMin , THMin , THMax , 
     &                        ELMin
99010 FORMAT (1X,'LAEVT:',16X,'kinematical cuts',/,22X,
     &        '------------------',/,9X,'W (min)   =',F7.1,
     &        ' GeV    (max) =',F7.1,' GeV',/,9X,'y (min)   =',F7.3,8X,
     &        '(max) =',F7.3,/,9X,'Q^2 (min) =',F7.1,' GeV^2  (max) =',
     &        F7.1,' GeV^2',/,' (Lab)   E_g (min) =',F7.1,' GeV',/,
     &        ' (Lab) theta (min) =',F7.4,8X,'(max) =',F7.4,
     &        '   for E_lpt >',F7.1,' GeV',/)
 
C Lorentz-parameter for transf. into Lab
      bgta(1) = PNUcl(1)/AAM(1)
      bgta(2) = PNUcl(2)/AAM(1)
      bgta(3) = PNUcl(3)/AAM(1)
      bgta(4) = PNUcl(4)/AAM(1)
C LT of incident lepton into Lab and dump it in DTEVT1
      CALL DT_DALTRA(bgta(4),-bgta(1),-bgta(2),-bgta(3),PLEpt0(1),
     &               PLEpt0(2),PLEpt0(3),PLEpt0(4),pltot,PPL0(1),PPL0(2)
     &               ,PPL0(3),PPL0(4))
      CALL DT_DALTRA(bgta(4),-bgta(1),-bgta(2),-bgta(3),PNUcl(1),
     &               PNUcl(2),PNUcl(3),PNUcl(4),pltot,PPA(1),PPA(2),
     &               PPA(3),PPA(4))
C maximum energy of photon nucleon system
      ptotgn = SQRT((YMAx*PPL0(1)+PPA(1))**2+(YMAx*PPL0(2)+PPA(2))
     &         **2+(YMAx*PPL0(3)+PPA(3))**2)
      etotgn = YMAx*PPL0(4) + PPA(4)
      egnmax = SQRT((etotgn-ptotgn)*(etotgn+ptotgn))
      egnmax = MIN(egnmax,ECMax)
C minimum energy of photon nucleon system
      ptotgn = SQRT((YMIn*PPL0(1)+PPA(1))**2+(YMIn*PPL0(2)+PPA(2))
     &         **2+(YMIn*PPL0(3)+PPA(3))**2)
      etotgn = YMIn*PPL0(4) + PPA(4)
      egnmin = SQRT((etotgn-ptotgn)*(etotgn+ptotgn))
      egnmin = MAX(egnmin,ECMin)
 
C limits for Glauber-initialization
      Q2Li = Q2Min
      Q2Hi = MAX(Q2Li,MIN(Q2Hi,Q2Max))
      ECMli = MAX(egnmin,THREE)
      ECMhi = egnmax
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99020) egnmin , egnmax , ECMli , 
     &                        ECMhi , Q2Li , Q2Hi
99020 FORMAT (1X,'resulting limits:',/,9X,'W (min)   =',F7.1,
     &        ' GeV    (max) =',F7.1,' GeV',/,/,' limits for ',
     &        'Glauber-initialization:',/,9X,'W (min)   =',F7.1,
     &        ' GeV    (max) =',F7.1,' GeV',/,9X,'Q^2 (min) =',F7.1,
     &        ' GeV^2  (max) =',F7.1,' GeV^2',/)
C initialization of Glauber-formalism
      IF ( NCOmpo.LE.0 ) THEN
         CALL DT_SHMAKI(Npmass,Npchar,Ntmass,Ntchar,Idp,Epn,Iglau)
      ELSE
         DO i = 1 , NCOmpo
            CALL DT_SHMAKI(Npmass,Npchar,IEMuma(i),IEMuch(i),Idp,Epn,0)
         END DO
      END IF
      CALL DT_SIGEMU
 
C initialization of run-statistics and histograms
      CALL DT_STATIS(1)
 
      CALL PHO_PHIST(1000,dum)
 
C maximum photon-nucleus cross section
      i1 = 1
      i2 = 1
      rat = ONE
      IF ( egnmax.GE.ECMnn(NEBini) ) THEN
         i1 = NEBini
         i2 = NEBini
         rat = ONE
      ELSE IF ( egnmax.GT.ECMnn(1) ) THEN
         DO i = 2 , NEBini
            IF ( egnmax.LT.ECMnn(i) ) THEN
               i1 = i - 1
               i2 = i
               rat = (egnmax-ECMnn(i1))/(ECMnn(i2)-ECMnn(i1))
               GOTO 100
            END IF
         END DO
      END IF
 100  sigmax = XSTot(i1,1,1) + rat*(XSTot(i2,1,1)-XSTot(i1,1,1))
      egnxx = egnmax
      i1 = 1
      i2 = 1
      rat = ONE
      IF ( egnmin.GE.ECMnn(NEBini) ) THEN
         i1 = NEBini
         i2 = NEBini
         rat = ONE
      ELSE IF ( egnmin.GT.ECMnn(1) ) THEN
         DO i = 2 , NEBini
            IF ( egnmin.LT.ECMnn(i) ) THEN
               i1 = i - 1
               i2 = i
               rat = (egnmin-ECMnn(i1))/(ECMnn(i2)-ECMnn(i1))
               GOTO 200
            END IF
         END DO
      END IF
 200  sigxx = XSTot(i1,1,1) + rat*(XSTot(i2,1,1)-XSTot(i1,1,1))
      IF ( sigxx.GT.sigmax ) egnxx = egnmin
      sigmax = MAX(sigmax,sigxx)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(9X,A,F8.3,A)')
     &                        'Sigma_tot (max) =' , sigmax , ' mb'
 
C plot photon flux table
      aymin = LOG(YMIn)
      aymax = LOG(YMAx)
      ayrge = aymax - aymin
      maxtab = 50
      ady = LOG(YMAx/YMIn)/DBLE(maxtab-1)
C     WRITE(LOUT,'(/,1X,A)') 'LAEVT:   photon flux '
      DO i = 1 , maxtab
         y = EXP(aymin+ady*DBLE(i-1))
         q2low = MAX(Q2Min,amlpt2*y**2/(ONE-y))
         ff1 = ALPHEM/TWOPI*((ONE+(ONE-y)**2)/y*LOG(Q2Max/q2low)
     &         -TWO*amlpt2*y*(ONE/q2low-ONE/Q2Max))
         ff2 = ALPHEM/TWOPI*((ONE+(ONE-y)**2)/y*LOG(Q2Max/q2low)
     &         -TWO*(ONE-y)/y*(ONE-q2low/Q2Max))
C        WRITE(LOUT,'(5X,3E15.4)') Y,FF1,FF2
      END DO
 
C maximum residual weight for flux sampling (dy/y)
      yy = YMIn
      q2low = MAX(Q2Min,amlpt2*yy**2/(ONE-yy))
      wghmax = (ONE+(ONE-yy)**2)*LOG(Q2Max/q2low)
     &         - TWO*amlpt2*yy*(ONE/q2low-ONE/Q2Max)*yy
 
      CALL DT_NEWHGR(YMIn,YMAx,ZERO,xdumb,49,ihfly0)
      CALL DT_NEWHGR(YMIn,YMAx,ZERO,xdumb,49,ihfly1)
      CALL DT_NEWHGR(YMIn,YMAx,ZERO,xdumb,49,ihfly2)
      CALL DT_NEWHGR(q2low,Q2Max,ZERO,xdumb,20,ihflq0)
      CALL DT_NEWHGR(q2low,Q2Max,ZERO,xdumb,20,ihflq1)
      CALL DT_NEWHGR(q2low,Q2Max,ZERO,xdumb,20,ihflq2)
      CALL DT_NEWHGR(egnmin,egnmax,ZERO,xdumb,20,ihfle0)
      CALL DT_NEWHGR(egnmin,egnmax,ZERO,xdumb,20,ihfle1)
      CALL DT_NEWHGR(egnmin,egnmax,ZERO,xdumb,20,ihfle2)
      CALL DT_NEWHGR(ZERO,EGMax,ZERO,xdumb,20,ihflu0)
      CALL DT_NEWHGR(ZERO,EGMax,ZERO,xdumb,20,ihflu1)
      CALL DT_NEWHGR(ZERO,EGMax,ZERO,xdumb,20,ihflu2)
      xblow = 0.001D0
      CALL DT_NEWHGR(xblow,ONE,ZERO,xdumb,-40,ihflx0)
      CALL DT_NEWHGR(xblow,ONE,ZERO,xdumb,-40,ihflx1)
      CALL DT_NEWHGR(xblow,ONE,ZERO,xdumb,-40,ihflx2)
 
      itry = 0
      itrw = 0
      nc0 = 0
      nc1 = 0
 
C generate events
      DO ievt = 1 , Nevts
         IF ( MOD(ievt,nmsg).EQ.0 ) THEN
C           OPEN(LDAT,FILE='/scrtch3/hr/sroesler/statusd5.out',
C    &                                         STATUS='UNKNOWN')
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,I8,A)') ievt - 1 , 
     &           ' events sampled'
C           CLOSE(LDAT)
         END IF
         NEVent = ievt
 
 250     itry = itry + 1
 
C  sample y
 300     itrw = itrw + 1
         yy = EXP(ayrge*DT_RNDM(rat)+aymin)
         q2low = MAX(Q2Min,amlpt2*yy**2/(ONE-yy))
         q2log = LOG(Q2Max/q2low)
         wgh = (ONE+(ONE-yy)**2)
     &         *q2log - TWO*amlpt2*yy*(ONE/q2low-ONE/Q2Max)*yy
 
         IF ( LPRi.GT.4 .AND. wghmax.LT.wgh ) WRITE (LOUt,99030) yy , 
     &        wghmax , wgh
99030    FORMAT (1X,'LAEVT:   weight error!',3E12.5)
         IF ( DT_RNDM(yy)*wghmax.GT.wgh ) GOTO 300
 
C  sample Q2
         yeff = ONE + (ONE-yy)**2
 350     Q2 = q2low*EXP(q2log*DT_RNDM(yy))
         wgh = (yeff-TWO*(ONE-yy)*q2low/Q2)/yeff
         IF ( wgh.LT.DT_RNDM(Q2) ) GOTO 350
 
C        NC0 = NC0+1
C        CALL DT_FILHGR(YY,ONE,IHFLY0,NC0)
C        CALL DT_FILHGR(Q2,ONE,IHFLQ0,NC0)
 
C  kinematics at lepton-photon vertex
C   scattered electron
         yq2 = SQRT((ONE-yy)*Q2)
         q2e = Q2/(4.0D0*PLEpt0(4))
         e1y = (ONE-yy)*PLEpt0(4)
         CALL DT_DSFECF(sif,cof)
         PLEpt1(1) = yq2*cof
         PLEpt1(2) = yq2*sif
         PLEpt1(3) = e1y - q2e
         PLEpt1(4) = e1y + q2e
C        THETA = ACOS( (E1Y-Q2E)/(E1Y+Q2E) )
C   radiated photon
         PGAmm(1) = -PLEpt1(1)
         PGAmm(2) = -PLEpt1(2)
         PGAmm(3) = PLEpt0(3) - PLEpt1(3)
         PGAmm(4) = PLEpt0(4) - PLEpt1(4)
C   E_cm cut
         ptotgn = SQRT((PGAmm(1)+PNUcl(1))**2+(PGAmm(2)+PNUcl(2))
     &            **2+(PGAmm(3)+PNUcl(3))**2)
         etotgn = PGAmm(4) + PNUcl(4)
         ecmgn = (etotgn-ptotgn)*(etotgn+ptotgn)
         IF ( ecmgn.LT.0.1D0 ) GOTO 300
         ecmgn = SQRT(ecmgn)
         IF ( (ecmgn.LT.ECMin) .OR. (ecmgn.GT.ECMax) ) GOTO 300
 
C  Lorentz-transformation into nucleon-rest system
         CALL DT_DALTRA(bgta(4),-bgta(1),-bgta(2),-bgta(3),PGAmm(1),
     &                  PGAmm(2),PGAmm(3),PGAmm(4),pgtot,PPG(1),PPG(2),
     &                  PPG(3),PPG(4))
         CALL DT_DALTRA(bgta(4),-bgta(1),-bgta(2),-bgta(3),PLEpt1(1),
     &                  PLEpt1(2),PLEpt1(3),PLEpt1(4),pltot,PPL1(1),
     &                  PPL1(2),PPL1(3),PPL1(4))
C  temporary checks..
         q2tmp = ABS(PPG(4)**2-pgtot**2)
 
         IF ( LPRi.GT.4 .AND. ABS(Q2-q2tmp).GT.0.01D0 )
     &        WRITE (LOUt,99040) Q2 , q2tmp
99040    FORMAT (1X,'LAEVT:    inconsistent kinematics (Q2,Q2TMP) ',
     &           2F10.4)
         ecmtmp = SQRT((PPG(4)+AAM(1)-pgtot)*(PPG(4)+AAM(1)+pgtot))
 
         IF ( LPRi.GT.4 .AND. ABS(ecmgn-ecmtmp).GT.TINY10 )
     &        WRITE (LOUt,99050) ecmgn , ecmtmp
99050    FORMAT (1X,'LAEVT:    inconsistent kinematics (ECMGN,ECMTMP) ',
     &           2F10.2)
         yytmp = PPG(4)/PPL0(4)
 
         IF ( LPRi.GT.4 .AND. ABS(yy-yytmp).GT.0.01D0 )
     &        WRITE (LOUt,99060) yy , yytmp
99060    FORMAT (1X,'LAEVT:    inconsistent kinematics (YY,YYTMP) ',
     &           2F10.4)
 
C  lepton tagger (Lab)
         theta = ACOS(PPL1(3)/pltot)
         IF ( PPL1(4).GT.ELMin ) THEN
            IF ( (theta.LT.THMin) .OR. (theta.GT.THMax) ) GOTO 300
         END IF
C  photon energy-cut (Lab)
         IF ( PPG(4).LT.EGMin ) GOTO 300
         IF ( PPG(4).GT.EGMax ) GOTO 300
C   x_Bj cut
         xbj = ABS(Q2/(1.876D0*PPG(4)))
         IF ( xbj.LT.XBJmin ) GOTO 300
 
         nc0 = nc0 + 1
         CALL DT_FILHGR(Q2,ONE,ihflq0,nc0)
         CALL DT_FILHGR(yy,ONE,ihfly0,nc0)
         CALL DT_FILHGR(xbj,ONE,ihflx0,nc0)
         CALL DT_FILHGR(PPG(4),ONE,ihflu0,nc0)
         CALL DT_FILHGR(ecmgn,ONE,ihfle0,nc0)
 
C  rotation angles against z-axis
         cod = PPG(3)/pgtot
C        SID = SQRT((ONE-COD)*(ONE+COD))
         ppt = SQRT(PPG(1)**2+PPG(2)**2)
         sid = ppt/pgtot
         cof = ONE
         sif = ZERO
         IF ( pgtot*sid.GT.TINY10 ) THEN
            cof = PPG(1)/(sid*pgtot)
            sif = PPG(2)/(sid*pgtot)
            anorf = SQRT(cof*cof+sif*sif)
            cof = cof/anorf
            sif = sif/anorf
         END IF
 
         IF ( IXStbl.EQ.0 ) THEN
C  change to photon projectile
            IJProj = 7
C  set virtuality
            VIRt = Q2
C  re-initialize LTs with new kinematics
C  !!PGAMM ist set in cms (ECMGN) along z
            Epn = ZERO
            ppn = ZERO
            CALL DT_LTINI(IJProj,IJTarg,Epn,ppn,ecmgn,0)
C  force Lab-system
            IFRame = 1
C  get emulsion component if requested
C  convolute with cross section
            IF ( IEMul.GT.0 ) CALL DT_GETEMU(Ntmass,Ntchar,kkmat,0)
            CALL DT_SIGGAT(q2low,egnxx,stotx,kkmat)
            CALL DT_SIGGAT(Q2,ecmgn,stot,kkmat)
 
            IF ( LPRi.GT.4 .AND. stotx.LT.stot )
     &            WRITE (LOUt,'(1X,A,/,6E12.3)')
     &            'LAEVT: warning STOTX<STOT ! ' , q2low , egnmax , 
     &           stotx , Q2 , ecmgn , stot
            IF ( DT_RNDM(Q2)*stotx.GT.stot ) GOTO 250
            nc1 = nc1 + 1
            CALL DT_FILHGR(Q2,ONE,ihflq1,nc1)
            CALL DT_FILHGR(yy,ONE,ihfly1,nc1)
            CALL DT_FILHGR(xbj,ONE,ihflx1,nc1)
            CALL DT_FILHGR(PPG(4),ONE,ihflu1,nc1)
            CALL DT_FILHGR(ecmgn,ONE,ihfle1,nc1)
C  composite targets only
            kkmat = -kkmat
C  sample this event
            CALL DT_KKINC(Npmass,Npchar,Ntmass,Ntchar,IJProj,Epn,kkmat,
     &                    irej)
C  rotate momenta of final state particles back in photon-nucleon syst.
            DO i = NPOint(4) , NHKk
               IF ( (ABS(ISThkk(i)).EQ.1) .OR. (ISThkk(i).EQ.1000) .OR. 
     &              (ISThkk(i).EQ.1001) ) THEN
                  px = PHKk(1,i)
                  py = PHKk(2,i)
                  pz = PHKk(3,i)
                  CALL DT_MYTRAN(1,px,py,pz,cod,sid,cof,sif,PHKk(1,i),
     &               PHKk(2,i),PHKk(3,i))
               END IF
            END DO
         END IF
 
         CALL DT_FILHGR(Q2,ONE,ihflq2,nc1)
         CALL DT_FILHGR(yy,ONE,ihfly2,nc1)
         CALL DT_FILHGR(xbj,ONE,ihflx2,nc1)
         CALL DT_FILHGR(PPG(4),ONE,ihflu2,nc1)
         CALL DT_FILHGR(ecmgn,ONE,ihfle2,nc1)
 
C  dump this event to histograms
 
         CALL PHO_PHIST(2000,dum)
 
      END DO
 
      wgy = ALPHEM/TWOPI*wghmax*DBLE(itry)/DBLE(itrw)
      wgy = wgy*LOG(YMAx/YMIn)
      weight = wgy*sigmax*DBLE(Nevts)/DBLE(itry)
 
C     HEADER = ' LAEVT:  Q^2 distribution 0'
C     CALL DT_OUTHGR(IHFLQ0,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  Q^2 distribution 1'
C     CALL DT_OUTHGR(IHFLQ1,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  Q^2 distribution 2'
C     CALL DT_OUTHGR(IHFLQ2,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  y   distribution 0'
C     CALL DT_OUTHGR(IHFLY0,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  y   distribution 1'
C     CALL DT_OUTHGR(IHFLY1,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  y   distribution 2'
C     CALL DT_OUTHGR(IHFLY2,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  x   distribution 0'
C     CALL DT_OUTHGR(IHFLX0,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  x   distribution 1'
C     CALL DT_OUTHGR(IHFLX1,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  x   distribution 2'
C     CALL DT_OUTHGR(IHFLX2,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  E_g distribution 0'
C     CALL DT_OUTHGR(IHFLU0,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  E_g distribution 1'
C     CALL DT_OUTHGR(IHFLU1,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  E_g distribution 2'
C     CALL DT_OUTHGR(IHFLU2,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  E_c distribution 0'
C     CALL DT_OUTHGR(IHFLE0,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  E_c distribution 1'
C     CALL DT_OUTHGR(IHFLE1,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
C     HEADER = ' LAEVT:  E_c distribution 2'
C     CALL DT_OUTHGR(IHFLE2,0,0,0,0,0,HEADER,0,NEVTS,ONE,1,1,-1)
 
C print run-statistics and histograms to output-unit 6
 
      CALL PHO_PHIST(3000,dum)
 
 
      IF ( IXStbl.EQ.0 ) CALL DT_STATIS(2)
      END SUBROUTINE
