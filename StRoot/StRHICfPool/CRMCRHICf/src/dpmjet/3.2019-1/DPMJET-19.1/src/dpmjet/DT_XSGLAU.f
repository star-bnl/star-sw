
      SUBROUTINE DT_XSGLAU(Na,Nb,Jjproj,Xi,Q2i,Ecmi,Ie,Iq,Nidx)
 
C***********************************************************************
C Total, elastic, quasi-elastic, inelastic cross sections according to *
C Glauber's approach.                                                  *
C  NA / NB     mass numbers of proj./target nuclei                     *
C  JJPROJ      bamjet-index of projectile (=1 in case of proj.nucleus) *
C  XI,Q2I,ECMI kinematical variables x, Q^2, E_cm                      *
C  IE,IQ       indices of energy and virtuality (the latter for gamma  *
C              projectiles only)                                       *
C  NIDX        index of projectile/target nucleus                      *
C This version dated 17.3.98  is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION abszx , ai , alpgam , ALPHEM , amhi2 , amhi20 , 
     &                 amlo2 , AMP , AMP2 , amv , amv2 , ar , b , bdum , 
     &                 bnorm , bprod , coop1 , coop2 , coot1 , coot2
      DOUBLE PRECISION dcoh , digamm , DT_RNCLUS , DT_SIGVP , dum , 
     &                 dumzer , Ecmi , elab , facb , facdi , facdi1 , 
     &                 facdi2 , facm , facn , fca , gam , GEV2FM , 
     &                 GEV2MB , ONE , ONETHI
      DOUBLE PRECISION PI , plab , Q2i , r , rca , RNUCLE , rpnt , 
     &                 rprncl , rrelab , rtancl , s , sdel , sdel2 , 
     &                 sdelb , sdelm , sdeln , sdif1 , sdir , sdqe , 
     &                 sdqe2
      DOUBLE PRECISION sdqeb , sdqem , sdqen , sdum1 , sdum2 , sdum3 , 
     &                 sela , sela2 , selab , selam , selan , shi , 
     &                 sigel , sigmv , sigmvd , spro , spro2 , sprob , 
     &                 sprom , spron
      DOUBLE PRECISION sqe2 , sqe22 , sqe2b , sqe2m , sqe2n , sqep , 
     &                 sqep2 , sqepb , sqepm , sqepn , sqet , sqet2 , 
     &                 sqetb , sqetm , sqetn , stot , stot2 , stotb , 
     &                 stotm , stotn
      DOUBLE PRECISION THREE , TINY25 , TWO , TWOPI , weight , x , x11 , 
     &                 x12 , x21 , x22 , xamhi , xamlo , Xi , xnu , 
     &                 xy11 , xy12 , xy21 , xy22 , y11 , y12
      DOUBLE PRECISION y21 , y22 , ZERO , zero1
      INTEGER i , ia , ib , Ie , iina , iinb , ijproj , im , ina , inb , 
     &        int1 , int2 , ipnt , Iq , is , isiteb , istart , istatb , 
     &        j , Jjproj
      INTEGER jpoint , k , kjproj , kk1 , kk2 , MAXINT , MAXNCL , 
     &        MAXSQU , MAXVQU , Na , Nb , Nidx , nlines , NPOINT , ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      COMPLEX*16 czero , cone , ctwo
      CHARACTER*12 cfile
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,THREE=3.0D0,
     &           ONETHI=ONE/THREE,TINY25=1.0D-25)
C proton mass
C approx. nucleon radius
      PARAMETER (TWOPI=6.283185307179586454D+00,PI=TWOPI/TWO,
     &           GEV2MB=0.38938D0,GEV2FM=0.1972D0,ALPHEM=ONE/137.0D0,
     &           AMP=0.938D0,AMP2=AMP**2,RNUCLE=1.12D0)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
C parameters for hA-diffraction
      INCLUDE 'inc/dtdiha'
 
#ifdef FOR_CORSIKA
cdh  datadir for path to the data sets to be read in by dpmjet/phojet
      COMMON /DATADIR/ DATADIR
      CHARACTER*132    DATADIR
#endif

      COMPLEX*16 pp11(MAXNCL) , pp12(MAXNCL) , pp21(MAXNCL) , 
     &           pp22(MAXNCL) , ompp11 , ompp12 , ompp21 , ompp22 , 
     &           dipp11 , dipp12 , dipp21 , dipp22 , avdipp , pptmp1 , 
     &           pptmp2
      COMPLEX*16 c , ca , ci
      DIMENSION coop1(3,MAXNCL) , coot1(3,MAXNCL) , coop2(3,MAXNCL) , 
     &          coot2(3,MAXNCL) , bprod(KSITEB)
 
      PARAMETER (NPOINT=16)
      DIMENSION abszx(NPOINT) , weight(NPOINT)
 
      LOGICAL lfirst , lopen
      DATA lfirst , lopen/.TRUE. , .FALSE./
 
      ntarg = ABS(Nidx)
C for quasi-elastic neutrino scattering set projectile to proton
C it should not have an effect since the whole Glauber-formalism is
C not needed for these interactions..
      IF ( MCGene.EQ.4 ) THEN
         ijproj = 1
      ELSE
         ijproj = Jjproj
      END IF
 
      IF ( (ABS(IOGlb).EQ.1) .AND. (.NOT.lopen) ) THEN
         i = INDEX(CGLb,' ')
         IF ( i.EQ.0 ) THEN
            cfile = CGLb//'.glb'
#ifndef FOR_CORSIKA
            OPEN (LDAt,FILE=CGLb//'.glb',STATUS='UNKNOWN')
#else
c  modification for use with corsika using path to data file in DATADIR
            OPEN(LDAT,STATUS='UNKNOWN',
     &        FILE=DATADIR(1:INDEX(DATADIR,' ')-1)//CGLB//'.glb')
#endif
         ELSE IF ( i.GT.1 ) THEN
            cfile = CGLb(1:i-1)//'.glb'
#ifndef FOR_CORSIKA
            OPEN (LDAt,FILE=CGLb(1:i-1)//'.glb',STATUS='UNKNOWN')
#else
c  modification for use with corsika using path to data file in DATADIR
            OPEN(LDAT,STATUS='UNKNOWN',
     &        FILE=DATADIR(1:INDEX(DATADIR,' ')-1)//CGLB(1:I-1)//'.glb')
#endif
         ELSE
            STOP 'XSGLAU 1'
         END IF
         lopen = .TRUE.
      END IF
 
      czero = DCMPLX(ZERO,ZERO)
      cone = DCMPLX(ONE,ZERO)
      ctwo = DCMPLX(TWO,ZERO)
      NEBini = Ie
      NQBini = Iq
 
C re-define kinematics
      s = Ecmi**2
      Q2 = Q2i
      x = Xi
C  g(Q2=0)-A, h-A, A-A scattering
      IF ( (x.LE.ZERO) .AND. (Q2.LE.ZERO) .AND. (s.GT.ZERO) ) THEN
         Q2 = 0.0001D0
         x = Q2/(s+Q2-AMP2)
C  g(Q2>0)-A scattering
      ELSE IF ( (x.LE.ZERO) .AND. (Q2.GT.ZERO) .AND. (s.GT.ZERO) ) THEN
         x = Q2/(s+Q2-AMP2)
      ELSE IF ( (x.GT.ZERO) .AND. (Q2.LE.ZERO) .AND. (s.GT.ZERO) ) THEN
         Q2 = (s-AMP2)*x/(ONE-x)
      ELSE IF ( (x.GT.ZERO) .AND. (Q2.GT.ZERO) ) THEN
         s = Q2*(ONE-x)/x + AMP2
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'XSGLAU: inconsistent input ' , 
     &        s , Q2 , x
         STOP
      END IF
      ECMnn(Ie) = SQRT(s)
      Q2G(Iq) = Q2
      xnu = (s+Q2-AMP2)/(TWO*AMP)
 
C parameters determining statistics in evaluating Glauber-xsection
      NSTatb = JSTatb
      NSIteb = JBInsb
      IF ( NSIteb.GT.KSITEB ) NSIteb = KSITEB
 
C set up interaction geometry (common /DTGLAM/)
C  projectile/target radii
      rprncl = DT_RNCLUS(Na)
      rtancl = DT_RNCLUS(Nb)
      IF ( ijproj.EQ.7 ) THEN
         RASh(1) = ZERO
         RBSh(ntarg) = rtancl
         BMAx(ntarg) = 2.0D0*(RASh(1)+RBSh(ntarg))
      ELSE IF ( Nidx.LE.-1 ) THEN
         RASh(1) = rprncl
         RBSh(ntarg) = rtancl
         BMAx(ntarg) = 2.0D0*(RASh(1)+RBSh(ntarg))
      ELSE
         RASh(ntarg) = rprncl
         RBSh(1) = rtancl
         BMAx(ntarg) = 2.0D0*(RASh(ntarg)+RBSh(1))
      END IF
C  maximum impact-parameter
      BSTep(ntarg) = BMAx(ntarg)/DBLE(NSIteb-1)
 
C slope, rho ( Re(f(0))/Im(f(0)) )
      IF ( ((ijproj.LE.40) .OR. ((ijproj.GE.97) .AND. (ijproj.LE.103))
     &     .OR. (ijproj.EQ.109) .OR. (ijproj.EQ.115)) .AND. 
     &     (ijproj.NE.7) ) THEN
         IF ( MCGene.EQ.2 ) THEN
            zero1 = ZERO
            CALL DT_PHOXS(ijproj,1,ECMnn(Ie),zero1,sdum1,sdum2,sdum3,
     &                    BSLope,0)
         ELSE
            BSLope = 8.5D0*(1.0D0+0.065D0*LOG(s))
         END IF
         IF ( ECMnn(Ie).LE.3.0D0 ) THEN
            ROSh = -0.43D0
         ELSE IF ( (ECMnn(Ie).GT.3.0D0) .AND. (ECMnn(Ie).LE.50.D0) )
     &             THEN
            ROSh = -0.63D0 + 0.175D0*LOG(ECMnn(Ie))
         ELSE IF ( ECMnn(Ie).GT.50.0D0 ) THEN
            ROSh = 0.1D0
         END IF
      ELSE IF ( ijproj.EQ.7 ) THEN
         ROSh = 0.1D0
      ELSE
         BSLope = 6.0D0*(1.0D0+0.065D0*LOG(s))
         ROSh = 0.01D0
      END IF
 
C projectile-nucleon xsection (in fm)
      IF ( ijproj.EQ.7 ) THEN
         SIGsh = DT_SIGVP(x,Q2)/10.0D0
      ELSE
         elab = (s-AAM(ijproj)**2-AMP2)/(TWO*AMP)
         plab = SQRT((elab-AAM(ijproj))*(elab+AAM(ijproj)))
C        SIGSH = DT_SHNTOT(IJPROJ,1,ZERO,PLAB)/10.0D0
         dumzer = ZERO
         CALL DT_XSHN(ijproj,1,plab,dumzer,SIGsh,sigel)
         SIGsh = SIGsh/10.0D0
      END IF
 
C parameters for projectile diffraction (hA scattering only)
      IF ( (MCGene.EQ.2) .AND. (Na.EQ.1) .AND. (Nb.GT.1) .AND. 
     &     (ijproj.NE.7) .AND. (DIBeta.GE.ZERO) ) THEN
         zero1 = ZERO
         CALL DT_PHOXS(ijproj,1,ECMnn(Ie),zero1,stot,sdum2,sdif1,bdum,0)
C        DIBETA = SDIF1/STOT
         DIBeta = 0.2D0
         digamm = SQRT(DIAlph**2+DIBeta**2)
         IF ( DIBeta.LE.ZERO ) THEN
            alpgam = ONE
         ELSE
            alpgam = DIAlph/digamm
         END IF
         facdi1 = ONE - alpgam
         facdi2 = ONE + alpgam
         facdi = SQRT(facdi1*facdi2)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'DIBETA,DIALPH,DIGAMM: ' , 
     &        DIBeta , DIAlph , digamm
      ELSE
         DIBeta = -1.0D0
         DIAlph = ZERO
         digamm = ZERO
         facdi1 = ZERO
         facdi2 = 2.0D0
         facdi = ZERO
      END IF
 
C initializations
      DO i = 1 , NSIteb
         BSIte(0,Iq,ntarg,i) = ZERO
         BSIte(Ie,Iq,ntarg,i) = ZERO
         bprod(i) = ZERO
      END DO
      stot = ZERO
      stot2 = ZERO
      sela = ZERO
      sela2 = ZERO
      sqep = ZERO
      sqep2 = ZERO
      sqet = ZERO
      sqet2 = ZERO
      sqe2 = ZERO
      sqe22 = ZERO
      spro = ZERO
      spro2 = ZERO
      sdel = ZERO
      sdel2 = ZERO
      sdqe = ZERO
      sdqe2 = ZERO
      facn = ONE/DBLE(NSTatb)
 
      ipnt = 0
      rpnt = ZERO
 
C  initialize Gauss-integration for photon-proj.
      jpoint = 1
      IF ( ijproj.EQ.7 ) THEN
         IF ( INTrge(1).EQ.1 ) THEN
            amlo2 = (3.0D0*AAM(13))**2
         ELSE IF ( INTrge(1).EQ.2 ) THEN
            amlo2 = AAM(33)**2
         ELSE
            amlo2 = AAM(96)**2
         END IF
         IF ( INTrge(2).EQ.1 ) THEN
            amhi2 = s/TWO
         ELSE IF ( INTrge(2).EQ.2 ) THEN
            amhi2 = s/4.0D0
         ELSE
            amhi2 = s
         END IF
         amhi20 = (ECMnn(Ie)-AMP)**2
         IF ( amhi2.GE.amhi20 ) amhi2 = amhi20
         xamlo = LOG(amlo2+Q2)
         xamhi = LOG(amhi2+Q2)
C*PHOJET105a
C        CALL GSET(XAMLO,XAMHI,NPOINT,ABSZX,WEIGHT)
C*PHOJET112
 
         CALL PHO_GAUSET(xamlo,xamhi,NPOINT,abszx,weight)
 
C*
         jpoint = NPOINT
C ratio direct/total photon-nucleon xsection
         CALL DT_POILIK(Nb,ntarg,ECMnn(Ie),Q2,ipnt,rpnt,1)
      END IF
 
C read pre-initialized profile-function from file
      IF ( IOGlb.EQ.1 ) THEN
         READ (LDAt,'(5I10,E15.5)') kjproj , ia , ib , istatb , isiteb , 
     &         dum
         IF ( (ia.NE.Na) .OR. (ib.NE.Nb) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010) cfile , ia , ib , 
     &           istatb , isiteb , Na , Nb , NSTatb , NSIteb
99010       FORMAT (' XSGLAU: inconsistent input data in file ',A12,/,
     &              ' (IA,IB,ISTATB,ISITEB) ',4I10,/,
     &              ' (NA,NB,NSTATB,NSITEB) ',4I10)
            STOP
         END IF
 
         IF ( LPRi.GT.4 .AND. lfirst ) WRITE (LOUt,99020) cfile
99020    FORMAT (/,' XSGLAU: impact parameter distribution read from ',
     &           'file ',A12,/)
         READ (LDAt,'(6E12.5)') XSTot(Ie,Iq,ntarg) , XSEla(Ie,Iq,ntarg)
     &         , XSQep(Ie,Iq,ntarg) , XSQet(Ie,Iq,ntarg) , 
     &         XSQe2(Ie,Iq,ntarg) , XSPro(Ie,Iq,ntarg)
         READ (LDAt,'(6E12.5)') XETot(Ie,Iq,ntarg) , XEEla(Ie,Iq,ntarg)
     &         , XEQep(Ie,Iq,ntarg) , XEQet(Ie,Iq,ntarg) , 
     &         XEQe2(Ie,Iq,ntarg) , XEPro(Ie,Iq,ntarg)
         nlines = INT(DBLE(NSIteb)/7.0D0)
         IF ( nlines.GT.0 ) THEN
            DO i = 1 , nlines
               istart = 7*i - 6
               READ (LDAt,'(7E11.4)')
     &               (BSIte(Ie,Iq,ntarg,j),j=istart,istart+6)
            END DO
         END IF
         istart = 7*nlines + 1
         IF ( istart.LE.NSIteb ) READ (LDAt,'(7E11.4)')
     &        (BSIte(Ie,Iq,ntarg,j),j=istart,NSIteb)
         lfirst = .FALSE.
         GOTO 99999
C variable projectile/target/energy runs:
C read pre-initialized profile-functions from file
      ELSE IF ( IOGlb.EQ.100 ) THEN
         CALL DT_GLBSET(ijproj,iina,iinb,rrelab,0)
         GOTO 99999
      END IF
 
C cross sections averaged over NSTATB nucleon configurations
      DO is = 1 , NSTatb
C        IF ((NA.EQ.207).AND.(NB.EQ.207)) WRITE(LOUT,*) 'conf. ',IS
         stotn = ZERO
         selan = ZERO
         sqepn = ZERO
         sqetn = ZERO
         sqe2n = ZERO
         spron = ZERO
         sdeln = ZERO
         sdqen = ZERO
 
         IF ( Nidx.LE.-1 ) THEN
            CALL DT_CONUCL(coop1,Na,RASh(1),0)
            CALL DT_CONUCL(coot1,Nb,RBSh(ntarg),1)
            IF ( (.NOT.LPRod) .OR. (ijproj.EQ.7) ) THEN
               CALL DT_CONUCL(coop2,Na,RASh(1),0)
               CALL DT_CONUCL(coot2,Nb,RBSh(ntarg),1)
            END IF
         ELSE
            CALL DT_CONUCL(coop1,Na,RASh(ntarg),0)
            CALL DT_CONUCL(coot1,Nb,RBSh(1),1)
            IF ( (.NOT.LPRod) .OR. (ijproj.EQ.7) ) THEN
               CALL DT_CONUCL(coop2,Na,RASh(ntarg),0)
               CALL DT_CONUCL(coot2,Nb,RBSh(1),1)
            END IF
         END IF
 
C  integration over impact parameter B
         DO ib = 1 , NSIteb - 1
            stotb = ZERO
            selab = ZERO
            sqepb = ZERO
            sqetb = ZERO
            sqe2b = ZERO
            sprob = ZERO
            sdir = ZERO
            sdelb = ZERO
            sdqeb = ZERO
            b = DBLE(ib)*BSTep(ntarg)
            facb = 10.0D0*TWOPI*b*BSTep(ntarg)
 
C   integration over M_V^2 for photon-proj.
            DO im = 1 , jpoint
               pp11(1) = cone
               pp12(1) = cone
               pp21(1) = cone
               pp22(1) = cone
               IF ( ijproj.EQ.7 ) THEN
                  DO k = 2 , Nb
                     pp11(k) = cone
                     pp12(k) = cone
                     pp21(k) = cone
                     pp22(k) = cone
                  END DO
               END IF
               shi = ZERO
               facm = ONE
               dcoh = 1.0D10
 
               IF ( ijproj.EQ.7 ) THEN
                  amv2 = EXP(abszx(im)) - Q2
                  amv = SQRT(amv2)
                  IF ( amv2.LT.16.0D0 ) THEN
                     r = TWO
                  ELSE IF ( (amv2.GE.16.0D0) .AND. (amv2.LT.121.0D0) )
     &                      THEN
                     r = 10.0D0/3.0D0
                  ELSE
                     r = 11.0D0/3.0D0
                  END IF
C    define M_V dependent properties of nucleon scattering amplitude
C     V_M-nucleon xsection
                  sigmvd = rpnt*SIGsh/(amv2+Q2+RL2)*10.0D0
                  sigmv = (ONE-rpnt)*SIGsh/(amv2+Q2+RL2)
C     slope-parametrisation a la Kaidalov
                  BSLope = 2.0D0*(2.0D0+AAM(32)**2/(amv2+Q2)
     &                     +0.25D0*LOG(s/(amv2+Q2)))
C    coherence length
                  IF ( ISHad(3).EQ.1 ) dcoh = TWO*xnu/(amv2+Q2)*GEV2FM
C    integration weight factor
                  facm = ALPHEM/(3.0D0*PI*(ONE-x))*r*amv2/(amv2+Q2)
     &                   *(ONE+EPSpol*Q2/amv2)*weight(im)
               END IF
               GSH = 10.0D0/(TWO*BSLope*GEV2MB)
               gam = GSH
               IF ( ijproj.EQ.7 ) THEN
                  rca = gam*sigmv/TWOPI
               ELSE
                  rca = gam*SIGsh/TWOPI
               END IF
               fca = -ROSh*rca
               ca = DCMPLX(rca,fca)
               ci = cone
 
               DO ina = 1 , Na
                  kk1 = 1
                  int1 = 1
                  kk2 = 1
                  int2 = 1
                  DO inb = 1 , Nb
C    photon-projectile: check for supression by coherence length
                     IF ( ijproj.EQ.7 ) THEN
                        IF ( ABS(coot1(3,inb)-coot1(3,kk1)).GT.dcoh )
     &                       THEN
                           kk1 = inb
                           int1 = int1 + 1
                        END IF
                        IF ( ABS(coot2(3,inb)-coot2(3,kk2)).GT.dcoh )
     &                       THEN
                           kk2 = inb
                           int2 = int2 + 1
                        END IF
                     END IF
 
                     x11 = b + coot1(1,inb) - coop1(1,ina)
                     y11 = coot1(2,inb) - coop1(2,ina)
                     xy11 = gam*(x11*x11+y11*y11)
                     IF ( xy11.LE.15.0D0 ) THEN
                        c = cone - ca*EXP(-xy11)
                        ar = DBLE(pp11(int1))
                        ai = DIMAG(pp11(int1))
                        IF ( ABS(ar).LT.TINY25 ) ar = ZERO
                        IF ( ABS(ai).LT.TINY25 ) ai = ZERO
                        pp11(int1) = DCMPLX(ar,ai)
                        pp11(int1) = pp11(int1)*c
                        ar = DBLE(c)
                        ai = DIMAG(c)
                        shi = shi + LOG(ar*ar+ai*ai)
                     END IF
                     IF ( (.NOT.LPRod) .OR. (ijproj.EQ.7) ) THEN
                        x12 = b + coot2(1,inb) - coop1(1,ina)
                        y12 = coot2(2,inb) - coop1(2,ina)
                        xy12 = gam*(x12*x12+y12*y12)
                        IF ( xy12.LE.15.0D0 ) THEN
                           c = cone - ca*EXP(-xy12)
                           ar = DBLE(pp12(int2))
                           ai = DIMAG(pp12(int2))
                           IF ( ABS(ar).LT.TINY25 ) ar = ZERO
                           IF ( ABS(ai).LT.TINY25 ) ai = ZERO
                           pp12(int2) = DCMPLX(ar,ai)
                           pp12(int2) = pp12(int2)*c
                        END IF
                        x21 = b + coot1(1,inb) - coop2(1,ina)
                        y21 = coot1(2,inb) - coop2(2,ina)
                        xy21 = gam*(x21*x21+y21*y21)
                        IF ( xy21.LE.15.0D0 ) THEN
                           c = cone - ca*EXP(-xy21)
                           ar = DBLE(pp21(int1))
                           ai = DIMAG(pp21(int1))
                           IF ( ABS(ar).LT.TINY25 ) ar = ZERO
                           IF ( ABS(ai).LT.TINY25 ) ai = ZERO
                           pp21(int1) = DCMPLX(ar,ai)
                           pp21(int1) = pp21(int1)*c
                        END IF
                        x22 = b + coot2(1,inb) - coop2(1,ina)
                        y22 = coot2(2,inb) - coop2(2,ina)
                        xy22 = gam*(x22*x22+y22*y22)
                        IF ( xy22.LE.15.0D0 ) THEN
                           c = cone - ca*EXP(-xy22)
                           ar = DBLE(pp22(int2))
                           ai = DIMAG(pp22(int2))
                           IF ( ABS(ar).LT.TINY25 ) ar = ZERO
                           IF ( ABS(ai).LT.TINY25 ) ai = ZERO
                           pp22(int2) = DCMPLX(ar,ai)
                           pp22(int2) = pp22(int2)*c
                        END IF
                     END IF
                  END DO
               END DO
 
               ompp11 = czero
               ompp21 = czero
               dipp11 = czero
               dipp21 = czero
               DO k = 1 , int1
                  IF ( pp11(k).EQ.czero ) THEN
                     pptmp1 = czero
                     pptmp2 = czero
                  ELSE
                     pptmp1 = pp11(k)**(ONE-DIAlph-digamm)
                     pptmp2 = pp11(k)**(ONE-DIAlph+digamm)
                  END IF
                  avdipp = 0.5D0*(facdi1*(cone-pptmp1)
     &                     +facdi2*(cone-pptmp2))
                  ompp11 = ompp11 + avdipp
C                 OMPP11 = OMPP11+(CONE-PP11(K))
                  avdipp = 0.5D0*facdi*(pptmp1-pptmp2)
                  dipp11 = dipp11 + avdipp
                  IF ( pp21(k).EQ.czero ) THEN
                     pptmp1 = czero
                     pptmp2 = czero
                  ELSE
                     pptmp1 = pp21(k)**(ONE-DIAlph-digamm)
                     pptmp2 = pp21(k)**(ONE-DIAlph+digamm)
                  END IF
                  avdipp = 0.5D0*(facdi1*(cone-pptmp1)
     &                     +facdi2*(cone-pptmp2))
                  ompp21 = ompp21 + avdipp
C                 OMPP21 = OMPP21+(CONE-PP21(K))
                  avdipp = 0.5D0*facdi*(pptmp1-pptmp2)
                  dipp21 = dipp21 + avdipp
               END DO
               ompp12 = czero
               ompp22 = czero
               dipp12 = czero
               dipp22 = czero
               DO k = 1 , int2
                  IF ( pp12(k).EQ.czero ) THEN
                     pptmp1 = czero
                     pptmp2 = czero
                  ELSE
                     pptmp1 = pp12(k)**(ONE-DIAlph-digamm)
                     pptmp2 = pp12(k)**(ONE-DIAlph+digamm)
                  END IF
                  avdipp = 0.5D0*(facdi1*(cone-pptmp1)
     &                     +facdi2*(cone-pptmp2))
                  ompp12 = ompp12 + avdipp
C                 OMPP12 = OMPP12+(CONE-PP12(K))
                  avdipp = 0.5D0*facdi*(pptmp1-pptmp2)
                  dipp12 = dipp12 + avdipp
                  IF ( pp22(k).EQ.czero ) THEN
                     pptmp1 = czero
                     pptmp2 = czero
                  ELSE
                     pptmp1 = pp22(k)**(ONE-DIAlph-digamm)
                     pptmp2 = pp22(k)**(ONE-DIAlph+digamm)
                  END IF
                  avdipp = 0.5D0*(facdi1*(cone-pptmp1)
     &                     +facdi2*(cone-pptmp2))
                  ompp22 = ompp22 + avdipp
C                 OMPP22 = OMPP22+(CONE-PP22(K))
                  avdipp = 0.5D0*facdi*(pptmp1-pptmp2)
                  dipp22 = dipp22 + avdipp
               END DO
 
               sprom = ONE - EXP(shi)
               sprob = sprob + facm*sprom
               IF ( (.NOT.LPRod) .OR. (ijproj.EQ.7) ) THEN
                  stotm = DBLE(ompp11+ompp22)
                  selam = DBLE(ompp11*DCONJG(ompp22))
                  sqepm = DBLE(ompp11*DCONJG(ompp21)) - selam
                  sqetm = DBLE(ompp11*DCONJG(ompp12)) - selam
                  sqe2m = DBLE(ompp11*DCONJG(ompp11)) - selam - sqepm - 
     &                    sqetm
                  sdelm = DBLE(dipp11*DCONJG(dipp22))
                  sdqem = DBLE(dipp11*DCONJG(dipp21)) - sdelm
                  stotb = stotb + facm*stotm
                  selab = selab + facm*selam
                  sdelb = sdelb + facm*sdelm
                  IF ( Nb.GT.1 ) THEN
                     sqepb = sqepb + facm*sqepm
                     sdqeb = sdqeb + facm*sdqem
                  END IF
                  IF ( Na.GT.1 ) sqetb = sqetb + facm*sqetm
                  IF ( (Na.GT.1) .AND. (Nb.GT.1) ) sqe2b = sqe2b + 
     &                 facm*sqe2m
                  IF ( ijproj.EQ.7 ) sdir = sdir + facm*sigmvd
               END IF
 
            END DO
 
            stotn = stotn + facb*stotb
            selan = selan + facb*selab
            sqepn = sqepn + facb*sqepb
            sqetn = sqetn + facb*sqetb
            sqe2n = sqe2n + facb*sqe2b
            spron = spron + facb*sprob
            sdeln = sdeln + facb*sdelb
            sdqen = sdqen + facb*sdqeb
 
            IF ( ijproj.EQ.7 ) THEN
               bprod(ib+1) = bprod(ib+1) + facn*facb*(stotb-selab-sqepb)
            ELSE IF ( DIBeta.GT.ZERO ) THEN
               bprod(ib+1) = bprod(ib+1)
     &                       + facn*facb*(stotb-selab-sqepb-sqetb-sqe2b)
            ELSE
               bprod(ib+1) = bprod(ib+1) + facn*facb*sprob
            END IF
 
         END DO
 
         stot = stot + facn*stotn
         stot2 = stot2 + facn*stotn**2
         sela = sela + facn*selan
         sela2 = sela2 + facn*selan**2
         sqep = sqep + facn*sqepn
         sqep2 = sqep2 + facn*sqepn**2
         sqet = sqet + facn*sqetn
         sqet2 = sqet2 + facn*sqetn**2
         sqe2 = sqe2 + facn*sqe2n
         sqe22 = sqe22 + facn*sqe2n**2
         spro = spro + facn*spron
         spro2 = spro2 + facn*spron**2
         sdel = sdel + facn*sdeln
         sdel2 = sdel2 + facn*sdeln**2
         sdqe = sdqe + facn*sdqen
         sdqe2 = sdqe2 + facn*sdqen**2
 
      END DO
 
C final cross sections
C 1) total
      XSTot(Ie,Iq,ntarg) = stot
      IF ( ijproj.EQ.7 ) XSTot(Ie,Iq,ntarg) = XSTot(Ie,Iq,ntarg)
     &     + DBLE(Nb)*sdir
C 2) elastic
      XSEla(Ie,Iq,ntarg) = sela
C 3) quasi-el.: A+B-->A+X (excluding 2)
      XSQep(Ie,Iq,ntarg) = sqep
C 4) quasi-el.: A+B-->X+B (excluding 2)
      XSQet(Ie,Iq,ntarg) = sqet
C 5) quasi-el.: A+B-->X (excluding 2-4)
      XSQe2(Ie,Iq,ntarg) = sqe2
C 6) production (= STOT-SELA-SQEP-SQET-SQE2!)
      IF ( sdel.GT.ZERO ) THEN
         XSPro(Ie,Iq,ntarg) = stot - sela - sqep - sqet - sqe2
      ELSE
         XSPro(Ie,Iq,ntarg) = spro
      END IF
C 7) projectile diffraction (el. scatt. off target)
      XSDel(Ie,Iq,ntarg) = sdel
C 8) projectile diffraction (quasi-el. scatt. off target)
      XSDqe(Ie,Iq,ntarg) = sdqe
C  stat. errors
      XETot(Ie,Iq,ntarg) = SQRT(ABS(stot2-stot**2)/DBLE(NSTatb-1))
      XEEla(Ie,Iq,ntarg) = SQRT(ABS(sela2-sela**2)/DBLE(NSTatb-1))
      XEQep(Ie,Iq,ntarg) = SQRT(ABS(sqep2-sqep**2)/DBLE(NSTatb-1))
      XEQet(Ie,Iq,ntarg) = SQRT(ABS(sqet2-sqet**2)/DBLE(NSTatb-1))
      XEQe2(Ie,Iq,ntarg) = SQRT(ABS(sqe22-sqe2**2)/DBLE(NSTatb-1))
      XEPro(Ie,Iq,ntarg) = SQRT(ABS(spro2-spro**2)/DBLE(NSTatb-1))
      XEDel(Ie,Iq,ntarg) = SQRT(ABS(sdel2-sdel**2)/DBLE(NSTatb-1))
      XEDqe(Ie,Iq,ntarg) = SQRT(ABS(sdqe2-sdqe**2)/DBLE(NSTatb-1))
 
      IF ( ijproj.EQ.7 ) THEN
         bnorm = XSTot(Ie,Iq,ntarg) - XSEla(Ie,Iq,ntarg)
     &           - XSQep(Ie,Iq,ntarg)
      ELSE
         bnorm = XSPro(Ie,Iq,ntarg)
      END IF
      DO i = 2 , NSIteb
         BSIte(Ie,Iq,ntarg,i) = bprod(i)/bnorm + BSIte(Ie,Iq,ntarg,i-1)
         IF ( (Ie.EQ.1) .AND. (Iq.EQ.1) ) BSIte(0,1,ntarg,i) = bprod(i)
     &        /bnorm + BSIte(0,1,ntarg,i-1)
      END DO
 
C write profile function data into file
      IF ( (IOGlb.EQ.-1) .OR. (IOGlb.EQ.-100) ) THEN
         WRITE (LDAt,'(5I10,1P,E15.5)') ijproj , Na , Nb , NSTatb , 
     &          NSIteb , ECMnn(Ie)
         WRITE (LDAt,'(1P,6E12.5)') XSTot(Ie,Iq,ntarg) , 
     &          XSEla(Ie,Iq,ntarg) , XSQep(Ie,Iq,ntarg) , 
     &          XSQet(Ie,Iq,ntarg) , XSQe2(Ie,Iq,ntarg) , 
     &          XSPro(Ie,Iq,ntarg)
         WRITE (LDAt,'(1P,6E12.5)') XETot(Ie,Iq,ntarg) , 
     &          XEEla(Ie,Iq,ntarg) , XEQep(Ie,Iq,ntarg) , 
     &          XEQet(Ie,Iq,ntarg) , XEQe2(Ie,Iq,ntarg) , 
     &          XEPro(Ie,Iq,ntarg)
         nlines = INT(DBLE(NSIteb)/7.0D0)
         IF ( nlines.GT.0 ) THEN
            DO i = 1 , nlines
               istart = 7*i - 6
               WRITE (LDAt,'(1P,7E11.4)')
     &                (BSIte(Ie,Iq,ntarg,j),j=istart,istart+6)
            END DO
         END IF
         istart = 7*nlines + 1
         IF ( istart.LE.NSIteb ) WRITE (LDAt,'(1P,7E11.4)')
     &        (BSIte(Ie,Iq,ntarg,j),j=istart,NSIteb)
      END IF
 
 
C     IF (ABS(IOGLB).EQ.1) CLOSE(LDAT)
 
99999 END SUBROUTINE
