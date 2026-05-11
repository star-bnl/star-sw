
      SUBROUTINE DT_XKSAMP(Nn,Ecm)
 
C***********************************************************************
C Sampling of parton x-values and chain system for one interaction.    *
C                                   processed by S. Roesler, 9.8.95    *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION AMAS , AMAU , amchk1 , amchk2 , AMIS , AMIU , 
     &                 amsvq1 , amsvq2 , amvsq1 , amvsq2 , bsqma , 
     &                 DT_BETREJ , DT_DBETAR , DT_RNDM , DT_SAMPLW , 
     &                 DT_SAMPXB , dxpsq , dxtsq , Ecm , frcdiq
      INTEGER i , icous , ipp , ipval , irej1 , isq , itt , itval , 
     &        ivv , ixstmp , ixvpr , ixvta , ixvtmp , j , jipp , jitt , 
     &        jj , MAXINT , MAXNCL , MAXSQU
      INTEGER MAXVQU , Nn , nscoun , nsea , NSEATY , nsmax
      DOUBLE PRECISION OHALF , ONE , plw , rnsmax , SQMA , ssma1q , 
     &                 ssma2q , TINY10 , unoprv , valfra , xdthr , 
     &                 xdtmp , xpsaqi , xpsqi , xpsqth , xpsqw , 
     &                 xpsqxx , xpvdco , xpvdi , xpvqi
      DOUBLE PRECISION xsmax , xspmax , xssthr , xsthr , xstmax , 
     &                 xtsaqi , xtsqi , xtsqth , xtsqw , xtsqxx , 
     &                 xtvdco , xtvdi , xtvqi , xvcut , xvhi , XVMAX , 
     &                 xvthr , xxsea , xxseam , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ZERO=0.0D0,OHALF=0.5D0,ONE=1.0D0)
      SAVE 
 
C lower cuts for (valence-sea/sea-valence) chain masses
C   antiquark-quark (u/d-sea quark)    (s-sea quark)
C   quark-diquark   (u/d-sea quark)    (s-sea quark)
C maximum lower valence-x threshold
C fraction of sea-diquarks sampled out of sea-partons
C*test
C    &           FRCDIQ = 0.9D0,
C*
C
C
C maximum number of trials to generate x's for the required number
C of sea quark pairs for a given hadron
C    &           NSEATY = 3
      PARAMETER (AMIU=0.5D0,AMIS=0.8D0,AMAU=2.6D0,AMAS=2.6D0,
     &           XVMAX=0.98D0,SQMA=0.7D0,NSEATY=12)
 
      LOGICAL zuovp , zuosp , zuovt , zuost , intlo
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C event history
 
 
      INCLUDE 'inc/dtevt1'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C interface between Glauber formalism and DPM
      INCLUDE 'inc/dtglif'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C threshold values for x-sampling (DTUNUC 1.x)
      INCLUDE 'inc/dtxcut'
C x-values of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmx'
C flavors of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmf'
C auxiliary common for x-value and flavor storage of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmi'
C auxiliary common for x-value and flavor storage of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpm0'
C auxiliary common for chain system storage (DTUNUC 1.x)
      INCLUDE 'inc/dtchsy'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
 
      DIMENSION zuovp(MAXVQU) , zuosp(MAXSQU) , zuovt(MAXVQU) , 
     &          zuost(MAXSQU) , intlo(MAXINT)
 
C (1) initializations
C-----------------------------------------------------------------------
 
C*test
      IF ( Ecm.LT.4.5D0 ) THEN
C        FRCDIQ = 0.6D0
         frcdiq = 0.4D0
      ELSE IF ( (Ecm.GE.4.5D0) .AND. (Ecm.LT.7.5) ) THEN
C        FRCDIQ = 0.6D0+(ECM-4.5D0)/3.0D0*0.3D0
         frcdiq = 0.4D0 + (Ecm-4.5D0)/3.0D0*0.3D0
      ELSE
C        FRCDIQ = 0.9D0
         frcdiq = 0.7D0
      END IF
C*
      DO i = 1 , MAXSQU
         zuosp(i) = .FALSE.
         zuost(i) = .FALSE.
         IF ( i.LE.MAXVQU ) THEN
            zuovp(i) = .FALSE.
            zuovt(i) = .FALSE.
         END IF
      END DO
 
C lower thresholds for x-selection
C  sea-quarks       (default: CSEA=0.2)
      IF ( Ecm.LT.10.0D0 ) THEN
C*!!test
         xsthr = ((12.0D0-Ecm)/5.0D0+1.0D0)*CSEa/Ecm
C        XSTHR = ((12.0D0-ECM)/5.0D0+1.0D0)*CSEA/ECM**2.0D0
         nsea = NSEATY
C        XSTHR = ONE/ECM**2
      ELSE
C*sr 30.3.98
C        XSTHR = CSEA/ECM
         xsthr = CSEa/Ecm**2
C        XSTHR = ONE/ECM**2
C*
         IF ( (IP.GE.150) .AND. (IT.GE.150) )
     &        xsthr = 2.5D0/(Ecm*SQRT(Ecm))
         nsea = NSEATY
      END IF
C                   (default: SSMIMA=0.14) used for sea-diquarks (?)
      xssthr = SSMima/Ecm
      bsqma = SQMA/Ecm
C  valence-quarks   (default: CVQ=1.0)
      xvthr = CVQ/Ecm
C  valence-diquarks (default: CDQ=2.0)
      xdthr = CDQ/Ecm
 
C maximum-x for sea-quarks
      xvcut = xvthr + xdthr
      IF ( xvcut.GT.XVMAX ) THEN
         xvcut = XVMAX
         xvthr = xvcut/3.0D0
         xdthr = xvcut - xvthr
      END IF
      xxseam = ONE - xvcut
C*sr 18.4. test: DPMJET
C     XXSEAM=1.0 - XVTHR*(1.D0+0.3D0*DT_RNDM(V1))
C    &            - XDTHR*(1.D0+0.3D0*DT_RNDM(V2))
C    &             -0.01*(1.D0+1.5D0*DT_RNDM(V3))
C*
C maximum number of sea-pairs allowed kinematically
C     NSMAX  = INT(OHALF*XXSEAM/XSTHR)
      rnsmax = OHALF*xxseam/xsthr
      IF ( rnsmax.GT.10000.0D0 ) THEN
         nsmax = 10000
      ELSE
         nsmax = INT(OHALF*xxseam/xsthr)
      END IF
C check kinematical limit for valence-x thresholds
C (should be obsolete now)
      IF ( xvcut.GT.XVMAX ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) xvcut , Ecm
99010    FORMAT (' XKSAMP:    kin. limit for valence-x',
     &           '  thresholds not allowed (',2E9.3,')')
C        XVTHR = XVMAX-XDTHR
C        IF (XVTHR.LT.ZERO) STOP
         STOP
      END IF
 
C set eta for valence-x sampling (BETREJ)
C   (UNON per default, UNOM used for projectile mesons only)
      IF ( (IJProj.NE.0) .AND. (IBProj.EQ.0) ) THEN
         unoprv = UNOm
      ELSE
         unoprv = UNOn
      END IF
 
C (2) select parton x-values of interacting projectile nucleons
C-----------------------------------------------------------------------
 
      IXPv = 0
      IXPs = 0
 
      DO ipp = 1 , IP
C   get interacting projectile nucleon as sampled by Glauber
         IF ( JSSh(ipp).NE.0 ) THEN
            ixstmp = IXPs
            ixvtmp = IXPv
 20         IXPs = ixstmp
            IXPv = ixvtmp
C     JIPP is the actual number of sea-pairs sampled for this nucleon
            jipp = MIN(JSSh(ipp)-1,nsmax)
 40         xxsea = ZERO
            IF ( jipp.GT.0 ) THEN
               xsmax = xxseam - 2.0D0*DBLE(jipp)*xsthr
C???
               IF ( xsthr.GE.xsmax ) THEN
                  jipp = jipp - 1
                  GOTO 40
               END IF
 
C>>>get x-values of sea-quark pairs
               nscoun = 0
               plw = 0.5D0
C     accumulator for sea x-values
 50            xxsea = ZERO
               nscoun = nscoun + 1
               IF ( DBLE(nscoun)/DBLE(nsea).GT.0.5D0 ) plw = 1.0D0
               IF ( nscoun.GT.nsea ) THEN
C     decrease the number of interactions after NSEA trials
                  jipp = jipp - 1
                  nscoun = 0
               END IF
               DO isq = 1 , jipp
C     sea-quarks
                  IF ( IPSq(IXPs+1).LE.2 ) THEN
C*sr 8.4.98 (1/sqrt(x))
C                    XPSQI = DT_SAMPEX(XSTHR,XSMAX)
C                    XPSQI = DT_SAMSQX(XSTHR,XSMAX)
                     xpsqi = DT_SAMPLW(xsthr,xsmax,plw)
C*
                  ELSE IF ( xsmax.GT.xsthr+bsqma ) THEN
                     xpsqi = DT_SAMPXB(xsthr+bsqma,xsmax,bsqma)
                  ELSE
C*sr 8.4.98 (1/sqrt(x))
C                       XPSQI = DT_SAMPEX(XSTHR,XSMAX)
C                       XPSQI = DT_SAMSQX(XSTHR,XSMAX)
                     xpsqi = DT_SAMPLW(xsthr,xsmax,plw)
C*
                  END IF
C     sea-antiquarks
                  IF ( IPSaq(IXPs+1).GE.-2 ) THEN
C*sr 8.4.98 (1/sqrt(x))
C                    XPSAQI = DT_SAMPEX(XSTHR,XSMAX)
C                    XPSAQI = DT_SAMSQX(XSTHR,XSMAX)
                     xpsaqi = DT_SAMPLW(xsthr,xsmax,plw)
C*
                  ELSE IF ( xsmax.GT.xsthr+bsqma ) THEN
                     xpsaqi = DT_SAMPXB(xsthr+bsqma,xsmax,bsqma)
                  ELSE
C*sr 8.4.98 (1/sqrt(x))
C                       XPSAQI = DT_SAMPEX(XSTHR,XSMAX)
C                       XPSAQI = DT_SAMSQX(XSTHR,XSMAX)
                     xpsaqi = DT_SAMPLW(xsthr,xsmax,plw)
C*
                  END IF
                  xxsea = xxsea + xpsqi + xpsaqi
C     check for maximum allowed sea x-value
                  IF ( xxsea.GE.xxseam ) THEN
                     IXPs = IXPs - isq + 1
                     GOTO 50
                  END IF
C     accept this sea-quark pair
                  IXPs = IXPs + 1
                  XPSq(IXPs) = xpsqi
                  XPSaq(IXPs) = xpsaqi
                  IFRosp(IXPs) = ipp
                  zuosp(IXPs) = .TRUE.
               END DO
            END IF
 
C>>>get x-values of valence partons
C     valence quark
            IF ( xvthr.GT.0.05D0 ) THEN
               xvhi = ONE - xxsea - xdthr
               xpvqi = DT_BETREJ(OHALF,unoprv,xvthr,xvhi)
            ELSE
 60            xpvqi = DT_DBETAR(OHALF,unoprv)
               IF ( (xpvqi.LT.xvthr) .OR. (ONE-xpvqi-xxsea.LT.xdthr) )
     &              GOTO 60
            END IF
C     valence diquark
            xpvdi = ONE - xpvqi - xxsea
C       reject according to x**1.5
            xdtmp = xpvdi**1.5D0
            IF ( DT_RNDM(xpvdi).GT.xdtmp ) GOTO 20
C     accept these valence partons
            IXPv = IXPv + 1
            XPVq(IXPv) = xpvqi
            XPVd(IXPv) = xpvdi
            IFRovp(IXPv) = ipp
            ITOvp(ipp) = IXPv
            zuovp(IXPv) = .TRUE.
 
         END IF
      END DO
 
C (3) select parton x-values of interacting target nucleons
C-----------------------------------------------------------------------
 
      IXTv = 0
      IXTs = 0
 
      DO itt = 1 , IT
C   get interacting target nucleon as sampled by Glauber
         IF ( JTSh(itt).NE.0 ) THEN
            ixstmp = IXTs
            ixvtmp = IXTv
 80         IXTs = ixstmp
            IXTv = ixvtmp
C     JITT is the actual number of sea-pairs sampled for this nucleon
            jitt = MIN(JTSh(itt)-1,nsmax)
 100        xxsea = ZERO
            IF ( jitt.GT.0 ) THEN
               xsmax = xxseam - 2.0D0*DBLE(jitt)*xsthr
C???
               IF ( xsthr.GE.xsmax ) THEN
                  jitt = jitt - 1
                  GOTO 100
               END IF
 
C>>>get x-values of sea-quark pairs
               nscoun = 0
               plw = 0.5D0
C     accumulator for sea x-values
 110           xxsea = ZERO
               nscoun = nscoun + 1
               IF ( DBLE(nscoun)/DBLE(nsea).GT.0.5D0 ) plw = 1.0D0
               IF ( nscoun.GT.nsea ) THEN
C     decrease the number of interactions after NSEA trials
                  jitt = jitt - 1
                  nscoun = 0
               END IF
               DO isq = 1 , jitt
C     sea-quarks
                  IF ( ITSq(IXTs+1).LE.2 ) THEN
C*sr 8.4.98 (1/sqrt(x))
C                    XTSQI = DT_SAMPEX(XSTHR,XSMAX)
C                    XTSQI = DT_SAMSQX(XSTHR,XSMAX)
                     xtsqi = DT_SAMPLW(xsthr,xsmax,plw)
C*
                  ELSE IF ( xsmax.GT.xsthr+bsqma ) THEN
                     xtsqi = DT_SAMPXB(xsthr+bsqma,xsmax,bsqma)
                  ELSE
C*sr 8.4.98 (1/sqrt(x))
C                       XTSQI = DT_SAMPEX(XSTHR,XSMAX)
C                       XTSQI = DT_SAMSQX(XSTHR,XSMAX)
                     xtsqi = DT_SAMPLW(xsthr,xsmax,plw)
C*
                  END IF
C     sea-antiquarks
                  IF ( ITSaq(IXTs+1).GE.-2 ) THEN
C*sr 8.4.98 (1/sqrt(x))
C                    XTSAQI = DT_SAMPEX(XSTHR,XSMAX)
C                    XTSAQI = DT_SAMSQX(XSTHR,XSMAX)
                     xtsaqi = DT_SAMPLW(xsthr,xsmax,plw)
C*
                  ELSE IF ( xsmax.GT.xsthr+bsqma ) THEN
                     xtsaqi = DT_SAMPXB(xsthr+bsqma,xsmax,bsqma)
                  ELSE
C*sr 8.4.98 (1/sqrt(x))
C                       XTSAQI = DT_SAMPEX(XSTHR,XSMAX)
C                       XTSAQI = DT_SAMSQX(XSTHR,XSMAX)
                     xtsaqi = DT_SAMPLW(xsthr,xsmax,plw)
C*
                  END IF
                  xxsea = xxsea + xtsqi + xtsaqi
C     check for maximum allowed sea x-value
                  IF ( xxsea.GE.xxseam ) THEN
                     IXTs = IXTs - isq + 1
                     GOTO 110
                  END IF
C     accept this sea-quark pair
                  IXTs = IXTs + 1
                  XTSq(IXTs) = xtsqi
                  XTSaq(IXTs) = xtsaqi
                  IFRost(IXTs) = itt
                  zuost(IXTs) = .TRUE.
               END DO
            END IF
 
C>>>get x-values of valence partons
C     valence quark
            IF ( xvthr.GT.0.05D0 ) THEN
               xvhi = ONE - xxsea - xdthr
               xtvqi = DT_BETREJ(OHALF,UNOn,xvthr,xvhi)
            ELSE
 120           xtvqi = DT_DBETAR(OHALF,UNOn)
               IF ( (xtvqi.LT.xvthr) .OR. (ONE-xtvqi-xxsea.LT.xdthr) )
     &              GOTO 120
            END IF
C     valence diquark
            xtvdi = ONE - xtvqi - xxsea
C       reject according to x**1.5
            xdtmp = xtvdi**1.5D0
            IF ( DT_RNDM(xpvdi).GT.xdtmp ) GOTO 80
C     accept these valence partons
            IXTv = IXTv + 1
            XTVq(IXTv) = xtvqi
            XTVd(IXTv) = xtvdi
            IFRovt(IXTv) = itt
            ITOvt(itt) = IXTv
            zuovt(IXTv) = .TRUE.
 
         END IF
      END DO
 
C (4) get valence-valence chains
C-----------------------------------------------------------------------
 
      NVV = 0
      DO i = 1 , Nn
         intlo(i) = .TRUE.
         ipval = ITOvp(INTer1(i))
         itval = ITOvt(INTer2(i))
         IF ( zuovp(ipval) .AND. zuovt(itval) ) THEN
            intlo(i) = .FALSE.
            zuovp(ipval) = .FALSE.
            zuovt(itval) = .FALSE.
            NVV = NVV + 1
            ISKpch(8,NVV) = 0
            INTvv1(NVV) = ipval
            INTvv2(NVV) = itval
         END IF
      END DO
 
C (5) get sea-valence chains
C-----------------------------------------------------------------------
 
      NSV = 0
      NDV = 0
      plw = 0.5D0
      DO i = 1 , Nn
         IF ( intlo(i) ) THEN
            ipval = ITOvp(INTer1(i))
            itval = ITOvt(INTer2(i))
            DO j = 1 , IXPs
               IF ( zuosp(j) .AND. (IFRosp(j).EQ.INTer1(i)) .AND. 
     &              zuovt(itval) ) THEN
                  zuosp(j) = .FALSE.
                  zuovt(itval) = .FALSE.
                  intlo(i) = .FALSE.
                  IF ( LSEadi .AND. (DT_RNDM(plw).GT.frcdiq) ) THEN
C   sample sea-diquark pair
                     CALL DT_SAMSDQ(Ecm,itval,j,2,irej1)
                     IF ( irej1.EQ.0 ) GOTO 200
                  END IF
                  NSV = NSV + 1
                  ISKpch(4,NSV) = 0
                  INTsv1(NSV) = j
                  INTsv2(NSV) = itval
 
C>>>correct chain kinematics according to minimum chain masses
C     the actual chain masses
                  amsvq1 = XPSq(j)*XTVd(itval)*Ecm**2
                  amsvq2 = XPSaq(j)*XTVq(itval)*Ecm**2
C     get lower mass cuts
                  IF ( IPSq(j).EQ.3 ) THEN
C       q being s-quark
                     amchk1 = AMAS
                     amchk2 = AMIS
                  ELSE
C       q being u/d-quark
                     amchk1 = AMAU
                     amchk2 = AMIU
                  END IF
C       q-qq chain
C         chain mass above minimum - resampling of sea-q x-value
                  IF ( amsvq1.GT.amchk1 ) THEN
                     xpsqth = amchk1/(XTVd(itval)*Ecm**2)
C*sr 8.4.98 (1/sqrt(x))
C                    XPSQXX      = DT_SAMPEX(XPSQTH,XPSQ(J))
C                    XPSQXX      = DT_SAMSQX(XPSQTH,XPSQ(J))
                     xpsqxx = DT_SAMPLW(xpsqth,XPSq(j),plw)
C*
                     XPVd(ipval) = XPVd(ipval) + XPSq(j) - xpsqxx
                     XPSq(j) = xpsqxx
C         chain mass below minimum - reset sea-q x-value and correct
C                                    diquark-x of the same nucleon
                  ELSE IF ( amsvq1.LT.amchk1 ) THEN
                     xpsqw = amchk1/(XTVd(itval)*Ecm**2)
                     dxpsq = xpsqw - XPSq(j)
                     IF ( XPVd(ipval).GE.xdthr+dxpsq ) THEN
                        XPVd(ipval) = XPVd(ipval) - dxpsq
                        XPSq(j) = xpsqw
                     END IF
                  END IF
C       aq-q chain
C         chain mass below minimum - reset sea-aq x-value and correct
C                                    diquark-x of the same nucleon
                  IF ( amsvq2.LT.amchk2 ) THEN
                     xpsqw = amchk2/(XTVq(itval)*Ecm**2)
                     dxpsq = xpsqw - XPSaq(j)
                     IF ( XPVd(ipval).GE.xdthr+dxpsq ) THEN
                        XPVd(ipval) = XPVd(ipval) - dxpsq
                        XPSaq(j) = xpsqw
                     END IF
                  END IF
C>>>end of chain mass correction
 
                  GOTO 200
               END IF
            END DO
         END IF
 200  END DO
 
C (6) get valence-sea chains
C-----------------------------------------------------------------------
 
      NVS = 0
      NVD = 0
      DO i = 1 , Nn
         IF ( intlo(i) ) THEN
            ipval = ITOvp(INTer1(i))
            itval = ITOvt(INTer2(i))
            DO j = 1 , IXTs
               IF ( zuovp(ipval) .AND. zuost(j) .AND. 
     &              (IFRost(j).EQ.INTer2(i)) ) THEN
                  zuost(j) = .FALSE.
                  zuovp(ipval) = .FALSE.
                  intlo(i) = .FALSE.
                  IF ( LSEadi .AND. (DT_RNDM(Ecm).GT.frcdiq) ) THEN
C   sample sea-diquark pair
                     CALL DT_SAMSDQ(Ecm,ipval,j,1,irej1)
                     IF ( irej1.EQ.0 ) GOTO 300
                  END IF
                  NVS = NVS + 1
                  ISKpch(6,NVS) = 0
                  INTvs1(NVS) = ipval
                  INTvs2(NVS) = j
 
C>>>correct chain kinematics according to minimum chain masses
C     the actual chain masses
                  amvsq1 = XPVq(ipval)*XTSaq(j)*Ecm**2
                  amvsq2 = XPVd(ipval)*XTSq(j)*Ecm**2
C     get lower mass cuts
                  IF ( ITSq(j).EQ.3 ) THEN
C       q being s-quark
                     amchk1 = AMIS
                     amchk2 = AMAS
                  ELSE
C       q being u/d-quark
                     amchk1 = AMIU
                     amchk2 = AMAU
                  END IF
C       q-aq chain
C         chain mass below minimum - reset sea-aq x-value and correct
C                                    diquark-x of the same nucleon
                  IF ( amvsq1.LT.amchk1 ) THEN
                     xtsqw = amchk1/(XPVq(ipval)*Ecm**2)
                     dxtsq = xtsqw - XTSaq(j)
                     IF ( XTVd(itval).GE.xdthr+dxtsq ) THEN
                        XTVd(itval) = XTVd(itval) - dxtsq
                        XTSaq(j) = xtsqw
                     END IF
                  END IF
C       qq-q chain
C         chain mass above minimum - resampling of sea-q x-value
                  IF ( amvsq2.GT.amchk2 ) THEN
                     xtsqth = amchk2/(XPVd(ipval)*Ecm**2)
C*sr 8.4.98 (1/sqrt(x))
C                    XTSQXX      = DT_SAMPEX(XTSQTH,XTSQ(J))
C                    XTSQXX      = DT_SAMSQX(XTSQTH,XTSQ(J))
                     xtsqxx = DT_SAMPLW(xtsqth,XTSq(j),plw)
C*
                     XTVd(itval) = XTVd(itval) + XTSq(j) - xtsqxx
                     XTSq(j) = xtsqxx
C         chain mass below minimum - reset sea-q x-value and correct
C                                    diquark-x of the same nucleon
                  ELSE IF ( amvsq2.LT.amchk2 ) THEN
                     xtsqw = amchk2/(XPVd(ipval)*Ecm**2)
                     dxtsq = xtsqw - XTSq(j)
                     IF ( XTVd(itval).GE.xdthr+dxtsq ) THEN
                        XTVd(itval) = XTVd(itval) - dxtsq
                        XTSq(j) = xtsqw
                     END IF
                  END IF
C>>>end of chain mass correction
 
                  GOTO 300
               END IF
            END DO
         END IF
 300  END DO
 
C (7) get sea-sea chains
C-----------------------------------------------------------------------
 
      NSS = 0
      NDS = 0
      NSD = 0
      DO i = 1 , Nn
         IF ( intlo(i) ) THEN
            ipval = ITOvp(INTer1(i))
            itval = ITOvt(INTer2(i))
C   loop over target partons not yet matched
            DO j = 1 , IXTs
               IF ( zuost(j) .AND. (IFRost(j).EQ.INTer2(i)) ) THEN
C   loop over projectile partons not yet matched
                  DO jj = 1 , IXPs
                     IF ( zuosp(jj) .AND. (IFRosp(jj).EQ.INTer1(i)) )
     &                    THEN
                        zuosp(jj) = .FALSE.
                        zuost(j) = .FALSE.
                        intlo(i) = .FALSE.
                        NSS = NSS + 1
                        ISKpch(1,NSS) = 0
                        INTss1(NSS) = jj
                        INTss2(NSS) = j
 
C---->chain recombination option
                        valfra = DBLE(NVV/(NVV+IXPs+IXTs))
                        IF ( IREcom.EQ.1 .AND. 
     &                       (DT_RNDM(bsqma).GT.valfra) ) THEN
C       sea-sea chains may recombine with valence-valence chains
C       only if they have the same projectile or target nucleon
                           DO ivv = 1 , NVV
                              IF ( ISKpch(8,ivv).NE.99 ) THEN
                               ixvpr = INTvv1(ivv)
                               ixvta = INTvv2(ivv)
                               IF ( (INTer1(i).EQ.IFRovp(ixvpr)) .OR. 
     &                            (INTer2(i).EQ.IFRovt(ixvta)) ) THEN
C         recombination possible, drop old v-v and s-s chains
                               ISKpch(1,NSS) = 99
                               ISKpch(8,ivv) = 99
 
C         (a) assign new s-v chains
C         ~~~~~~~~~~~~~~~~~~~~~~~~~
                               IF ( LSEadi .AND. 
     &                            (DT_RNDM(valfra).GT.frcdiq) ) THEN
C           sample sea-diquark pair
                               CALL DT_SAMSDQ(Ecm,ixvta,jj,2,irej1)
                               IF ( irej1.EQ.0 ) GOTO 302
                               END IF
                               NSV = NSV + 1
                               ISKpch(4,NSV) = 0
                               INTsv1(NSV) = jj
                               INTsv2(NSV) = ixvta
C>>>>>>>>>>>correct chain kinematics according to minimum chain masses
C           the actual chain masses
                               amsvq1 = XPSq(jj)*XTVd(ixvta)*Ecm**2
                               amsvq2 = XPSaq(jj)*XTVq(ixvta)*Ecm**2
C           get lower mass cuts
                               IF ( IPSq(jj).EQ.3 ) THEN
C             q being s-quark
                               amchk1 = AMAS
                               amchk2 = AMIS
                               ELSE
C             q being u/d-quark
                               amchk1 = AMAU
                               amchk2 = AMIU
                               END IF
C           q-qq chain
C             chain mass above minimum - resampling of sea-q x-value
                               IF ( amsvq1.GT.amchk1 ) THEN
                               xpsqth = amchk1/(XTVd(ixvta)*Ecm**2)
C*sr 8.4.98 (1/sqrt(x))
                               xpsqxx = DT_SAMPLW(xpsqth,XPSq(jj),plw)
C    &                                    DT_SAMSQX(XPSQTH,XPSQ(JJ))
C    &                                    DT_SAMPEX(XPSQTH,XPSQ(JJ))
C*
                               XPVd(ipval) = XPVd(ipval) + XPSq(jj)
     &                            - xpsqxx
                               XPSq(jj) = xpsqxx
C             chain mass below minimum - reset sea-q x-value and correct
C                                        diquark-x of the same nucleon
                               ELSE IF ( amsvq1.LT.amchk1 ) THEN
                               xpsqw = amchk1/(XTVd(ixvta)*Ecm**2)
                               dxpsq = xpsqw - XPSq(jj)
                               IF ( XPVd(ipval).GE.xdthr+dxpsq ) THEN
                               XPVd(ipval) = XPVd(ipval) - dxpsq
                               XPSq(jj) = xpsqw
                               END IF
                               END IF
C           aq-q chain
C             chain mass below minimum - reset sea-aq x-value and correct
C                                        diquark-x of the same nucleon
                               IF ( amsvq2.LT.amchk2 ) THEN
                               xpsqw = amchk2/(XTVq(ixvta)*Ecm**2)
                               dxpsq = xpsqw - XPSaq(jj)
                               IF ( XPVd(ipval).GE.xdthr+dxpsq ) THEN
                               XPVd(ipval) = XPVd(ipval) - dxpsq
                               XPSaq(jj) = xpsqw
                               END IF
                               END IF
C>>>>>>>>>>>end of chain mass correction
 
C         (b) assign new v-s chains
C         ~~~~~~~~~~~~~~~~~~~~~~~~~
 302                           IF ( LSEadi .AND. 
     &                            (DT_RNDM(amsvq2).GT.frcdiq) ) THEN
C           sample sea-diquark pair
                               CALL DT_SAMSDQ(Ecm,ixvpr,j,1,irej1)
                               IF ( irej1.EQ.0 ) GOTO 400
                               END IF
                               NVS = NVS + 1
                               ISKpch(6,NVS) = 0
                               INTvs1(NVS) = ixvpr
                               INTvs2(NVS) = j
C>>>>>>>>>>>correct chain kinematics according to minimum chain masses
C           the actual chain masses
                               amvsq1 = XPVq(ixvpr)*XTSaq(j)*Ecm**2
                               amvsq2 = XPVd(ixvpr)*XTSq(j)*Ecm**2
C           get lower mass cuts
                               IF ( ITSq(j).EQ.3 ) THEN
C             q being s-quark
                               amchk1 = AMIS
                               amchk2 = AMAS
                               ELSE
C             q being u/d-quark
                               amchk1 = AMIU
                               amchk2 = AMAU
                               END IF
C           q-aq chain
C             chain mass below minimum - reset sea-aq x-value and correct
C                                        diquark-x of the same nucleon
                               IF ( amvsq1.LT.amchk1 ) THEN
                               xtsqw = amchk1/(XPVq(ixvpr)*Ecm**2)
                               dxtsq = xtsqw - XTSaq(j)
                               IF ( XTVd(itval).GE.xdthr+dxtsq ) THEN
                               XTVd(itval) = XTVd(itval) - dxtsq
                               XTSaq(j) = xtsqw
                               END IF
                               END IF
                               IF ( amvsq2.GT.amchk2 ) THEN
                               xtsqth = amchk2/(XPVd(ixvpr)*Ecm**2)
C*sr 8.4.98 (1/sqrt(x))
                               xtsqxx = DT_SAMPLW(xtsqth,XTSq(j),plw)
C    &                                    DT_SAMSQX(XTSQTH,XTSQ(J))
C    &                                    DT_SAMPEX(XTSQTH,XTSQ(J))
C*
                               XTVd(itval) = XTVd(itval) + XTSq(j)
     &                            - xtsqxx
                               XTSq(j) = xtsqxx
                               ELSE IF ( amvsq2.LT.amchk2 ) THEN
                               xtsqw = amchk2/(XPVd(ixvpr)*Ecm**2)
                               dxtsq = xtsqw - XTSq(j)
                               IF ( XTVd(itval).GE.xdthr+dxtsq ) THEN
                               XTVd(itval) = XTVd(itval) - dxtsq
                               XTSq(j) = xtsqw
                               END IF
                               END IF
C>>>>>>>>>end of chain mass correction
C       jump out of s-s chain loop
                               GOTO 400
                               END IF
                              END IF
                           END DO
                        END IF
C---->end of chain recombination option
 
C     sample sea-diquark pair (projectile)
                        IF ( LSEadi .AND. (DT_RNDM(bsqma).GT.frcdiq) )
     &                       THEN
                           CALL DT_SAMSDQ(Ecm,j,jj,4,irej1)
                           IF ( irej1.EQ.0 ) THEN
                              ISKpch(1,NSS) = 99
                              GOTO 400
                           END IF
                        END IF
C     sample sea-diquark pair (target)
                        IF ( LSEadi .AND. (DT_RNDM(Ecm).GT.frcdiq) )
     &                       THEN
                           CALL DT_SAMSDQ(Ecm,jj,j,3,irej1)
                           IF ( irej1.EQ.0 ) THEN
                              ISKpch(1,NSS) = 99
                              GOTO 400
                           END IF
                        END IF
C>>>>>correct chain kinematics according to minimum chain masses
C     the actual chain masses
                        ssma1q = XPSq(jj)*XTSaq(j)*Ecm**2
                        ssma2q = XPSaq(jj)*XTSq(j)*Ecm**2
C     check for lower mass cuts
                        IF ( (ssma1q.LT.SSMimq) .OR. (ssma2q.LT.SSMimq)
     &                       ) THEN
                           ipval = ITOvp(INTer1(i))
                           itval = ITOvt(INTer2(i))
                           IF ( (XPVd(ipval).GT.xdthr+3.5D0*xssthr)
     &                        .AND. (XTVd(itval).GT.xdthr+3.5D0*xssthr)
     &                        ) THEN
C       maximum allowed x values for sea quarks
                              xspmax = ONE - XPVq(ipval) - xdthr - 
     &                           1.2D0*xssthr
                              xstmax = ONE - XTVq(itval) - xdthr - 
     &                           1.2D0*xssthr
C       resampling of x values not possible - skip sea-sea chains
                              IF ( (xspmax.LE.xssthr+0.05D0) .OR. 
     &                           (xstmax.LE.xssthr+0.05D0) ) GOTO 316
C       resampling of x for projectile sea quark pair
                              icous = 0
 304                          icous = icous + 1
                              IF ( xssthr.GT.0.05D0 ) THEN
                               xpsqi = DT_BETREJ(XSEacu,UNOsea,xssthr,
     &                            xspmax)
                               xpsaqi = DT_BETREJ(XSEacu,UNOsea,xssthr,
     &                            xspmax)
                              ELSE
 306                           xpsqi = DT_DBETAR(XSEacu,UNOsea)
                               IF ( (xpsqi.LT.xssthr) .OR. 
     &                            (xpsqi.GT.xspmax) ) GOTO 306
 308                           xpsaqi = DT_DBETAR(XSEacu,UNOsea)
                               IF ( (xpsaqi.LT.xssthr) .OR. 
     &                            (xpsaqi.GT.xspmax) ) GOTO 308
                              END IF
C       final test of remaining x for projectile diquark
                              xpvdco = XPVd(ipval) - xpsqi - xpsaqi + 
     &                           XPSq(jj) + XPSaq(jj)
                              IF ( xpvdco.LE.xdthr ) THEN
C!!!
C                                IF (ICOUS.LT.5) GOTO 310
                               IF ( icous.LT.0.5D0 ) GOTO 304
                               GOTO 316
                              END IF
C       resampling of x for target sea quark pair
                              icous = 0
 310                          icous = icous + 1
                              IF ( xssthr.GT.0.05D0 ) THEN
                               xtsqi = DT_BETREJ(XSEacu,UNOsea,xssthr,
     &                            xstmax)
                               xtsaqi = DT_BETREJ(XSEacu,UNOsea,xssthr,
     &                            xstmax)
                              ELSE
 312                           xtsqi = DT_DBETAR(XSEacu,UNOsea)
                               IF ( (xtsqi.LT.xssthr) .OR. 
     &                            (xtsqi.GT.xstmax) ) GOTO 312
 314                           xtsaqi = DT_DBETAR(XSEacu,UNOsea)
                               IF ( (xtsaqi.LT.xssthr) .OR. 
     &                            (xtsaqi.GT.xstmax) ) GOTO 314
                              END IF
C       final test of remaining x for target diquark
                              xtvdco = XTVd(itval) - xtsqi - xtsaqi + 
     &                           XTSq(j) + XTSaq(j)
                              IF ( xtvdco.LT.xdthr ) THEN
                               IF ( icous.LT.5 ) GOTO 310
                               GOTO 316
                              END IF
                              XPVd(ipval) = xpvdco
                              XTVd(itval) = xtvdco
                              XPSq(jj) = xpsqi
                              XPSaq(jj) = xpsaqi
                              XTSq(j) = xtsqi
                              XTSaq(j) = xtsaqi
C>>>>>end of chain mass correction
                              GOTO 400
                           END IF
C     come here to discard s-s interaction
C     resampling of x values not allowed or unsuccessful
 316                       intlo(i) = .FALSE.
                           zuost(j) = .TRUE.
                           zuosp(jj) = .TRUE.
                           NSS = NSS - 1
                        END IF
C   consider next s-s interaction
                        GOTO 400
                     END IF
                  END DO
               END IF
            END DO
         END IF
 400  END DO
 
C correct x-values of valence quarks for non-matching sea quarks
      DO i = 1 , IXPs
         IF ( zuosp(i) ) THEN
            ipval = ITOvp(IFRosp(i))
            XPVq(ipval) = XPVq(ipval) + XPSq(i) + XPSaq(i)
            XPSq(i) = ZERO
            XPSaq(i) = ZERO
            zuosp(i) = .FALSE.
         END IF
      END DO
      DO i = 1 , IXTs
         IF ( zuost(i) ) THEN
            itval = ITOvt(IFRost(i))
            XTVq(itval) = XTVq(itval) + XTSq(i) + XTSaq(i)
            XTSq(i) = ZERO
            XTSaq(i) = ZERO
            zuost(i) = .FALSE.
         END IF
      END DO
      DO i = 1 , IXPv
         IF ( zuovp(i) ) ISThkk(IFRovp(i)) = 13
      END DO
      DO i = 1 , IXTv
         IF ( zuovt(i) ) ISThkk(IFRovt(i)+IP) = 14
      END DO
 
      END SUBROUTINE
