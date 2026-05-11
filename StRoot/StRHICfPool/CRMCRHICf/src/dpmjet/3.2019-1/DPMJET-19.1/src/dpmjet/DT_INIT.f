
      SUBROUTINE DT_INIT(Ncases,Epn,Npmass,Npchar,Ntmass,Ntchar,Idp,
     &                   Iglau)
 
C***********************************************************************
C Initialization of event generation                                   *
C This version dated  7.4.98  is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION adp , aphi , aplow , appn , cmener , cmeold , 
     &                 Epn , epnsav , ONE , pdum , phi , plow , ppn , 
     &                 sigav , sumfra , what , xc , xdum , xdumb , xlim1
      DOUBLE PRECISION xlim2 , xlim3 , xmod , xseaco , ZERO
      INTEGER i , ibin , ichain , icw , idip , idit , Idp , idt , idum , 
     &        idum1 , ifirst , Iglau , ihdum , ihshma , ii , iip , iit , 
     &        iplow , ippn
      INTEGER iprang , iratio , irej1 , itlow , iwhat , 
     &        iwhat1 , iwhat2 , ixsqel , j , 
     &        k , kc , kframe , MXCARD , na1 , na2
      INTEGER na3 , na4 , Ncases , ncip , ncit , ncp , nevfit , Npchar , 
     &        Npmass , Ntchar , Ntmass


#ifdef FOR_FLUKA
      INTEGER inseed , iseed1 , iseed2 , isrnd1 , isrnd2 
#endif
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C names of hadrons used in input-cards
      INCLUDE 'inc/dtpain'
#ifdef FOR_FLUKA
      INCLUDE '(DIMPAR)'
      INCLUDE '(PAREVT)'
      INCLUDE '(EVAFLG)'
      INCLUDE '(FRBKCM)'
#else
      INCLUDE 'DIMPAR'
      INCLUDE 'PAREVT'
      INCLUDE 'EVAFLG'
      INCLUDE 'FRBKCM'
#endif
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
C interface HADRIN-DPM
      INCLUDE 'inc/hnthre'
C central particle production, impact parameter biasing
      INCLUDE 'inc/dtimpa'
C parameter for intranuclear cascade
      INCLUDE 'inc/dtfoti'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
C threshold values for x-sampling (DTUNUC 1.x)
      INCLUDE 'inc/dtxcut'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C n-n cross section fluctuations
      INCLUDE 'inc/dtxsfl'
C flags for particle decays
      INCLUDE 'inc/dtfrpa'
C diquark-breaking mechanism
      INCLUDE 'inc/dtdiqb'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
C parameters for hA-diffraction
      INCLUDE 'inc/dtdiha'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C kinematical cuts for lepton-nucleus interactions
      INCLUDE 'inc/dtlcut'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
C cuts for variable energy runs
      INCLUDE 'inc/dtvare'
C flags for activated histograms
      INCLUDE 'inc/dthis3'
 
      INCLUDE 'inc/pydat1'
 
      INCLUDE 'inc/pydat3'
 
C LEPTO
C*LUND single / double precision
      INCLUDE 'inc/leptou'
C LEPTO
      INCLUDE 'inc/leptoi'
C steering flags for qel neutrino scattering modules
      INCLUDE 'inc/qneuto'
C event flag
      INCLUDE 'inc/dtevno'
 
 
      INTEGER PYCOMP
 
C     DIMENSION XPARA(5)
      DIMENSION xdumb(40) , iprang(5)
 
      PARAMETER (MXCARD=64)
      CHARACTER*78 cline , ctitle
      CHARACTER*60 cwhat
      CHARACTER*8 blank , sdum
      CHARACTER*10 code , codewd
      CHARACTER*72 header
      LOGICAL lstart , leinp , lxstab, lext
      DIMENSION what(6) , code(MXCARD)
      DATA code/'TITLE     ' , 'PROJPAR   ' , 'TARPAR    ' , 
     &     'ENERGY    ' , 'MOMENTUM  ' , 'CMENERGY  ' , 'EMULSION  ' , 
     &     'FERMI     ' , 'TAUFOR    ' , 'PAULI     ' , 'COULOMB   ' , 
     &     'HADRIN    ' , 'EVAP      ' , 'EMCCHECK  ' , 'MODEL     ' , 
     &     'PHOINPUT  ' , 'GLAUBERI  ' , 'FLUCTUAT  ' , 'CENTRAL   ' , 
     &     'RECOMBIN  ' , 'COMBIJET  ' , 'XCUTS     ' , 'INTPT     ' , 
     &     'CRONINPT  ' , 'SEADISTR  ' , 'SEASU3    ' , 'DIQUARKS  ' , 
     &     'RESONANC  ' , 'DIFFRACT  ' , 'SINGLECH  ' , 'NOFRAGME  ' , 
     &     'HADRONIZE ' , 'POPCORN   ' , 'PARDECAY  ' , 'BEAM      ' , 
     &     'LUND-MSTU ' , 'LUND-MSTJ ' , 'LUND-MDCY ' , 'LUND-PARJ ' , 
     &     'LUND-PARU ' , 'OUTLEVEL  ' , 'FRAME     ' , 'L-TAG     ' , 
     &     'L-ETAG    ' , 'ECMS-CUT  ' , 'VDM-PAR1  ' , 'HISTOGRAM ' , 
     &     'XS-TABLE  ' , 'GLAUB-PAR ' , 'GLAUB-INI ' , 'VDM-PAR2  ' , 
     &     'XS-QELPRO ' , 'RNDMINIT  ' , 'LEPTO-CUT ' , 'LEPTO-LST ' , 
     &     'LEPTO-PARL' , 'START     ' , 'STOP      ' , 'FUSION    ' , 
     &     'FLOW      ' , 'COLLSCA   ' , 'CHTWOMES  ' , '          ' , 
     &     '          '/
      DATA blank/'        '/
 
      DATA lstart , lxstab , ifirst/.TRUE. , .FALSE. , 1/
      DATA lext /.FALSE./
      DATA cmeold/0.0D0/
 
C---------------------------------------------------------------------
C at the first call of INIT: initialize event generation
      epnsav = Epn
      IF ( lstart ) THEN
         CALL DT_TITLE
C   initialization and test of the random number generator
#ifdef FOR_FLUKA
         IF ( ITRspt.NE.1 ) THEN
 
            CALL FLRNOC(isrnd1,isrnd2,iseed1,iseed2)
            isrnd1 = -1
            inseed = 1
            CALL RNINIT(inseed,isrnd1,iseed1,iseed2)
 
         END IF
#else
C         CALL DT_RNDMTE(1)
         IF ( ITRspt.NE.1 ) CALL DT_RNDMST(22,54,76,92)
#endif
C   initialization of BAMJET, DECAY and HADRIN
         CALL DT_DDATAR
         CALL DT_DHADDE
         CALL DT_DCHANT
         CALL DT_DCHANH
C   set default values for input variables
         CALL DT_DEFAUL(Epn,ppn)
         Iglau = 0
         ixsqel = 0
C   flag for collision energy input
         leinp = .FALSE.
         lstart = .FALSE.
      END IF
 
C---------------------------------------------------------------------
 100  CONTINUE
C bypass reading input cards (e.g. for use with Fluka)
C  in this case Epn is expected to carry the beam momentum
#ifdef FOR_FLUKA
      IF ( Ncases.EQ.-1 ) THEN
         IP = Npmass
         IPZ = Npchar
         ppn = epnsav
         Epn = ZERO
         cmener = ZERO
         leinp = .TRUE.
         MKCron = 0
         what(1) = 1
         what(2) = 0
         codewd = 'START     '
         lext = .TRUE.
         GOTO 300
      END IF
#else
#ifdef FOR_CORSIKA
      IF (NCASES.LE.-1) THEN        !variable energy with air (TP20170630)
         IP      = NPMASS
         IPZ     = NPCHAR
         IT      = NTMASS
         ITZ     = NTCHAR
         PPN     = EPNSAV
         VARELO = 10.D0
         VAREHI = PPN*1.1D0
         EPN     = ZERO
         CMENER  = ZERO
         LEINP   = .TRUE.
         MKCRON  = 0
         WHAT(1) = 1
         WHAT(2) = 0
         CODEWD  = 'START     '
         lext = .TRUE.
         LEVPRT = .TRUE.
         IF(NCASES.EQ.-2)THEN
           IOGLB  = 0           ! don't use glauber tables
         ELSE
           IOGLB  = 100         ! use glauber tables
         ENDIF
         GOTO 300
      ELSEIF (NCASES.EQ.-100) THEN        !make glauber table
         if(ifirst.ne.1)stop
         ifirst=2
         IP      = NPMASS
         IPZ     = NPCHAR
         IT      = NTMASS
         ITZ     = NTCHAR
         PPN     = EPNSAV
         WHAT(1) = 10.D0
         WHAT(2) = PPN*1.1D0
         WHAT(3) = 7d0
         WHAT(4) = 56d0
         WHAT(5) = 7d0 
         CODEWD  = 'GLAUB-INI'
         goto 300
      ENDIF
#else
      IF ( Ncases.EQ.-1 ) THEN
         IP = Npmass
         IPZ = Npchar
         IT = Ntmass
         ITZ = Ntchar
         ppn = epnsav
         VARelo = 10.D0
         VARehi = Epn*1.D0
         Epn = ZERO
         cmener = ZERO
         leinp = .TRUE.
         MKCron = 0
         what(1) = 1
         what(2) = 0
         codewd = 'START     '
         lext = .TRUE.
         LEVprt = .TRUE.
         GOTO 300
      END IF
#endif
#endif
C read control card from input-unit LINP
      READ (LINp,'(A78)',END=400) cline
      IF ( cline(1:1).EQ.'*' ) THEN
C comment-line
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A78)') cline
         GOTO 100
      END IF
C     READ(CLINE,1000,END=9999) CODEWD,(WHAT(I),I=1,6),SDUM
C1000 FORMAT(A10,6E10.0,A8)
      DO i = 1 , 6
         what(i) = ZERO
      END DO
      READ (cline,99010,END=400) codewd , cwhat , sdum
99010 FORMAT (A10,A60,A8)
      READ (cwhat,*,END=200) (what(i),i=1,6)
 
 200  IF ( LPRi.GT.4 ) WRITE (LOUt,99110) codewd , (what(i),i=1,6) , 
     &                        sdum
 
 
C check for valid control card and get card index
 300  icw = 0
      DO i = 1 , MXCARD
         IF ( codewd.EQ.code(i) ) icw = i
      END DO
      IF ( icw.EQ.0 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) codewd
99020    FORMAT (/,1X,'---> ',A10,': invalid control-card !',/)
         GOTO 100
      END IF
 
C------------------------------------------------------------
C       TITLE   ,  PROJPAR ,  TARPAR  ,  ENERGY  ,  MOMENTUM,
C
C------------------------------------------------------------
C       CMENERGY,  EMULSION,  FERMI   ,  TAUFOR  ,  PAULI   ,
C
C------------------------------------------------------------
C       COULOMB ,  HADRIN  ,  EVAP    ,  EMCCHECK,  MODEL   ,
C
C------------------------------------------------------------
C       PHOINPUT,  GLAUBERI,  FLUCTUAT,  CENTRAL ,  RECOMBIN,
C
C------------------------------------------------------------
C       COMBIJET,  XCUTS   ,  INTPT   ,  CRONINPT,  SEADISTR,
C
C------------------------------------------------------------
C       SEASU3  ,  DIQUARKS,  RESONANC,  DIFFRACT,  SINGLECH,
C
C------------------------------------------------------------
C       NOFRAGME, HADRONIZE,  POPCORN ,  PARDECAY,  BEAM    ,
C
C------------------------------------------------------------
C      LUND-MSTU, LUND-MSTJ, LUND-MDCY, LUND-PARJ, LUND-PARU,
C
C------------------------------------------------------------
C       OUTLEVEL,  FRAME   , L-TAG    ,  L-ETAG  ,  ECMS-CUT,
C
C------------------------------------------------------------
C       VDM-PAR1, HISTOGRAM, XS-TABLE , GLAUB-PAR, GLAUB-INI,
C
C------------------------------------------------------------
C               ,          ,  VDM-PAR2, XS-QELPRO, RNDMINIT ,
C
C------------------------------------------------------------
C      LEPTO-CUT, LEPTO-LST,LEPTO-PARL,  START   ,  STOP    ,
C      FUSION  , FLOW     ,COLLSCA    ,  CHTWOMES   ,        )
      IF ( icw.EQ.1 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = TITLE                       *
C                                                                   *
C       what (1..6), sdum   no meaning                              *
C                                                                   *
C       Note:  The control-card following this must consist of      *
C              a string of characters usually giving the title of   *
C              the run.                                             *
C                                                                   *
C********************************************************************
 
         READ (LINp,'(A78)') ctitle
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(//,5X,A78,//)') ctitle
         GOTO 100
      ELSE IF ( icw.EQ.2 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = PROJPAR                     *
C                                                                   *
C       what (1) =  mass number of projectile nucleus  default: 1   *
C       what (2) =  charge of projectile nucleus       default: 1   *
C       what (3..6)   no meaning                                    *
C       sdum        projectile particle code word                   *
C                                                                   *
C       Note: If sdum is defined what (1..2) have no meaning.       *
C                                                                   *
C********************************************************************
 
         IF ( sdum.EQ.blank ) THEN
            IP = INT(what(1))
            IPZ = INT(what(2))
            IJProj = 1
            IBProj = 1
         ELSE
            IJProj = 0
            DO ii = 1 , 30
               IF ( sdum.EQ.BTYpe(ii) ) THEN
                  IP = 1
                  IPZ = 1
                  IF ( ii.EQ.26 ) THEN
                     IJProj = 135
                  ELSE IF ( ii.EQ.27 ) THEN
                     IJProj = 136
                  ELSE IF ( ii.EQ.28 ) THEN
                     IJProj = 133
                  ELSE IF ( ii.EQ.29 ) THEN
                     IJProj = 134
                  ELSE
                     IJProj = ii
                  END IF
                  IBProj = IIBar(IJProj)
C photon
                  IF ( (IJProj.EQ.7) .AND. (what(1).GT.ZERO) )
     &                 VIRt = what(1)
C lepton
                  IF ( ((IJProj.EQ.3) .OR. (IJProj.EQ.4) .OR. 
     &                 (IJProj.EQ.10) .OR. (IJProj.EQ.11)) .AND. 
     &                 (what(1).GT.ZERO) ) Q2Hi = what(1)
               END IF
            END DO
            IF ( IJProj.EQ.0 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99030)
99030          FORMAT (/,1X,'invalid PROJPAR card !',/)
               GOTO 400
            END IF
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.3 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = TARPAR                      *
C                                                                   *
C       what (1) =  mass number of target nucleus      default: 1   *
C       what (2) =  charge of target nucleus           default: 1   *
C       what (3..6)   no meaning                                    *
C       sdum        target particle code word                       *
C                                                                   *
C       Note: If sdum is defined what (1..2) have no meaning.       *
C                                                                   *
C********************************************************************
 
         IF ( sdum.EQ.blank ) THEN
            IT = INT(what(1))
            ITZ = INT(what(2))
            IJTarg = 1
            IBTarg = 1
         ELSE
            IJTarg = 0
            DO ii = 1 , 30
               IF ( sdum.EQ.BTYpe(ii) ) THEN
                  IT = 1
                  ITZ = 1
                  IJTarg = ii
                  IBTarg = IIBar(IJTarg)
               END IF
            END DO
            IF ( IJTarg.EQ.0 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99040)
99040          FORMAT (/,1X,'invalid TARPAR card !',/)
               GOTO 400
            END IF
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.4 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = ENERGY                      *
C                                                                   *
C       what (1) =  energy (GeV) of projectile in Lab.              *
C                   if what(1) < 0:  |what(1)| = kinetic energy     *
C                                                default: 200 GeV   *
C                   if |what(2)| > 0: min. energy for variable      *
C                                     energy runs                   *
C       what (2) =  max. energy for variable energy runs            *
C                   if what(2) < 0:  |what(2)| = kinetic energy     *
C                                                                   *
C********************************************************************
 
         Epn = what(1)
         ppn = ZERO
         cmener = ZERO
         IF ( (ABS(what(2)).GT.ZERO) .AND. 
     &        (ABS(what(2)).GT.ABS(what(1))) ) THEN
            VARelo = what(1)
            VARehi = what(2)
            Epn = VARehi
         END IF
         leinp = .TRUE.
         GOTO 100
      ELSE IF ( icw.EQ.5 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = MOMENTUM                    *
C                                                                   *
C       what (1) =  momentum (GeV/c) of projectile in Lab.          *
C                                                default: 200 GeV/c *
C       what (2..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         Epn = ZERO
         ppn = what(1)
         cmener = ZERO
         leinp = .TRUE.
         GOTO 100
      ELSE IF ( icw.EQ.6 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = CMENERGY                    *
C                                                                   *
C       what (1) =  energy in nucleon-nucleon cms.                  *
C                                                default: none      *
C       what (2..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         Epn = ZERO
         ppn = ZERO
         cmener = what(1)
         leinp = .TRUE.
         GOTO 100
      ELSE IF ( icw.EQ.7 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = EMULSION                    *
C                                                                   *
C               definition of nuclear emulsions                     *
C                                                                   *
C     what(1)      mass number of emulsion component                *
C     what(2)      charge of emulsion component                     *
C     what(3)      fraction of events in which a scattering on a    *
C                  nucleus of this properties is performed          *
C     what(4,5,6)  as what(1,2,3) but for another component         *
C                                             default: no emulsion  *
C     sdum         no meaning                                       *
C                                                                   *
C     Note: If this input-card is once used with valid parameters   *
C           TARPAR is obsolete.                                     *
C           Not the absolute values of the fractions are important  *
C           but only the ratios of fractions of different comp.     *
C           This control card can be repeatedly used to define      *
C           emulsions consisting of up to 10 elements.              *
C                                                                   *
C********************************************************************
 
         IF ( (what(1).GT.ZERO) .AND. (what(2).GT.ZERO) .AND. 
     &        (ABS(what(3)).GT.ZERO) ) THEN
            NCOmpo = NCOmpo + 1
            IF ( NCOmpo.GT.NCOMPX ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99050)
99050          FORMAT (1X,
     &                 'too many emulsion components - program stopped')
               STOP
            END IF
            IEMuma(NCOmpo) = INT(what(1))
            IEMuch(NCOmpo) = INT(what(2))
            EMUfra(NCOmpo) = what(3)
            IEMul = 1
C        CALL SHMAKF(IDUM,IDUM,IEMUMA(NCOMPO),IEMUCH(NCOMPO))
         END IF
         IF ( (what(4).GT.ZERO) .AND. (what(5).GT.ZERO) .AND. 
     &        (ABS(what(6)).GT.ZERO) ) THEN
            NCOmpo = NCOmpo + 1
            IF ( NCOmpo.GT.NCOMPX ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99110)
               STOP
            END IF
            IEMuma(NCOmpo) = INT(what(4))
            IEMuch(NCOmpo) = INT(what(5))
            EMUfra(NCOmpo) = what(6)
C        CALL SHMAKF(IDUM,IDUM,IEMUMA(NCOMPO),IEMUCH(NCOMPO))
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.8 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = FERMI                       *
C                                                                   *
C       what (1) = -1 Fermi-motion of nucleons not treated          *
C                                                 default: 1        *
C       what (2) =    scale factor for Fermi-momentum               *
C                                                 default: 0.75     *
C       what (3..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         IF ( what(1).EQ.-1.0D0 ) THEN
            LFErmi = .FALSE.
         ELSE
            LFErmi = .TRUE.
         END IF
         xmod = what(2)
         IF ( xmod.GE.ZERO ) FERmod = xmod
         GOTO 100
      ELSE IF ( icw.EQ.9 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = TAUFOR                      *
C                                                                   *
C          formation time supressed intranuclear cascade            *
C                                                                   *
C    what (1)      formation time (in fm/c)                         *
C                  note: what(1)=10. corresponds roughly to an      *
C                        average formation time of 1 fm/c           *
C                                                 default: 5. fm/c  *
C    what (2)      number of generations followed                   *
C                                                 default: 25       *
C    what (3) = 1. p_t-dependent formation zone                     *
C             = 2. constant formation zone                          *
C                                                 default: 1        *
C    what (4)      modus of selection of nucleus where the          *
C                  cascade if followed first                        *
C             = 1.  proj./target-nucleus with probab. 1/2           *
C             = 2.  nucleus with highest mass                       *
C             = 3.  proj. nucleus if particle is moving in pos. z   *
C                   targ. nucleus if particle is moving in neg. z   *
C                                                 default: 1        *
C    what (5..6), sdum   no meaning                                 *
C                                                                   *
C********************************************************************
 
         TAUfor = what(1)
         KTAuge = INT(what(2))
         INCmod = 1
         IF ( (what(3).GE.1.0D0) .AND. (what(3).LE.2.0D0) )
     &        ITAuve = INT(what(3))
         IF ( (what(4).GE.1.0D0) .AND. (what(4).LE.3.0D0) )
     &        INCmod = INT(what(4))
         GOTO 100
      ELSE IF ( icw.EQ.10 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = PAULI                       *
C                                                                   *
C       what (1) =  -1  Pauli's principle for secondary             *
C                       interactions not treated                    *
C                                                    default: 1     *
C       what (2..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         IF ( what(1).EQ.-1.0D0 ) THEN
            LPAuli = .FALSE.
         ELSE
            LPAuli = .TRUE.
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.11 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = COULOMB                     *
C                                                                   *
C       what (1) = -1. Coulomb-energy treatment switched off        *
C                                                    default: 1     *
C       what (2..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         ICOul = 1
         IF ( what(1).EQ.-1.0D0 ) THEN
            ICOul = 0
         ELSE
            ICOul = 1
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.12 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = HADRIN                      *
C                                                                   *
C                       HADRIN module                               *
C                                                                   *
C    what (1) = 0. elastic/inelastic interactions with probab.      *
C                  as defined by cross-sections                     *
C             = 1. inelastic interactions forced                    *
C             = 2. elastic interactions forced                      *
C                                                 default: 1        *
C    what (2)      upper threshold in total energy (GeV) below      *
C                  which interactions are sampled by HADRIN         *
C                                                 default: 5. GeV   *
C    what (3..6), sdum   no meaning                                 *
C                                                                   *
C********************************************************************
 
         iwhat = INT(what(1))
         IF ( (iwhat.GE.0) .AND. (iwhat.LE.2) ) INThad = iwhat
         IF ( (what(2).GT.ZERO) .AND. (what(2).LT.15.0D0) )
     &        EHAdth = what(2)
 
C********************************************************************
C                                                                   *
C               control card:  codewd = EVAP                        *
C                                                                   *
C                    evaporation module                             *
C                                                                   *
C********************************************************************
C                                                                   *
C               control card:  codewd = EVAP                        *
C                                                                   *
C                    evaporation module                             *
C                                                                   *
C  Obsoleted by A.Ferrari, it can no longer work since FLUKA2005.6  *
C                                                                   *
C********************************************************************
 
         GOTO 100
      ELSE IF ( icw.EQ.13 ) THEN
         GOTO 100
      ELSE IF ( icw.EQ.14 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = EMCCHECK                    *
C                                                                   *
C    extended energy-momentum / quantum-number conservation check   *
C                                                                   *
C       what (1) = -1   extended check not performed                *
C                                                    default: 1.    *
C       what (2..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         IF ( what(1).EQ.-1 ) THEN
            LEMcck = .FALSE.
         ELSE
            LEMcck = .TRUE.
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.15 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = MODEL                       *
C                                                                   *
C     Model to be used to treat nucleon-nucleon interactions        *
C                                                                   *
C       sdum = DTUNUC    two-chain model                            *
C            = PHOJET    multiple chains including minijets         *
C            = LEPTO     DIS                                        *
C            = QNEUTRIN  quasi-elastic neutrino scattering          *
C                                                  default: PHOJET  *
C                                                                   *
C       if sdum = LEPTO:                                            *
C       what (1)         (variable INTER)                           *
C                        = 1  gamma exchange                        *
C                        = 2  W+-   exchange                        *
C                        = 3  Z0    exchange                        *
C                        = 4  gamma/Z0 exchange                     *
C                                                                   *
C       if sdum = QNEUTRIN:                                         *
C       what (1)         = 0  elastic scattering on nucleon and     *
C                             tau does not decay (default)          *
C                        = 1  decay of tau into mu..                *
C                        = 2  decay of tau into e..                 *
C                        = 10 CC events on p and n                  *
C                        = 11 NC events on p and n                  *
C                                                                   *
C       what (2..6)      no meaning                                 *
C                                                                   *
C********************************************************************
 
         IF ( sdum.EQ.CMOdel(1) ) THEN
            MCGene = 1
         ELSE IF ( sdum.EQ.CMOdel(2) ) THEN
            MCGene = 2
         ELSE IF ( sdum.EQ.CMOdel(3) ) THEN
            MCGene = 3
            IF ( (what(1).GE.1.0D0) .AND. (what(1).LE.4.0D0) )
     &           INTer = INT(what(1))
         ELSE IF ( sdum.EQ.CMOdel(4) ) THEN
            MCGene = 4
            iwhat = INT(what(1))
            IF ( (iwhat.EQ.1) .OR. (iwhat.EQ.2) .OR. (iwhat.EQ.10) .OR. 
     &           (iwhat.EQ.11) ) NEUdec = iwhat
         ELSE
            STOP ' Unknown model !'
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.16 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = PHOINPUT                    *
C                                                                   *
C       Start of input-section for PHOJET-specific input-cards      *
C       Note:  This section will not be finished before giving      *
C              ENDINPUT-card                                        *
C       what (1..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         IF ( LPHoin ) THEN
 
            irej1 = LPRi
            CALL PHO_INIT(LINp,LOUt,irej1)
 
            IF ( irej1.NE.0 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &               'INIT:   reading PHOJET-input failed'
               STOP
            END IF
            LPHoin = .FALSE.
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.17 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = GLAUBERI                    *
C                                                                   *
C        Pre-initialization of impact parameter selection           *
C                                                                   *
C        what (1..6), sdum   no meaning                             *
C                                                                   *
C********************************************************************
 
         IF ( ifirst.NE.99 ) THEN
            CALL DT_RNDMST(12,34,56,78)
            CALL DT_RNDMTE(1)
            OPEN (40,FILE='outdata0/shm.out',STATUS='UNKNOWN')
C        OPEN(11,FILE='outdata0/shm.dbg',STATUS='UNKNOWN')
            ifirst = 99
         END IF
 
         ippn = 8
         plow = 10.0D0
C     IPPN = 1
C     PLOW = 100.0D0
         phi = 1.0D5
         aplow = LOG10(plow)
         aphi = LOG10(phi)
         adp = (aphi-aplow)/DBLE(ippn)
 
         iplow = 1
         idip = 1
         iip = 5
C     IPLOW = 1
C     IDIP  = 1
C     IIP   = 1
         iprang(1) = 1
         iprang(2) = 2
         iprang(3) = 5
         iprang(4) = 10
         iprang(5) = 20
 
         itlow = 30
         idit = 3
         iit = 60
C     IDIT  = 10
C     IIT   = 21
 
         DO ncit = 1 , iit
            IT = itlow + (ncit-1)*idit
C        IPHI = IT
C        IDIP = 10
C        IIP  = (IPHI-IPLOW)/IDIP
C        IF (IIP.EQ.0) IIP = 1
C        IF (IT.EQ.IPLOW) IIP = 0
 
            DO ncip = 1 , iip
               IP = iprang(ncip)
CC           IF (NCIP.LE.IIP) THEN
C               IP = IPLOW+(NCIP-1)*IDIP
CC           ELSE
CC              IP = IT
CC           ENDIF
               IF ( IP.LE.IT ) THEN
 
                  DO ncp = 1 , ippn + 1
                     appn = aplow + DBLE(ncp-1)*adp
                     ppn = 10**appn
 
                     OPEN (12,FILE='outdata0/shm.sta',STATUS='UNKNOWN')
                     WRITE (12,'(1X,2I5,E15.3)') IP , IT , ppn
                     CLOSE (12)
 
                     xlim1 = 0.0D0
                     xlim2 = 50.0D0
                     xlim3 = ZERO
                     ibin = 50
                     CALL DT_NEWHGR(xdum,xdum,xdum,xdumb,-1,ihdum)
                     CALL DT_NEWHGR(xlim1,xlim2,xlim3,xdumb,ibin,ihshma)
 
                     nevfit = 5
C              IF ((IP.GT.10).OR.(IT.GT.10)) THEN
C                 NEVFIT = 5
C              ELSE
C                 NEVFIT = 10
C              ENDIF
                     sigav = 0.0D0
 
                     DO i = 1 , nevfit
                        CALL DT_SHMAKI(IP,idum1,IT,idum1,IJProj,ppn,99)
                        sigav = sigav + XSPro(1,1,1)
                        DO j = 1 , 50
                           xc = DBLE(j)
                           CALL DT_FILHGR(xc,BSIte(1,1,1,j),ihshma,i)
                        END DO
                     END DO
 
                     CALL DT_EVTHIS(idum)
                     header = ' BSITE'
C              CALL OUTGEN(IHSHMA,0,0,0,0,0,HEADER,0,NEVFIT,ONE,0,1,-1)
 
C              CALL GENFIT(XPARA)
C              WRITE(40,'(2I4,E11.3,F6.0,5E11.3)')
C    &              IP,IT,PPN,SIGAV/DBLE(NEVFIT),XPARA
 
                  END DO
               END IF
 
            END DO
 
         END DO
 
         STOP
      ELSE IF ( icw.EQ.18 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = FLUCTUAT                    *
C                                                                   *
C           Treatment of cross section fluctuations                 *
C                                                                   *
C       what (1) = 1  treat cross section fluctuations              *
C                                                    default: 0.    *
C       what (1..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         IFLuct = 0
         IF ( what(1).EQ.ONE ) THEN
            IFLuct = 1
            CALL DT_FLUINI
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.19 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = CENTRAL                     *
C                                                                   *
C       what (1) = 1.  central production forced     default: 0     *
C  if what (1) < 0 and > -100                                       *
C       what (2) = min. impact parameter             default: 0     *
C       what (3) = max. impact parameter             default: b_max *
C  if what (1) < -99                                                *
C       what (2) = fraction of cross section         default: 1     *
C  if what (1) = -1 : evaporation/fzc suppressed                    *
C  if what (1) < -1 : evaporation/fzc allowed                       *
C                                                                   *
C       what (4..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         ICEntr = INT(what(1))
         IF ( ICEntr.LT.0 ) THEN
            IF ( ICEntr.GT.-100 ) THEN
               BIMin = what(2)
               BIMax = what(3)
            ELSE
               XSFrac = what(2)
            END IF
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.20 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = RECOMBIN                    *
C                                                                   *
C                     Chain recombination                           *
C        (recombine S-S and V-V chains to V-S chains)               *
C                                                                   *
C       what (1) = -1. recombination switched off    default: 1     *
C       what (2..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         IREcom = 1
         IF ( what(1).EQ.-1.0D0 ) IREcom = 0
         GOTO 100
      ELSE IF ( icw.EQ.21 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = COMBIJET                    *
C                                                                   *
C               chain fusion (2 q-aq --> qq-aqaq)                   *
C                                                                   *
C       what (1) = 1   fusion treated                               *
C                                                    default: 0.    *
C       what (2)       minimum number of uncombined chains from     *
C                      single projectile or target nucleons         *
C                                                    default: 0.    *
C       what (3..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         LCO2cr = .FALSE.
         IF ( INT(what(1)).EQ.1 ) LCO2cr = .TRUE.
         IF ( what(2).GE.ZERO ) CUTof = what(2)
         GOTO 100
      ELSE IF ( icw.EQ.22 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = XCUTS                       *
C                                                                   *
C                 thresholds for x-sampling                         *
C                                                                   *
C    what (1)    defines lower threshold for val.-q x-value (CVQ)   *
C                                                 default: 1.       *
C    what (2)    defines lower threshold for val.-qq x-value (CDQ)  *
C                                                 default: 2.       *
C    what (3)    defines lower threshold for sea-q x-value (CSEA)   *
C                                                 default: 0.2      *
C    what (4)    sea-q x-values in S-S chains (SSMIMA)              *
C                                                 default: 0.14     *
C    what (5)    not used                                           *
C                                                 default: 2.       *
C    what (6), sdum   no meaning                                    *
C                                                                   *
C    Note: Lower thresholds (what(1..3)) are def. as x_thr=CXXX/ECM *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GE.0.5D0 ) CVQ = what(1)
         IF ( what(2).GE.ONE ) CDQ = what(2)
         IF ( what(3).GE.0.1D0 ) CSEa = what(3)
         IF ( what(4).GE.ZERO ) THEN
            SSMima = what(4)
            SSMimq = SSMima**2
         END IF
         IF ( what(5).GT.2.0D0 ) VVMthr = what(5)
         GOTO 100
      ELSE IF ( icw.EQ.23 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = INTPT                       *
C                                                                   *
C     what (1) = -1   intrinsic transverse momenta of partons       *
C                     not treated                default: 1         *
C     what (2..6), sdum   no meaning                                *
C                                                                   *
C********************************************************************
 
         IF ( what(1).EQ.-1.0D0 ) THEN
            LINtpt = .FALSE.
         ELSE
            LINtpt = .TRUE.
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.24 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = CRONINPT                    *
C                                                                   *
C    Cronin effect (multiple scattering of partons at chain ends)   *
C                                                                   *
C       what (1) = -1  Cronin effect not treated     default: 1     *
C       what (2) = 0   scattering parameter          default: 0.64  *
C       what (3..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         IF ( what(1).EQ.-1.0D0 ) THEN
            MKCron = 0
         ELSE
            MKCron = 1
         END IF
         CROnco = what(2)
         GOTO 100
      ELSE IF ( icw.EQ.25 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = SEADISTR                    *
C                                                                   *
C     what (1)  (XSEACO)  sea(x) prop. 1/x**what (1)   default: 1.  *
C     what (2)  (UNON)                                 default: 2.  *
C     what (3)  (UNOM)                                 default: 1.5 *
C     what (4)  (UNOSEA)                               default: 5.  *
C                        qdis(x) prop. (1-x)**what (1)  etc.        *
C     what (5..6), sdum   no meaning                                *
C                                                                   *
C********************************************************************
 
         xseaco = what(1)
         XSEacu = 1.05D0 - xseaco
         UNOn = what(2)
         IF ( UNOn.LT.0.1D0 ) UNOn = 2.0D0
         UNOm = what(3)
         IF ( UNOm.LT.0.1D0 ) UNOm = 1.5D0
         UNOsea = what(4)
         IF ( UNOsea.LT.0.1D0 ) UNOsea = 5.0D0
         GOTO 100
      ELSE IF ( icw.EQ.26 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = SEASU3                      *
C                                                                   *
C          Treatment of strange-quarks at chain ends                *
C                                                                   *
C       what (1)   (SEASQ)  strange-quark supression factor         *
C                  iflav = 1.+rndm*(2.+SEASQ)                       *
C                                                    default: 1.    *
C       what (2..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         SEAsq = what(1)
         GOTO 100
      ELSE IF ( icw.EQ.27 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = DIQUARKS                    *
C                                                                   *
C     what (1) = -1.  sea-diquark/antidiquark-pairs not treated     *
C                                                    default: 1.    *
C     what (2..6), sdum   no meaning                                *
C                                                                   *
C********************************************************************
 
         IF ( what(1).EQ.-1.0D0 ) THEN
            LSEadi = .FALSE.
         ELSE
            LSEadi = .TRUE.
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.28 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = RESONANC                    *
C                                                                   *
C                 treatment of low mass chains                      *
C                                                                   *
C    what (1) = -1 low chain masses are not corrected for resonance *
C                  masses (obsolete for BAMJET-fragmentation)       *
C                                       default: 1.                 *
C    what (2) = -1 massless partons     default: 1. (massive)       *
C                                       default: 1. (massive)       *
C    what (3) = -1 chain-system containing chain of too small       *
C                  mass is rejected (note: this does not fully      *
C                  apply to S-S chains) default: 0.                 *
C    what (4..6), sdum   no meaning                                 *
C                                                                   *
C********************************************************************
 
         IREsco = 1
         IMShl = 1
         IREsrj = 0
         IF ( what(1).EQ.-ONE ) IREsco = 0
         IF ( what(2).EQ.-ONE ) IMShl = 0
         IF ( what(3).EQ.-ONE ) IREsrj = 1
         GOTO 100
      ELSE IF ( icw.EQ.29 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = DIFFRACT                    *
C                                                                   *
C                Treatment of diffractive events                    *
C                                                                   *
C     what (1) = (ISINGD) 0  no single diffraction                  *
C                         1  single diffraction included            *
C                       +-2  single diffractive events only         *
C                       +-3  projectile single diffraction only     *
C                       +-4  target single diffraction only         *
C                        -5  double pomeron exchange only           *
C                      (neg. sign applies to PHOJET events)         *
C                                                     default: 0.   *
C                                                                   *
C     what (2) = (IDOUBD) 0  no double diffraction                  *
C                         1  double diffraction included            *
C                         2  double diffractive events only         *
C                                                     default: 0.   *
C     what (3) = 1 projectile diffraction treated (2-channel form.) *
C                                                     default: 0.   *
C     what (4) = alpha-parameter in projectile diffraction          *
C                                                     default: 0.   *
C     what (5..6), sdum   no meaning                                *
C                                                                   *
C********************************************************************
 
         IF ( ABS(what(1)).GT.ZERO ) ISIngd = INT(what(1))
         IF ( ABS(what(2)).GT.ZERO ) IDOubd = INT(what(2))
         IF ( (ISIngd.GT.1) .AND. (IDOubd.GT.1) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99060)
99060       FORMAT (1X,'INIT:   inconsistent DIFFRACT - input !',/,11X,
     &              'IDOUBD is reset to zero')
            IDOubd = 0
         END IF
         IF ( what(3).GT.ZERO ) DIBeta = what(3)
         IF ( what(4).GT.ZERO ) DIAlph = what(4)
         GOTO 100
      ELSE IF ( icw.EQ.30 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = SINGLECH                    *
C                                                                   *
C       what (1) = 1.  Regge contribution (one chain) included      *
C                                                   default: 0.     *
C       what (2..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         ISIcha = 0
         IF ( what(1).EQ.ONE ) ISIcha = 1
         GOTO 100
      ELSE IF ( icw.EQ.31 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = NOFRAGME                    *
C                                                                   *
C                 biased chain hadronization                        *
C                                                                   *
C       what (1..6) = -1  no of hadronizsation of S-S chains        *
C                   = -2  no of hadronizsation of D-S chains        *
C                   = -3  no of hadronizsation of S-D chains        *
C                   = -4  no of hadronizsation of S-V chains        *
C                   = -5  no of hadronizsation of D-V chains        *
C                   = -6  no of hadronizsation of V-S chains        *
C                   = -7  no of hadronizsation of V-D chains        *
C                   = -8  no of hadronizsation of V-V chains        *
C                   = -9  no of hadronizsation of comb. chains      *
C                                  default:  complete hadronization *
C       sdum   no meaning                                           *
C                                                                   *
C********************************************************************
 
         DO i = 1 , 6
            ichain = INT(what(i))
            IF ( (ichain.LE.-1) .AND. (ichain.GE.-9) )
     &           LHAdro(ABS(ichain)) = .FALSE.
         END DO
         GOTO 100
      ELSE IF ( icw.EQ.32 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = HADRONIZE                   *
C                                                                   *
C           hadronization model and parameter switch                *
C                                                                   *
C       what (1) = 1    hadronization via BAMJET                    *
C                = 2    hadronization via JETSET                    *
C                                                    default: 2     *
C       what (2) = 1..3 parameter set to be used                    *
C                       JETSET: 3 sets available                    *
C                               ( = 3 default JETSET-parameters)    *
C                       BAMJET: 1 set available                     *
C                                                    default: 1     *
C       what (3..6), sdum   no meaning                              *
C                                                                   *
C********************************************************************
 
         iwhat1 = INT(what(1))
         iwhat2 = INT(what(2))
         IF ( (iwhat1.EQ.1) .OR. (iwhat1.EQ.2) ) IFRag(1) = iwhat1
         IF ( (iwhat1.EQ.2) .AND. (iwhat2.GE.1) .AND. (iwhat2.LE.3) )
     &        IFRag(2) = iwhat2
         GOTO 100
      ELSE IF ( icw.EQ.33 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = POPCORN                     *
C                                                                   *
C  "Popcorn-effect" in fragmentation and diquark breaking diagrams  *
C                                                                   *
C   what (1) = (PDB) frac. of diquark fragmenting directly into     *
C                    baryons (PYTHIA/JETSET fragmentation)          *
C                    (JETSET: = 0. Popcorn mechanism switched off)  *
C                                                    default: 0.5   *
C   what (2) = probability for accepting a diquark breaking         *
C              diagram involving the generation of a u/d quark-     *
C              antiquark pair                        default: 0.0   *
C   what (3) = same a what (2), here for s quark-antiquark pair     *
C                                                    default: 0.0   *
C   what (4..6), sdum   no meaning                                  *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GE.0.0D0 ) PDB = what(1)
         IF ( what(2).GE.0.0D0 ) THEN
            PDBsea(1) = what(2)
            PDBsea(2) = what(2)
         END IF
         IF ( what(3).GE.0.0D0 ) PDBsea(3) = what(3)
         DO i = 1 , 8
            DBRka(1,i) = DBRkr(1,i)*PDBsea(1)/(1.D0-PDBsea(1))
            DBRka(2,i) = DBRkr(2,i)*PDBsea(2)/(1.D0-PDBsea(2))
            DBRka(3,i) = DBRkr(3,i)*PDBsea(3)/(1.D0-PDBsea(3))
         END DO
         GOTO 100
      ELSE IF ( icw.EQ.34 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = PARDECAY                    *
C                                                                   *
C      what (1) = 1.  Sigma0/Asigma0 are decaying within JETSET     *
C               = 2.  pion^0 decay after intranucl. cascade         *
C                                                default: no decay  *
C      what (2..6), sdum   no meaning                               *
C                                                                   *
C********************************************************************
 
         IF ( what(1).EQ.ONE ) ISIg0 = 1
         IF ( what(1).EQ.2.0D0 ) IPI0 = 1
         GOTO 100
      ELSE IF ( icw.EQ.35 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = BEAM                        *
C                                                                   *
C              definition of beam parameters                        *
C                                                                   *
C      what (1/2)  > 0 : energy of beam 1/2 (GeV)                   *
C                  < 0 : abs(what(1/2)) energy per charge of        *
C                        beam 1/2 (GeV)                             *
C                  (beam 1 is directed into positive z-direction)   *
C      what (3)    beam crossing angle, defined as 2x angle between *
C                  one beam and the z-axis (micro rad)              *
C      what (4)    angle with x-axis defining the collision plane   *
C      what (5..6), sdum   no meaning                               *
C                                                                   *
C      Note: this card requires previously defined projectile and   *
C            target identities (PROJPAR, TARPAR)                    *
C                                                                   *
C********************************************************************
 
         CALL DT_BEAMPR(what,ppn,1)
         Epn = ZERO
         cmener = ZERO
         leinp = .TRUE.
         GOTO 100
      ELSE IF ( icw.EQ.36 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = LUND-MSTU                   *
C                                                                   *
C          set parameter MSTU in JETSET-common /LUDAT1/             *
C                                                                   *
C       what (1) =  index according to LUND-common block            *
C       what (2) =  new value of MSTU( int(what(1)) )               *
C       what (3), what(4) and what (5), what(6) further             *
C                   parameter in the same way as what (1) and       *
C                   what (2)                                        *
C                        default: default-Lund or corresponding to  *
C                                 the set given in HADRONIZE        *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GT.ZERO ) THEN
            NMStu = NMStu + 1
            IMStu(NMStu) = INT(what(1))
            MSTux(NMStu) = INT(what(2))
         END IF
         IF ( what(3).GT.ZERO ) THEN
            NMStu = NMStu + 1
            IMStu(NMStu) = INT(what(3))
            MSTux(NMStu) = INT(what(4))
         END IF
         IF ( what(5).GT.ZERO ) THEN
            NMStu = NMStu + 1
            IMStu(NMStu) = INT(what(5))
            MSTux(NMStu) = INT(what(6))
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.37 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = LUND-MSTJ                   *
C                                                                   *
C          set parameter MSTJ in JETSET-common /LUDAT1/             *
C                                                                   *
C       what (1) =  index according to LUND-common block            *
C       what (2) =  new value of MSTJ( int(what(1)) )               *
C       what (3), what(4) and what (5), what(6) further             *
C                   parameter in the same way as what (1) and       *
C                   what (2)                                        *
C                        default: default-Lund or corresponding to  *
C                                 the set given in HADRONIZE        *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GT.ZERO ) THEN
            NMStj = NMStj + 1
            IMStj(NMStj) = INT(what(1))
            MSTjx(NMStj) = INT(what(2))
         END IF
         IF ( what(3).GT.ZERO ) THEN
            NMStj = NMStj + 1
            IMStj(NMStj) = INT(what(3))
            MSTjx(NMStj) = INT(what(4))
         END IF
         IF ( what(5).GT.ZERO ) THEN
            NMStj = NMStj + 1
            IMStj(NMStj) = INT(what(5))
            MSTjx(NMStj) = INT(what(6))
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.38 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = LUND-MDCY                   *
C                                                                   *
C  set parameter MDCY(I,1) for particle decays in JETSET-common     *
C                                                      /LUDAT3/     *
C                                                                   *
C       what (1-6) = PDG particle index of particle which should    *
C                    not decay                                      *
C                        default: default-Lund or forced in         *
C                                 DT_INITJS                         *
C                                                                   *
C********************************************************************
 
         DO i = 1 , 6
            IF ( what(i).NE.ZERO ) THEN
 
               kc = PYCOMP(INT(what(i)))
 
               MDCy(kc,1) = 0
            END IF
         END DO
         GOTO 100
      ELSE IF ( icw.EQ.39 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = LUND-PARJ                   *
C                                                                   *
C          set parameter PARJ in JETSET-common /LUDAT1/             *
C                                                                   *
C       what (1) =  index according to LUND-common block            *
C       what (2) =  new value of PARJ( int(what(1)) )               *
C       what (3), what(4) and what (5), what(6) further             *
C                   parameter in the same way as what (1) and       *
C                   what (2)                                        *
C                        default: default-Lund or corresponding to  *
C                                 the set given in HADRONIZE        *
C                                                                   *
C********************************************************************
 
         IF ( what(1).NE.ZERO ) THEN
            NPArj = NPArj + 1
            IPArj(NPArj) = INT(what(1))
            PARjx(NPArj) = what(2)
         END IF
         IF ( what(3).NE.ZERO ) THEN
            NPArj = NPArj + 1
            IPArj(NPArj) = INT(what(3))
            PARjx(NPArj) = what(4)
         END IF
         IF ( what(5).NE.ZERO ) THEN
            NPArj = NPArj + 1
            IPArj(NPArj) = INT(what(5))
            PARjx(NPArj) = what(6)
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.40 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = LUND-PARU                   *
C                                                                   *
C          set parameter PARJ in JETSET-common /LUDAT1/             *
C                                                                   *
C       what (1) =  index according to LUND-common block            *
C       what (2) =  new value of PARU( int(what(1)) )               *
C       what (3), what(4) and what (5), what(6) further             *
C                   parameter in the same way as what (1) and       *
C                   what (2)                                        *
C                        default: default-Lund or corresponding to  *
C                                 the set given in HADRONIZE        *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GT.ZERO ) THEN
            NPAru = NPAru + 1
            IPAru(NPAru) = INT(what(1))
            PARux(NPAru) = what(2)
         END IF
         IF ( what(3).GT.ZERO ) THEN
            NPAru = NPAru + 1
            IPAru(NPAru) = INT(what(3))
            PARux(NPAru) = what(4)
         END IF
         IF ( what(5).GT.ZERO ) THEN
            NPAru = NPAru + 1
            IPAru(NPAru) = INT(what(5))
            PARux(NPAru) = what(6)
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.41 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = OUTLEVEL                    *
C                                                                   *
C                    output control switches                        *
C                                                                   *
C       what (1) =  internal rejection informations  default: 0     *
C       what (2) =  energy-momentum conservation check output       *
C                                                    default: 0     *
C       what (3) =  internal warning messages        default: 0     *
C       what (4..6), sdum    not yet used                           *
C                                                                   *
C********************************************************************
 
         DO k = 1 , 6
            IOUlev(k) = INT(what(k))
         END DO
         GOTO 100
      ELSE IF ( icw.EQ.42 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = FRAME                       *
C                                                                   *
C          frame in which final state is given in DTEVT1            *
C                                                                   *
C       what (1) = 1  target rest frame (laboratory)                *
C                = 2  nucleon-nucleon cms                           *
C                                                    default: 1     *
C                                                                   *
C********************************************************************
 
         kframe = INT(what(1))
         IF ( (kframe.GE.1) .AND. (kframe.LE.2) ) IFRame = kframe
         GOTO 100
      ELSE IF ( icw.EQ.43 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = L-TAG                       *
C                                                                   *
C                        lepton tagger:                             *
C   definition of kinematical cuts for radiated photon and          *
C   outgoing lepton detection in lepton-nucleus interactions        *
C                                                                   *
C       what (1) = y_min                                            *
C       what (2) = y_max                                            *
C       what (3) = Q^2_min                                          *
C       what (4) = Q^2_max                                          *
C       what (5) = theta_min  (Lab)                                 *
C       what (6) = theta_max  (Lab)                                 *
C                                       default: no cuts            *
C       sdum    no meaning                                          *
C                                                                   *
C********************************************************************
 
         YMIn = what(1)
         YMAx = what(2)
         Q2Min = what(3)
         Q2Max = what(4)
         THMin = what(5)
         THMax = what(6)
         GOTO 100
      ELSE IF ( icw.EQ.44 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = L-ETAG                      *
C                                                                   *
C                        lepton tagger:                             *
C       what (1) = min. outgoing lepton energy  (in Lab)            *
C       what (2) = min. photon energy           (in Lab)            *
C       what (3) = max. photon energy           (in Lab)            *
C                                       default: no cuts            *
C       what (2..6), sdum    no meaning                             *
C                                                                   *
C********************************************************************
 
         ELMin = MAX(what(1),ZERO)
         EGMin = MAX(what(2),ZERO)
         EGMax = MAX(what(3),ZERO)
         GOTO 100
      ELSE IF ( icw.EQ.45 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = ECMS-CUT                    *
C                                                                   *
C     what (1) = min. c.m. energy to be sampled                     *
C     what (2) = max. c.m. energy to be sampled                     *
C     what (3) = min x_Bj         to be sampled                     *
C                                       default: no cuts            *
C     what (3..6), sdum    no meaning                               *
C                                                                   *
C********************************************************************
 
         ECMin = what(1)
         ECMax = what(2)
         IF ( ECMin.GT.ECMax ) ECMin = ECMax
         XBJmin = MAX(what(3),ZERO)
         GOTO 100
      ELSE IF ( icw.EQ.46 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = VDM-PAR1                    *
C                                                                   *
C      parameters in gamma-nucleus cross section calculation        *
C                                                                   *
C       what (1) =  Lambda^2                       default: 2.      *
C       what (2)    lower limit in M^2 integration                  *
C                =  1  (3m_pi)^2                                    *
C                =  2  (m_rho0)^2                                   *
C                =  3  (m_phi)^2                   default: 1       *
C       what (3)    upper limit in M^2 integration                  *
C                =  1   s/2                                         *
C                =  2   s/4                                         *
C                =  3   s                          default: 3       *
C       what (4)    CKMT F_2 structure function                     *
C                =  2212  proton                                    *
C                =  100   deuteron                 default: 2212    *
C       what (5)    calculation of gamma-nucleon xsections          *
C                =  1  according to CKMT-parametrization of F_2     *
C                =  2  integrating SIGVP over M^2                   *
C                =  3  using SIGGA                                  *
C                =  4  PHOJET cross sections       default:  4      *
C                                                                   *
C       what (6), sdum    no meaning                                *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GE.ZERO ) RL2 = what(1)
         IF ( (what(2).GE.1) .AND. (what(2).LE.3) ) INTrge(1)
     &        = INT(what(2))
         IF ( (what(3).GE.1) .AND. (what(3).LE.3) ) INTrge(2)
     &        = INT(what(3))
         IF ( (what(4).EQ.2212) .OR. (what(4).EQ.100) )
     &        IDPdf = INT(what(4))
         IF ( (what(5).GE.1) .AND. (what(5).LE.4) )
     &        MODega = INT(what(5))
         GOTO 100
      ELSE IF ( icw.EQ.47 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = HISTOGRAM                   *
C                                                                   *
C           activate different classes of histograms                *
C                                                                   *
C                                default: no histograms             *
C                                                                   *
C********************************************************************
 
         DO j = 1 , 6
            IF ( (what(j).GE.100) .AND. (what(j).LE.150) ) THEN
               IHIspp(INT(what(j))-100) = 1
            ELSE IF ( (ABS(what(j)).GE.200) .AND. (ABS(what(j)).LE.250)
     &                ) THEN
               IHIsxs(INT(ABS(what(j)))-200) = 1
               IF ( what(j).LT.ZERO ) IXStbl = 1
            END IF
         END DO
         GOTO 100
      ELSE IF ( icw.EQ.48 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = XS-TABLE                    *
C                                                                   *
C    output of cross section table for requested interaction        *
C              - particle production deactivated ! -                *
C                                                                   *
C       what (1)      lower energy limit for tabulation             *
C                > 0  Lab. frame                                    *
C                < 0  nucleon-nucleon cms                           *
C       what (2)      upper energy limit for tabulation             *
C                > 0  Lab. frame                                    *
C                < 0  nucleon-nucleon cms                           *
C       what (3) > 0  # of equidistant lin. bins in E               *
C                < 0  # of equidistant log. bins in E               *
C       what (4)      lower limit of particle virtuality (photons)  *
C       what (5)      upper limit of particle virtuality (photons)  *
C       what (6) > 0  # of equidistant lin. bins in Q^2             *
C                < 0  # of equidistant log. bins in Q^2             *
C                                                                   *
C********************************************************************
 
         IF ( what(1).EQ.99999.0D0 ) THEN
            iratio = INT(what(2))
            GOTO 100
         END IF
         cmener = ABS(what(2))
         IF ( .NOT.lxstab ) THEN
 
Cc       CALL BERTTP
Cc       CALL INCINI
 
         END IF
         IF ( (.NOT.lxstab) .OR. (cmener.NE.cmeold) ) THEN
            cmeold = cmener
            IF ( what(2).GT.ZERO )
     &           cmener = SQRT(2.0D0*AAM(1)**2+2.0D0*what(2)*AAM(1))
            Epn = ZERO
            ppn = ZERO
C        WRITE(LOUT,*) 'CMENER = ',CMENER
            CALL DT_LTINI(IJProj,IJTarg,Epn,ppn,cmener,1)
            CALL DT_PHOINI
         END IF
         CALL DT_XSTABL(what,ixsqel,iratio)
         ixsqel = 0
         lxstab = .TRUE.
         GOTO 100
      ELSE IF ( icw.EQ.49 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = GLAUB-PAR                   *
C                                                                   *
C                parameters in Glauber-formalism                    *
C                                                                   *
C    what (1)  # of nucleon configurations sampled in integration   *
C              over nuclear desity                default: 1000     *
C    what (2)  # of bins for integration over impact-parameter and  *
C              for profile-function calculation   default: 49       *
C    what (3)  = 1 calculation of tot., el. and qel. cross sections *
C                                                 default: 0        *
C    what (4)  = 1   read pre-calculated impact-parameter distrib.  *
C                    from "sdum".glb                                *
C              =-1   dump pre-calculated impact-parameter distrib.  *
C                    into "sdum".glb                                *
C              = 100 read pre-calculated impact-parameter distrib.  *
C                    for variable projectile/target/energy runs     *
C                    from "sdum".glb                                *
C                                                 default: 0        *
C    what (5..6)   no meaning                                       *
C    sdum      if |what (4)| = 1 name of in/output-file (sdum.glb)  *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GT.ZERO ) JSTatb = INT(what(1))
         IF ( what(2).GT.ZERO ) JBInsb = INT(what(2))
         IF ( what(3).EQ.ONE ) LPRod = .FALSE.
         IF ( (ABS(what(4)).EQ.ONE) .OR. (what(4).EQ.100) ) THEN
            IOGlb = INT(what(4))
            CGLb = sdum
         END IF
         GOTO 100
      ELSE IF ( icw.EQ.50 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = GLAUB-INI                   *
C                                                                   *
C             pre-initialization of profile function                *
C                                                                   *
C       what (1)      lower energy limit for initialization         *
C                > 0  Lab. frame                                    *
C                < 0  nucleon-nucleon cms                           *
C       what (2)      upper energy limit for initialization         *
C                > 0  Lab. frame                                    *
C                < 0  nucleon-nucleon cms                           *
C       what (3) > 0  # of equidistant lin. bins in E               *
C                < 0  # of equidistant log. bins in E               *
C       what (4)      maximum projectile mass number for which the  *
C                     Glauber data are initialized for each         *
C                     projectile mass number                        *
C                     (if <= mass given with the PROJPAR-card)      *
C                                              default: 18          *
C       what (5)      steps in mass number starting from what (4)   *
C                     up to mass number defined with PROJPAR-card   *
C                     for which Glauber data are initialized        *
C                                              default: 5           *
C       what (6)      no meaning                                    *
C       sdum          no meaning                                    *
C                                                                   *
C********************************************************************
 
         IOGlb = -100
         CALL DT_GLBINI(what)
         GOTO 100
      ELSE IF ( icw.EQ.51 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = VDM-PAR2                    *
C                                                                   *
C      parameters in gamma-nucleus cross section calculation        *
C                                                                   *
C      what (1) = 0 no suppression of shadowing by direct photon    *
C                   processes                                       *
C               = 1 suppression ..                   default: 1     *
C      what (2) = 0 no suppression of shadowing by anomalous        *
C                   component if photon-F_2                         *
C               = 1 suppression ..                   default: 1     *
C      what (3) = 0 no suppression of shadowing by coherence        *
C                   length of the photon                            *
C               = 1 suppression ..                   default: 1     *
C      what (4) = 1 longitudinal polarized photons are taken into   *
C                   account                                         *
C                   eps*R*Q^2/M^2 = what(4)*Q^2/M^2  default: 0     *
C      what (5..6), sdum    no meaning                              *
C                                                                   *
C********************************************************************
 
         IF ( (what(1).EQ.ZERO) .OR. (what(1).EQ.ONE) ) ISHad(1)
     &        = INT(what(1))
         IF ( (what(2).EQ.ZERO) .OR. (what(2).EQ.ONE) ) ISHad(2)
     &        = INT(what(2))
         IF ( (what(3).EQ.ZERO) .OR. (what(3).EQ.ONE) ) ISHad(3)
     &        = INT(what(3))
         EPSpol = what(4)
         GOTO 100
      ELSE IF ( icw.EQ.52 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  XS-QELPRO                            *
C                                                                   *
C     what (1..6), sdum    no meaning                               *
C                                                                   *
C********************************************************************
 
         ixsqel = ABS(what(1))
         GOTO 100
      ELSE IF ( icw.EQ.53 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  RNDMINIT                             *
C                                                                   *
C           initialization of random number generator               *
C                                                                   *
C     what (1..4)    values for initialization (= 1..168)           *
C     what (5..6), sdum    no meaning                               *
C                                                                   *
C********************************************************************
 
         IF ( (what(1).LT.1.0D0) .OR. (what(1).GT.168.0D0) ) THEN
            na1 = 22
         ELSE
            na1 = what(1)
         END IF
         IF ( (what(2).LT.1.0D0) .OR. (what(2).GT.168.0D0) ) THEN
            na2 = 54
         ELSE
            na2 = what(2)
         END IF
         IF ( (what(3).LT.1.0D0) .OR. (what(3).GT.168.0D0) ) THEN
            na3 = 76
         ELSE
            na3 = what(3)
         END IF
         IF ( (what(4).LT.1.0D0) .OR. (what(4).GT.168.0D0) ) THEN
            na4 = 92
         ELSE
            na4 = what(4)
         END IF
         CALL DT_RNDMST(na1,na2,na3,na4)
         GOTO 100
      ELSE IF ( icw.EQ.54 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = LEPTO-CUT                   *
C                                                                   *
C          set parameter CUT in LEPTO-common /LEPTOU/               *
C                                                                   *
C       what (1) =  index in CUT-array                              *
C       what (2) =  new value of CUT( int(what(1)) )                *
C       what (3), what(4) and what (5), what(6) further             *
C                   parameter in the same way as what (1) and       *
C                   what (2)                                        *
C                        default: default-LEPTO parameters          *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GT.ZERO ) CUT(INT(what(1))) = what(2)
         IF ( what(3).GT.ZERO ) CUT(INT(what(3))) = what(4)
         IF ( what(5).GT.ZERO ) CUT(INT(what(5))) = what(6)
         GOTO 100
      ELSE IF ( icw.EQ.55 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = LEPTO-LST                   *
C                                                                   *
C          set parameter LST in LEPTO-common /LEPTOU/               *
C                                                                   *
C       what (1) =  index in LST-array                              *
C       what (2) =  new value of LST( int(what(1)) )                *
C       what (3), what(4) and what (5), what(6) further             *
C                   parameter in the same way as what (1) and       *
C                   what (2)                                        *
C                        default: default-LEPTO parameters          *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GT.ZERO ) LST(INT(what(1))) = INT(what(2))
         IF ( what(3).GT.ZERO ) LST(INT(what(3))) = INT(what(4))
         IF ( what(5).GT.ZERO ) LST(INT(what(5))) = INT(what(6))
         GOTO 100
      ELSE IF ( icw.EQ.56 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = LEPTO-PARL                  *
C                                                                   *
C          set parameter PARL in LEPTO-common /LEPTOU/              *
C                                                                   *
C       what (1) =  index in PARL-array                             *
C       what (2) =  new value of PARL( int(what(1)) )               *
C       what (3), what(4) and what (5), what(6) further             *
C                   parameter in the same way as what (1) and       *
C                   what (2)                                        *
C                        default: default-LEPTO parameters          *
C                                                                   *
C********************************************************************
 
         IF ( what(1).GT.ZERO ) PARl(INT(what(1))) = what(2)
         IF ( what(3).GT.ZERO ) PARl(INT(what(3))) = what(4)
         IF ( what(5).GT.ZERO ) PARl(INT(what(5))) = what(6)
         GOTO 100
      ELSE IF ( icw.EQ.57 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd = START                       *
C                                                                   *
C       what (1) =   number of events                default: 100.  *
C       what (2) = 0 Glauber initialization follows                 *
C                = 1 Glauber initialization supressed, fitted       *
C                    results are used instead                       *
C                    (this does not apply if emulsion-treatment     *
C                     is requested)                                 *
C                = 2 Glauber initialization is written to           *
C                    output-file shmakov.out                        *
C                = 3 Glauber initialization is read from input-file *
C                    shmakov.out                     default: 0     *
C       what (3..6)  no meaning                                     *
C       what (3..6)  no meaning                                     *
C                                                                   *
C********************************************************************
 
 
C check for cross-section table output only
         IF ( lxstab ) STOP
 
         Ncases = INT(what(1))
         IF ( Ncases.LE.0 ) Ncases = 100
         Iglau = INT(what(2))
         IF ( (Iglau.NE.1) .AND. (Iglau.NE.2) .AND. (Iglau.NE.3) )
     &        Iglau = 0
 
         Npmass = IP
         Npchar = IPZ
         Ntmass = IT
         Ntchar = ITZ
         Idp = IJProj
         idt = IJTarg
         IF ( Idp.LE.0 ) Idp = 1
C muon neutrinos: temporary (missing index)
C (new patch in projpar: therefore the following this is probably not
C  necessary anymore..)
C     IF (IDP.EQ.26) IDP = 5
C     IF (IDP.EQ.27) IDP = 6
 
C redefine collision energy
         IF ( leinp ) THEN
            IF ( ABS(VARehi).GT.ZERO ) THEN
               pdum = ZERO
               IF ( VARelo.LT.EHAdlo ) VARelo = EHAdlo
               CALL DT_LTINI(Idp,idt,VARelo,pdum,VARclo,1)
               pdum = ZERO
               CALL DT_LTINI(Idp,idt,VARehi,pdum,VARchi,1)
            END IF
            CALL DT_LTINI(Idp,idt,Epn,ppn,cmener,1)
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99070)
99070       FORMAT (1X,'INIT:   collision energy not defined!',/,1X,
     &              '              -program stopped-      ')
            STOP
         END IF
 
C switch off evaporation (even if requested) if central coll. requ.
         IF ( (ICEntr.EQ.-1) .OR. (ICEntr.GT.0) .OR. (ICEntr.EQ.-100)
     &        .OR. (XSFrac.LT.0.5D0) ) THEN
            IF ( LEVprt ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99080)
99080          FORMAT (1X,/,
     &                 'Warning!  Evaporation request rejected since',
     &                 ' central collisions forced.')
Cc          LEVPRT = .FALSE.
Cc          LDEEXG = .FALSE.
Cc          LHEAVY = .FALSE.
            END IF
         END IF
 
C initialization of evaporation-module
 
C  initialize evaporation if the code is not used as Fluka event generator
         IF ( ITRspt.NE.1 ) THEN
Cc       CALL BERTTP
Cc       CALL INCINI
         END IF
Cc    IF (LEVPRT) LHEAVY = .TRUE.
 
C save the default JETSET-parameter
         CALL DT_JSPARA(0)
 
C force use of phojet for g-A
         IF ( (Idp.EQ.7) .AND. (MCGene.NE.3) ) MCGene = 2
C initialization of nucleon-nucleon event generator
C initialization of LEPTO event generator
         IF ( MCGene.EQ.2 ) CALL DT_PHOINI
 
 
         IF ( MCGene.EQ.3 )
     &         STOP ' This version does not contain LEPTO !'
 
C initialization of quasi-elastic neutrino scattering
         IF ( MCGene.EQ.4 ) THEN
            IF ( IJProj.EQ.5 ) THEN
               NEUtyp = 1
            ELSE IF ( IJProj.EQ.6 ) THEN
               NEUtyp = 2
            ELSE IF ( IJProj.EQ.135 ) THEN
               NEUtyp = 3
            ELSE IF ( IJProj.EQ.136 ) THEN
               NEUtyp = 4
            ELSE IF ( IJProj.EQ.133 ) THEN
               NEUtyp = 5
            ELSE IF ( IJProj.EQ.134 ) THEN
               NEUtyp = 6
            END IF
         END IF
 
C normalize fractions of emulsion components
         IF ( NCOmpo.GT.0 ) THEN
            sumfra = ZERO
            DO i = 1 , NCOmpo
               sumfra = sumfra + EMUfra(i)
            END DO
            IF ( sumfra.GT.ZERO ) THEN
               DO i = 1 , NCOmpo
                  EMUfra(i) = EMUfra(i)/sumfra
               END DO
            END IF
         END IF
 
C disallow Cronin's multiple scattering for nucleus-nucleus interactions
         IF ( (IP.GT.1) .AND. (MKCron.GT.0) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99090)
99090       FORMAT (/,1X,'INIT:  multiple scattering disallowed',/)
            MKCron = 0
         END IF
#ifndef FOR_FLUKA
C initialization of Glauber-formalism (moved to xAEVT, sr 26.3.96)
         IF (lext) THEN
            IF ( NCOmpo.LE.0 ) THEN
               CALL DT_SHMAKI(IP,IPZ,IT,ITZ,Idp,ppn,Iglau)
            ELSE
               DO i = 1 , NCOmpo
                  CALL DT_SHMAKI(IP,IPZ,IEMuma(i),IEMuch(i),Idp,ppn,0)
               END DO
            END IF
         ENDIF
#endif
C pre-tabulation of elastic cross-sections
C* anfe: Obsolete. Triggers multiparticle initialization
C     CALL DT_SIGTBL(JDUM,JDUM,DUM,DUM,-1)
 
         CALL DT_XTIME
 
         RETURN
      ELSE IF ( icw.EQ.58 ) THEN
         GOTO 500
      ELSE IF ( icw.EQ.59 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd =  FUSION                     *
C                                                                   *
C             WHAT(1)=1 FUSION  default: no FUSION                  *
C                                                                   *
C       what (2..6)  no meaning                                     *
C                                                                   *
C********************************************************************
         IFUsion = 0
         IFUsion = ABS(what(1))
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) ' DT_INIT : IFUSION = ' , 
     &        IFUsion
 
C********************************************************************
C                                                                   *
C               control card:  codewd =  FLOW                       *
C                                                                   *
C                                                                   *
C                                                                   *
C       what (1..6)  no meaning                                     *
C                                                                   *
C********************************************************************
 
C********************************************************************
C                                                                   *
C               control card:  codewd =  COLLSCA                    *
C                                                                   *
C                                                                   *
C                                                                   *
C       what (1..6)  no meaning                                     *
C                                                                   *
C********************************************************************
 
C********************************************************************
C                                                                   *
C               control card:  codewd = CHTWOMES                    *
C                                                                   *
C                                                                   *
C                                                                   *
C       what (1..6)  no meaning                                     *
C                                                                   *
C********************************************************************
         GOTO 100
      ELSE IF ( icw.EQ.60 ) THEN
         GOTO 100
      ELSE IF ( icw.EQ.61 ) THEN
         GOTO 100
      ELSE IF ( icw.EQ.63 .OR. icw.EQ.64 ) THEN
 
C********************************************************************
C                                                                   *
C               control card:  codewd =                             *
C                                                                   *
C                                                                   *
C                                                                   *
C       what (1..6)  no meaning                                     *
C                                                                   *
C********************************************************************
         STOP
      ELSE
         GOTO 100
C
C------------------------------------------------------------
 
      END IF
 
C********************************************************************
C                                                                   *
C               control card:  codewd = STOP                        *
C                                                                   *
C               stop of the event generation                        *
C                                                                   *
C       what (1..6)  no meaning                                     *
C                                                                   *
C********************************************************************
 
 
 400  IF ( LPRi.GT.4 ) WRITE (LOUt,99100)
99100 FORMAT (1X,'---> unexpected end of input !')
 
 500  STOP
99110 FORMAT (A10,6G10.3,A8)
      END SUBROUTINE
