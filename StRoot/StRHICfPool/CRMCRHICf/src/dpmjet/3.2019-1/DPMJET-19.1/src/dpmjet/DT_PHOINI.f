
      SUBROUTINE DT_PHOINI
 
C***********************************************************************
C Initialization PHOJET-event generator for nucleon-nucleon interact.  *
C This version dated 16.11.95 is written by S. Roesler                 *
C Last change: s.r. 21.01.01                                           *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amp2 , ecmini , epf , etf , ONE , pp , ppf , 
     &                 ppftmp , pt , ptf , ptftmp , q2 , scpf , sigmax , 
     &                 TINY10 , ZERO
      INTEGER i , IDT_IPDGHA , ijp , ijt , irej1 , isav , k
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ZERO=0.0D0,ONE=1.0D0)
 
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
 
C*anfe  Obsolete cascade modifications...
C* parameters for cascade calculations:
C* maximum mumber of PDF's which can be defined in phojet (limited
C* by the dimension of ipdfs in pho_setpdf)
C      PARAMETER (MAXPDF = 20)
C* PDF parametrization and number of set for the first 30 hadrons in
C* the bamjet-code list
C*   negative numbers mean that the PDF is set in phojet,
C*   zero stands for "not a hadron"
C      DIMENSION IPARPD(30),ISETPD(30)
C* PDF parametrization
C      DATA IPARPD /
C     &  -5,-5, 0, 0, 0, 0,-5,-5,-5, 0, 0, 5,-5,-5, 5, 5, 5, 5, 5, 5,
C     &   5, 5,-5, 5, 5, 0, 0, 0, 0, 0/
C* number of set
C      DATA ISETPD /
C     &  -6,-6, 0, 0, 0, 0,-3,-6,-6, 0, 0, 2,-2,-2, 2, 2, 6, 6, 2, 6,
C     &   6, 6,-2, 2, 2, 0, 0, 0, 0, 0/
 
C*PHOJET105a
C     COMMON /GLOCMS/ XECM,XPCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C     PARAMETER ( MAXPRO = 16 )
C     PARAMETER ( MAXTAB = 20 )
C     COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
C    &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
C     CHARACTER*8 MDLNA
C     COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(100)
C     COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
C*PHOJET110
C  current beam selection
      INCLUDE 'inc/pobeam'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general process information
      INCLUDE 'inc/poprcs'
C*
      DIMENSION pp(4) , pt(4)
 
      LOGICAL lstart
      DATA lstart/.TRUE./
 
      ijp = IJProj
      ijt = IJTarg
      q2 = VIRt
C lepton-projectiles: initialize real photon instead
      IF ( (ijp.EQ.3) .OR. (ijp.EQ.4) .OR. (ijp.EQ.10) .OR. (ijp.EQ.11)
     &     ) THEN
         ijp = 7
         q2 = ZERO
      END IF
 
 
C switch Reggeon off
C     IPAMDL(3)= 0
      irej1 = LPRi
      IF ( LPHoin ) CALL PHO_INIT(-1,LOUt,irej1)
 
      IF ( IP.EQ.1 ) THEN
         CALL PHO_SETPAR(1,IDT_IPDGHA(ijp),0,ZERO)
      ELSE
         CALL PHO_SETPAR(1,2212,0,ZERO)
      END IF
      IF ( IT.EQ.1 ) THEN
         CALL PHO_SETPAR(2,IDT_IPDGHA(ijt),0,ZERO)
      ELSE
         CALL PHO_SETPAR(2,2212,0,ZERO)
      END IF
 
      DO k = 1 , 4
         pp(k) = ZERO
         pt(k) = ZERO
      END DO
C get max. possible momenta of incoming particles to be used for PHOJET ini.
      ppf = ZERO
      ptf = ZERO
      scpf = 1.5D0
      IF ( UMO.GE.1.E5 ) scpf = 5.0D0
      IF ( NCOmpo.GT.0 ) THEN
         DO i = 1 , NCOmpo
            IF ( IT.GT.1 ) THEN
               CALL DT_NCLPOT(IEMuch(i),IEMuma(i),ITZ,IT,ZERO,ZERO,0)
            ELSE
               CALL DT_NCLPOT(IPZ,IP,IEMuch(i),IEMuma(i),ZERO,ZERO,0)
            END IF
            ppftmp = MAX(PFErmp(1),PFErmn(1))
            ptftmp = MAX(PFErmp(2),PFErmn(2))
            IF ( ppftmp.GT.ppf ) ppf = ppftmp
            IF ( ptftmp.GT.ptf ) ptf = ptftmp
         END DO
      ELSE
         CALL DT_NCLPOT(IPZ,IP,ITZ,IT,ZERO,ZERO,0)
         ppf = MAX(PFErmp(1),PFErmn(1))
         ptf = MAX(PFErmp(2),PFErmn(2))
      END IF
      ptf = -ptf
      ppf = scpf*ppf
      ptf = scpf*ptf
      IF ( ijp.EQ.7 ) THEN
         amp2 = SIGN(PMAss(1)**2,PMAss(1))
         pp(3) = PPCm
         pp(4) = SQRT(amp2+pp(3)**2)
      ELSE
         epf = SQRT(ppf**2+PMAss(1)**2)
         CALL DT_LTNUC(ppf,epf,pp(3),pp(4),2)
      END IF
      etf = SQRT(ptf**2+PMAss(2)**2)
      CALL DT_LTNUC(ptf,etf,pt(3),pt(4),3)
      ecmini = SQRT((pp(4)+pt(4))**2-(pp(1)+pt(1))**2-(pp(2)+pt(2))
     &         **2-(pp(3)+pt(3))**2)
      IF ( lstart ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) IP , IPZ , scpf , ppf , pp
99010    FORMAT (
     &         ' DT_PHOINI:    PHOJET initialized for projectile A,Z = '
     &         ,I3,',',I2,/,F4.1,'xp_F(max) = ',E10.3,'  p(max) = ',
     &         4E10.3)
         IF ( NCOmpo.GT.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99020) scpf , ptf , pt
99020       FORMAT (
     &         ' DT_PHOINI:    PHOJET initialized for target emulsion  '
     &         ,/,F4.1,'xp_F(max) = ',E10.3,'  p(max) = ',4E10.3)
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) IT , ITZ , scpf , ptf , 
     &           pt
99030       FORMAT (
     &         ' DT_PHOINI:    PHOJET initialized for target     A,Z = '
     &         ,I3,',',I2,/,F4.1,'xp_F(max) = ',E10.3,'  p(max) = ',
     &         4E10.3)
         END IF
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99040) ecmini
99040    FORMAT (' E_cm = ',E10.3)
 
         IF ( LPRi.GT.4 .AND. ijp.EQ.8 ) WRITE (LOUt,99050)
99050    FORMAT (
     &         ' DT_PHOINI: warning! proton parameters used for neutron'
     &         ,' projectile')
         lstart = .FALSE.
      END IF
C switch off new diffractive cross sections at low energies for nuclei
C (temporary solution)
      IF ( (ISWmdl(30).NE.0) .AND. ((IP.GT.1) .OR. (IT.GT.1)) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &         ' DT_PHOINI: model-switch 30 for nuclei re-set !'
         CALL PHO_SETMDL(30,0,1)
      END IF
C
C     IF (IJP.EQ.7) THEN
C        AMP2  = SIGN(PMASS(1)**2,PMASS(1))
C        PP(3) = PPCM
C        PP(4) = SQRT(AMP2+PP(3)**2)
C     ELSE
C        PFERMX = ZERO
C        IF (IP.GT.1) PFERMX = 0.5D0
C        EFERMX = SQRT(PFERMX**2+PMASS(1)**2)
C        CALL DT_LTNUC(PFERMX,EFERMX,PP(3),PP(4),2)
C     ENDIF
C     PFERMX = ZERO
C     IF ((IT.GT.1).OR.(NCOMPO.GT.0)) PFERMX = -0.5D0
C     EFERMX = SQRT(PFERMX**2+PMASS(2)**2)
C     CALL DT_LTNUC(PFERMX,EFERMX,PT(3),PT(4),3)
C*sr 26.10.96
      isav = IPAmdl(13)
      IF ( (ISHad(2).EQ.1) .AND. 
     &     ((IJProj.EQ.7) .OR. (IJProj.EQ.3) .OR. (IJProj.EQ.4) .OR. 
     &     (IJProj.EQ.10) .OR. (IJProj.EQ.11)) ) IPAmdl(13) = 1
C*
 
C  Initialize phojet with protons and other hadrons on demand
      CALL PHO_SETPAR(1,2212,0,ZERO)
      CALL PHO_SETPAR(2,2212,0,ZERO)
 
      CALL PHO_EVENT(-1,pp,pt,sigmax,irej1)
 
C*sr 26.10.96
      IPAmdl(13) = isav
C*
C
C patch for cascade calculations:
C define parton distribution functions for other hadrons, i.e. other
C then defined already in phojet
C*anfe remove cascade mod, since the PDFs are defined in PHOJET
C       IF ((IOGLB.EQ.100).AND..FALSE.) THEN
C
C          IF (LPRI.GT.4)
C      .   WRITE(LOUT,1006)
C  1006    FORMAT(/,1X,'PHOINI: additional parton distribution functions',
C      &          ' assiged (ID,IPAR,ISET)',/)
C          NPDF = 0
C          DO 3 I=1,30
C             IF (IPARPD(I).NE.0) THEN
C                WRITE(LOUT,*) 'Cascade path overwrites PDF settings.'
C                NPDF = NPDF+1
C                IF (NPDF.GT.MAXPDF) STOP ' PHOINI: npdf > maxpdf !'
C                IF ((IPARPD(I).GT.0).AND.(ISETPD(I).GT.0)) THEN
C                   IDPDG = IDT_IPDGHA(I)
C                   IPAR  = IPARPD(I)
C                   ISET  = ISETPD(I)
C
C                   IF (LPRI.GT.4)
C      .            WRITE(LOUT,'(13X,A8,3I6)') ANAME(I),IDPDG,IPAR,ISET
C                   CALL PHO_SETPDF(IDPDG,IDUM,IPAR,ISET,0,0,-1)
C                ENDIF
C             ENDIF
C     3    CONTINUE
C       ENDIF
 
C     CALL PHO_PHIST(-1,SIGMAX)
 
      IF ( irej1.NE.0 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99060)
99060    FORMAT (1X,'PHOINI:   PHOJET event-initialization failed!')
         STOP
      END IF
 
      END SUBROUTINE
