
      SUBROUTINE DT_INITJS(Mode)
 
C***********************************************************************
C Initialize JETSET paramters.                                         *
C           MODE = 0 default settings                                  *
C                = 1 PHOJET settings                                   *
C                = 2 DTUNUC settings                                   *
C This version dated 16.02.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER i , idpdg , idx , idxsta , iunstab , j , kc , kp , 
     &        MCIHAD , mdef12 , Mode , MPDGHA , NUNSTAB
      DOUBLE PRECISION ONE , pdef1 , pdef18 , pdef19 , pdef2 , pdef21 , 
     &                 pdef3 , pdef4 , pdef41 , pdef42 , pdef5 , pdef6 , 
     &                 pdef7 , TINY10 , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ONE=1.0D0,ZERO=0.0D0)
 
      LOGICAL lfirst , lfirdt , lfirph
#ifdef FOR_FLUKA
      INCLUDE '(DIMPAR)'
      INCLUDE '(PART)'
#else
      INCLUDE 'DIMPAR'
      INCLUDE 'PART'
#endif
 
      INCLUDE 'inc/pydat1'
 
      INCLUDE 'inc/pydat2'
 
      INCLUDE 'inc/pydat3'
 
C flags for particle decays
      INCLUDE 'inc/dtfrpa'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C**af a flag to prevent overwriting decay settings 
      DATA OVwtdc /.True./

      INTEGER PYCOMP
 
      DIMENSION idxsta(40)
C          K0s   pi0  lam   alam  sig+  asig+ sig-  asig- tet0  atet0
C          tet- atet-  om-  aom-   D+    D-    D0    aD0   Ds+   aDs+
C          etac lamc+alamc+sigc++ sigc+ sigc0asigc++asigc+asigc0 Ksic+
C         Ksic0 aKsic+aKsic0 sig0 asig0
      DATA idxsta/310 , 111 , 3122 , -3122 , 3222 , -3222 , 3112 , 
     &     -3112 , 3322 , -3322 , 3312 , -3312 , 3334 , -3334 , 411 , 
     &     -411 , 421 , -421 , 431 , -431 , 441 , 4122 , -4122 , 4222 , 
     &     4212 , 4112 , -4222 , -4212 , -4112 , 4232 , 4132 , -4232 , 
     &     -4132 , 3212 , -3212 , 5*0/
 
      PARAMETER (NUNSTAB=11)
      DIMENSION iunstab(NUNSTAB)
      DATA iunstab/4132 , 4232 , 4332 , 4312 , 4322 , 4324 , 4214 , 
     &     4224 , 4314 , 4114 , 10421/
 
      DATA lfirst , lfirdt , lfirph/.TRUE. , .TRUE. , .TRUE./
 
      IF ( lfirst ) THEN
C save default settings
         pdef1 = PARj(1)
         pdef2 = PARj(2)
         pdef3 = PARj(3)
         pdef4 = PARj(4)
         pdef5 = PARj(5)
         pdef6 = PARj(6)
         pdef7 = PARj(7)
         pdef18 = PARj(18)
         pdef19 = PARj(19)
         pdef21 = PARj(21)
         pdef41 = PARj(41)
         pdef42 = PARj(42)
         mdef12 = MSTj(12)
         ! DO I=1,200
         !  WRITE(LOUT,*) 'PARJ(',I,') =',PARJ(I)
         ! END DO
 
C LUJETS / PYJETS array-dimensions
 
         MSTu(4) = 4000
 
C increase maximum number of JETSET-error prints
         MSTu(22) = 50000
         IF (OVwtdc) THEN
C prevent particles from decaying
            DO i = 1 , 35
               IF ( i.LT.34 ) THEN
   
                  kc = PYCOMP(idxsta(i))
   
                  IF ( i.EQ.2 ) THEN
C  pi0 decay
C                 MDCY(KC,1) = 1
                     MDCy(kc,1) = 0
                  ELSE
                     MDCy(kc,1) = 0
                  END IF
               ELSE IF (((i.EQ.34).OR.(i.EQ.35)).AND.(ISIg0.EQ.0))
     &                THEN
   
                  kc = PYCOMP(idxsta(i))
   
                  MDCy(kc,1) = 0
               END IF
            END DO
         END IF
C prevent some charmed baryons which don't have a BAMJET code to be
C set as stable
         DO i = 1 , NUNSTAB
            kc = PYCOMP(iunstab(i))
            MDCy(kc,1) = 1
            kc = PYCOMP(-iunstab(i))
            MDCy(kc,1) = 1
         END DO
C
 
C as Fluka event-generator: allow only paprop particles to be stable
C and let all other particles decay (i.e. those with strong decays)
         IF ( (ITRspt.EQ.1).AND.OVwtdc) THEN
            DO i = 1 , IDMAXP
               IF ( KPToip(i).NE.0 ) THEN
                  idpdg = MPDGHA(i)
                  DO j = 1 , NUNSTAB
                     IF ( ABS(idpdg).EQ.iunstab(j) ) GOTO 20
                  END DO
                  kc = PYCOMP(idpdg)
                  IF ( kc.GT.0 ) THEN
                     IF ( MDCy(kc,1).EQ.1 ) THEN
 
                        IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                        ' DT_INITJS: Decay flag for FLUKA-' , 
     &                       'transport : particle should not ' , 
     &                       'decay : ' , idpdg , '  ' , ANAme(i)
                        MDCy(kc,1) = 0
                     END IF
                  END IF
               END IF
 20         END DO
            DO kc = 1 , 500
               idpdg = KCHg(kc,4)
               kp = MCIHAD(idpdg)
               IF ( kp.GT.0 ) THEN
                  IF ( (MDCy(kc,1).EQ.0) .AND. (KPToip(kp).EQ.0) .AND. 
     &                 (ANAme(kp).NE.'BLANK   ') .AND. 
     &                 (ANAme(kp).NE.'RNDFLV  ') ) THEN
 
                     IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                     ' DT_INITJS: Decay flag for FLUKA-' , 
     &                    'transport: particle should decay ' , ': ' , 
     &                    idpdg , ' ' , ANAme(kp)
                     MDCy(kc,1) = 1
                  END IF
               END IF
            END DO
         END IF
 
C
C popcorn:
C          IF (PDB.LE.ZERO) THEN
C *   no popcorn-mechanism
C             MSTJ(12) = 1
C          ELSE
C             MSTJ(12) = 3
C             PARJ(5)  = PDB
C          ENDIF
C set JETSET-parameter requested by input cards
         IF ( NMStu.GT.0 ) THEN
            DO i = 1 , NMStu
               MSTu(IMStu(i)) = MSTux(i)
            END DO
         END IF
         IF ( NMStj.GT.0 ) THEN
            DO i = 1 , NMStj
               MSTj(IMStj(i)) = MSTjx(i)
            END DO
         END IF
         IF ( NPAru.GT.0 ) THEN
            DO i = 1 , NPAru
               PARu(IPAru(i)) = PARux(i)
            END DO
         END IF
         lfirst = .FALSE.
      END IF
C
C PARJ(1)  suppression of qq-aqaq pair prod. compared to
C          q-aq pair prod.                      (default: 0.1)
C PARJ(2)  strangeness suppression               (default: 0.3)
C PARJ(3)  extra suppression of strange diquarks (default: 0.4)
C PARJ(6)  extra suppression of sas-pair shared by B and
C          aB in BMaB                           (default: 0.5)
C PARJ(7)  extra suppression of strange meson M in BMaB
C          configuration                        (default: 0.5)
C PARJ(18) spin 3/2 baryon suppression           (default: 1.0)
C PARJ(21) width sigma in Gaussian p_x, p_y transverse
C          momentum distrib. for prim. hadrons  (default: 0.35)
C PARJ(42) b-parameter for symmetric Lund-fragmentation
C          function                             (default: 0.9 GeV^-2)
C
 
C*anfe Reset all parameters before changing anything.
      PARj(1) = pdef1
      PARj(2) = pdef2
      PARj(3) = pdef3
      PARj(4) = pdef4
      PARj(5) = pdef5
      PARj(6) = pdef6
      PARj(7) = pdef7
      PARj(18) = pdef18
      PARj(19) = pdef19
      PARj(21) = pdef21
      PARj(41) = pdef41
      PARj(42) = pdef42
      MSTj(12) = mdef12
 
C PHOJET settings
C*anfe try PYTHIA default
      IF ( Mode.EQ.1 .OR. Mode.EQ.2 ) THEN
 
C*anfe 26.08.2015 fragmentation parameters
C* Fragmentation parameters for 'new' Popcorn in PYTHIA
         ! PARJ(1) = 0.5D0
         ! ! PARJ(2) = 0.2D0
         ! ! PARJ(3) = 0.9D0
         ! ! PARJ(4) = 0.D0
         ! ! PARJ(5) = 0.2D0
         ! ! PARJ(6) = 0.9D0
         ! ! PARJ(7) = 1.D0
         ! ! PARJ(11) = 0.5D0
         ! PARJ(8) = 0.1D0
         ! PARJ(9) = 0.5D0
         ! PARJ(10) = 1.5D0
         ! PARJ(18)= 0.19D0
         ! ! PARJ(19)= 0.05D0
         ! PARJ(21)= 0.43D0
         ! ! PARJ(41)=0.3D0
         ! ! PARJ(42)=1.D0
         ! PARJ(45)=2.D0
         ! MSTJ(12)=5
C* Parameters for traditional popcorn model
         ! PARJ(1) = 0.25D0
         ! PARJ(2) = 0.2D0
         ! PARJ(3) = 0.9D0
         ! ! PARJ(4) = 0.D0
         ! PARJ(5) = 0.2D0
         ! ! PARJ(6) = 0.9D0
         ! PARJ(7) = 1.D0
         ! PARJ(11) = 0.5D0
         ! ! PARJ(19)= 0.05D0
         ! PARJ(21)= 0.42D0
         ! PARJ(41)=0.2D0
         ! PARJ(42)=0.75D0
         MSTj(12) = 3
 
         PARj(1) = 0.08D0
         PARj(2) = 0.16D0
         PARj(3) = 0.9D0
         PARj(5) = 0.2D0
         PARj(7) = 0.85D0
         PARj(18) = 0.1D0
 
         PARj(21) = 0.42D0
         PARj(41) = 0.3D0
         PARj(42) = 0.85D0
 
         IF ( NPArj.GT.0 ) THEN
            DO i = 1 , NPArj
               IF ( IPArj(i).GT.0 ) PARj(IPArj(i)) = PARjx(i)
            END DO
         END IF
         IF ( lfirph ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &            'DT_INITJS: JETSET-parameter for PHOJET'
            CALL DT_JSPARA(0)
            lfirph = .FALSE.
         END IF
C DTUNUC settings
      ELSE IF ( Mode.EQ.2 ) THEN
         IF ( IFRag(2).EQ.1 ) THEN
C*sr 28.04.99 parameter tuning
            PARj(1) = 0.11D0
            PARj(2) = 0.36D0
            PARj(3) = 0.8D0
            PARj(19) = 0.2D0
            PARj(21) = 0.3D0
            PARj(41) = 0.3D0
            PARj(42) = 0.58D0
            IF ( NPArj.GT.0 ) THEN
               DO i = 1 , NPArj
                  IF ( IPArj(i).LT.0 ) THEN
                     idx = ABS(IPArj(i))
                     PARj(idx) = PARjx(i)
                  END IF
               END DO
            END IF
            IF ( lfirdt ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &               'DT_INITJS: JETSET-parameter for DTUNUC'
               CALL DT_JSPARA(0)
               lfirdt = .FALSE.
            END IF
         ELSE IF ( IFRag(2).EQ.2 ) THEN
            PARj(1) = 0.11D0
            PARj(2) = 0.27D0
            PARj(3) = 0.3D0
            PARj(6) = 0.35D0
            PARj(7) = 0.45D0
            PARj(18) = 0.66D0
            PARj(21) = 0.60D0
            PARj(42) = 1.3D0
         ELSE
            PARj(1) = pdef1
            PARj(2) = pdef2
            PARj(3) = pdef3
            PARj(6) = pdef6
            PARj(7) = pdef7
            PARj(18) = pdef18
            PARj(21) = pdef21
            PARj(42) = pdef42
         END IF
      ELSE
         PARj(1) = pdef1
         PARj(2) = pdef2
         PARj(3) = pdef3
         PARj(5) = pdef5
         PARj(6) = pdef6
         PARj(7) = pdef7
         PARj(18) = pdef18
         PARj(19) = pdef19
         PARj(21) = pdef21
         PARj(41) = pdef41
         PARj(42) = pdef42
         MSTj(12) = mdef12
      END IF
C     PARJ(18)=1.D0
C     WRITE(6,*)' INITJS:',' MODE = ',MODE
C     DO 2222 I=1,50
C       WRITE(6,*)' PARJ(',I,') = ',PARJ(I)
C2222 CONTINUE
C     WRITE(6,*)' MSTJ(12) = ',MSTJ(12)
      END SUBROUTINE
