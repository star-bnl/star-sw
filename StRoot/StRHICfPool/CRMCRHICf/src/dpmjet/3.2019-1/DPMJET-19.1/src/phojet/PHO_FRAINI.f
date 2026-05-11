
      SUBROUTINE PHO_FRAINI(Idefau)
C***********************************************************************
C
C     initialization of fragmentation packages
C      (currently LUND JETSET)
C
C     initialization for JETSET call in DTUNUC 1.04 (J.R. 6/93)
C                      changed to work in PHOJET   (R.E. 1/94)
C
C     input:  IDEFAU    0  no hadronization at all
C                       1  do not touch any parameter of JETSET
C                       2  default parameters kept, decay length 10mm to
C                          define stable particles
C                       3  load tuned parameters for JETSET 7.3
C             neg. value:  prevent strange/charm hadrons from decaying
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION def19 , def2 , def21 , def41 , def42 , EPS
      INTEGER idef12 , idefab , Idefau , kc
      SAVE 
 
      PARAMETER (EPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      INCLUDE 'inc/pyjets'
 
      INCLUDE 'inc/pydat1'
 
      INCLUDE 'inc/pydat2'
 
      INCLUDE 'inc/pydat3'
 
      INTEGER PYCOMP
      EXTERNAL PYCOMP
 
      idefab = ABS(Idefau)
 
      IF ( idefab.EQ.0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &         'PHO_FRAINI: hadronization switched off'
         RETURN
      END IF
C  defaults
      def2 = PARj(2)
      idef12 = MSTj(12)
      def19 = PARj(19)
      def41 = PARj(41)
      def42 = PARj(42)
      def21 = PARj(21)
 
C  declare stable particles
      IF ( idefab.GE.2 ) MSTj(22) = 2
 
C  load optimized parameters
C*anfe try PYTHIA default
      IF ( idefab.GE.3 .AND. .FALSE. ) THEN
 
C       PARJ(19)=0.19
C  Lund a-parameter
C  (default=0.3)
         PARj(41) = 0.3D0
C  Lund b-parameter
C  (default=1.0)
         PARj(42) = 1.0D0
C  Lund sigma parameter in pt distribution
C  (default=0.36)
C*anfe 26.08.2015 fragmentation parameters
         PARj(1) = 0.09D0
         PARj(2) = 0.22D0
         PARj(3) = 0.9D0
         PARj(5) = 0.1D0
         PARj(7) = 0.95D0
         PARj(21) = 0.42D0
      END IF
C
C  prevent particles decaying
      IF ( Idefau.LT.0 ) THEN
C                 K0S
 
         kc = PYCOMP(310)
 
         MDCy(kc,1) = 0
C                 PI0
 
         kc = PYCOMP(111)
 
         MDCy(kc,1) = 0
C                 LAMBDA
 
         kc = PYCOMP(3122)
 
         MDCy(kc,1) = 0
C                 ALAMBDA
 
         kc = PYCOMP(-3122)
 
         MDCy(kc,1) = 0
C                 SIG+
 
         kc = PYCOMP(3222)
 
         MDCy(kc,1) = 0
C                 ASIG+
 
         kc = PYCOMP(-3222)
 
         MDCy(kc,1) = 0
C                 SIG-
 
         kc = PYCOMP(3112)
 
         MDCy(kc,1) = 0
C                 ASIG-
 
         kc = PYCOMP(-3112)
 
         MDCy(kc,1) = 0
C                 SIG0
 
         kc = PYCOMP(3212)
 
         MDCy(kc,1) = 0
C                 ASIG0
 
         kc = PYCOMP(-3212)
 
         MDCy(kc,1) = 0
C                 TET0
 
         kc = PYCOMP(3322)
 
         MDCy(kc,1) = 0
C                 ATET0
 
         kc = PYCOMP(-3322)
 
         MDCy(kc,1) = 0
C                 TET-
 
         kc = PYCOMP(3312)
 
         MDCy(kc,1) = 0
C                 ATET-
 
         kc = PYCOMP(-3312)
 
         MDCy(kc,1) = 0
C                 OMEGA-
 
         kc = PYCOMP(3334)
 
         MDCy(kc,1) = 0
C                 AOMEGA-
 
         kc = PYCOMP(-3334)
 
         MDCy(kc,1) = 0
C                 D+
 
         kc = PYCOMP(411)
 
         MDCy(kc,1) = 0
C                 D-
 
         kc = PYCOMP(-411)
 
         MDCy(kc,1) = 0
C                 D0
 
         kc = PYCOMP(421)
 
         MDCy(kc,1) = 0
C                 A-D0
 
         kc = PYCOMP(-421)
 
         MDCy(kc,1) = 0
C                 DS+
 
         kc = PYCOMP(431)
 
         MDCy(kc,1) = 0
C                 A-DS+
 
         kc = PYCOMP(-431)
 
         MDCy(kc,1) = 0
C                ETAC
 
         kc = PYCOMP(441)
 
         MDCy(kc,1) = 0
C                LAMBDAC+
 
         kc = PYCOMP(4122)
 
         MDCy(kc,1) = 0
C                A-LAMBDAC+
 
         kc = PYCOMP(-4122)
 
         MDCy(kc,1) = 0
C                SIGMAC++
 
         kc = PYCOMP(4222)
 
         MDCy(kc,1) = 0
C                SIGMAC+
 
         kc = PYCOMP(4212)
 
         MDCy(kc,1) = 0
C                SIGMAC0
 
         kc = PYCOMP(4112)
 
         MDCy(kc,1) = 0
C                A-SIGMAC++
 
         kc = PYCOMP(-4222)
 
         MDCy(kc,1) = 0
C                A-SIGMAC+
 
         kc = PYCOMP(-4212)
 
         MDCy(kc,1) = 0
C                A-SIGMAC0
 
         kc = PYCOMP(-4112)
 
         MDCy(kc,1) = 0
C                KSIC+
 
         kc = PYCOMP(4232)
 
         MDCy(kc,1) = 0
C                KSIC0
 
         kc = PYCOMP(4132)
 
         MDCy(kc,1) = 0
C                A-KSIC+
 
         kc = PYCOMP(-4232)
 
         MDCy(kc,1) = 0
C                A-KSIC0
 
         kc = PYCOMP(-4132)
 
         MDCy(kc,1) = 0
      END IF
 
      IF ( LPRi.GT.4 ) WRITE (LO,99010) Idefau , def2 , PARj(2) , 
     &                        idef12 , MSTj(12) , def19 , PARj(19) , 
     &                        def41 , PARj(41) , def42 , PARj(42) , 
     &                        def21 , PARj(21)
99010 FORMAT (/' PHO_FRAINI: fragmentation initialization ISWMDL(6)',
     &        I3/,' --------------------------------------------------',
     &        /,5X,
     &        'parameter description               default / current',/,
     &        5X,'PARJ( 2) strangeness suppression : ',2F7.3,/,5X,
     &        'MSTJ(12) popcorn                 : ',2I7,/,5X,
     &        'PARJ(19) popcorn                 : ',2F7.3,/,5X,
     &        'PARJ(41) Lund a                  : ',2F7.3,/,5X,
     &        'PARJ(42) Lund b                  : ',2F7.3,/,5X,
     &        'PARJ(21) sigma in pt distribution: ',2F7.3,/)
 
      END SUBROUTINE
