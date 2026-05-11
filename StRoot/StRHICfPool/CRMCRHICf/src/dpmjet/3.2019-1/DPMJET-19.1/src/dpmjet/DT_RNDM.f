#ifndef FOR_CORSIKA

      DOUBLE PRECISION FUNCTION DT_RNDM(Vdummy)
C***********************************************************************
C                                                                      *
C                 7) Random number generator package                   *
C                                                                      *
C    THIS IS A PACKAGE CONTAINING A RANDOM NUMBER GENERATOR AND        *
C    SERVICE ROUTINES.                                                 *
C    THE ALGORITHM IS FROM                                             *
C      'TOWARD A UNVERSAL RANDOM NUMBER GENERATOR'                     *
C      G.MARSAGLIA, A.ZAMAN ;  FSU-SCRI-87-50                          *
C    IMPLEMENTATION BY K. HAHN  DEC. 88,                               *
C    THIS GENERATOR SHOULD NOT DEPEND ON THE HARD WARE ( IF A REAL HAS *
C    AT LEAST 24 SIGNIFICANT BITS IN INTERNAL REPRESENTATION ),        *
C    THE PERIOD IS ABOUT 2**144,                                       *
C    TIME FOR ONE CALL AT IBM-XT IS ABOUT 0.7 MILLISECONDS,            *
C    THE PACKAGE CONTAINS                                              *
C      FUNCTION DT_RNDM(I)                  : GENERATOR                *
C      SUBROUTINE DT_RNDMST(NA1,NA2,NA3,NB4): INITIALIZATION           *
C      SUBROUTINE DT_RNDMIN(U,C,CD,CM,I,J)  : PUT SEED TO GENERATOR    *
C      SUBROUTINE DT_RNDMOU(U,C,CD,CM,I,J)  : TAKE SEED FROM GENERATOR *
C      SUBROUTINE DT_RNDMTE(IO)             : TEST OF GENERATOR        *
C---                                                                   *
C    FUNCTION DT_RNDM(I)                                               *
C       GIVES UNIFORMLY DISTRIBUTED RANDOM NUMBERS  IN (0..1)          *
C       I  - DUMMY VARIABLE, NOT USED                                  *
C    SUBROUTINE DT_RNDMST(NA1,NA2,NA3,NB1)                             *
C       INITIALIZES THE GENERATOR, MUST BE CALLED BEFORE USING DT_RNDM *
C       NA1,NA2,NA3,NB1  - VALUES FOR INITIALIZING THE GENERATOR       *
C                          NA? MUST BE IN 1..178 AND NOT ALL 1         *
C                          12,34,56  ARE THE STANDARD VALUES           *
C                          NB1 MUST BE IN 1..168                       *
C                          78  IS THE STANDARD VALUE                   *
C    SUBROUTINE DT_RNDMIN(U,C,CD,CM,I,J)                               *
C       PUTS SEED TO GENERATOR ( BRINGS GENERATOR IN THE SAME STATUS   *
C       AS AFTER THE LAST DT_RNDMOU CALL )                             *
C       U(97),C,CD,CM,I,J  - SEED VALUES AS TAKEN FROM DT_RNDMOU       *
C    SUBROUTINE DT_RNDMOU(U,C,CD,CM,I,J)                               *
C       TAKES SEED FROM GENERATOR                                      *
C       U(97),C,CD,CM,I,J  - SEED VALUES                               *
C    SUBROUTINE DT_RNDMTE(IO)                                          *
C       TEST OF THE GENERATOR                                          *
C       IO     - DEFINES OUTPUT                                        *
C                  = 0  OUTPUT ONLY IF AN ERROR IS DETECTED            *
C                  = 1  OUTPUT INDEPENDEND ON AN ERROR                 *
C       DT_RNDMTE USES DT_RNDMIN AND DT_RNDMOU TO BRING GENERATOR TO   *
C       SAME STATUS                                                    *
C       AS BEFORE CALL OF DT_RNDMTE                                    *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION C , CD , CM , FLRNDM , U , Vdummy
      INTEGER I , J
      SAVE 
 
C random number generator
#ifndef FOR_FLUKA
      COMMON /DTRAND/ U(97) , C , CD , CM , I , J
#endif
C counter of calls to random number generator
C uncomment if needed
C     COMMON /DTRNCT/ IRNCT0,IRNCT1
C     LOGICAL LFIRST
C     DATA LFIRST /.TRUE./
 
C counter of calls to random number generator
C uncomment if needed
C     IF (LFIRST) THEN
C        IRNCT0 = 0
C        IRNCT1 = 0
C        LFIRST = .FALSE.
C     ENDIF
#ifdef FOR_FLUKA
      DT_RNDM = FLRNDM(Vdummy)
#else
 100  DT_RNDM = U(I) - U(J)
      IF ( DT_RNDM.LT.0.0D0 ) DT_RNDM = DT_RNDM + 1.0D0
      U(I) = DT_RNDM
      I = I - 1
      IF ( I.EQ.0 ) I = 97
      J = J - 1
      IF ( J.EQ.0 ) J = 97
      C = C - CD
      IF ( C.LT.0.0D0 ) C = C + CM
      DT_RNDM = DT_RNDM - C
      IF ( DT_RNDM.LT.0.0D0 ) DT_RNDM = DT_RNDM + 1.0D0
 
      IF ( (DT_RNDM.EQ.0.D0) .OR. (DT_RNDM.EQ.1.D0) ) GOTO 100
#endif
C counter of calls to random number generator
C uncomment if needed
C     IRNCT0 = IRNCT0+1
 
      END FUNCTION
#endif
