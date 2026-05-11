
      SUBROUTINE DT_DTCHOI(T,P,Pp,E,Ee,I,Ii,N,Am1,Am2)
 
      IMPLICIT NONE
      DOUBLE PRECISION Am1 , Am2 , am3 , ama , amb , an , bm , DT_RNDM , 
     &                 E , Ee , etma , P , Pp , r , T , tma , tmax , 
     &                 tmi , tmin , vb
      INTEGER I , ib , Ii , iii , k , N
      SAVE 
 
C     ****************************
C     TCHOIC CALCULATES A RANDOM VALUE
C     FOR THE FOUR-MOMENTUM-TRANSFER T
C     ****************************
 
C particle properties (BAMJET index convention),
C (dublicate of DTPART for HADRIN)
      INCLUDE 'inc/hnablt'
C slope parameters for HADRIN interactions
      INCLUDE 'inc/hnslop'
 
      ama = Am1
      amb = Am2
      IF ( I.GT.30 .AND. Ii.GT.30 ) THEN
         iii = Ii
         am3 = Am2
         IF ( ama.GT.amb ) THEN
            iii = I
            am3 = Am1
         END IF
      ELSE
         iii = Ii
         am3 = Am2
         IF ( I.GT.30 ) THEN
            iii = I
            am3 = Am1
         END IF
      END IF
      ib = IBArh(iii)
      ama = am3
      k = INT((ama-0.75D0)/0.05D0)
      IF ( k.LT.2 ) k = 1
      IF ( k.GE.26 ) k = 25
      IF ( ib.NE.0 ) THEN
         bm = BBB(k)
      ELSE
         bm = BBM(k)
      END IF
C     NORMALIZATION
      tmin = -2.0D0*(E*Ee-P*Pp) + AMH(N)**2 + Am1**2
      tmax = -2.0D0*(E*Ee+P*Pp) + AMH(N)**2 + Am1**2
      vb = DT_RNDM(tmin)
C*sr test
C     IF (VB.LT.0.2D0) BM=BM*0.1
C    **0.5
      bm = bm*5.05D0
C*
      tmi = bm*tmin
      tma = bm*tmax
      etma = 0.D0
      IF ( ABS(tma).LE.120.D0 ) etma = EXP(tma)
      an = (1.0D0/bm)*(EXP(tmi)-etma)
      r = DT_RNDM(tmi)
      T = (1.0D0/bm)*LOG(etma+r*an*bm)
      END SUBROUTINE
