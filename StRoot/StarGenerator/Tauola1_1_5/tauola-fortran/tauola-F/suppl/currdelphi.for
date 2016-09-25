      SUBROUTINE CURR(MNUM,PIM1,PIM2,PIM3,PIM4,HADCUR)                  CURR   2
C     ==================================================================CURR   3
C     hadronic current for 4 pi final state                             CURR   4
C     R. Fisher, J. Wess and F. Wagner Z. Phys C3 (1980) 313            CURR   5
C     R. Decker Z. Phys C36 (1987) 487.                                 CURR   6
C     M. Gell-Mann, D. Sharp, W. Wagner Phys. Rev. Lett 8 (1962) 261.   CURR   7
C     ==================================================================CURR   8
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU             PARMAS 2
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1                PARMAS 3
     *                 ,AMK,AMKZ,AMKST,GAMKST                           PARMAS 4
C                                                                       PARMAS 5
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU             PARMAS 6
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1                PARMAS 7
     *                 ,AMK,AMKZ,AMKST,GAMKST                           PARMAS 8
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL                DECPAR 2
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL                DECPAR 3
      REAL  PIM1(4),PIM2(4),PIM3(4),PIM4(4),PAA(4)                      CURR  11
cam   COMPLEX HADCUR(4),FORM1,FORM2,FORM3,FPIKM                         CURR  12
      COMPLEX HADCUR(4),FORM1,FORM2,FORM3,WIGFOR                        CURR  13
      COMPLEX BWIGN                                                     CURR  14
      REAL PA(4),PB(4)                                                  CURR  15
      REAL AA(4,4),PP(4,4)                                              CURR  16
      DATA PI /3.141592653589793238462643/                              CURR  17
      DATA  FPI /93.3E-3/                                               CURR  18
      BWIGN(A,XM,XG)=1.0/CMPLX(A-XM**2,XM*XG)                           CURR  19
C                                                                       CURR  20
C --- masses and constants                                              CURR  21
cam   rho' taken as in Dolinsky et al (PhysLett B174 (1986) 453)        CURR  22
cam   (best fit to Argus data)                                          CURR  23
      G1=12.924                                                         CURR  24
      G2=1475.98                                                        CURR  25
      G =G1*G2                                                          CURR  26
cam   ELPHA=-.1                                                         CURR  27
cam   AMROP=1.7                                                         CURR  28
cam   GAMROP=0.26                                                       CURR  29
CALEPH      ELPHA= .02                                                        CURR  30
CALEPH      AMROP=1.250                                                       CURR  31
CALEPH      GAMROP=0.125                                                      CURR  32
c DELPHI tuning
      ELPHA=-.09                                                        CURR  30
      AMROP=1.450                                                       CURR  31
      GAMROP=0.300                                                      CURR  32
ccccccccccccccccccccccccccc
      AMOM=.782                                                         CURR  33
      GAMOM=0.0085                                                      CURR  34
cam   ARFLAT=1.0                                                        CURR  35
cam   AROMEG=1.0                                                        CURR  36
cALEPH   ARFLAT=1.3                                                        CURR  35
cALEPH   AROMEG=2.0                                                        CURR  36
cDELPHI tuning
      ARFLAT=1.0                                                        CURR  37
      AROMEG=1.6                                                        CURR  38
cccccccccccccccccccc
C                                                                       CURR  39
      FRO=0.266*AMRO**2                                                 CURR  40
      COEF1=2.0*SQRT(3.0)/FPI**2*ARFLAT                                 CURR  41
      COEF2=FRO*G*AROMEG                                                CURR  42
C --- initialization of four vectors                                    CURR  43
      DO 7 K=1,4                                                        CURR  44
      DO 8 L=1,4                                                        CURR  45
 8    AA(K,L)=0.0                                                       CURR  46
      HADCUR(K)=CMPLX(0.0)                                              CURR  47
      PAA(K)=PIM1(K)+PIM2(K)+PIM3(K)+PIM4(K)                            CURR  48
      PP(1,K)=PIM1(K)                                                   CURR  49
      PP(2,K)=PIM2(K)                                                   CURR  50
      PP(3,K)=PIM3(K)                                                   CURR  51
 7    PP(4,K)=PIM4(K)                                                   CURR  52
C                                                                       CURR  53
      IF (MNUM.EQ.1) THEN                                               CURR  54
C ===================================================================   CURR  55
C pi- pi- p0 pi+ case                                            ====   CURR  56
c !!!!! included rho- production !!!!!!!!!!!!!!!
C ===================================================================   CURR  57
       QQ=PAA(4)**2-PAA(3)**2-PAA(2)**2-PAA(1)**2                       CURR  58
C --- loop over thre contribution of the non-omega current              CURR  59
       DO 201 K=1,4                                                     CURR  60
        SK=(PP(K,4)+PIM4(4))**2-(PP(K,3)+PIM4(3))**2                    CURR  61
     $    -(PP(K,2)+PIM4(2))**2-(PP(K,1)+PIM4(1))**2                    CURR  62
          if(k.ne.4) go to 27
        SK=(PP(3,4)+PIM1(4))**2-(PP(3,3)+PIM1(3))**2                    CURR  61
     $    -(PP(3,2)+PIM1(2))**2-(PP(3,1)+PIM1(1))**2                    CURR  62
  27  continue
C -- definition of AA matrix                                            CURR  63
C -- cronecker delta                                                    CURR  64
        DO 202 I=1,4                                                    CURR  65
         DO 203 J=1,4                                                   CURR  66
 203     AA(I,J)=0.0                                                    CURR  67
 202    AA(I,I)=1.0                                                     CURR  68
C ... and the rest ...                                                  CURR  69
        DO 204 L=1,3                                                    CURR  70
            if(k.eq.4) go to 37
         IF (L.NE.K) THEN                                               CURR  71
          DENOM=(PAA(4)-PP(L,4))**2-(PAA(3)-PP(L,3))**2                 CURR  72
     $         -(PAA(2)-PP(L,2))**2-(PAA(1)-PP(L,1))**2                 CURR  73
          DO 205 I=1,4                                                  CURR  74
          DO 205 J=1,4                                                  CURR  75
                      SIG= 1.0                                          CURR  76
           IF(J.NE.4) SIG=-SIG                                          CURR  77
           AA(I,J)=AA(I,J)                                              CURR  78
     $            -SIG*(PAA(I)-2.0*PP(L,I))*(PAA(J)-PP(L,J))/DENOM      CURR  79
 205      CONTINUE                                                      CURR  80
         ENDIF                                                          CURR  81
         go to 204
  37     continue
          if(l.eq.3) go to 204
          if(l.eq.1) l1=2
          if(l.eq.2) l1=4
          DENOM=(PAA(4)-PP(L1,4))**2-(PAA(3)-PP(L1,3))**2                 CURR  72
     $         -(PAA(2)-PP(L1,2))**2-(PAA(1)-PP(L1,1))**2                 CURR  73
          DO 705 I=1,4                                                  CURR  74
          DO 705 J=1,4                                                  CURR  75
                      SIG= 1.0                                          CURR  76
           IF(J.NE.4) SIG=-SIG                                          CURR  77
           AA(I,J)=AA(I,J)                                              CURR  78
     $            -SIG*(PAA(I)-2.0*PP(L1,I))*(PAA(J)-PP(L1,J))/DENOM      CURR  79
 705      CONTINUE                                                      CURR  80
 204    CONTINUE                                                        CURR  82
C --- let's add something to HADCURR                                    CURR  83
cam     FORM1= FPIKM(SQRT(SK),AMPI,AMPI) *FPIKM(SQRT(QQ),AMPI,AMPI)     CURR  84
C       FORM1= FPIKM(SQRT(SK),AMPI,AMPI) *FPIKMD(SQRT(QQ),AMPI,AMPI)    CURR  85
        FORM1=WIGFOR(SK,AMRO,GAMRO)                                     CURR  86
C                                                                       CURR  87
       FIX=1.0                                                          CURR  88
       IF (K.EQ.3) FIX= 0.6                                             CURR  89
       IF (K.EQ.4) FIX=-1.8                                             CURR  89
       DO 206 I=1,4                                                     CURR  90
       DO 206 J=1,4                                                     CURR  91
         if(k.eq.4) go to 17
        HADCUR(I)=                                                      CURR  92
     $  HADCUR(I)+CMPLX(FIX*COEF1)*FORM1*AA(I,J)*(PP(K,J)-PP(4,J))      CURR  93
        go to 206
  17    continue
        HADCUR(I)=                                                      CURR  92
     $  HADCUR(I)+CMPLX(FIX*COEF1)*FORM1*AA(I,J)*(PP(1,J)-PP(3,J))      CURR  93
 206   CONTINUE                                                         CURR  94
C --- end of the non omega current (3 possibilities)                    CURR  95
 201   CONTINUE                                                         CURR  96
C                                                                       CURR  97
C                                                                       CURR  98
C --- there are two possibilities for omega current                     CURR  99
C --- PA PB are corresponding first and second pi-'s                    CURR 100
       DO 301 KK=1,2                                                    CURR 101
        DO 302 I=1,4                                                    CURR 102
         PA(I)=PP(KK,I)                                                 CURR 103
         PB(I)=PP(3-KK,I)                                               CURR 104
 302    CONTINUE                                                        CURR 105
C --- lorentz invariants                                                CURR 106
         QQA=0.0                                                        CURR 107
         SS23=0.0                                                       CURR 108
         SS24=0.0                                                       CURR 109
         SS34=0.0                                                       CURR 110
         QP1P2=0.0                                                      CURR 111
         QP1P3=0.0                                                      CURR 112
         QP1P4=0.0                                                      CURR 113
         P1P2 =0.0                                                      CURR 114
         P1P3 =0.0                                                      CURR 115
         P1P4 =0.0                                                      CURR 116
        DO 303 K=1,4                                                    CURR 117
                     SIGN=-1.0                                          CURR 118
         IF (K.EQ.4) SIGN= 1.0                                          CURR 119
         QQA=QQA+SIGN*(PAA(K)-PA(K))**2                                 CURR 120
         SS23=SS23+SIGN*(PB(K)  +PIM3(K))**2                            CURR 121
         SS24=SS24+SIGN*(PB(K)  +PIM4(K))**2                            CURR 122
         SS34=SS34+SIGN*(PIM3(K)+PIM4(K))**2                            CURR 123
         QP1P2=QP1P2+SIGN*(PAA(K)-PA(K))*PB(K)                          CURR 124
         QP1P3=QP1P3+SIGN*(PAA(K)-PA(K))*PIM3(K)                        CURR 125
         QP1P4=QP1P4+SIGN*(PAA(K)-PA(K))*PIM4(K)                        CURR 126
         P1P2=P1P2+SIGN*PA(K)*PB(K)                                     CURR 127
         P1P3=P1P3+SIGN*PA(K)*PIM3(K)                                   CURR 128
         P1P4=P1P4+SIGN*PA(K)*PIM4(K)                                   CURR 129
 303    CONTINUE                                                        CURR 130
C                                                                       CURR 131
        FORM2=COEF2*(BWIGN(QQ,AMRO,GAMRO)+ELPHA*BWIGN(QQ,AMROP,GAMROP)) CURR 132
C        FORM3=BWIGN(QQA,AMOM,GAMOM)*(BWIGN(SS23,AMRO,GAMRO)+           CURR 133
C     $        BWIGN(SS24,AMRO,GAMRO)+BWIGN(SS34,AMRO,GAMRO))           CURR 134
        FORM3=BWIGN(QQA,AMOM,GAMOM)                                     CURR 135
C                                                                       CURR 136
        DO 304 K=1,4                                                    CURR 137
         HADCUR(K)=HADCUR(K)+FORM2*FORM3*(                              CURR 138
     $             PB  (K)*(QP1P3*P1P4-QP1P4*P1P3)                      CURR 139
     $            +PIM3(K)*(QP1P4*P1P2-QP1P2*P1P4)                      CURR 140
     $            +PIM4(K)*(QP1P2*P1P3-QP1P3*P1P2) )                    CURR 141
 304    CONTINUE                                                        CURR 142
 301   CONTINUE                                                         CURR 143
C                                                                       CURR 144
      ELSE                                                              CURR 145
C ===================================================================   CURR 146
C pi0 pi0 p0 pi- case                                            ====   CURR 147
C ===================================================================   CURR 148
       QQ=PAA(4)**2-PAA(3)**2-PAA(2)**2-PAA(1)**2                       CURR 149
       DO 101 K=1,3                                                     CURR 150
C --- loop over thre contribution of the non-omega current              CURR 151
        SK=(PP(K,4)+PIM4(4))**2-(PP(K,3)+PIM4(3))**2                    CURR 152
     $    -(PP(K,2)+PIM4(2))**2-(PP(K,1)+PIM4(1))**2                    CURR 153
C -- definition of AA matrix                                            CURR 154
C -- cronecker delta                                                    CURR 155
        DO 102 I=1,4                                                    CURR 156
         DO 103 J=1,4                                                   CURR 157
 103     AA(I,J)=0.0                                                    CURR 158
 102    AA(I,I)=1.0                                                     CURR 159
C                                                                       CURR 160
C ... and the rest ...                                                  CURR 161
        DO 104 L=1,3                                                    CURR 162
         IF (L.NE.K) THEN                                               CURR 163
          DENOM=(PAA(4)-PP(L,4))**2-(PAA(3)-PP(L,3))**2                 CURR 164
     $         -(PAA(2)-PP(L,2))**2-(PAA(1)-PP(L,1))**2                 CURR 165
          DO 105 I=1,4                                                  CURR 166
          DO 105 J=1,4                                                  CURR 167
                      SIG=1.0                                           CURR 168
           IF(J.NE.4) SIG=-SIG                                          CURR 169
           AA(I,J)=AA(I,J)                                              CURR 170
     $            -SIG*(PAA(I)-2.0*PP(L,I))*(PAA(J)-PP(L,J))/DENOM      CURR 171
 105      CONTINUE                                                      CURR 172
         ENDIF                                                          CURR 173
 104    CONTINUE                                                        CURR 174
C --- let's add something to HADCURR                                    CURR 175
cam     FORM1= FPIKM(SQRT(SK),AMPI,AMPI) *FPIKM(SQRT(QQ),AMPI,AMPI)     CURR 176
C       FORM1= FPIKM(SQRT(SK),AMPI,AMPI) *FPIKMD(SQRT(QQ),AMPI,AMPI)    CURR 177
        FORM1=WIGFOR(SK,AMRO,GAMRO)                                     CURR 178
        DO 106 I=1,4                                                    CURR 179
        DO 106 J=1,4                                                    CURR 180
         HADCUR(I)=                                                     CURR 181
     $   HADCUR(I)+CMPLX(COEF1)*FORM1*AA(I,J)*(PP(K,J)-PP(4,J))         CURR 182
 106    CONTINUE                                                        CURR 183
C --- end of the non omega current (3 possibilities)                    CURR 184
 101   CONTINUE                                                         CURR 185
      ENDIF                                                             CURR 186
      END                                                               CURR 187
