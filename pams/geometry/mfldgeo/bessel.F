      subroutine bessel
      print *,' bessel functions loaded '
      end

C
      REAL FUNCTION BESI(X,N,IER)
C
      IER=0
      IF(X.GT.170.) GO TO 14
      BIO=ABS(X)
C
      IF(BIO-3.75)1,1,2
1     Z=X*X*7.111111E-2
      BIO=(((((4.5813E-3*Z+3.60768E-2)*Z+2.659732E-1)*Z+
     1    1.206749)*Z+3.089942)*Z+3.515623)*Z+1.
C
      GO TO 3
C
2     Z=3.75/BIO
      BIO=EXP(BIO)/SQRT(BIO)*((((((((3.92377E-3*Z-1.647633E-2)*Z
     1  +2.635537E-2)*Z-2.057706E-2)*Z+9.16281E-3)*Z-1.57565E-3)*Z
     1  +2.25319E-3)*Z+1.328592E-2)*Z+3.989423E-1)
C
3     IF(N)4,5,6
4     IER=1
5     BESI=BIO
      RETURN
6     FN=N+N
      Q1=X/FN
      IF(ABS(X)-5.E-4) 11,11,7
C
7     A0=1.
      A1=0.
      B0=0.
      B1=1.
      FI=FN
C
8     FI=FI+2
      AN=FI/ABS(X)
      A=AN*A1+A0
      B=AN*B1+B0
      A0=A1
      B0=B1
      A1=A
      B1=B
      Q0=Q1
      Q1=A/B
      IF(ABS((Q1-Q0)/Q1)-1.E-6) 9,9,8
C
9     IF(X) 10,11,11
10    Q1=-Q1
11    K=N
      BESI=BIO
12    Q1=X/(FN+X*Q1)
      BESI=BESI*Q1
      FN=FN-2
      K=K-1
      IF(K) 13,13,12
13    RETURN
14    IER=2
      BESI=0.
      RETURN
      END
C
      REAL FUNCTION DERBSI(X,M,IER)
C
      IER1=0
      IER2=0
C
      IF(M) 1000,2000,3000
C
1000  CONTINUE
      DERBSI=0.0
      IER=1
      RETURN
C
2000  CONTINUE
      DERBSI=BESI(X,1,IER)
      RETURN
C
3000  CONTINUE
      M1=M-1
      M2=M+1
      DERBSI=0.5*(BESI(X,M1,IER1)+BESI(X,M2,IER2))
      IER=IER1+IER2
      RETURN
C
      END
C
C
C      REAL FUNCTION DERBSJ(M,X,IER)
C
C      IF(M.GE.0) GO TO 1000
C      DERBSJ=0.
C      IER=1
C      RETURN
C
C1000  CONTINUE
C      IF(M.GT.0) GO TO 2000
C      DERBSJ=-1.*BESJN(1,X)
C      RETURN
C
C2000  CONTINUE
C      M1=M-1
C      M2=M+1
C      DERBSJ=.5*(BESJN(M1,X)-BESJN(M2,X))
C      RETURN
C
C      END
C
