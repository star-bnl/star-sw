
      SUBROUTINE PHO_DOR94LO(X,Q2,Uv,Dv,Del,Udb,Sb,Gl)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C                                                                 *
C    G R V  -  P R O T O N  - P A R A M E T R I Z A T I O N S     *
C                                                                 *
C                         1994 UPDATE                             *
C                                                                 *
C                 FOR A DETAILED EXPLANATION SEE                  *
C                   M. GLUECK, E.REYA, A.VOGT :                   *
C                   DO-TH 94/24  =  DESY 94-206                   *
C                    (TO APPEAR IN Z. PHYS. C)                    *
C                                                                 *
C   THE PARAMETRIZATIONS ARE FITTED TO THE EVOLVED PARTONS FOR    *
C        Q**2 / GEV**2  BETWEEN   0.4   AND  1.E6                 *
C             X         BETWEEN  1.E-5  AND   1.                  *
C   LARGE-X REGIONS, WHERE THE DISTRIBUTION UNDER CONSIDERATION   *
C   IS NEGLIGIBLY SMALL, WERE EXCLUDED FROM THE FIT.              *
C                                                                 *
C   HEAVY QUARK THRESHOLDS  Q(H) = M(H)  IN THE BETA FUNCTION :   *
C                   M(C)  =  1.5,  M(B)  =  4.5                   *
C   CORRESPONDING LAMBDA(F) VALUES IN GEV FOR  Q**2 > M(H)**2 :   *
C      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
C             LAMBDA(5)  =  0.153,                                *
C      NLO :  LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
C             LAMBDA(5)  =  0.131.                                *
C   THE NUMBER OF ACTIVE QUARK FLAVOURS IS  NF = 3  EVERYWHERE    *
C   EXCEPT IN THE BETA FUNCTION, I.E. THE HEAVY QUARKS C,B,...    *
C   ARE NOT PRESENT AS PARTONS IN THE Q2-EVOLUTION.               *
C   IF NEEDED, HEAVY QUARK DENSITIES CAN BE TAKEN FROM THE 1991   *
C   GRV PARAMETRIZATION.                                          *
C                                                                 *
C   NLO DISTRIBUTIONS ARE GIVEN IN MS-BAR FACTORIZATION SCHEME    *
C   (SUBROUTINE GRV94HO) AS WELL AS IN THE DIS SCHEME (GRV94DI),  *
C   THE LEADING ORDER PARAMETRIZATION IS PROVIDED BY "GRV94LO".   *
C                                                                 *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C...INPUT PARAMETERS :
C
C    X   = MOMENTUM FRACTION
C    Q2  = SCALE Q**2 IN GEV**2
C
C...OUTPUT (ALWAYS X TIMES THE DISTRIBUTION) :
C
C    UV  = U(VAL) = U - U(BAR)
C    DV  = D(VAL) = D - D(BAR)
C    DEL = D(BAR) - U(BAR)
C    UDB = U(BAR) + D(BAR)
C    SB  = S = S(BAR)
C    GL  = GLUON
C
C...LO PARAMETRIZATION :
C

      IMPLICIT NONE
      DOUBLE PRECISION ad , ae , ag , agx , akd , ake , akg , aks , 
     &                 aku , akx , alg , als , alx , as , au , bd , be , 
     &                 beg , bes , bex
      DOUBLE PRECISION bg , bgx , bkd , bke , bkg , bku , bkx , bs , 
     &                 bu , cd , ce , cg , cu , cx , dd , de , Del , 
     &                 dg , ds , dst
      DOUBLE PRECISION du , Dv , dx , eg , esg , ess , est , esx , ex , 
     &                 Gl , lam2 , mu2 , nd , ne , nu , PHO_DOR94FS , 
     &                 PHO_DOR94FV , PHO_DOR94FW , Q2 , s
      DOUBLE PRECISION s2 , s3 , Sb , Udb , Uv , X
      SAVE 
 
      mu2 = 0.23
      lam2 = 0.2322*0.2322
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      ds = SQRT(s)
      s2 = s*s
      s3 = s2*s
C...UV :
      nu = 2.284 + 0.802*s + 0.055*s2
      aku = 0.590 - 0.024*s
      bku = 0.131 + 0.063*s
      au = -0.449 - 0.138*s - 0.076*s2
      bu = 0.213 + 2.669*s - 0.728*s2
      cu = 8.854 - 9.135*s + 1.979*s2
      du = 2.997 + 0.753*s - 0.076*s2
      Uv = PHO_DOR94FV(X,nu,aku,bku,au,bu,cu,du)
C...DV :
      nd = 0.371 + 0.083*s + 0.039*s2
      akd = 0.376
      bkd = 0.486 + 0.062*s
      ad = -0.509 + 3.310*s - 1.248*s2
      bd = 12.41 - 10.52*s + 2.267*s2
      cd = 6.373 - 6.208*s + 1.418*s2
      dd = 3.691 + 0.799*s - 0.071*s2
      Dv = PHO_DOR94FV(X,nd,akd,bkd,ad,bd,cd,dd)
C...DEL :
      ne = 0.082 + 0.014*s + 0.008*s2
      ake = 0.409 - 0.005*s
      bke = 0.799 + 0.071*s
      ae = -38.07 + 36.13*s - 0.656*s2
      be = 90.31 - 74.15*s + 7.645*s2
      ce = 0.0
      de = 7.486 + 1.217*s - 0.159*s2
      Del = PHO_DOR94FV(X,ne,ake,bke,ae,be,ce,de)
C...UDB :
      alx = 1.451
      bex = 0.271
      akx = 0.410 - 0.232*s
      bkx = 0.534 - 0.457*s
      agx = 0.890 - 0.140*s
      bgx = -0.981
      cx = 0.320 + 0.683*s
      dx = 4.752 + 1.164*s + 0.286*s2
      ex = 4.119 + 1.713*s
      esx = 0.682 + 2.978*s
      Udb = PHO_DOR94FW(X,s,alx,bex,akx,bkx,agx,bgx,cx,dx,ex,esx)
C...SB :
      als = 0.914
      bes = 0.577
      aks = 1.798 - 0.596*s
      as = -5.548 + 3.669*ds - 0.616*s
      bs = 18.92 - 16.73*ds + 5.168*s
      dst = 6.379 - 0.350*s + 0.142*s2
      est = 3.981 + 1.638*s
      ess = 6.402
      Sb = PHO_DOR94FS(X,s,als,bes,aks,as,bs,dst,est,ess)
C...GL :
      alg = 0.524
      beg = 1.088
      akg = 1.742 - 0.930*s
      bkg = -0.399*s2
      ag = 7.486 - 2.185*s
      bg = 16.69 - 22.74*s + 5.779*s2
      cg = -25.59 + 29.71*s - 7.296*s2
      dg = 2.792 + 2.215*s + 0.422*s2 - 0.104*s3
      eg = 0.807 + 2.005*s
      esg = 3.841 + 0.316*s
      Gl = PHO_DOR94FW(X,s,alg,beg,akg,bkg,ag,bg,cg,dg,eg,esg)
 
      END SUBROUTINE
