
C
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C                                                                 *
C    G R V  -  P R O T O N  - P A R A M E T R I Z A T I O N S     *
C                                                                 *
C                 FOR A DETAILED EXPLANATION SEE :                *
C              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/07             *
C                                                                 *
C   THE PARAMETRIZATIONS ARE FITTED TO THE PARTON DISTRIBUTIONS   *
C   FOR Q ** 2 BETWEEN MU ** 2 (=  0.25 / 0.30  GEV ** 2  IN LO   *
C   / HO) AND  1.E8 GEV ** 2  AND FOR X BETWEEN  1.E-5  AND  1.   *
C   REGIONS, WHERE THE DISTRIBUTION UNDER CONSIDERATION IS NEG-   *
C   LIGIBLE, I.E. BELOW ABOUT 1.E-4, WERE EXCLUDED FROM THE FIT.  *
C                                                                 *
C              HEAVY QUARK THRESHOLDS  Q(H) = M(H) :              *
C         M(C)  =  1.5,  M(B)  =  4.5,  M(T)  =  100  GEV         *
C                                                                 *
C      CORRESPONDING LAMBDA(F) VALUES FOR F ACTIVE FLAVOURS :     *
C      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
C             LAMBDA(5)  =  0.153,   LAMBDA(6)  =  0.082  GEV     *
C      HO :   LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
C             LAMBDA(5)  =  0.131,   LAMBDA(6)  =  0.053  GEV     *
C                                                                 *
C   HO DISTRIBUTION REFER TO THE MS-BAR SCHEME OF BARDEEN ET AL.  *
C                                                                 *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      SUBROUTINE PHO_DOR92LO(X,Q2,Udv,Dv,Gl,Udb,Sb,Cb,Bb)
      IMPLICIT NONE
      DOUBLE PRECISION agb , agc , agd , agg , ags , agu , agud , akb , 
     &                 akc , akd , akg , aks , aku , akud , alb , alc , 
     &                 alg , als , alu , Bb
      DOUBLE PRECISION bbo , bc , bd , beb , bec , beg , bes , beu , 
     &                 bgg , bgu , bkg , bku , bs , bud , Cb , cg , cu , 
     &                 db , dc , dd
      DOUBLE PRECISION dg , ds , du , dud , Dv , eb , ec , eg , esb , 
     &                 esc , esg , ess , est , esu , eu , Gl , lam2 , 
     &                 mu2 , nd , nud
      DOUBLE PRECISION PHO_DOR92FS , PHO_DOR92FV , PHO_DOR92FW , Q2 , 
     &                 s , s2 , s3 , Sb , sbo , sc , ss , Udb , Udv , X
      SAVE 
 
      mu2 = 0.25
      lam2 = 0.232*0.232
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      s2 = s*s
      s3 = s2*s
C...X * (UV + DV) :
      nud = 0.663 + 0.191*s - 0.041*s2 + 0.031*s3
      akud = 0.326
      agud = -1.97 + 6.74*s - 1.96*s2
      bud = 24.4 - 20.7*s + 4.08*s2
      dud = 2.86 + 0.70*s - 0.02*s2
      Udv = PHO_DOR92FV(X,nud,akud,agud,bud,dud)
C...X * DV :
      nd = 0.579 + 0.283*s + 0.047*s2
      akd = 0.523 - 0.015*s
      agd = 2.22 - 0.59*s - 0.27*s2
      bd = 5.95 - 6.19*s + 1.55*s2
      dd = 3.57 + 0.94*s - 0.16*s2
      Dv = PHO_DOR92FV(X,nd,akd,agd,bd,dd)
C...X * G :
      alg = 0.558
      beg = 1.218
      akg = 1.00 - 0.17*s
      bkg = 0.0
      agg = 0.0 + 4.879*s - 1.383*s2
      bgg = 25.92 - 28.97*s + 5.596*s2
      cg = -25.69 + 23.68*s - 1.975*s2
      dg = 2.537 + 1.718*s + 0.353*s2
      eg = 0.595 + 2.138*s
      esg = 4.066
      Gl = PHO_DOR92FW(X,s,alg,beg,akg,bkg,agg,bgg,cg,dg,eg,esg)
C...X * UBAR = X * DBAR :
      alu = 1.396
      beu = 1.331
      aku = 0.412 - 0.171*s
      bku = 0.566 - 0.496*s
      agu = 0.363
      bgu = -1.196
      cu = 1.029 + 1.785*s - 0.459*s2
      du = 4.696 + 2.109*s
      eu = 3.838 + 1.944*s
      esu = 2.845
      Udb = PHO_DOR92FW(X,s,alu,beu,aku,bku,agu,bgu,cu,du,eu,esu)
C...X * SBAR = X * S :
      ss = 0.0
      als = 0.803
      bes = 0.563
      aks = 2.082 - 0.577*s
      ags = -3.055 + 1.024*s**0.67
      bs = 27.4 - 20.0*s**0.154
      ds = 6.22
      est = 4.33 + 1.408*s
      ess = 8.27 - 0.437*s
      Sb = PHO_DOR92FS(X,s,ss,als,bes,aks,ags,bs,ds,est,ess)
C...X * CBAR = X * C :
      sc = 0.888
      alc = 1.01
      bec = 0.37
      akc = 0.0
      agc = 0.0
      bc = 4.24 - 0.804*s
      dc = 3.46 + 1.076*s
      ec = 4.61 + 1.490*s
      esc = 2.555 + 1.961*s
      Cb = PHO_DOR92FS(X,s,sc,alc,bec,akc,agc,bc,dc,ec,esc)
C...X * BBAR = X * B :
      sbo = 1.351
      alb = 1.00
      beb = 0.51
      akb = 0.0
      agb = 0.0
      bbo = 1.848
      db = 2.929 + 1.396*s
      eb = 4.71 + 1.514*s
      esb = 4.02 + 1.239*s
      Bb = PHO_DOR92FS(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,esb)
 
      END SUBROUTINE
