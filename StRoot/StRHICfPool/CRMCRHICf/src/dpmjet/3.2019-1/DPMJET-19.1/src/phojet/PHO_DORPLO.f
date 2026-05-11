
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C                                                                 *
C         G R V - P I O N - P A R A M E T R I Z A T I O N S       *
C                                                                 *
C                 FOR A DETAILED EXPLANATION SEE :                *
C              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/16             *
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
      SUBROUTINE PHO_DORPLO(X,Q2,Vap,Glp,Qbp,Cbp,Bbp)
      IMPLICIT NONE
      DOUBLE PRECISION agb , agc , agg , ags , agv , akb , akc , akg , 
     &                 aks , akv , alb , alc , alg , als , bbo , Bbp , 
     &                 bc , beb , bec , beg
      DOUBLE PRECISION bes , bgg , bkg , bs , Cbp , cg , db , dc , dg , 
     &                 ds , dv , eb , ec , eg , esb , esc , esg , ess , 
     &                 est , Glp
      DOUBLE PRECISION lam2 , mu2 , nv , PHO_DORFGP , PHO_DORFQP , 
     &                 PHO_DORFVP , Q2 , Qbp , s , s2 , sbo , sc , sl , 
     &                 Vap , X
      SAVE 
 
      mu2 = 0.25
      lam2 = 0.232*0.232
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      ds = SQRT(s)
      s2 = s*s
C...X * VALENCE :
      nv = 0.519 + 0.180*s - 0.011*s2
      akv = 0.499 - 0.027*s
      agv = 0.381 - 0.419*s
      dv = 0.367 + 0.563*s
      Vap = PHO_DORFVP(X,nv,akv,agv,dv)
C...X * GLUON :
      alg = 0.599
      beg = 1.263
      akg = 0.482 + 0.341*ds
      bkg = 0.0
      agg = 0.678 + 0.877*s - 0.175*s2
      bgg = 0.338 - 1.597*s
      cg = 0.0 - 0.233*s + 0.406*s2
      dg = 0.390 + 1.053*s
      eg = 0.618 + 2.070*s
      esg = 3.676
      Glp = PHO_DORFGP(X,s,alg,beg,akg,bkg,agg,bgg,cg,dg,eg,esg)
C...X * QBAR (SU(3)-SYMMETRIC SEA) :
      sl = 0.0
      als = 0.55
      bes = 0.56
      aks = 2.538 - 0.763*s
      ags = -0.748
      bs = 0.313 + 0.935*s
      ds = 3.359
      est = 4.433 + 1.301*s
      ess = 9.30 - 0.887*s
      Qbp = PHO_DORFQP(X,s,sl,als,bes,aks,ags,bs,ds,est,ess)
C...X * CBAR = X * C :
      sc = 0.888
      alc = 1.02
      bec = 0.39
      akc = 0.0
      agc = 0.0
      bc = 1.008
      dc = 1.208 + 0.771*s
      ec = 4.40 + 1.493*s
      esc = 2.032 + 1.901*s
      Cbp = PHO_DORFQP(X,s,sc,alc,bec,akc,agc,bc,dc,ec,esc)
C...X * BBAR = X * B :
      sbo = 1.351
      alb = 1.03
      beb = 0.39
      akb = 0.0
      agb = 0.0
      bbo = 0.0
      db = 0.697 + 0.855*s
      eb = 4.51 + 1.490*s
      esb = 3.056 + 1.694*s
      Bbp = PHO_DORFQP(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,esb)
 
      END SUBROUTINE
