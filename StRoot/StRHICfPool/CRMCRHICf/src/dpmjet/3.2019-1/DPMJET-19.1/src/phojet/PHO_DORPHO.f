
      SUBROUTINE PHO_DORPHO(X,Q2,Vap,Glp,Qbp,Cbp,Bbp)
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
 
      mu2 = 0.3
      lam2 = 0.248*0.248
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      ds = SQRT(s)
      s2 = s*s
C...X * VALENCE :
      nv = 0.456 + 0.150*ds + 0.112*s - 0.019*s2
      akv = 0.505 - 0.033*s
      agv = 0.748 - 0.669*ds - 0.133*s
      dv = 0.365 + 0.197*ds + 0.394*s
      Vap = PHO_DORFVP(X,nv,akv,agv,dv)
C...X * GLUON :
      alg = 1.096
      beg = 1.371
      akg = 0.437 - 0.689*ds
      bkg = -0.631
      agg = 1.324 - 0.441*ds - 0.130*s
      bgg = -0.955 + 0.259*s
      cg = 1.075 - 0.302*s
      dg = 1.158 + 1.229*s
      eg = 0.0 + 2.510*s
      esg = 2.604 + 0.165*s
      Glp = PHO_DORFGP(X,s,alg,beg,akg,bkg,agg,bgg,cg,dg,eg,esg)
C...X * QBAR (SU(3)-SYMMETRIC SEA) :
      sl = 0.0
      als = 0.85
      bes = 0.96
      aks = -0.350 + 0.806*s
      ags = -1.663
      bs = 3.148
      ds = 2.273 + 1.438*s
      est = 3.214 + 1.545*s
      ess = 1.341 + 1.938*s
      Qbp = PHO_DORFQP(X,s,sl,als,bes,aks,ags,bs,ds,est,ess)
C...X * CBAR = X * C :
      sc = 0.820
      alc = 0.98
      bec = 0.0
      akc = 0.0 - 0.457*s
      agc = 0.0
      bc = -1.00 + 1.40*s
      dc = 1.318 + 0.584*s
      ec = 4.45 + 1.235*s
      esc = 1.496 + 1.010*s
      Cbp = PHO_DORFQP(X,s,sc,alc,bec,akc,agc,bc,dc,ec,esc)
C...X * BBAR = X * B :
      sbo = 1.297
      alb = 0.99
      beb = 0.0
      akb = 0.0 - 0.172*s
      agb = 0.0
      bbo = 0.0
      db = 1.447 + 0.485*s
      eb = 4.79 + 1.164*s
      esb = 1.724 + 2.121*s
      Bbp = PHO_DORFQP(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,esb)
 
      END SUBROUTINE
