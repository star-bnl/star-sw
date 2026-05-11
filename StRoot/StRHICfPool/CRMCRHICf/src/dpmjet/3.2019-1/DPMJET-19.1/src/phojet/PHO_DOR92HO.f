
      SUBROUTINE PHO_DOR92HO(X,Q2,Udv,Dv,Gl,Udb,Sb,Cb,Bb)
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
 
      mu2 = 0.3
      lam2 = 0.248*0.248
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      ds = SQRT(s)
      s2 = s*s
      s3 = s2*s
C...X * (UV + DV) :
      nud = 0.330 + 0.151*s - 0.059*s2 + 0.027*s3
      akud = 0.285
      agud = -2.28 + 15.73*s - 4.58*s2
      bud = 56.7 - 53.6*s + 11.21*s2
      dud = 3.17 + 1.17*s - 0.47*s2 + 0.09*s3
      Udv = PHO_DOR92FV(X,nud,akud,agud,bud,dud)
C...X * DV :
      nd = 0.459 + 0.315*ds + 0.515*s
      akd = 0.624 - 0.031*s
      agd = 8.13 - 6.77*ds + 0.46*s
      bd = 6.59 - 12.83*ds + 5.65*s
      dd = 3.98 + 1.04*s - 0.34*s2
      Dv = PHO_DOR92FV(X,nd,akd,agd,bd,dd)
C...X * G :
      alg = 1.128
      beg = 1.575
      akg = 0.323 + 1.653*s
      bkg = 0.811 + 2.044*s
      agg = 0.0 + 1.963*s - 0.519*s2
      bgg = 0.078 + 6.24*s
      cg = 30.77 - 24.19*s
      dg = 3.188 + 0.720*s
      eg = -0.881 + 2.687*s
      esg = 2.466
      Gl = PHO_DOR92FW(X,s,alg,beg,akg,bkg,agg,bgg,cg,dg,eg,esg)
C...X * UBAR = X * DBAR :
      alu = 0.594
      beu = 0.614
      aku = 0.636 - 0.084*s
      bku = 0.0
      agu = 1.121 - 0.193*s
      bgu = 0.751 - 0.785*s
      cu = 8.57 - 1.763*s
      du = 10.22 + 0.668*s
      eu = 3.784 + 1.280*s
      esu = 1.808 + 0.980*s
      Udb = PHO_DOR92FW(X,s,alu,beu,aku,bku,agu,bgu,cu,du,eu,esu)
C...X * SBAR = X * S :
      ss = 0.0
      als = 0.756
      bes = 0.101
      aks = 2.942 - 1.016*s
      ags = -4.60 + 1.167*s
      bs = 9.31 - 1.324*s
      ds = 11.49 - 1.198*s + 0.053*s2
      est = 2.630 + 1.729*s
      ess = 8.12
      Sb = PHO_DOR92FS(X,s,ss,als,bes,aks,ags,bs,ds,est,ess)
C...X * CBAR = X * C :
      sc = 0.820
      alc = 0.98
      bec = 0.0
      akc = -0.625 - 0.523*s
      agc = 0.0
      bc = 1.896 + 1.616*s
      dc = 4.12 + 0.683*s
      ec = 4.36 + 1.328*s
      esc = 0.677 + 0.679*s
      Cb = PHO_DOR92FS(X,s,sc,alc,bec,akc,agc,bc,dc,ec,esc)
C...X * BBAR = X * B :
      sbo = 1.297
      alb = 0.99
      beb = 0.0
      akb = 0.0 - 0.193*s
      agb = 0.0
      bbo = 0.0
      db = 3.447 + 0.927*s
      eb = 4.68 + 1.259*s
      esb = 1.892 + 2.199*s
      Bb = PHO_DOR92FS(X,s,sbo,alb,beb,akb,agb,bbo,db,eb,esb)
 
      END SUBROUTINE
