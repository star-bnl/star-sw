#define T_xxx
#include <assert.h>
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TRVector.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "T.h"
#include "Riostream.h"
ClassImp(TT);
struct Geometry_t {
  Int_t Barrel;
  Int_t Layer;
  Int_t NoLadders;
  Int_t NoWafers;
};
const Int_t NoLayers = 7;
// Barrel, Layer  ladder wafer
const Geometry_t SvtSsdConfig[] = 
  {    {1,     1,      8,   4}, // even
       {1,     2,      8,   4}, // odd
       {2,     3,     12,   6}, // event
       {2,     4,     12,   6}, // odd
       {3,     5,     16,   7}, // even
       {3,     6,     16,   7}, // odd
       {4,     7,     20,  16}  // Ssd
  };
const Int_t BL[4] = {8, 12, 16, 20}; // ladders in barrel
struct HybridFit_t {
  HybridFit_t() {noentries=0; AmX = TRVector(6); S = TRSymMatrix(6);}
  Int_t noentries;
  TRVector AmX;
  TRSymMatrix S;
};
//________________________________________________________________________________
Double_t STcheb(Int_t N, Double_t *par, Double_t x) {// N polynome degree, dimension is par[N+1]
  if (N < 0 || N > 12) return 0;
  Double_t T0 = 1;
  Double_t T1 = 2*x - 1;
  Double_t T2;
  Double_t Sum = par[0]*T0;
  if (N >= 1) {
    T1 = 2*x - 1;
    Sum += par[1]*T1;
    for (int n = 2; n <= N; n++) {
      T2 = 2*(2*x - 1)*T1 - T0;
      Sum += par[n]*T2;
      T0 = T1;
      T1 = T2;
    }
  }
  return Sum;
}
#if 0
//________________________________________________________________________________
Double_t DriftCorHack(Int_t barrel, Int_t ladder, Int_t wafer, Double_t u) {
  struct data_t {
    Int_t barrel, layer, ladder, wafer, hybrid, Npar;
    Double_t param[12];
    Double_t dparam[12];
    Char_t *Comment;
  };
  static Int_t N = 0;
  static data_t *pointers[3][16][7][2];
  if (N == 0) {
    N = sizeof(Data)/sizeof(data_t);
    memset (pointers,0, 3*16*7*2*sizeof(data_t *));
  }
  Int_t h = 1;
  if (u > 0) h = 2;
  assert(barrel >= 1 && barrel <=  3);
  assert(ladder >= 1 && ladder <= 16);
  assert(wafer  >= 1 && wafer  <=  7);
  data_t *p = pointers[barrel-1][ladder-1][wafer-1][h-1];
  if (! p) {
    for (Int_t i = 0; i < N; i++) {
      if (Data[i].barrel == barrel && 
	  Data[i].ladder == ladder && 
	  Data[i].wafer  == wafer && 
	  Data[i].hybrid == h) {
	p = Data + i;
	pointers[barrel-1][ladder-1][wafer-1][h-1] = p;
	break;
      }
    }
  }
  
  return p ? STcheb(p->Npar, p->param, TMath::Abs(u/3.)) : 0;
}
#endif
//________________________________________________________________________________
void TT::Loop(Int_t Nevents) {
//   In a ROOT session, you can do:
//      Root > .L T.C
//      Root > T t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
  /*   Local
      --------  
                            (1  0          0)
          RotateX(alpha) =  (0  1     -alpha)
                            (0  alpha      1)  

                            (1       0  beta)
          RotateY(beta) =   (0       1     0)
                            (-beta   0     1)

                            (1     -gamma     0)
          RotateZ(gamma) =  (gamma      1     0)
                            (0          0     1)

                            (1     -gamma   beta)
          Rx*Ry*Rz     =    (gamma      1 -alpha)
                            (-beta  alpha      1)


          T is transformation from "real" local (l) coordinate system to "known" as local (l') (local -> Master) 
             (u')             (     1  gamma  -beta)(u)   (du)
          l'=(v')          =  (-gamma      1  alpha)(v) + (dv)
             (w')             (  beta -alpha      1)(w)   (dw)

                      du  dv   dw      alpha          beta    gamma
	  (u - uP) = (-1,  0, tuP,    tuP*vP,      -tuP*uP,      vP) =
          (v - vP)   ( 0, -1, tvP,    tvP*vP,      -tvP*uP,     -uP)

          (u - uP) =  -du    +tuP*dw +tuP*vP*alpha -tuP*uP*beta +vP*gamma;
          (v - vP) =     -dv +tvP*dw +tvP*vP*alpha -tvP*uP*beta -uP*gamma;
or
          (u - uP) =  -du    +tuP*(dw +vP*alpha -uP*beta) +vP*gamma;
          (v - vP) =     -dv +tvP*(dw +vP*alpha -uP*beta) -uP*gamma;
	  Assume uniform distribution over tuP, tvP, vP, uP then:
	     <u - uP>                   => -du;
	     <v - vP>                   => -dv
          1. <u - uP>       versus  tuP => dw
          2  <v - vP>       versus  tvP => dw
          3. <u - uP>       versus   vP => gamma
          4  <v - vP>       versus  -uP => gamma
          5. <(u - uP)/tuP> versus   vP => alpha
	  6  <(v - vP)/tvP> versus   vP => alpha
          7. <(u - uP)/tuP> versus  -uP => beta;
          8. <(v - vP)/tvP> versus  -uP => beta;
	  ________________________________________________________________________________
	  Global
	  ------  
                        (dx)   (     1 -gamma  beta )(xG)    (xP)
       r' = dr + R*r = 	(dy) + ( gamma      1 -alpha)(yG) => (yP) 
                        (dz)   ( -beta  alpha     1 )(zG)    (zP)

       dX = xP - xG = dx            -gamma*yG + beta*zG
       dY = yP - yG = dy + gamma*xG           -alpha*zG
       dZ = zP - zG = dz   -beta*xG +alpha*yG

                      (     1  gamma -beta )(xP-dx)    (xG)
       R^-1*(r-dr) =  (-gamma      1  alpha)(yP-dy) => (yG) 
                      (  beta -alpha     1 )(zP-dz)    (zG)


       dX = xG - xP = -dx            +gamma*yP  -beta*zP
       dY = yG - yP = -dy  -gamma*xP           +alpha*zP
       dZ = zG - zP = -dz   +beta*xP -alpha*yP

       ________________________________________________________________________________
       <dX> => -dx
       <dY> => -dy
       <dZ> => -dz
      0. dX versus  xP
      1. dX versus  yP  => gamma
      2. dX versus -zP  => beta

      3. dY versus -xP  => gamma
      4. dY versus  yP
      5. dY versus  zP  => alpha

      6. dZ versus  xP  => beta
      7. dZ versus -yP  => alpha
      8. dZ versus  zP
      ________________________________________________________________________________
      Account that prediction should be on detector plane
       (xP,yP,zP) == (x,y,z)
      
       dX = xG - x;
       dY = yG - y;
       dZ = zG - z;
      (dxP,dyP,dPz) = track prediction direction in GCS
      (xP,yP,zP)    = track prediction position in GCS 
      (wx.wy.wz)    = detector plane direction in GCS
      (vx,vy,vz)    = (wx,wy,wz)/dLz, dLz track direction in LCS

      dX = dx*(-1+dxP*vx) + dy*(   dxP*vy) + dz*(   dxP*vz) + alpha*(   dxP*(-vy*z+vz*y)) + beta*(-z+dxP*(vx*z-vz*x)) + gamma*( y+dxP*(-vx*y+vy*x))
      dY = dx*(   dyP*vx) + dy*(-1+dyP*vy) + dz*(   dyP*vz) + alpha*( z+dyP*(-vy*z+vz*y)) + beta*(   dyP*(vx*z-vz*x)) + gamma*(-x+dyP*(-vx*y+vy*x))
      dZ = dx*(   dzP*vx) + dy*(   dzP*vy) + dz*(-1+dzP*vz) + alpha*(-y+dzP*(-vy*z+vz*y)) + beta*( x+dzP*(vx*z-vz*x)) + gamma*(   dzP*(-vx*y+vy*x))


------------------
      dRT = I + DT;
      q = (dx,dy,dz,alpha,beta,gamma)
      A = -I + j * vT; v = wG/(wG*dG)
      Delta = (dX,dY,dZ)  = DRT*(x_q - dt) = DRT*(p + h*j) = A ( dt - DT *p) = A * s;
      r0 = DR*t + dt); w0 = DR*R*wL = DR*wG
      w0T *( p + h*j - DR*t - dt) = 0;

                      (     1  gamma -beta )
      DRT =           (-gamma      1  alpha); 
                      (  beta -alpha     1 )

                      (     0  gamma -beta )
      DT = DRT - I =  (-gamma      0  alpha); 
                      (  beta -alpha     0 )

             (     0  gamma -beta ) (x)    (          gamma*y - beta *z)
      DT*p = (-gamma      0  alpha)*(y) =  (-gamma*x          + alpha*z)
             (  beta -alpha     0 ) (z)    ( beta *x -alpha*y          )


 
                                                        ( dx - gamma*y + beta *z)
      s                               = dt - DT * p =   ( dy + gamma*x - alpha*z)
                                                        ( dz - beta *x + alpha*y) 
      q =             (dx,dy,dZ,alpha,beta,gamma)
                      ( 1  0  0     0    z    -y)
      ds / d q = B =  ( 0  1  0    -z    0     x)
                      ( 0  0  1     y   -x     0)


                       (-1  0  0)    (jx)                 ((-1 + jx*vx)       jx*vy        jx*vz ) 
       A = -I + j*vT = ( 0 -1  0)  + (jy) * (vx vy vz) =  (      jy*vx  (-1 + jy*vy)       jy*vz )
                       ( 0  0 -1)    (jz)                 (      jz*vx        jz*vy  (-1 + jz*vz))

                (-1+jx*vx    jx*vy    jx*vz)    ( 1  0  0     0    z    -y)  
       A * B =  (   jy*vx -1+jy*vy    jy*vz) *  ( 0  1  0    -z    0     x) =
	        (   jz*vx    jz*vy -1+jz*vz)    ( 0  0  1     y   -x     0)

      (-1+jx*vx    jx*vy    jx*vz    jx*(-vy*z+vz*y) -z+jx*(vx*z-vz*x)  y+jx*(-vx*y+vy*x))
    = (   jy*vx -1+jy*vy    jy*vz  z+jy*(-vy*z+vz*y)    jy*(vx*z-vz*x) -x+jy*(-vx*y+vy*x))
      (   jz*vx    jz*vy -1+jz*vz -y+jz*(-vy*z+vz*y)  x+jz*(vx*z-vy*x)    jz*(-vx*y+vy*x))
	
  */
  if (fChain == 0) return;  
  struct PlotName_t {
    Char_t *Name;
    Char_t *Title;
    Double_t xmax[2]; // svt  ssd 
  };
  //                        svt   ssd
  //   static Double_t Dv[2] = {3.000, 2.10};
  //   Double_t Radii[7] = { 6.37, 7.38, 10.38, 11.27, 14.19, 15.13, 23.80};
  static Double_t Du[2] = {3.000, 3.65};
  static Double_t Sv[2] = {6.305, 4.35};
  const  PlotName_t plotNameD[10] = {// plots for drift
     {"dutuP","<u - uP>       versus  tuP =>  dw for Drift", { 0.5, 0.5}},                    //  0
     {"dvtvP","<v - vP>       versus  tvP =>  dw for Drift", { 2.5, 2.5}},                    //  1
     {"duvP", "<u - uP>       versus    v =>  gamma for Drift", { -2, -2}},                   //  2 z
     {"dvuP", "<v - vP>       versus    u => -gamma for Drift", { -1, -1}},                   //  3
     {"duOvertuPvP","<(u - uP)/tuP> versus  v => alpha for Drift", { -2, -2}},                //  4 z
     {"dvOvertvPvP","<(v - vP)/tvP> versus  v => alpha for Drift", { -2, -2}},                //  5 z
     {"duOvertuPuP","<(u - uP)/tuP> versus  u => -beta for Drift", { -1, -1}},                //  6
     {"dvOvertvPuP","<(v - vP)/tvP> versus  u => -beta for Drift", { -1, -1}},                //  7
     {"duuH"       , "<u - uP>       versus  uHat for Drift",      {1.2, -1}},                //  8 
     {"duvH"       , "<u - uP>       versus  vHat for Drift",      {1.2, -2}}                 //  9 z
  };
  const  PlotName_t plotName[37] = {
    {"dutuP","<u - uP>       versus  tuP =>  dw", { 0.5, 0.5}},                    //  0
    {"dvtvP","<v - vP>       versus  tvP =>  dw", { 2.5, 2.5}},                    //  1
    {"duvP", "<u - uP>       versus   vP =>  gamma", { -2, -2}},                   //  2 z
    {"dvuP", "<v - vP>       versus  -uP =>  gamma", { -1, -1}},                   //  3
    {"duOvertuPvP","<(u - uP)/tuP> versus   vP => alpha", { -2, -2}},             //  4 z
    {"dvOvertvPvP","<(v - vP)/tvP> versus   vP => alpha", { -2, -2}},             //  5 z
    {"duOvertuPuP","<(u - uP)/tuP> versus  -uP => beta", { -1, -1}},              //  6
    {"dvOvertvPuP","<(v - vP)/tvP> versus  -uP => beta", { -1, -1}},              //  7
    {"duuP", "<u - uP>       versus  -uP", { -1, -1}},                             //  8 
    {"dvvP", "<v - vP>       versus   vP", { -2, -2}},                             //  9 z
    {"dXvsX","dX versus  x"          , { 16, 24}},                                // 10
    {"dXvsY","dX versus  y  => gamma", { 16, 24}},                                // 11
    {"dXvsZ","dX versus -z  => beta",  { 24.,36}},                                // 12
    {"dYvsX","dY versus -x  => gamma", { 16, 24}},                                // 13
    {"dYvsY","dY versus  y"          , { 16, 24}},                                // 14
    {"dYvsZ","dY versus  z  => alpha", { 24.,36}},                                // 15
    {"dZvsX","dZ versus  x  => beta",  { 16, 24}},                                // 16
    {"dZvsY","dZ versus -y  => alpha", { 16, 24}},                                // 17
    {"dZvsZ","dZ versus  z",           { 24.,36}},                                // 18
    
    {"dX4dx","dX vs -1+jx*vx          => dx",  {2.2,2.2}},                    // 19
    {"dX4dy","dX vs    jx*vy          => dy",    { 1, 1}},                    // 20
    {"dX4dz","dX vs    jx*vz          => dz",  {.01,.01}},                    // 21
    {"dX4da","dX vs    jx*(-vy*z+vz*y)=> alpha", {20,20}},                    // 22
    {"dX4db","dX vs  -z+jx*(vx*z-vz*x)=> beta ", {40,40}},                    // 23
    {"dX4dg","dX vs  y+jx*(-vx*y+vy*x)=> alpha", {25,25}},                    // 24
    
    {"dY4dx","dY vs    jy*vx          => dx",    { 1, 1}},                    // 25
    {"dY4dy","dY vs -1+jy*vy          => dy",  {2.2,2.2}},                    // 26
    {"dY4dz","dY vs    jy*vz          => dz",  {.01,.01}},                    // 27
    {"dY4da","dY vs  z+jy*(-vy*z+vz*y)=> alpha", {35,35}},                    // 28
    {"dY4db","dY vs     jy*(vx*z-vz*x)=> beta ", {20,20}},                    // 29
    {"dY4dg","dY vs -x+jy*(-vx*y+vy*x)=> gamma", {25,25}},                    // 30

    {"dZ4dx","dZ vs    jz*vx          => dx",  {2.2,2.2}},                    // 31
    {"dZ4dy","dZ vs    jz*vy          => dy",  {2.2,2.2}},                    // 32
    {"dZ4dz","dZ vs -1+jz*vz          => dz",  {2.2,2.2}},                    // 33
    {"dZ4da","dZ vs -y+jz*(-vy*z+vz*y)=> alpha", {80,80}},                    // 34
    {"dZ4db","dZ vs  x+jz*( vx*z-vy*x)=> beta ", {80,80}},                    // 35
    {"dZ4dg","dZ vs    jz*(-vx*y+vy*x)=> gamma", {10,10}},                    // 36
    
  };
   
  const Int_t ssdSector[20] = {// 100*sector + ladder
    101, 102,
    203, 204, 205, 206, 207, 208, 209,
    310, 311, 312, 
    413, 414, 415, 416, 417, 418, 419,
    120
  };
  TFile *fOut = new TFile(fOutFileName,"recreate");
  TString Name, Title;
  TH1D *LSF = new TH1D("LSF","Matrix and right part for Least Squred Fit",6*28,0,6*28);
  TH1D *LSFB[4];
  for (Int_t barrel = 1; barrel <= 4; barrel++) 
    LSFB[barrel-1] = new TH1D(Form("LSFB%i",barrel),
			      Form("Matrix and right part for Least Squred Fit for barrel %i",barrel),
			      BL[barrel-1]*28,0,BL[barrel-1]*28);
   //             T  B  l    W
  TH2F *LocPlots[10][4][20][17];
  memset(LocPlots,0,10*4*20*17*sizeof(TH2F *));
  //  TH2F *LocPlots[9][4][20][17];
  //  memset(LocPlots,0,9*4*20*17*sizeof(TH2F *));
   for (Int_t L = 0; L < NoLayers; L++) {// over Layers
     Int_t barrel = SvtSsdConfig[L].Barrel;
     Int_t layer  = SvtSsdConfig[L].Layer;
     Int_t NoLadders = SvtSsdConfig[L].NoLadders;
     Int_t NoWafers = SvtSsdConfig[L].NoWafers;
     //     if (! AllWafers) NoWafers = 2; // use wafer index for Positive / negatives
     for (Int_t ladder = 1; ladder <= NoLadders; ladder++) {
       if (barrel <= 3 && (ladder-1)%2 != layer%2) continue;
       for (Int_t wafer = 0; wafer <= NoWafers; wafer++) {// wafer == 0 for whole ladder
	 Int_t Id = ladder + 100*(wafer + 10*layer);
	 for (Int_t t = 0; t < 10; t++) {
	   if (NoWafers > 2 && wafer != 0) {
	     Name = Form("%s%04i",plotNameD[t].Name,Id);
	     Title = Form("%s for barrel %i, layer %i ladder %i, ",plotNameD[t].Title,barrel,layer,ladder);
	   } else {
	     Name = Form("%s%04i",plotNameD[t].Name,Id);
	     Title = Form("%s for barrel %i, layer %i ladder %i, ",plotNameD[t].Title,barrel,layer,ladder);
	   }
	   if (AllWafers) {
	     if (wafer == 0) Title += "all wafers";
	     else            Title += Form("wafer %i",wafer);
	   } else {
	     Title += "all wafers";
	     if (wafer == 1) Title += " Positive";
	     if (wafer == 2) Title += " Negative";
	   }
	   Int_t n = 100;
	   Int_t k = 0;
	   if (barrel > 3) k = 1;
	   Double_t xmax = plotNameD[t].xmax[k];
#if 0
	   cout << plotNameD[t].Name << "/" << plotNameD[t].Title 
		<< "\txmax " << plotNameD[t].xmax[0] << "\t" <<  plotNameD[t].xmax[1]
		<< "\txmax = " << xmax << endl;
#endif
	   if (xmax > 0 && t >= 8 && ! (NoWafers > 2 && wafer != 0)) xmax = -1;
	   if (xmax < 0) {
	     Int_t m = - (Int_t) xmax;
	     if (m == 1) xmax = Du[k];
	     else        xmax = Sv[k]/2.;
	   }
	   if ((wafer == 0 || ! AllWafers) && (t == 2 || t == 4 || t == 5 || t == 9)) {
	     switch (barrel) {
	     case 1: xmax = 12; break;
	     case 2: xmax = 18; break;
	     case 3: xmax = 21; break;
	     case 0:
	     case 4: xmax = 35; break;
	     default: xmax = 40; break;
	     }
	     xmax += 2;
	     n = (Int_t) (4*xmax);
	   }
	   Double_t ymax = rCut/2;
	   Int_t ny = 500;
	   Double_t dy = ymax/ny;
	   //	   if (t >= 4 && t <= 7) ymax *= 10;
	   LocPlots[t][barrel-1][ladder-1][wafer] = new TH2F(Name,Title,n,-xmax,xmax,ny+1,-ymax-dy,ymax+dy);
	 }
       }
     }
   }
   //              T  S
   TH2F *GloPlots[27][6];
   memset(GloPlots,0,27*6*sizeof(TH2F *));
   for (Int_t s = 0; s < 6; s++) {
     for (Int_t i = 0; i < 27; i++) {
       Int_t t = i+10;
       Name = Form("%s%i",plotName[t].Name,s);
       if (s < 2) 
	 Title = Form("%s for SVT Clam shell %i",plotName[t].Title,s);
       else 
	 Title = Form("%s for SSD Sector %i",plotName[t].Title,s-1);
       Int_t m = 0;
       if (s > 1) m = 1;
       Double_t xmax = plotName[t].xmax[m];
       Int_t n = (Int_t) (4.*xmax);
       if (n < 100) n = 100;
       Double_t ymax = rCut;
       GloPlots[i][s] = new TH2F(Name,Title, n,-xmax,xmax,500,-ymax,ymax);
     }
   }
   //                T  B   L
   TH2F *GloBLPlots[27][4][20];
   memset(GloBLPlots,0,27*4*20*sizeof(TH2F *));
   if (LaddersInGlobal) {
     for (Int_t barrel = 0; barrel < 4; barrel++) {
       for (Int_t ladder = 0; ladder < BL[barrel]; ladder++) {
	 for (Int_t i = 0; i < 27; i++) {
	   Int_t t = i+10;
	   Name = Form("%sB%iL%02i",plotName[t].Name,barrel+1,ladder+1);
	   Title = Form("%s for barrel %i ladder %02i",plotName[t].Title,barrel+1,ladder+1);
	   Int_t m = 0;
	   if (barrel > 3) m = 1;
	   Double_t xmax = plotName[t].xmax[m];
	   Int_t n = (Int_t) (4.*xmax);
	   if (n < 100) n = 100;
	   Double_t ymax = 2.50;
	   GloBLPlots[i][barrel][ladder] = new TH2F(Name,Title, n,-xmax,xmax,500,-ymax,ymax);
	 }
       }
     }
   }
   Long64_t nentries = fChain->GetEntriesFast();
   if (Nevents > 0 && nentries > Nevents) nentries = Nevents;
   Long64_t nbytes = 0, nb = 0;
   Int_t TreeNo = -1;
   TString currentFile("");
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
     Long64_t ientry = LoadTree(jentry);
     if (ientry < 0) break;
     nb = fChain->GetEntry(jentry);   nbytes += nb;
     if (! jentry%1000 || TreeNo != fChain->GetTreeNumber()) {
       if (jentry > 0) fOut->Flush();
       cout << "Read event \t" << jentry 
	    << " so far. switch to file " << fChain->GetCurrentFile()->GetName() << endl;
       TreeNo = fChain->GetTreeNumber();
     }
     if (VertexZCut > 0 && TMath::Abs(fVertex[2]) > VertexZCut) continue;
     UInt_t Ntrack = fNPTracks;     
     Int_t  run    = fEvtHdr_fRun;
     for (UInt_t trk = 0; trk < Ntrack; trk++) {
       Int_t Npoints = fTracks_fNpoint[trk];
       if (minNoFitPoints > 0 && Npoints%100 < minNoFitPoints) continue;
       if (UseSsd && Npoints < 1000) continue; 
       if (UseSvt && Npoints <  100) continue; 
       if (TpcLengthCut > 0 && fTracks_fLength[trk] < TpcLengthCut) continue;
       if (dEdxCut > 0      && (fTracks_fdEdx[trk] <= 1e-7 || fTracks_fdEdx[trk] > dEdxCut)) continue;
	  
       if (TMath::Abs(fTracks_fTanL[trk]) > 3) continue;
       if (EastWest) {
	 Double_t zTPC = fVertex[2] + 60*fTracks_fTanL[trk];
	 Double_t zCut = 0;
	 if (EastWest > 2) zCut = 60.*TMath::SinH(0.5);
	 if (EastWest%2 == 1 && ! (zTPC < -zCut || fTracks_fTanL[trk] < 0)) continue;
	 if (EastWest%2 == 0 && ! (zTPC >  zCut || fTracks_fTanL[trk] > 0)) continue;
       }
       Int_t Nsp = fTracks_fNsp[trk];
       //       if (Nsp <= 0 || Nsp >= 10) continue;
       for (Int_t hit = 0; hit < Nsp; hit++) {
	 Int_t k = fTracks_fIdHitT[trk][hit]-1;
	 //	for (UInt_t k = 0;  k < fNhit; k++) {
	 Int_t barrel = fHits_barrel[k];
	 Int_t layer  = fHits_layer[k];
	 Int_t ladder = fHits_ladder[k];
	 Int_t wafer  = fHits_wafer[k];
	 Int_t hybrid = fHits_hybrid[k];
	 Double32_t anode = fHits_anode[k];
	 Int_t sector = -1;
	 if (fHits_isTrack[k]) continue; // prediction only
	 if (layer < 7 && fHits_hitFlag[k] > 3) continue;
	 //Run V	if (layer < 7 && IsNotValidHybrid(barrel,ladder,wafer,hybrid,run,anode)) continue;
	 //Run VI
	 if (layer < 7 && IsNotValidHybrid(barrel,ladder,wafer,hybrid,run,anode)) continue;
	 if (layer < 7) {
	   sector = 0;
	   if (ladder > SvtSsdConfig[layer-1].NoLadders/2) sector = 1;
	 } else         {sector = ssdSector[ladder-1]/100 + 1; barrel = 4;}
	 if (sector < 0 || sector > 5) {
	   cout << "Sector " << sector;
	   cout << " for barrel " << barrel 
		<< ", ladder " << ladder
		<< ",wafer " <<  wafer << "\t";
	   cout << "is not been defined" << endl;
	   continue;
	 }
	 if (! GloPlots[0][sector]) {
	   cout << "GloPlots[0][" << sector << "]"; 
	   cout << " for barrel " << barrel 
		<< ", ladder " << ladder
		<< ",wafer " <<  wafer << "\t";
	   cout << "is not been defined" << endl;
	   continue;
	 }
	 if (LaddersInGlobal && ! GloBLPlots[0][barrel-1][ladder-1]) {
	   cout << "GloBLPlots[0][" << sector << "]"; 
	   cout << " for barrel " << barrel 
		<< ", ladder " << ladder
		<< ",wafer " <<  wafer << "\t";
	   cout << "is not been defined" << endl;
	   continue;
	 }
	 if (AllWafers) {
	   if (! LocPlots[0][barrel-1][ladder-1][wafer]) {
	     cout << "locPlots";  
	     cout << " for barrel " << barrel 
		  << ", ladder " << ladder
		  << ",wafer " <<  wafer << "\t";
	     cout << "is not been defined" << endl;
	     continue;
	   }
	 }
	 if (DipCut > 0 && TMath::Abs(fHits_pT[k]) < DipCut*fHits_pMom[k]) continue;
	 //       Int_t NoWafers = SvtSsdConfig[layer-1].NoWafers;
	 Double32_t xPG = fHits_xPG[k];
	 Double32_t zPG = fHits_zPG[k];
	 Double32_t yPG = fHits_yPG[k];
	 Double32_t uP = fHits_uP[k];       
	 Double32_t vP = fHits_vP[k];
	 Double32_t tuP = fHits_tuP[k];       
	 Double32_t tvP = fHits_tvP[k];
	 Double32_t xPL = fHits_xPL[k];
	 Double32_t zPL = fHits_zPL[k];
	 Double_t dxP = fHits_cxPG[k];
	 Double_t dyP = fHits_cyPG[k];
	 Double_t dzP = fHits_czPG[k];
#ifdef __USE_GLOBAL__
	 if (fGlobal) {
	   xPG = fHits_xPGlG[k];
	   zPG = fHits_zPGlG[k];
	   yPG = fHits_yPGlG[k];
	   uP = fHits_uPGl[k];       
	   vP = fHits_vPGl[k];
	   tuP = fHits_tuPGl[k];       
	   tvP = fHits_tvPGl[k];
	   xPL = fHits_xPGlL[k];
	   zPL = fHits_zPGlL[k];
	   dxP = fHits_cxPGlG[k];
	   dyP = fHits_cyPGlG[k];
	   dzP = fHits_czPGlG[k];
	 }
#endif
	 Double32_t xG = fHits_xG[k];       
	 Double32_t yG = fHits_yG[k];       
	 Double32_t zG = fHits_zG[k];       
	 Double32_t dX = xG - xPG;
	 Double32_t dY = yG - yPG;
	 Double32_t dZ = zG - zPG;
	 Double32_t u = fHits_u[k];       
	 Double32_t v = fHits_v[k];
	 Double32_t uHat = fHits_uHat[k];       
	 if (hybrid == 1) uHat -= 0.1;
	 if (hybrid == 2) uHat += 0.1;
	 //	 Double32_t vHat = fHits_vHat[k];       
	 Double32_t vHat = 1. - anode/240.;
	 if (hybrid == 1) vHat = - vHat;
	 if (hybrid == 1) vHat -= 0.1;
	 if (hybrid == 2) vHat += 0.1;
	 Double32_t zL = fHits_zL[k];
	 if (barrel <= 3) {zPL -= 23.5250; zL -= 23.5250;}
	 Double32_t xL = fHits_xL[k];
	 Double_t DxL = xL - xPL;
#if 0
	 Double32_t yL = fHits_yL[k];
	 Double32_t yPL = fHits_yPL[k];
	 Double_t DyL = yL - yPL;
#endif
	 Double_t DzL = zL - zPL;
	 Double32_t du = u - uP;
#if 0
	 if (barrel <= 3) du -= DriftCorHack(barrel, ladder, wafer, u);
#endif
	 Double32_t dv = v - vP;
	 if (TMath::Abs(fHits_pT[k]) < 0.2) continue;
	 //       if (TMath::Abs(du) > 0.5 || TMath::Abs(dv) > 0.5) continue;
	 if (TMath::Abs(du) > rCut || TMath::Abs(dv) > rCut) continue;
	 Int_t m = 0;
	 if (barrel > 3) m = 1;
	 if (TMath::Abs(xPG) > plotName[10].xmax[m] ||
	     TMath::Abs(yPG) > plotName[11].xmax[m] ||
	     TMath::Abs(zPG) > plotName[12].xmax[m]) continue;
	 if (TMath::Abs(uP) > Du[m] || TMath::Abs(vP) > Sv[m]/2) continue;
	 Double_t uA = TMath::Abs(u);
	 Double_t vA = TMath::Abs(v);
	 if (layer < 7) {
	   if (uMax > 0 && uA > uMax) continue;
	   if (uMin > 0 && uA < uMin) continue;
	   if (vMax > 0 && vA > vMax) continue;
	   if (vMin > 0 && vA < vMin) continue;
	 }
	 Double_t x = xPG;
	 Double_t y = yPG;
	 Double_t z = zPG;
	 Double_t jL2 = TMath::Sqrt(1. + tuP*tuP + tvP*tvP);
	 Double_t wG[3] = {fHits_wGu[k],fHits_wGv[k], fHits_wGw[k]};
	 Double_t vx =  fHits_wGu[k]*jL2;
	 Double_t vy =  fHits_wGv[k]*jL2;
	 Double_t vz =  fHits_wGw[k]*jL2;
	 Double_t vars[27][2] = {
	   {dX ,  x}, // 0
	   {dX ,  y},
	   {dX , -z},
	   {dY , -x},
	   {dY ,  y},
	   {dY ,  z},
	   {dZ ,  x},
	   {dZ , -y},
	   {dZ ,  z}, // 8 
	   {dX, (-1+dxP*vx) }, //  9 
	   {dX, (   dxP*vy) }, // 10
	   {dX, (   dxP*vz) },
	   {dX, (   dxP*(-vy*z+vz*y))},
	   {dX, (-z+dxP*( vx*z-vz*x))},
	   {dX, ( y+dxP*(-vx*y+vy*x))},
	   {dY, (   dyP*vx) }, // 15
	   {dY, (-1+dyP*vy) },
	   {dY, (   dyP*vz) },
	   {dY, ( z+dyP*(-vy*z+vz*y))},
	   {dY, (   dyP*( vx*z-vz*x))},
	   {dY, (-x+dyP*(-vx*y+vy*x))},
	   {dZ, (   dzP*vx) }, // 21
	   {dZ, (   dzP*vy) },
	   {dZ, (-1+dzP*vz) },
	   {dZ, (-y+dzP*(-vy*z+vz*y))},
	   {dZ, ( x+dzP*( vx*z-vz*x))},
	   {dZ, (   dzP*(-vx*y+vy*x))}
	 };
	 for (Int_t l = 0; l < 9; l++) {
	   GloPlots[l][sector]->Fill(vars[l][1],vars[l][0]);
	   if (LaddersInGlobal)
	     GloBLPlots[l][barrel-1][ladder-1]->Fill(vars[l][1],vars[l][0]);
	 }
	 Double_t vxyz[3] = {vx, vy, vz};
	 TRMatrix vR(3,1,vxyz);
	 Double_t dxyzP[3] = {dxP, dyP, dzP};
	 TRMatrix dR(3,1,dxyzP);
	 static TRMatrix UR(TRArray::kUnit,3);
	 TRMatrix AR(dR,TRArray::kAxBT,vR);// cout << "AR\t" << AR;
	 AR -= UR;// cout << "AR\t" << AR;
	 TRMatrix BR(3,6,
		     1. , 0., 0., 0.,  z,-y,
		     0.,  1., 0., -z, 0., x,
		     0.,  0., 1.,  y, -x, 0.);//  cout << "BR\t" << BR;
	 TRMatrix ABR(AR,TRArray::kAxB,BR);//    cout << "ABR\t" << ABR;
	 Double_t dxyz[3] = {dX, dY, dZ};
	 TRVector mX;
	 TRMatrix A(0,6);
	 for (UInt_t l = 0; l < 3; l++) {
	   if (l == 0 && TMath::Abs(wG[0]) >= 0.999 ||
	       l == 1 && TMath::Abs(wG[1]) >= 0.999) continue;
	   mX.AddRow(&dxyz[l]);
	   A.AddRow(ABR.GetRow(l));
	   for (UInt_t k = 0; k < 6; k++) {
	     UInt_t lk = k + 6*l + 9;
	     GloPlots[lk][sector]->Fill(ABR(l,k),dxyz[l]);
	     if (LaddersInGlobal)
	       GloBLPlots[lk][barrel-1][ladder-1]->Fill(ABR(l,k),dxyz[l]);
#if 0
	     if (TMath::Abs(vars[l][0]) < 1.e-3 || TMath::Abs(vars[l][1]) < 1.e-3) {
	       cout << "l\t" << l << "\tdX/dY/dZ = " << dX << "\t" << dY << "\t" << dZ << endl;
	       cout << "x/y/z(PG) = " << x << "\t" << y << "\t" << z << endl;
	       cout << "x/y/z( G) = " << xG << "\t" << yG << "\t" << zG << endl;
	       cout << "dirXYZ    = " << dxP << "\t" << dyP << "\t" << dzP << endl;
	       cout << "v  xyz    = " << vx  << "\t" << vy << "\t" << vz << endl;
	     }
#endif
	   }
	 }
	 TRVector AmX(A,TRArray::kATxB,mX);// cout << "AmX\t" << AmX << endl; 
	 TRSymMatrix SX(A,TRArray::kATxA);// cout << "SX\t" << SX << endl;
	 Double_t *array = LSF->GetArray();
	 Double_t *amX = AmX.GetArray();
	 Double_t *sX  = SX.GetArray();
	 Int_t im = 1 + 28*sector;
	 Int_t is = im + 6;
	 TCL::vadd(amX,array+im,array+im,6);
	 TCL::vadd(sX,array+is,array+is,21);
	 
         TRVector duv(2,du,dv);
	 TRMatrix P(2,6,
		    -1.,  0., tuP,    tuP*vP,      -tuP*uP,      vP,
		     0., -1., tvP,    tvP*vP,      -tvP*uP,     -uP);
	 TRVector pm(P,TRArray::kATxB,duv);
	 TRSymMatrix PX(P,TRArray::kATxA);
	 array = LSFB[barrel-1]->GetArray();
	 amX = pm.GetArray();
	 sX  = PX.GetArray();
	 im = 1 + 28*(ladder-1);
	 is = im + 6;
	 TCL::vadd(amX,array+im,array+im,6);
	 TCL::vadd(sX,array+is,array+is,21);
	 

	 Double32_t duOvertuP = du/tuP;
	 Double32_t dvOvertvP = dv/tvP;
#if 1
	 if (AllWafers) {
	   if (wafer == 0) {
	     LocPlots[0][barrel-1][ladder-1][wafer]->Fill(tuP,du);
	     LocPlots[1][barrel-1][ladder-1][wafer]->Fill(tvP,dv);
	     LocPlots[2][barrel-1][ladder-1][wafer]->Fill( vP,du);
	     LocPlots[3][barrel-1][ladder-1][wafer]->Fill(-uP,dv);
	     LocPlots[4][barrel-1][ladder-1][wafer]->Fill( vP,duOvertuP);
	     LocPlots[5][barrel-1][ladder-1][wafer]->Fill( vP,dvOvertvP);
	     LocPlots[6][barrel-1][ladder-1][wafer]->Fill(-uP,duOvertuP);
	     LocPlots[7][barrel-1][ladder-1][wafer]->Fill(-uP,dvOvertvP);
	     LocPlots[8][barrel-1][ladder-1][wafer]->Fill(-uP,du);
	     LocPlots[9][barrel-1][ladder-1][wafer]->Fill( vP,dv);
	   } else {
	     LocPlots[0][barrel-1][ladder-1][wafer]->Fill(tuP,du);
	     LocPlots[1][barrel-1][ladder-1][wafer]->Fill(tvP,dv);
	     LocPlots[2][barrel-1][ladder-1][wafer]->Fill( v ,du);
	     LocPlots[3][barrel-1][ladder-1][wafer]->Fill( u ,dv);
	     LocPlots[4][barrel-1][ladder-1][wafer]->Fill( v ,duOvertuP);
	     LocPlots[5][barrel-1][ladder-1][wafer]->Fill( v ,dvOvertvP);
	     LocPlots[6][barrel-1][ladder-1][wafer]->Fill( u ,duOvertuP);
	     LocPlots[7][barrel-1][ladder-1][wafer]->Fill( u ,dvOvertvP);
	     LocPlots[8][barrel-1][ladder-1][wafer]->Fill( uHat ,du);
	     LocPlots[9][barrel-1][ladder-1][wafer]->Fill( vHat ,du);
	   }
	 } else {
	   wafer = 1;
	   if (fHits_pT[k] < 0) wafer = 2;
	   LocPlots[0][barrel-1][ladder-1][wafer]->Fill(tuP,du);
	   LocPlots[1][barrel-1][ladder-1][wafer]->Fill(tvP,dv);
	   LocPlots[2][barrel-1][ladder-1][wafer]->Fill( zPL,du);
	   LocPlots[3][barrel-1][ladder-1][wafer]->Fill(-uP,dv);
	   LocPlots[4][barrel-1][ladder-1][wafer]->Fill( zPL,duOvertuP);
	   LocPlots[5][barrel-1][ladder-1][wafer]->Fill( zPL,dvOvertvP);
	   LocPlots[6][barrel-1][ladder-1][wafer]->Fill(-uP,duOvertuP);
	   LocPlots[7][barrel-1][ladder-1][wafer]->Fill(-uP,dvOvertvP);
	   LocPlots[8][barrel-1][ladder-1][wafer]->Fill(-uP,du);
	   LocPlots[9][barrel-1][ladder-1][wafer]->Fill( zPL,dv);
	 }
	 //       vP = vP + Sv[m]*(wafer - NoWafers/2 - 0.5);
#endif
	 wafer = 0;
	 Double32_t DxLOvertuP = DxL/tuP;
	 Double32_t DzLOvertvP = DzL/tvP;
	 LocPlots[0][barrel-1][ladder-1][wafer]->Fill(tuP,DxL);  // => dw
	 LocPlots[1][barrel-1][ladder-1][wafer]->Fill(tvP,DzL);  // => dw
	 LocPlots[2][barrel-1][ladder-1][wafer]->Fill( zPL,DxL); // => gamma
	 LocPlots[3][barrel-1][ladder-1][wafer]->Fill(-xPL,DzL); // => gamma (-)
	 LocPlots[4][barrel-1][ladder-1][wafer]->Fill( zPL,DxLOvertuP); //=> alpha
	 LocPlots[5][barrel-1][ladder-1][wafer]->Fill( zPL,DzLOvertvP); //=> alpha
	 LocPlots[6][barrel-1][ladder-1][wafer]->Fill(-xPL,DxLOvertuP); //=> beta (-)
	 LocPlots[7][barrel-1][ladder-1][wafer]->Fill(-xPL,DzLOvertvP); //=> beta (-)
	 LocPlots[8][barrel-1][ladder-1][wafer]->Fill(-xPL,DxL);        // 
	 LocPlots[9][barrel-1][ladder-1][wafer]->Fill( zPL,DzL);        //
       }
     }
   }
#if 0
   if (LSF) {
     for (Int_t s = 0; s < 6; s++) {
       Double_t *array = LSF->GetArray();
       Int_t im = 1 + 28*s;
       Int_t is = im + 6;
       cout << "sector " << s << "================================" << endl;
       TRVector AmX(6,array+im);  cout << "AmX " << AmX << endl;
       TRSymMatrix S(6,array+is);  cout << "S " << S << endl;
       TRSymMatrix SInv(S,TRArray::kInverted); cout << "SInv " << SInv << endl;
       TRVector  X(SInv,TRArray::kSxA,AmX); cout << "X " << X << endl;
     }
   }
#endif
   fOut->Write();
}
#ifndef __CINT__
//________________________________________________________________________________
void FillNt(HybridFit_t *HFit[4][20][16][2]) {
  if (! HFit) return;
  for (Int_t barrel = 1; barrel <= 4; barrel++) {
    for (Int_t ladder = 1; ladder <= 20; ladder++) {
      for (Int_t wafer = 1; wafer <= 16; wafer++) {
	for (Int_t hybrid = 1; hybrid <= 2; hybrid++) {
	  HybridFit_t *fit = HFit[barrel-1][ladder-1][wafer-1][hybrid-1];
	  if (! fit) continue;
	  if (fit->noentries < 100) continue;
	  cout << "B/L/W/H\t" << barrel << "/" << ladder << "/" << wafer << "/" << hybrid << endl;
	  cout << "AmX " << fit->AmX << endl;
	  cout << "S " << fit->S << endl;
	  TRSymMatrix SInv(fit->S,TRArray::kInverted); cout << "SInv " << SInv << endl;
	  TRVector  X(SInv,TRArray::kSxA,fit->AmX); cout << "X " << X << endl;
	  for (Int_t i = 0; i < 6; i++) {
	    cout << X(i) << " +/- " << TMath::Sqrt(SInv(i,i)) << endl;
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
void TT::MakeNt() {
  if (fChain == 0) return;  
  // Book 
   Long64_t nentries = fChain->GetEntriesFast();
   Long64_t nbytes = 0, nb = 0;
   Int_t TreeNo = -1;
   TString currentFile("");
   //                 B   l   W  H
   HybridFit_t  *HFit[4][20][16][2];
   memset(HFit, 0, 4*20*16*2*sizeof(HybridFit_t *));
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
     Long64_t ientry = LoadTree(jentry);
     if (ientry < 0) break;
     nb = fChain->GetEntry(jentry);   nbytes += nb;
     if (! jentry%1000 || TreeNo != fChain->GetTreeNumber()) {
       cout << "Read event \t" << jentry 
	    << " so far. switch to file " << fChain->GetCurrentFile()->GetName() << endl;
       if (TreeNo > -1) FillNt(HFit);
       TreeNo = fChain->GetTreeNumber();
     }
     if (VertexZCut > 0 && TMath::Abs(fVertex[2]) > VertexZCut) continue;
     UInt_t Ntrack = fNPTracks;     
     for (UInt_t trk = 0; trk < Ntrack; trk++) {
       Int_t Npoints = fTracks_fNpoint[trk];
       if (minNoFitPoints > 0 && Npoints%100 < minNoFitPoints) continue;
       if (UseSsd && Npoints < 1000) continue; 
       if (UseSvt && Npoints <  100) continue; 
       Int_t Nsp = fTracks_fNsp[trk];
       //       if (Nsp <= 0 || Nsp >= 10) continue;
       for (Int_t hit = 0; hit < Nsp; hit++) {
	 Int_t k = fTracks_fIdHitT[trk][hit];
	 //	for (UInt_t k = 0;  k < fNhit; k++) {
	 Int_t barrel = fHits_barrel[k];
	 Int_t layer  = fHits_layer[k];
	 if (layer < 7 && fHits_hitFlag[k] > 3) continue;
	 Int_t ladder = fHits_ladder[k];
	 Int_t wafer  = fHits_wafer[k];
	 Int_t hybrid = fHits_hybrid[k];
	 Double32_t u = fHits_u[k];       
	 Double32_t v = fHits_v[k];
	 Double32_t uP = fHits_uP[k];       
	 Double32_t vP = fHits_vP[k];
	 Double32_t du = u - uP;
	 Double32_t dv = v - vP;
	 if (TMath::Abs(fHits_pT[k]) < 0.2) continue;
	 //       if (TMath::Abs(du) > 0.5 || TMath::Abs(dv) > 0.5) continue;
	 if (TMath::Abs(du) > rCut || TMath::Abs(dv) > rCut) continue;
	 Double_t uA = TMath::Abs(u);
	 Double_t vA = TMath::Abs(v);
	 if (layer < 7) {
	   if (uMax > 0 && uA > uMax) continue;
	   if (uMin > 0 && uA < uMin) continue;
	   if (vMax > 0 && vA > vMax) continue;
	   if (vMin > 0 && vA < vMin) continue;
	 }
	 HybridFit_t *fit = HFit[barrel-1][ladder-1][wafer-1][hybrid-1];
	 if (! fit) {
	   HFit[barrel-1][ladder-1][wafer-1][hybrid-1] = fit = new HybridFit_t();
	   fit->noentries = 0;
	 }
	 fit->noentries++;
	 Double_t a[12] = {
	   1, u, v, 0, 0, 0,
	   0, 0, 0, 1, u, v
	 };
	 TRMatrix A(2,6,a); // cout << "A\t" << A << endl;
	 TRVector duv(2,du,dv);// cout << "duv\t" << duv << endl;
	 //	 cout << "fit->AmX\t" << fit->AmX << endl;
	 fit->AmX += TRVector(A,TRArray::kATxB,duv);
	 //	 cout << "fit->AmX\t" << fit->AmX << endl;
	 //	 cout << "fit->S\t" << fit->S << endl;
	 fit->S   += TRSymMatrix(A,TRArray::kATxA);
	 //	 cout << "fit->S\t" << fit->S << endl;
       }
     }
   }
   FillNt(HFit);
}
#endif
//________________________________________________________________________________
void TT::Loop4BadAnodes(Int_t Nevents) {
  //           B  l    W
  const Int_t B =  3;
  const Int_t L = 16;
  const Int_t W =  7;
  const Int_t H =  2;
  TFile *fOut = new TFile(fOutFileName,"recreate");
  TH1F *hists[B][L][W][H];
  memset(hists, 0, B*L*W*H*sizeof(TH1F *));
  for (Int_t barrel = 1; barrel <= B; barrel++) {
    Int_t nl = SvtSsdConfig[2*barrel-1].NoLadders;
    Int_t nw = SvtSsdConfig[2*barrel-1].NoWafers;
    for (Int_t ladder = 1; ladder <= nl; ladder++) 
      for (Int_t wafer = 1; wafer <= nw; wafer++) 
	for (Int_t hybrid = 1; hybrid <= H; hybrid++) {
	  hists[barrel-1][ladder-1][wafer-1][hybrid-1] = 
	    new TH1F(Form("B%iL%02iW%iH%i",barrel,ladder,wafer,hybrid),
		     Form("Anode for Barrel %i Ladder %i, Wafer %i Hybrid %i",
			  barrel,ladder,wafer,hybrid),
		     240,0,240);
      }
  }
  Long64_t nentries = fChain->GetEntriesFast();
  if (Nevents > 0 && nentries > Nevents) nentries = Nevents;
  Long64_t nbytes = 0, nb = 0;
  Int_t TreeNo = -1;
  TString currentFile("");
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    if (! jentry%1000 || TreeNo != fChain->GetTreeNumber()) {
      if (jentry > 0) fOut->Flush();
      cout << "Read event \t" << jentry 
	   << " so far. switch to file " << fChain->GetCurrentFile()->GetName() << endl;
      TreeNo = fChain->GetTreeNumber();
    }
    for (UInt_t k = 0; k < fNhit; k++) {
      Int_t barrel = fHits_barrel[k];
      if (barrel < 1 || barrel > 3) continue;
      Int_t ladder = fHits_ladder[k];
      Int_t wafer  = fHits_wafer[k];
      Int_t hybrid = fHits_hybrid[k];
      TH1F *hist = hists[barrel-1][ladder-1][wafer-1][hybrid-1];
      if (! hist) continue;
      Double32_t anode = fHits_anode[k];
      hist->Fill(anode);
    }
  }
  fOut->Write();
}
#if 0
//________________________________________________________________________________
void T::LoopTB(Int_t Nevents) {
  struct PlotName_t {
    Char_t *Name;
    Char_t *Title;
    Int_t    nx;
    Int_t    ny;
    Double_t xmin; 
    Double_t xmax; 
    Double_t ymin;
    Double_t ymax;  
    Double_t zmin;
    Double_t zmax; 
  };
  const  PlotName_t plotNameTB = // plots for time bins and anodes
    { "timeB","time for 80 anodes", 128, 3, 0,128, 0,3, 0,0 };

  TFile *fOut = new TFile(fOutFileName,"recreate");
  
  TString Name, Title;
  const Int_t NB =  3;
  const Int_t NL = 16;
  const Int_t NW =  7;
  const Int_t NH =  2;
  const Int_t NA =  3;
  //              B   L   W   H   A
  TH1F *LocPlots[NB][NL][NW][NH][NA];
  TH1F * LocAll = new TH1F("All","all", plotNameTB.nx, plotNameTB.xmin, plotNameTB.xmax);
  TH1F * uAll = new TH1F("Uall","ua", 200, -5., 5.);
  TH1F * uCut = new TH1F("Ucut","uc", 200, -3., 3.);
  TH1F * vCut = new TH1F("Vcut","vc", 200, -3., 3.);
   memset(LocPlots,0,NB*NL*NW*NH*sizeof(TH2F *));
   for (Int_t L = 0; L < 6; L++) {// over Layers
     Int_t barrel    = SvtSsdConfig[L].Barrel;
     Int_t layer     = SvtSsdConfig[L].Layer;
     Int_t NoLadders = SvtSsdConfig[L].NoLadders;
     Int_t NoWafers  = SvtSsdConfig[L].NoWafers;
     for (Int_t ladder = 1; ladder <= NoLadders; ladder++) {
       if (barrel <= 3 && (ladder-1)%2 != layer%2) continue;
       for (Int_t wafer = 1; wafer <= NoWafers; wafer++) {// wafer == 0 for whole ladder
	 for (Int_t hybrid = 1; hybrid <= 2; hybrid++) {
	   for (Int_t anode =1; anode <=3; anode++) {
	     Name = plotNameTB.Name;
	     Name += Form("L%02iB%iW%02iH%iA%i", ladder, barrel, wafer, hybrid, anode);
	     Title = Form("%s for layer %i B %i L %i W %i H %i G %i",
			  plotNameTB.Title, layer, barrel, ladder, wafer, hybrid, anode);
	     LocPlots[barrel-1][ladder-1][wafer-1][hybrid-1][anode-1] = 
	       new TH1F(Name, Title, plotNameTB.nx, plotNameTB.xmin, plotNameTB.xmax );
           }	   
	 }
       }
     }
   }
   Long64_t nentries = fChain->GetEntriesFast();
   if (Nevents > 0 && nentries > Nevents) nentries = Nevents;
   Long64_t nbytes = 0, nb = 0;
   Int_t TreeNo = -1;
   TString currentFile("");

   for (Long64_t jentry=0; jentry<nentries;jentry++) {
     Long64_t ientry = LoadTree(jentry);
     if (ientry < 0) break;
     nb = fChain->GetEntry(jentry);   nbytes += nb;
     if (! jentry%1000 || TreeNo != fChain->GetTreeNumber()) {
       cout << "Read event \t" << jentry 
	    << " so far, switch to file " << fChain->GetCurrentFile()->GetName() << endl;
       TreeNo = fChain->GetTreeNumber();
     }
//     if (VertexZCut > 0 && TMath::Abs(fVertex[2]) > VertexZCut) continue;
     UInt_t Ntrack = fNPTracks;     
     for (UInt_t trk = 0; trk < Ntrack; trk++) {
       Int_t Npoints = fTracks_fNpoint[trk];
       if (minNoFitPoints > 0 && Npoints%100 < minNoFitPoints) continue;
       if (UseSsd && Npoints < 1000) continue; 
       if (UseSvt && Npoints <  100) continue; 
       Int_t Nsp = fTracks_fNsp[trk];
       for (Int_t hit = 0; hit < Nsp; hit++) {
	 Int_t k = fTracks_fIdHitT[trk][hit] - 1;
	 assert(k>=0);
	 Int_t barrel = fHits_barrel[k];
	 Int_t layer  = fHits_layer[k];
	 Int_t ladder = fHits_ladder[k];
	 Int_t wafer  = fHits_wafer[k];
	 Int_t hybrid = fHits_hybrid[k];

	 if (layer <= 6 ){
	   Double32_t u = fHits_u[k];       
	   Double32_t v = fHits_v[k];
	   Double32_t uP = fHits_uP[k];       
	   Double32_t vP = fHits_vP[k];
	   Double32_t du = u - uP;
	   Double32_t dv = v - vP;
	   if (TMath::Abs(fHits_pT[k]) < 0.2) continue;
	   uCut->Fill(du);
	   if (TMath::Abs(du) > rCut ) continue;
	   vCut->Fill(dv);
	   if (TMath::Abs(dv) > rCut) continue;
	   Double32_t anode = fHits_anode[k];
	   Double32_t timeb = fHits_timeb[k];
	   Int_t   group = ((Int_t)anode)/80;
	   if (group >= 3) group = 2;
	   if (group <  0) group = 0;
	   LocPlots[barrel-1][ladder-1][wafer-1][hybrid-1][group]->Fill( timeb );
           LocAll->Fill( timeb );
           uAll->Fill( u );
	 }
       }
     }
   } 
	fOut->Write();
}
#endif

//________________________________________________________________________________
Int_t TT::IsNotValidHybrid(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Int_t run, Double_t anode) {
  Int_t iok = 0; 
  if (anode <= 10 || anode >= 230) {iok =  1; goto ENDL;}
#if 0
#if !defined(__CINT__)
  struct Hybrids_t {
    Char_t  hybrid[10];
    Char_t  run[5];
    Int_t   npeaks; // -88 noise only, -99 dead
    Double_t mu0, sigma0;
    Double_t mu1, sigma1;
    Double_t mu2, sigma2;
    Double_t mu3, sigma3;
    Double_t mu4, sigma4;
    Double_t mu5, sigma5;
    Double_t mu6, sigma6;
    Double_t mu7, sigma7;
    Double_t mu8, sigma8;
    Double_t mu9, sigma9;
  };
  static const Hybrids_t Hybrids[] = {
  };
  static const UInt_t NoHybrids = sizeof(Hybrids)/sizeof(Hybrids_t);
  static TString oldHybrid("");
  static TString oldRun("");
  static Bool_t InitDone = kFALSE;
  static const Int_t NB = 3;
  static const Int_t NL =12;
  static const Int_t NW = 7;
  static const Int_t NH = 2;
  static const Int_t ND = 3;
  static Hybrids_t *ptr[NB][NL][NW][NH][ND];
  if (! InitDone) {
    InitDone = kTRUE;
    memset(ptr, 0, NB*NL*NW*NH*ND*sizeof(Hybrids_t *));
    Int_t b, l, w, h, d, day;
    for (UInt_t k = 0; k < NoHybrids; k++) {
      sscanf(&Hybrids[k].hybrid[0],"B%iL%02iW%iH%i",&b,&l,&w,&h);
      if (b > 0 && b <= NB && l > 0 && l <= NL && w > 0 && w <= NW && h > 0 && h <= NH) {
	sscanf(&Hybrids[k].run[0],"0%2iD",&day);
        d = 0;
	if (day == 49) d = 1;
	if (day == 69) d = 2;
	ptr[b-1][l-1][w-1][h-1][d-1] = (Hybrids_t *) &Hybrids[k];
      }
    }
  }
  Int_t day = (run/1000)%1000; 
  Int_t d = 0;
  Hybrids_t *p = 0;
  Int_t k;
  Double_t *peaks = 0;
  if (day == 49) d = 1;
  if (day == 69) d = 2;
  if (barrel > 0 && barrel <= NB && ladder > 0 && ladder <= NL && wafer > 0 && wafer <= NW && hybrid > 0 && hybrid <= NH && d < ND) {
    p = ptr[barrel-1][ladder-1][wafer-1][hybrid-1][d-1];
    if (! p) {iok =  4; goto ENDL;}
    if (p->npeaks < 0) {iok =  2; goto ENDL;}
    peaks = (Double_t *) &p->mu0;
    for (k = 0; k < p->npeaks; k++) {
      if (TMath::Abs(anode-peaks[2*k]) < 3*peaks[2*k+1]) {iok =  3; goto ENDL;}
    }
  }
#endif
#endif
 ENDL:
  return iok;
}  
