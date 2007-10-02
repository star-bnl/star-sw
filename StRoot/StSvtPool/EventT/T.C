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
          RotateZ(beta) =   (gamma      1     0)
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
     if (! AllWafers) NoWafers = 2; // use wafer index for Positive / negatives
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
	 //Run V	 if (layer < 7 && IsNotValidHybrid(barrel,ladder,wafer,hybrid,run,anode)) continue;
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
	 wafer = 0;
	 Double32_t DxLOvertuP = DxL/tuP;
	 Double32_t DzLOvertvP = DzL/tvP;
	 LocPlots[0][barrel-1][ladder-1][wafer]->Fill(tuP,DxL);
	 LocPlots[1][barrel-1][ladder-1][wafer]->Fill(tvP,DzL);
	 LocPlots[2][barrel-1][ladder-1][wafer]->Fill( zPL,DxL);
	 LocPlots[3][barrel-1][ladder-1][wafer]->Fill(-xPL,DzL);
	 LocPlots[4][barrel-1][ladder-1][wafer]->Fill( zPL,DxLOvertuP);
	 LocPlots[5][barrel-1][ladder-1][wafer]->Fill( zPL,DzLOvertvP);
	 LocPlots[6][barrel-1][ladder-1][wafer]->Fill(-xPL,DxLOvertuP);
	 LocPlots[7][barrel-1][ladder-1][wafer]->Fill(-xPL,DzLOvertvP);
	 LocPlots[8][barrel-1][ladder-1][wafer]->Fill(-xPL,DxL);
	 //	 LocPlots[9][barrel-1][ladder-1][wafer]->Fill( zPL,DzL);
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
#if !defined(__CINT__)
  struct Hybrids_t {
    Char_t  hybrid[10];
    Char_t  run[5];
    Int_t   npeaks;
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
    {"B1L01W1H1","021D",  2,   8.83, 0.25, 229.29, 1.05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W1H1","049D",  2,   8.83, 0.25, 229.29, 1.05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B1L01W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W1H1","069D",    -99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead

    {"B1L01W1H2","021D",  3,   0.90, 0.30,  14.23, 0.65, 224.22, 1.46,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W1H2","049D",  3,   0.90, 0.30,  14.23, 0.65, 224.22, 1.46,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B1L01W1H2","049D", -7,  14.16, 0.70,  64.50,20.00, 125.50,18.00, 153.50, 7.05, 173.27, 6.66, 203.12, 9.84, 223.93, 1.49,0,0,0,0,0,0},
    {"B1L01W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L01W2H1","021D",  2,   3.75, 0.44,12.96, 0.25,      0,    0,      0,    0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W2H1","049D",  4,  12.96, 0.25,71.52, 5.22, 194.40, 0.83, 239.51, 0.01,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W2H1","069D",-99,      0,    0,    0,    0,      0,    0,      0,    0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L01W2H2","021D",-88,   5.60, 0.16,  21.45, 0.84, 229.71, 0.45,227,4,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    //noise only    {"B1L01W2H2","049D",  5,   5.60, 0.16,  53.78, 0.71, 140.09, 0.78,227,4,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L01W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L01W3H1","021D",  3,   0.65, 0.35, 205.23, 1.95, 220.09, 2.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, //  
    //  {"B1L01W3H1","049D",  3,   0.65, 0.35, 205.23, 1.95, 220.09, 2.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, //  
    {"B1L01W3H1","049D", -7,   0.00, 1.24,  59.33, 5.19,  76.42, 3.28, 111.50,20.00, 147.33, 2.49, 168.51, 6.32, 192.37, 1.76,0,0,0,0,0,0}, // noisy
    {"B1L01W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L01W3H2","021D",  2, 116.96, 0.95, 240.00, 0.98,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, 
    {"B1L01W3H2","049D",  4,  12.90, 0.25, 117.11, 1.01, 226.15, 0.65, 239.45, 0.10,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W3H2","069D",  3,  12.20, 0.60, 117.10, 1.02, 226.46, 0.96,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L01W4H1","021D",  1,   7.81, 0.25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W4H1","049D",  1,   7.50, 0.06,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W4H1","069D",  1,   7.59, 0.37,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L01W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L01W4H2","069D",  1, 189.99, 1.46,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L02W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L02W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L02W1H1","069D",  2,   0.52, 0.03, 239.81, 0.24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L02W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L02W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L02W1H2","069D",  1,   8.52, 0.17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L02W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L02W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L02W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L02W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, //-6,  20.50, 2.86,  33.50,20.00,  88.50,20.00, 139.50,20.00, 174.50,20.00, 221.50,20.00,0,0,0,0,0,0,0,0},
    {"B1L02W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, 
    {"B1L02W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L02W3H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// only noise
    {"B1L02W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// probably missed 
    {"B1L02W3H1","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    //
    {"B1L02W3H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// only noise
    {"B1L02W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// probably missed 
    {"B1L02W3H2","069D",-88, 233.54, 0.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    //
    {"B1L02W4H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    {"B1L02W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// propably missed
    {"B1L02W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// propably missed
    //
    {"B1L02W4H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    {"B1L02W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// propably missed
    {"B1L02W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// propably missed
    //
    {"B1L03W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W1H1","069D",  1,  10.50, 0.04,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L03W1H2","021D",  2, 156.79, 0.36, 168.67, 0.46,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W1H2","049D",  2, 156.59, 0.47, 236.50, 1.23,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L03W2H1","021D",  2, 203.51, 0.18, 239.52, 0.19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B1L03W2H1","069D", -8,  31.50,20.00, 103.50,20.00, 135.50, 6.94, 155.50, 8.05, 178.50,11.56, 203.66, 7.73, 221.50, 4.90, 239.51, 0.15,0,0,0,0},
    //
    {"B1L03W2H2","021D",  1,   9.50,11.33,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W2H2","049D",  1, 231.50,12.54,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, //  <<<<<<<<
    //  {"B1L03W2H2","069D", -3,   2.50,20.00,   7.55, 0.04, 230.50,15.75,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W2H2","069D",  1, 231.50,12.54,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, //  <<<<<<<<
    //
    {"B1L03W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
#if 0
    {"B1L03W3H2","021D", -3,  90.62, 5.35, 112.50, 7.21, 145.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W3H2","049D", -4,  88.74, 4.82, 110.50,10.83, 127.24, 0.27, 153.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0}, //  <<<<<<<<
    {"B1L03W3H2","069D", -4,  90.11, 5.10, 110.22, 7.15, 129.50, 7.34, 164.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0},
#else
    {"B1L03W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
#endif
    {"B1L03W4H1","021D",  1, 239.42, 0.16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B1L03W4H1","049D", -6,  82.96, 2.15, 105.41, 6.60, 124.11, 9.66, 142.20, 6.01, 158.50, 8.86, 192.50,20.00,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B1L03W4H1","049D",  1,  82.96, 2.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B1L03W4H1","069D",  1, 239.54, 0.04,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L03W4H2","021D",  1, 190.49, 0.11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B1L03W4H2","049D", -3,  89.50,20.00, 187.50,20.00, 227.79, 0.92,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    //  {"B1L03W4H2","069D", -2, 103.50,20.00, 198.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W4H2","049D",  1,  40.00,16.00, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L03W4H2","069D",  1,  40.00,16.00, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  
    {"B1L04W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L04W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B1L04W1H2","069D",  2, 177.56, 0.01, 210.60, 0.44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W1H2","069D",  1, 210.60, 0.44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L04W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L04W2H2","021D",  2,   0.69, 0.32, 239.73, 1.28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W2H2","049D",  2,   0.31, 0.44, 239.79, 0.47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // bands
    {"B1L04W2H2","069D",  2,   0.67, 0.33, 237.50,13.81,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L04W3H1","021D",  1,   0.17, 0.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W3H1","069D",  1,   0.56, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L04W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W3H2","069D",  1,   0.41, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L04W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W4H1","069D",  2,  58.11, 0.33, 239.49, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
#if 0
    {"B1L04W4H2","021D", -4,   9.75, 2.29,  88.50, 5.01, 112.50,11.12, 153.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W4H2","049D", -4,  10.11, 2.31,  87.50, 4.78, 117.50,13.32, 153.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W4H2","069D", -4,   9.17, 2.81,  89.50, 5.82, 118.50,15.05, 172.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0},
#else
    {"B1L04W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L04W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
#endif
    {"B1L05W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L05W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    //
    {"B1L05W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W2H1","049D",  3,   9.92, 1.13,  69.50, 5.21, 239.49, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    //
    {"B1L05W2H2","021D",  1,   0.52, 0.02,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W2H2","049D",  1,   0.53, 0.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    //
    {"B1L05W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L05W3H2","021D",  2,  10.21, 1.92, 239.69, 0.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W3H2","049D",  3,  10.37, 2.42, 189.50,17.12, 239.49, 0.02,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B1L05W3H2","069D",  2,  10.12, 1.53, 239.82, 0.24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L05W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B1L05W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L05W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L06W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L06W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W1H2","069D",  2, 101.14, 0.25, 232.01, 0.90,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L06W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W2H1","069D",  1,  78.78, 0.95,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L06W2H2","021D",  1, 189.65, 0.41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W2H2","049D",  1, 189.59, 0.39,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W2H2","069D",  3,  22.04, 0.28, 105.90, 0.27, 189.50, 0.07,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L06W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L06W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L06W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L06W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L06W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L07W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L07W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L07W1H2","021D",  2,   0.42, 0.05, 239.41, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L07W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L07W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L07W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L07W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L07W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    //
    {"B1L07W3H1","021D",  1,  86.49, 0.49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W3H1","049D",  2,  86.49, 0.49, 118.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B1L07W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L07W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W3H2","069D",  2, 228.60, 2.62, 240.00, 4.28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L07W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B1L07W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B1L07W4H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L07W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L07W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L08W1H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L08W1H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L08W2H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L08W2H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L08W3H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W3H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W3H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L08W3H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W3H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L08W4H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B1L08W4H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B1L08W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L01W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // non linear dependence
    {"B2L01W1H1","069D",  1, 239.38, 0.11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L01W1H2","021D",  1, 239.40, 0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // non linear dependence
    {"B2L01W1H2","069D",  2,   0.51, 0.01,  39.70, 0.11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L01W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // non linear dependence
    {"B2L01W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L01W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // non linear dependence
    {"B2L01W2H2","069D",  1, 155.48, 0.20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L01W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // non linear dependence
    {"B2L01W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L01W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // non linear dependence
    {"B2L01W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
#if 0
    {"B2L01W4H1","021D",  2,  15.04, 0.29, 239.41, 0.06,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W4H1","049D",  2, 162.44, 0.06, 239.49, 0.02,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
#else
    {"B2L01W4H1","021D",-88, 161.08, 1.21, 239.59, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L01W4H1","049D",-88, 161.08, 1.21, 239.59, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
#endif
    {"B2L01W4H1","069D",-88, 161.08, 1.21, 239.59, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    
    {"B2L01W4H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W4H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L01W4H2","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    
    {"B2L01W5H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W5H1","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L01W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L01W5H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L01W5H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L01W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L01W6H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L01W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L01W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L01W6H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L01W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L01W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L02W1H1","021D",  1,  39.87, 1.55,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B2L02W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L02W1H1","049D",  1,  39.87, 1.55,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W1H1","069D",  1,  39.38, 0.92,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L02W1H2","021D", -5,  88.77, 5.13, 107.50, 9.17, 127.50, 9.79, 169.50,20.00, 219.64, 0.52,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W1H2","049D", -3,  90.50,20.00, 165.50,20.00, 217.65,10.81,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W1H2","069D", -4,  90.50,17.39, 129.50,16.63, 169.50,20.00, 220.34,20.00,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L02W2H1","021D",  1,  22.50, 0.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W2H1","049D",  1,  22.51, 0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W2H1","069D",  1,  22.54, 0.32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L02W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L02W3H1","021D",  1, 129.54, 0.95,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W3H1","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<noise only
    {"B2L02W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L02W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W3H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L02W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L02W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L02W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L02W5H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only 
    {"B2L02W5H1","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // noise only
    {"B2L02W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//probaly missed
    
    {"B2L02W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W5H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W5H2","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    
    {"B2L02W6H1","021D", -6,  47.63, 4.31,  59.67, 1.33,  81.50,20.00, 130.05,13.32, 167.77,18.76, 186.50, 4.61,0,0,0,0,0,0,0,0},
    {"B2L02W6H1","049D", -3,  49.50, 3.30,  80.50,20.00, 166.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W6H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B2L02W6H2","021D", -6,  37.57, 6.24,  51.50, 2.50,  80.50,20.00, 156.50,20.00, 182.50, 6.25, 212.50,15.64,0,0,0,0,0,0,0,0},
    {"B2L02W6H2","049D", -3, 141.50,20.00, 186.01, 7.52, 211.80, 2.70,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L02W6H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L03W1H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W1H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W2H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W2H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L03W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L03W3H1","021D",  1,  12.45, 0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W3H1","049D",  1,  12.48, 0.05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W3H1","069D",  1,  12.50, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L03W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L03W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L03W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L03W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L03W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B2L03W5H2","049D",  2, 150.06, 0.29, 207.26, 0.70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L03W5H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W5H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    //  {"B2L03W6H1","021D", -7,  21.50,10.07,  65.50,20.00, 105.50,12.22, 131.50,11.54, 170.07,20.00, 214.45,11.09, 239.78, 0.53,0,0,0,0,0,0},
    {"B2L03W6H1","021D",  2,   0.32, 0.17, 239.61, 0.20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W6H1","049D",  2,   9.90, 0.24, 239.50, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L03W6H1","069D",  2,   0.32, 0.17, 239.61, 0.20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L03W6H2","021D",  1, 232.89, 0.86,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W6H2","049D",  1, 233.47, 0.52,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L03W6H2","069D",  1, 233.51, 0.50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<< where 021D ?
    {"B2L04W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W2H1","021D",  2, 152.70, 0.54, 238.67, 1.84,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W2H1","049D",  3, 129.75, 0.12, 152.52, 0.07, 232.05, 3.23,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L04W2H1","069D",  1, 152.44, 0.11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W2H2","021D", -3,  83.62, 1.70, 104.50, 8.71, 141.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W2H2","049D",  1, 109.50,18.33,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W2H2","069D",  2,  89.09, 4.85, 127.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W3H1","021D",  1, 230.73, 0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W3H1","049D",  1, 230.59, 0.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W4H1","049D",  1, 232.45, 0.93,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W4H1","069D",  1, 232.93, 0.97,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W5H2","021D",  1, 103.77, 2.63,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W5H2","049D",  1, 103.70, 2.53,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W5H2","069D",  1, 103.68, 2.30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W6H1","021D",  1, 231.39, 1.77,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B2L04W6H1","049D",  2, 200.51, 1.41, 231.15, 1.65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L04W6H1","049D",  1, 231.15, 1.65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W6H1","069D",  1, 231.28, 1.67,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L04W6H2","021D",  1,  69.53, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B2L04W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L04W6H2","049D",  1,  69.53, 0.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L04W6H2","069D",  1,  69.53, 0.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L05W1H1","021D",  3,  67.01, 0.58, 113.27, 0.82, 239.40, 8.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B2L05W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L05W1H1","049D",  1,  67.01, 0.58,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L05W1H1","069D",  1, 204.91, 6.34,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L05W1H2","021D", -3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W1H2","049D", -3,  88.50, 5.39, 128.50,17.68, 160.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L05W1H2","069D",  2, 104.50,14.84, 185.03, 4.33,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B2L05W2H1","021D",  2,  64.99, 0.02, 240.00,13.66,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L05W2H1","069D",  1, 222.61, 0.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //
    {"B2L05W2H2","021D",  3,   0.60, 0.06, 218.50,20.00, 239.46, 0.36,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W2H2","049D",  2,   0.53, 0.03, 239.49, 0.02,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L05W2H2","069D",  4,  26.19, 4.97,  65.12, 1.26, 188.84, 4.12, 239.73, 0.45,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L05W3H1","021D",  1, 240.00, 9.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L05W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L05W3H2","021D",  3,   0.27, 8.76, 124.76, 1.55, 239.86, 0.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L05W3H2","049D",  2,   0.31, 0.78, 124.94, 0.23,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W3H2","069D",  2,   0.26, 0.84, 109.50,15.95,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L05W4H1","021D",  2, 213.79, 1.52, 238.85, 0.70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W4H1","049D",  1, 238.79, 0.66,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // bands
    {"B2L05W4H1","069D",  2, 213.88, 1.88, 238.85, 0.61,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L05W4H2","021D",  3,  15.72, 0.96, 204.03, 2.98, 230.97, 8.60,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W4H2","049D",  5,   0.49, 0.01,  14.58, 1.43, 176.01, 0.31, 203.92, 2.35, 229.85, 6.81,0,0,0,0,0,0,0,0,0,0}, // bands
    {"B2L05W4H2","069D",  2, 204.11, 2.33, 233.11, 8.75,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L05W5H1","021D", -5,   8.90, 1.95,  73.55, 5.66, 104.02, 4.43, 208.50,20.00, 240.00, 0.68,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W5H1","049D",-88,   5.50, 2.29,  23.89, 4.51, 109.39, 0.17, 119.50,20.00, 163.67, 1.43, 200.00, 4.18,0,0,0,0,0,0,0,0},//nose only
    {"B2L05W5H1","069D", -7,   4.51, 2.96,  23.32, 5.13,  82.40, 1.04, 106.02, 2.16, 121.50,20.00, 162.15, 0.66, 199.27, 4.32,0,0,0,0,0,0},
    //
    {"B2L05W5H2","021D",  4,   0.69, 0.39, 130.85, 0.81, 168.16, 3.98, 195.01, 0.31,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W5H2","049D",-88, 103.66, 2.47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  noise only 
    {"B2L05W5H2","069D",-88, 103.96, 2.29, 214.40, 4.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    
    {"B2L05W6H1","021D",  1,  12.50, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W6H1","049D",-88, 231.15, 1.63,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  noise only 
    {"B2L05W6H1","069D",-88, 231.27, 1.67,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    
    {"B2L05W6H2","021D",  2,  69.61, 0.18, 144.12, 3.34,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L05W6H2","049D",-88,  69.61, 0.18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  noise only 
    {"B2L05W6H2","069D",-88,  69.49, 0.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    
    {"B2L06W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // very strange shape for half ladder 

    {"B2L06W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, 

    {"B2L06W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L06W3H1","021D",  1, 239.49, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W3H1","069D",  1, 239.49, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L06W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L06W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L06W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L06W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L06W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L06W5H1","021D", -9,  13.11,10.24,  26.66, 3.00,  39.50, 4.96,  53.45, 3.09,  70.99, 5.10, 100.81,15.82, 139.49,16.26, 164.40, 4.51, 182.06, 2.96,0,0},
    {"B2L06W5H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L06W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L06W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L06W5H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L06W6H1","021D",  1, 147.73, 1.78,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L06W6H1","069D",  3, 104.64, 0.07, 147.53, 1.88, 172.60, 0.59,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L06W6H2","021D", -3,  87.24, 4.48, 111.50,11.30, 155.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L06W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L06W6H2","069D", -5,  87.02, 4.55, 110.50,11.59, 132.74, 5.88, 145.98, 5.23, 178.50,20.00,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L07W1H1","021D",  1, 179.53, 0.85,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W1H1","049D",  1, 178.63, 0.77,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W1H1","069D",  1, 178.63, 0.75,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L07W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W1H2","069D",  1, 180.95, 0.65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L07W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L07W3H2","021D",  1,  71.66, 0.44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W3H2","049D",  1,  71.66, 0.44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W3H2","069D",  1,  71.66, 0.44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B2L07W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B2L07W4H1","021D", -4,  85.81, 3.86, 103.50, 8.45, 127.50,17.31, 184.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W4H1","049D", -2,  88.50, 6.67, 143.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L07W4H2","021D",  2,  71.06, 1.62, 129.67, 0.43,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W4H2","049D",  2,  70.70, 1.34, 129.66, 0.42,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L07W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L07W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W5H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L07W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    //  {"B2L07W6H2","021D", -2,  91.82, 6.26, 131.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L07W6H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L07W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L07W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L08W1H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L08W1H1","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  noise only 
    {"B2L08W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B2L08W1H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L08W1H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  noise only 
    {"B2L08W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B2L08W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L08W2H1","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  noise only 
    {"B2L08W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B2L08W2H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L08W2H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  noise only 
    {"B2L08W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B2L08W3H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L08W3H1","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  noise only 
    {"B2L08W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L08W3H2","021D",-88, 180.17, 1.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// noise only
    {"B2L08W3H2","049D",-88, 165.50, 0.04,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, //  noise only // where 069D
    {"B2L08W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L08W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L08W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L08W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L08W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L08W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L08W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L08W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//funny shape
    {"B2L08W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L08W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L08W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//fanny shape
    {"B2L08W5H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L08W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L08W6H1","021D",  1, 190.53, 0.39,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//fanny shape
    {"B2L08W6H1","049D",  1, 190.50, 0.04,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // for whole ladder problem for maximum drift
    {"B2L08W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L08W6H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//fanny shape
    {"B2L08W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L08W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L09W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L09W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W1H2","069D",  1, 191.17, 2.33,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L09W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L09W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W2H2","069D",  1, 233.13, 0.48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L09W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L09W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L09W4H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L09W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L09W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L09W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L09W5H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L09W5H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L09W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L09W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W5H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L09W6H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L09W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L09W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L09W6H2","021D", -2,  40.40, 5.76, 225.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L09W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B2L10W1H1","021D", -2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W1H1","049D", -2,  90.50, 6.54, 175.50, 8.45,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W1H1","069D", -2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B2L10W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W1H2","069D",  2,  21.82, 0.45, 224.95, 0.26,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L10W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    //  {"B2L10W2H2","021D", -7,   0.55, 0.06,  23.24,12.14,  46.64, 7.27,  75.50,20.00, 145.50,20.00, 180.50, 6.55, 221.95, 8.59,0,0,0,0,0,0},
    {"B2L10W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L10W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L10W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L10W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L10W3H1","029D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    //  {"B2L10W3H1","069D", -6,  29.50, 8.21,  52.19, 1.08,  94.55, 5.83, 111.86, 6.75, 124.50, 1.96, 211.01, 0.84,0,0,0,0,0,0,0,0},
    
    {"B2L10W3H2","021D", -3,  21.50,20.00,  69.50, 7.43, 205.29,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W3H2","049D", -5,  17.50,20.00,  38.50,17.05,  68.06, 6.95, 205.50,20.00, 239.59, 0.22,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W3H2","069D", -4,   0.21, 0.22,  69.50, 6.96, 208.50,20.00, 240.00, 0.49,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L10W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L10W4H2","021D",  1, 226.37, 1.68,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W4H2","049D",  1, 226.71, 1.79,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W4H2","069D",  1, 226.83, 1.72,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    //  {"B2L10W5H1","021D", -5,  21.50, 9.62,  56.50,10.80,  73.50, 3.07, 169.50, 7.97, 189.46, 0.42,0,0,0,0,0,0,0,0,0,0},
    //  {"B2L10W5H1","049D", -3,  20.50,20.00, 168.50, 6.24, 189.47, 0.41,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W5H1","021D",  1, 189.50, 0.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L10W5H1","049D",  1, 189.50, 0.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L10W5H1","069D",  1, 189.50, 0.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B2L10W5H2","021D",  1,   5.50, 0.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W5H2","049D",  1,   5.53, 0.06,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W5H2","069D",  1,   5.46, 0.16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L10W6H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B2L10W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W6H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L10W6H2","021D",  2,   2.50, 0.07,  97.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W6H2","049D",  1,   2.48, 0.06,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L10W6H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B2L11W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// fanny shape for whole ladder
    {"B2L11W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// very strange shape for whole ladder
    {"B2L11W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// very strange shape for whole ladder
    
    {"B2L11W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W1H2","049D",  1, 215.62, 0.93,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W1H2","069D",  1, 215.51, 0.88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L11W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L11W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L11W3H1","021D",  1, 176.01, 0.27,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L11W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L11W3H2","021D",  1, 160.46, 2.74,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W3H2","049D",  1, 160.60, 2.48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W3H2","069D",  1, 160.45, 2.39,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L11W4H1","021D",  2, 109.51, 0.07, 138.45, 0.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W4H1","049D",  2, 109.41, 0.12, 138.51, 0.06,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W4H1","069D",  2, 109.50, 0.09, 138.49, 0.05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L11W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W4H2","069D",  1, 202.49, 0.04,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
#if 0
    {"B2L11W5H1","021D", -3,  89.46, 5.48, 109.50, 9.07, 147.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W5H1","049D",  1, 108.50,17.66,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L11W5H1","069D", -2,  97.29, 9.46, 143.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
#else
    {"B2L11W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
#endif
    {"B2L11W5H2","021D",  1,  10.84, 0.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W5H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W5H2","069D",  1,  10.79, 0.21,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L11W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W6H1","049D",  1, 209.67, 0.56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B2L11W6H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L11W6H2","021D",  1, 232.49, 0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L11W6H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L12W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L12W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L12W2H1","021D",  2,   0.40, 0.17,  28.54, 0.38,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W2H1","049D",  2,  28.62, 0.13, 112.02, 0.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W2H1","069D",  1,  28.51, 0.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L12W2H2","021D",  1, 190.50, 0.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W2H2","049D",  1, 190.50, 0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W2H2","069D",  1, 190.51, 0.05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L12W3H1","021D",  2, 208.48, 0.07, 239.54, 0.04,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W3H1","049D",  2, 208.51, 0.10, 239.49, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W3H1","069D",  2, 208.51, 0.02, 239.43, 0.07,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L12W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B2L12W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B2L12W4H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W4H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W5H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W5H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W6H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W6H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B2L12W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L01W1H1","021D",  2,   7.86, 0.28, 232.20, 0.59,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W1H1","049D",  2,   8.07, 0.88, 188.50, 0.04,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W1H1","069D",  1,   8.41, 1.25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L01W1H2","021D",  1, 233.50, 0.06,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W1H2","049D",  1, 233.54, 0.07,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W1H2","069D",  1, 237.50, 4.71,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L01W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W2H1","069D",  1, 239.48, 0.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L01W2H2","021D",  1, 232.12, 0.89,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// where 049D
    {"B3L01W2H2","069D",  2, 199.59, 1.12, 231.87, 0.64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    /// 
    {"B3L01W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B3L01W3H1","049D", -6,  21.93, 0.29,  49.50,19.93, 131.50,19.99, 144.61, 0.01, 203.50,20.00, 239.44, 0.22,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L01W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L01W3H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L01W3H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L01W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L01W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W4H1","049D",  1,   0.90, 0.38,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L01W4H2","021D",  3,  63.49, 1.30,  74.59, 4.79, 153.32, 6.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W4H2","049D", -2,  75.50, 5.77, 154.72, 6.56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W4H2","069D", -2,  75.50, 5.59, 153.91, 5.21,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L01W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L01W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L01W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L01W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L01W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W6H1","069D",  1, 232.50, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L01W6H2","021D",  1, 231.61, 0.49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W6H2","069D",  3,  76.53, 2.87, 156.42, 2.16, 235.33, 5.37,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
#if 0
    {"B3L01W7H1","021D",  1, 234.59, 0.64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W7H1","049D",  1,  14.37, 0.74,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L01W7H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
#else
    {"B3L01W7H1","021D",  2, 234.59, 0.64, 14.37, 0.74,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L01W7H1","049D",  2, 234.59, 0.64, 14.37, 0.74,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L01W7H1","069D",  2, 234.59, 0.64, 14.37, 0.74,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
#endif
    {"B3L01W7H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W7H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L01W7H2","069D",  1,  77.32, 1.85,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L02W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L02W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W1H2","049D", -3,  86.72, 3.88, 104.50, 9.42, 149.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W1H2","069D",  2,  84.71, 2.68, 129.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L02W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L02W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L02W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L02W5H1","049D",  2,   0.86, 0.32, 227.22, 0.92,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // bands
    {"B3L02W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// band
    
    {"B3L02W5H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L02W5H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L02W5H2","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// -"-

    {"B3L02W6H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L02W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L02W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L02W6H2","021D",-88, 229.29, 0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// where 049 and 069 // almost dead
    {"B3L02W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L02W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L02W7H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L02W7H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L02W7H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L02W7H2","021D",-88,   0.22, 0.22, 134.88, 0.98,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//amost dead
    {"B3L02W7H2","049D",-88,   0.00, 1.79,  77.50,20.00, 141.50,10.26,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  dead
    {"B3L02W7H2","069D",-88,   0.00, 1.97,  92.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// only noise
    
    {"B3L03W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L03W1H2","021D",  1,   8.59, 0.29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W1H2","049D",  1,   8.49, 0.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W1H2","069D",  2,   8.64, 0.31,  44.52, 0.70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L03W2H1","021D",-88,  32.50, 8.72,  61.63, 7.43, 215.50, 5.89, 231.86, 0.31,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L03W2H1","049D",-88,   0.49, 0.01,   9.50, 3.48,  72.06, 5.43, 157.72, 1.82,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L03W2H1","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  almost dead
    
    {"B3L03W2H2","021D",-88,   0.96, 0.23, 237.50, 9.49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L03W2H2","049D",-88, 110.47, 0.89, 175.50, 0.17, 214.91, 3.96, 239.49, 0.01,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    {"B3L03W2H2","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  almost dead
    
    {"B3L03W3H1","021D",  1, 232.51, 0.16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W3H1","049D",  1, 230.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W3H1","069D",  1, 232.67, 0.38,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L03W3H2","021D",  3,   0.51, 0.02,  79.50, 0.13, 239.54, 0.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W3H2","049D",  2,  15.24, 2.16, 231.46, 0.41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W3H2","069D",  1, 231.55, 0.32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L03W4H1","021D",  2, 115.13, 0.69, 157.62, 0.56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W4H1","049D",  5,   0.51, 0.59,  54.77, 7.19, 115.13, 0.54, 223.98, 1.14, 234.42, 1.96,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L03W4H2","021D",  4,   0.39, 0.13,  82.71, 1.11,  93.29, 1.35, 167.31, 5.07,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W4H2","049D",  3,   0.51, 0.14,  71.42, 0.01, 166.50, 5.06,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W4H2","069D",  1, 166.94, 5.75,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L03W5H1","021D",  2,   0.50, 0.01, 233.82, 0.42,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W5H1","049D",  2,   0.27, 0.43, 233.89, 0.41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W5H1","069D",  2, 135.23, 6.05, 233.41, 1.07,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L03W5H2","021D",  1,  29.78, 0.72,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W5H2","049D",  1,  29.47, 0.80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W5H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L03W6H1","021D",  1, 231.24, 2.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W6H1","049D",  1, 230.59, 0.58,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W6H1","069D",  2,  14.20, 2.39, 232.21, 1.56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L03W6H2","021D",  1, 240.00, 8.08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W6H2","049D",  1, 233.69, 0.49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W6H2","069D",  1, 233.02, 1.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L03W7H1","021D",  4,   0.00, 2.00,  38.78, 0.47, 120.17, 0.58, 240.00, 2.58,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W7H1","049D",  5,  38.90, 0.28,  80.87, 0.52, 166.33, 1.02, 178.21, 0.77, 233.56, 2.39,0,0,0,0,0,0,0,0,0,0},
    {"B3L03W7H1","069D",  3,  38.83, 0.57, 178.06, 0.66, 233.10, 1.50,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L03W7H2","021D",  3, 151.88, 0.44, 186.65, 0.66, 238.43, 1.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B3L03W7H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L03W7H2","049D",-88, 151.89, 0.48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L03W7H2","069D",-88, 151.89, 0.48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  almost dead
    
    {"B3L04W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W1H1","049D",  2,   8.52, 0.17, 232.64, 0.86,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L04W1H1","069D",  1, 138.07, 0.65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L04W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L04W2H1","021D",  1, 232.53, 0.65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W2H1","049D",  1, 232.55, 0.61,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W2H1","069D",  1, 232.40, 0.69,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L04W2H2","021D",  2,  43.48, 0.37, 233.00, 0.44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W2H2","049D",  2,  43.45, 0.40, 233.22, 0.46,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W2H2","069D",  1,  43.67, 0.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L04W3H1","021D",  1, 148.11, 0.39,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W3H1","049D",  1, 148.30, 0.38,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W3H1","069D",  2, 148.35, 0.42, 231.50, 0.14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L04W3H2","021D",  1, 230.14, 0.93,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W3H2","049D",  1, 229.66, 1.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W3H2","069D",  1, 231.34, 2.54,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L04W4H1","021D",  1, 233.56, 0.43,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W4H1","049D",  1,   8.65, 1.05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L04W4H1","069D",  1,   8.61, 0.43,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L04W4H2","021D",  1, 137.58, 0.33,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W4H2","049D",  1, 232.50, 0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L04W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L04W5H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L04W5H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L04W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L04W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L04W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L04W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L04W6H1","021D",-88,   7.27, 0.85,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L04W6H1","049D",-88,   8.18, 0.85,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    {"B3L04W6H1","069D",-88,   7.99, 0.90,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    
    {"B3L04W6H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L04W6H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    {"B3L04W6H2","069D",-88, 231.50, 0.16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    
    {"B3L04W7H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L04W7H1","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    //  {"B3L04W7H1","049D", -6,  18.50, 9.21,  50.50,15.58,  74.17, 4.58, 176.50,11.54, 209.50,11.94, 229.38, 3.76,0,0,0,0,0,0,0,0},
    {"B3L04W7H1","069D",-88, 232.50, 1.19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    
    {"B3L04W7H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L04W7H2","049D",-88,  30.06, 0.23,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    {"B3L04W7H2","069D",-88,   8.50, 0.13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    
    {"B3L05W1H1","021D", -3,  19.46, 5.14,  98.71, 5.78, 177.01, 5.42,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W1H1","049D", -4,  19.83, 5.07,  99.10, 5.92, 176.64, 5.85, 221.98, 2.16,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W1H1","069D",  1,   0.56, 0.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L05W1H2","021D", -3,  16.36, 5.50,  96.97, 4.67, 176.41, 5.13,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W1H2","049D", -3,  17.18, 6.01,  96.88, 5.41, 177.06, 6.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W1H2","069D", -3,  13.70, 4.63,  95.64, 4.30, 176.49, 4.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L05W2H1","021D", -4,  17.22, 6.28,  98.60, 6.15, 175.05, 5.22, 231.42, 1.70,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W2H1","049D", -5,  17.37, 6.40,  67.62, 2.67,  98.84, 6.08, 175.87, 5.78, 230.38, 2.91,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W2H1","069D", -2,  15.70, 5.45, 173.56, 3.29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L05W2H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L05W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L05W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L05W3H1","021D", -5,  17.95, 5.60,  95.03, 3.51, 148.06, 1.52, 177.79, 5.97, 239.49, 0.03,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W3H1","049D", -6,  18.93, 6.44,  67.52, 4.33,  97.36, 6.09, 147.90, 2.77, 179.20, 6.99, 231.02, 3.96,0,0,0,0,0,0,0,0},
    {"B3L05W3H1","069D", -4,  16.74, 5.32,  95.70, 5.39, 178.08, 5.51, 239.48, 0.01,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W3H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L05W3H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L05W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L05W4H1","021D",-88,  52.21, 1.33,  64.07, 1.24,  80.49, 0.11,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W4H1","049D",-88,  32.53, 0.05, 110.87, 0.05, 196.51, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    {"B3L05W4H1","069D",-88,  32.48, 0.16,  83.41, 2.26, 112.52, 0.17, 163.49, 0.15, 216.50, 0.06,0,0,0,0,0,0,0,0,0,0},//only noise
    
    {"B3L05W4H2","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W4H2","049D",-88, 124.50, 6.30, 152.79, 3.67, 178.02, 1.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead//  <<<<<<<<
    {"B3L05W4H2","069D",-88, 223.13, 0.71,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    
    {"B3L05W5H1","021D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W5H1","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    {"B3L05W5H1","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    
    {"B3L05W5H2","021D",-88, 107.52, 0.02,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W5H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead//  <<<<<<<<
    {"B3L05W5H2","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    
    {"B3L05W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W6H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, //<<<<<<<<<<<<<<<<<<<<<<<<< ====

    {"B3L05W6H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W6H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B3L05W7H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W7H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W7H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B3L05W7H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W7H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L05W7H2","069D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise

    {"B3L06W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L06W2H1","021D",  1, 232.40, 1.05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W2H1","049D",  1, 232.16, 1.02,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W2H1","069D",  1, 232.03, 0.88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B3L06W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W3H1","021D",  1, 232.11, 0.75,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L06W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L06W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B3L06W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//
    {"B3L06W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B3L06W4H2","021D",  1,  27.23, 0.28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W4H2","049D",  1,  27.51, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L06W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W5H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W5H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L06W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W6H1","069D",  1, 176.31, 0.57,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L06W6H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W6H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W7H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W7H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W7H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W7H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W7H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L06W7H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L07W1H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W1H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W2H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W2H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W3H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W3H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W3H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L07W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L07W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L07W3H2","069D",  2,   0.73, 0.35, 239.44, 0.05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L07W4H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W4H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W5H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W5H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W6H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W6H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W7H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W7H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W7H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W7H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W7H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L07W7H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L08W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L08W1H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L08W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L08W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead ?
    {"B3L08W2H2","069D",  4,  13.79, 0.51, 142.49, 0.89, 226.94, 1.06, 238.59, 0.50,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W3H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L08W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W3H2","021D", -6,  17.66, 3.70,  54.51,12.05, 104.50,20.00, 123.54, 0.01, 194.50,20.00, 239.42, 0.17,0,0,0,0,0,0,0,0},
    {"B3L08W3H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L08W3H2","069D",  1, 239.55, 0.19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W4H1","021D",  1, 239.46, 0.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L08W4H1","069D",  4,   9.90, 0.27, 165.48, 0.06, 202.15, 1.86, 239.58, 0.34,0,0,0,0,0,0,0,0,0,0,0,0},// bands
    
    {"B3L08W4H2","021D",  2,   0.77, 0.36, 157.99, 0.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L08W4H2","069D",  2,   0.00, 0.85, 237.44, 0.43,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W5H2","049D",  1,  54.15, 0.24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W5H2","069D",  1,  54.57, 0.34,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L08W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W6H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W6H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W6H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W7H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W7H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W7H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W7H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W7H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L08W7H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L09W1H1","021D",-88, 111.50, 7.98,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//only noise
    {"B3L09W1H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//missed ?
    {"B3L09W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L09W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W1H2","049D",-88,   0.52, 0.02, 127.23, 8.18, 215.75, 1.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    {"B3L09W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L09W2H1","021D", -4,  78.61, 8.44, 118.32,10.04, 158.81, 6.82, 218.85, 2.66,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L09W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L09W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W2H2","049D", -5,  57.50,14.40,  91.89, 7.58, 124.93,13.25, 173.50,12.52, 202.03, 3.04,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    //  {"B3L09W3H1","021D", -5,  12.74, 6.42,  34.06, 2.70,  52.50,17.84, 198.50,20.00, 225.50, 6.44,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W3H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  only noise
    {"B3L09W3H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// missed ?
    {"B3L09W3H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L09W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W3H2","049D",-88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//dead
    {"B3L09W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L09W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W4H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W4H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B3L09W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// big slope
    {"B3L09W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// missed ?
    {"B3L09W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// big slope

    {"B3L09W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L09W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L09W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L09W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W6H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W6H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W6H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

    {"B3L09W7H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// big slope
    {"B3L09W7H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L09W7H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// big slope

    {"B3L09W7H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L09W7H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L09W7H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L10W1H1","021D",  2,  42.11, 1.28, 239.73, 0.46,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W1H1","049D",  2, 187.47, 0.15, 239.41, 0.18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L10W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// missed 021D
    {"B3L10W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L10W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L10W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L10W3H1","021D",  2, 172.33, 0.84, 239.62, 0.19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W3H1","049D",  2,   0.57, 0.07, 239.38, 0.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W3H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L10W3H2","021D",  1,   8.50, 0.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W3H2","049D",  1,   8.49, 0.02,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L10W4H1","021D",  1, 239.77, 0.18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W4H1","049D",  1, 239.57, 0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L10W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W4H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L10W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W5H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L10W5H2","021D",  1,  27.41, 0.19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L10W5H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W5H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L10W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W6H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W6H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W6H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W6H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W7H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W7H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W7H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W7H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W7H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L10W7H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W1H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W1H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W2H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W2H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L11W2H2","021D",  1, 211.51, 0.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W2H2","049D",  1, 211.51, 0.09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B3L11W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L11W2H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L11W3H1","021D",  1, 201.83, 0.37,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W3H1","049D",  1, 201.64, 1.37,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W3H1","069D",  1, 201.64, 1.37,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    //  {"B3L11W3H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L11W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W3H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W3H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L11W4H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W4H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W5H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W5H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W6H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W6H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W7H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W7H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W7H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W7H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W7H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L11W7H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L12W1H1","021D",  1, 240.00, 7.07,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// bands
    {"B3L12W1H1","049D",  2,   0.64, 0.36, 236.62, 2.70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W1H1","069D",  3,  42.66, 2.85, 212.13, 1.80, 236.20, 4.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// bands
    
    {"B3L12W1H2","021D",  3,  14.50, 4.35, 155.09, 1.83, 240.00, 8.35,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W1H2","049D",  3,   0.89, 0.33,  14.50, 5.59, 240.00, 6.24,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L12W1H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// bands
    
    {"B3L12W2H1","021D", -2,  19.50, 8.45, 240.00,14.85,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W2H1","049D", -2,  17.50, 6.89, 237.50,13.44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W2H1","069D", -3,  19.50, 7.66, 223.50, 7.73, 240.00, 6.66,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W2H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W2H2","069D",  2,  10.50, 9.66, 239.72, 0.22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W3H1","021D",  3,  16.50, 8.34, 228.50, 4.88, 240.00, 0.85,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W3H1","049D",  3,  14.50, 7.83, 228.50, 4.92, 239.88, 0.61,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L12W3H1","069D",  3,  15.96, 8.63, 227.50, 3.51, 240.00, 5.87,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W3H2","049D",  1, 240.00,15.90,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W3H2","069D",  1, 240.00, 0.57,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W4H1","021D",  4,   0.93, 0.51,  22.39, 5.50, 112.68, 1.02, 209.50,17.22,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W4H1","049D",  4,  20.83, 4.13, 112.85, 1.32, 195.39, 0.92, 209.50,18.17,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L12W4H1","069D",  4,   1.01, 0.64,  21.11, 4.83, 114.61, 0.09, 208.50,15.56,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W4H2","021D",  1, 240.00, 0.63,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W4H2","049D",  2, 224.50, 5.85, 239.57, 0.48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W4H2","069D",  3,   0.00, 3.26, 221.42,11.43, 239.45, 0.32,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W5H1","021D",  3,  27.98, 1.94,  87.53, 2.88, 222.85, 1.57,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W5H1","049D",  5,   0.00, 0.74,  27.23, 2.25,  66.19, 7.31,  87.14, 3.28, 224.08, 4.47,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W5H1","069D",  5,   0.00, 0.92,  27.62, 2.07,  65.79, 1.91,  86.84, 4.37, 222.99, 1.63,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W5H2","021D",  3,   0.52, 0.02,  48.12, 3.88, 239.46, 0.13,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W5H2","049D",  1, 239.50, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W5H2","069D",  3,  47.78, 3.03, 203.14, 2.44, 239.47, 0.33,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W6H1","021D",  2,   0.44, 0.40, 239.59, 0.17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W6H1","049D",  2,   0.53, 0.04, 239.56, 0.19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W6H1","069D",  3,  22.54, 1.59, 206.84, 2.77, 239.49, 0.01,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W6H2","021D", -3,   0.00, 1.06,  65.50,20.00, 151.50, 6.54,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W6H2","049D", -4,   0.00, 1.36,  46.29,20.00, 132.50,20.00, 155.50, 1.92,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W6H2","069D", -4,   0.00, 1.67,  28.50,18.24,  70.50,20.00, 152.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W7H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W7H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W7H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L12W7H2","021D",  2,  10.17, 0.85, 111.50, 0.03,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W7H2","049D",  2,  10.11, 0.85, 112.03, 0.70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L12W7H2","069D",  2,  10.06, 0.81, 111.93, 0.60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L13W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L13W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L13W2H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L13W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L13W3H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W3H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W3H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L13W3H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W3H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L13W4H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W4H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W4H1","069D",  2,  12.50, 0.14,  96.11, 0.27,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L13W4H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W4H2","049D",  1,   0.41, 0.11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W4H2","069D",  1, 189.38, 1.38,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L13W5H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W5H1","049D",  1,   0.41, 0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W5H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// big slope
    
    {"B3L13W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W6H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W6H1","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W6H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L13W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L13W7H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W7H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W7H1","069D",  1, 210.25, 3.47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L13W7H2","021D",  1,  46.62, 0.18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W7H2","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L13W7H2","069D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L14W1H1","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L14W1H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L14W2H1","021D",  2,   0.81, 0.64, 240.00, 0.76,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W2H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L14W3H1","021D",  2,   1.50, 0.80, 240.00, 0.72,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W3H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W3H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L14W3H2","021D",  1, 188.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W3H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L14W4H1","021D",  3,   0.03, 0.01, 134.91, 2.22, 240.00, 0.93,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L14W4H2","021D", -3, 113.50,20.00, 139.50,20.00, 212.50,11.45,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L14W5H1","021D",  -2,  19.96, 1.59,  97.61, 0.56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W5H1","049D",  -9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L14W5H1","069D",  -9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L14W5H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//almost dead
    {"B3L14W5H2","049D",  4,   9.25, 0.84, 171.28, 4.15, 187.27, 3.17, 240.00, 0.68,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    
    {"B3L14W6H1","021D", -3,  70.00,11.59, 116.50,20.00, 177.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W6H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L14W6H1","069D",  3,  79.27, 1.06, 117.92, 0.85, 177.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L14W6H2","021D", -3,  86.50,20.00, 133.50,15.43, 177.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W6H2","049D", -4,  65.50,20.00, 107.02,20.00, 170.50,20.00, 201.50, 9.30,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W6H2","069D", -9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    
    {"B3L14W7H1","021D", -5,  36.67, 2.84,  62.50,10.65, 127.50, 9.38, 177.50,10.18, 206.30, 8.23,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W7H1","049D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L14W7H1","069D",  3,  11.72, 1.66, 144.46, 0.37, 195.33, 1.63,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    
    {"B3L14W7H2","021D",  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W7H2","049D", -4,  37.50,12.93,  69.50, 6.20, 167.50, 7.71, 216.50,20.00,0,0,0,0,0,0,0,0,0,0,0,0},
    {"B3L14W7H2","069D", -4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},//  <<<<<<<<
    {"B3L15W1H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W1H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W2H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W2H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W3H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W3H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W3H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W3H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W3H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W4H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W4H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W5H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W5H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W6H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W6H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W7H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W7H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W7H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W7H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W7H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L15W7H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W1H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W1H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W1H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W1H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W1H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W1H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W2H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W2H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W2H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W2H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W2H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W2H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W3H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W3H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W3H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W3H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W3H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W3H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W4H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W4H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W4H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W4H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W4H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W4H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W5H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W5H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W5H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W5H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W5H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W5H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W6H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W6H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W6H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W6H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W6H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W6H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W7H1","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W7H1","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W7H1","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W7H2","021D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W7H2","049D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},// dead
    {"B3L16W7H2","069D",-99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} // dead
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
  Int_t iok = 0; 
  Int_t day = (run/1000)%1000; 
  Int_t d = 0;
  Hybrids_t *p = 0;
  Int_t k;
  Double_t *peaks = 0;
  if (anode <= 10 || anode >= 230) {iok =  1; goto ENDL;}
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
 ENDL:
  return iok;
}  
