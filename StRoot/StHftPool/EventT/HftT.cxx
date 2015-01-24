#include <TStyle.h>
#include <TCanvas.h>
#include <assert.h>
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TRVector.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "HftT.h"
#include "TKey.h"
#include "Riostream.h"
ClassImp(HftT);
static Int_t _debug = 0;
struct Geometry_t {
  Int_t Layer;
  Int_t Sector;
  Int_t NoLadders;
  Int_t NoSensors;
};
const Int_t NoLayers = 3;
//               kIstNumSensorsPerLadder = 6
const Geometry_t HftConfig[4] = 
  // Pxl: geoHMatrixSensorOnGlobal(Int_t sector, Int_t ladder, Int_t sensor)
  // Ist : int sensorId = 1000 + ((int)newHit->getLadder() - 1) * kIstNumSensorsPerLadder + (int)newHit->getSensor();
  //    Layer  Sector NoLadders NoSensors                  i_sector [1-10], i_ladder[1-n], i_sensor[1-n]
  {    {   1,     10,        1,   0}, //   10}, // Pxl 1  id =  i_sector * 40 + (i_ladder - 1) * 10 + i_sensor + 1     
       {   2,     10,        3,   0}, //   10}, // Pxl 2
       {   3,     -1,       24,   0}, //    6}, // Ist    id = 1000 + (i_ladder-1) * 6 + i_sensor + 1
       {   4,     -1,       20,   0}  //   16}  // Sst
  };
//________________________________________________________________________________
HftT::HftT(TTree *tree) : fChain(0), _e(0), fOutFileName("HftH.root")
{
  // if parameter tree is not specified (or zero), connect the file
  // used to generate this class and read the Tree.
  if (tree == 0) {
    TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("st_physics_15100090_raw_5500008.tree.root");
    if (!f || !f->IsOpen()) {
      f = new TFile("st_physics_15100090_raw_5500008.tree.root");
    }
    f->GetObject("t",tree);
    
  }
  Init(tree);
}
//________________________________________________________________________________
HftT::~HftT()
{
  if (!fChain) return;
  delete fChain->GetCurrentFile();
}
//________________________________________________________________________________
Int_t HftT::GetEntry(Long64_t entry)
{
  // Read contents of entry.
  if (!fChain) return 0;
  return fChain->GetEntry(entry);
}
//________________________________________________________________________________
Long64_t HftT::LoadTree(Long64_t entry)
{
  // Set the environment to read one entry
  if (!fChain) return -5;
  Long64_t centry = fChain->LoadTree(entry);
  if (centry < 0) return centry;
  if (fChain->GetTreeNumber() != fCurrent) {
    fCurrent = fChain->GetTreeNumber();
    Notify();
  }
  return centry;
}
//________________________________________________________________________________
void HftT::Init(TTree *tree)
{
  // The Init() function is called when the selector needs to initialize
  // a new tree or chain. Typically here the branch addresses and branch
  // pointers of the tree will be set.
  // It is normally not necessary to make changes to the generated
  // code, but the routine can be extended by the user if needed.
  // Init() will be called many times when running on PROOF
  // (once per file to be processed).
  
  // Set branch addresses and branch pointers
  if (!tree) return;
  fChain = tree;
  fCurrent = -1;
  //   fChain->SetMakeClass(1);
  if (!_e) _e = new EventT();
  fChain->SetBranchAddress("e.",&_e);
  Notify();
}
//________________________________________________________________________________
Bool_t HftT::Notify()
{
  // The Notify() function is called when a new file is opened. This
  // can be either for a new TTree in a TChain or when when a new TTree
  // is started when using PROOF. It is normally not necessary to make changes
  // to the generated code, but the routine can be extended by the
  // user if needed. The return value is currently not used.
  
  return kTRUE;
}
//________________________________________________________________________________
void HftT::Show(Long64_t entry)
{
  // Print contents of entry.
  // If entry is not specified, print current entry
  if (!fChain) return;
  fChain->Show(entry);
}
//________________________________________________________________________________
Int_t HftT::Cut(Long64_t entry)
{
  // This function may be called from Loop.
  // returns  1 if entry is accepted.
  // returns -1 otherwise.
  return 1;
}
//________________________________________________________________________________
void HftT::Loop(Int_t Nevents) {
  //   In a ROOT session, you can do:
  //      Root > .L HftT.C
  //      Root > HftT t
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
       dZ = zG - zP = -dz  +beta *xP -alpha*yP
       
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
    Double_t xmax[3]; // pxl ist  ssd 
  };
  static Double_t Du[2] = {3.000, 3.65};
  static Double_t Sv[2] = {6.305, 4.35};
  enum {eLocPlots = 8, eGlobPlots = 37};
  const  PlotName_t plotNameD[eLocPlots] = {// plots for drift
    {"dutuP","<u - uP>       versus  tuP =>  dw ", { 0.5, 0.5, 1}},                    //  0
    {"dvtvP","<v - vP>       versus  tvP =>  dw ", { 2.5, 2.5, 1}},                    //  1
    {"duvP", "<u - uP>       versus    v =>  gamma (our beta)", { 1, -1, 1}},          //  2 z
    {"dvuP", "<v - vP>       versus    u => -gamma (our -beta)", { 1, -1, 1}},         //  3
    {"duOvertuPvP","<(u - uP)/tuP> versus  v => alpha ", { 1, -1, 1}},                 //  4 z
    {"dvOvertvPvP","<(v - vP)/tvP> versus  v => alpha ", { 1, -1, 1}},                 //  5 z
    {"duOvertuPuP","<(u - uP)/tuP> versus  u => -beta (our gamma)", { 1, -1, 1}},      //  6
    {"dvOvertvPuP","<(v - vP)/tvP> versus  u => -beta (our gamma)", { 1, -1, 1}},      //  7
  };
  const  PlotName_t plotName[eGlobPlots] = {
    {"dutuP","<u - uP>       versus  tuP =>  dw", { 0.5, 0.5, 1}},                    //  0
    {"dvtvP","<v - vP>       versus  tvP =>  dw", { 2.5, 2.5, 1}},                    //  1
    {"duvP", "<u - uP>       versus   vP =>  gamma", { -2, -2, 1}},                   //  2 z
    {"dvuP", "<v - vP>       versus  -uP =>  gamma", { -1, -1, 1}},                   //  3
    {"duOvertuPvP","<(u - uP)/tuP> versus   vP => alpha", { -2, -2, 1}},             //  4 z
    {"dvOvertvPvP","<(v - vP)/tvP> versus   vP => alpha", { -2, -2, 1}},             //  5 z
    {"duOvertuPuP","<(u - uP)/tuP> versus  -uP => beta", { -1, -1, 1}},              //  6
    {"dvOvertvPuP","<(v - vP)/tvP> versus  -uP => beta", { -1, -1, 1}},              //  7
    {"duuP", "<u - uP>       versus  -uP", { -1, -1, 1}},                             //  8 
    {"dvvP", "<v - vP>       versus   vP", { -2, -2, 1}},                             //  9 z

    {"dXvsX","dX versus  x"          , { 10, 15, 24}},                                // 10
    {"dXvsY","dX versus  y  => gamma", { 10, 15, 24}},                                // 11
    {"dXvsZ","dX versus -z  => beta",  { 11, 24, 24}},                                // 12
    {"dYvsX","dY versus -x  => gamma", { 10, 15, 24}},                                // 13
    {"dYvsY","dY versus  y"          , { 10, 15, 24}},                                // 14
    {"dYvsZ","dY versus  z  => alpha", { 11 ,24, 24}},                                // 15
    {"dZvsX","dZ versus  x  => beta",  { 10, 15, 24}},                                // 16
    {"dZvsY","dZ versus -y  => alpha", { 10, 15, 24}},                                // 17
    {"dZvsZ","dZ versus  z",           { 11, 24, 24}},                                // 18
    
    {"dX4dx","dX vs -1+jx*vx          => dx",    {  3, 3, 24}},                    // 19
    {"dX4dy","dX vs    jx*vy          => dy",    {1.2, 1, 24}},                    // 20
    {"dX4dz","dX vs    jx*vz          => dz",    {.01,.8, 24}},                    // 21
    {"dX4da","dX vs    jx*(-vy*z+vz*y)=> alpha", { 6,15, 24}},                    // 22
    {"dX4db","dX vs  -z+jx*(vx*z-vz*x)=> beta ", {10,50, 24}},                    // 23
    {"dX4dg","dX vs  [y]+jx*(-vx*y+vy*x)=> gamma", { 4, 6, 24}},                    // 24
    
    {"dY4dx","dY vs    jy*vx          => dx",    { 1.2, 0.8, 24}},                    // 25
    {"dY4dy","dY vs -1+jy*vy          => dy",  {2.2,2.2, 24}},                    // 26
    {"dY4dz","dY vs    jy*vz          => dz",  {.01,1.0, 24}},                    // 27
    {"dY4da","dY vs  z+jy*(-vy*z+vz*y)=> alpha", {10,50, 24}},                    // 28
    {"dY4db","dY vs     jy*(vx*z-vz*x)=> beta ", { 6,15, 24}},                    // 29
    {"dY4dg","dY vs [-x]+jy*(-vx*y+vy*x)=> gamma", {4,5, 24}},                    // 30
    
    {"dZ4dx","dZ vs    jz*vx          => dx",  { 3, 2.2, 24}},                    // 31
    {"dZ4dy","dZ vs    jz*vy          => dy",  {1.2,0.8, 24}},                    // 32
    {"dZ4dz","dZ vs -1+jz*vz          => dz",  {0.1,0.8, 24}},                    // 33
    {"dZ4da","dZ vs -y+jz*(-vy*z+vz*y)=> alpha", { 6,15, 24}},                    // 34
    {"dZ4db","dZ vs  x+jz*( vx*z-vy*x)=> beta ", {10,50, 24}},                    // 35
    {"dZ4dg","dZ vs    jz*(-vx*y+vy*x)=> gamma", {4,  6, 24}},                     // 36


  };
  Double_t rCut = 1.0;
  TFile *fOut = new TFile(fOutFileName,"recreate");
  TString Name, Title;
#if 0
  TH1D *LSF = new TH1D("LSF","Matrix and right part for Least Squred Fit",6*28,0,6*28);
  //  TH1D *LSF = new TH1D("LSF","Matrix and right part for Least Squred Fit",15*28,0,15*28);
  TH1D *LSFB[4];
  for (Int_t layern = 1; layern <= 4; layern++) 
    LSFB[layern-1] = new TH1D(Form("LSFB%i",layern),
			      Form("Matrix and right part for Least Squred Fit for layer %i",layern),
			      HftConfig[layern-1].NoLadders*28,0,HftConfig[layern-1].NoLadders*28);
#endif
#if 0
  //                     T  Ly ld   W+1
  TH2F *LocPlots[eLocPlots][4][30][17];
  memset(LocPlots,0,eLocPlots*4*30*17*sizeof(TH2F *));
  for (Int_t L = 0; L < NoLayers; L++) {// over Layers
    Int_t layer     = HftConfig[L].Layer;
    Int_t sector    = HftConfig[L].Sector;
    Int_t NoLadders = HftConfig[L].NoLadders;
    Int_t NoSensors = HftConfig[L].NoSensors;
    for (Int_t ladder = 1; ladder <= NoLadders; ladder++) {
      //if (layer >= 2 ) continue;   // Ignore IST/SSD for now
      for (Int_t sensor = 0; sensor <= NoSensors; sensor++) {// sensor == 0 for whole ladder
	Int_t Id = 1000*L + 10*(ladder-1) + sensor;
	if (layer == 1 && (ladder - 1) %4 != 0) continue;
	if (layer == 2 && (ladder - 1) %4 == 0) continue;
	for (Int_t t = 0; t < eLocPlots; t++) {
	  Name = Form("%s%04i",plotNameD[t].Name,Id);
	  Title = Form("%s for  layer %i ladder %i, ",plotNameD[t].Title,layer,ladder);
	  if (sensor == 0) Title += "all sensors";
	  else             Title += Form("sensor %i",sensor);
	  Int_t n = 100;
	  Int_t k = 0;
	  if (layer >= 3) k = 1;
	  Double_t xmax = plotNameD[t].xmax[k];
	  //	   if (xmax > 0 && t >= 8 && ! (NoSensors > 2 && sensor != 0)) xmax = -1;
	  if (xmax < 0) {
	    Int_t m = - (Int_t) xmax;
	    if (m == 1) xmax = Du[k];
	    else        xmax = Sv[k]/2.;
	  }
	  if ((sensor == 0 ) && (t == 2 || t == 4 || t == 5 || t == 9)) {
	    switch (layer) {
	    case 1: 
	    case 2: xmax = 21; break;
	    case 3: xmax = 21; break;
	    case 0:
	    case 4: xmax = 35; break;
	    default: xmax = 40; break;
	    }
	    n = (Int_t) (50*xmax);
	  }
	  Double_t ymax = rCut/2;
	  Int_t ny = 500;
	  Double_t dy = ymax/ny;
	  LocPlots[t][layer-1][ladder-1][sensor] = new TH2F(Name,Title,n,-xmax,xmax,ny+1,-ymax-dy,ymax+dy);
	}
      }
    }
  }  // NoLayers
  //              T  S
#endif
  TH2F *GloPlots[27][16];
  memset(GloPlots,0,27*15*sizeof(TH2F *));
  for (Int_t s = 1; s <= 16; s++) { 
    for (Int_t i = 0; i < 27; i++) {
      Int_t t = i+10;
      Name = Form("%s%02i",plotName[t].Name,s);
      if (s <= 10) 	      Title = Form("%s for PXL Sector  %i",plotName[t].Title,s);
      if (s == 11)            Title = Form("%s for Pxl 1 half",plotName[t].Title);
      if (s == 12)            Title = Form("%s for Pxl 2 half",plotName[t].Title);
      if (s == 13)            Title = Form("%s for All Pxl",plotName[t].Title);
      if (s == 14)            Title = Form("%s for All Ist",plotName[t].Title);
      if (s == 15)            Title = Form("%s for All Ssd",plotName[t].Title);
      if (s == 16)            Title = Form("%s for All HFT",plotName[t].Title);
#if 1
      Int_t m = 0;
      if (s == 14) m = 1;
      if (s == 15) m = 2;
      if (s == 16) m = 1;
      Double_t xmax = plotName[t].xmax[m];
      Int_t n = (Int_t) (4.*xmax);
      if (n < 100) n = 100;
      if (n > 400) n = 400;
      Double_t ymax = 0.4;
#else
      Int_t    n = 100;
      Double_t xmax = 0;
      Double_t ymax = 0;
#endif
      GloPlots[i][s-1] = new TH2F(Name,Title, n,-xmax,xmax,500,-ymax,ymax);
    }
  }
  Long64_t nentries = fChain->GetEntriesFast();
  if (Nevents > 0 && nentries > Nevents) nentries = Nevents;
  Long64_t nbytes = 0, nb = 0;
  Int_t TreeNo = -1;
  TString currentFile("");
  cout <<" nentries="<<nentries<<"   Nevents="<<Nevents<<endl;
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
    TClonesArray *matched = _e->GetMatchHits();
    Int_t NHits = _e->fNmatchhit;
    for (Int_t k = 0; k < NHits; k++) {
      HitMatchT *hitK = _e->GetHitMatchT(k);
      if (! hitK) continue;
      if (_debug) {
	cout << "hitK\t"; hitK->Print("");
      }
#if __BEST__
      Double_t difK = hitK->Diff();
      Int_t k1 = k + 1;
      for (; k1 < NHits; k1++) {
	HitMatchT *hitK1 = _e->GetHitMatchT(k1);
	if (! hitK1) break;
	if (_debug) {
	  cout << "hitK1\t"; hitK1->Print("");
	}
	if (hitK1->detId != hitK->detId) break;
	k = k1;
	if (hitK1->Diff() < hitK->Diff()) {
	  hitK = hitK1;
	}
      }
      if (! hitK) continue;
#endif
      Int_t layer  = hitK->Layer();
      Int_t sector = hitK->Sector();
      Int_t ladder = hitK->Ladder();
      Int_t half   = hitK->Half();
      if (ladder == 0) continue;
      Int_t sensor  = hitK->Sensor();
      Double32_t xGP = hitK->xGP;
      Double32_t zGP = hitK->zGP;
      Double32_t yGP = hitK->yGP;
      Double32_t uP = hitK->xLP;       
      Double32_t vP = hitK->zLP;
      Double32_t tuP = hitK->tuP;       
      Double32_t tvP = hitK->tvP;
      Double32_t xLP = hitK->xLP;
      Double32_t zLP = hitK->zLP;
      Double32_t dxP = hitK->dxGP;
      Double32_t dyP = hitK->dyGP;
      Double32_t dzP = hitK->dzGP;
      Double32_t xG = hitK->xG;       
      Double32_t yG = hitK->yG;       
      Double32_t zG = hitK->zG;       
      Double32_t dX = xG - xGP;
      Double32_t dY = yG - yGP;
      Double32_t dZ = zG - zGP;
      Double32_t u = hitK->xL;       
      Double32_t v = hitK->zL;
      Double32_t zL = hitK->zL;
      Double32_t xL = hitK->xL;
      Double32_t DxL = xL - xLP;
      Double32_t DzL = zL - zLP;
      Double32_t du = u - uP;
      Double32_t dv = v - vP;
      Double_t uA = TMath::Abs(u);
      Double_t vA = TMath::Abs(v);
      Double_t x = xGP;
      Double_t y = yGP;
      Double_t z = zGP;
      Double_t jL2 = TMath::Sqrt(1. + tuP*tuP + tvP*tvP);
      Double_t wG[3] = {hitK->wGu,hitK->wGv, hitK->wGw};
      Double_t vx =  hitK->wGu*jL2;
      Double_t vy =  hitK->wGv*jL2;
      Double_t vz =  hitK->wGw*jL2;
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
	//	   {dX, ( y+dxP*(-vx*y+vy*x))},
	{dX, ( dxP*(-vx*y+vy*x))},
	{dY, (   dyP*vx) }, // 15
	{dY, (-1+dyP*vy) },
	{dY, (   dyP*vz) },
	{dY, ( z+dyP*(-vy*z+vz*y))},
	{dY, (   dyP*( vx*z-vz*x))},
	//	   {dY, (-x+dyP*(-vx*y+vy*x))},
	{dY, (dyP*(-vx*y+vy*x))},
	{dZ, (   dzP*vx) }, // 21
	{dZ, (   dzP*vy) },
	{dZ, (-1+dzP*vz) },
	{dZ, (-y+dzP*(-vy*z+vz*y))},
	{dZ, ( x+dzP*( vx*z-vz*x))},
	{dZ, (   dzP*(-vx*y+vy*x))}
      };
      Int_t ss[4] = {0, 0, 0, 16};
      if (layer <= 2) {
	ss[0] = sector;               // Pxl sector
	if (sector <= 5) ss[1] = 11;  // Pxl 1-st half
	else             ss[1] = 12;  // Pxl 2-nd half
	ss[2] = 13;                   // Pxl as whole
      } else if (layer == 3) {
	ss[0] = 14;                   // Ist
      } else if (layer == 4) {
	ss[0] = 15;                   // Sst
      }
      for (Int_t k = 0; k < 4; k++) {
	Int_t s = ss[k];
	if (! s) continue;
	for (Int_t l = 0; l < 27; l++) {
	  GloPlots[l][s-1]->Fill(vars[l][1],vars[l][0]); // Pxl Sector
	}
      }
#if 0
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
	if ((l == 0 && TMath::Abs(wG[0]) >= 0.999) ||
	    (l == 1 && TMath::Abs(wG[1]) >= 0.999)) continue;
	mX.AddRow(&dxyz[l]);
	A.AddRow(ABR.GetRow(l));
	for (UInt_t jk = 0; jk < 6; jk++) {
	  UInt_t lk = jk + 6*l + 9;
	  //	   cout << "lk = "<< lk << "  sector=" << sector <<endl;
	  // layer 1 out until resolved
	  if (layer !=1 )
	    //	     if (layer !=4 )
	    {
	      GloPlots[lk][sector-1]->Fill(ABR(l,jk),dxyz[l]);
	      if(layer == 1 || layer ==2)  GloPlots[lk][12]->Fill(ABR(l,jk),dxyz[l]);
	      if((layer == 1 || layer ==2) && sector <= 5)  GloPlots[lk][10]->Fill(ABR(l,jk),dxyz[l]);
	      if((layer == 1 || layer ==2) && sector >= 6)  GloPlots[lk][11]->Fill(ABR(l,jk),dxyz[l]);
	    }
	  else
	    {
	      GloPlots[lk][sector-1]->Fill(-ABR(l,jk),dxyz[l]);
	      if(layer == 1 || layer ==2)  GloPlots[lk][12]->Fill(-ABR(l,jk),dxyz[l]);
	      if((layer == 1 || layer ==2) && sector <= 5)  GloPlots[lk][10]->Fill(-ABR(l,jk),dxyz[l]);
	      if((layer == 1 || layer ==2) && sector >= 6)  GloPlots[lk][11]->Fill(-ABR(l,jk),dxyz[l]);
	      
	    }
	  if (layer !=1 )
	    {
	      GloBLPlots[lk][layer-1][ladder-1]->Fill(ABR(l,jk),dxyz[l]);
	    }
	  else
	    {
	      GloBLPlots[lk][layer-1][ladder-1]->Fill(-ABR(l,jk),dxyz[l]);
	    }
      //	 cout << "I am out of the lk loop on GloBLPlots"<<endl;
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
      array = LSFB[layer-1]->GetArray();
      amX = pm.GetArray();
      sX  = PX.GetArray();
      im = 1 + 28*(ladder-1);
      is = im + 6;
      TCL::vadd(amX,array+im,array+im,6);
      TCL::vadd(sX,array+is,array+is,21);
      //	 cout << "I am out of the LSFB --- jentry="<<jentry<<"   k="<<k<<endl;
      Double32_t duOvertuP = du/tuP;
      Double32_t dvOvertvP = dv/tvP;
      sensor = 0;
      LocPlots[0][layer-1][ladder][sensor]->Fill(tuP,du);
      LocPlots[1][layer-1][ladder][sensor]->Fill(tvP,dv);
      LocPlots[2][layer-1][ladder][sensor]->Fill( v ,du);
      LocPlots[3][layer-1][ladder][sensor]->Fill( u ,dv);
      LocPlots[4][layer-1][ladder][sensor]->Fill( v ,duOvertuP);
      LocPlots[5][layer-1][ladder][sensor]->Fill( v ,dvOvertvP);
      LocPlots[6][layer-1][ladder][sensor]->Fill( u ,duOvertuP);
      LocPlots[7][layer-1][ladder][sensor]->Fill( u ,dvOvertvP);
#endif
    }
    //       cout << "I am out of the hit loop --- jentry="<<jentry<<"   trk="<<trk<<endl;
    if (jentry%1000 == 0) cout << "jentry="<<jentry<<endl;
  }
  TIter nextkey( gDirectory->GetListOfKeys() );
  TKey *key = 0;
  while ((key = (TKey*) nextkey())) {
    TObject *obj = key->ReadObj();
    if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
      TH1 *h1 = (TH1*)obj;
      cout << "Found histogram " << h1->GetName() << " with " << h1->GetEntries() << " entries" << endl;
      if (h1->GetEntries() == 0) delete  h1;
    }
  }
  fOut->Write();
}
