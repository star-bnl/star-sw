/*
  root.exe -q -b 'TpcAlignerDraw.C+(0,"*Corr*.root")' >& TpcAlignerDraw.log &
foreach f (`ls -1d 2014*.root`)
set b = `basename ${f} .root`; root.exe -q -b 'TpcAlignerDraw.C+(0,"'${f}'")' >& ${b}.log &
end
FPE_OFF
foreach f (`ls -1d 2014*.IO*.Errors.root`)
set b = `basename ${f} .root`; root.exe -q -b ${f} 'TpcAlignerDraw.C+(2)' >& ${b}.Fit.log &
end
*/
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TLegend.h"
#include "TProfile.h"
#include "TTree.h"
#include "TFile.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TVector3.h"
#include "TMinuit.h"
#include "TMinuitMinimizer.h"
#include "TMath.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "StarRoot/TDirIter.h"
#include "StarRoot/TTreeIter.h"
#include "TArrayD.h"
#include "TRVector.h"
#include "TRMatrix.h"
#include "TGeoMatrix.h"
#endif
/* 
   TpcW2STree->Draw("(abs(HelixU.Rho)-abs(HelixS.Rho))/(abs(HelixU.Rho)+abs(HelixS.Rho))>>dC(100,-2e-1,2e-1)") => sigma = 3.55241e-02
   TpcInOutTree->Draw("(abs(In.Rho)-abs(Out.Rho))/(abs(In.Rho)+abs(Out.Rho))>>dC(100,-2e-1,2e-1)") => 6.24288e-02
 */
static Int_t _debug = 0;
#define PrPP(B) if (_debug) {cout << (#B) << " = \t" << (B) << endl;}
//________________________________________________________________________________
struct PlotName_t {
  const Char_t *Name;
  const Char_t *Title;
  Int_t nx;
  Double_t xmin, xmax;
  Int_t nz;
  Double_t zmin, zmax;
};
enum {NPlots = 20, NFPlots=14, NwsPlots=38, NwsFPlots=32};
const  PlotName_t plotNameD[NPlots] = {// plots for drift
  {"dXdy"     ,"dX  versus  tX         =>  dy",    100,  -0.5,  0.5, 500, -1.000, 1.000}, // 0 -> <dx>, dy   
  {"dXdalpha" ,"dX  versus -tX*zO      =>  alpha", 100,  -60.,  40., 500, -1.000, 1.000}, // 1 -> <dx>, alpha
  {"dXdbeta"  ,"dX  versus -zO         =>  beta",  100, -210., -10., 500, -1.000, 1.000}, // 2 -> <dx>, beta 
  {"dXdgamma" ,"dX  versus  yO + tX*xO =>  gamma", 100,  118., 130., 500, -1.000, 1.000}, // 3 -> <dx>, gamma
  {"dZdy"     ,"dZ  versus  tZ         =>  dy",    100, -0.8 ,  0.6, 500, -1.000, 1.000}, // 4 -> <dz>, dy   
  {"dZdalpha" ,"dZ  versus -(yO+tZ*zO) =>  alpha", 100, -200., -50., 500, -1.000, 1.000}, // 5 -> <dz>, alpha
  {"dZdbeta"  ,"dZ  versus  xO         =>  beta",  100,  -30.,  30., 500, -1.000, 1.000}, // 6 -> <dz>, beta 
  {"dZdgamma" ,"dZ  versus  tZ*xO      =>  gamma", 100,  -10.,  10., 500, -1.000, 1.000}, // 7 -> <dz>, gamma
  {"dnXdbeta" ,"dnX versus -nzO        =>  beta",  100,  -0.8,  0.8, 500, -0.025, 0.025}, // 8 ->       beta 
  {"dnXdgamma","dnX versus  nyO        =>  gamma", 100,   0.7,  1.0, 500, -0.025, 0.025}, // 9 ->       gamma
  {"dnYdalpha","dnY versus  nzO        =>  alpha", 100,  -0.8,  0.8, 500, -0.025, 0.025}, //10 ->       alpha 
  {"dnYdgamma","dnY versus -nxO        =>  gamma", 100,   -.6,   .6, 500, -0.025, 0.025}, //11 ->       gamma
  {"dnZdalpha","dnZ versus -nyO        =>  alpha", 100,  -1.0,  -.7, 500, -0.025, 0.025}, //12 ->       alpha
  {"dnZdbeta" ,"dnZ versus  nxO        =>  beta" , 100,   -.6,   .6, 500, -0.025, 0.025}, //13 ->       beta
  {"dX"       ,"dX  versus Z"                    , 200,    10,  210, 500, -1.000, 1.000}, //14
  {"dY"       ,"dY  versus Z"                    , 200,    10,  210, 500, -0.001, 0.001}, //15
  {"dZ"       ,"dZ  versus Z"                    , 200,    10,  210, 500, -1.000, 1.000}, //16
  {"dnX"      ,"dnX versus Z"                    , 200,    10,  210, 500, -0.025, 0.025}, //17
  {"dnY"      ,"dnY versus Z"                    , 200,    10,  210, 500, -0.025, 0.025}, //18
  {"dnZ"      ,"dnZ versus Z"                    , 200,    10,  210, 500, -0.025, 0.025}  //19
};
const  PlotName_t plotNameWS[NwsPlots] = {// plots for drift
  { "dYdxS", "ly => xS",							  100,-1.0,  1.0,100, -1.000, 1.000}, // 0
  { "dYdaS", "r33*ZW+r32*YW+r31*XW+tz => aS",					  100,-210, -207,100, -1.000, 1.000}, // 1  empty
  { "dYdbS", "ly*r33*ZW+ly*r32*YW+ly*r31*XW+ly*tz => bS",			  100,-100, 100.,100, -1.000, 1.000}, // 2
  { "dYdgS", "(-ly*r23-r13)*ZW+(-ly*r22-r12)*YW+(-ly*r21-r11)*XW-ly*ty-tx => gS", 100, -65, -55.,100, -1.000, 1.000}, // 3
  { "dZdxS", "lz => xS",							  100,-0.5,  0.5,100, -0.700, 0.700}, // 4
  { "dZdaS", "-r23*ZW-r22*YW-r21*XW-ty => aS",					  100,-40.,  40.,100, -0.700, 0.700}, // 5
  { "dZdbS", "(lz*r33+r13)*ZW+(lz*r32+r12)*YW+(lz*r31+r11)*XW+lz*tz+tx => bS",	  100, 20., 200.,100, -0.700, 0.700}, // 6
  { "dZdgS", "-lz*r23*ZW-lz*r22*YW-lz*r21*XW-lz*ty => gS",			  100, -5.,   5.,100, -0.700, 0.700}, // 7
  {"dnXdbS", "-r33*nzW-r32*nyW-r31*nxW => bS",					  100,-0.4,  0.6,100, -0.010, 0.010}, // 8
  {"dnXdgS", "r23*nzW+r22*nyW+r21*nxW => gS",					  100,-0.3,  0.3,100, -0.010, 0.010}, // 9
  {"dnYdaS", "r33*nzW+r32*nyW+r31*nxW => aS",					  100,-0.7,  0.4,100, -0.020, 0.020}, //10
  {"dnYdgS", "-r13*nzW-r12*nyW-r11*nxW => gS",					  100,-1.0,  1.0,100, -0.020, 0.020}, //11
  {"dnZdaS", "-r23*nzW-r22*nyW-r21*nxW => aS",					  100,-0.7,  0.7,100, -0.005, 0.005}, //12
  {"dnZdbS", "r13*nzW+r12*nyW+r11*nxW => bS",					  100,-1.0,  1.0,100, -0.005, 0.005}, //13
  { "dYdxW", "r21-ly*r11 => xW",						  100,-1.0,  1.0,100, -1.000, 1.000}, //14
  { "dYdyW", "r22-ly*r12 => yW",						  100,-1.5,  1.5,100, -1.000, 1.000}, //15
  //  { "dYdzW", "r23-ly*r13  => zW",						  100,-1.0,  1.0,100, -1.000, 1.000}, //16
  { "dYdaW", "(ly*r12-r22)*ZW+(r23-ly*r13)*YW => aW",				  100,-210,  210,100, -1.000, 1.000}, //17
  { "dYdbW", "(r21-ly*r11)*ZW+(ly*r13-r23)*XW  => bW",				  100, -80,   80,100, -1.000, 1.000}, //18
  { "dYdgW", "(ly*r11-r21)*YW+(r22-ly*r12)*XW => gW",				  100,-125,  125,100, -1.000, 1.000}, //19
  { "dZdxW", "r31-lz*r11 => xW",						  100,-0.3,  0.3,100, -0.700, 0.700}, //20
  { "dZdyW", "r32-lz*r12 => yW",						  100,-0.5,  0.5,100, -0.700, 0.700}, //21
  //  { "dZdzW", "r33-lz*r13  => zW",						  100,-1.0,  1.0,100, -0.700, 0.700}, //22
  { "dZdaW", "(lz*r12-r32)*ZW+(r33-lz*r13)*YW => aW",				  100,-150,  150,100, -0.700, 0.700}, //23
  { "dZdbW", "(r31-lz*r11)*ZW+(lz*r13-r33)*XW => bW",				  100,-200,  200,100, -0.700, 0.700}, //24
  { "dZdgW", "(lz*r11-r31)*YW+(r32-lz*r12)*XW => gW",				  100, -10,   10,100, -0.700, 0.700}, //25
  {"dnXdaW", "r13*nyW-r12*nzW => aW",						  100,-.05, 0.05,100, -0.010, 0.010}, //26
  {"dnXdbW", "r11*nzW-r13*nxW => bW",						  100,-0.5,  0.5,100, -0.010, 0.010}, //27
  {"dnXdgW", "r12*nxW-r11*nyW => gW",						  100,-0.4,  0.4,100, -0.010, 0.010}, //28
  {"dnYdaW", "r23*nyW-r22*nzW => aW",						  100,-0.4,  0.0,100, -0.020, 0.020}, //29
  {"dnYdbW", "r21*nzW-r23*nxW => bW",						  100,-.05, 0.05,100, -0.020, 0.020}, //30
  {"dnYdgW", "r22*nxW-r21*nyW => gW",						  100,-1.0,  1.0,100, -0.020, 0.020}, //31
  {"dnZdaW", "r33*nyW-r32*nzW => aW",						  100,-0.7,  0.7,100, -0.005, 0.005}, //32
  {"dnZdbW", "r31*nzW-r33*nxW => bW",						  100,-1.0,  1.0,100, -0.005, 0.005}, //33
  //  {"dnZdgW", "r32*nxW-r31*nyW => gW",						  100,-1.0,  1.0,100, -0.020, 0.020}, //34
  { "dX"       ,"dX  versus Z",                   				  100, -10,  200,100, -0.050, 0.050}, //35
  { "dY"       ,"dY  versus Z",                   				  100, -10,  200,100, -2.500, 2.500}, //36
  { "dZ"       ,"dZ  versus Z",                   				  100, -10,  200,100, -0.700, 0.700}, //37
  { "dnX"      ,"dnX versus Z",                   				  100, -10,  200,100, -0.010, 0.010}, //38
  { "dnY"      ,"dnY versus Z",                                                   100, -10,  200,100, -0.020, 0.020}, //39
  { "dnZ"      ,"dnZ versus Z",                                                   100, -10,  200,100, -0.005, 0.005}  //40
};
//________________________________________________________________________________
void TpcAlignerDrawIO(const Char_t *files = "*.root", Bool_t laser = kFALSE) {
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcInOutTree");
  while ((file = (Char_t *) Dir.NextFile())) {
    if (TString(file).Contains("MuDst")) continue;
    if (TString(file).Contains("event")) continue;
    iter.AddFile(file); NFiles++;
  }
  const Double_t&    field                                    = iter("field");
#if 0
  const Double_t&    charge                                   = iter("charge");
  const Int_t&       NoFitPoints                              = iter("NoFitPoints");
#endif
  const Double_t&    pX                                       = iter("pX");
  const Double_t&    pY                                       = iter("pY");
  const Double_t&    pZ                                       = iter("pZ");
  const Int_t&       TriggerId                                = iter("TriggerId");
  const Int_t&       In_sector                                = iter("In.sector");
  const Double_t&    In_Rho                                   = iter("In.Rho");
#if 0
  const Double_t&    In_dRho                                  = iter("In.dRho");
#endif
  const Double_t&    In_pxyz_mX1                              = iter("In.nx");
  const Double_t&    In_pxyz_mX2                              = iter("In.ny");
  const Double_t&    In_pxyz_mX3                              = iter("In.nz");
  const Double_t&    In_xyz_mX1                               = iter("In.x");
  const Double_t&    In_xyz_mX2                               = iter("In.y");
  const Double_t&    In_xyz_mX3                               = iter("In.z");
  const Double_t*&   In_fCov                                  = iter("In.fCov[15]");
  const Double_t&    In_Chi2                                  = iter("In.Chi2");
#if 0
  const Int_t&       In_Ndf                                   = iter("In.Ndf");
  const Int_t&       In_Npoints                               = iter("In.Npoints");
  const Int_t&       In_Nused                                 = iter("In.Nused");
#endif
  const Int_t&       Out_sector                               = iter("Out.sector");
  const Double_t&    Out_Rho                                  = iter("Out.Rho");
#if 0
  const Double_t&    Out_dRho                                 = iter("Out.dRho");
#endif
  const Double_t&    Out_pxyz_mX1                             = iter("Out.nx");
  const Double_t&    Out_pxyz_mX2                             = iter("Out.ny");
  const Double_t&    Out_pxyz_mX3                             = iter("Out.nz");
  const Double_t&    Out_xyz_mX1                              = iter("Out.x");
  const Double_t&    Out_xyz_mX2                              = iter("Out.y");
  const Double_t&    Out_xyz_mX3                              = iter("Out.z");
  const Double_t*&   Out_fCov                                 = iter("Out.fCov[15]");
  const Double_t&    Out_Chi2                                 = iter("Out.Chi2");
#if 0
  const Int_t&       Out_Ndf                                  = iter("Out.Ndf");
  const Int_t&       Out_Npoints                              = iter("Out.Npoints");
  const Int_t&       Out_Nused                                = iter("Out.Nused");
#endif
  // Book Histograms
  TString Out(files); 
  Out.ReplaceAll(".root","Plots");
  Out.ReplaceAll("*","");
  //  Out += ".2GeVC";
  if (TMath::Abs(field) > 1)   Out += ".1GeVC";
  if (laser) Out += ".Laser";
  Out += ".Cut";
  Out += ".Errors";
  Out += ".root";
  TFile *fOut = new TFile(Out,"recreate");
  TH3F *plots3D[NPlots];
  for (Int_t i = 0; i < NPlots; i++) {
#if 1
    plots3D[i] = new TH3F(plotNameD[i].Name, plotNameD[i].Title, 24, 0.5, 24.5, 
			  plotNameD[i].nx, plotNameD[i].xmin, plotNameD[i].xmax,
			  plotNameD[i].nz, plotNameD[i].zmin, plotNameD[i].zmax);
#else
    plots3D[i] = new TH3F(plotNameD[i].Name, plotNameD[i].Title, 24, 0.0, 0.0,
			  plotNameD[i].nx, 0.0, 0.0, 
			  plotNameD[i].nz, 0.0, 0.0);
#endif
  }
  TH1D *LSF[24];
  for (Int_t sec = 1; sec <= 24; sec++) LSF[sec-1] = new TH1D(Form("LSF_%02i",sec),Form("Matrix and right part for Least Squared Fit for sector = %02i",sec),28,0,28.);
  Int_t Ntracks = 0;
  while (iter.Next()) {
#if 0
    if     (laser) {if (TriggerId <   9200 || TriggerId >   9201) continue; // not Laser
    } else         {if (TriggerId < 310811 || TriggerId > 310813) continue; // not Cosmic
    }
#endif
    if (In_sector != Out_sector) continue;
    if (In_Chi2 > 5.0 || Out_Chi2 > 5.0) continue;
    TVector3 pxyz(pX,pY,pZ);
    //    if (pxyz.Mag() < 2.0) continue;
    if (TMath::Abs(field) > 1 && pxyz.Mag() < 1.0) continue;
    if (laser && pxyz.Mag() < 100.0) continue;
    //    if (In_Ndf < 15 || Out_Ndf < 15) continue;
    Int_t sector = In_sector;
    TVector3 pxyzIn(In_pxyz_mX1,In_pxyz_mX2,In_pxyz_mX3);  PrPP(pxyzIn);
    TVector3 rI(In_xyz_mX1,In_xyz_mX2,In_xyz_mX3);         PrPP(rI);
    TVector3 pxyzOut(Out_pxyz_mX1,Out_pxyz_mX2,Out_pxyz_mX3);   PrPP(pxyzOut);
    TVector3 rO(Out_xyz_mX1,Out_xyz_mX2,Out_xyz_mX3); PrPP(rO);
    Double_t dev = (TMath::Abs(In_Rho)-TMath::Abs(Out_Rho));
    //    if (TMath::Abs(dev) > 5e-4) continue;
    if (TMath::Abs(dev) > 6e-3) continue; // 3D distortions
    TRSymMatrix CIn(5,In_fCov);                       PrPP(CIn);
    TRSymMatrix COut(5,Out_fCov);                     PrPP(COut);
    TRSymMatrix C(CIn);
    C += COut;                                        PrPP(C);
#if 1
    C(0,0) += 1./2.2e2; // 2e2
    C(1,1) += 1./2.2e2; // 2.1e2
    C(2,2) += 1./5.6e4; // 3.76e4
    C(3,3) += 1e-6;     // 5.9e5
    C(4,4) += 1./3.8e5; // 3e5                              PrPP(C);
#endif
    TRSymMatrix GI(C);
    Int_t iFail = TRSymMatrix::TrchLU(C.GetArray(),GI.GetArray(),5);
    if (iFail != 0) {
      if (! _debug ) {
	_debug = 1;
	PrPP(pxyzIn);  PrPP(rI); PrPP(CIn);
	PrPP(pxyzOut); PrPP(rO); PrPP(COut);
	PrPP(C);
	_debug = 0;
      }
      continue;
    }
    TRSymMatrix G(C,TRArray::kInverted);              PrPP(G);
    TVector3 nI = pxyzIn.Unit();                      PrPP(nI);
    TVector3 nO = pxyzOut.Unit();                     PrPP(nO);
    Double_t norm = nI*nO;
    if (norm <0) nO *= -1;
    TVector3 dn = nO - nI;                            PrPP(dn);
    TVector3 dr = rO - rI;                            PrPP(dr);
    Double_t tX = nO.X()/nO.Y();
    Double_t tZ = nO.Z()/nO.Y();
    TRVector mX(5, dr.X(), dr.Z(), dn.X(), dn.Y(), dn.Z());  PrPP(mX);
#if 1
    if (mX(0) < plotNameD[14].zmin || mX(0) > plotNameD[14].zmax) continue;
    if (mX(1) < plotNameD[16].zmin || mX(1) > plotNameD[16].zmax) continue;
    if (mX(2) < plotNameD[17].zmin || mX(2) > plotNameD[17].zmax) continue;
    if (mX(3) < plotNameD[18].zmin || mX(3) > plotNameD[18].zmax) continue;
    if (mX(4) < plotNameD[19].zmin || mX(4) > plotNameD[19].zmax) continue;
#endif
/*============================== from maxima
    xS yS zS         aS  bS      gS xW  yW zW      aW   bW        gW
dX [-1,lx ,0,-lx*ZW   , -ZW,YW+lx*XW,1,-lx,0,lx*ZW   ,  ZW,-YW-lx*XW],
dZ [ 0,lz,-1,-lz*ZW-YW,  XW,   lz*XW,0,-lz,1,lz*ZW+YW, -XW,   -lz*XW],
dnX[ 0, 0, 0,        0,-nzW,       nyW,  0,0,       0,   0, nzW,-nyW],
dnY[ 0, 0, 0,      nzW,   0,      -nxW,  0,0,    -nzW,   0,      nxW],
dnZ[ 0, 0, 0,     -nyW, nxW,         0,  0,0,     nyW,-nxW,        0] */
    TRMatrix A(5,6,
	       //                 0    1    2                    3         4                   5
	       //                dx   dy   dz                alpha      beta               gamma
	       /* dX 0*/	-1.,  tX,  0.,         -tX*rO.Z() ,  -rO.Z(), rO.Y() + tX*rO.X(),
	       /* dZ 1*/         0.,  tZ, -1., -(rO.Y()+tZ*rO.Z()),   rO.X(),          tZ*rO.X(),
	       /* nX 2*/         0.,  0.,  0.,                 0.0,  -nO.Z(),             nO.Y(),
	       /* nY 3*/         0.,  0.,  0.,              nO.Z(),      0.0,            -nO.X(),
	       /* nZ 4*/         0.,  0.,  0.,             -nO.Y(),   nO.X(),                0.0);  PrPP(A);
    TRVector mGX(G,TRArray::kSxA,mX);  PrPP(mGX);
    TRVector AmX(A,TRArray::kATxB,mGX);  PrPP(AmX);
    TRSymMatrix SX(A,TRArray::kATxSxA,G);   PrPP(SX);
    Int_t sec = sector-1;
    Double_t *array = LSF[sec]->GetArray();
    Double_t *amX = AmX.GetArray();
    Double_t *sX  = SX.GetArray();
    array[0]++;
    Int_t im = 1;
    Int_t is = im + 6;
    if (rO.Z() > 40 && rO.Z() < 200) {
      TCL::vadd(amX,array+im,array+im,6); if (_debug) {TRVector XX(6,array+im);  PrPP(XX);}
      TCL::vadd(sX,array+is,array+is,21); if (_debug) {TRSymMatrix S(6,array+is);  PrPP(S);}
      array[28] += G.Product(mX);
    }
    TRMatrix V(NPlots,2,
	       mX(0)  ,  A(0,1), // "dXdy"     ,"dX  versus  tX         =>  dy", 
	       mX(0)  ,  A(0,3), // "dXdalpha" ,"dX  versus -tX*zO      =>  alpha
	       mX(0)  ,  A(0,4), // "dXdbeta"  ,"dX  versus -zO         =>  beta"
	       mX(0)  ,  A(0,5), // "dXdgamma" ,"dX  versus  yO + tX*xO =>  gamma
	       mX(1)  ,  A(1,1), // "dZdy"     ,"dZ  versus  tZ         =>  dy", 
	       mX(1)  ,  A(1,3), // "dZdalpha" ,"dZ  versus -(yO+tZ*zO) =>  alpha
	       mX(1)  ,  A(1,4), // "dZdbeta"  ,"dZ  versus  xO         =>  beta"
	       mX(1)  ,  A(1,5), // "dZdgamma" ,"dZ  versus  tZ*xO      =>  gamma
	       mX(2)  ,  A(2,4), // "dnXdbeta" ,"dnX versus -nzO        =>  beta"
	       mX(2)  ,  A(2,5), // "dnXdgamma","dnX versus  nyO        =>  gamma
	       mX(3)  ,  A(3,3), // "dnYdalpha","dnY versus  nzO        =>  alpha
	       mX(3)  ,  A(3,5), // "dnYdgamma","dnY versus -nxO        =>  gamma
	       mX(4)  ,  A(4,3), // "dnZdalpha","dnZ versus -nyO        =>  alpha
	       mX(4)  ,  A(4,4), // "dnZdbeta" ,"dnZ versus  nxO        =>  beta"
	       mX(0)  , rO.Z() , // "dX"       ,"dX  versus Z"                   
	       dr.Y() , rO.Z() , // "dY"       ,"dY  versus Z"                   
	       mX(1)  , rO.Z() , // "dZ"       ,"dZ  versus Z"                   
	       mX(2)  , rO.Z() , // "dnX"      ,"dnX versus Z"                   
	       mX(3)  , rO.Z() , // "dnY"      ,"dnY versus Z"                   
	       mX(4)  , rO.Z()   // "dnZ"      ,"dnZ versus Z"                        
	       ); PrPP(V);
    for (Int_t i = 0; i < NPlots; i++) plots3D[i]->Fill(sector, V(i,1), V(i,0));
    if (Ntracks%10000 == 0) {cout << "read track no\t" << Ntracks << endl;}
      Ntracks++;
  }
  fOut->Write();
}
//________________________________________________________________________________
void TpcAlignerDrawW2S(const Char_t *files = "*.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcW2STree");
  while ((file = (Char_t *) Dir.NextFile())) {
    if (TString(file).Contains("MuDst")) continue;
    if (TString(file).Contains("event")) continue;
    iter.AddFile(file); NFiles++;
  }
#if 0
  const Int_t&       TriggerId                                = iter("TriggerId");
  const Double_t*&   RW2S_fScale                              = iter("RW2S.fScale[3]");
  const Int_t&       HelixW_sector                            = iter("HelixW.sector");
  const Double_t&    HelixW_Rho                               = iter("HelixW.Rho");
  const Double_t&    HelixW_dRho                              = iter("HelixW.dRho");
  const Double_t*&   HelixW_fCov                              = iter("HelixW.fCov[15]");
  const Double_t&    HelixW_Chi2                              = iter("HelixW.Chi2");
  const Int_t&       HelixW_Ndf                               = iter("HelixW.Ndf");
  const Int_t&       HelixW_Npoints                           = iter("HelixW.Npoints");
  const Int_t&       HelixW_Nused                             = iter("HelixW.Nused");
  const Double_t&    HelixU_dRho                              = iter("HelixU.dRho");
  const Int_t&       HelixU_Npoints                           = iter("HelixU.Npoints");
  const Int_t&       HelixU_Nused                             = iter("HelixU.Nused");
  const Double_t&    HelixS_dRho                              = iter("HelixS.dRho");
  const Int_t&       HelixS_Npoints                           = iter("HelixS.Npoints");
  const Int_t&       HelixS_Nused                             = iter("HelixS.Nused");
#endif
  const Double_t*&   RW2S_fTranslation                        = iter("RW2S.fTranslation[3]");
  const Double_t*&   RW2S_fRotationMatrix                     = iter("RW2S.fRotationMatrix[9]");
  const Double_t&    XW                                       = iter("HelixW.x");
  const Double_t&    YW                                       = iter("HelixW.y");
  const Double_t&    ZW                                       = iter("HelixW.z");
  const Double_t&    nxW                                      = iter("HelixW.nx");
  const Double_t&    nyW                                      = iter("HelixW.ny");
  const Double_t&    nzW                                      = iter("HelixW.nz");

  const Int_t&       HelixU_sector                            = iter("HelixU.sector");
  const Double_t&    HelixU_Rho                               = iter("HelixU.Rho");
  const Double_t&    XU                                       = iter("HelixU.x");
  const Double_t&    YU                                       = iter("HelixU.y");
  const Double_t&    ZU                                       = iter("HelixU.z");
  const Double_t&    nxU                                      = iter("HelixU.nx");
  const Double_t&    nyU                                      = iter("HelixU.ny");
  const Double_t&    nzU                                      = iter("HelixU.nz");
  const Double_t*&   HelixU_fCov                              = iter("HelixU.fCov[15]");
  const Double_t&    HelixU_Chi2                              = iter("HelixU.Chi2");
  const Int_t&       HelixU_Ndf                               = iter("HelixU.Ndf");
  
  const Int_t&       HelixS_sector                            = iter("HelixS.sector");
  const Double_t&    HelixS_Rho                               = iter("HelixS.Rho");
  const Double_t&    XS                                       = iter("HelixS.x");
  const Double_t&    YS                                       = iter("HelixS.y");
  const Double_t&    ZS                                       = iter("HelixS.z");
  const Double_t&    nxS                                      = iter("HelixS.nx");
  const Double_t&    nyS                                      = iter("HelixS.ny");
  const Double_t&    nzS                                      = iter("HelixS.nz");
  const Double_t*&   HelixS_fCov                              = iter("HelixS.fCov[15]");
  const Double_t&    HelixS_Chi2                              = iter("HelixS.Chi2");
  const Int_t&       HelixS_Ndf                               = iter("HelixS.Ndf");
  // Book Histograms
  TString Out(files); 
  Out.ReplaceAll(".root","W2S");
  Out.ReplaceAll("*","");
  Out += ".root";
  TFile *fOut = new TFile(Out,"recreate");
  TH3F *plots3D[NwsPlots];
  for (Int_t i = 0; i < NwsPlots; i++) {
#if 1
    plots3D[i] = new TH3F(plotNameWS[i].Name, plotNameWS[i].Title, 24, 0.5, 24.5, 
			  plotNameWS[i].nx, plotNameWS[i].xmin, plotNameWS[i].xmax,
			  plotNameWS[i].nz, plotNameWS[i].zmin, plotNameWS[i].zmax);
#else
    plots3D[i] = new TH3F(plotNameWS[i].Name, plotNameWS[i].Title, 24, 0.5, 24.5, 
			  plotNameWS[i].nx, 0.,0.,
			  plotNameWS[i].nz, 0.,0.);
#endif
  }
  Int_t NP  = 6*24; // Total no. of parameters
  Int_t NPP = NP*(NP+1)/2; // size of matrix
  Int_t NT  = NP + NPP; // bin0 -> no. of entries; bin_NT+1 = chi2
  TH1D *LSF = new TH1D("LSF","Matrix and right part for Least Squared Fit ",NT,0,NT);
  Int_t Ntracks = 0;
  while (iter.Next()) {
    Int_t w = HelixU_sector;
    Int_t s = HelixS_sector;
    if (w < 1 || w > 24 ||
	s < 1 || w > 24) continue;
    if (HelixU_Ndf < 15 || HelixS_Ndf < 15) continue;
    if (HelixU_Chi2/ HelixU_Ndf > 5 || HelixS_Chi2/HelixS_Ndf > 5) continue;
    if (XS > 160 || XU > 160)               continue; // don't use end of tracks
#if 1
    Double_t RhoU = TMath::Abs(HelixU_Rho);
    Double_t RhoS = TMath::Abs(HelixS_Rho);
    if (RhoU > 1.5e-3 || RhoS > 1.5e-3) continue; // 1GeV/c cut
    Double_t dRho = RhoU - RhoS;
    if (TMath::Abs(dRho) > 6e-3) continue; // 3D distortions
#endif
    TRSymMatrix CU(5,HelixU_fCov);                  PrPP(CU);
    TRSymMatrix CS(5,HelixS_fCov);                  PrPP(CS);
    TRSymMatrix C(CU);
    C += CS;   PrPP(C);
#if 0
    static Double_t DY = 0.913;
    static Double_t DZ = 0.2518;
    static Double_t DnX = 3.431e-3;
    static Double_t DnY = 6.446e-3;
    static Double_t DnZ = 1.153e-3;
    C(0,0) += DY*DY;
    C(1,1) += DZ*DZ;
    C(2,2) += DnX*DnX;
    C(3,3) += DnY*DnY;
    C(4,4) += DnZ*DnZ; PrPP(C);
#endif
#if 0
#if 0
    TRSymMatrix GI(C);
    Int_t iFail = TRSymMatrix::TrchLU(C.GetArray(),GI.GetArray(),5);
    if (iFail != 0) {
      if (! _debug ) {
	PrPP(CS); PrPP(CU); PrPP(C);
      }
      continue;
    }
#else
    TRSymMatrix GI(5, 
		   1.,
		   0., 1.,
		   0., 0., 1.,
		   0., 0., 0., 1.,
		   0., 0., 0., 0., 1.);
#endif
#endif
    TRSymMatrix G(C,TRArray::kInverted);              PrPP(G);
    TVector3 nW(nxW,nyW,nzW);                         PrPP(nW);
    TVector3 nU(nxU,nyU,nzU);                         PrPP(nU);
    TVector3 nS(nxS,nyS,nzS);                         PrPP(nS);
    TVector3 rW(XW,YW,ZW);                         PrPP(rW);
    TVector3 rU(XU,YU,ZU);                         PrPP(rU);
    TVector3 rS(XS,YS,ZS);                         PrPP(rS);

    TVector3 dn = nS - nU;                            PrPP(dn);
    TVector3 dr = rS - rU;                            PrPP(dr);
    TRVector mX(5, dr.Y(), dr.Z(), dn.X(), dn.Y(), dn.Z());  PrPP(mX);
#if 1
    Int_t iok = 0;
    for (Int_t i = 0; i < 5; i++) {
      if (mX(i) < plotNameWS[NwsPlots-5+i].zmin || mX(i) > plotNameWS[NwsPlots-5+i].zmax) {iok = -1 - i; break;}
    }
    if (iok) continue;
#endif
    const Double_t &r11 = *&RW2S_fRotationMatrix[0];
    const Double_t &r12 = *&RW2S_fRotationMatrix[1];
    const Double_t &r13 = *&RW2S_fRotationMatrix[2];
    const Double_t &r21 = *&RW2S_fRotationMatrix[3];
    const Double_t &r22 = *&RW2S_fRotationMatrix[4];
    const Double_t &r23 = *&RW2S_fRotationMatrix[5];
    const Double_t &r31 = *&RW2S_fRotationMatrix[6];
    const Double_t &r32 = *&RW2S_fRotationMatrix[7];
    const Double_t &r33 = *&RW2S_fRotationMatrix[8];
    const Double_t &tx  = *&RW2S_fTranslation[0];
    const Double_t &ty  = *&RW2S_fTranslation[1];
    const Double_t &tz  = *&RW2S_fTranslation[2];
    if (_debug) {
      TGeoHMatrix T;
      T.SetRotation(RW2S_fRotationMatrix);
      T.SetTranslation(RW2S_fTranslation);
      T.Print();
      cout << r11 << "\t" << r12 << "\t" << r13 << "\t" << tx << endl;
      cout << r21 << "\t" << r22 << "\t" << r23 << "\t" << ty << endl;
      cout << r31 << "\t" << r32 << "\t" << r33 << "\t" << tz  <<endl;
    }
    const Double_t ly = nU.Y()/nU.X();
    const Double_t lz = nU.Z()/nU.X();
    /*============================== from maxima
  [xS, yS, zS,                      aS,                                                      bS,                                                          gS],
dY[ly,-1., 0., r33*ZW+r32*YW+r31*XW+tz,                     ly*r33*ZW+ly*r32*YW+ly*r31*XW+ly*tz, (-ly*r23-r13)*ZW+(-ly*r22-r12)*YW+(-ly*r21-r11)*XW-ly*ty-tx], 
dZ[lz, 0.,-1.,-r23*ZW-r22*YW-r21*XW-ty,(lz*r33+r13)*ZW+(lz*r32+r12)*YW+(lz*r31+r11)*XW+lz*tz+tx,                        -lz*r23*ZW-lz*r22*YW-lz*r21*XW-lz*ty],
dn[0., 0., 0.,                      0.,                                -r33*nzW-r32*nyW-r31*nxW,                                     r23*nzW+r22*nyW+r21*nxW],
dn[0., 0., 0., r33*nzW+r32*nyW+r31*nxW,                                                      0.,                                    -r13*nzW-r12*nyW-r11*nxW],
dn[0., 0., 0.,-r23*nzW-r22*nyW-r21*nxW,                                 r13*nzW+r12*nyW+r11*nxW,                                                          0.] 
  [        xW,         yW,         zW,                              aW,                              bW,                              gW]   
dY[r21-ly*r11, r22-ly*r12, r23-ly*r13, (ly*r12-r22)*ZW+(r23-ly*r13)*YW, (r21-ly*r11)*ZW+(ly*r13-r23)*XW, (ly*r11-r21)*YW+(r22-ly*r12)*XW],  
dZ[r31-lz*r11, r32-lz*r12, r33-lz*r13, (lz*r12-r32)*ZW+(r33-lz*r13)*YW, (r31-lz*r11)*ZW+(lz*r13-r33)*XW, (lz*r11-r31)*YW+(r32-lz*r12)*XW],   
dn[        0.,         0.,         0.,                 r13*nyW-r12*nzW,                 r11*nzW-r13*nxW,                 r12*nxW-r11*nyW],  
dn[        0.,         0.,         0.,                 r23*nyW-r22*nzW,                 r21*nzW-r23*nxW,                 r22*nxW-r21*nyW], 
dn[        0.,         0.,         0.,                 r33*nyW-r32*nzW,                 r31*nzW-r33*nxW,                 r32*nxW-r31*nyW]
    */
    TRMatrix A_ws(5,6,
		  /*     xS, yS, zS,                      aS,                                                      bS,                                                          gS*/
		  /*dY */ly,-1., 0., r33*ZW+r32*YW+r31*XW+tz,                     ly*r33*ZW+ly*r32*YW+ly*r31*XW+ly*tz, (-ly*r23-r13)*ZW+(-ly*r22-r12)*YW+(-ly*r21-r11)*XW-ly*ty-tx,
		  /*dZ */lz, 0.,-1.,-r23*ZW-r22*YW-r21*XW-ty,(lz*r33+r13)*ZW+(lz*r32+r12)*YW+(lz*r31+r11)*XW+lz*tz+tx,                        -lz*r23*ZW-lz*r22*YW-lz*r21*XW-lz*ty,
		  /*dnX*/0., 0., 0.,                      0.,                                -r33*nzW-r32*nyW-r31*nxW,                                     r23*nzW+r22*nyW+r21*nxW,
		  /*dnY*/0., 0., 0., r33*nzW+r32*nyW+r31*nxW,                                                      0.,                                    -r13*nzW-r12*nyW-r11*nxW,
		  /*dnZ*/0., 0., 0.,-r23*nzW-r22*nyW-r21*nxW,                                 r13*nzW+r12*nyW+r11*nxW,                                                          0.); 
    PrPP(A_ws);
    TRMatrix B_ws(5,6,
		  /*             xW,         yW,         zW,                              aW,                              bW,                              gW*/
		  /*dY */r21-ly*r11, r22-ly*r12, r23-ly*r13, (ly*r12-r22)*ZW+(r23-ly*r13)*YW, (r21-ly*r11)*ZW+(ly*r13-r23)*XW, (ly*r11-r21)*YW+(r22-ly*r12)*XW,
		  /*dZ */r31-lz*r11, r32-lz*r12, r33-lz*r13, (lz*r12-r32)*ZW+(r33-lz*r13)*YW, (r31-lz*r11)*ZW+(lz*r13-r33)*XW, (lz*r11-r31)*YW+(r32-lz*r12)*XW,
		  /*dnX*/        0.,         0.,         0.,                 r13*nyW-r12*nzW,                 r11*nzW-r13*nxW,                 r12*nxW-r11*nyW,
		  /*dnY*/        0.,         0.,         0.,                 r23*nyW-r22*nzW,                 r21*nzW-r23*nxW,                 r22*nxW-r21*nyW,
		  /*dnZ*/        0.,         0.,         0.,                 r33*nyW-r32*nzW,                 r31*nzW-r33*nxW,                 r32*nxW-r31*nyW);
    PrPP(B_ws);
    TRMatrix A(5,NP);
    for (Int_t i = 0; i < 5; i++) 
      for (Int_t j = 0; j < 6; j++) {
	Int_t ks = 6*(s-1);
	A(i,ks+j) = A_ws(i,j);
	Int_t kw = 6*(w-1);
	A(i,kw+j) = B_ws(i,j);
      }
    PrPP(A);
    TRVector mGX(G,TRArray::kSxA,mX);  PrPP(mGX);
    TRVector AmX(A,TRArray::kATxB,mGX);  PrPP(AmX);
    TRSymMatrix SX(A,TRArray::kATxSxA,G);   PrPP(SX);
    Double_t *array = LSF->GetArray();
    Double_t *amX = AmX.GetArray();
    Double_t *sX  = SX.GetArray();
    array[0]++;
    Int_t im = 1;
    Int_t is = im + NP;
    //    if (rS.Z() > 40 && rS.Z() < 200) {
      TCL::vadd(amX,array+im,array+im,NP); if (_debug) {TRVector XX(NP,array+im);  PrPP(XX);}
      TCL::vadd(sX,array+is,array+is,NPP); if (_debug) {TRSymMatrix S(NP,array+is);  PrPP(S);}
      array[NT+1] += G.Product(mX);        if (_debug) {PrPP(array[NT+1]);}
      //    }
    Double_t dw = w;
    Double_t ds = s;
    TRMatrix V(NwsPlots,3,
	       mX(0), A_ws(0,0), ds, //  "dYdxS", "ly => xS",								       
	       mX(0), A_ws(0,3), ds, //  "dYdaS", "r33*ZW+r32*YW+r31*XW+tz => aS",
	       mX(0), A_ws(0,4), ds, //  "dYdbS", "ly*r33*ZW+ly*r32*YW+ly*r31*XW+ly*tz => bS",
	       mX(0), A_ws(0,5), ds, //  "dYdgS", "(-ly*r23-r13)*ZW+(-ly*r22-r12)*YW+(-ly*r21-r11)*XW-ly*ty-tx => gS",
	       mX(1), A_ws(1,0), ds, //  "dZdxS", "lz => xS",								       
	       mX(1), A_ws(1,3), ds, //  "dZdaS", "-r23*ZW-r22*YW-r21*XW-ty => aS",
	       mX(1), A_ws(1,4), ds, //  "dZdbS", "(lz*r33+r13)*ZW+(lz*r32+r12)*YW+(lz*r31+r11)*XW+lz*tz+tx => bS",
	       mX(1), A_ws(1,5), ds, //  "dZdgS", "-lz*r23*ZW-lz*r22*YW-lz*r21*XW-lz*ty => gS",				       
	       mX(2), A_ws(2,4), ds, // "dnXdbS", "-r33*nzW-r32*nyW-r31*nxW => bS",					       
	       mX(2), A_ws(2,5), ds, // "dnXdgS", "r23*nzW+r22*nyW+r21*nxW => gS",					       10
	       mX(3), A_ws(3,3), ds, // "dnYdaS", "r33*nzW+r32*nyW+r31*nxW => aS",					       
	       mX(3), A_ws(3,5), ds, // "dnYdgS", "-r13*nzW-r12*nyW-r11*nxW => gS",					       
	       mX(4), A_ws(4,3), ds, // "dnZdaS", "-r23*nzW-r22*nyW-r21*nxW => aS",					       
	       mX(4), A_ws(4,4), ds, // "dnZdbS", "r13*nzW+r12*nyW+r11*nxW => bS",					       
	       mX(0), B_ws(0,0), dw, //  "dYdxW", "r21-ly*r11 => xW",							       
	       mX(0), B_ws(0,1), dw, //  "dYdyW", "r22-ly*r12 => yW",							       
	       //	       mX(0), B_ws(0,2), dw, //  "dYdzW", "r23-ly*r13  => zW",							       
	       mX(0), B_ws(0,3), dw, //  "dYdaW", "(ly*r12-r22)*ZW+(r23-ly*r13)*YW => aW",				       
	       mX(0), B_ws(0,4), dw, //  "dYdbW", "(r21-ly*r11)*ZW+(ly*r13-r23)*XW  => bW",				       
	       mX(0), B_ws(0,5), dw, //  "dYdgW", "(ly*r11-r21)*YW+(r22-ly*r12)*XW => gW",				       20
	       mX(1), B_ws(1,0), dw, //  "dZdxW", "r31-lz*r11 => xW",							       
	       mX(1), B_ws(1,1), dw, //  "dZdyW", "r32-lz*r12 => yW",							       
	       //	       mX(1), B_ws(1,2), dw, //  "dZdzW", "r33-lz*r13  => zW",							       
	       mX(1), B_ws(1,3), dw, //  "dZdaW", "(lz*r12-r32)*ZW+(r33-lz*r13)*YW => aW",				       
	       mX(1), B_ws(1,4), dw, //  "dZdbW", "(r31-lz*r11)*ZW+(lz*r13-r33)*XW => bW",				       
	       mX(1), B_ws(1,5), dw, //  "dZdgW", "(lz*r11-r31)*YW+(r32-lz*r12)*XW => gW",				       
	       mX(2), B_ws(2,3), dw, // "dnXdaW", "r13*nyW-r12*nzW => aW",						       
	       mX(2), B_ws(2,4), dw, // "dnXdbW", "r11*nzW-r13*nxW => bW",						       
	       mX(2), B_ws(2,5), dw, // "dnXdgW", "r12*nxW-r11*nyW => gW",						       
	       mX(3), B_ws(3,3), dw, // "dnYdaW", "r23*nyW-r22*nzW => aW",						       
	       mX(3), B_ws(3,4), dw, // "dnYdbW", "r21*nzW-r23*nxW => bW",						       
	       mX(3), B_ws(3,5), dw, // "dnYdgW", "r22*nxW-r21*nyW => gW",						       
	       mX(4), B_ws(4,3), dw, // "dnZdaW", "r33*nyW-r32*nzW => aW",						       
	       mX(4), B_ws(4,4), dw, // "dnZdbW", "r31*nzW-r33*nxW => bW",						       
	       //	       mX(4), B_ws(4,5), dw, // "dnZdgW", "r32*nxW-r31*nyW => gW",						       
	       dr.X(),   rS.Z(),  ds, // "dX"       ,"dX  versus Z",                   					       
	       mX(0),    rS.Z(),  ds, // "dY"       ,"dY  versus Z",                   					       
	       mX(1) ,   rS.Z(),  ds, // "dZ"       ,"dZ  versus Z",                   					       
	       mX(2) ,   rS.Z(),  ds, // "dnX"      ,"dnX versus Z",                   					       
	       mX(3) ,   rS.Z(),  ds, // "dnY"      ,"dnY versus Z",                                                             
	       mX(4) ,   rS.Z(),  ds  // "dnZ"      ,"dnZ versus Z",                                                           
	       ); PrPP(V);
    for (Int_t i = 0; i < NwsPlots; i++) plots3D[i]->Fill(V(i,2), V(i,1), V(i,0));
    if (Ntracks%10000 == 0) {cout << "read track no\t" << Ntracks << endl;}
    Ntracks++;
    //    if (Ntracks > 100000) break;
  }
  fOut->Write();

}
//________________________________________________________________________________
Double_t g2g(Double_t *xx, Double_t *par) {
  Double_t x = xx[0];
  Double_t A = TMath::Exp(par[0]);
  Double_t mu1  = par[1];
  Double_t sig1 = par[2];
  if (A < 1 && TMath::Abs(x -mu1) < 3*sig1) {TF1::RejectPoint(); return 0;}
  Double_t B    = TMath::Exp(par[3]);
  Double_t mu2  = par[4];
  Double_t sig2 = par[5];
  Double_t gra  = par[6];
  Double_t dev1 = (x - mu1)/sig1;
  Double_t dev2 = (x - mu2)/sig2;
  Double_t value = A*TMath::Exp(-0.5*dev1*dev1) + B*TMath::Exp(-0.5*dev2*dev2) + gra;
  return value;
}
//________________________________________________________________________________
void TDrawIO() {
  TMinuitMinimizer::UseStaticMinuit();
  if (! gMinuit) new TMinuit(10);
  gMinuit->SetPrintLevel(-2);
#if 1
  TF1 *gp = new TF1("gp",g2g,-100,100,7);
  struct Par_t {
    const Char_t *Name;
    Double_t p, pmin, pmax;
  };
  const Par_t par[7] = {
    {"logN1",    5.,    0.,   25.},
    {"mu1",      0.,   -1.,    1.},
    {"sigma1",0.01, 0.001,   0.10},
    {"logN2",    1.,    0.,   25.},
    {"mu2",      0.,   -1.,    1.},
    {"sigma2", 0.10,  0.01,    1.},
    {"grass",   0.0,  0.00,    1.}
  };
  for (Int_t i = 0; i < 7; i++) {
    gp->SetParName(i,par[i].Name);
    gp->SetParameter(i,par[i].p);
    gp->SetParLimits(i,par[i].pmin, par[i].pmax);
  }
#else
  TF1 *gp = new TF1("gp","gaus(0)",-100,100);
  gp->SetParameters(100.,0.,1.);
#endif
  Int_t nx = 24;
  Int_t ny = NFPlots;
  Int_t scaleX = 800/nx;
  Int_t scaleY = 600/ny;
  //  Int_t scale  = TMath::Min(scaleX,scaleY);
  TCanvas *c1 = new TCanvas("TpcInOut","TpcInOut Alignment" ,10,10,10+scaleX*nx,10+scaleY*ny);
  cout << "nx/ny = " << nx << "/" << ny << endl;
  c1->Divide(ny,nx);
  TString line("");
  TString lTitle("");
  TString lineC("");
  ofstream out;
  ofstream outC;
  TString Out("Results.IO_");
  Out += gSystem->BaseName(gDirectory->GetName());
  Out.ReplaceAll(".root","");
  Out.ReplaceAll("*","");
  //  Out += t.AsString();
  Out.ReplaceAll(" ","");
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  Out += ".h";
  if (gSystem->AccessPathName(Out)) outC.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              outC.open(Out, ios::app);
  Int_t head = 0;
  for (Int_t i = 1; i <= nx; i++) {
    if (! head) {
      out  <<  "_______________________________________________________________________________________________________"  << endl;
      out  <<  "| x mkm         | y mkm         | z mkm         |alpha mrad     |beta mrad      |gamma mrad     |Comment" << endl;
      cout <<  "_______________________________________________________________________________________________________"  << endl;
      cout <<  "| x mkm         | y mkm         | z mkm         |alpha mrad     |beta mrad      |gamma mrad     |Comment" << endl;
      outC << "struct data_t {" << endl;
      outC << "\tInt_t sector;" << endl;
      outC << "\tDouble_t x, Dx, y, Dy, z, Dz, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;" << endl;
      outC << "\tconst Char_t *Comment;" << endl;
      outC << "};" << endl;
      outC << "data_t Data[] = {" << endl;
    }
    head++;
    Int_t sector = i;
    out  << "__________________________________________________________________________________________________ " << sector << endl;
    cout << "__________________________________________________________________________________________________ " << sector << endl;
    TH1D *LSF = (TH1D *) gDirectory->Get(Form("LSF_%02i",sector));
    struct Val_t {
      Double_t val;
      Double_t valError;
      Int_t    iFlag;
    };
    Val_t ValA[7]; memset (ValA, 0, sizeof(ValA));
    if (LSF) {
      Double_t *array = LSF->GetArray();
      Int_t NP = array[0];
      if (NP > 0) {
	Double_t yTy = array[28];
	Int_t im = 1;
	Int_t is = im + 6;
	TRVector AmX(6,array+im);  // cout << "AmX " << AmX << endl;
	TRSymMatrix S(6,array+is); // cout << "S " << S << endl;
	//#define FREEZE_ALPHA_BETA
#ifdef  FREEZE_ALPHA_BETA
	for (Int_t i = 3; i < 5; i++) {
	  AmX(i) = 0;
	  for (Int_t j = 0; j < 5; j++) {
	    S(j,i) = 0;
	  }
	}
	// cout << "AmX " << AmX << endl;
	// cout << "S " << S << endl;
#else /* ! FREEZE_ALPHA_BETA */
	//#define FREEZE_BETA
#ifdef  FREEZE_BETA
	for (Int_t i = 4; i < 5; i++) {
	  AmX(i) = 0;
	  for (Int_t j = 0; j < 5; j++) {
	    S(j,i) = 0;
	  }
	}
	// cout << "AmX " << AmX << endl;
	// cout << "S " << S << endl;
#endif /* FREEZE_BETA */
#endif /* FREEZE_ALPHA_BETA */
	TRSymMatrix SInv(S,TRArray::kInverted);  // cout << "SInv " << SInv << endl;
	TRVector  X(SInv,TRArray::kSxA,AmX);     // cout << "X " << X << endl;
	Double_t chi2 = yTy;
	chi2 -= AmX*X;
	//	Double_t mSIm = SInv.Product(AmX); 
	// cout << "chi2 = " << chi2 << "/ NDF = " << NP << " mS-1m " << mSIm << endl;
	line = "";
	for (Int_t m = 0; m < 6; m++) {
	  if (SInv(m,m) > 0) {
	    Double_t scale = 1e4;   // => mkm
	    if (m > 2) scale = 1e3; // => mrad
	    ValA[m].val =  scale*X(m);
	    ValA[m].valError = scale*TMath::Sqrt(SInv(m,m));
	    ValA[m].iFlag = 1;
	  } else {
	    ValA[m].val = ValA[m].valError = 0; ValA[m].iFlag = 0;
	  }
	  line  += Form("|%7.2f+-%5.2f ", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
	}
      }
    }
    cout << line << endl;
    out << line << endl;
    for (Int_t j = 0; j < NFPlots; j++) {
      TH3 *h3 = (TH3 *) gDirectory->Get(plotNameD[j].Name);
      if (! h3) continue;
      Int_t ij = j + 1 + ny*(i-1);
      c1->cd(ij)->SetLogz(1);
      TH1 *fit = 0;
      h3->GetXaxis()->SetRange(sector,sector);
      TH1 *sp = h3->Project3D("z");
      if (sp->GetEntries() < 100) continue;
      sp->Fit("gaus","qem");
      Double_t Mu = 0;
      Double_t dMu = 0;
      Mu = sp->GetFunction("gaus")->GetParameter(1);
      dMu = sp->GetFunction("gaus")->GetParError(1);
      TH2 *h = (TH2 *) h3->Project3D("zy");
      h->SetName(Form("%s_%i",h->GetName(),sector));
      h->FitSlicesY(0,0,-1,10,"qeg3s");
      //      SlicesYFit(h,0,0,10,"qnig3");
      fit = (TH1 *) gDirectory->Get(Form("%s_1",h->GetName()));
      //       TH1 *sig = (TH1 *) gDirectory->Get(Form("%s_2",h->GetName()));
      //       TH1 *gra = (TH1 *) gDirectory->Get(Form("%s_3",h->GetName()));
      Double_t slope = 0;
      Double_t dslope = 0;
      TLegend *leg = new TLegend(0.1,0.2,0.6,0.3,"");
      lTitle = "";
      leg->SetTextSize(0.025);
      if (fit) {
	fit->SetTitle(h->GetTitle());
	fit->SetMarkerStyle(20);
	fit->SetMarkerColor(1);
	fit->SetMaximum(0.2);
	fit->SetMinimum(-.2);
	fit->SetStats(1);
	fit->Fit("pol1","qe");
	TF1 *pol1 = fit->GetFunction("pol1");
	if (! pol1 ) goto endhLoop;
	Double_t prob = pol1->GetProb();
	if (prob >= 0) {
#if 1
	  Double_t xm = fit->GetMean();
	  Mu     = pol1->Eval(xm);
	  dMu    = TMath::Sqrt(pol1->GetParError(0)*pol1->GetParError(0) + (xm*pol1->GetParError(1))*(xm*pol1->GetParError(1)));
	  if (dMu > 99.99e-4) dMu=  99.99e-4;
#endif
	  slope  = pol1->GetParameter(1);
	  dslope = pol1->GetParError(1);
	  if (dslope > 99.99e-3) dslope = 99.99e-3;
	} else {
	  Mu = slope = 0;
	  dMu = dslope = 0;
	}
	Val_t Vals[6]; memset (Vals, 0, sizeof(Vals));
	static const Char_t *dxv[3] = {"dX", "dY","dZ"};
	TString Name(h->GetName());
	TString Title(h->GetTitle());
	for (Int_t m = 0; m < 3; m++) {
	  if (Name.BeginsWith(dxv[m]) && dMu > 0 && dMu < 99.99e-4) {
	    Vals[m].val =     -1e4*Mu;
	    Vals[m].valError = 1e4*dMu;
	    Vals[m].iFlag = 1;
	    lTitle += Form(" %s = %7.2f+-%5.2f (#mum)", dxv[m],Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	  }
	}	  
	Int_t index = Title.Index("=>");
	TString tag("");
	if (index >= 0) {
	  index = index+2;
	  static TString separator("[^ ;,]+");
	  TString t(Title.Data()+index);
	  TObjArray *array = t.Tokenize(separator);
	  tag = ((TObjString *) array->At(0))->GetString();
	  delete array;
	}
	static const Char_t *lvar[6] = {"dx","dy","dz","#alpha","#beta","#gamma"};
	for (Int_t m = 0; m < 6; m++) {
	  if (dslope <= 0) continue;
	  if (m == 1 && tag.Contains("dy") && dslope < 99.99e-4) {
	    Vals[m].val =      1e4*slope;
	    Vals[m].valError = 1e4*dslope;
	    Vals[m].iFlag = 1;
	    lTitle += Form(" %s = %7.2f+-%5.2f (#mum)", lvar[m],Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	  } else if (dslope < 99.99e-3) {
	    if ((m == 3 && tag.Contains("alpha")) ||
		(m == 4 && tag.Contains("beta"))  ||
		(m == 5 &&  tag.Contains("gamma"))) {
	      Vals[m].val =      1e3*slope;
	      Vals[m].valError = 1e3*dslope;
	      Vals[m].iFlag = 1;
	      lTitle += Form(" %s = %7.2f+-%5.2f (mrad)", lvar[m],Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	    }
	  }
	}
	lTitle += Form(" prob = %5.3f",prob);
	leg->AddEntry(pol1,lTitle);
	line = "";
	lineC = Form("\t{%2i",sector);
	for (Int_t m = 0; m < 6; m++) {
	  if (! Vals[m].iFlag) {
	    line  += "|               ";
	    lineC += ",      0,-9.99";
	  } else {
	    line  += Form("|%7.2f+-%5.2f ", Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	    lineC += Form(",%7.2f,%5.2f", Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
#define __AVERAGE_IO__
#ifdef  __AVERAGE_IO__
	    if (ValA[m].iFlag) {
	      Double_t w0 = 1./(Vals[m].valError*Vals[m].valError);
	      Double_t w1 = 1./(ValA[m].valError*ValA[m].valError);
	      ValA[m].val = (w0*Vals[m].val + w1*ValA[m].val)/(w0 + w1);
	      ValA[m].valError = 1./TMath::Sqrt(w0 + w1);
	      ValA[m].iFlag++;
	    } else {
	      ValA[m] = Vals[m];
	    }
#endif
	  }
	}
	line += "|"; line += fit->GetName(); line += "/"; line += h->GetTitle(); 
	lineC += ",\""; lineC += fit->GetName(); lineC += "\"},";
	cout << line << endl;
	out << line << endl;
      }
    endhLoop:
      if (h) h->Draw("colz");
      if (fit) {
	fit->Draw("same"); 
	TF1 *pol1 = fit->GetFunction("pol1"); 
	if (pol1) {pol1->SetLineColor(2); pol1->Draw("same");}
      }
      leg->Draw();
    }
    out  << "__________________________________________________________________________________________________ " << sector << endl;
    cout << "__________________________________________________________________________________________________ " << sector << endl;
    line = ""; 
    lineC = Form("\t{%2i",sector);
    for (Int_t m = 0; m < 6; m++) {
      if (! ValA[m].iFlag 
#ifdef   FREEZE_BETA  
	  || (m == 4) 
#endif
#ifdef   FREEZE_ALPHA_BETA  
	  || (m == 3 || m == 4) 
#endif
	  ) {
	line  += "|               ";
	lineC += ",      0,-9.99";
      } else {
	line  += Form("|%7.2f+-%5.2f ", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
	lineC += Form(",%7.2f,%5.2f", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
      }
    }
#ifdef __AVERAGE_IO__
    lineC += ",\"Average ";
#else
    lineC += ",\"LSF ";
#endif
#ifdef   FREEZE_ALPHA_BETA  
    lineC += " Fixed alpha beta";
#endif
#ifdef   FREEZE_BETA  
    lineC += ",\"LSF Fixed beta";
#endif
    lineC += "\"},";
    cout << line << endl;
    out << line << endl;
    outC << lineC << endl;
  }
  out.close();
  outC.close();
}
//________________________________________________________________________________
void TDrawW2S() {
  TMinuitMinimizer::UseStaticMinuit();
  if (! gMinuit) new TMinuit(10);
  gMinuit->SetPrintLevel(-2);
#if 1
  TF1 *gp = new TF1("gp",g2g,-100,100,7);
  struct Par_t {
    const Char_t *Name;
    Double_t p, pmin, pmax;
  };
  const Par_t par[7] = {
    {"logN1",    5.,    0.,   25.},
    {"mu1",      0.,   -1.,    1.},
    {"sigma1",0.01, 0.001,   0.10},
    {"logN2",    1.,    0.,   25.},
    {"mu2",      0.,   -1.,    1.},
    {"sigma2", 0.10,  0.01,    1.},
    {"grass",   0.0,  0.00,    1.}
  };
  for (Int_t i = 0; i < 7; i++) {
    gp->SetParName(i,par[i].Name);
    gp->SetParameter(i,par[i].p);
    gp->SetParLimits(i,par[i].pmin, par[i].pmax);
  }
#else
  TF1 *gp = new TF1("gp","gaus(0)",-100,100);
  gp->SetParameters(100.,0.,1.);
#endif
  Int_t nx = 24;
  Int_t ny = NwsFPlots;
  Int_t scaleX = 800/nx;
  Int_t scaleY = 600/ny;
  //  Int_t scale  = TMath::Min(scaleX,scaleY);
  TCanvas *c1 = new TCanvas("TpcWS","Tpc Sector to Sector Alignment" ,10,10,10+scaleX*nx,10+scaleY*ny);
  cout << "nx/ny = " << nx << "/" << ny << endl;
  c1->Divide(ny,nx);
  TString line("");
  TString lTitle("");
  TString lineC("");
  ofstream out;
  ofstream outC;
  TString Out("Results.W2S_NoLSFg5");
  Out += gSystem->BaseName(gDirectory->GetName());
  Out.ReplaceAll(".root","");
  //  Out += t.AsString();
  Out.ReplaceAll(" ","");
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  Out += ".h";
  if (gSystem->AccessPathName(Out)) outC.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              outC.open(Out, ios::app);
  TH1D *LSF = (TH1D *) gDirectory->Get("LSF");
  Double_t *array = LSF->GetArray();
  Int_t NEntries = array[0];
  if (NEntries  < 1000) return;
  Int_t NP  = 6*24; // Total no. of parameters
  Int_t NPP = NP*(NP+1)/2; // size of matrix
  Int_t NT  = NP + NPP; // bin0 -> no. of entries; bin_NT+1 = chi2
  Double_t yTy = array[NT+1];
  Int_t im = 1;
  Int_t is = im + NP;
  TRVector AmX(NP,array+im);  PrPP(AmX);
  TRSymMatrix S(NP,array+is); PrPP(S);
#if 0
  TRSymMatrix SInv(S);
  Int_t iFail = TRSymMatrix::TrchLU(S.GetArray(),SInv.GetArray(),5);
  if (iFail) {
    cout << "TRSymMatrix Invertion failed" << endl;
    SInv = TRSymMatrix(S,TRArray::kInvertedPosDef);  PrPP(SInv);
  } else {
    SInv = TRSymMatrix(S,TRArray::kInverted);  PrPP(SInv);
  }
#else
  TRSymMatrix SInv(S,TRArray::kInverted); PrPP(SInv);
  if (_debug) {
    TRSymMatrix P(NP);
    for (Int_t i = 0; i < NP; i++)
      for (Int_t j = 0; j < NP; j++) {
	P(i,j) = 0;
	for (Int_t k = 0; k < NP; k++)
	  P(i,j) += SInv(i,k)*S(k,j);
      }
    PrPP(P);
  }
#endif
  TRVector  X(SInv,TRArray::kSxA,AmX);     PrPP(X);
  Double_t chi2 = yTy;
  chi2 -= AmX*X; PrPP(chi2);
  Int_t head = 0;
  for (Int_t i = 1; i <= nx; i++) {
    Int_t sector = i;
    if (! head) {
      out  <<  "_____________________________________________________________________________________________________________"  << endl;
      out  <<  "| x mkm           | y mkm           | z mkm           |alpha mrad     |beta mrad      |gamma mrad     |Comment" << endl;
      cout <<  "_____________________________________________________________________________________________________________"  << endl;
      cout <<  "| x mkm           | y mkm           | z mkm           |alpha mrad     |beta mrad      |gamma mrad     |Comment" << endl;
      outC << "struct data_t {" << endl;
      outC << "\tInt_t sector;" << endl;
      outC << "\tDouble_t x, Dx, y, Dy, z, Dz, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;" << endl;
      outC << "\tconst Char_t *Comment;" << endl;
      outC << "};" << endl;
      outC << "data_t Data[] = {" << endl;
    }
    head++;
    out  << "__________________________________________________________________________________________________ " << sector << endl;
    cout << "__________________________________________________________________________________________________ " << sector << endl;
    struct Val_t {
      Double_t val;
      Double_t valError;
      Int_t    iFlag;
    };
    Val_t ValA[7]; memset (ValA, 0, sizeof(ValA));
    if (LSF) {
      line = "";
      for (Int_t m = 0; m < 6; m++) {
	Int_t l = 6*(i-1) + m;
	if (SInv(l,l) > 0) {
	  Double_t scale = 1e4;   // => mkm
	  if (m > 2) scale = 1e3; // => mrad
	  ValA[m].val =  scale*X(l);
	  ValA[m].valError = scale*TMath::Sqrt(SInv(l,l)); // scale by a factor of 5
	  ValA[m].iFlag = 1; // 0, No LSF1;
	} else {
	  ValA[m].val = ValA[m].valError = 0; ValA[m].iFlag = 0;
	}
	if (m < 3) 
	  line  += Form("|%8.2f+-%6.2f ", TMath::Max(-9999.99,TMath::Min( 9999.99,ValA[m].val)),TMath::Min(999.99,ValA[m].valError)); 
	else
	  line  += Form("|%7.2f+-%5.2f ", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
      }
    }
    cout << line << endl;
    out << line << endl;
    for (Int_t j = 0; j < NwsPlots; j++) {
      TH3 *h3 = (TH3 *) gDirectory->Get(plotNameWS[j].Name);
      if (! h3) continue;
      Int_t ij = j + 1 + ny*(i-1);
      c1->cd(ij)->SetLogz(1);
      TH1 *fit = 0;
      h3->GetXaxis()->SetRange(sector,sector);
      TH1 *sp = h3->Project3D("z");
      if (sp->GetEntries() < 100) continue;
      //      sp->Fit("gaus","qem");
      sp->Fit("gaus","qem");
      Double_t Mu = 0;
      Double_t dMu = 0;
      Mu = sp->GetFunction("gaus")->GetParameter(1);
      dMu = sp->GetFunction("gaus")->GetParError(1);
      TH2 *h = (TH2 *) h3->Project3D("zy");
      h->SetName(Form("%s_%i",h->GetName(),sector));
      h->FitSlicesY(0,0,-1,10,"qeg3s");
      //      SlicesYFit(h,0,0,10,"qnig3");
      fit = (TH1 *) gDirectory->Get(Form("%s_1",h->GetName()));
      //       TH1 *sig = (TH1 *) gDirectory->Get(Form("%s_2",h->GetName()));
      //       TH1 *gra = (TH1 *) gDirectory->Get(Form("%s_3",h->GetName()));
      Double_t slope = 0;
      Double_t dslope = 0;
      TLegend *leg = new TLegend(0.1,0.2,0.6,0.3,"");
      lTitle = "";
      leg->SetTextSize(0.025);
      if (fit) {
	fit->SetTitle(h->GetTitle());
	fit->SetMarkerStyle(20);
	fit->SetMarkerColor(1);
	fit->SetMaximum(0.2);
	fit->SetMinimum(-.2);
	fit->SetStats(1);
	fit->Fit("pol1","qe");
	TF1 *pol1 = fit->GetFunction("pol1");
	if (! pol1 ) goto endhLoop;
	Double_t prob = pol1->GetProb();
	if (prob >= 0) {
#if 0
	  Mu     = pol1->GetParameter(0);
	  dMu    = pol1->GetParError(0);
#else
	  Double_t xm = fit->GetMean();
	  Mu     = pol1->Eval(xm);
	  dMu    = TMath::Sqrt(pol1->GetParError(0)*pol1->GetParError(0) + (xm*pol1->GetParError(1))*(xm*pol1->GetParError(1)));
#endif
	  slope  = pol1->GetParameter(1);
	  dslope = pol1->GetParError(1);
	} else {
	  slope = dslope = 0;
#if 1
	  Mu = dMu = 0;
#endif
	}
	Val_t Vals[6]; memset (Vals, 0, sizeof(Vals));
	static const Char_t *dxv[3] = {"dX", "dY","dZ"};
	TString Name(h->GetName());
	TString Title(h->GetTitle());
	for (Int_t m = 0; m < 3; m++) {
	  if (Name.BeginsWith(dxv[m]) && dMu > 0 && dMu < 999.99e-4) {
	    Vals[m].val =     -1e4*Mu;
	    Vals[m].valError = 1e4*dMu;
	    Vals[m].iFlag = 1;
	    lTitle += Form(" %s = %8.2f+-%6.2f (#mum)", dxv[m],Vals[m].val,TMath::Min(999.99,Vals[m].valError)); 
	  }
	}	  
	Int_t index = Title.Index("=>");
	TString tag("");
	if (index >= 0) {
	  index = index+2;
	  static TString separator("[^ ;,]+");
	  TString t(Title.Data()+index);
	  TObjArray *a = t.Tokenize(separator);
	  tag = ((TObjString *) a->At(0))->GetString();
	  delete a;
	}
	static const Char_t *lvar[6] = {"dx","dy","dz","#alpha","#beta","#gamma"};
	static const Char_t *t[6] = {"x","y","z","a","b","g"};
	for (Int_t m = 0; m < 6; m++) {
	  if (dslope <= 0) continue;
	  if (m < 3 && dslope >= 999.99e-4) continue;
	  if (m >=3 && dslope >=  99.99e-3) continue;
	  if (tag.Contains(t[m])) {
	    if (m < 3) {
	      Vals[m].val =      1e4*slope;
	      Vals[m].valError = 1e4*dslope;
	      Vals[m].iFlag = 1;
	      lTitle += Form(" %s = %8.2f+-%6.2f (#mum)", lvar[m],Vals[m].val,TMath::Min(999.99,Vals[m].valError)); 
	    } else {
	      Vals[m].val =      1e3*slope;
	      Vals[m].valError = 1e3*dslope;
	      Vals[m].iFlag = 1;
	      lTitle += Form(" %s = %7.2f+-%5.2f (mrad)", lvar[m],Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	    }
	  }
	}
	lTitle += Form(" prob = %5.3f",prob);
	leg->AddEntry(pol1,lTitle);
	line = "";
	lineC = Form("\t{%2i",sector);
	for (Int_t m = 0; m < 6; m++) {
	  if (! Vals[m].iFlag) {
	    if (m > 2) 	      line  += "|               ";
	    else              line  += "|                 ";
	    lineC += ",      0,-9.99";
	  } else {
	    if (m > 2) {
	      line  += Form("|%7.2f+-%5.2f ", Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	      lineC += Form(",%7.2f,%5.2f", Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	    } else {
	      line  += Form("|%8.2f+-%6.2f ", Vals[m].val,TMath::Min(999.99,Vals[m].valError)); 
	      lineC += Form(",%8.2f,%6.2f", Vals[m].val,TMath::Min(999.99,Vals[m].valError)); 
	    }
#define __AVERAGE_WS__
#ifdef  __AVERAGE_WS__
	    if (ValA[m].iFlag) {
	      Double_t w0 = 1./(Vals[m].valError*Vals[m].valError);
	      Double_t w1 = 1./(ValA[m].valError*ValA[m].valError);
	      ValA[m].val = (w0*Vals[m].val + w1*ValA[m].val)/(w0 + w1);
	      ValA[m].valError = 1./TMath::Sqrt(w0 + w1);
	      ValA[m].iFlag++;
	    } else {
	      ValA[m] = Vals[m];
	    }
#endif
	  }
	}
	line += "|"; line += fit->GetName(); line += "/"; line += h->GetTitle(); 
	lineC += ",\""; lineC += fit->GetName(); lineC += "\"},";
	cout << line << endl;
	out << line << endl;
      }
    endhLoop:
      if (h) h->Draw("colz");
      if (fit) {
	fit->Draw("same"); 
	TF1 *pol1 = fit->GetFunction("pol1"); 
	if (pol1) {pol1->SetLineColor(2); pol1->Draw("same");}
      }
      leg->Draw();
    }
    out  << "__________________________________________________________________________________________________ " << sector << endl;
    cout << "__________________________________________________________________________________________________ " << sector << endl;
    line = ""; 
    lineC = Form("\t{%2i",sector);
    for (Int_t m = 0; m < 6; m++) {
      if (! ValA[m].iFlag 
#ifdef   FREEZE_BETA  
	  || (m == 4) 
#endif
#ifdef   FREEZE_ALPHA_BETA  
	  || (m == 3 || m == 4) 
#endif
	  ) {
	if (m > 2) 
	  line  += "|               ";
	else 
	  line  += "|                ";
	lineC += ",      0,-9.99";
      } else {
	if (m > 2) {
	  line  += Form("|%7.2f+-%5.2f ", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
	  lineC += Form(",%7.2f,%5.2f", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
	} else {
	  line  += Form("|%8.2f+-%6.2f ", TMath::Max(-9999.99,TMath::Min( 9999.99,ValA[m].val)),TMath::Min(999.99,ValA[m].valError)); 
	  lineC += Form(",%8.2f,%6.2f", ValA[m].val,TMath::Min(999.99,ValA[m].valError)); 
	}
      }
    }
#ifdef __AVERAGE_WS__
    lineC += ",\"Average ";
#else
    lineC += ",\"LSF ";
#endif
#ifdef   FREEZE_ALPHA_BETA  
    lineC += " Fixed alpha beta";
#endif
#ifdef   FREEZE_BETA  
    lineC += ",\"LSF Fixed beta";
#endif
    lineC += "\"},";
    cout << line << endl;
    out << line << endl;
    outC << lineC << endl;
  }
  out.close();
  outC.close();
}
//________________________________________________________________________________
void TpcAlignerDraw(Int_t jcase = 0, const Char_t *files = "*.root") {
  switch (jcase) {
  case 0: TpcAlignerDrawIO(files);  break;
  case 1: TpcAlignerDrawW2S(files); break;
  case 2: TDrawIO();                break;
  case 3: TDrawW2S();               break;
  default:
    break;
  };
}
