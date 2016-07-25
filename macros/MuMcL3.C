/* 
   root.exe lMuDst.C MuMcL3.C+
   root.exe lMuDst.C MuMcL3.root
>> .L MuMcL3.C+;  Init(0); DrawEff(); DrawQA(); // FPE_OFF
*/
//#define __DEVT__ 
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include <map>
#include <utility>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile3D.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryTrackCovariance.h"
#include "StarRoot/TPolynomial.h"
#include "StarRoot/KFVertex.h"
#include "StarRoot/KFParticle.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#include "Names.h"
#include "StBichsel/Bichsel.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#undef  StMessMgr
#undef ClassStMessMgr
#else
#ifndef __MAKECINT__
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))
class StMuDstMaker;
#endif
#endif
StMuDstMaker* maker = 0;
Int_t MinNoMcTpcHits = 29;
enum TrackMcType {kNotDefined, kMatched, kLost, kGost, kClone, kTotalMcType};
//static const Char_t *NameTrackMcName[kTotalMcType] = {"NotDefined","Matched","Lost","Ghost","Clone"};
enum TrackMatchType {kPositive, kNegative, kTotalSigns,                                     // switch between charges
		     kGl = 0, kPr, kTotalT,                                                 // switch between global and primary tracks
		     kMc = 0, kMcHit, kMcRec, kMcClone, kRcGhost, kMcLost, kTotalMatchType, // match type
		     kTotalQAPr = 10,                                                       // no. of plots for Primary tracks
		     kTotalQAGl = 13                                                        // no. of plots for Global  tracks
};
struct PlotName_t {
  TrackMatchType    k;
  const Char_t *Name;
  const Char_t *Title;
};
struct VarName_t {
  const Char_t *Name;
  const Char_t *Title;
  Int_t nx;
  Double_t xmin, xmax;
  Int_t ny;
  Double_t ymin, ymax;
  Int_t nz;
  Double_t zmin, zmax;
  Double_t  min,  max; // min and max for plots
};

TH3F **hists[2][2]   = {{0,0}, {0,0}}; // Efficiencies versus phi, eta and pT
TH3F **histsQA[2][2] = {{0,0}, {0,0}}; // Quality versus NfitPoints and Nbad hits
TH3F **histsK[2][2][2]  = {{{0,0}, {0,0}}, {{0,0}, {0,0}}}; // Quality versus eta and pT, 0 -> all, 1 -> pion
TProfile3D *PdEdx[2][NHYPS]; // <z> = <log((dE/dx)_measured/(dE/dx)_predicted)> versus phi, eta and pT; 0 => I70, 1 => Fit
TH3F *LdEdx[2][NHYPS]; //  z  =  log((dE/dx)_measured/(dE/dx)_predicted)  versus TpcTrackLength and log10(beta*gamma);  0 => I70, 1 => Fit
TH1F *GiD[4] = {0, 0, 0, 0};
TH2F *McRcHit = 0;
TFile *fOut = 0;
const Char_t *NameCharge[kTotalSigns] = {"Pos", "Neg"};
const Char_t *TitleCharge[kTotalSigns] = {"(+)", "(-)"};
const Char_t *NameTrType[kTotalT] = {"Gl", "Pr"};
const Char_t *TitleTrType[kTotalT] = {"Global Tracks", "Primary Tracks"};
const Char_t *proj[2] = {"zx","zy"};

const  PlotName_t plotNameMatch[kTotalMatchType] = {
  {kMc,      "Mc",      "Mc tracks All"},
  {kMcHit,   "McHit",   Form("Mc tracks which have >= %i Mc Hits",MinNoMcTpcHits)},
  {kMcRec,   "McRec",   "Rc tracks matched with only Mc track"},
  {kMcClone, "McClone", "Mc tracks matched with > 1 Rc track (Clone)"},
  {kRcGhost, "RcGhost", "Rc tracks without Mc partner"},
  {kMcLost,  "McLost",  "Mc tracks without reconstructed one"}
};
struct VarGl_t {
  Double_t ChiSqXY;
  Double_t dDcaXY; 
  Double_t dDcaZ;  
  Double_t dPsi;   
  Double_t dPti;  
  Double_t dPtiR; 
  Double_t dTanL;  
  Double_t pDcaXY; 
  Double_t pDcaZ;  
  Double_t pPsi;   
  Double_t pPti;  
  Double_t pPtiR; 
  Double_t pTanL;  
};
const Char_t *HitName = "vs NoFitPnts and no. bad hits";
const Char_t *KinName = "vs   #eta and pT/|q|";
const Char_t *KinPionName = "vs   #eta and pT/|q| for pion";
#ifndef __DEVT__
const Int_t noFit =  45;
#else
const Int_t noFit = 100;
#endif
const VarName_t plotGlVar[kTotalQAGl] = {      //no.fit                    no.bad          
  {"ChiSqXY",   "#chi^{2}_{Track}/NDF",          noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100,  0.,  10., 0.000, 6.000},
  {"dDcaXY",    "difference in Dca_{XY}",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -5.,   5., -.250, 1.500},
  {"dDcaZ",     "difference in Dca_{Z}",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -5.,   5., -.250, 1.500},
  {"dPsi",      "difference in  #Psi ",          noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.004, 0.040},
  {"dPti" ,     "difference in q/pT",            noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.020, 0.200},
  {"dPtiR" ,    "difference in relative q/pT",   noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.004, 0.040},
  {"dTanL",     "difference in tan( #lambda )",  noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.004, 0.040},
  {"pDcaXY",    "pull in Dca_{XY}",              noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.300, 3.000},
  {"pDcaZ",     "pull in Dca_{Z}",               noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.100,10.000},
  {"pPsi",      "pull in  #Psi ",                noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.300, 3.000},
  {"pPti" ,     "pull in q/pT",                  noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.250, 2.500},
  {"pPtiR" ,    "pull in relative q/pT",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.230, 3.500},
  {"pTanL",     "pull in tan( #lambda )",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.600, 6.000}
};
struct VarPr_t {
  Double_t ChiSqXY;
  Double_t ChiSqZ; 
  Double_t deta;  
  Double_t dPsi;   
  Double_t dPti;   
  Double_t dPtiR;
  Double_t peta;  
  Double_t pPsi;   
  Double_t pPti;   
  Double_t pPtiR;  
};
const VarName_t plotPrVar[kTotalQAPr] = {
  {"ChiSqXY",   "#chi^{2}_{Track}/NDF",       noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100,  0.,  10., 0.000, 4.000},
  {"ChiSqZ",    "#chi^{2}_{Vx} ",             noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100,  0., 100., 0.000,10.000},
  {"deta",      "difference in  #eta",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.002, 0.025},
  {"dPsi",      "difference in  #Psi ",       noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.002, 0.020},
  {"dPti",      "difference in q/pT",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.015, 0.150},
  {"dPtiR",     "relative difference in q/pT",noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.006, 0.060},
  {"peta",      "pull for  #eta",             noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10.,-1.000,10.000},
  {"pPsi",      "pull for  #Psi ",            noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.500, 5.000},
  {"pPti",      "pull for q/pT",              noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.500, 5.000},
  {"pPtiR",     "pull for Relative q/pT",     noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.500, 5.000}
};
Int_t nPng = 0;
//________________________________________________________________________________
static Int_t _debug = 0;
void SetDebug(Int_t k) {_debug = k;}
Int_t Debug() {return _debug;}
//________________________________________________________________________________
//________________________________________________________________________________
void Init(const Char_t *outfName="Out.root") {
  Int_t opt = 1;
  if (outfName) {// recreate
    fOut = new TFile(outfName,"recreate");
    opt = 0;
  }
#if 1
  Int_t    npT    = 98;
  //  Double_t pTMax =   10;
  const Double_t ptBins[102] = {
    0.07, 0.08, 0.11, 0.14, 0.16, 0.17, 0.19, 0.21, 0.22, 0.23,
    0.24, 0.26, 0.27, 0.28, 0.29, 0.30, 0.31, 0.32, 0.33, 0.34,
    0.35, 0.36, 0.37, 0.38, 0.39, 0.40, 0.41, 0.42, 0.43, 0.44,
    0.45, 0.46, 0.47, 0.48, 0.49, 0.50, 0.51, 0.53, 0.54, 0.55,
    0.56, 0.57, 0.58, 0.60, 0.61, 0.62, 0.63, 0.65, 0.66, 0.67,
    0.69, 0.70, 0.72, 0.73, 0.75, 0.76, 0.78, 0.80, 0.81, 0.83,
    0.85, 0.87, 0.89, 0.91, 0.93, 0.96, 0.98, 1.01, 1.03, 1.06,
    1.09, 1.12, 1.16, 1.19, 1.23, 1.27, 1.31, 1.35, 1.40, 1.45,
    1.51, 1.57, 1.64, 1.71, 1.80, 1.89, 2.00, 2.11, 2.24, 2.39,
    2.57, 2.78, 3.05, 3.38, 3.80, 4.28, 4.96, 5.88, 7.25, 8.55,
   10.00, 25.0
  };
#else
  Int_t    npT    = 100;
  Double_t ptBins[101];
  ptBins[0] = 0;
  for (Int_t i = 1; i <= npT; i++) ptBins[i] = i;
#endif
  enum {nphiM = 10};
  Double_t dphiMask[nphiM] = {0,  5., 10., 12., 14., 15., 16., 18., 20., 25.};
  Int_t    nphi   = 12*nphiM;
  Double_t *phiBins = new Double_t [nphi+1];
  Int_t i = 0;
  for (Int_t sec = 0; sec < 12; sec++) {
    Double_t phi = -180 + 30*sec;
    for (Int_t j = 0; j < nphiM; j++, i++) {
      phiBins[i] = phi +  dphiMask[j];
    }
  }
  phiBins[nphi] = 180.;
#ifdef __DEVT__
  Int_t    neta   = 100; 
  Double_t etamax = 2.5;
#else
  Int_t    neta   =  60; 
  Double_t etamax = 1.5;
#endif
  Double_t deta = 2*etamax/neta;
  Double_t *etaBins = new Double_t [neta+1];
  for (i = 0; i <= neta; i++) {etaBins[i] = -etamax + deta*i;}
  for (Int_t s = kPositive; s < kTotalSigns; s++) {
    for (Int_t k = kGl; k < kTotalT; k++) {
      hists[s][k] = new TH3F*[kTotalMatchType];
      for (i = 0; i < kTotalMatchType; i++) {
	if (! opt) {
	  hists[s][k][i] = new TH3F(Form("%s%s%s",NameCharge[s],plotNameMatch[i].Name,NameTrType[k]), 
				    Form("%s %s %s %s",TitleCharge[s],plotNameMatch[i].Title,KinName,TitleTrType[k]),
				    nphi, phiBins,
				    neta, etaBins,
				    npT, ptBins);
	  hists[s][k][i]->GetXaxis()->SetTitle("#phi (degree)");
	  hists[s][k][i]->GetYaxis()->SetTitle("  #eta");         
	  hists[s][k][i]->GetZaxis()->SetTitle("pT/|q| (GeV/c)");   
	  hists[s][k][i]->SetMarkerColor(s+1);       
	} else {
	  hists[s][k][i] = (TH3F *) gDirectory->Get(Form("%s%s%s",NameCharge[s],plotNameMatch[i].Name,NameTrType[k]));
	}
      }
    }
  }
  assert(kTotalQAPr == sizeof(VarPr_t)/sizeof(Double_t));
  assert(kTotalQAGl == sizeof(VarGl_t)/sizeof(Double_t));
  for (Int_t s = kPositive; s < kTotalSigns; s++) {
    for (Int_t k = kGl; k < kTotalT; k++) {
      Int_t N = kTotalQAGl;
      if (k == kPr) N = kTotalQAPr;
      histsQA[s][k] = new TH3F*[N];
      histsK[0][s][k]  = new TH3F*[N];
      histsK[1][s][k]  = new TH3F*[N];
      for (i = 0; i < N; i++) {
	if (! opt) {
	  if (k == kGl) {
	    histsQA[s][k][i] = new TH3F(Form("%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]), 
					Form("%s %s %s %s",TitleCharge[s],plotGlVar[i].Title,HitName,TitleTrType[k]),
					plotGlVar[i].nx, plotGlVar[i].xmin, plotGlVar[i].xmax,
					plotGlVar[i].ny, plotGlVar[i].ymin, plotGlVar[i].ymax,
					plotGlVar[i].nz, plotGlVar[i].zmin, plotGlVar[i].zmax);
	    Double_t *zBins = new Double_t[plotGlVar[i].nz+1];
	    Double_t dz = (plotGlVar[i].zmax - plotGlVar[i].zmin)/plotGlVar[i].nz;
	    for (Int_t j = 0; j <= plotGlVar[i].nz; j++) zBins[j] = plotGlVar[i].zmin + dz*j;
	    histsK[0][s][k][i] = new TH3F(Form("All%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]), 
					  Form("%s %s %s %s",TitleCharge[s],plotGlVar[i].Title,KinName,TitleTrType[k]),
					  neta, etaBins,
					  npT, ptBins,
					  plotGlVar[i].nz, zBins);
	    histsK[1][s][k][i] = new TH3F(Form("pi%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]), 
				       Form("%s %s %s %s",TitleCharge[s],plotGlVar[i].Title,KinName,TitleTrType[k]),
					  neta, etaBins,
					  npT, ptBins,
					  plotGlVar[i].nz, zBins);
	    delete [] zBins;
	  } else {
	    histsQA[s][k][i] = new TH3F(Form("%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]), 
					Form("%s %s %s",TitleCharge[s],plotPrVar[i].Title,TitleTrType[k]),
					plotPrVar[i].nx, plotPrVar[i].xmin, plotPrVar[i].xmax,
					plotPrVar[i].ny, plotPrVar[i].ymin, plotPrVar[i].ymax,
					plotPrVar[i].nz, plotPrVar[i].zmin, plotPrVar[i].zmax);
	    Double_t *zBins = new Double_t[plotPrVar[i].nz+1];
	    Double_t dz = (plotPrVar[i].zmax - plotPrVar[i].zmin)/plotPrVar[i].nz;
	    for (Int_t j = 0; j <= plotPrVar[i].nz; j++) zBins[j] = plotPrVar[i].zmin + dz*j;
	    histsK[0][s][k][i] = new TH3F(Form("All%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]), 
					  Form("%s %s %s %s",TitleCharge[s],plotPrVar[i].Title,KinName,TitleTrType[k]),
					  neta, etaBins,
					  npT, ptBins,
					  plotPrVar[i].nz, zBins);
	    histsK[1][s][k][i] = new TH3F(Form("pi%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]), 
					  Form("%s %s %s %s",TitleCharge[s],plotPrVar[i].Title,KinName,TitleTrType[k]),
					  neta, etaBins,
					  npT, ptBins,
					  plotPrVar[i].nz, zBins);
	    delete [] zBins;
	  }
	  histsQA[s][k][i]  ->GetXaxis()->SetTitle("No. of Fit Points");
	  histsQA[s][k][i]  ->GetYaxis()->SetTitle("No. of Bad Points"); 
	  histsK[0][s][k][i]->GetXaxis()->SetTitle("  #eta");             
	  histsK[0][s][k][i]->GetYaxis()->SetTitle("pT/|q| (GeV/c)");      	
	  histsK[1][s][k][i]->GetXaxis()->SetTitle("  #eta");	       	
	  histsK[1][s][k][i]->GetYaxis()->SetTitle("pT/|q| (GeV/c)");      	
	  if (k == kGl) {
	    histsQA[s][k][i]  ->GetZaxis()->SetTitle(plotGlVar[i].Title);
	    histsK[0][s][k][i]->GetZaxis()->SetTitle(plotGlVar[i].Title);
	    histsK[1][s][k][i]->GetZaxis()->SetTitle(plotGlVar[i].Title);
	  } else {
	    histsQA[s][k][i]  ->GetZaxis()->SetTitle(plotPrVar[i].Title);
	    histsK[0][s][k][i]->GetZaxis()->SetTitle(plotPrVar[i].Title);
	    histsK[1][s][k][i]->GetZaxis()->SetTitle(plotPrVar[i].Title);
	  }
	  histsQA[s][k][i]->SetMarkerColor(s+1);
	  histsK[0][s][k][i]->SetMarkerColor(s+1);
	  histsK[1][s][k][i]->SetMarkerColor(s+1);
	} else {
	  if (k == kGl) {
	    histsQA[s][k][i] = (TH3F *) gDirectory->Get(Form("%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]));
	    histsK[0][s][k][i] = (TH3F *) gDirectory->Get(Form("All%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]));
	    histsK[1][s][k][i] = (TH3F *) gDirectory->Get(Form("pi%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]));
	  } else {
	    histsQA[s][k][i] = (TH3F *) gDirectory->Get(Form("%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]));
	    histsK[0][s][k][i] = (TH3F *) gDirectory->Get(Form("All%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]));
	    histsK[1][s][k][i] = (TH3F *) gDirectory->Get(Form("pi%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]));
	  }
	}
      }
    }
  }
  delete [] phiBins;
  delete [] etaBins;
  // dE/dx block for matched primary tracks
  const Char_t *dEdxTypes[2] = {"I70","Fit"};
  for (Int_t h = 0; h < NHYPS; h++) {
    for (i = 0; i < 2; i++) {
      PdEdx[i][h] = new TProfile3D(Form("Zav%s%s",dEdxTypes[i],HistNames[h]),
				   Form("< z_{%s} > versus  #phi,  #eta,  p_{T}/|q| for %s",dEdxTypes[i],Names[h]),
				   90, -TMath::Pi(), TMath::Pi(),
				   60, -1.2, 1.2,
				   npT, 0, 10, "S");
      PdEdx[i][h]->GetXaxis()->SetTitle("#phi (rad)");
      PdEdx[i][h]->GetYaxis()->SetTitle("  #eta");         
      PdEdx[i][h]->GetZaxis()->SetTitle("pT/|q| (GeV/c)");   
      PdEdx[i][h]->GetZaxis()->Set(npT,ptBins);
      LdEdx[i][h] = new TH3F(Form("Z%s%s",dEdxTypes[i],HistNames[h]),
			     Form(" z_{%s}  versus TpcTrackLength and log_{10} (#beta #gamma) for %s",dEdxTypes[i],Names[h]),
			     110, 0, 220, 220,-1,10, 100, -1, 1); 
    }
  }
  GiD[0] = new TH1F("GiD","Geant ID for all MC tracks",50,0.5,50.5);
  GiD[1] = new TH1F("GiDG",Form("Geant ID for MC tracks with >= %i Tpc MC hits",MinNoMcTpcHits),50,0.5,50.5);
  GiD[2] = new TH1F("GiDPr","Geant ID for all primary MC tracks",50,0.5,50.5);
  GiD[3] = new TH1F("GiDPrG",Form("Geant ID for primary MC tracks with >= %i Tpc MC hits",MinNoMcTpcHits),50,0.5,50.5);
  McRcHit = new TH2F("McRcHit","No. RC hits in TPC versus No. MC ones",80,-0.5,79.5,80,-0.5,79.5);
}
//________________________________________________________________________________
void FillQAGl(const StMuTrack *gTrack = 0, const StMuMcTrack *mcTrack = 0, const StDcaGeometry *dcaG = 0, const StMuMcVertex *mcVertex = 0) {
  if (! gTrack || ! mcTrack) return;
  if (! dcaG   || ! mcVertex) return;
  TrackMatchType s = kPositive;
  if (mcTrack->Charge() < 0) s = kNegative;
  VarGl_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  TRSymMatrix Cov(5,dcaG->errMatrix());
  Double_t vtx[3] = {mcVertex->XyzV().x(), mcVertex->XyzV().y(), mcVertex->XyzV().z()};
  THelixTrack     thelix =  dcaG->thelix();
  Double_t ermx[3];
  Double_t pars[2];
  thelix.Dca(vtx,pars[0],pars[1],ermx,2);
  var.ChiSqXY = gTrack->chi2xy();
  var.dDcaXY  = pars[0];
  var.dDcaZ   = pars[1];
  Double_t *Dir = thelix.Dir();
  Double_t phi =  TMath::ATan2(Dir[1],Dir[0]);
  var.dPsi    = phi - mcTrack->Pxyz().phi(); 
  Double_t pTqRC = gTrack->pt()/gTrack->charge();
  Double_t pTqMC = mcTrack->pT()/mcTrack->Charge();
  var.dPti    = 1./pTqRC - 1./pTqMC;
  var.dPtiR   = pTqMC/pTqRC - 1;
  Double_t    tanDip =  mcTrack->Pxyz().z()/mcTrack->Pxyz().perp();
  var.dTanL   = thelix.GetTan() - tanDip;
  if (ermx[0] <= 0 || ermx[2] <= 0) {
    gTrack->Print();
    mcTrack->Print();
    dcaG->Print("");
    mcVertex->Print();
    thelix.Print();
  } else {
    var.pDcaXY  = var.dDcaXY/TMath::Sqrt(ermx[0]);
    var.pDcaZ   = var.dDcaZ /TMath::Sqrt(ermx[2]);
  }
  if (Cov(2,2) <= 0 || Cov(3,3) <= 0 || Cov(4,4) <= 0) {
    gTrack->Print();
    mcTrack->Print();
    dcaG->Print("");
    mcVertex->Print();
  } else {
    var.pPsi    = var.dPsi  /TMath::Sqrt(Cov(2,2));
    var.pPti    = var.dPti  /TMath::Sqrt(Cov(3,3));
    var.pPtiR   = var.dPtiR /TMath::Sqrt(Cov(3,3)) / mcTrack->Pxyz().perp();
    var.pTanL   = var.dTanL /TMath::Sqrt(Cov(4,4));
  }
  Double_t *x = &var.ChiSqXY;
  for (Int_t i = 0; i < kTotalQAGl; i++) {
    histsQA[s][kGl][i]->Fill(gTrack->nHitsFit(), gTrack->nHitsFit()*(100.-gTrack->qaTruth())/100., x[i]);
    histsK[0][s][kGl][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9)
      histsK[1][s][kGl][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
  }
  McRcHit->Fill(gTrack->nHitsFit(),mcTrack->No_tpc_hit());
}
//________________________________________________________________________________
void FillQAPr(const StMuTrack *pTrack = 0, const StMuMcTrack *mcTrack = 0, const StMuPrimaryTrackCovariance *cov = 0) {
  if (! pTrack || ! mcTrack) return;
  if (! mcTrack->Charge()) return;
  TrackMatchType s = kPositive;
  if (mcTrack->Charge() < 0) s = kNegative;
  VarPr_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  var.ChiSqXY = pTrack->chi2xy();
  var.ChiSqZ  = pTrack->chi2z();
  Double_t Eta = mcTrack->Pxyz().pseudoRapidity();
  var.deta    = pTrack->eta() - Eta;
  var.dPsi    = pTrack->phi() - mcTrack->Pxyz().phi();
  Double_t pTqRC = pTrack->pt()/pTrack->charge();
  Double_t pTqMC = mcTrack->pT()/mcTrack->Charge();
  var.dPti    = 1./pTqRC - 1./pTqMC;
  var.dPtiR   = pTqMC/pTqRC - 1;
  if (cov) {
    TRSymMatrix Cov(3,cov->errMatrix());
    if (Cov(0,0) <= 0 || Cov(1,1) <= 0 || Cov(2,2) <= 0) {
      pTrack->Print();
      mcTrack->Print();
      cov->Print();
      Cov.Print();
    } else {
      var.peta    = var.deta / TMath::Sqrt(Cov(0,0)) / TMath::CosH(Eta);
      var.pPsi    = var.dPsi / TMath::Sqrt(Cov(1,1));
      var.pPti    = var.dPti / TMath::Sqrt(Cov(2,2)); 
      var.pPtiR   = var.dPtiR/ TMath::Sqrt(Cov(2,2)) / (mcTrack->pT()/mcTrack->Charge());
    }			
  }		     
  Double_t *x = &var.ChiSqXY;
  for (Int_t i = 0; i < kTotalQAPr; i++) {
    histsQA[s][kPr][i]->Fill(pTrack->nHitsFit(), pTrack->nHitsFit()*(100.-pTrack->qaTruth())/100., x[i]);
    histsK[0][s][kPr][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9)
      histsK[1][s][kPr][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
  }
}
//________________________________________________________________________________
Bool_t Accept(const StMuTrack *gTrack = 0) {
  if (! gTrack)            return kFALSE;
  if (! gTrack->idTruth()) return kFALSE;
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  if (  gTrack->nHitsFit() < 10) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t AcceptGhost(const StMuTrack *gTrack = 0) {
  if (! gTrack)            return kFALSE;
  // if (  gTrack->idTruth()) return kFALSE;
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  if (  gTrack->nHitsFit() < 10) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t AcceptVX(const StMuPrimaryVertex *Vtx = 0) {
  if (! Vtx) return kFALSE;
  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
TrackMcType TrackType(const StMuMcTrack *mcTrack, multimap<Int_t,Int_t> &Mc2RcTracks) {
  Int_t Id = mcTrack->Id()-1;
  pair<multimap<Int_t,Int_t>::iterator,multimap<Int_t,Int_t>::iterator> ret = Mc2RcTracks.equal_range(Id);
  Int_t count = 0;
  for (multimap<Int_t,Int_t>::iterator it = ret.first; 
       it != ret.second; 
       ++it, ++count) 
    {}

  TrackMcType iok = kNotDefined;
  if      (count == 0) { iok = kLost;}
  else {
    if (count == 1) iok = kMatched;
    else            iok = kClone;
  }
  //  cout << " Marked as " << NameTrackMcName[iok] << endl;
  return iok;
}
#if 0
//________________________________________________________________________________
TrackMcType TrackType(Int_t IdMcTrack, Int_t IdGlTrack, multimap<Int_t,Int_t> &Mc2RcTracks) {
  multimap<Int_t,Int_t>::iterator it;
  pair<multimap<Int_t,Int_t>::iterator,multimap<Int_t,Int_t>::iterator> ret;
  ret = Mc2RcTracks.equal_range(IdMcTrack);
  if (ret.first == ret.second) return kLost; 
  
  for (it = ret.first; it != ret.second; ++it) {
  }
  return kMatched;
}
#endif
//________________________________________________________________________________
void ForceAnimate(unsigned int times=0, int msecDelay=0) {
  unsigned int  counter = times;
  while( (!times || counter) && !gSystem->ProcessEvents()) { --counter; if (msecDelay) gSystem->Sleep(msecDelay);} 
}
//________________________________________________________________________________
void DrawPng(TCanvas *c) {
  TString pngName("");
  if (c) {
    c->Update(); pngName = c->GetName();
    pngName.ReplaceAll(" ","_");
    pngName.ReplaceAll("(","_");
    pngName.ReplaceAll(")","_");
    pngName.ReplaceAll("{","_");
    pngName.ReplaceAll("}","_");
    pngName.ReplaceAll("<","lt");
    pngName.ReplaceAll(">","gt");
    pngName.ReplaceAll(".","_");
    pngName.ReplaceAll("/","_");
    pngName.ReplaceAll("^","_");
    pngName.ReplaceAll("__","_");
    pngName.ReplaceAll("__","_");
    pngName += ".png"; 
    TVirtualX::Instance()->WritePixmap(c->GetCanvasID(),-1,-1,(Char_t *)pngName.Data());
    nPng++;
    cout << "Draw #\t" << nPng << "\t" << pngName << endl;
  }
}
//________________________________________________________________________________
void MinMax(TH1 *h, Double_t &min, Double_t &max, Double_t amax = 1000) {
  if (! h) return;
  Int_t n = h->GetNbinsX();
  for (Int_t i = 1; i <= n; i++) {
    Double_t y = h->GetBinContent(i);
    Double_t dy = h->GetBinError(i);
    if (TMath::Abs(y+dy) > amax) continue;
    if (TMath::Abs(y-dy) > amax) continue;
    if (y > 0 && y < 3*dy) continue;
    if (y < 0 && y >  -2*dy) continue;
    if (y + dy > max) max = y + dy;
    if (y - dy < min) min = y - dy;
  }
  if (min < -0.5*max) min = -0.5*max;
}
//________________________________________________________________________________
void DrawH3s(TH3F *h3s[2], Int_t animate = 0, Double_t min = 1e9, Double_t max = -1e9) {
  if (! h3s[0] || ! h3s[1]) return;
  TH2 *h2[2] = {0,0};
  TH1 *h1[2] = {0,0};
  TH1 *s1[2] = {0,0};
  for (Int_t p = 0; p < 2; p++) {// zx and zy
    for (Int_t s = kPositive; s < kTotalSigns; s++) {
      TH3 *h3 = h3s[s];
      if (! h3) continue;
      h2[s] = (TH2 *) h3->Project3D(proj[p]);
      TString Title(h2[s]->GetTitle());
      Title.ReplaceAll("(+) ","");
      Title.ReplaceAll("(-) ","");
      Title.ReplaceAll(Form("%s projection",proj[p]),"");
      //      Title.ReplaceAll("  Pr"," Primary tracks");
      //      Title.ReplaceAll("  Gl"," Global tracks");
      Title.ReplaceAll(HitName,"");
      Title.ReplaceAll(KinPionName,"");
      Title.ReplaceAll(KinName,"");
      h2[s]->SetTitle(Title);
      if ( !p) {h2[s]->GetXaxis()->SetTitle(h3->GetXaxis()->GetTitle()); }
      else     {h2[s]->GetXaxis()->SetTitle(h3->GetYaxis()->GetTitle()); }
      h2[s]->GetYaxis()->SetTitle(h3->GetZaxis()->GetTitle()); 
      h2[s]->SetMarkerColor(h3->GetMarkerColor());
      TString NameH(h3->GetName());
      cout << "Histogram: " << NameH.Data() << "\t" << Title.Data() << endl;
      if (NameH.Contains("ChiSq",TString::kIgnoreCase)) {
	h1[s] = (TH1 *) h2[s]->ProfileX();
	h1[s]->SetStats(0);
	h1[s]->SetMarkerColor(h2[s]->GetMarkerColor());
	h1[s]->GetXaxis()->SetTitle(h2[s]->GetXaxis()->GetTitle());
	h1[s]->GetYaxis()->SetTitle(h2[s]->GetYaxis()->GetTitle());
#ifdef __MinMax__
	MinMax(h1[s],min,max,500);
#endif /* __MinMax__ */
      } else {
	h2[s]->FitSlicesY(0,0,-1,10,"qeg3s");
	h1[s] = (TH1 *) gDirectory->Get(Form("%s_1",h2[s]->GetName()));
	if (h1[s]) {
	  h1[s]->SetTitle(Form("Fitted %s",Title.Data()));
	  h1[s]->SetStats(0);
	  h1[s]->SetMarkerColor(h2[s]->GetMarkerColor());
	  h1[s]->GetXaxis()->SetTitle(h2[s]->GetXaxis()->GetTitle());
	  h1[s]->GetYaxis()->SetTitle(h2[s]->GetYaxis()->GetTitle());
#ifdef __MinMax__
	  MinMax(h1[s],min,max,10);
#endif /* __MinMax__ */
	  s1[s] = (TH1 *) gDirectory->Get(Form("%s_2",h2[s]->GetName()));
	  if (s1[s]) {
	    s1[s]->SetTitle(Form("#sigma %s",Title.Data()));
	    s1[s]->SetMarkerStyle(21);
	    s1[s]->SetMarkerColor(h2[s]->GetMarkerColor());
#ifdef __MinMax__
	    MinMax(s1[s],min,max,10);
#endif /* __MinMax__ */
	    if (min > -0.1*max) min = -0.1*max;
	  }
	}
      }
    }
    if (h1[0] && h1[1]) {
      Double_t yy = 0.3;
      if (! s1[0] || ! s1[1]) yy = 0.2; 
      //      TLegend *l = new TLegend(0.7,0.1,0.9,0.1+yy);
      TString Name(h1[0]->GetName());
      Name.ReplaceAll("Pos","");
      Name.ReplaceAll("Neg","");
      TCanvas *c = new TCanvas(Name.Data(),Name.Data(),400,400);
      if (max > 0) max *= 1.1;
      else         max *= 0.9;
      if (min > 0) min *= 0.9;
      else         min *= 1.1;
      TString xName(h1[0]->GetXaxis()->GetTitle());
#if 1
      if (xName.Contains("pT",TString::kIgnoreCase) ||
	  xName.Contains("pT",TString::kIgnoreCase)) c->SetLogx(1);
      h1[0]->SetMinimum(min);
      h1[0]->SetMaximum(max);
#endif
      for (Int_t s = kPositive; s < kTotalSigns; s++) {
	if (s == kPositive) h1[s]->Draw(); 
	else                h1[s]->Draw("same"); 
	if (! s1[s]) {
	  //	  l->AddEntry(h1[s], Form("averaged %s",TitleCharge[s]));
	} else {
	  //	  l->AddEntry(h1[s], Form("%s #mu",TitleCharge[s]));
	  s1[s]->Draw("same");
	  //	  l->AddEntry(s1[s], Form("%s #sigma",TitleCharge[s]));
	}
      }
      //      l->Draw();
      if (animate) ForceAnimate(0,200);
      c->Update();
      DrawPng(c);
      delete c;
    }
  }
}
//________________________________________________________________________________
void Check() {
  if (! gDirectory ) {cout << "There is no input file. Exit" << endl; return;}
  if (! hists[0][0]) Init(0);
  if (! hists[0][0]) {cout << "There are no input histograms. Exit" << endl; return;}
}
//________________________________________________________________________________
void DrawQA(Int_t kk = -1, Int_t ii = -1, Int_t ll = -2) {// versus Nhits
  Check();
  Int_t animate = 0;
#if 0
  TCanvas *c1 = (TCanvas *) ((TList*) gROOT->GetListOfCanvases())->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  c1->cd();
#endif
  Int_t k1 = kGl, k2 = kTotalT;
  if (kk >= 0) {k1 = kk; k2 = kk + 1;}
  for (Int_t k = k1; k < k2; k++) {
    Int_t N = kTotalQAGl;
    if (k == kPr) N = kTotalQAPr;
    cout << "k = " << k << "\t" << TitleTrType[k] << endl;
    Int_t i1 = 0, i2 = N;
    if (ii >= 0) {i1 = ii; i2 = ii + 1;}
    for (Int_t i = i1; i < i2; i++) {
      cout << "i = " << i << "\t";
      if (k == kPr) cout << plotPrVar[i].Name;
      else          cout << plotGlVar[i].Name;
      cout << endl;
      TH3F *h3s[2];
      Int_t l1 = -1, l2 = 2;
      if (ll >= -1 && ll < l2) { l1 = ll; l2 = ll + 1;}
      for (Int_t l = l1; l < l2; l++) {
	if (l < 0) {h3s[0] = histsQA[kPositive][k][i]; h3s[1] = histsQA[kNegative][k][i];}
	else       {h3s[0] = histsK[l][kPositive][k][i]; h3s[1] = histsK[l][kNegative][k][i];}
	cout << "l = " << l << "\t";
	if (! h3s[0] || ! h3s[1]) {cout << "No. Plots" << endl; continue;}
	cout << h3s[0]->GetName() << "\t" << h3s[1]->GetName() << endl;
	Double_t min =  1e9;
	Double_t max = -1e9;
	if (k == kPr) {min = plotPrVar[i].min;  max = plotPrVar[i].max;}
	else          {min = plotGlVar[i].min;  max = plotGlVar[i].max;}
	DrawH3s(h3s, animate, min, max);
      }
    }
  }
}
//________________________________________________________________________________
void DrawEff(Double_t ymax=1.0, Double_t pTmin = -1, Int_t animate=0) {// Efficiencies
  Check();
  struct Eff_t {
    const Char_t *Name;
    const Char_t *Title;
    TrackMatchType kDividend;
    TrackMatchType kDivider;
    Double_t min, max;
  };
  enum Effiencies {kEffTotal = 9};
  static Int_t Break = 0;
  Eff_t eff[kEffTotal] = {
    {"GeomA", "Geometrical acceptance",kMcHit,   kMc,    0.0, 100.0},
    {"EffA",  "Effeciency over all",   kMcRec,   kMc,    0.0, 100.0},
    {"EffG",  "Effeciency wrt Geom",   kMcRec,   kMcHit, 0.0, 100.0},
    {"CloneA","Clone  over all",       kMcClone, kMc,    0.0,  25.0},
    {"CloneG","Clone wrt Geom",        kMcClone, kMcHit, 0.0,  60.0},
    {"LostA", "Lost over all",         kMcLost,  kMc,    0.0,  50.0},
    {"LostG", "Lost wrt Geom",         kMcLost,  kMcHit, 0.0,  70.0},
    {"GhostA","Ghost over all",        kRcGhost, kMc,    0.0, 110.0},
    {"GhostG","Ghost wrt Geom",        kRcGhost, kMcHit, 0.0, 110.0}
  };
  const Double_t pTmins[4] = {0.11, 0.5, 1.01, 2.0};
  //                       Phi Eta pT
  const Char_t *proj3[3] = {"x","y","z"};
  for (Int_t k = kGl; k < kTotalT; k++) {
    for (Int_t i = 0; i < kEffTotal; i++) {
      Int_t i1 = eff[i].kDividend;
      Int_t i2 = eff[i].kDivider;
      for (Int_t p = 0; p < 3; p++) { // projections
	TString Name(eff[i].Name);
	TString Title(eff[i].Title);
	TH1 *heff[8]; memset(heff, 0, sizeof(heff));
	Double_t min = eff[i].min;
	Double_t max = eff[i].max;
	Int_t NS = kTotalSigns;
	if (pTmin < 0 && p != 2) NS *= 4;
	for (Int_t l = kPositive; l < NS; l++) {
	  Int_t s = l%kTotalSigns;
	  TH3F *Dividend = hists[s][k][i1];
	  TH3F *Divider  = hists[s][k][i2];
	  Int_t nbinsX = Dividend->GetNbinsX();
	  Int_t nbinsY = Dividend->GetNbinsY();
	  Int_t nbinsZ = Dividend->GetNbinsZ();
	  Int_t binX1 = 1, binX2 = nbinsX;
	  Int_t binY1 = 1, binY2 = nbinsY;
	  Int_t binZ1 = 1, binZ2 = nbinsZ;
	  Dividend->GetYaxis()->SetRange(binY1,binY2);
	  Divider->GetYaxis()->SetRange(binY1,binY2);
	  Dividend->GetZaxis()->SetRange(binZ1,binZ2);
	  Divider->GetZaxis()->SetRange(binZ1,binZ2);
	  cout << "Sum " << Divider->GetName() << "\tentries = " << Divider->GetEntries() << endl;
	  cout << "Eff " << Dividend->GetName() << "\tentries = " << Dividend->GetEntries() << endl;
	  if (p != 1) {
	    binY1 = Dividend->GetYaxis()->FindBin(-ymax);
	    binY2 = Dividend->GetYaxis()->FindBin( ymax);
	    Dividend->GetYaxis()->SetRange(binY1,binY2);
	    Divider->GetYaxis()->SetRange(binY1,binY2);
	  } 
	  if (p != 2) {
	    if (NS == kTotalSigns) {
	      binZ1 = Dividend->GetZaxis()->FindBin(pTmin);
	    } else {
	      binZ1 = Dividend->GetZaxis()->FindBin(pTmins[l/2]);
	    }
	    Dividend->GetZaxis()->SetRange(binZ1,binZ2);
	    Divider->GetZaxis()->SetRange(binZ1,binZ2);
	  }
	  heff[l] = Dividend->Project3D(proj3[p]); 
	  if      (p == 0) heff[l]->SetXTitle(Dividend->GetXaxis()->GetTitle());
	  else if (p == 1) heff[l]->SetXTitle(Dividend->GetYaxis()->GetTitle());
	  else if (p == 2) heff[l]->SetXTitle(Dividend->GetZaxis()->GetTitle());
	  if (l == 0) heff[l]->SetName(Form("%s%s",eff[i].Name,heff[l]->GetName()));
	  else        heff[l]->SetName(Form("%s%s_%i",eff[i].Name,heff[l]->GetName(),l));
	  heff[l]->SetTitle(Form("%s for %s vs %s",eff[i].Title,TitleTrType[k],heff[l]->GetXaxis()->GetTitle()));
	  heff[l]->SetYTitle("Efficiency (%)");
	  heff[l]->SetStats(0);
	  heff[l]->SetMarkerColor(l+1);
	  Title = heff[l]->GetTitle();
	  if (binY1 != binY2) Title += Form(" at |  #eta | <= %3.1f",ymax);
	  if (binZ1 > 0)      Title += Form(" at pT > %3.2f",pTmins[l/2]);
	  heff[l]->SetTitle(Title);
	  TH1 *temp =Divider->Project3D(proj3[p]); 
	  cout << heff[l]->GetName() << "\t" << heff[l]->GetEntries() << " sum " << temp->GetEntries() << endl;
	  if (temp->GetEntries() < 1) continue;
	  if (temp->GetNbinsX() != heff[l]->GetNbinsX()) {
	    cout << "No. of bins in " <<  heff[l]->GetName() << " and " << temp->GetName() << " is different. Ignore these histograms" << endl;
	    delete heff[l]; heff[l] = 0;
	    delete temp;
	    continue;
	  }
	  Double_t Val = 0;
	  Double_t Sum = 0;
	  for (Int_t bin = binX1; bin <= binX2; bin++) {
	    Double_t val = heff[l]->GetBinContent(bin); Val += val;
	    Double_t sum = temp->GetBinContent(bin);    Sum += sum;
	    Double_t err = 0;
	    if (sum < 1.e-7 || val > sum) {val = 1.05;}
	    else { val /= sum;     err = TMath::Sqrt(val*(1.-val)/sum);}
	    heff[l]->SetBinContent(bin,100*val);
	    heff[l]->SetBinError(bin,100*err);
	  }
	  cout << heff[l]->GetName() 
	       << "[" << binX1 << "," << binX2 << "]"
	       << "[" << binY1 << "," << binY2 << "]"
	       << "[" << binZ1 << "," << binZ2 << "]"
	       << " Val = " << Val << "\tSum = " << Sum << endl;
#ifdef __MinMax__
	  MinMax(heff[l],min,max,200);
#endif /* __MinMax__ */
	}
	if (heff[0] && heff[1]) {
	  Name = heff[0]->GetName();
	  TCanvas *c = new TCanvas(Name.Data(),Name.Data(),400,400);
	  if (p == 2) c->SetLogx(1);
	  TLegend *l = 0;
	  if (NS > kTotalSigns) l = new TLegend(0.1,0.4,0.4,0.6);
	  for (Int_t s = kPositive; s < NS; s++) {
	    if (s == kPositive) {heff[s]->SetMinimum(min); heff[s]->SetMaximum(max); heff[s]->Draw();}
	    else                 heff[s]->Draw("same");
	    if (l) l->AddEntry(heff[s],Form("%s with pT/|q| > %3.1f",TitleCharge[s%2],pTmins[s/2]));
	    if (l && s == kPositive) l->Draw();
	    c->Update();
	  }
	  if (animate) ForceAnimate(0,200);
	  DrawPng(c);
	  if (Break) return;
	  delete c;
	}
      }
    }
  }
}
//________________________________________________________________________________
void DrawdEdx() {
  Check();
  TH3F *ZI70piPzB = (TH3F *) gDirectory->Get("ZI70piPzB");
  if (! ZI70piPzB) return;
  TH3F *ZI70pizB = new TH3F(*ZI70piPzB);
  ZI70pizB->SetName("ZI70pizB");
  TH3F *ZI70piNzB = (TH3F *) gDirectory->Get("ZI70piNzB"); 
  if (ZI70piNzB) {
    ZI70pizB->Add(ZI70piNzB);
  }
  TString Name(ZI70pizB->GetName());
  TCanvas *c = new TCanvas(Name.Data(),Name.Data(),400,400);
  ZI70pizB->Project3D("zx")->Draw("colz");
  TH2 *ZI70pizB_zx = (TH2 *) gDirectory->Get("ZI70pizB_zx");
  if (! ZI70pizB_zx) return;
  ZI70pizB_zx->FitSlicesY(0,0,-1,10,"qeg3s");
  TH1 *ZI70pizB_zx_2 = (TH1 *) gDirectory->Get("ZI70pizB_zx_2");
  if (! ZI70pizB_zx_2) return;
  ZI70pizB_zx_2->SetAxisRange(20,220);
  ZI70pizB_zx_2->SetMaximum(0.16);
  ZI70pizB_zx_2->SetMinimum(0.04);
  ZI70pizB_zx_2->SetStats(0);
  ZI70pizB_zx_2->SetTitle("dE/dx resolution versus track length in active TPC");
  ZI70pizB_zx_2->SetXTitle("track length in active TPC (cm)");
  TF1 *pl3 = TPolynomial::MakePol(3);
  ZI70pizB_zx_2->Fit(pl3,"er","",20,220);
  TLegend *l = new TLegend(0.15,0.8,0.85,0.9);
  l->AddEntry(ZI70pizB_zx_2,Form("#sigma(@76cm) = %5.2f%\% : #sigma(@128cm) = %5.2f%\%",
				 100*pl3->Eval(76),100*pl3->Eval(128)));
  l->Draw();
  c->Update();
  DrawPng(c);
  delete c;
}
//________________________________________________________________________________
void Draw() {
  Check();
  DrawQA();
  DrawEff();
  DrawdEdx();
}
//________________________________________________________________________________
void MuMcL3(Long64_t nevent = 999999,
	  //	  const char* file="/star/rcf/test/dev/trs_sl302.ittf/Wed/year_2011/pp500_pileup/rcf10100_90_200evts_Wplus_enu.MuDst.root",
	  const char* file="MuDst/*.MuDst.root",
	  const char* filter="st:MuDst.root",
	  const  char* outFile="MuMcL3.root") {
  //  int counter=0;
  TString CDir(gSystem->BaseName(gSystem->pwd()));
  if (CDir.Contains("devT") && CDir != "devTR") MinNoMcTpcHits = 11;
  cout << "Run with " << CDir.Data() << " set MinNoMcTpcHits = " << MinNoMcTpcHits << " which corrresponds to 10 real hit in TPC" << endl;
  StMuTimer timer;
  Init(outFile);
  timer.start();
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,filter,1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  cout << "time to load chain: " << timer.elapsedTime() <<endl;
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent",
    "PrimaryVertices",
    "PrimaryTracks",
    "L3Tracks",
    "CovPrimTrack",
    "CovGlobTrack",
    "StStMuMcVertex",
    "StStMuMcTrack"
#if 0
    ,"KFTracks",
    "KFVertices"
#endif
  };
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  timer.reset();
  timer.start();
  TChain *tree = maker->chain();
  if (! tree) {
    cout << "No TTree" << endl;
    return;
  }
  Long64_t nentries = tree->GetEntries();
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  if (nentries <= 0) return;
#if 0
  tree->SetParallelUnzip();      //  parallel unzipping
#endif
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,0)
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);
#endif
  for (Long64_t ev = 0; ev < nevent; ev++) {
#if 0
    tree->LoadTree(ev);  //this call is required when using the cache;
    tree->GetEntry(ev);        //read complete event in memory
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    mu->set(maker);
#else
    if (maker->Make()) break;
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
#endif
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    if (Debug()) {cout << " #" << ev;}
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (Debug()) {cout << "\tPrimaryVertices " << NoPrimaryVertices;}
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();      if (Debug()) {cout << "\tPrimaryTracks " << NoPrimaryTracks;}
    TClonesArray *L3Tracks     = mu->array(muL3);  
    Int_t NoL3Tracks = L3Tracks->GetEntriesFast();        if (Debug()) {cout << "\tL3Tracks " << NoL3Tracks;}
    TClonesArray *CovPrimTrack     = mu->covPrimTrack();          if (Debug()) {cout << "\tCovPrimTrack " << CovPrimTrack->GetEntriesFast();}
    TClonesArray *CovGlobTrack     = mu->covGlobTrack();          if (Debug()) {cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();}
    TClonesArray *MuMcVertices   = mu->mcArray(0); 
    Int_t NoMuMcVertices = MuMcVertices->GetEntriesFast();        if (Debug()) {cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices;}
    TClonesArray *MuMcTracks     = mu->mcArray(1); 
    Int_t NoMuMcTracks = MuMcTracks->GetEntriesFast();            if (Debug()) {cout << "\t" << StMuArrays::mcArrayTypes[1] << " " << NoMuMcTracks;}
    TClonesArray *KFTracks = mu->KFTracks();
    Int_t NoKFTracks = KFTracks->GetEntriesFast();                if (Debug()) {cout << "\tKFTracks " << NoKFTracks;}
    TClonesArray *KFVertices = mu->KFVertices();
    Int_t NoKFVertices = KFVertices->GetEntriesFast();            if (Debug()) {cout << "\tKFVertices " << NoKFVertices;}
    if (Debug())                                                               {cout << endl;}
    const Double_t field = muEvent->magneticField()*kilogauss;
    if (! NoMuMcVertices || ! NoMuMcTracks) {
      cout << "Ev. " << ev << " has no MC information ==> skip it" << endl;
      continue;
    }
#if 0
    // =============  Build map between global and primary tracks from proper vertex
    map<Int_t,Int_t> Gl2Pr;
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
      if (! Accept(pTrack)) continue;
      Int_t l = pTrack->vertexIndex();
      if (l < 0) continue;
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (! Vtx) continue; // ??????
      if (Vtx->idTruth() != 1) continue;
      Int_t kg = pTrack->index2L3();
      Gl2Pr.insert(pair<Int_t,Int_t>(kg,k));
    }
#endif
    // =============  Build map between global and Mc tracks
    multimap<Int_t,Int_t> Mc2RcTracks;
    for (Int_t kg = 0; kg < NoL3Tracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) L3Tracks->UncheckedAt(kg);
      if (! Accept(gTrack)) continue;
      //      gTrack->Print();
      // Check Mc
      if (gTrack->idTruth() < 0 || gTrack->idTruth() > NoMuMcTracks) {
	cout << "Illegal idTruth " << gTrack->idTruth() << " The track is ignored" << endl;
	continue;
      }
      StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(gTrack->idTruth()-1);
      if (mcTrack->Id() != gTrack->idTruth()) {
	cout << "Mismatched idTruth " << gTrack->idTruth() << " and mcTrack Id " <<  mcTrack->Id() 
	     << " The track is ignored" <<  endl;
      }
      //      mcTrack->Print();
      Mc2RcTracks.insert(pair<Int_t,Int_t>(gTrack->idTruth()-1,kg)); // Id shifted by 1
    }
    // =============  Build map between  Rc and Mc vertices
    multimap<Int_t,Int_t> Mc2RcVertices;
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (! AcceptVX(Vtx)) continue;
      //      Vtx->Print();
      // Check Mc
      if (Vtx->idTruth() < 0 || Vtx->idTruth() > NoMuMcVertices) {
	cout << "Illegal idTruth " << Vtx->idTruth() << " The track is ignored" << endl;
	continue;
      }
      StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(Vtx->idTruth()-1);
      if (mcVertex->Id() != Vtx->idTruth()) {
	cout << "Mismatched idTruth " << Vtx->idTruth() << " and mcVertex Id " <<  mcVertex->Id() 
	     << " The vertex is ignored" <<  endl;
      }
      //      mcVertex->Print();
      Mc2RcVertices.insert(pair<Int_t,Int_t>(Vtx->idTruth()-1,l)); // Id shifted by 1
    }
    // Loop over Mc Tracks
    for (Int_t m = 0; m < NoMuMcTracks; m++) {
      StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(m);
      if (! mcTrack) continue;
      Int_t IdVx = mcTrack->IdVx();
      GiD[0]->Fill(mcTrack->GePid());
      if (mcTrack->No_tpc_hit() >= MinNoMcTpcHits) GiD[1]->Fill(mcTrack->GePid());
      if (IdVx == 1) {
	GiD[2]->Fill(mcTrack->GePid());
	if (mcTrack->No_tpc_hit() >= MinNoMcTpcHits) GiD[3]->Fill(mcTrack->GePid());
      }
      if (! mcTrack->Charge()) continue;
      TrackMatchType s = kPositive;
      if (mcTrack->Charge() < 0) s = kNegative;
      hists[s][kGl][kMc]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp());
      if (IdVx == 1) hists[s][kPr][kMc]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp());
      if (mcTrack->No_tpc_hit() < MinNoMcTpcHits) continue; 
      hists[s][kGl][kMcHit]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp());
      if (IdVx == 1) hists[s][kPr][kMcHit]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp());
      TrackMcType kType = TrackType(mcTrack,Mc2RcTracks);
      Int_t l = -1;
      switch (kType) {
      case kMatched: l = kMcRec  ; break;
      case kLost:    l = kMcLost ; break;
      case kClone:   l = kMcClone; break;
      default: l = 1; break;
      }
      hists[s][kGl][l]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp());
      if (IdVx == 1) hists[s][kPr][l]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp());
      if (l == kMcRec) {
	Int_t Id = mcTrack->Id()-1;
	pair<multimap<Int_t,Int_t>::iterator,multimap<Int_t,Int_t>::iterator> ret;
	ret = Mc2RcTracks.equal_range(Id);
	multimap<Int_t,Int_t>::iterator it;
	Int_t kg = -1;
	Int_t count = 0;
	for (it=Mc2RcTracks.equal_range(Id).first; it!=Mc2RcTracks.equal_range(Id).second; ++it, ++count) {
	  kg = (*it).second;
	}
	assert(count == 1);
	// Track QA
	StMuTrack *gTrack = (StMuTrack *) L3Tracks->UncheckedAt(kg);
	Int_t kgc = gTrack->index2Cov();
	if (kgc < 0) continue;
	StDcaGeometry *dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
	StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(IdVx-1);
	FillQAGl(gTrack, mcTrack, dcaG, mcVertex);
#if 0
	Int_t k = Gl2Pr[kg];
	if (! k) continue;
	StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
	Int_t kpc = pTrack->index2Cov();
	if (kpc < 0) continue;
	StMuPrimaryTrackCovariance *cov = (StMuPrimaryTrackCovariance *) CovPrimTrack->UncheckedAt(kpc);
	FillQAPr(pTrack, mcTrack, cov);
	// dE/dx block
	const StMuProbPidTraits &PiD = pTrack->probPidTraits();
	Double_t I[2] = {PiD.dEdxTruncated(), PiD.dEdxFit()};
	Double_t TrackLength = PiD.dEdxTrackLength();
	Int_t Gid = mcTrack->GePid();
	static Bichsel *m_Bichsel = Bichsel::Instance();
	Double_t pMomentum = pTrack->helix().momentum(field).mag();
	//	const StThreeVectorF &pVx  = pTrack->momentum();
	for (Int_t h = 0; h < NHYPS; h++) {
	  if (GEANTiD[h] == Gid) {
	    Double_t bghyp = TMath::Log10(pMomentum/Masses[h]);
	    Double_t Pred[2]  = {1.e-6*m_Bichsel->GetI70(bghyp,1.0),
				 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(bghyp,1.0))};
	    for (Int_t mm = 0; mm < 2; mm++) {
	      if (I[mm] <= 0 || Pred[mm] <= 0) continue;
	      Double_t z = TMath::Log(I[mm]/Pred[mm]);
	      PdEdx[mm][h]->Fill(pTrack->phi(), pTrack->eta(), pTrack->pt(), z);
	      LdEdx[mm][h]->Fill(TrackLength, bghyp, z);
	    }
	    break;
	  }
	}
#endif
      }
    }
    // check for ghosts
    for (Int_t kg = 0; kg < NoL3Tracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) L3Tracks->UncheckedAt(kg);
      if ( ! AcceptGhost(gTrack)) continue;
      if ( gTrack->idTruth()) continue;
      TrackMatchType s = kPositive;
      if (gTrack->charge() < 0) s = kNegative;
      hists[s][kGl][kRcGhost]->Fill(TMath::RadToDeg()*gTrack->phi(),gTrack->eta(),(gTrack->charge()*gTrack->pt()));
    }
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (Vtx->idTruth() != 1) continue;
      for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
	if (! pTrack) continue;
        if (pTrack->vertexIndex() != l) continue;
	if (! AcceptGhost(pTrack)) continue;
	if (pTrack->idParentVx() == 1) continue;
	TrackMatchType s = kPositive;
	if (pTrack->charge() < 0) s = kNegative;
	hists[s][kPr][kRcGhost]->Fill(TMath::RadToDeg()*pTrack->phi(),pTrack->eta(),pTrack->pt());
      }
    }
#if 0
    // Loop over KF Vetrices and KF particles
    // Map between Id and position in Clones Array
    map<Int_t,Int_t> VerId2k;
    for (Int_t l = 0; l < NoKFVertices; l++) {
      const KFVertex *vertex = (const KFVertex *) KFVertices->UncheckedAt(l);
      if (! vertex) continue;
      Int_t Id = vertex->GetID();
      VerId2k[Id] = l;
    }
    map<Int_t,Int_t> ParId2k;
    for (Int_t k = 0; k < NoKFTracks; k++) {
      const KFParticle *particle = (const KFParticle *) KFTracks->UncheckedAt(k);
      if (! particle) continue;
      Int_t Id = particle->GetID();
      ParId2k[Id] = k;
    }
    for (Int_t l = 0; l < NoKFVertices; l++) {
      const KFVertex *vertex = (const KFVertex *) KFVertices->UncheckedAt(l);
      cout << *vertex << endl;
      if (vertex->IdTruth()) {
	StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(vertex->IdTruth()-1);
	if (mcVertex) cout << "Mc Vertex:" << *mcVertex << endl;
      }
      Int_t IdPtrk = vertex->GetParentID(); //reconstructed parent track
      if (IdPtrk) {
	Int_t k = ParId2k[IdPtrk];
	const KFParticle *particle = (const KFParticle *) KFTracks->UncheckedAt(k);
	if (particle) cout << "Parent Track:" << *particle << endl;
      }
      Int_t m = vertex->IdParentMcVx(); // MC parent track
      if (m) {
	StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(m-1);
	if (! mcTrack) continue;
	cout << "Parent Mc Track:" << *mcTrack << endl;
      }
    }
    cout << "-----------------------------------" << endl;
    for (Int_t k = 0; k < NoKFTracks; k++) {
      const KFParticle *particle = (const KFVertex *) KFTracks->UncheckedAt(k);
      cout << *particle << endl;
      if (! particle->GetID()) {cout << "beam" << endl; continue;}
      Int_t IdPVx = particle->GetParentID(); //reconstructed parent vertex
      if (particle->IdTruth()) {
	StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(particle->IdTruth()-1);
	if (mcTrack) cout << "Mc Track:" << *mcTrack << endl;
      }
      if (IdPVx) {
	Int_t l = VerId2k[IdPVx];
	const KFVertex *vertex = (const KFVertex *) KFVertices->UncheckedAt(l);
	if (vertex) cout << "Parent Vertex:" << *vertex << endl;
      }
      Int_t m = particle->IdParentMcVx(); // MC parent vertex
      if (m) {
	StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(m-1);
	if (! mcVertex) continue;
	cout << "Parent Mc Vertex:" << *mcVertex << endl;
      }
      
    }
    cout << "===================================" << endl;
#endif
  }
  if (nentries >= 1000) {
    Draw();
  }
  if (fOut) fOut->Write();
}
