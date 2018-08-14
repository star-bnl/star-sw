/* 

   root.exe lMuDst.C MuMc.C+
   root.exe lMuDst.C MuMc.root

>> .L MuMc.C+;  Init(0); DrawEff(); DrawQA(); // FPE_OFF

*/

//#define HFT_ACCEPTANCE
//#define TPC_ACCEPTANCE
#define FTS_ACCEPTANCE
#define DEBUG


//#define PTCUT(pt) (pt<0.3 /* accept pt > 300 MeV */ ) 
float ptMn = 0.00;
#define PTCUT(pt) (pt<ptMn) /* accept pT > ptMn  false-->accept, true-->reject */

// Vertex and eta cuts on tracks
float etaMn   = -2.00;
float etaMx   = +5.00;
float vertzMn = -50.0;
float vertzMx = +50.0;

//int assume_decay_vtx = 1;

#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include <map>
#include <utility>
#include <vector>
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
#define TESTBIT(n,i) ((bool)(((n) & (1 << i)) != 0))
class StMuDstMaker;
#endif
#endif



bool warn_illegal_idtruth  = true;
bool warn_mismatch_idtruth = true;


//void FitSlicesYInRange( TH2* h, double mmin, double mmax, int cut = 10 );
//________________________________________________________________________________
void FitSlicesYInRange( TH2* h, double a, double b, int cut=10 ){
  TF1 gaus("gaus","gaus(0)",a,b);
  gaus.SetRange(a,b);
  h->FitSlicesY(&gaus,0,-1,cut, "qeg3s"); 
}

//________________________________________________________________________________
bool  VZCUT( const StMuMcVertex *vtx ) { 

  const StThreeVectorF & vertex = vtx->XyzV();
  if ( vertex[2] < vertzMn || vertex[2] > vertzMx ) return true;

  return false; // false accepts, true rejects
}
//________________________________________________________________________________
bool  ETACUT( StMuMcTrack *track ){
//
// Cut on track pseudorapidity.  Please note: this will implicitly select
// primary tracks and reject secondaries.  IF secondaries should be inclued
// (e.g. when looking for efficiencies for displaced vertices, etc...) we
// need to access the 3-vector momentum of the track to obtain eta.
//
// NOTE: Even with changing how eta is calculated, need to change the way
// that the code matches to the MC vertex... I believe it is implicitly
// assumed the we only look at tracks associated to the *primary* MC vertex.
//
  float eta = track->Eta(); //  float eta = track->Pxyz().pseudoRapidity();

  if ( eta < etaMn || eta > etaMx ) {
    //    cout << etaMn << " " << eta << " " << etaMx << endl;
    return true;
  }
  return false;
}
//________________________________________________________________________________
TArrayD LogBins( const int nbinx, const double  xmin, const double xmax ){
// Helper function to define a set of logarithmic bins for a histogram
  double xxmin = TMath::Log(xmin);
  double xxmax = TMath::Log(xmax);
  double delx  = xxmax - xxmin;
  delx /= nbinx;
  //  double xbins[ nbinx+1 ];
  TArrayD xbins( nbinx+1 );
  for ( int ii=0;ii<=nbinx; ii++ ) {
      xbins[ii] =  TMath::Exp( xxmin + ii*delx );
      cout << Form("%02i %f",ii,xbins[ii] ) << endl;
    }
  return xbins;
}



//________________________________________________________________________________
StMuDstMaker* maker = 0;

// Minimum number of MC hits in each tracking detector for track to be considered in the acceptance

#ifdef HFT_ACCEPTANCE
int MinNoMcTpcHits = 29;
int MinNoMcPxlHits = 2;
int MinNoMcIstHits = 1;
int MinNoMcSstHits = 0;
int MinNoMcHits    = MinNoMcTpcHits + MinNoMcPxlHits + MinNoMcIstHits + MinNoMcSstHits;

int MinNoTpcHits = 10;
int MinNoPxlHits =  2;
int MinNoIstHits =  1;
int MinNoSsdHits =  0;
#endif
#ifdef TPC_ACCEPTANCE
int MinNoMcTpcHits = 29;
int MinNoMcPxlHits = 0;
int MinNoMcIstHits = 0;
int MinNoMcSstHits = 0;
int MinNoMcHits    = MinNoMcTpcHits + MinNoMcPxlHits + MinNoMcIstHits + MinNoMcSstHits;

int MinNoTpcHits = 10;
int MinNoPxlHits =  0;
int MinNoIstHits =  0;
int MinNoSsdHits =  0;
#endif
#ifdef FTS_ACCEPTANCE
int MinNoMcTpcHits = 0;
int MinNoMcPxlHits = 0;
int MinNoMcIstHits = 0;
int MinNoMcSstHits = 0;
int MinNoMcFtsHits = 7; // All 7 FTS planes
int MinNoMcHits    = MinNoMcTpcHits + MinNoMcPxlHits + MinNoMcIstHits + MinNoMcSstHits;

int MinNoTpcHits =  0;
int MinNoPxlHits =  0;
int MinNoIstHits =  0;
int MinNoSsdHits =  0;
int MinNoFtsHits =  6; // 6 of 7 planes
#endif
//________________________________________________________________________________
enum TrackMcType {
  kNotDefined, 
  kMatched, 
  kLost, 
  kGost, 
  kClone, 
  kTotalMcType
};
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
  int nx;
  double xmin, xmax;
  int ny;
  double ymin, ymax;
  int nz;
  double zmin, zmax;
  double  min,  max; // min and max for plots
};
//________________________________________________________________________________
TH3F *hXYfirst, *hXYlast; // Y vs X vs mccharge
TH3F *hRZfirst, *hRZlast; // R vs Z vs mccharge

//TH3F *hVertexXYZ; // RC - MC vertex

TH3F **hists[2][2]   = {{0,0}, {0,0}};                      // Efficiencies versus phi, eta and pT
TH3F **histsQA[2][2] = {{0,0}, {0,0}};                      // Quality versus NfitPoints and Nbad hits
TH3F **histsK[2][2][2]  = {{{0,0}, {0,0}}, {{0,0}, {0,0}}}; // Quality versus eta and pT, 0 -> all, 1 -> pion
TH3F **histsL[2][2][2]  = {{{0,0}, {0,0}}, {{0,0}, {0,0}}}; // Quality versus phi and pT, 0 -> all, 1 -> pion
TProfile3D *PdEdx[2][NHYPS];                                // <z> = <log((dE/dx)_measured/(dE/dx)_predicted)> versus phi, eta and pT; 0 => I70, 1 => Fit
TH3F *LdEdx[2][NHYPS];                                      //  z  =  log((dE/dx)_measured/(dE/dx)_predicted)  versus TpcTrackLength and log10(beta*gamma);  0 => I70, 1 => Fit
//TH1F *GiD[4] = {0, 0, 0, 0};
vector<TH1F *> GiD;
TH2F *McRcHit = 0;
TFile *fOut = 0;
const Char_t *NameCharge  [kTotalSigns] = {"Pos", "Neg"};
const Char_t *TitleCharge [kTotalSigns] = {"(+)", "(-)"};
const Char_t *NameTrType  [kTotalT] = {"Gl", "Pr"};
const Char_t *TitleTrType [kTotalT] = {"Global Tracks", "Primary Tracks"};
const Char_t *proj[] = {"zx","zy","xy","x","y"};

// Names for efficiency plots
const  PlotName_t plotNameMatch[kTotalMatchType] = { 
  {kMc,      "Mc",      "Mc tracks All"},
  {kMcHit,   "McHit",   Form("Mc tracks which have >= %i Mc Hits",MinNoMcHits)},
  {kMcRec,   "McRec",   "Rc tracks matched with only Mc track"},
  {kMcClone, "McClone", "Rc tracks matched with > 1 Mc track (Clone)"},
  {kRcGhost, "RcGhost", "Rc tracks without Mc partner"},
  {kMcLost,  "McLost",  "Mc tracks without reconstructed one"}
};
struct VarGl_t {
  double ChiSqXY;
  double dDcaXY; 
  double dDcaZ;  
  double dPsi;   
  double dPti;  
  double dPtiR; 
  double dTanL;  
  double pDcaXY; 
  double pDcaZ;  
  double pPsi;   
  double pPti;  
  double pPtiR; 
  double pTanL;  
};
const Char_t *HitName = "vs NoFitPnts and no. bad hits";
const Char_t *KinName = "vs   #eta and pT";
const Char_t *KinPionName = "vs   #eta and pT for pion";
#ifndef __DEVT__
const int noFit =  47;
#else
const int noFit = 100;
#endif
const int nbinPlotVar = 25;

//________________________________________________________________________________
#ifdef HFT_ACCEPTANCE
const VarName_t plotGlVar[kTotalQAGl] = {        //no.fit                   no.bad          
  {"ChiSqXY",   "#chi^{2}_{Track}/NDF",          noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar,  0.,  10., 0.000, 6.000},
  {"dDcaXY",    "difference in Dca_{XY}",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar*10, -0.250,  0.250, -.0020, 0.020},
  {"dDcaZ",     "difference in Dca_{Z}",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar*10, -0.250,  0.250, -.0020, 0.020},
  {"dPsi",      "difference in  #Psi ",          noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.01, 0.01, -.004, 0.040},
  {"dPti" ,     "difference in q/pT",            noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar*10, -0.1, 0.1, -.020, 0.200},
  {"dPtiR" ,    "difference in relative q/pT",   noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar*10, -0.1, 0.1, -.004, 0.040},
  {"dTanL",     "difference in tan( #lambda )",  noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.004, 0.040},
  {"pDcaXY",    "pull in Dca_{XY}",              noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.300, 3.000},
  {"pDcaZ",     "pull in Dca_{Z}",               noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.100,10.000},
  {"pPsi",      "pull in  #Psi ",                noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.300, 3.000},
  {"pPti" ,     "pull in q/pT",                  noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.250, 2.500},
  {"pPtiR" ,    "pull in relative q/pT",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.230, 3.500},
  {"pTanL",     "pull in tan( #lambda )",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.600, 6.000}
};
#endif
#ifdef TPC_ACCEPTANCE
const VarName_t plotGlVar[kTotalQAGl] = {        //no.fit                   no.bad          
  {"ChiSqXY",   "#chi^{2}_{Track}/NDF",          noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar,  0.,  10., 0.000, 6.000},
  {"dDcaXY",    "difference in Dca_{XY}",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -5.,   5., -.250, 1.500},
  {"dDcaZ",     "difference in Dca_{Z}",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -5.,   5., -.250, 1.500},
  {"dPsi",      "difference in  #Psi ",          noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.004, 0.040},
  {"dPti" ,     "difference in q/pT",            noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.020, 0.200},
  {"dPtiR" ,    "difference in relative q/pT",   noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.004, 0.040},
  {"dTanL",     "difference in tan( #lambda )",  noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.004, 0.040},
  {"pDcaXY",    "pull in Dca_{XY}",              noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.300, 3.000},
  {"pDcaZ",     "pull in Dca_{Z}",               noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.100,10.000},
  {"pPsi",      "pull in  #Psi ",                noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.300, 3.000},
  {"pPti" ,     "pull in q/pT",                  noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.250, 2.500},
  {"pPtiR" ,    "pull in relative q/pT",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.230, 3.500},
  {"pTanL",     "pull in tan( #lambda )",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.600, 6.000}
};
#endif


#ifdef FTS_ACCEPTANCE
const VarName_t plotGlVar[kTotalQAGl] = {        //no.fit                   no.bad          
  {"ChiSqXY",   "#chi^{2}_{Track}/NDF",          10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar,  0.,  10., 0.000, 6.000},
  {"dDcaXY",    "difference in Dca_{XY}",        10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -5.,   5., -.250, 1.500},
  {"dDcaZ",     "difference in Dca_{Z}",         10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -5.,   5., -.250, 1.500},
  {"dPsi",      "difference in  #Psi ",          10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.004, 0.040},
  {"dPti" ,     "difference in q/pT",            10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.020, 0.200},
  {"dPtiR" ,    "difference in relative q/pT",   10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.004, 0.040},
  {"dTanL",     "difference in tan( #lambda )",  10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.004, 0.040},
  {"pDcaXY",    "pull in Dca_{XY}",              10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.300, 3.000},
  {"pDcaZ",     "pull in Dca_{Z}",               10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.100,10.000},
  {"pPsi",      "pull in  #Psi ",                10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.300, 3.000},
  {"pPti" ,     "pull in q/pT",                  10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.250, 2.500},
  {"pPtiR" ,    "pull in relative q/pT",         10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.230, 3.500},
  {"pTanL",     "pull in tan( #lambda )",        10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.600, 6.000}
};
#endif

/// HACKED... this is not consistent w/ TPC usage now... so need to fix...
const VarName_t plotPrVar[kTotalQAPr] = {
  {"ChiSqXY",   "#chi^{2}_{Track}/NDF",       10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar,  0.,  10., 0.000, 4.000},
  {"ChiSqZ",    "#chi^{2}_{Vx} ",             10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar,  0., 100., 0.000,10.000},
  {"deta",      "difference in  #eta",        10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.002, 0.025},
  {"dPsi",      "difference in  #Psi ",       10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.002, 0.020},
  {"dPti",      "difference in q/pT",         10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.015, 0.150},
  {"dPtiR",     "relative difference in q/pT",10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -0.1, 0.1, -.006, 0.060},
  {"peta",      "pull for  #eta",             10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10.,-1.000,10.000},
  {"pPsi",      "pull for  #Psi ",            10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.500, 5.000},
  {"pPti",      "pull for q/pT",              10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.500, 5.000},
  {"pPtiR",     "pull for Relative q/pT",     10+1, -0.5, 10 + 0.5, 11,-0.5, 10.5, nbinPlotVar, -10., 10., -.500, 5.000}
};

struct VarPr_t {
  double ChiSqXY;
  double ChiSqZ; 
  double deta;  
  double dPsi;   
  double dPti;   
  double dPtiR;
  double peta;  
  double pPsi;   
  double pPti;   
  double pPtiR;  
};

int nPng = 0;
//________________________________________________________________________________
void Init(const Char_t *outfName="Out.root") {

  int opt = 1; // On opt==1 read from file.  Otherwise...
  if (outfName) {// Create new file, book new historgrams
    fOut = new TFile(outfName,"recreate");
    opt = 0;
  }

#define LOG_PT_BINS
  //#define NEW_ETA_BINS
#define FWD_ETA_BINS


  // --------------------------------------------------------------------------
  //
  // Book efficiency plots in hists[...]
  //
#ifdef STD_PT_BINS // standard pT bins
  int    npT    = 98;        // Number of PT bins
  //  double pTMax =   10;
  const double ptBins[102] = {
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
#endif

#ifdef LOG_PT_BINS
  int npT = 20;
  const TArrayD _ptBins = LogBins( npT, 0.07, 5.0 );
  const double* ptBins = _ptBins.GetArray();
#endif

#ifdef FIXED_PT_BINS // fixed pT bins
  int    npT    = 100;
  double ptBins[101];
  ptBins[0] = 0;
  for (int i = 1; i <= npT; i++) ptBins[i] = i;
#endif

#if 1
  enum {nphiM = 3};
  double dphiMask[nphiM] = {0,  10, 20};
  int    nphi   = 12*nphiM;
  double *phiBins = new double [nphi+1];
  int i = 0;
  for (int sec = 0; sec < 12; sec++) {
    double phi = -180 + 30*sec;
    for (int j = 0; j < nphiM; j++, i++) {
      phiBins[i] = phi +  dphiMask[j];
    }
  }
  phiBins[nphi] = 180.;

#else
  int    nphi   = 6, i=0;
  double* phiBins = new double[nphi+1];
  phiBins[0]=-180;
  phiBins[1]=-120;
  phiBins[2]=- 60;
  phiBins[3]=   0;
  phiBins[4]=  60;
  phiBins[5]= 120;
  phiBins[6]= 180;
#endif
  



#ifdef STD_ETA_BINS
    const int    neta   = 60;
    const double etamax = 1.5;
    const double etamin = -1.5;
#endif

#ifdef NEW_ETA_BINS
    const int    neta = 15;
    const double etamax = 1.5;
    const double etamin = -1.5;
#endif

#ifdef FWD_ETA_BINS
    const int    neta   = 12;
    const double etamax = 5.0;
    const double etamin = 2.0;
#endif


  double deta = (etamax-etamin)/neta;
  double *etaBins = new double [neta+1];

  for (i = 0; i <= neta; i++) {
    etaBins[i] = etamin + deta*i;}


  for (int s = kPositive; s < kTotalSigns; s++) {
    for (int k = kGl; k < kTotalT; k++) {
      hists[s][k] = new TH3F*[kTotalMatchType];
      for (i = 0; i < kTotalMatchType; i++) {
	if (! opt) { // Book efficiency histogram
	  hists[s][k][i] = new TH3F(Form("%s%s%s",NameCharge[s],plotNameMatch[i].Name,NameTrType[k]), 
				    Form("%s %s %s %s",TitleCharge[s],plotNameMatch[i].Title,KinName,TitleTrType[k]),
				    nphi, phiBins,
				    neta, etaBins,
				    npT, ptBins);
	  hists[s][k][i]->GetXaxis()->SetTitle("#phi (degree)");
	  hists[s][k][i]->GetYaxis()->SetTitle("  #eta");         
	  hists[s][k][i]->GetZaxis()->SetTitle("pT (GeV/c)");   
	  hists[s][k][i]->SetMarkerColor(s+1);       
	} else { // Load efficiency histogram
	  hists[s][k][i] = (TH3F *) gDirectory->Get(Form("%s%s%s",NameCharge[s],plotNameMatch[i].Name,NameTrType[k]));
	}
      }
    }
  }
  // Done with efficiency histograms
  // ----------------------------------------------------------------------------------------------


  // Sanity check
  assert(kTotalQAPr == sizeof(VarPr_t)/sizeof(double));
  assert(kTotalQAGl == sizeof(VarGl_t)/sizeof(double));


  // -----------------------------------------------------------------------------------------------
  //
  // Book QA histograms
  //

  // Loop over sign states kPositive=0, kNegative=1, kTotalSigns=2 (in enum TrackMatchType)
  for (int s = kPositive; s < kTotalSigns; s++) {
    for (int k = kGl; k < kTotalT; k++) {         // gGl=0, kPr=1, kTotal=2 (in enum) 
      int         N = kTotalQAGl;                 // Total number of plots  (in enum)
      if (k == kPr) N = kTotalQAPr;                 // Total number of plots  (in enum)
      histsQA[s][k]    = new TH3F*[N];
      histsK[0][s][k]  = new TH3F*[N];
      histsK[1][s][k]  = new TH3F*[N];
      histsL[0][s][k]  = new TH3F*[N];
      histsL[1][s][k]  = new TH3F*[N];
      for (i = 0; i < N; i++) {                     // Loop over plots
	if (! opt) {
	  if (k == kGl) {                           // Book Global Track Plots
	    histsQA[s][k][i] = new TH3F(Form("%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]), 
					Form("%s %s %s %s",TitleCharge[s],plotGlVar[i].Title,HitName,TitleTrType[k]),
					plotGlVar[i].nx, plotGlVar[i].xmin, plotGlVar[i].xmax,
					plotGlVar[i].ny, plotGlVar[i].ymin, plotGlVar[i].ymax,
					plotGlVar[i].nz, plotGlVar[i].zmin, plotGlVar[i].zmax);
	    double *zBins = new double[plotGlVar[i].nz+1];
	    double dz = (plotGlVar[i].zmax - plotGlVar[i].zmin)/plotGlVar[i].nz;
	    for (int j = 0; j <= plotGlVar[i].nz; j++) zBins[j] = plotGlVar[i].zmin + dz*j;
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

	    histsL[0][s][k][i] = new TH3F(Form("All%s%s%s_phi",NameCharge[s],plotGlVar[i].Name,NameTrType[k]), 
					  Form("%s %s %s %s",TitleCharge[s],plotGlVar[i].Title,KinName,TitleTrType[k]),
					  nphi, phiBins,
					  npT, ptBins,
					  plotGlVar[i].nz, zBins);
	    histsL[1][s][k][i] = new TH3F(Form("pi%s%s%s_phi",NameCharge[s],plotGlVar[i].Name,NameTrType[k]), 
				       Form("%s %s %s %s",TitleCharge[s],plotGlVar[i].Title,KinName,TitleTrType[k]),
					  nphi, phiBins,
					  npT, ptBins,
					  plotGlVar[i].nz, zBins);

	    delete [] zBins;
	  } else {                                  // Book primary track plots
	    histsQA[s][k][i] = new TH3F(Form("%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]), 
					Form("%s %s %s",TitleCharge[s],plotPrVar[i].Title,TitleTrType[k]),
					plotPrVar[i].nx, plotPrVar[i].xmin, plotPrVar[i].xmax,
					plotPrVar[i].ny, plotPrVar[i].ymin, plotPrVar[i].ymax,
					plotPrVar[i].nz, plotPrVar[i].zmin, plotPrVar[i].zmax);
	    double *zBins = new double[plotPrVar[i].nz+1];
	    double dz = (plotPrVar[i].zmax - plotPrVar[i].zmin)/plotPrVar[i].nz;
	    for (int j = 0; j <= plotPrVar[i].nz; j++) zBins[j] = plotPrVar[i].zmin + dz*j;
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

	    histsL[0][s][k][i] = new TH3F(Form("All%s%s%s_phi",NameCharge[s],plotPrVar[i].Name,NameTrType[k]), 
					  Form("%s %s %s %s",TitleCharge[s],plotPrVar[i].Title,KinName,TitleTrType[k]),
					  nphi, phiBins,
					  npT, ptBins,
					  plotPrVar[i].nz, zBins);
	    histsL[1][s][k][i] = new TH3F(Form("pi%s%s%s_phi",NameCharge[s],plotPrVar[i].Name,NameTrType[k]), 
					  Form("%s %s %s %s",TitleCharge[s],plotPrVar[i].Title,KinName,TitleTrType[k]),
					  nphi, phiBins,
					  npT, ptBins,
					  plotPrVar[i].nz, zBins);

	    delete [] zBins;
	  }
	  histsQA[s][k][i]  ->GetXaxis()->SetTitle("No. of Fit Points");
	  histsQA[s][k][i]  ->GetYaxis()->SetTitle("No. of Bad Points"); 
	  histsK[0][s][k][i]->GetXaxis()->SetTitle("  #eta");             
	  histsK[0][s][k][i]->GetYaxis()->SetTitle("pT (GeV/c)");      	
	  histsK[1][s][k][i]->GetXaxis()->SetTitle("  #eta");	       	
	  histsK[1][s][k][i]->GetYaxis()->SetTitle("pT (GeV/c)");      	

	  histsL[0][s][k][i]->GetXaxis()->SetTitle("  #phi");             
	  histsL[0][s][k][i]->GetYaxis()->SetTitle("pT (GeV/c)");      	
	  histsL[1][s][k][i]->GetXaxis()->SetTitle("  #phi");	       	
	  histsL[1][s][k][i]->GetYaxis()->SetTitle("pT (GeV/c)");      	

	  if (k == kGl) {
	    histsQA[s][k][i]  ->GetZaxis()->SetTitle(plotGlVar[i].Title);
	    histsK[0][s][k][i]->GetZaxis()->SetTitle(plotGlVar[i].Title);
	    histsK[1][s][k][i]->GetZaxis()->SetTitle(plotGlVar[i].Title);
	    histsL[0][s][k][i]->GetZaxis()->SetTitle(plotGlVar[i].Title);
	    histsL[1][s][k][i]->GetZaxis()->SetTitle(plotGlVar[i].Title);
	  } else {
	    histsQA[s][k][i]  ->GetZaxis()->SetTitle(plotPrVar[i].Title);
	    histsK[0][s][k][i]->GetZaxis()->SetTitle(plotPrVar[i].Title);
	    histsK[1][s][k][i]->GetZaxis()->SetTitle(plotPrVar[i].Title);
	    histsL[0][s][k][i]->GetZaxis()->SetTitle(plotPrVar[i].Title);
	    histsL[1][s][k][i]->GetZaxis()->SetTitle(plotPrVar[i].Title);
	  }
	  histsQA[s][k][i]->SetMarkerColor(s+1);
	  histsK[0][s][k][i]->SetMarkerColor(s+1);
	  histsK[1][s][k][i]->SetMarkerColor(s+1);
	  histsL[0][s][k][i]->SetMarkerColor(s+1);
	  histsL[1][s][k][i]->SetMarkerColor(s+1);
	} else {
	  if (k == kGl) {
	    histsQA[s][k][i] = (TH3F *) gDirectory->Get(Form("%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]));
	    histsK[0][s][k][i] = (TH3F *) gDirectory->Get(Form("All%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]));
	    histsK[1][s][k][i] = (TH3F *) gDirectory->Get(Form("pi%s%s%s",NameCharge[s],plotGlVar[i].Name,NameTrType[k]));
	    histsL[0][s][k][i] = (TH3F *) gDirectory->Get(Form("All%s%s%s_phi",NameCharge[s],plotGlVar[i].Name,NameTrType[k]));
	    histsL[1][s][k][i] = (TH3F *) gDirectory->Get(Form("pi%s%s%s_phi",NameCharge[s],plotGlVar[i].Name,NameTrType[k]));
	  } else {
	    histsQA[s][k][i] = (TH3F *) gDirectory->Get(Form("%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]));
	    histsK[0][s][k][i] = (TH3F *) gDirectory->Get(Form("All%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]));
	    histsK[1][s][k][i] = (TH3F *) gDirectory->Get(Form("pi%s%s%s",NameCharge[s],plotPrVar[i].Name,NameTrType[k]));
	    histsL[0][s][k][i] = (TH3F *) gDirectory->Get(Form("All%s%s%s_phi",NameCharge[s],plotPrVar[i].Name,NameTrType[k]));
	    histsL[1][s][k][i] = (TH3F *) gDirectory->Get(Form("pi%s%s%s_phi",NameCharge[s],plotPrVar[i].Name,NameTrType[k]));
	  }
	}
      }
    }
  }
  delete [] phiBins;
  delete [] etaBins;
  // dE/dx block for matched primary tracks
  const Char_t *dEdxTypes[2] = {"I70","Fit"};
  for (int h = 0; h < NHYPS; h++) {
    for (i = 0; i < 2; i++) {
      PdEdx[i][h] = new TProfile3D(Form("Zav%s%s",dEdxTypes[i],HistNames[h]),
				   Form("< z_{%s} > versus  #phi,  #eta,  p_{T} for %s",dEdxTypes[i],Names[h]),
				   90, -TMath::Pi(), TMath::Pi(),
				   60, -1.2, 1.2,
				   npT, 0, 10, "S");
      PdEdx[i][h]->GetXaxis()->SetTitle("#phi (rad)");
      PdEdx[i][h]->GetYaxis()->SetTitle("  #eta");         
      PdEdx[i][h]->GetZaxis()->SetTitle("pT (GeV/c)");   
      PdEdx[i][h]->GetZaxis()->Set(npT,ptBins);
      LdEdx[i][h] = new TH3F(Form("Z%s%s",dEdxTypes[i],HistNames[h]),
			     Form(" z_{%s}  versus TpcTrackLength and log_{10} (#beta #gamma) for %s",dEdxTypes[i],Names[h]),
			     110, 0, 220, 220,-1,10, 100, -1, 1); 
    }
  }


  //
  // GEANT ID HISTOGRAMS
  //
  GiD .push_back( new TH1F("GiD",         "Geant ID for all MC tracks",                                            50,0.5,50.5) );
  GiD .push_back( new TH1F("GiDG",   Form("Geant ID for MC tracks with >= %i Tpc MC hits",MinNoMcHits),         50,0.5,50.5) );
  GiD .push_back( new TH1F("GiDPr",       "Geant ID for all primary MC tracks",                                    50,0.5,50.5) );
  GiD .push_back( new TH1F("GiDPrG", Form("Geant ID for primary MC tracks with >= %i Tpc MC hits",MinNoMcHits), 50,0.5,50.5) );



  //
  // HIT CORRELATIONS
  //
  McRcHit = new TH2F("McRcHit","No. RC hits in TPC versus No. MC ones",80,-0.5,79.5,80,-0.5,79.5);


  //
  // Plots of first vs last hit
  //
  hXYfirst = new TH3F("hXYfirst","First hit on track;X [cm]; Y [cm]; Q_{mc}",101,-225.0*1.005,+225*1.005,101,-225.0*1.005,+225*1.005,3,-1.5,1.5);
  hXYlast  = new TH3F("hXYlast","Last hit on track;X [cm]; Y [cm]; Q_{mc}",101,-225.0*1.005,+225*1.005,101,-225.0*1.005,+225*1.005,3,-1.5,1.5);

  hRZfirst = new TH3F("hRZfirst","First hit on track;Z [cm]; R [cm]; Q_{mc}", 101,-225.0*1.005,+225*1.005, 226, -0.5,+225.5,3,-1.5,1.5);
  hRZlast  = new TH3F("hRZlast","Last hit on track;Z [cm]; R [cm]; Q_{mc}",   101,-225.0*1.005,+225*1.005, 226, -0.5,+225.5,3,-1.5,1.5);

  //  hVertexXYZ = new TH3F("hVertexXYZ","RC Vertex - MC vertex; dX[cm]; dY[cm] dZ[cm]", 201, -2*1.005, +2*1.005, 201, -2*1.005, +2*1.005, 201, -2*1.005, +2*1.005 );

}


//________________________________________________________________________________
void FillQAGl(const StMuTrack *gTrack = 0, const StMuMcTrack *mcTrack = 0, const StDcaGeometry *dcaG = 0, const StMuMcVertex *mcVertex = 0) {
  // ==========================================================
  // Apply pT cut

  if ( PTCUT(mcTrack->Pxyz().perp())) return;


  // ==========================================================
  if (! gTrack || ! mcTrack) return;
  if (! dcaG   || ! mcVertex) return;
  TrackMatchType s = kPositive;
  if (int(mcTrack->Charge()) < 0) s = kNegative;
  VarGl_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  TRSymMatrix Cov(5,dcaG->errMatrix());
  double vtx[3] = {mcVertex->XyzV().x(), mcVertex->XyzV().y(), mcVertex->XyzV().z()};
  THelixTrack     thelix =  dcaG->thelix();
  double ermx[3];
  double pars[2];
  thelix.Dca(vtx,pars[0],pars[1],ermx,2);
  var.ChiSqXY = gTrack->chi2xy();
  var.dDcaXY  = pars[0];
  var.dDcaZ   = pars[1];
  double *Dir = thelix.Dir();
  double phi =  TMath::ATan2(Dir[1],Dir[0]);
  var.dPsi    = phi - mcTrack->Pxyz().phi(); 
  var.dPti    = 1./(gTrack->charge()*gTrack->pt()) - 1./(int(mcTrack->Charge())*mcTrack->pT());
  var.dPtiR   = (int(mcTrack->Charge())*mcTrack->pT())   /   (gTrack->charge()*gTrack->pt()) - 1;

  /*
  cout << " dPti=" << var.dPti
       << " gQ=" << gTrack->charge()
       << " mQ=" << (int)mcTrack->Charge()
       << " gPT=" << gTrack->pt()
       << " mPT=" << mcTrack->pT()
       << endl;
  */

  double    tanDip =  mcTrack->Pxyz().z()/mcTrack->Pxyz().perp();
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
  double *x = &var.ChiSqXY;
  for (int i = 0; i < kTotalQAGl; i++) {
    histsQA[s][kGl][i]->Fill(gTrack->nHitsFit(), gTrack->nHitsFit()*(100.-gTrack->qaTruth())/100., x[i]);
    histsK[0][s][kGl][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    histsL[0][s][kGl][i]->Fill(mcTrack->Pxyz().phi()*180.0/TMath::Pi(), mcTrack->Pxyz().perp(), x[i]);
    if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) {
      histsK[1][s][kGl][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
      histsL[1][s][kGl][i]->Fill(mcTrack->Pxyz().phi()*180.0/TMath::Pi()           , mcTrack->Pxyz().perp(), x[i]);
    }
  }
  McRcHit->Fill(gTrack->nHitsFit(),mcTrack->No_tpc_hit());

  hXYfirst->Fill( gTrack->firstPoint()[0], gTrack->firstPoint()[1], mcTrack->Charge() );
  hXYlast ->Fill( gTrack->lastPoint()[0],  gTrack->lastPoint()[1],  mcTrack->Charge() );

  hRZfirst->Fill(  gTrack->firstPoint()[2], gTrack->firstPoint().perp(), mcTrack->Charge() );
  hRZlast ->Fill(   gTrack->lastPoint()[2], gTrack->lastPoint().perp(), mcTrack->Charge() );

}
//________________________________________________________________________________
void FillQAPr(const StMuTrack *pTrack = 0, const StMuMcTrack *mcTrack = 0, const StMuPrimaryTrackCovariance *cov = 0) {
  // ==========================================================
  // Apply pT cut
  if ( PTCUT(mcTrack->Pxyz().perp()) ) return;

  // ==========================================================
  if (! pTrack || ! mcTrack) return;
  if (! int(mcTrack->Charge())) return;
  TrackMatchType s = kPositive;
  if (int(mcTrack->Charge()) < 0) s = kNegative;
  VarPr_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  var.ChiSqXY = pTrack->chi2xy();
  var.ChiSqZ  = pTrack->chi2z();
  double Eta = mcTrack->Pxyz().pseudoRapidity();
  var.deta    = pTrack->eta() - Eta;
  var.dPsi    = pTrack->phi() - mcTrack->Pxyz().phi();
  var.dPti    = 1./(pTrack->charge()*pTrack->pt()) - 1./(int(mcTrack->Charge())*mcTrack->pT());
  var.dPtiR   = (int(mcTrack->Charge())*mcTrack->pT())/(pTrack->charge()*pTrack->pt()) - 1;
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
      var.pPtiR   = var.dPtiR/ TMath::Sqrt(Cov(2,2)) / (mcTrack->Charge()*mcTrack->pT());
    }			
  }		     
  double *x = &var.ChiSqXY;
  for (int i = 0; i < kTotalQAPr; i++) {
    histsQA[s][kPr][i]->Fill(pTrack->nHitsFit(), pTrack->nHitsFit()*(100.-pTrack->qaTruth())/100., x[i]);
    histsK[0][s][kPr][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    histsL[0][s][kPr][i]->Fill(mcTrack->Pxyz().phi()*180.0/TMath::Pi()           , mcTrack->Pxyz().perp(), x[i]);
    if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9){
      histsK[1][s][kPr][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
      histsL[1][s][kPr][i]->Fill(mcTrack->Pxyz().phi()*180.0/TMath::Pi()           , mcTrack->Pxyz().perp(), x[i]);
    }
  }
}
//________________________________________________________________________________
//
// Function to accept and classify a good global or primary track
//
#define REJECT( reason ) { /*cout << #reason << endl;*/ return false; }
bool Accept(const StMuTrack *gTrack = 0) {

  if (! gTrack)            REJECT( Null track ); // return kFALSE;
  if (! gTrack->idTruth()) REJECT( idTruth == 0 ); // return kFALSE; //cout << "has idtruth" << endl;
  if (! gTrack->charge())  REJECT( charge == 0 ); //return kFALSE; //cout << "has charge" << endl;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) REJECT( bad fit flag ); // return kFALSE; //cout << "has flag" << endl; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) REJECT( pileup flag ); // return kFALSE;  //cout << "isnot pileup" << endl; // pile up track in TPC

  //  if (  gTrack->nHitsFit(kTpcId) < 10) return kFALSE; // Number of TPC hits

  if ( gTrack -> nHitsFit( kTpcId ) < MinNoTpcHits ) REJECT( More TPC); // return false; //cout << "pass tpc cut" << endl;
  if ( gTrack -> nHitsFit( kPxlId ) < MinNoPxlHits ) REJECT( More PXL); // return false; 
  if ( gTrack -> nHitsFit( kIstId ) < MinNoIstHits ) REJECT( More IST); // return false;
  if ( gTrack -> nHitsFit( kSsdId ) < MinNoSsdHits ) REJECT( More SSD); // return false; //cout << "pass hft cut" << endl;
  // We are missing kFtsId here...
  if ( gTrack -> nHitsFit(        ) < MinNoFtsHits ) REJECT( More cowbell); // return false; //cout << "pass fts cut" << endl;

  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
//
// Function to accept and classify a ghost track
//
bool AcceptGhost(const StMuTrack *gTrack = 0) {
  if (! gTrack)            return kFALSE;
  // if (  gTrack->idTruth()) return kFALSE;
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC

  // if (  gTrack->nHitsFit(kTpcId) < 10) return kFALSE; // Number of TPC hits

  if ( gTrack -> nHitsFit( kTpcId ) < MinNoTpcHits ) return false;
  if ( gTrack -> nHitsFit( kPxlId ) < MinNoPxlHits ) return false;
  if ( gTrack -> nHitsFit( kIstId ) < MinNoIstHits ) return false;
  if ( gTrack -> nHitsFit( kSsdId ) < MinNoSsdHits ) return false;
  if ( gTrack -> nHitsFit(        ) < MinNoFtsHits ) return false;

  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
//
// Function to accept and classify a good vertex
//
bool AcceptVX(const StMuPrimaryVertex *Vtx = 0) {
  if (! Vtx) return kFALSE;
  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
TrackMcType TrackType(const StMuMcTrack *mcTrack, multimap<int,int> &Mc2RcTracks) {
  int Id = mcTrack->Id()-1;
  pair<multimap<int,int>::iterator,multimap<int,int>::iterator> ret = Mc2RcTracks.equal_range(Id);
  int count = 0;
  for (multimap<int,int>::iterator it = ret.first; 
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
TrackMcType TrackType(int IdMcTrack, int IdGlTrack, multimap<int,int> &Mc2RcTracks) {
  multimap<int,int>::iterator it;
  pair<multimap<int,int>::iterator,multimap<int,int>::iterator> ret;
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
    //    TVirtualX::Instance()->WritePixmap(c->GetCanvasID(),-1,-1,(Char_t *)pngName.Data());
    c->Print( pngName );
    nPng++;
    cout << "Draw #\t" << nPng << "\t" << pngName << endl;
  }
}

//________________________________________________________________________________
void MinMax(TH1 *h, double &min, double &max, double amax = 1000) {
  if (! h) return;
  int n = h->GetNbinsX();
  for (int i = 1; i <= n; i++) {
    double y = h->GetBinContent(i);
    double dy = h->GetBinError(i);
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
void DrawH3s(TH3F *h3s[2], int animate = 0, double min = 1e9, double max = -1e9) {
  const int fitslicescut = 5;
  if (! h3s[0] || ! h3s[1]) return;
  TH2 *h2[2] = {0,0};
  TH1 *h1[2] = {0,0};
  TH1 *s1[2] = {0,0};
  TH1 *n1[2] = {0,0}; // nfit pts nbad fit pts
  TH1 *b1[2] = {0,0};
  TH2 *nb[2] = {0,0}; // nfit pts vs bad fit pts

  for (int p = 0; p < 2; p++) {// zx and zy
    for (int s = kPositive; s < kTotalSigns; s++) {
      TH3 *h3 = h3s[s];
      if (! h3) continue;
      h2[s] = (TH2 *) h3->Project3D(proj[p]);
      TString Name(h2[s]->GetName());
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

	if(!n1[s]) n1[s] = (TH1 *) h3->Project3D("x");
	if(!b1[s]) b1[s] = (TH1 *) h3->Project3D("y");
	if(!nb[s]) nb[s] = (TH2 *) h3->Project3D("yx");

	h1[s] = (TH1 *) h2[s]->ProfileX();
	h1[s]->SetStats(0);
	h1[s]->SetMarkerColor(h2[s]->GetMarkerColor());
	h1[s]->GetXaxis()->SetTitle(h2[s]->GetXaxis()->GetTitle());
	h1[s]->GetYaxis()->SetTitle(h2[s]->GetYaxis()->GetTitle());
#ifdef __MinMax__
	MinMax(h1[s],min,max,500);
#endif /* __MinMax__ */
      } else {



	  h2[s]->FitSlicesY(0,0,-1,fitslicescut,"qeg3s");




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
      double yy = 0.3;
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
      for (int s = kPositive; s < kTotalSigns; s++) {
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

  if ( n1[0]&&n1[1] )
  { 
    TString name=n1[0]->GetName();
    name.ReplaceAll("Pos","");
    name.ReplaceAll("Neg","");
    TCanvas *canvas = new TCanvas(name,name,400,400);
    canvas -> SetLogy(1);
    n1[0]->SetTitle(""); n1[1]->SetTitle("");
    n1[0]->SetLineWidth(2);
    n1[1]->SetLineWidth(2);
    n1[1]->SetLineColor(2);
    n1[0]->Draw();
    n1[1]->Draw("same");
    canvas->Update();
    DrawPng(canvas);
    //  delete canvas;
  }

  if ( b1[0]&&b1[1] )
  { 
    TString name=b1[0]->GetName();
    name.ReplaceAll("Pos","");
    name.ReplaceAll("Neg","");
    TCanvas *canvas = new TCanvas(name,name,400,400);
    canvas -> SetLogy(1);
    b1[0]->SetTitle(""); b1[1]->SetTitle("");
    b1[0]->SetLineWidth(2);
    b1[1]->SetLineWidth(2);
    b1[1]->SetLineColor(2);
    b1[0]->Draw();
    b1[1]->Draw("same");
    canvas->Update();
    DrawPng(canvas);
    //  delete canvas;
  }

  for ( int ii=0;ii<1;ii++ )
    if ( nb[1] ) 
      {
	TString name=nb[ii]->GetName();
	TCanvas *canvas = new TCanvas(name,name,400,400);
	canvas -> SetLogz(1);      
	nb[ii]->Draw("colz");
	TH1 *nvb = (TH1*)nb[ii]->ProfileX("S");
	nvb->SetMarkerColor(1);
	nvb->Draw("same");
	canvas -> Update();
	DrawPng(canvas);
      }
  
}
//________________________________________________________________________________
void Check() {
  if (! gDirectory ) {cout << "There is no input file. Exit" << endl; return;}
  if (! hists[0][0]) Init(0);
  if (! hists[0][0]) {cout << "There are no input histograms. Exit" << endl; return;}
}
//________________________________________________________________________________
void DrawQA(int kk = -1, int ii = -1, int ll = -2) {// versus Nhits
  Check();
  int animate = 0;
#if 1
  TCanvas *c1 = (TCanvas *) ((TList*) gROOT->GetListOfCanvases())->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  c1->cd();
#endif
  int k1 = kGl, k2 = kTotalT;
  if (kk >= 0) {k1 = kk; k2 = kk + 1;}
  for (int k = k1; k < k2; k++) {
    int N = kTotalQAGl;
    if (k == kPr) N = kTotalQAPr;
    cout << "k = " << k << "\t" << TitleTrType[k] << endl;
    int i1 = 0, i2 = N;
    if (ii >= 0) {i1 = ii; i2 = ii + 1;}
    for (int i = i1; i < i2; i++) {
      cout << "i = " << i << "\t";
      if (k == kPr) cout << plotPrVar[i].Name;
      else          cout << plotGlVar[i].Name;
      cout << endl;
      TH3F *h3s[2];
      int l1 = -1, l2 = 2;
      if (ll >= -1 && ll < l2) { l1 = ll; l2 = ll + 1;}
      for (int l = l1; l < l2; l++) {
	if (l < 0) {h3s[0] = histsQA[kPositive][k][i]; h3s[1] = histsQA[kNegative][k][i];}
	else       {h3s[0] = histsK[l][kPositive][k][i]; h3s[1] = histsK[l][kNegative][k][i];}
	cout << "l = " << l << "\t";
	if (! h3s[0] || ! h3s[1]) {cout << "No. Plots" << endl; continue;}
	cout << h3s[0]->GetName() << "\t" << h3s[1]->GetName() << endl;
	double min =  1e9;
	double max = -1e9;
	if (k == kPr) {min = plotPrVar[i].min;  max = plotPrVar[i].max;}
	else          {min = plotGlVar[i].min;  max = plotGlVar[i].max;}
	DrawH3s(h3s, animate, min, max);
      }
    }
    // repeat for phi plots
    for (int i = i1; i < i2; i++) {
      cout << "i = " << i << "\t";
      if (k == kPr) cout << plotPrVar[i].Name;
      else          cout << plotGlVar[i].Name;
      cout << endl;
      TH3F *h3s[2];
      int l1 = -1, l2 = 2;
      if (ll >= -1 && ll < l2) { l1 = ll; l2 = ll + 1;}
      for (int l = l1; l < l2; l++) {
	if (l < 0) {h3s[0] = histsQA[kPositive][k][i]; h3s[1] = histsQA[kNegative][k][i];}
	else       {h3s[0] = histsL[l][kPositive][k][i]; h3s[1] = histsL[l][kNegative][k][i];}
	cout << "l = " << l << "\t";
	if (! h3s[0] || ! h3s[1]) {cout << "No. Plots" << endl; continue;}
	cout << h3s[0]->GetName() << "\t" << h3s[1]->GetName() << endl;
	double min =  1e9;
	double max = -1e9;
	if (k == kPr) {min = plotPrVar[i].min;  max = plotPrVar[i].max;}
	else          {min = plotGlVar[i].min;  max = plotGlVar[i].max;}
	DrawH3s(h3s, animate, min, max);
      }
    }




  }
}
//________________________________________________________________________________
void DrawEff(double ymax=1.0, double pTmin = -1, int animate=0) {// Efficiencies
  Check();
  struct Eff_t {
    const Char_t *Name;
    const Char_t *Title;
    TrackMatchType kDividend;
    TrackMatchType kDivider;
    double min, max;
  };
  enum Effiencies {kEffTotal = 9};
  static int Break = 0;
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
  const double pTmins[4] = {0.11, 0.5, 1.01, 2.0};
  //                       Phi Eta pT
  const Char_t *proj3[3] = {"x","y","z"};
  for (int k = kGl; k < kTotalT; k++) {
    for (int i = 0; i < kEffTotal; i++) {
      int i1 = eff[i].kDividend;
      int i2 = eff[i].kDivider;
      for (int p = 0; p < 3; p++) { // projections
	TString Name(eff[i].Name);
	TString Title(eff[i].Title);
	TH1 *heff[8]; memset(heff, 0, sizeof(heff));
	double min = eff[i].min;
	double max = eff[i].max;
	int NS = kTotalSigns;
	if (pTmin < 0 && p != 2) NS *= 4;
	for (int l = kPositive; l < NS; l++) {
	  int s = l%kTotalSigns;
	  TH3F *Dividend = hists[s][k][i1];
	  TH3F *Divider  = hists[s][k][i2];
	  int nbinsX = Dividend->GetNbinsX();
	  int nbinsY = Dividend->GetNbinsY();
	  int nbinsZ = Dividend->GetNbinsZ();
	  int binX1 = 1, binX2 = nbinsX;
	  int binY1 = 1, binY2 = nbinsY;
	  int binZ1 = 1, binZ2 = nbinsZ;
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
	  double Val = 0;
	  double Sum = 0;
	  for (int bin = binX1; bin <= binX2; bin++) {
	    double val = heff[l]->GetBinContent(bin); Val += val;
	    double sum = temp->GetBinContent(bin);    Sum += sum;
	    double err = 0;
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
	  for (int s = kPositive; s < NS; s++) {
	    if (s == kPositive) {heff[s]->SetMinimum(min); heff[s]->SetMaximum(max); heff[s]->Draw();}
	    else                 heff[s]->Draw("same");
	    if (l) l->AddEntry(heff[s],Form("%s with pT > %3.1f",TitleCharge[s%2],pTmins[s/2]));
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
  DrawEff(1.0,0.11);
  //  DrawdEdx();
}


//
// Check that 
//
int CheckMcHits( StMuMcTrack *track )
{
  int nhits = 0;
  if ( (nhits+=track -> No_tpc_hit()) < MinNoMcTpcHits ) return 0;
  if ( (nhits+=track -> No_pix_hit()) < MinNoMcPxlHits ) return 0;
  if ( (nhits+=track -> No_ist_hit()) < MinNoMcIstHits ) return 0;
  if ( (nhits+=track -> No_ssd_hit()) < MinNoMcSstHits ) return 0;
  if ( (nhits+=track -> No_fts_hit()) < MinNoMcFtsHits ) return 0;
  return nhits;
}


//________________________________________________________________________________
void MuMc(Long64_t nevent = 999999,
	  const char* file="simple/*.MuDst.root",
	  int nFiles=999999999,
	  const char* filter="st:MuDst.root",
	  const  char* outFile="./MuMc.root") {
  //  int counter=0;
  TString CDir(gSystem->BaseName(gSystem->pwd()));
  //  if (CDir.Contains("devT") && CDir != "devTR") MinNoMcTpcHits = 11;
  cout << "Run with " << CDir.Data() << endl;
  cout << "  MinNoMcTpcHits = " << MinNoMcTpcHits << " which corrresponds to 10 real hit in TPC" << endl;
  cout << "  MinNoMcSstHits = " << MinNoMcSstHits <<  endl;
  cout << "  MinNoMcIstHits = " << MinNoMcIstHits <<  endl;
  cout << "  MinNoMcPxlHits = " << MinNoMcPxlHits <<  endl;
  cout << "  MinNoMcPxlHits = " << MinNoMcFtsHits <<  endl;
  cout << "----------------------------------------------" << endl;
  cout << "  MinNoMcHits = " << MinNoMcHits <<  endl;


  StMuTimer timer;
  Init(outFile);
  timer.start();
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,filter,nFiles);   // set up maker in read mode
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
    "GlobalTracks",
    "CovPrimTrack",
    "CovGlobTrack",
    "StStMuMcVertex",
    "StStMuMcTrack"
  };
  int Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (int i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  timer.reset();
  timer.start();
  TChain *tree = maker->chain();  assert(tree);
  Long64_t nentries = tree->GetEntries();
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  if (nentries < 10) { cout <<"aborting on too few events" << endl; return;}
#if 0
  tree->SetParallelUnzip();      //  parallel unzipping
#endif
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,0)
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);
#endif


  // ..............................................................................................
  //
  //
  //                                                                              BEGIN EVENT LOOP
  //
  //
  // ..............................................................................................


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
    // cout << " #" << ev;
    //    int referenceMultiplicity = muEvent->refMult(); // get the reference multiplicity
    // cout << " refMult= "<< referenceMultiplicity;
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    int NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  // cout << "\tPrimaryVertices " << NoPrimaryVertices;
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    int NoPrimaryTracks = PrimaryTracks->GetEntriesFast();  // cout << "\tPrimaryTracks " << NoPrimaryTracks;
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    int NoGlobalTracks = GlobalTracks->GetEntriesFast();  // cout << "\tGlobalTracks " << NoGlobalTracks;
    TClonesArray *CovPrimTrack     = mu->covPrimTrack(); // cout << "\tCovPrimTrack " << CovPrimTrack->GetEntriesFast();
    TClonesArray *CovGlobTrack     = mu->covGlobTrack(); // cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();
    TClonesArray *MuMcVertices   = mu->mcArray(0); 
    int NoMuMcVertices = MuMcVertices->GetEntriesFast(); // cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices;
    TClonesArray *MuMcTracks     = mu->mcArray(1); 
    int NoMuMcTracks = MuMcTracks->GetEntriesFast(); // cout << "\t" << StMuArrays::mcArrayTypes[1] << " " << NoMuMcTracks;
    // cout << endl;
    const double field = muEvent->magneticField()*kilogauss;




    if (! NoMuMcVertices || ! NoMuMcTracks) {
      cout << "Ev. " << ev << " has no MC information ==> skip it" << endl;
      continue;
    }




    // =============  Build map between global and primary tracks from proper vertex
    map<int,int> Gl2Pr;
    for (int k = 0; k < NoPrimaryTracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);                     if (! Accept(pTrack)) continue;
      int l = pTrack->vertexIndex();                                                       if (l < 0) continue;
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);      if (Vtx->idTruth() != 1) continue;
      int kg = pTrack->index2Global();                                                     Gl2Pr.insert(pair<int,int>(kg,k));

      cout << "k primary = " << k << " k global = " << kg << endl;

    }

    //
    // Is there a logic error here?  Case where we have only one track per
    // event?
    //




    // =============  Build map between global and Mc tracks
    multimap<int,int> Mc2RcTracks;
    for (int kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);      
      if (! Accept(gTrack)) continue;
      if (gTrack->idTruth() < 0 || gTrack->idTruth() > NoMuMcTracks) {
	if ( warn_illegal_idtruth ) cout << "Illegal idTruth " << gTrack->idTruth() << " The track is ignored" << endl;
	continue;
      }

      StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(gTrack->idTruth()-1);
      //      mcTrack->Print();
      if (mcTrack->Id() != gTrack->idTruth()) {
	if ( warn_mismatch_idtruth ) cout << "Mismatched idTruth " << gTrack->idTruth() << " and mcTrack Id " <<  mcTrack->Id() 
	     << " The track is ignored" <<  endl;
      }

      Mc2RcTracks.insert(pair<int,int>(gTrack->idTruth()-1,kg)); // Id shifted by 1
    }




    // =============  Build map between  Rc and Mc vertices
    multimap<int,int> Mc2RcVertices;
    for (int l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (! AcceptVX(Vtx)) continue;
      //      double 
      //	dX = Vtx->position().x(),
      //	dY = Vtx->position().y(),
      //	dZ = Vtx->position().z();
      //      Vtx->Print();
      // Check Mc
      if (Vtx->idTruth() < 0 || Vtx->idTruth() > NoMuMcVertices) {
	if ( warn_illegal_idtruth ) cout << "Illegal idTruth " << Vtx->idTruth() << " The track is ignored" << endl;
	continue;
      }


      StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(Vtx->idTruth()-1 );
      
      if (mcVertex->Id() != Vtx->idTruth()) {
	if ( warn_mismatch_idtruth )	cout << "Mismatched idTruth " << Vtx->idTruth() << " and mcVertex Id " <<  mcVertex->Id()  << " The vertex is ignored" <<  endl;
      }
      //      mcVertex->Print();
      //      dX -= mcVertex->XyzV().x();
      //      dY -= mcVertex->XyzV().y();
      //      dZ -= mcVertex->XyzV().z();
      Mc2RcVertices.insert(pair<int,int>(Vtx->idTruth()-1,l)); // Id shifted by 1
      //      hVertexXYZ->Fill( dX, dY, dZ );
    }


    //
    // Loop over Mc Tracks
    //
    for (int m = 0; m < NoMuMcTracks; m++) {

      StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(m);
      if (! mcTrack) continue;

      int IdVx       = mcTrack->IdVx();
      bool primaryVx = ( IdVx == 1 );


      // Get the corresponding MC vertex for this track
      StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt( IdVx - 1 );
      if ( VZCUT( mcVertex ) )
	{
	  continue; // skip based on vertex cut
	}

      if ( ETACUT(mcTrack) )
	{
	  //	  mcTrack->Print();
	  continue; // skip based on pseudorapidy cut
	}


      GiD[0]->Fill(mcTrack->GePid());

      if ( CheckMcHits( mcTrack ) ) GiD[1]->Fill(mcTrack->GePid());     //if (mcTrack->No_tpc_hit() >= MinNoMcTpcHits) GiD[1]->Fill(mcTrack->GePid());
      if (primaryVx) {
	GiD[2]->Fill(mcTrack->GePid());
	if ( CheckMcHits( mcTrack ) ) GiD[3]->Fill(mcTrack->GePid());	//if (mcTrack->No_tpc_hit() >= MinNoMcTpcHits) GiD[3]->Fill(mcTrack->GePid());
      }
      if (! mcTrack->Charge()) continue;


      //
      // Fill Efficiency plots
      //

      // Determine sign of track
      TrackMatchType s = kPositive;      if (mcTrack->Charge() < 0) s = kNegative;

                     hists[s][kGl][kMc]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp()); // global 
      if (primaryVx) hists[s][kPr][kMc]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp()); // primary

      // Too few MC hits in TPC.  Consisdered out of acceptance
      if ( !CheckMcHits( mcTrack) ) continue;      //if (mcTrack->No_tpc_hit() < MinNoMcTpcHits) continue;  

                     hists[s][kGl][kMcHit]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp()); // global
      if (primaryVx) hists[s][kPr][kMcHit]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp()); // primary



      // Determine classification of the MC<-->RC track match
      TrackMcType kType = TrackType(mcTrack,Mc2RcTracks);
      int l = -1;
      switch (kType) 	{
      case kMatched: l = kMcRec  ; break;
      case kLost:    l = kMcLost ; break;
      case kClone:   l = kMcClone; break;
      default: l = 1; break;
      }

      // Efficiency histograms are filled at this point *regardless* of reco track hits, etc...

                     hists[s][kGl][l]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp());
      if (primaryVx) hists[s][kPr][l]->Fill(TMath::RadToDeg()*mcTrack->Pxyz().phi(), mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp());


      // mcTrack->Print();
      // cout << "MAtch type = " << l << " kMcRec= "<< kMcRec << endl;

      if (l == kMcRec) { // MC track with reco partner
	int Id = mcTrack->Id()-1;
	pair<multimap<int,int>::iterator,multimap<int,int>::iterator> ret;
	ret = Mc2RcTracks.equal_range(Id);
	multimap<int,int>::iterator it;
	int kg = -1;
	int count = 0;
	for (it=Mc2RcTracks.equal_range(Id).first; it!=Mc2RcTracks.equal_range(Id).second; ++it, ++count) {
	  kg = (*it).second;
	}
	assert(count == 1);
	// Track QA
	StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
	// << --- Insert conditions on Reco Tracks here ???
	int kgc = gTrack->index2Cov();
	if (kgc < 0) continue;
	StDcaGeometry *dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
	StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(IdVx-1);
	cout << "Fill QA Gl" << endl;
	FillQAGl(gTrack, mcTrack, dcaG, mcVertex);
	int k = Gl2Pr[kg];
	//	cout << "k=" << k << endl;// global to primary not mapped?
	//	if (! k) continue;
	StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
	if (0==pTrack) continue;

	int kpc = pTrack->index2Cov();
	if (kpc < 0) continue;
	StMuPrimaryTrackCovariance *cov = (StMuPrimaryTrackCovariance *) CovPrimTrack->UncheckedAt(kpc);
	cout << "Fill QA Pr" << endl;
	FillQAPr(pTrack, mcTrack, cov);
	// dE/dx block
	const StMuProbPidTraits &PiD = pTrack->probPidTraits();
	double I[2] = {PiD.dEdxTruncated(), PiD.dEdxFit()};
	double TrackLength = PiD.dEdxTrackLength();
	int Gid = mcTrack->GePid();
	static Bichsel *m_Bichsel = Bichsel::Instance();
	double pMomentum = pTrack->helix().momentum(field).mag();
	//	const StThreeVectorF &pVx  = pTrack->momentum();
	for (int h = 0; h < NHYPS; h++) {
	  if (GEANTiD[h] == Gid) {
	    double bghyp = TMath::Log10(pMomentum/Masses[h]);
	    double Pred[2]  = {1.e-6*m_Bichsel->GetI70(bghyp,1.0),
				 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(bghyp,1.0))};
	    for (int mm = 0; mm < 2; mm++) {
	      if (I[mm] <= 0 || Pred[mm] <= 0) continue;
	      double z = TMath::Log(I[mm]/Pred[mm]);
	      PdEdx[mm][h]->Fill(pTrack->phi(), pTrack->eta(), pTrack->pt(), z);
	      LdEdx[mm][h]->Fill(TrackLength, bghyp, z);
	    }
	    break;
	  }
	}
      } // reco partner
    }
    // check for ghosts
    for (int kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if ( ! AcceptGhost(gTrack)) continue;
      if ( gTrack->idTruth()) continue;
      TrackMatchType s = kPositive;
      if (gTrack->charge() < 0) s = kNegative;
      hists[s][kGl][kRcGhost]->Fill(TMath::RadToDeg()*gTrack->phi(),gTrack->eta(),(gTrack->charge()*gTrack->pt()));
    }
    for (int l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (Vtx->idTruth() != 1) continue;
      for (int k = 0; k < NoPrimaryTracks; k++) {
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
    // Loop over Mc Vertices
    for (int n = 0; n < NoMuMcVertices; n++) {
      StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(n);
      int IdVx = mcVertex->Id();
      // Loop over all Mc tracks from the primary vertex and try to find match with reconstructed primary tracks.
      for (int m = 0; m < NoMuMcTracks; m++) {
	StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(m);
	if (! mcTrack->Charge()) continue;     // charged particle
	if (mcTrack->IdVx() !=  IdVx) continue; //
	TrackType(mcTrack,Mc2RcTracks);
      }
    }
#endif
  }
  if (fOut) fOut->Write();
  Draw();
}
