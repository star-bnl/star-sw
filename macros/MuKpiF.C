//#define GlobalTracks_type
#define __TCFIT__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TBenchmark.h"
#if 0
#include "StBichsel/Bichsel.h"
#include "BetheBloch.h"
#endif
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TLorentzVector.h"
#include "TVector3.h"
//#include "StThreeVectorF.hh"
#include "THelixTrack.h"
#include "TRVector.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "StDcaGeometry.h"
#ifdef __TCFIT__
#include "StarRoot/TCFit.h"
#include "StarRoot/THelixTrack.h"
#endif
#include "KFParticle.h"
#include "MVertex.h"
#include "MTrack.h"
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
class Bichsel;
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
static const Double_t __PROB_SCALE__  = 1000.;
static const Double_t __SIGMA_SCALE__ = 1000.;
static const Double_t ame  = 0.51099907e-3;
static const Double_t amPi = 0.13956995;
static const Double_t amP  = 0.93827231;
static const Double_t amK  = 0.4936770;
static const Double_t pTCut = 0.1; // 0.1;
static const Double_t mKpiMin = 1.4;
static const Double_t mKpiMax = 2.4;
static const Double_t DcaCut  = 0.0600;
static       Int_t    _debug = 0;
#if 0
class Bichsel;
Bichsel *m_Bichsel = 0;
#endif
#ifndef __CuCu__
const UInt_t NTriggerIDs = 42;
#else
const UInt_t NTriggerIDs = 32;
#endif
#define PrP(A)   if (_debug) {cout << "\t" << (#A) << " = \t" << ( A ) << endl;}
//________________________________________________________________________________
Bool_t isTrigger(UInt_t id, const UInt_t *ids) {
  for (UInt_t i = 0; i < NTriggerIDs; i++) {
    if (ids[i] && id == ids[i]) return kTRUE;
  }
  return kFALSE;
}
//________________________________________________________________________________
void MuKpiF(const Char_t *files="*.MuDst.root", 
	    const Char_t *Out="MuKpi.root"
#ifdef __TCFIT__
	    , Double_t dLCut = 3
#endif
	    ) {
  TTreeIter iter("MuDst");
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TDirIter Dir(files);
  Char_t *file = 0;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++; file1 = file;}
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    TString dir = gSystem->BaseName(file1); 
    dir.ReplaceAll(".MuDst","");
    output += dir;
  }
#ifdef __TCFIT__
  output.ReplaceAll(".root","");
  output += Form("Cut%i.root",(Int_t) dLCut);
#endif
  cout << "Output for " << output << endl;
  const Int_t*&      RunId                  = iter("MuEvent.mRunInfo.mRunId");
  const Float_t*&    MuEvent_mRunInfo_mCenterOfMassEnergy     = iter("MuEvent.mRunInfo.mCenterOfMassEnergy");
#ifdef __TCFIT__
  const Double_t*&mMagneticFieldZ            = iter("MuEvent.mRunInfo.mMagneticFieldZ");
#endif
  const UInt_t*& mNTriggerId_mId             = iter(Form("MuEvent.mTriggerIdCollection.mNTriggerId.mId[%i]",NTriggerIDs));
  const Int_t    &NoPrimVertex               = iter("PrimaryVertices");
  const Float_t *&PrimVertexX                = iter("PrimaryVertices.mPosition.mX1");
  const Float_t *&PrimVertexY                = iter("PrimaryVertices.mPosition.mX2");
  const Float_t *&PrimVertexZ                = iter("PrimaryVertices.mPosition.mX3");
  const Float_t *&PrimVerSigX                = iter("PrimaryVertices.mPosError.mX1");
  const Float_t *&PrimVerSigY                = iter("PrimaryVertices.mPosError.mX2");
  const Float_t *&PrimVerSigZ                = iter("PrimaryVertices.mPosError.mX3");
  
  const Int_t    &NoTracks                   = iter("PrimaryTracks");
  const Int_t   *&PrimaryTracks_mVertexIndex = iter("PrimaryTracks.mVertexIndex");
  //   const UChar_t *&mNHitsFitInner             = iter("PrimaryTracks.mNHitsFitInner");
  //   const UChar_t *&mNHitsPossInner            = iter("PrimaryTracks.mNHitsPossInner");
  //   const UChar_t *&mNHitsFit                  = iter("PrimaryTracks.mNHitsFit");
  
  //   const Float_t *&pT                         = iter("PrimaryTracks.mPt");
  const Float_t *&pX                         = iter("PrimaryTracks.mP.mX1");
  const Float_t *&pY                         = iter("PrimaryTracks.mP.mX2");
  const Float_t *&pZ                         = iter("PrimaryTracks.mP.mX3");
  //   const Float_t *&Eta                        = iter("PrimaryTracks.mEta");
  //   const Float_t *&Phi                        = iter("PrimaryTracks.mPhi");
  const Int_t   *&mIndex2Global              = iter("PrimaryTracks.mIndex2Global");
  //   const Float_t*&    mDCA_mX1                   = iter("PrimaryTracks.mDCA.mX1");
  //   const Float_t*&    mDCA_mX2                   = iter("PrimaryTracks.mDCA.mX2");
  //   const Float_t*&    mDCA_mX3                   = iter("PrimaryTracks.mDCA.mX3");
  //   const Float_t*&    mDCAGlobal_mX1             = iter("PrimaryTracks.mDCAGlobal.mX1");
  //   const Float_t*&    mDCAGlobal_mX2             = iter("PrimaryTracks.mDCAGlobal.mX2");
  //   const Float_t*&    mDCAGlobal_mX3             = iter("PrimaryTracks.mDCAGlobal.mX3");
  
  // Global
  const Int_t    &NoTracksGl                   = iter("GlobalTracks");
  const UChar_t *&mNHitsFitInnerGl             = iter("GlobalTracks.mNHitsFitInner");
  const UChar_t *&mNHitsPossInnerGl            = iter("GlobalTracks.mNHitsPossInner");
  const UChar_t *&mNHitsFitGl                  = iter("GlobalTracks.mNHitsFit");
  const Float_t *&pTGl                         = iter("GlobalTracks.mPt");
  const Float_t *&pXGl                         = iter("GlobalTracks.mP.mX1");
  const Float_t *&pYGl                         = iter("GlobalTracks.mP.mX2");
  const Float_t *&pZGl                         = iter("GlobalTracks.mP.mX3");
  const Float_t *&EtaGl                        = iter("GlobalTracks.mEta");
  //   const Float_t *&PhiGl                        = iter("GlobalTracks.mPhi");
  const Int_t*&      GlobalTracks_mIndex2Cov                  = iter("GlobalTracks.mIndex2Cov");
  const Float_t*&    CovGlobTrack_mImp                        = iter("CovGlobTrack.mImp");
  const Float_t*&    CovGlobTrack_mZ                          = iter("CovGlobTrack.mZ");
  const Float_t*&    CovGlobTrack_mPsi                        = iter("CovGlobTrack.mPsi");
  const Float_t*&    CovGlobTrack_mPti                        = iter("CovGlobTrack.mPti");
  const Float_t*&    CovGlobTrack_mTan                        = iter("CovGlobTrack.mTan");
  const Float_t*&    CovGlobTrack_mCurv                       = iter("CovGlobTrack.mCurv");
  const Float_t*&    CovGlobTrack_mImpImp                     = iter("CovGlobTrack.mImpImp");
  const Float_t*&    CovGlobTrack_mZImp                       = iter("CovGlobTrack.mZImp");
  const Float_t*&    CovGlobTrack_mZZ                         = iter("CovGlobTrack.mZZ");
  const Float_t*&    CovGlobTrack_mPsiImp                     = iter("CovGlobTrack.mPsiImp");
  const Float_t*&    CovGlobTrack_mPsiZ                       = iter("CovGlobTrack.mPsiZ");
  const Float_t*&    CovGlobTrack_mPsiPsi                     = iter("CovGlobTrack.mPsiPsi");
  const Float_t*&    CovGlobTrack_mPtiImp                     = iter("CovGlobTrack.mPtiImp");
  const Float_t*&    CovGlobTrack_mPtiZ                       = iter("CovGlobTrack.mPtiZ");
  const Float_t*&    CovGlobTrack_mPtiPsi                     = iter("CovGlobTrack.mPtiPsi");
  const Float_t*&    CovGlobTrack_mPtiPti                     = iter("CovGlobTrack.mPtiPti");
  const Float_t*&    CovGlobTrack_mTanImp                     = iter("CovGlobTrack.mTanImp");
  const Float_t*&    CovGlobTrack_mTanZ                       = iter("CovGlobTrack.mTanZ");
  const Float_t*&    CovGlobTrack_mTanPsi                     = iter("CovGlobTrack.mTanPsi");
  const Float_t*&    CovGlobTrack_mTanPti                     = iter("CovGlobTrack.mTanPti");
  const Float_t*&    CovGlobTrack_mTanTan                     = iter("CovGlobTrack.mTanTan");
  const Float_t*&    mDCAGlobal_mX1Gl              = iter("GlobalTracks.mDCAGlobal.mX1");
  const Float_t*&    mDCAGlobal_mX2Gl              = iter("GlobalTracks.mDCAGlobal.mX2");
  const Float_t*&    mDCAGlobal_mX3Gl              = iter("GlobalTracks.mDCAGlobal.mX3");
  const Short_t*&    QGl                           = iter("GlobalTracks.mHelix.mQ");
  const Int_t   *&NSigmaElectronGl             = iter("GlobalTracks.mNSigmaElectron");
  const Int_t   *&NSigmaPionGl                 = iter("GlobalTracks.mNSigmaPion");
  const Int_t   *&NSigmaKaonGl                 = iter("GlobalTracks.mNSigmaKaon");
  const Int_t   *&NSigmaProtonGl               = iter("GlobalTracks.mNSigmaProton");
  const Float_t*&    dEdxTrackLength           = iter("GlobalTracks.mProbPidTraits.mdEdxTrackLength");
  const Float_t*&    GdEdx                     = iter("GlobalTracks.mdEdx");
  
  TFile *fOut = new TFile(output,"recreate");
  TString Name;
  TString Title;
  // two particle plots
  struct Plot_t {
    const Char_t *Name;
    const Char_t *Title;
  };
  //  enum Size {NT = 7, Nsys = 1, NSigns = 4, NZ = 4, NL = 10, NGJ = 3, Ncut = 6, NpT = 5, Neta = 3, NF = 0, NEfM = 2 + NF};
  enum Size {NT = 6, Nsys = 1, NSigns = 4, NZ = 1, NL = 1, NGJ = 1, Ncut = 6, NpT = 1, Neta = 1, NF = 0, NEfM = 2 + NF};
  //  const Int_t NT= 6;
  const Plot_t SysTypes[6] = { //t
    {"DL","|decay length|"},                              // 0
    {"SL","|decay length|/#sigma_{dL}"},                  // 1
    {"EM","Effective mass"},                              // 2
    {"DLKF","|decay length| from KFParticle"},            // 3
    {"SLKF","|decay length|/#sigma_{dL} from KFParticle"},// 4
    {"EMKF","Effective mass from KFParticle"},            // 5
  };
  //  const Int_t Nsys = 1;
  const Plot_t SysNames[Nsys] = { //s 
    {"Kpi","Kpi"}
  };
  //  const Int_t NSigns = 4;
  const Plot_t ChargeNames[NSigns] = { //c
    {"PP","(++)"},
    {"NP","(-+)"},
    {"PN","(+-)"},
    {"NN","(--)"},
  };
  TH2D *pTKpi[NSigns];
  TH2D *pKpi[NSigns];
  TH2D *pTKpiC[NSigns];
  TH2D *pKpiC[NSigns];
  TH2D *pIdKpi[NSigns], *pIdKpiC[NSigns];
  TH2D *DCAxyKpi[NSigns], *DCAzKpi[NSigns];
  TH2D *dEdxKpi[NSigns];
  TH1D *CosTGJKpi[NSigns];
  
  TH2D *TdEdx  = new TH2D("TdEdx","Log10(dEdx(keV/cm)) versus Log10(p(GeV/c))",150,-1.,2., 500,0.,2.5);
  TH2D *TdEdxKaonC = new TH2D("TdEdxKaonC","Log10(dEdx(keV/cm)) versus Log10(p(GeV/c)) for selected Kaon",150,-1.,2., 500,0.,2.5);
  TH2D *TdEdxPionC = new TH2D("TdEdxPionC","Log10(dEdx(keV/cm)) versus Log10(p(GeV/c)) for selected Pion",150,-1.,2., 500,0.,2.5);
  for (Int_t i = 0; i < NSigns; i++) {
    pTKpi[i] = new TH2D(Form("pTKpi%s", ChargeNames[i].Name),Form("pT K versus pT pi %s",ChargeNames[i].Title),
			100,0.0,5.0,100,0.0,5.0);
    pTKpi[i]->SetXTitle("pT of pion");
    pTKpi[i]->SetYTitle("pT of Kaon");
    pKpi[i] = new TH2D(Form("pKpi%s", ChargeNames[i].Name),Form("p K versus p pi %s",ChargeNames[i].Title),
		       100,0.0,5.0,100,0.0,5.0);
    pKpi[i]->SetXTitle("p of pion");
    pKpi[i]->SetYTitle("p of Kaon");
    pTKpiC[i] = new TH2D(Form("pTKpiC%s", ChargeNames[i].Name),Form("pT K versus pT pi %s after cut",ChargeNames[i].Title),
			 100,0.0,5.0,100,0.0,5.0);
    pTKpiC[i]->SetXTitle("pT of pion");
    pTKpiC[i]->SetYTitle("pT of Kaon");
    pKpiC[i] = new TH2D(Form("pKpiC%s", ChargeNames[i].Name),Form("p K versus p pi %s after cut",ChargeNames[i].Title),
			100,0.0,5.0,100,0.0,5.0);
    pKpiC[i]->SetXTitle("p of pion");
    pKpiC[i]->SetYTitle("p of Kaon");
    pIdKpi[i] = new TH2D(Form("pIdKpi%s", ChargeNames[i].Name),Form("pId K versus pId pi %s",ChargeNames[i].Title),
			 100,-5.0,5.0,100,-5.0,5.0);
    pIdKpi[i]->SetXTitle("n#sigma of pion");
    pIdKpi[i]->SetYTitle("n#sigma of Kaon");
    pIdKpiC[i] = new TH2D(Form("pIdKpiC%s", ChargeNames[i].Name),Form("pId K versus pId pi %s after cut",ChargeNames[i].Title),
			  100,-5.0,5.0,100,-5.0,5.0);
    pIdKpiC[i]->SetXTitle("n#sigma of pion");
    pIdKpiC[i]->SetYTitle("n#sigma of Kaon");
    
    DCAxyKpi[i] = new TH2D(Form("DCAxyKpi%s", ChargeNames[i].Name),Form("DCAxy K versus DCAxy pi %s",ChargeNames[i].Title),
			   100,-0.2,0.2,100,-0.2,0.2);
    DCAxyKpi[i]->SetXTitle("DCAxy of pion");
    DCAxyKpi[i]->SetYTitle("DCAxy of Kaon");
    
    DCAzKpi[i] = new TH2D(Form("DCAzKpi%s", ChargeNames[i].Name),Form("DCAz K versus DCAz pi %s",ChargeNames[i].Title),
			  100,-0.2,0.2,100,-0.2,0.2);
    DCAzKpi[i]->SetXTitle("DCAz of pion");
    DCAzKpi[i]->SetYTitle("DCAz of Kaon");
    
    dEdxKpi[i] = new TH2D(Form("dEdxKpi%s", ChargeNames[i].Name),Form("dEdx K versus dEdx pi %s",ChargeNames[i].Title),
			  100,0.,2e-5,100,0.,2e-5);
    dEdxKpi[i]->SetXTitle("dEdx of pion");
    dEdxKpi[i]->SetYTitle("dEdx of Kaon");
    CosTGJKpi[i] = new TH1D(Form("CosTGJKpi%s", ChargeNames[i].Name),Form("Cos Theta GJ K for Kpi%s",ChargeNames[i].Title),100,-1.,1.);
  }
  // Separated cuts
  //  const Int_t NZ = 4;
  const Plot_t ZName[4] = {
    {"","no Zpr cut"},
    {"Z30","|Zpr| < 30"},
    {"Z20","|Zpr| < 20"},
    {"Z10","|Zpr| < 10"}
  };
  //  const Int_t NL = 10;
  const Plot_t LName[10] = {// l
    {"","All decay length"},
    {"L",Form("decay length and DCAs < %i mkm",(Int_t) (1e4*DcaCut))},
    {"Lp","decay length > 0"},
    {"Ln","decay length < 0"},
    {"Lp1","decay length >  #sigma"},
    {"Ln1","decay length <  #sigma"},
    {"Lp2","decay length > 2#sigma"},
    {"Ln2","decay length <-2#sigma"},
    {"Lp3","decay length > 3#sigma"},
    {"Ln3","decay length <-3#sigma"}
  };
  //  const Int_t NGJ = 3;
  const Plot_t TName[3] = {//m 
    {"",""},
    {"GJP","|cos(Theta_{GJ})| <= 0.6"},
    {"GJN","|cos(Theta_{GJ})| >  0.6"}
  };
  // commulicative cuts
  //  const Int_t Ncut = 6;
  const Plot_t CutNames[6] = {//cut
    {"","No cuts"},
    {"S","no.of SSD+SVT hit for both tracks > 0"},
    {"dEdx","n_{Sigma} < 2"},
    {"SdEdx","no.of SSD+SVT hit for both tracks > 0 and n_{Sigma} < 2"},
    {"S2","no.of SSD+SVT hit for both tracks > 1"},
    {"S2dEdx","no.of SSD+SVT hit for both tracks > 1 and n_{Sigma} < 2"}
    //     {"Sdca","dca/sigma_dca > 3 for both tracks"},
    //     {"t2","more than 2 tracks with dca s.t.d. > 2"},
    //     {"t3","more than 2 tracks with dca s.t.d. > 3"},
    //     {"t4","more than 2 tracks with dca s.t.d. > 4"},
    //     {"t5","more than 2 tracks with dca s.t.d. > 5"},
    //     {"t6","more than 2 tracks with dca s.t.d. > 6"},
    //     {"t7","more than 2 tracks with dca s.t.d. > 7"}
  };
  //  const Int_t NpT = 5;
  const Double_t pTmin[5] = {0.0, 0.5, 1.0, 2.0, 5.0};
  const Plot_t pTNames[5] = {
    {"","All pT"},
    {"pT05","pT > 0.5"},
    {"pT10","pT > 1.0"},
    {"pT20","pT > 2.0"},
    {"pT50","pT > 5.0"}
  };
  //  const Int_t Neta = 3; 
  const Double_t etamin[3] = {0.0, 0.5, 1.0};
  const Plot_t etaNames[3] = {
    {"","All eta"},
    {"Y05","|eta| < 0.5"},
    {"Y10","|eta| < 1.0"}
  };
  TH2D *priVtxXY = new TH2D("priVtxXY","Primary vertex x y",100,-2.5,2.5,100,-2.5,2.5);
  TH1D *priVtxZ  = new TH1D("priVtxZ","Primary vertex z before Z cut",100,-200,200);
  TH1D *priVtxSigmaZ  = new TH1D("priVtxSigmaZ","Primary vertex Sigma z before cut",100,0,0.2);
  TH1D *priR = new TH1D("priR","Primary vertex R",100,0,2.5);
  TH1D *nPrim  = new TH1D("nPrim","Total No. of primary vertices per event",20,0,20);
  TH1D *nTracks = new TH1D("nTracks","total no. of primary tracks",200,0,2000);
  TH1D *nTracksU = new TH1D("nTracksU","total no. of primary tracks used",200,0,2000);
  TH1D *Prob   = new TH1D("Prob","TCFit probability",100,0,1);
  TH1D *ProbKF = new TH1D("ProbKF","KFParticle probability",100,0,1);
  TH1  *hists[Nsys][NSigns][NT][NZ][NL][NGJ][Ncut][NpT][Neta];
  memset (hists, 0, Nsys*NSigns*NT*NZ*NL*NGJ*Ncut*NpT*Neta*sizeof(TH1 *));
  for (Int_t s = 0; s < Nsys; s++) 
    for (Int_t z = 0; z < NZ; z++) 
      for (Int_t c = 0; c < NSigns; c++) 
	for (Int_t t = 0; t < NT; t++) {
	  Int_t  nx = 200;
	  Double_t xmin = 0, xmax = 0.2;
	  if (t%3 == 1) {
	    xmax = 20;
	  } 
	  if (t%3 == 2) {
	    nx   = 400;
	    xmin = amK + amPi;
	    xmax = xmin + 2;
	  }
	  for (Int_t l = 0; l < NL; l++) {
	    for (Int_t mGJ = 0; mGJ < NGJ; mGJ++) {
	      TString Cut, CutN;
	      for (Int_t cut = 0; cut < Ncut; cut++) {
		for (Int_t kpT = 0; kpT < NpT; kpT++ ) {
		  for (Int_t keta = 0; keta < Neta; keta++) {
		    Name  = SysTypes[t].Name; Title = SysTypes[t].Title;
		    Name  = SysTypes[t].Name; Title = SysTypes[t].Title;
		    Name += SysNames[s].Name; Title += " "; Title += SysNames[s].Title;
		    Name += ChargeNames[c].Name; Title += " "; Title += ChargeNames[c].Title;
		    Name += ZName[z].Name; Title += " "; Title += ZName[z].Title;
		    Name += LName[l].Name; Title += " "; Title += LName[l].Title;
		    Name += TName[mGJ].Name; Title += " "; Title += TName[mGJ].Title;
		    Name += CutNames[cut].Name; Title += " "; Title += CutNames[cut].Title;
		    Name += pTNames[kpT].Name; Title += " "; Title += pTNames[kpT].Title;
		    Name += etaNames[keta].Name; Title += " "; Title += etaNames[keta].Title;
		    hists[s][c][t][z][l][mGJ][cut][kpT][keta] = (TH1 *) new TH1D(Name,Title,nx,xmin,xmax);
		  }
		}
	      }
	    }
	  }
	}
  static const Double_t L = 34.42;  // half a SSD ladder's length 
  static const Double_t R = 22.80;
  // Fitter 
  Int_t NevProc = 0;
#ifdef __TCFIT__
  TCFitV0 dat;
  TCFit tc("Fit decay length",&dat); 
  tc.SetDebug(_debug);
#endif
  MTrack track[2];
  KFParticle particle[4];
  while (iter.Next()) { // Loop over events
    NevProc++;
    Double_t dEdxScale = 1;
    Int_t run = RunId[0];
    if (run < 10000) {
      dEdxScale = TMath::Exp(7.81779499999999961e-01);
    } else {
      if (run >= 5338005 && run <= 6177009) {// Run V, CuCu
	if (MuEvent_mRunInfo_mCenterOfMassEnergy[0] > 60 && MuEvent_mRunInfo_mCenterOfMassEnergy[0] < 70) {// 62 GeV
	  // 76007 is cu-zdc-narrow (ZDC coincidence with 80 cm vertex cut),
	  // 76011 is cu-bbc-narrow (BBC coincidence with vertex cut). 
	  // 76002 is cu-zdc-tacs (ZDC coincidence with no vertex cut.) 
	  if (run <  6069077 && ! (isTrigger(76002,mNTriggerId_mId) || isTrigger(76011,mNTriggerId_mId)) ) continue;
	  if (run >= 6069077 && ! (isTrigger(76007,mNTriggerId_mId) || isTrigger(76011,mNTriggerId_mId)) ) continue;
	} else {
	  /* 2005 CuCu http://www.star.bnl.gov/protected/common/common2005/trigger2005/CuFAQ.html
	     Trigger Id	 Type	                         Offline vertex cut	 Trigger setup
	     66007	         Min Bias (cu-zdc-narrow)	 |vz|<30 cm	         cuProductionHighTower, cuProductionMinBias
	     66203	         Barrel High Tower (cu-bemc-ht18)|vz|<30 cm	         cuProductionHighTower */
	  if (! (isTrigger(66007,mNTriggerId_mId) || isTrigger(66203,mNTriggerId_mId))) continue;
	}
      } 
    }
    /* 2007 AuAu http://www.star.bnl.gov/protected/common/common2007/trigger2007/triggers2007.html
       Production Trigger id's (remember that trigger id's < 1000 are test trigger id's with no guarantee as to sanity):
       Trigger Id	Name	First Run	Last Run	Description
       200001	mb-vpd	8097121	8102062	ZDC coincidence+VPD cut at 5 cm. Note: the ZDC West was somewhat wide, may introduce inefficiencies. Killer bits on.
       200003	mb-vpd	8103029	8113074	ZDC coincidence+VPD cut at 5 cm. Killer bits on.
       200013	mb-vpd	8113077	8177059	ZDC coincidence+VPD cut at 5 cm. Killer bits off.
       200020	mb30cm	8120052	8159044	ZDC coincidence+VPD cut at 30 cm. Only in 2007LowLuminosity trgsetupname.
       200212	btag	8097121	8102029	 ZDC coincidence + VPD cut at 5 cm + Barrel High Tower at threshold 1 (18, 4.1 GeV, accepting Et>4.3 GeV). 
       Note: ZDC West wide, may lead to inefficiency. NOTE: this trigger id was reused for another meaning.
       200213	btag	8103029	8113068	 ZDC coincidence + VPD cut at 5 cm + Barrel High Tower at threshold 1 (18, 4.1 GeV, accepting Et>4.3 GeV).
       200214	btag	8113102	8177038	 ZDC coincidence + VPD cut at 5 cm + Barrel High Tower at threshold 1 (18, 4.1 GeV, accepting Et>4.3 GeV). ZDC killer bits off.
       200601	L2-upsilon	8103029	8113068	L2 upsilon, based on VPD at 30 cm + L0 BHT1 (18, 4.08 GeV accepting Et>4.3 GeV) + L2 upsilon. 
       No SSD required, but cannot happen at the same time as mb-vpd.
       200602	L2-upsilon	8113102	8177038	L2 upsilon, based on ZDC + VPD at 30 cm + L0 BHT1 (18, 4.08 GeV accepting Et>4.3 GeV) + L2 upsilon. 
       No SSD required, but cannot happen at the same time as mb-vpd. ZDC killer bits off
       200610	upsilon-mb	8103029	8113068	 Luminosity monitor for the L2-upsilon. ZDC coincidence + VPD at 30 cm. ZDC killer bits on.
       200611	upsilon-mb	8113102	8177038	Luminosity monitor for the L2-upsilon. ZDC coincidence + VPD at 30 cm. ZDC killer bits off.
       200620	L2-gamma	8109015	8113068	Tagger for express stream. Based on bht2-mb, with additional higher pt cluster cuts on cluster energy.
       200621	L2-gamma	8113102	8177038	Tagger for express stream. Based on bht2-mb, with additional higher pt cluster cuts on cluster energy.    */

    Double_t HZ = 0.000299792458 * mMagneticFieldZ[0];
    KFParticle::SetField(mMagneticFieldZ[0]);
    //    if (NoTracks < 10) continue;
    nPrim->Fill(NoPrimVertex);
    for (Int_t l = 0; l < NoPrimVertex; l++) {
      if (l != 0) continue; // the best vertex is the first one
      priVtxXY->Fill(PrimVertexX[l],PrimVertexY[l]);
      priVtxZ->Fill(PrimVertexZ[l]);
      priR->Fill(TMath::Sqrt(PrimVertexX[l]*PrimVertexX[l] +PrimVertexY[l]*PrimVertexY[l]));
      priVtxSigmaZ->Fill(PrimVerSigZ[l]);
      if (PrimVerSigZ[l] < 0 || PrimVerSigZ[l] > 0.080) continue;
      Double_t eta_max = - TMath::Log(TMath::Tan(0.5*TMath::ATan2(R, L-PrimVertexZ[l])));
      Double_t eta_min = - TMath::Log(TMath::Tan(0.5*TMath::ATan2(R,-L-PrimVertexZ[l])));
      Double_t vtx[3] = {PrimVertexX[l], PrimVertexY[l], PrimVertexZ[l]};
      MVertex vert;
      //      vert.SetXYZ(PrimVertexX[l],PrimVertexY[l],PrimVertexZ[l]);
      vert.SetXYZ(0.,0.,PrimVertexZ[l]); // track will be moved to (x,y)_Vertex
      Double_t CovVert[6] = {
	PrimVerSigX[l]*PrimVerSigX[l], 
	0, PrimVerSigY[l]*PrimVerSigY[l],
	0, 0, PrimVerSigZ[l]*PrimVerSigZ[l]};
      if (CovVert[0] < 1e-8) CovVert[0] = 1e-8;
      if (CovVert[2] < 1e-8) CovVert[2] = 1e-8;
      if (CovVert[5] < 1e-8) CovVert[5] = 1e-8;
      vert.SetCovarianceMatrix(CovVert);
      vert.SetNContributors(2);
      vert.SetChi2(1.01);
      
      particle[0] = KFParticle(vert);
#if 0
      if (_debug > 1) {
	cout << "1. Construction from Vertex" << endl<< endl;
	cout << "	Vertex	Particle:";
	for(int i=0; i<3; i++)
	  cout <<"\tfP["<<i<<"]	"<< vert.GetParameter(i) << "	" << particle[0].Parameter(i);
	cout << endl;
	cout <<"fNDF	"<< 2*vert.GetNContributors()-3 << "	" << particle[0].GetNDF();
	cout <<"fChi2	"<< vert.GetChi2() << "	" << particle[0].GetChi2() << endl;
      }
#endif
      // pairs
      Int_t NoFSvtHits[2];  //[0] -> K, [1] -> pi
      Int_t NoFSsdHits[2];
      Int_t NoFSvtSsdHits[2];
      Int_t NoPSvtHits[2];
      Int_t NoPSsdHits[2];
      Int_t NoPSvtSsdHits[2];
      Double_t dcaXY[2];
      Double_t dcaZ[2];
      Double_t sigmaXY[2];
      Double_t sigmaZ[2];
      TVector3 p[4];
      TLorentzVector p4[4][2]; // K,pi; e,e;
      Int_t charge[2];
      TVector3 dir[3];
      TVector3 dcaG[2];
      Double_t EffMass[NEfM];
      TLorentzVector PP[4];
      Double_t dL;
      Double_t eL;
      Double_t slength;
      Double_t chisq;
      Double_t prob, probKF;
      Double_t dslength;
      nTracks->Fill(NoTracks);
      Int_t NoTracksU = 0;
      // 
      //      if (NoTracks > 250) continue; //http://www.star.bnl.gov/protected/heavy/baumgart/D0_CuCu.html
      for (Int_t k = 0; k<NoTracks; k++) { // <==== 1st particle K
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	Int_t kg = mIndex2Global[k];  // <==== index of global associated with the above primary track
	if (kg < 0 || kg > NoTracksGl) continue;
	if (mNHitsFitGl[kg] < 15) continue;
	//	if (pTGl[kg] < 0.1) continue;
	if (pTGl[kg] < pTCut) continue;
	if (EtaGl[kg] <= eta_min || EtaGl[kg] >= eta_max) continue;
	if (dEdxTrackLength[kg] < 40.0) continue;
	//	if (mSigmaDcaDGl[kg] <= 0 || mSigmaDcaDGl[kg] > 1 || mSigmaDcaZGl[kg] <= 0 || mSigmaDcaZGl[kg] > 1) continue;
	NoFSvtHits[0] =  (mNHitsFitInnerGl[kg] & 0x7);
	NoFSsdHits[0] = ((mNHitsFitInnerGl[kg] & 0x18) >> 3);
	NoFSvtSsdHits[0] = NoFSvtHits[0] + NoFSsdHits[0];
	NoPSvtHits[0] =  (mNHitsPossInnerGl[kg] & 0x7);
	NoPSsdHits[0] = ((mNHitsPossInnerGl[kg] & 0x18) >> 3);
	NoPSvtSsdHits[0] = NoPSvtHits[0] + NoPSsdHits[0];
	p[0] = TVector3(pXGl[kg],pYGl[kg],pZGl[kg]);
	p4[0][0].SetVectMag(p[0],amK);
	//<<< p[0] = TVector3(-5.5036, -3.6384,-1.2434);
	p[2] = TVector3(pX[k],pY[k],pZ[k]);
	  p4[0][1].SetVectMag(p[2],amK);
	charge[0] = 0;
	if  (QGl[kg] < 0) charge[0] = 1;
	
	dcaG[0] = TVector3(mDCAGlobal_mX1Gl[kg],mDCAGlobal_mX2Gl[kg],mDCAGlobal_mX3Gl[kg]);
	
	Int_t kgc = GlobalTracks_mIndex2Cov[kg];
        static StDcaGeometry dcaGeometry;
	Double_t parsK[6] = {
	  CovGlobTrack_mImp[kgc],CovGlobTrack_mZ[kgc],CovGlobTrack_mPsi[kgc],
	  CovGlobTrack_mPti[kgc],CovGlobTrack_mTan[kgc],CovGlobTrack_mCurv[kgc]};
	Double_t errsK[15] = {
	  CovGlobTrack_mImpImp[kgc],
	  CovGlobTrack_mZImp[kgc],  CovGlobTrack_mZZ[kgc],
	  CovGlobTrack_mPsiImp[kgc],CovGlobTrack_mPsiZ[kgc],CovGlobTrack_mPsiPsi[kgc],
	  CovGlobTrack_mPtiImp[kgc],CovGlobTrack_mPtiZ[kgc],CovGlobTrack_mPtiPsi[kgc],CovGlobTrack_mPtiPti[kgc],
	  CovGlobTrack_mTanImp[kgc],CovGlobTrack_mTanZ[kgc],CovGlobTrack_mTanPsi[kgc],CovGlobTrack_mTanPti[kgc],CovGlobTrack_mTanTan[kgc]};
#if 0
	TRSymMatrix RK(5,errsK); PrP(RK);
	TRSymMatrix RKI(RK,TRArray::kInverted); PrP(RKI);
#endif
	dcaGeometry.set(parsK, errsK);
        THelixTrack     thelixK =  dcaGeometry.thelix();
	Double_t ermx[3];
	thelixK.Dca(vtx,parsK[0],parsK[1],ermx,2);
	dcaXY[0] = parsK[0];
	dcaZ[0]  = parsK[1];
	sigmaXY[0]   = 0.0;
	sigmaZ[0]    = 0.0;
        if (errsK[0] > 0) sigmaXY[0] = TMath::Sqrt(errsK[0]);
        if (errsK[2] > 0) sigmaZ[0]  = TMath::Sqrt(errsK[2]);
	sigmaXY[0] = TMath::Sqrt(sigmaXY[0]*sigmaXY[0] + 
				 PrimVerSigX[l]*PrimVerSigX[l] +  
				 PrimVerSigY[l]*PrimVerSigY[l]);
	sigmaZ[0] = TMath::Sqrt(sigmaZ[0]*sigmaZ[0] + PrimVerSigZ[l]*PrimVerSigZ[l]);
	NoTracksU++;
	TRVector p1(6);
	TRSymMatrix C1(21);
	dcaGeometry.GetXYZ(p1.GetArray(),C1.GetArray());
	track[0].SetParameters(p1.GetArray());
	track[0].SetCovarianceMatrix(C1.GetArray());
	track[0].SetNDF(1);
	track[0].SetChi2(1.5);
	track[0].SetID(15);
	Int_t q   = 1;
	Int_t pdg = 321;
	if (charge[0]) {
	  q = -1;
	  pdg = -321;
	} 
	track[0].SetCharge(q);
	particle[1] = KFParticle(track[0], pdg);
#if 0
	if (_debug) {
	  cout << "2. Construction from Track" << endl<< endl;
	  cout << "	Track	Particle";
	  for(int i=0; i<6; i++)
	    cout <<"\tfP["<<i<<"]	"<< track[0].GetParameter(i) << "	" << particle[1].Parameter(i);
	  cout << endl;
	}
#endif
	for (Int_t i = 0; i < NoTracks; i++) {// <==== 2nd particle pi
	  if (i ==  k) continue;
	  if (PrimaryTracks_mVertexIndex[i] != l) continue;
	  Int_t ig = mIndex2Global[i];
	  if (ig < 0 || ig > NoTracksGl) continue;
	  if (pTGl[ig] < pTCut) continue;
	  if (EtaGl[ig] <= eta_min || EtaGl[ig] >= eta_max) continue;
	  //	  if (mSigmaDcaDGl[ig] <= 0 || mSigmaDcaDGl[ig] > 1 || mSigmaDcaZGl[ig] <= 0 || mSigmaDcaZGl[ig] > 1) continue;
	  if (dEdxTrackLength[ig] < 40.0) continue;
	  NoFSvtHits[1] =  (mNHitsFitInnerGl[ig] & 0x7);
	  NoFSsdHits[1] = ((mNHitsFitInnerGl[ig] & 0x18) >> 3);
	  NoFSvtSsdHits[1] = NoFSvtHits[1] + NoFSsdHits[1];
	  NoPSvtHits[1] =  (mNHitsPossInnerGl[ig] & 0x7);
	  NoPSsdHits[1] = ((mNHitsPossInnerGl[ig] & 0x18) >> 3);
	  NoPSvtSsdHits[1] = NoPSvtHits[1] + NoPSsdHits[1];
	  p[1] = TVector3(pXGl[ig],pYGl[ig],pZGl[ig]);
	  p4[1][0].SetVectMag(p[1],amPi);
	  p[3] = TVector3(pX[i],pY[i],pZ[i]);
	  p4[1][1].SetVectMag(p[3],amPi);
	  charge[1] = 0;
	  if  (QGl[ig] < 0) charge[1] = 1;
	  dcaG[1] = TVector3(mDCAGlobal_mX1Gl[ig],mDCAGlobal_mX2Gl[ig],mDCAGlobal_mX3Gl[ig]);
	  Int_t kgc = GlobalTracks_mIndex2Cov[ig];
	  static StDcaGeometry dcaGeometry;
	  Double_t parsPi[6] = {
	    CovGlobTrack_mImp[kgc],CovGlobTrack_mZ[kgc],CovGlobTrack_mPsi[kgc],
	    CovGlobTrack_mPti[kgc],CovGlobTrack_mTan[kgc],CovGlobTrack_mCurv[kgc]};
	  Double_t errsPi[15] = {
	    CovGlobTrack_mImpImp[kgc],
	    CovGlobTrack_mZImp[kgc],  CovGlobTrack_mZZ[kgc],
	    CovGlobTrack_mPsiImp[kgc],CovGlobTrack_mPsiZ[kgc],CovGlobTrack_mPsiPsi[kgc],
	    CovGlobTrack_mPtiImp[kgc],CovGlobTrack_mPtiZ[kgc],CovGlobTrack_mPtiPsi[kgc],CovGlobTrack_mPtiPti[kgc],
	    CovGlobTrack_mTanImp[kgc],CovGlobTrack_mTanZ[kgc],CovGlobTrack_mTanPsi[kgc],CovGlobTrack_mTanPti[kgc],CovGlobTrack_mTanTan[kgc]};
#if 0
	  if (_debug) {
	    TRSymMatrix Rpi(5,errsPi); PrP(Rpi);
	    TRSymMatrix RpiI(Rpi,TRArray::kInverted); PrP(RpiI);
	  }
#endif
	  dcaGeometry.set(parsPi, errsPi);
	  THelixTrack     thelixPi =  dcaGeometry.thelix();
	  Double_t ermx[3];
	  thelixPi.Dca(vtx,parsPi[0],parsPi[1],ermx,2);
	  dcaXY[1] = parsPi[0];
	  dcaZ[1]  = parsPi[1];
	  sigmaXY[1]   = 0;
	  sigmaZ[1]    = 0;
	  if (errsPi[0] > 0) sigmaXY[1] = TMath::Sqrt(errsPi[0]);
	  if (errsPi[2] > 0) sigmaZ[1]  = TMath::Sqrt(errsPi[2]);
	  sigmaXY[1] = TMath::Sqrt(sigmaXY[1]*sigmaXY[1] + 
				   PrimVerSigX[l]*PrimVerSigX[l] +  
				   PrimVerSigY[l]*PrimVerSigY[l]);
	  sigmaZ[1] = TMath::Sqrt(sigmaZ[1]*sigmaZ[1] + PrimVerSigZ[l]*PrimVerSigZ[l]);
	  TVector3 P(p[0]);
	  P += p[1];
	  dir[2] = P.Unit();
#ifdef __TCFIT__
	  tc.Reset(); dat.Reset();
	  for (Int_t ip = 0; ip < 2; ip++) {
	    dat.mTkBas[ip].Reset();
	    Double_t ptin = QGl[kg]/p[ip].Pt();
	    dat.mTkBas[ip].SetHz(1.); 	    
	    Double_t wk[9];
	    p[ip].GetXYZ(wk+3);
	    dcaG[ip].GetXYZ(wk+0);
	    THelixTrack th(wk+0,wk+3,ptin*HZ);
	    th.Backward();
	    double s =th.Path(0.,0.);
	    th.Move(s);th.Backward();
	    th.Eval(0.,wk,wk+3);
	    TVector3 pos(wk);
	    dir[ip] = TVector3(wk+3);
	    dat.mTkBas[ip].Reset();
	    dat.mTkBas[ip].Set(pos,dir[ip],ptin);
	    dat.mTkBas[ip].mass= (!ip)? amK:amPi;
	    dat.mTkBas[ip].Update();
	    Double_t *errs = &errsK[0];
	    if (ip)  errs = &errsPi[0];
	    for (Int_t n = 0; n < 5; n++) {
	      for (Int_t m = 0; m <= n; m++) {
		Int_t nm = (n*(n+1))/2 + m;
		dat.mTEBas[ip].Set(n,m,errs[nm]);
	      }
	    }
	    dat.mTkBas[0].SetHz(mMagneticFieldZ[0]/4.98478);   
	    dat.mTkBas[1].SetHz(mMagneticFieldZ[0]/4.98478);   
	  }
	  dat.Ready();
	  // 	  dat.FixPar(TCFitV0::kPHI_0);
	  // 	  dat.FixPar(TCFitV0::kPHI_1);
	  // 	  dat.FixPar(TCFitV0::kPTIN_0);
	  // 	  dat.FixPar(TCFitV0::kPTIN_1);
	  // 	  dat.FixPar(TCFitV0::kTANL_0);
	  // 	  dat.FixPar(TCFitV0::kTANL_1);
	  dat.FixPar(TCFitV0::kCNRJ);
	  tc.SetMaxIter(10);
	  //	  gBenchmark->Start("TCFit");
	  Int_t status = tc.Fit();
	  //	  gBenchmark->Stop("TCFit");
	  if (status) {
	    if (_debug) {
	      cout << "tc.Fit fails" << endl;
	    }
	    continue;
	  }
	  dL = dat.GetPar(TCFitV0::kLEN_2);
	  eL = sqrt(dat.ErMx(TCFitV0::kLEN_2,TCFitV0::kLEN_2));
	  chisq = dat.GetFcn();  
	  prob = TMath::Prob(chisq,dat.GetNDF());
	  Prob->Fill(prob);
	  if (_debug) {
	    cout << "dL = " << dL << " +/- " << eL << " chisq = " << chisq << " NDF = " << dat.GetNDF() 
		 << "\tprob = " << prob <<  endl;
	  }
	  slength = dL;
	  dslength = eL;
	  //	  }
#endif
	  TRVector p2(6);
	  TRSymMatrix C2(21);
	  dcaGeometry.GetXYZ(p1.GetArray(),C1.GetArray());
	  track[1].SetParameters(p1.GetArray());
	  track[1].SetCovarianceMatrix(C1.GetArray());
	  track[1].SetNDF(1);
	  track[1].SetChi2(1.5);
	  track[1].SetID(15);
	  q   = 1;
	  pdg = 211;
	  if (charge[1]) {
	    q = -1;
	    pdg = -211;
	  } 
	  track[1].SetCharge(q);
	  particle[2] = KFParticle(track[1], pdg);
#if 0
	  if (_debug > 1) {
	    cout << "2. Construction from an other Track" << endl<< endl;
	    cout << "	Track	Particle" << endl;
	    for(int i=0; i<6; i++)
	      cout <<"\tfP["<<i<<"]	"<< track[1].GetParameter(i) << "	" << particle[2].Parameter(i) << endl;
	    cout << endl;
	    
	    for(int i=0; i<21; i++)
	      cout <<"fC["<<i<<"]	"<< track[1].GetCovariance(i) << "	" << particle[2].Covariance(i) << endl;
	    cout << endl;
	    cout <<"fQ	"<< track[1].Charge() << "	" << particle[2].GetQ() << endl;
	    cout <<"PDG	"<< "11" << endl;
	    double M, dM;
	    particle[2].GetMass(M,dM);
	    cout <<"Mass		" << M << endl;
	    cout << endl;
	  }
#endif
	  particle[3] = KFParticle(particle[1],particle[2]);
#if 0
	  if (_debug > 1) {
	    cout << "3. Now we will create one more particle from track and call the construction from these 2 particles" << endl;
	    cout << "	Particle:";
	    for(int i=0; i<6; i++)
	      cout <<"\tfP["<<i<<"]	"<< particle[3].Parameter(i);
	    cout << endl;
	    cout <<"fQ	"<< particle[3].GetQ();
	    cout <<"\tMass	"<< particle[3].GetMass() << endl;
	  }
#endif
	  const KFParticle pVertex = particle[0];
	  int NDaughters = 2;
	  const KFParticle *vDaughters[2] = {&particle[1],&particle[2]};
	  
	  KFParticle D0;
	  //	  gBenchmark->Start("KFParticle");
	  D0.Construct(vDaughters,NDaughters,&pVertex,-1,0);
	  gBenchmark->Stop("KFParticle");
	  //	  probKF = TMath::Prob(D0.GetChi2(),D0.GetNDF());
	  ProbKF->Fill(probKF);
	  if (_debug) {
	    cout << "4.2 Construction without constrained Mass, with vertex hipotesis " << endl;
	    cout << "D0 x,y,z =" << D0.GetX() << ","<< D0.GetY() << ","<< D0.GetZ() << ","
		 << "\tpx,y,z =" << D0.GetPx() << ","<< D0.GetPy() << ","<< D0.GetPz() << ","
		 << "\tM = "     << D0.GetMass() << " +/- " << D0.GetErrMass() 
		 << "\tS = " << D0.GetS() << " +/- " << D0.GetErrS() << endl;
	    cout << "chi2 = " << D0.GetChi2() << "\tNDF = " << D0.GetNDF() << "\tprob = " << probKF << endl;
	  }
	  //--------------------------------------------------------------------------------
	  if (TMath::Abs(slength) > 0.20) continue;
	  //	  Int_t ki[2][2] = {{kg,ig},{ig,kg}};
	  //	  Int_t kip[2][2] = {{0,1},{1,0}};
	  Int_t c = charge[0] + 2*charge[1];
	  Double_t dEdx[2]   = {dEdxScale*GdEdx[kg],dEdxScale*GdEdx[ig]};
	  Double_t pId[2][4] = { 
	    {NSigmaKaonGl[kg]/__SIGMA_SCALE__, NSigmaPionGl[kg]/__SIGMA_SCALE__, 
	     NSigmaProtonGl[kg]/__SIGMA_SCALE__, NSigmaElectronGl[kg]/__SIGMA_SCALE__},
	    {NSigmaKaonGl[ig]/__SIGMA_SCALE__, NSigmaPionGl[ig]/__SIGMA_SCALE__, 
	     NSigmaProtonGl[ig]/__SIGMA_SCALE__, NSigmaElectronGl[ig]/__SIGMA_SCALE__}
	  };
	  Bool_t KaonPiD = 	    TMath::Abs(pId[0][0]) < 2.0;
	  Bool_t PionPiD = 	    TMath::Abs(pId[1][1]) < 2.0;
	  pIdKpi[c]->Fill(pId[1][1],pId[0][0]);
	  TdEdx->Fill(TMath::Log10(p[0].Mag()), TMath::Log10(dEdx[0])+6.);
	  TdEdx->Fill(TMath::Log10(p[1].Mag()), TMath::Log10(dEdx[1])+6.);
	  Bool_t KpiPiD = KaonPiD && PionPiD;
	  if (KpiPiD) {
	    pIdKpiC[c]->Fill(pId[1][1],pId[0][0]);
	    TdEdxKaonC->Fill(TMath::Log10(p[0].Mag()), TMath::Log10(dEdx[0])+6.);
	    TdEdxPionC->Fill(TMath::Log10(p[1].Mag()), TMath::Log10(dEdx[1])+6.);
	  }
	  PP[0]  = p4[0][0];
	  PP[0] += p4[1][0];
	  /*
	    The Gottfried - Jackson (GJ) frame is a rest frame of the Kpi system 
	    in which the  z-axis is in the direction of the beam momentum and the y-axis is in the 
	    direction of the vector cross-product of the target and recoil momenta.
	    Gottfried - Jackson:  cosTheta is projection in GJ frame momentum of K on direction of Kpi 
	    in original frame
	  */
	  TVector3 bF = PP[0].BoostVector();
	  TVector3 b(-bF.X(),-bF.Y(),-bF.Z());
	  TLorentzVector Kl(p4[0][0]);
	  Kl.Boost(b);
	  TVector3 dother(Kl.Vect());
	  TVector3 mother(PP[0].Vect());
	  Double_t cosTheta_GJ_K = dother.Dot(mother)/(dother.Mag()*mother.Mag()); 
	  EffMass[0] = PP[0].M();
	  PP[1]  = p4[0][1];
	  PP[1] += p4[1][1];
	  EffMass[1] = PP[1].M();
	  
	  if (_debug) {
	    cout << "MKpi_GL\t" << EffMass[0] << "\tMKpi_Pr\t" << EffMass[1] 
		 << "\tCos(Theta_GJ)\t" << cosTheta_GJ_K << endl;;
	  }
	  // 	  if ( mKpiMin < mKpiMax) {
	  // 	    if (EffMass[0] <  mKpiMin || EffMass[0] > mKpiMax) continue;
	  // 	  }
	  pTKpi[c]->Fill(p[1].Perp(),p[0].Perp());
	  pKpi[c]->Fill(p[1].Mag(),p[0].Mag());
	  if (p[0].Mag() < 0.5 || p[1].Mag() < 0.5) continue;
	  // || p[0].Mag()*p[1].Mag() < 0.65) continue;
	  pTKpiC[c]->Fill(p[1].Perp(),p[0].Perp());
	  pKpiC[c]->Fill(p[1].Mag(),p[0].Mag());
	  CosTGJKpi[c]->Fill(cosTheta_GJ_K);
	  DCAxyKpi[c]->Fill(dcaXY[1],dcaXY[0]);
	  DCAzKpi[c]->Fill(dcaZ[1],dcaZ[0]);
	  dEdxKpi[c]->Fill(dEdx[1],dEdx[0]);
	  Double_t LengthOverSigma = -99;
	  if (dslength > 0) LengthOverSigma = slength/dslength;
	  //                                 0 DL,1            SL,2        EM,3      EMVx,4  DLKF, SLKF, EMKF
	  Double_t Vars[6] = {TMath::Abs(slength), LengthOverSigma, EffMass[0],
			      TMath::Abs(D0.GetS()), 
			      D0.GetErrS() > 0 ? TMath::Abs(D0.GetS())/D0.GetErrS() : 0,
			      D0.GetMass()};
	  Bool_t DCACUT = 
	    TMath::Abs(dcaXY[0]) < DcaCut && TMath::Abs(dcaXY[1]) < DcaCut &&
	    TMath::Abs(dcaZ[0])  < DcaCut && TMath::Abs(dcaZ[1])  < DcaCut;
	  Bool_t LDCAcuts[2] = {prob   > 1e-2 && (dslength     > 0 && TMath::Abs(slength)   < DcaCut && DCACUT),
				probKF > 1e-2 && (D0.GetErrS() > 0 && TMath::Abs(D0.GetS()) < DcaCut && DCACUT)};
	  //                   0        1                       2                       3
	  Double_t slengthKF = D0.GetS();
	  Double_t LengthOverSigmaKF = Vars[5];
	  Bool_t BL[2][10] = {
	    {kTRUE, 
	     LDCAcuts[0], 
	     LDCAcuts[0] && slength > 0, 
	     LDCAcuts[0] && slength < 0,
	     LDCAcuts[0] &&  LengthOverSigma >  1, 
	     LDCAcuts[0] &&  LengthOverSigma < -1,
	     LDCAcuts[0] &&  LengthOverSigma >  2, 
	     LDCAcuts[0] &&  LengthOverSigma < -2, // 6,7
	     LDCAcuts[0] &&  LengthOverSigma >  3, 
	     LDCAcuts[0] &&  LengthOverSigma <  -3},// 8,9
	    {kTRUE, 
	     LDCAcuts[1], 
	     LDCAcuts[1] && slengthKF > 0, 
	     LDCAcuts[1] && slengthKF < 0,
	     LDCAcuts[1] &&  LengthOverSigmaKF >  1, 
	     LDCAcuts[1] &&  LengthOverSigmaKF < -1,
	     LDCAcuts[1] &&  LengthOverSigmaKF >  2, 
	     LDCAcuts[1] &&  LengthOverSigmaKF < -2, // 6,7
	     LDCAcuts[1] &&  LengthOverSigmaKF >  3, 
	     LDCAcuts[1] &&  LengthOverSigmaKF <  -3},// 8,9
	  };
	  Bool_t CCuts[Ncut] = {
	    kTRUE,                                                   // 0
	    NoFSvtSsdHits[0] > 0 && NoFSvtSsdHits[1]  > 0,           // 1
	    KpiPiD,                                                  // 2
	    KpiPiD && NoFSvtSsdHits[0] > 0 && NoFSvtSsdHits[1]  > 0, // 3
	    NoFSvtSsdHits[0] > 1 && NoFSvtSsdHits[1]  > 1,           // 4
	    KpiPiD && NoFSvtSsdHits[0] > 0 && NoFSvtSsdHits[1]  > 0};// 5
	  for (Int_t s = 0; s < Nsys; s++) {
	  Double_t pT = PP[s].Perp();
	    Int_t nz = 1;
	    if (TMath::Abs(PrimVertexZ[l]) < 10 ) nz = NZ;
	    else if (TMath::Abs(PrimVertexZ[l]) < 20 ) nz = NZ - 1;
	    else if (TMath::Abs(PrimVertexZ[l]) < 30 ) nz = NZ - 2;
	    if (nz <= 0) nz = 1;
	    Int_t neta = -1, npT = -1;
	    for (npT = NpT-1; npT > 0; npT--) {
	      //cout << npT << "\tpT " << pT << " pTmin " << pTmin[npT] << endl; 
	      if (pT > pTmin[npT]) break;
	    }
	    if (npT <=0 || npT >= NpT) npT = 1;
	    //	      for (npT = 1; npT < NpT; npT++) {if (pT < pTmin[npT]) break;}
	    Double_t eta = TMath::Abs(PP[s].Eta());
	    //	      for (neta = 1; neta < Neta; neta++) {if (eta < etamin[neta]) break;}
	    for (neta = 1; neta < Neta; neta++) {if (eta < etamin[neta]) break;}
	    if (neta >= Neta) neta = 1;
	    if (neta <= 0) neta = 1;
	    for (Int_t z = 0; z < nz; z++) {
	      for (Int_t lk = 0; lk < NL; lk++) {
		Bool_t GJCuts[3] = {kTRUE, TMath::Abs(cosTheta_GJ_K) <= 0.6, TMath::Abs(cosTheta_GJ_K) >  0.6};
		for (Int_t mGJ = 0; mGJ < NGJ; mGJ++) {
		  if (GJCuts[mGJ]) {
		    for (Int_t cut = 0; cut < Ncut; cut++) {
		      if (CCuts[cut]) {
			for (Int_t kpT = 0; kpT <= npT; kpT++) {
			  for (Int_t keta = 0; keta <= neta; keta++) {
			    for (Int_t t = 0; t < NT; t++) {
			      if (! BL[t/3][lk]) continue;
			      hists[s][c][t][z][lk][mGJ][cut][kpT][keta]->Fill(Vars[t]);
			    }
			  }
			}
		      }
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
      nTracksU->Fill(NoTracksU);
    }
    if (NevProc%10000 == 1) {
      cout << NevProc << "\tevents processed so far" << endl;
      //      gBenchmark->Show("TCFit");
      //      gBenchmark->Show("KFParticle");
    }
  }
  fOut->Write();
  delete fOut;
}
/*
EMKpiNP        0.108  EMKpiNPLp     0.116  EMKpiNPLn  0.232
EMKpiNPLp1    0.134  EMKpiNPLn1 0.748
EMKpiNPLp2    0.157  EMKpiNPLp2 1.711
EMKpiNPLp3    0.381  EMKpiNPLn3 0.772
EMKpiNPpT05    0.081  EMKpiNPLppT05 0.090
EMKpiNPpT10    0.119  EMKpiNPLppT10 0.110
EMKpiNPpT20    0.096  EMKpiNPLppT20 0.099  EMKpiNPLnpT20 0.217
EMKpiNPGJPpT20 0.172  EMKpiNPLpGJPpT20 0.136 EMKpiNPLnGJPpT20 0.128
EMKpiNPGJNpT20 0.123  EMKpiNPLpGJNpT20 0.143 EMKpiNPLnGJNpT20 0.180
EMKpiNPLp1pT20 0.104  EMKpiNPLn1pT20 0.515   EMKpiNPLp2GJPpT20 0.144 EMKpiNPLp2GJNpT20 0.254
EMKpiNPLp2pT20 0.130  EMKpiNPLn2pT20 1.173
EMKpiNPLp3pT20 0.507  EMKpiNPLp3pT20 9.999

01/20/08
KpiD0Mix        #std      cucu200c 63.4 M,  |z| < 10 -> 20M)
EMKpiNP         12.3742   
EMKpiNPLp       10.622 
EMKpiNPLp1      8.28414
EMKpiNPLp2      6.77897
EMKpiNPLp3      2.39407
EMKpiNPLn       7.284 
EMKpiNPLn1      4.77356
EMKpiNPLn2      3.43843
EMKpiNPLn3      1.57657
EMKpiNPdEdx     12.4304
EMKpiNPGJPdEdx  7.80157
EMKpiNPGJNdEdx  10.013
EMKpiNP 12.3742
EMKpiNPpT05     10.9887
EMKpiNPpT10     8.85068
EMKpiNPpT20     10.2894
EMKpiNPdEdxpT20 8.66682 
EMKpiNPGJPdEdxpT20      5.69601
EMKpiNPGJNdEdxpT20      6.42735
EMKpiNPLpGJNdEdxpT20    3.14536
EMKpiNPLnGJNdEdxpT20    5.04419
EMKpiNPpT50     3.461
EMKpiPN No signal

EMKpiNPpT10     4.01422
EMKpiPNpT10     3.42876 


 */


