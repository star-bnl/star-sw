//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Wed Nov 25 12:39:39 2015 by ROOT version 5.34/35
// from TTree T/TTree with SVT + SSD hits and tracks
// found on file: gmtTree_stat_din_dist.root
//////////////////////////////////////////////////////////

#ifndef StGmtQAPlotter_h
#define StGmtQAPlotter_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <iostream>
#include <TCanvas.h>
#include <TString.h>

//#define PRIMARY

using namespace std;

// Header file for the classes stored in the TTree if any.
#include <TObject.h>

// Fixed size dimensions of array or collections stored in the TTree if any.
const Int_t kMaxfTracks = 5000;
const Int_t kMaxfHits = 5000;

//_________________
class StGmtQAPlotter {
 public :
  TTree          *fChain;   //!pointer to the analyzed TTree or TChain
  Int_t           fCurrent; //!current Tree number in a TChain
  TFile          *oFile;    // Out file with histograms
  Bool_t          mWrite2File;

  //My methods
  void SetDefaults();
  void CreateHistograms();
  void CreateCanvases();
  void FillCanvases();
  void WriteCanvases2File();
  void Write2File();
  void Set1DHistoStyle(TH1* histo, Int_t mMode, Int_t mCharge);
  void SetHistosStyle();
  void MakeSlices();
  void PrintPtFitParameters();

  //Fit parameters
  double mUSlopeArr[8];
  double mUSlopeErrArr[8];
  double mUConstArr[8];
  double mUConstErrArr[8];

  double mVSlopeArr[8];
  double mVSlopeErrArr[8];
  double mVConstArr[8];
  double mVConstErrArr[8];

  /////////////////////////
  //      Histograms     //  
  /////////////////////////

  //Before cuts
  TH2F*  hUAdcDVsVAdcD[9];
  TH2F*  hDuAdcDVsDvAdcD[9];
  TH1F*  hDvAdcDOverDuAdcD[9];
  TH1F*  hFNhits;

  //After cuts
  // #0-7 are the real modules and #8 is integrated over all modules
  TH2F*  hUdiffVsBarrel;
  TH2F*  hUdiffVsBarrelPosCharge;
  TH2F*  hUdiffVsBarrelNegCharge;
  TH2F*  hVdiffVsBarrel;
  TH2F*  hVdiffVsBarrelPosCharge;
  TH2F*  hVdiffVsBarrelNegCharge;
  TH1D*  hUdiffVsBarrelSlices[2];  //Only mean and sigma
  TH1D*  hVdiffVsBarrelSlices[2];  //Only mean and sigma
  TH1D*  hUdiffVsBarrelSlicesPosCharge[2];  //Only mean and sigma
  TH1D*  hVdiffVsBarrelSlicesPosCharge[2];  //Only mean and sigma
  TH1D*  hUdiffVsBarrelSlicesNegCharge[2];  //Only mean and sigma
  TH1D*  hVdiffVsBarrelSlicesNegCharge[2];  //Only mean and sigma
  TH1F*  hUd[9];
  TH1F*  hUp[9];
  TH1F*  hVd[9];
  TH1F*  hVp[9];
  TH1F*  hTuP[9];
  TH1F*  hTvP[9];
  TH2F*  hUdVsUp[9];
  TH2F*  hVdVsVp[9];
  TH2F*  hUdVsVd[9];
  TH2F*  hUdiffVsInvPt[9];
  TH2F*  hVdiffVsInvPt[9];
  //Derivatives
  TH2F*  hUdiffVsTuP[9];
  TH1D*  hUdiffVsTuPSlices[8];
  TH2F*  hVdiffVsTvP[9];
  TH1D*  hVdiffVsTvPSlices[8];
  TH2F*  hUdiffVsVp[9];
  TH1D*  hUdiffVsVpSlices[8];
  TH2F*  hVdiffVsUp[9];
  TH1D*  hVdiffVsUpSlices[8];
  TH2F*  hUdiffOverTuPVsVp[9];
  TH1D*  hUdiffOverTuPVsVpSlices[8];
  TH2F*  hVdiffOverTvPVsVp[9];
  TH1D*  hVdiffOverTvPVsVpSlices[8];
  TH2F*  hUdiffOverTuPVsUp[9];
  TH1D*  hUdiffOverTuPVsUpSlices[8];
  TH2F*  hVdiffOverTvPVsUp[9];
  TH1D*  hVdiffOverTvPVsUpSlices[8];

  //For TDraw
  TH2D* dutuP[8];
  TH2D* dvtvP[8];
  TH2D* duvP[8];
  TH2D* dvuP[8];
  TH2D* duOvertuPvP[8];
  TH2D* dvOvertvPvP[8];
  TH2D* duOvertuPuP[8];
  TH2D* dvOvertvPuP[8];
  
  TH1D*  hUdiffVsInvPtSl[8];
  TH1D*  hVdiffVsInvPtSl[8];

  /////////////////////////
  //       Canvases      //  
  /////////////////////////

  TCanvas* cUAdcDVsVAdcDCanvas;
  TCanvas* cDuAdcDVsDvAdcDCanvas;
  TCanvas* cDvAdcDOverDuAdcDCanvas;

  TCanvas* cUdCanvas;
  TCanvas* cUpCanvas;
  TCanvas* cVdCanvas;
  TCanvas* cVpCanvas;
  TCanvas* cUdVsUpCanvas;
  TCanvas* cVdVsVpCanvas;
  TCanvas* cUdVsVdCanvas;
  TCanvas* cUdiffVsInversePtCanvas;
  TCanvas* cVdiffVsInversePtCanvas;

  TCanvas* cFitsUdiffVsInvPt;
  TCanvas* cFitsVdiffVsInvPt;
  TCanvas* cUandVdiffVsBarrel;
  
  //Derivatives
  TCanvas* cUdiffVsTuP;
  TCanvas* cVdiffVsTvP;
  TCanvas* cUdiffVsVp;
  TCanvas* cVdiffVsUp;
  TCanvas* cUdiffOverTuPVsVp;
  TCanvas* cVdiffOverTvPVsVp;
  TCanvas* cUdiffOverTuPVsUp;
  TCanvas* cVdiffOverTvPVsUp;

  TCanvas* cUdiffVsTuPSlices;
  TCanvas* cVdiffVsTvPSlices;
  TCanvas* cUdiffVsVpSlices;
  TCanvas* cVdiffVsUpSlices;
  TCanvas* cUdiffOverTuPVsVpSlices;
  TCanvas* cVdiffOverTvPVsVpSlices;
  TCanvas* cUdiffOverTuPVsUpSlices;
  TCanvas* cVdiffOverTvPVsUpSlices;

  // Declaration of leaf types
  //EventT          *EventT;
  UInt_t          fUniqueID;
  UInt_t          fBits;
  UInt_t          fNPTracks;
  UInt_t          fNtrack;
  UInt_t          fNhit;
  UInt_t          fFlag;
  Int_t           fEvtHdr_fEvtNum;
  Int_t           fEvtHdr_fRun;
  Int_t           fEvtHdr_fDate;
  Double32_t      fEvtHdr_fField;
  Double32_t      fVertex[3];
  Double32_t      fCovariantMatrix[6];
  Int_t           fTracks_;
  UInt_t          fTracks_fUniqueID[kMaxfTracks];   //[fTracks_]
  UInt_t          fTracks_fBits[kMaxfTracks];   //[fTracks_]
  Char_t          fTracks_beg[kMaxfTracks];   //[fTracks_]
  Double32_t      fTracks_fInvpT[kMaxfTracks];   //[fTracks_]
  Double32_t      fTracks_fTanL[kMaxfTracks];   //[fTracks_]
  Double32_t      fTracks_fPhi[kMaxfTracks];   //[fTracks_]
  Double32_t      fTracks_fRho[kMaxfTracks];   //[fTracks_]
  UInt_t          fTracks_fNpoint[kMaxfTracks];   //[fTracks_]
  UInt_t          fTracks_fNPpoint[kMaxfTracks];   //[fTracks_]
  Short_t         fTracks_fValid[kMaxfTracks];   //[fTracks_]
  UInt_t          fTracks_fNsp[kMaxfTracks];   //[fTracks_]
  UInt_t          fTracks_fIdHitT[kMaxfTracks][1000];   //[fTracks_]
  Double32_t      fTracks_fdEdx[kMaxfTracks];   //[fTracks_]
  Double32_t      fTracks_fLength[kMaxfTracks];   //[fTracks_]
  Char_t          fTracks_end[kMaxfTracks];   //[fTracks_]
  Int_t           fHits_;
  UInt_t          fHits_fUniqueID[kMaxfHits];   //[fHits_]
  UInt_t          fHits_fBits[kMaxfHits];   //[fHits_]
  Char_t          fHits_start[kMaxfHits];   //[fHits_]
  Int_t           fHits_Id[kMaxfHits];   //[fHits_]
  Int_t           fHits_sector[kMaxfHits];   //[fHits_]
  Int_t           fHits_barrel[kMaxfHits];   //[fHits_]
  Int_t           fHits_layer[kMaxfHits];   //[fHits_]
  Int_t           fHits_ladder[kMaxfHits];   //[fHits_]
  Int_t           fHits_wafer[kMaxfHits];   //[fHits_]
  Int_t           fHits_hybrid[kMaxfHits];   //[fHits_]
  Int_t           fHits_rdo[kMaxfHits];   //[fHits_]
  Double32_t      fHits_xG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_yG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_zG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_xGC[kMaxfHits];   //[fHits_]
  Double32_t      fHits_yGC[kMaxfHits];   //[fHits_]
  Double32_t      fHits_zGC[kMaxfHits];   //[fHits_]
  Double32_t      fHits_xL[kMaxfHits];   //[fHits_]
  Double32_t      fHits_yL[kMaxfHits];   //[fHits_]
  Double32_t      fHits_zL[kMaxfHits];   //[fHits_]
  Double32_t      fHits_u[kMaxfHits];   //[fHits_]
  Double32_t      fHits_v[kMaxfHits];   //[fHits_]
  Double32_t      fHits_w[kMaxfHits];   //[fHits_]
  Double32_t      fHits_tuP[kMaxfHits];   //[fHits_]
  Double32_t      fHits_tvP[kMaxfHits];   //[fHits_]
  Double32_t      fHits_uP[kMaxfHits];   //[fHits_]
  Double32_t      fHits_vP[kMaxfHits];   //[fHits_]
  Double32_t      fHits_pT[kMaxfHits];   //[fHits_]
  Double32_t      fHits_pMom[kMaxfHits];   //[fHits_]
  Double32_t      fHits_xPG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_yPG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_zPG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_cxPG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_cyPG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_czPG[kMaxfHits];   //[fHits_]
  Double32_t      fHits_wGu[kMaxfHits];   //[fHits_]
  Double32_t      fHits_wGv[kMaxfHits];   //[fHits_]
  Double32_t      fHits_wGw[kMaxfHits];   //[fHits_]
  Double32_t      fHits_xPL[kMaxfHits];   //[fHits_]
  Double32_t      fHits_yPL[kMaxfHits];   //[fHits_]
  Double32_t      fHits_zPL[kMaxfHits];   //[fHits_]
  Double32_t      fHits_uM[kMaxfHits];   //[fHits_]
  Double32_t      fHits_vM[kMaxfHits];   //[fHits_]
  Double32_t      fHits_anode[kMaxfHits];   //[fHits_]
  Double32_t      fHits_timeb[kMaxfHits];   //[fHits_]
  Int_t           fHits_NoHitPerTrack[kMaxfHits];   //[fHits_]
  Double32_t      fHits_uD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_vD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_duD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_dvD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_suD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_svD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_dsuD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_dsvD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_uAdcD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_vAdcD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_duAdcD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_dvAdcD[kMaxfHits];   //[fHits_]
  Double32_t      fHits_uHat[kMaxfHits];   //[fHits_]
  Double32_t      fHits_vHat[kMaxfHits];   //[fHits_]
  Int_t           fHits_NofHits[kMaxfHits];   //[fHits_]
  Int_t           fHits_NofFHits[kMaxfHits];   //[fHits_]
  Int_t           fHits_isFitted[kMaxfHits];   //[fHits_]
  Int_t           fHits_isTrack[kMaxfHits];   //[fHits_]
  Int_t           fHits_isUsedInFit[kMaxfHits];   //[fHits_]
  UInt_t          fHits_hitFlag[kMaxfHits];   //[fHits_]
  Double32_t      fHits_sLength[kMaxfHits];   //[fHits_]
  Double32_t      fHits_sLengthR[kMaxfHits];   //[fHits_]
  Double32_t      fHits_dR[kMaxfHits];   //[fHits_]
  Char_t          fHits_end[kMaxfHits];   //[fHits_]
#ifdef PRIMARY
  Bool_t          fHits_isPrimary[kMaxfHits];   //[fHits_]
  Bool_t          fHits_isCrossingMembrain[kMaxfHits];   //[fHits_]
#endif
  Bool_t          fIsValid;

  // List of branches
  TBranch        *b_EventT_fUniqueID;   //!
  TBranch        *b_EventT_fBits;   //!
  TBranch        *b_EventT_fNPTracks;   //!
  TBranch        *b_EventT_fNtrack;   //!
  TBranch        *b_EventT_fNhit;   //!
  TBranch        *b_EventT_fFlag;   //!
  TBranch        *b_EventT_fEvtHdr_fEvtNum;   //!
  TBranch        *b_EventT_fEvtHdr_fRun;   //!
  TBranch        *b_EventT_fEvtHdr_fDate;   //!
  TBranch        *b_EventT_fEvtHdr_fField;   //!
  TBranch        *b_EventT_fVertex;   //!
  TBranch        *b_EventT_fCovariantMatrix;   //!
  TBranch        *b_EventT_fTracks_;   //!
  TBranch        *b_fTracks_fUniqueID;   //!
  TBranch        *b_fTracks_fBits;   //!
  TBranch        *b_fTracks_beg;   //!
  TBranch        *b_fTracks_fInvpT;   //!
  TBranch        *b_fTracks_fTanL;   //!
  TBranch        *b_fTracks_fPhi;   //!
  TBranch        *b_fTracks_fRho;   //!
  TBranch        *b_fTracks_fNpoint;   //!
  TBranch        *b_fTracks_fNPpoint;   //!
  TBranch        *b_fTracks_fValid;   //!
  TBranch        *b_fTracks_fNsp;   //!
  TBranch        *b_fTracks_fIdHitT;   //!
  TBranch        *b_fTracks_fdEdx;   //!
  TBranch        *b_fTracks_fLength;   //!
  TBranch        *b_fTracks_end;   //!
  TBranch        *b_EventT_fHits_;   //!
  TBranch        *b_fHits_fUniqueID;   //!
  TBranch        *b_fHits_fBits;   //!
  TBranch        *b_fHits_start;   //!
  TBranch        *b_fHits_Id;   //!
  TBranch        *b_fHits_sector;   //!
  TBranch        *b_fHits_barrel;   //!
  TBranch        *b_fHits_layer;   //!
  TBranch        *b_fHits_ladder;   //!
  TBranch        *b_fHits_wafer;   //!
  TBranch        *b_fHits_hybrid;   //!
  TBranch        *b_fHits_rdo;   //!
  TBranch        *b_fHits_xG;   //!
  TBranch        *b_fHits_yG;   //!
  TBranch        *b_fHits_zG;   //!
  TBranch        *b_fHits_xGC;   //!
  TBranch        *b_fHits_yGC;   //!
  TBranch        *b_fHits_zGC;   //!
  TBranch        *b_fHits_xL;   //!
  TBranch        *b_fHits_yL;   //!
  TBranch        *b_fHits_zL;   //!
  TBranch        *b_fHits_u;   //!
  TBranch        *b_fHits_v;   //!
  TBranch        *b_fHits_w;   //!
  TBranch        *b_fHits_tuP;   //!
  TBranch        *b_fHits_tvP;   //!
  TBranch        *b_fHits_uP;   //!
  TBranch        *b_fHits_vP;   //!
  TBranch        *b_fHits_pT;   //!
  TBranch        *b_fHits_pMom;   //!
  TBranch        *b_fHits_xPG;   //!
  TBranch        *b_fHits_yPG;   //!
  TBranch        *b_fHits_zPG;   //!
  TBranch        *b_fHits_cxPG;   //!
  TBranch        *b_fHits_cyPG;   //!
  TBranch        *b_fHits_czPG;   //!
  TBranch        *b_fHits_wGu;   //!
  TBranch        *b_fHits_wGv;   //!
  TBranch        *b_fHits_wGw;   //!
  TBranch        *b_fHits_xPL;   //!
  TBranch        *b_fHits_yPL;   //!
  TBranch        *b_fHits_zPL;   //!
  TBranch        *b_fHits_uM;   //!
  TBranch        *b_fHits_vM;   //!
  TBranch        *b_fHits_anode;   //!
  TBranch        *b_fHits_timeb;   //!
  TBranch        *b_fHits_NoHitPerTrack;   //!
  TBranch        *b_fHits_uD;   //!
  TBranch        *b_fHits_vD;   //!
  TBranch        *b_fHits_duD;   //!
  TBranch        *b_fHits_dvD;   //!
  TBranch        *b_fHits_suD;   //!
  TBranch        *b_fHits_svD;   //!
  TBranch        *b_fHits_dsuD;   //!
  TBranch        *b_fHits_dsvD;   //!
  TBranch        *b_fHits_uAdcD;   //!
  TBranch        *b_fHits_vAdcD;   //!
  TBranch        *b_fHits_duAdcD;   //!
  TBranch        *b_fHits_dvAdcD;   //!
  TBranch        *b_fHits_uHat;   //!
  TBranch        *b_fHits_vHat;   //!
  TBranch        *b_fHits_NofHits;   //!
  TBranch        *b_fHits_NofFHits;   //!
  TBranch        *b_fHits_isFitted;   //!
  TBranch        *b_fHits_isTrack;   //!
  TBranch        *b_fHits_isUsedInFit;   //!
  TBranch        *b_fHits_hitFlag;   //!
  TBranch        *b_fHits_sLength;   //!
  TBranch        *b_fHits_sLengthR;   //!
#ifdef PRIMARY
  TBranch        *b_fHits_isPrimary;   //!
  TBranch        *b_fHits_isCrossingMembrain;   //!
#endif
  TBranch        *b_fHits_dR;   //!
  TBranch        *b_fHits_end;   //!
  TBranch        *b_EventT_fIsValid;   //!

  StGmtQAPlotter(TTree *tree=0, 
		 const Char_t *inFileName = "../files/gmtTree_stat_din_dist.root",
		 const Char_t *oFileName = "oGmtTree_stat_din_dist");
  virtual ~StGmtQAPlotter();
  virtual Int_t    Cut(Long64_t entry);
  virtual Int_t    GetEntry(Long64_t entry);
  virtual Long64_t LoadTree(Long64_t entry);
  virtual void     Init(TTree *tree);
  virtual void     Loop();
  virtual Bool_t   Notify();
  virtual void     Show(Long64_t entry = -1);
};

#endif

#ifdef StGmtQAPlotter_cxx
//_________________
StGmtQAPlotter::StGmtQAPlotter(TTree *tree, 
			       const Char_t *inFileName,
			       const Char_t *oFileName) : fChain(0) {

  cout << "Creating an instance of the StGmtQAPlotter..." << endl;
  // if parameter tree is not specified (or zero), connect the file
  // used to generate this class and read the Tree.
  if (tree == 0) {
    mWrite2File = true;
    TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject(inFileName);
    if (!f || !f->IsOpen()) {
      f = new TFile(inFileName);
    }
    f->GetObject("T",tree);
  }
  
  if(mWrite2File) {
    cout << "Creating output file: " << oFileName;
    oFile = new TFile(oFileName,"recreate");
    cout << "\t[DONE]" << endl;
  }
  
  Init(tree);
  cout << "The instance of the StGmtQAPlotter has been created" << endl
       << "Waiting for the command..." << endl;
}


//_________________
StGmtQAPlotter::~StGmtQAPlotter() {
  if (!fChain) return;
  delete fChain->GetCurrentFile();
}

//_________________
Int_t StGmtQAPlotter::GetEntry(Long64_t entry) {
  // Read contents of entry.
  if (!fChain) return 0;
  return fChain->GetEntry(entry);
}

//_________________
Long64_t StGmtQAPlotter::LoadTree(Long64_t entry) {
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

//_________________
void StGmtQAPlotter::Init(TTree *tree) {

  //Create histograms for work
  CreateHistograms();

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
  fChain->SetMakeClass(1);

  fChain->SetBranchAddress("fUniqueID", &fUniqueID, &b_EventT_fUniqueID);
  fChain->SetBranchAddress("fBits", &fBits, &b_EventT_fBits);
  fChain->SetBranchAddress("fNPTracks", &fNPTracks, &b_EventT_fNPTracks);
  fChain->SetBranchAddress("fNtrack", &fNtrack, &b_EventT_fNtrack);
  fChain->SetBranchAddress("fNhit", &fNhit, &b_EventT_fNhit);
  fChain->SetBranchAddress("fFlag", &fFlag, &b_EventT_fFlag);
  fChain->SetBranchAddress("fEvtHdr.fEvtNum", &fEvtHdr_fEvtNum, &b_EventT_fEvtHdr_fEvtNum);
  fChain->SetBranchAddress("fEvtHdr.fRun", &fEvtHdr_fRun, &b_EventT_fEvtHdr_fRun);
  fChain->SetBranchAddress("fEvtHdr.fDate", &fEvtHdr_fDate, &b_EventT_fEvtHdr_fDate);
  fChain->SetBranchAddress("fEvtHdr.fField", &fEvtHdr_fField, &b_EventT_fEvtHdr_fField);
  fChain->SetBranchAddress("fVertex[3]", fVertex, &b_EventT_fVertex);
  fChain->SetBranchAddress("fCovariantMatrix[6]", fCovariantMatrix, &b_EventT_fCovariantMatrix);
  fChain->SetBranchAddress("fTracks", &fTracks_, &b_EventT_fTracks_);
  fChain->SetBranchAddress("fTracks.fUniqueID", fTracks_fUniqueID, &b_fTracks_fUniqueID);
  fChain->SetBranchAddress("fTracks.fBits", fTracks_fBits, &b_fTracks_fBits);
  fChain->SetBranchAddress("fTracks.beg", fTracks_beg, &b_fTracks_beg);
  fChain->SetBranchAddress("fTracks.fInvpT", fTracks_fInvpT, &b_fTracks_fInvpT);
  fChain->SetBranchAddress("fTracks.fTanL", fTracks_fTanL, &b_fTracks_fTanL);
  fChain->SetBranchAddress("fTracks.fPhi", fTracks_fPhi, &b_fTracks_fPhi);
  fChain->SetBranchAddress("fTracks.fRho", fTracks_fRho, &b_fTracks_fRho);
  fChain->SetBranchAddress("fTracks.fNpoint", fTracks_fNpoint, &b_fTracks_fNpoint);
  fChain->SetBranchAddress("fTracks.fNPpoint", fTracks_fNPpoint, &b_fTracks_fNPpoint);
  fChain->SetBranchAddress("fTracks.fValid", fTracks_fValid, &b_fTracks_fValid);
  fChain->SetBranchAddress("fTracks.fNsp", fTracks_fNsp, &b_fTracks_fNsp);
  fChain->SetBranchAddress("fTracks.fIdHitT[1000]", fTracks_fIdHitT, &b_fTracks_fIdHitT);
  fChain->SetBranchAddress("fTracks.fdEdx", fTracks_fdEdx, &b_fTracks_fdEdx);
  fChain->SetBranchAddress("fTracks.fLength", fTracks_fLength, &b_fTracks_fLength);
  fChain->SetBranchAddress("fTracks.end", fTracks_end, &b_fTracks_end);
  fChain->SetBranchAddress("fHits", &fHits_, &b_EventT_fHits_);
  fChain->SetBranchAddress("fHits.fUniqueID", fHits_fUniqueID, &b_fHits_fUniqueID);
  fChain->SetBranchAddress("fHits.fBits", fHits_fBits, &b_fHits_fBits);
  fChain->SetBranchAddress("fHits.start", fHits_start, &b_fHits_start);
  fChain->SetBranchAddress("fHits.Id", fHits_Id, &b_fHits_Id);
  fChain->SetBranchAddress("fHits.sector", fHits_sector, &b_fHits_sector);
  fChain->SetBranchAddress("fHits.barrel", fHits_barrel, &b_fHits_barrel);
  fChain->SetBranchAddress("fHits.layer", fHits_layer, &b_fHits_layer);
  fChain->SetBranchAddress("fHits.ladder", fHits_ladder, &b_fHits_ladder);
  fChain->SetBranchAddress("fHits.wafer", fHits_wafer, &b_fHits_wafer);
  fChain->SetBranchAddress("fHits.hybrid", fHits_hybrid, &b_fHits_hybrid);
  fChain->SetBranchAddress("fHits.rdo", fHits_rdo, &b_fHits_rdo);
  fChain->SetBranchAddress("fHits.xG", fHits_xG, &b_fHits_xG);
  fChain->SetBranchAddress("fHits.yG", fHits_yG, &b_fHits_yG);
  fChain->SetBranchAddress("fHits.zG", fHits_zG, &b_fHits_zG);
  fChain->SetBranchAddress("fHits.xGC", fHits_xGC, &b_fHits_xGC);
  fChain->SetBranchAddress("fHits.yGC", fHits_yGC, &b_fHits_yGC);
  fChain->SetBranchAddress("fHits.zGC", fHits_zGC, &b_fHits_zGC);
  fChain->SetBranchAddress("fHits.xL", fHits_xL, &b_fHits_xL);
  fChain->SetBranchAddress("fHits.yL", fHits_yL, &b_fHits_yL);
  fChain->SetBranchAddress("fHits.zL", fHits_zL, &b_fHits_zL);
  fChain->SetBranchAddress("fHits.u", fHits_u, &b_fHits_u);
  fChain->SetBranchAddress("fHits.v", fHits_v, &b_fHits_v);
  fChain->SetBranchAddress("fHits.w", fHits_w, &b_fHits_w);
  fChain->SetBranchAddress("fHits.tuP", fHits_tuP, &b_fHits_tuP);
  fChain->SetBranchAddress("fHits.tvP", fHits_tvP, &b_fHits_tvP);
  fChain->SetBranchAddress("fHits.uP", fHits_uP, &b_fHits_uP);
  fChain->SetBranchAddress("fHits.vP", fHits_vP, &b_fHits_vP);
  fChain->SetBranchAddress("fHits.pT", fHits_pT, &b_fHits_pT);
  fChain->SetBranchAddress("fHits.pMom", fHits_pMom, &b_fHits_pMom);
  fChain->SetBranchAddress("fHits.xPG", fHits_xPG, &b_fHits_xPG);
  fChain->SetBranchAddress("fHits.yPG", fHits_yPG, &b_fHits_yPG);
  fChain->SetBranchAddress("fHits.zPG", fHits_zPG, &b_fHits_zPG);
  fChain->SetBranchAddress("fHits.cxPG", fHits_cxPG, &b_fHits_cxPG);
  fChain->SetBranchAddress("fHits.cyPG", fHits_cyPG, &b_fHits_cyPG);
  fChain->SetBranchAddress("fHits.czPG", fHits_czPG, &b_fHits_czPG);
  fChain->SetBranchAddress("fHits.wGu", fHits_wGu, &b_fHits_wGu);
  fChain->SetBranchAddress("fHits.wGv", fHits_wGv, &b_fHits_wGv);
  fChain->SetBranchAddress("fHits.wGw", fHits_wGw, &b_fHits_wGw);
  fChain->SetBranchAddress("fHits.xPL", fHits_xPL, &b_fHits_xPL);
  fChain->SetBranchAddress("fHits.yPL", fHits_yPL, &b_fHits_yPL);
  fChain->SetBranchAddress("fHits.zPL", fHits_zPL, &b_fHits_zPL);
  fChain->SetBranchAddress("fHits.uM", fHits_uM, &b_fHits_uM);
  fChain->SetBranchAddress("fHits.vM", fHits_vM, &b_fHits_vM);
  fChain->SetBranchAddress("fHits.anode", fHits_anode, &b_fHits_anode);
  fChain->SetBranchAddress("fHits.timeb", fHits_timeb, &b_fHits_timeb);
  fChain->SetBranchAddress("fHits.NoHitPerTrack", fHits_NoHitPerTrack, &b_fHits_NoHitPerTrack);
  fChain->SetBranchAddress("fHits.uD", fHits_uD, &b_fHits_uD);
  fChain->SetBranchAddress("fHits.vD", fHits_vD, &b_fHits_vD);
  fChain->SetBranchAddress("fHits.duD", fHits_duD, &b_fHits_duD);
  fChain->SetBranchAddress("fHits.dvD", fHits_dvD, &b_fHits_dvD);
  fChain->SetBranchAddress("fHits.suD", fHits_suD, &b_fHits_suD);
  fChain->SetBranchAddress("fHits.svD", fHits_svD, &b_fHits_svD);
  fChain->SetBranchAddress("fHits.dsuD", fHits_dsuD, &b_fHits_dsuD);
  fChain->SetBranchAddress("fHits.dsvD", fHits_dsvD, &b_fHits_dsvD);
  fChain->SetBranchAddress("fHits.uAdcD", fHits_uAdcD, &b_fHits_uAdcD);
  fChain->SetBranchAddress("fHits.vAdcD", fHits_vAdcD, &b_fHits_vAdcD);
  fChain->SetBranchAddress("fHits.duAdcD", fHits_duAdcD, &b_fHits_duAdcD);
  fChain->SetBranchAddress("fHits.dvAdcD", fHits_dvAdcD, &b_fHits_dvAdcD);
  fChain->SetBranchAddress("fHits.uHat", fHits_uHat, &b_fHits_uHat);
  fChain->SetBranchAddress("fHits.vHat", fHits_vHat, &b_fHits_vHat);
  fChain->SetBranchAddress("fHits.NofHits", fHits_NofHits, &b_fHits_NofHits);
  fChain->SetBranchAddress("fHits.NofFHits", fHits_NofFHits, &b_fHits_NofFHits);
  fChain->SetBranchAddress("fHits.isFitted", fHits_isFitted, &b_fHits_isFitted);
  fChain->SetBranchAddress("fHits.isTrack", fHits_isTrack, &b_fHits_isTrack);
  fChain->SetBranchAddress("fHits.isUsedInFit", fHits_isUsedInFit, &b_fHits_isUsedInFit);
  fChain->SetBranchAddress("fHits.hitFlag", fHits_hitFlag, &b_fHits_hitFlag);
  fChain->SetBranchAddress("fHits.sLength", fHits_sLength, &b_fHits_sLength);
  fChain->SetBranchAddress("fHits.sLengthR", fHits_sLengthR, &b_fHits_sLengthR);
#ifdef PRIMARY
  fChain->SetBranchAddress("fHits.isPrimary", fHits_isPrimary, &b_fHits_isPrimary);
  fChain->SetBranchAddress("fHits.isCrossingMembrain", fHits_isCrossingMembrain, &b_fHits_isCrossingMembrain);
#endif
  fChain->SetBranchAddress("fHits.dR", fHits_dR, &b_fHits_dR);
  fChain->SetBranchAddress("fHits.end", fHits_end, &b_fHits_end);
  fChain->SetBranchAddress("fIsValid", &fIsValid, &b_EventT_fIsValid);
  Notify();
}

//_________________
Bool_t StGmtQAPlotter::Notify() {
  // The Notify() function is called when a new file is opened. This
  // can be either for a new TTree in a TChain or when when a new TTree
  // is started when using PROOF. It is normally not necessary to make changes
  // to the generated code, but the routine can be extended by the
  // user if needed. The return value is currently not used.

  cout << "Processing file # " << fCurrent << endl;
  return kTRUE;
}

//_________________
void StGmtQAPlotter::Show(Long64_t entry) {
  // Print contents of entry.
  // If entry is not specified, print current entry
  if (!fChain) return;
  fChain->Show(entry);
}

//_________________
Int_t StGmtQAPlotter::Cut(Long64_t entry) {
  // This function may be called from Loop.
  // returns  1 if entry is accepted.
  // returns -1 otherwise.
  return 1;
}

//_________________
void StGmtQAPlotter::CreateHistograms() {
  
  cout << "Creating histograms...";
  TString sName,sTitle;
  Int_t    mLogUAdcDBins = 70;
  Double_t mLogUAdcDMin = 6.;
  Double_t mLogUAdcDMax = 13.;
  Int_t    mLogVAdcDBins = 70;
  Double_t mLogVAdcDMin = 6.;
  Double_t mLogVAdcDMax = 13.;

  Int_t    mDuAdcDBins = 120;
  Double_t mDuAdcDMin = 0.;
  Double_t mDuAdcDMax = 0.24;
  Int_t    mDvAdcDBins = 120;
  Double_t mDvAdcDMin = 0.;
  Double_t mDvAdcDMax = 0.24;

  Int_t    mDvAdcDOverDuAdcDBins = 100;
  Double_t mDvAdcDOverDuAdcDMin = 0.;
  Double_t mDvAdcDOverDuAdcDMax = 5.;

  //was 110 bins before
  Int_t    mUdBins = 55;
  Double_t mUdMin = 0.;
  Double_t mUdMax = 11.;
  Int_t    mUpBins = 55;
  Double_t mUpMin = 0.;
  Double_t mUpMax = 11.;

  //was 110 bins before
  Int_t    mVdBins = 55;
  Double_t mVdMin = 0.;
  Double_t mVdMax = 55.;
  Int_t    mVpBins = 110;
  Double_t mVpMin = 0.;
  Double_t mVpMax = 11.;

  Int_t    mFNhitsBins = 100;
  Double_t mFNhitsMin  = -0.5;
  Double_t mFNhitsMax  = 495.5;

  Int_t    mBarrelBins = 8;
  Double_t mBarrelMin = -0.5;
  Double_t mBarrelMax = 7.5;

  Int_t    mUdiffBins = 100;
  Double_t mUdiffMin = -2.5;
  Double_t mUdiffMax = 2.5;

  Int_t    mVdiffBins = 100;
  Double_t mVdiffMin = -2.5;
  Double_t mVdiffMax = 2.5;

  Int_t    mInversePtBins = 50;
  Double_t mInversePtMin = -2.5;
  Double_t mInversePtMax = 2.5;

  Int_t    mTuPBins = 100;
  Double_t mTuPMin = -1.;
  Double_t mTuPMax = 1.;

  Int_t    mTvPBins = 150;
  Double_t mTvPMin = -1.5;
  Double_t mTvPMax = 1.5;

  Int_t    mUdiffOverTuPBins = 200;
  Double_t mUdiffOverTuPMin = -10.;
  Double_t mUdiffOverTuPMax = 10.;

  Int_t    mVdiffOverTvPBins = 200;
  Double_t mVdiffOverTvPMin = -10.;
  Double_t mVdiffOverTvPMax = 10.;

  hUdiffVsBarrel = new TH2F("hUdiffVsBarrel","uD-uP vs GMT module # ;GMT module # ;uD-uP (cm)",
			    mBarrelBins,mBarrelMin,mBarrelMax,
			    mUdiffBins,mUdiffMin,mUdiffMax);
  hUdiffVsBarrel->Sumw2();

  hUdiffVsBarrelPosCharge = new TH2F("hUdiffVsBarrelPosCharge",
				     "+ charge uD-uP vs GMT module # ;GMT module # ;uD-uP (cm)",
				     mBarrelBins,mBarrelMin,mBarrelMax,
				     mUdiffBins,mUdiffMin,mUdiffMax);
  hUdiffVsBarrelPosCharge->Sumw2();
  hUdiffVsBarrelNegCharge = new TH2F("hUdiffVsBarrelNegCharge",
				     "- charge uD-uP vs GMT module # ;GMT module # ;uD-uP (cm)",
				     mBarrelBins,mBarrelMin,mBarrelMax,
				     mUdiffBins,mUdiffMin,mUdiffMax);
  hUdiffVsBarrelNegCharge->Sumw2();
 
  hVdiffVsBarrel = new TH2F("hVdiffVsBarrel","vD-vP vs GMT module # ;GMT module # ;vD-vP (cm)",
			    mBarrelBins,mBarrelMin,mBarrelMax,
			    mVdiffBins,mVdiffMin,mVdiffMax);
  hVdiffVsBarrel->Sumw2();
  hVdiffVsBarrelPosCharge = new TH2F("hVdiffVsBarrelPosCharge",
				     "+ charge vD-vP vs GMT module # ;GMT module # ;vD-vP (cm)",
				     mBarrelBins,mBarrelMin,mBarrelMax,
				     mVdiffBins,mVdiffMin,mVdiffMax);
  hVdiffVsBarrelPosCharge->Sumw2();
  hVdiffVsBarrelNegCharge = new TH2F("hVdiffVsBarrelNegCharge",
				     "- charge vD-vP vs GMT module # ;GMT module # ;vD-vP (cm)",
				     mBarrelBins,mBarrelMin,mBarrelMax,
				     mVdiffBins,mVdiffMin,mVdiffMax);
  hVdiffVsBarrelNegCharge->Sumw2();
  hFNhits = new TH1F("hFNhits","Number of GMT hits per event",
		     mFNhitsBins,mFNhitsMin,mFNhitsMax);
  hFNhits->Sumw2();

   for(Int_t iModule=0; iModule<9; iModule++) {

    //Before cuts
    sName.Form("hUAdcDVsVAdcD_%i",iModule);
    sTitle.Form("Log(uAdcD) vs Log(vAdcD) for GMT module #%i;Log(uAdcD);Log(uAdcD)",iModule);;
    hUAdcDVsVAdcD[iModule] = new TH2F(sName,sTitle,
				      mLogVAdcDBins,mLogVAdcDMin,mLogVAdcDMax,
				      mLogUAdcDBins,mLogUAdcDMin,mLogUAdcDMax);
    hUAdcDVsVAdcD[iModule]->Sumw2();
    sName.Form("hDuAdcDVsDvAdcD_%i",iModule);
    sTitle.Form("duAdcD vs dvAdcD for GMT module #%i;dvAdcD;duAdcD",iModule);
    hDuAdcDVsDvAdcD[iModule] = new TH2F(sName,sTitle,
					mDvAdcDBins,mDvAdcDMin,mDvAdcDMax,
					mDuAdcDBins,mDuAdcDMin,mDuAdcDMax);
    hDuAdcDVsDvAdcD[iModule]->Sumw2();
    sName.Form("hDvAdcDOverDuAdcD_%i",iModule);
    hDvAdcDOverDuAdcD[iModule] = new TH1F(sName,sTitle,
					  mDvAdcDOverDuAdcDBins,mDvAdcDOverDuAdcDMin,mDvAdcDOverDuAdcDMax);
    hDvAdcDOverDuAdcD[iModule]->Sumw2();	  

    //After cuts
    sName.Form("hUd_%i",iModule);
    sTitle.Form("uD for GMT module # ;uD (cm);dN/duD");
    hUd[iModule] = new TH1F(sName,sTitle,mUdBins,mUdMin,mUdMax);
    hUd[iModule]->Sumw2();
    sName.Form("hUp_%i",iModule);
    sTitle.Form("uP for GMT module # ;uP (cm);dN/duP");
    hUp[iModule] = new TH1F(sName,sTitle,mUpBins,mUpMin,mUpMax);
    hUp[iModule]->Sumw2();
    sName.Form("hVd_%i",iModule);
    sTitle.Form("vD for GMT module # ;vD (cm);dN/dvD");
    hVd[iModule] = new TH1F(sName,sTitle,mVdBins,mVdMin,mVdMax);
    hVd[iModule]->Sumw2();
    sName.Form("hVp_%i",iModule);
    sTitle.Form("vP for GMT module # ;vP (cm);dN/dvP");
    hVp[iModule] = new TH1F(sName,sTitle,mVpBins,mVpMin,mVpMax);
    hVp[iModule]->Sumw2();
    sName.Form("hUdVsUp_%i",iModule);
    sTitle.Form("uD vs uP for GMT module #%i;uP (cm);uD (cm)",iModule);
    hUdVsUp[iModule] = new TH2F(sName,sTitle,
				mUpBins,mUpMin,mUpMax,
				mUdBins,mUdMin,mUdMax);
    hUdVsUp[iModule]->Sumw2();
    sName.Form("hVdVsVp_%i",iModule);
    sTitle.Form("vD vs vP for GMT module #%i;vP (cm);vD (cm)",iModule);
    hVdVsVp[iModule] = new TH2F(sName,sTitle,
				mVpBins,mVpMin,mVpMax,
				mVdBins,mVdMin,mVdMax);
    hVdVsVp[iModule]->Sumw2();
    sName.Form("hUdVsVd_%i",iModule);
    sTitle.Form("uD vs vD for GMT module #%i;vD (cm);uD (cm)",iModule);
    hUdVsVd[iModule] = new TH2F(sName,sTitle,
				mVdBins,mVdMin,mVdMax,
				mUdBins,mUdMin,mUdMax);
    hUdVsVd[iModule]->Sumw2();

    //pT dependence
    sName.Form("hUdiffVsInvPt_%i",iModule);
    sTitle.Form("uD-uP vs 1/p_{T} for GMT module #%i;q/p_{T} (GeV/c)^{-1};uD-uP (cm)",iModule);
    hUdiffVsInvPt[iModule] = new TH2F(sName,sTitle,
				      mInversePtBins,mInversePtMin,mInversePtMax,
				      mUdiffBins,mUdiffMin,mUdiffMax);
    hUdiffVsInvPt[iModule]->Sumw2();
    sName.Form("hVdiffVsInvPt_%i",iModule);
    sTitle.Form("vD-vP vs 1/p_{T} for GMT module #%i;q/p_{T} (GeV/c)^{-1};vD-vP (cm)",iModule);
    hVdiffVsInvPt[iModule] = new TH2F(sName,sTitle,
				      mInversePtBins,mInversePtMin,mInversePtMax,
				      mUdiffBins,mUdiffMin,mUdiffMax);
    hVdiffVsInvPt[iModule]->Sumw2();

    //Derivatives
    sName.Form("hTuP_%i",iModule);
    sTitle.Form("tuP for GMT module #%i; tuP; dN/dtuP",iModule);
    hTuP[iModule] = new TH1F(sName,sTitle,mTuPBins,mTuPMin,mTuPMax);
    hTuP[iModule]->Sumw2();
    sName.Form("hTvP_%i",iModule);
    sTitle.Form("tvP for GMT module #%i; tvP; dN/dtvP",iModule);
    hTvP[iModule] = new TH1F(sName,sTitle,mTvPBins,mTvPMin,mTvPMax);
    hTvP[iModule]->Sumw2();

    //---------------
    sName.Form("hUdiffVsTuP_%i",iModule);
    sTitle.Form("uD-uP vs tuP for GMT module #%i; tuP; uD-uP (cm)",iModule);
    hUdiffVsTuP[iModule] = new TH2F(sName,sTitle,
				    mTuPBins,mTuPMin,mTuPMax,
				    mUdiffBins,mUdiffMin,mUdiffMax);
    hUdiffVsTuP[iModule]->Sumw2();

    sName.Form("hVdiffVsTvP_%i",iModule);
    sTitle.Form("vD-vP vs tvP for GMT module #%i; tvP; vD-vP (cm)",iModule);
    hVdiffVsTvP[iModule] = new TH2F(sName,sTitle,
				    mTvPBins,mTvPMin,mTvPMax,
				    mVdiffBins,mVdiffMin,mVdiffMax);
    hVdiffVsTvP[iModule]->Sumw2();

    sName.Form("hUdiffVsVp_%i",iModule);
    sTitle.Form("uD-uP vs vP for GMT module #%i; vP (cm); uD-uP (cm)",iModule);
    hUdiffVsVp[iModule] = new TH2F(sName,sTitle,
				   mVpBins,mVpMin,mVpMax,
				   mUdiffBins,mUdiffMin,mUdiffMax);
    hUdiffVsVp[iModule]->Sumw2();

    sName.Form("hVdiffVsUp_%i",iModule);
    sTitle.Form("vD-vP vs -uP for GMT module #%i; uP (cm); vD-vP (cm)",iModule);
    hVdiffVsUp[iModule] = new TH2F(sName,sTitle,
				   mUpBins,-mUpMax,-mUpMin,
				   mVdiffBins,mVdiffMin,mVdiffMax);
    hVdiffVsUp[iModule]->Sumw2();

    sName.Form("hUdiffOverTuPVsVp_%i",iModule);
    sTitle.Form(" (uD-uP)/tuP vs vP for GMT module #%i; vP (cm); (uD-uP)/tuP (cm)",iModule);
    hUdiffOverTuPVsVp[iModule] = new TH2F(sName,sTitle,
					  mVpBins,mVpMin,mVpMax,
					  mUdiffOverTuPBins,mUdiffOverTuPMin,mUdiffOverTuPMax);
    hUdiffOverTuPVsVp[iModule]->Sumw2();

    sName.Form("hUdiffOverTuPVsUp_%i",iModule);
    sTitle.Form(" (uD-uP)/tuP vs -uP for GMT module #%i; -uP (cm); (uD-uP)/tuP (cm)",iModule);
    hUdiffOverTuPVsUp[iModule] = new TH2F(sName,sTitle,
					  mUpBins,-mUpMax,-mUpMin,
					  mUdiffOverTuPBins,mUdiffOverTuPMin,mUdiffOverTuPMax);
    hUdiffOverTuPVsUp[iModule]->Sumw2();

    sName.Form("hVdiffOverTvPVsVp_%i",iModule);
    sTitle.Form(" (vD-vP)/tvP vs vP for GMT module #%i; vP (cm); (vD-vP)/tvP (cm)",iModule);
    hVdiffOverTvPVsVp[iModule] = new TH2F(sName,sTitle,
					  mVpBins,mVpMin,mVpMax,
					  mVdiffOverTvPBins,mVdiffOverTvPMin,mVdiffOverTvPMax);
    hVdiffOverTvPVsVp[iModule]->Sumw2();

    sName.Form("hVdiffOverTvPVsUp_%i",iModule);
    sTitle.Form(" (vD-vP)/tvP vs -uP for GMT module #%i; -uP (cm); (vD-vP)/tvP (cm)",iModule);
    hVdiffOverTvPVsUp[iModule] = new TH2F(sName,sTitle,
					  mUpBins,-mUpMax,-mUpMin,
					  mVdiffOverTvPBins,mVdiffOverTvPMin,mVdiffOverTvPMax);
    hVdiffOverTvPVsUp[iModule]->Sumw2();

    if(iModule<8) {
      sName.Form("dutuP%04i",iModule);
      sTitle.Form("uD-uP vs tuP =>  dw for GMT module #%i; tuP; uD-uP (cm)",iModule);
      dutuP[iModule] = new TH2D(sName,sTitle,
				mTuPBins,mTuPMin,mTuPMax,
				mUdiffBins,mUdiffMin,mUdiffMax);
      dutuP[iModule]->Sumw2();

      sName.Form("dvtvP%04i",iModule);
      sTitle.Form("vD-vP vs tvP => dw for GMT module #%i; tvP; vD-vP (cm)",iModule);
      dvtvP[iModule] = new TH2D(sName,sTitle,
				mTvPBins,mTvPMin,mTvPMax,
				mVdiffBins,mVdiffMin,mVdiffMax);
      dvtvP[iModule]->Sumw2();

      sName.Form("duvP%04i",iModule);
      sTitle.Form("uD-uP vs vP =>  gamma for GMT module #%i; vD (cm); uD-uP (cm)",iModule);
      duvP[iModule] = new TH2D(sName,sTitle,
			       mVpBins,mVpMin,mVpMax,
			       mUdiffBins,mUdiffMin,mUdiffMax);
      
      duvP[iModule]->Sumw2();

      sName.Form("dvuP%04i",iModule);
      sTitle.Form("vD-vP vs -uP =>  gamma  for GMT module #%i; -uP (cm); vD-vP (cm)", iModule);
      dvuP[iModule] = new TH2D(sName,sTitle,
			       mUpBins,-mUpMax,-mUpMin,
			       mVdiffBins,mVdiffMin,mVdiffMax);
      dvuP[iModule]->Sumw2();

      sName.Form("duOvertuPvP%04i",iModule);
      sTitle.Form("<(u - uP)/tuP> versus vP => alpha for GMT module #%i; vP (cm); (uD-uP)/tuP (cm)",iModule);
      duOvertuPvP[iModule] = new TH2D(sName,sTitle,
				      mVpBins,mVpMin,mVpMax,
				      mUdiffOverTuPBins,mUdiffOverTuPMin,mUdiffOverTuPMax);
      duOvertuPvP[iModule]->Sumw2();
      
      sName.Form("dvOvertvPvP%04i",iModule);
      sTitle.Form("<(v - vP)/tvP> versus vP => alpha for GMT module #%i;vP (cm);(vD-vP)/tvP (cm)",iModule);
      dvOvertvPvP[iModule] = new TH2D(sName,sTitle,
				      mVpBins,mVpMin,mVpMax,
				      mVdiffBins,mVdiffMin,mVdiffMax);
      dvOvertvPvP[iModule]->Sumw2();
      
      sName.Form("duOvertuPuP%04i",iModule);
      sTitle.Form("<(u - uP)/tuP> versus -uP => beta for GMT module #%i;-uP (cm);(uD-uP)/tuP (cm)",iModule);
      duOvertuPuP[iModule] = new TH2D(sName,sTitle,
				      mUpBins,-mUpMax,-mUpMin,
				      mUdiffOverTuPBins,mUdiffOverTuPMin,mUdiffOverTuPMax);
      duOvertuPuP[iModule]->Sumw2();

      sName.Form("dvOvertvPuP%04i",iModule);
      sTitle.Form("<(v - vP)/tvP> versus -uP => beta for GMT module #%i;-uP (cm);(vD-vP)/tvP (cm)",iModule);
      dvOvertvPuP[iModule] = new TH2D(sName,sTitle,
				      mUpBins,-mUpMax,-mUpMin,
				      mVdiffBins,mVdiffMin,mVdiffMax);
    }
    
  } //for(Int_t iModule=0; iModule<9; iModule++)

  cout << "\t[DONE]" << endl;
}

//_________________
void StGmtQAPlotter::Write2File() {

  cout << "Writing to file...";
  oFile->Write();
  WriteCanvases2File();
  oFile->Close();
  cout << "\t[DONE]" << endl;
}

#endif // #ifdef StGmtQAPlotter_cxx
