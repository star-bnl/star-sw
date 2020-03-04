//*-- Author : Yuri Fisyak 02/02/2016
#include "StMuMcAnalysisMaker.h"
#include "TDirectory.h"
#include "TROOT.h"
#include "TMath.h"
#include "TNtuple.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "StProbPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#if 1
#include "KFParticle/KFParticleSIMD.h"
#endif
#include "TSystem.h"
#include "TArrayD.h"
#include "TVirtualX.h"
ClassImp(StMuMcAnalysisMaker);
StMuMcAnalysisMaker *StMuMcAnalysisMaker::fgStMuMcAnalysisMaker = 0;
//                  [gp]     [type]           [particle] [pm]         [x]         [i]                  
static TH3F *fHistsT[kTotalT][kTotalTkTypes][kPartypeT][kTotalSigns][kVariables][kTotalQAll] = {0};
static TH3F *LdEdx[kTotalT][NHypTypes][kTotalSigns][NdEdxPiD] = {0};
static TH3F *LToF[kTotalT][NHypTypes][kTotalSigns][NToFPiD] = {0};
static TH1F *GiD[4] = {0};
static TH2F *McRcHit = 0;
static TH2F *hdEdX = 0;
static TH2F *hdEdXwithToF = 0;
static TH2F *hTofPID = 0;

static TH1F *hDCAp = 0;
static TH1F *hDCApi = 0;
static TH1F *hDCAK = 0;
static TH1F *hHFTDCAp = 0;
static TH1F *hHFTDCApi = 0;
static TH1F *hHFTDCAK = 0;
static TH1F *hSignalDCAp = 0;
static TH1F *hSignalDCApi = 0;
static TH1F *hSignalDCAK = 0;
static TH1F *hSignalHFTDCAp = 0;
static TH1F *hSignalHFTDCApi = 0;
static TH1F *hSignalHFTDCAK = 0;

static TH1F *hChiPrimHFTp = 0;
static TH1F *hChiPrimHFTK = 0;
static TH1F *hChiPrimHFTpi = 0;
static TH1F *hChiPrimHFTWithCutp = 0;
static TH1F *hChiPrimHFTWithCutK = 0;
static TH1F *hChiPrimHFTWithCutpi = 0;
static TH1F *hChiPrimHFTSignalp = 0;
static TH1F *hChiPrimHFTSignalK = 0;
static TH1F *hChiPrimHFTSignalpi = 0;
static TH1F *hChiPrimHFTSignalWithCutp = 0;
static TH1F *hChiPrimHFTSignalWithCutK = 0;
static TH1F *hChiPrimHFTSignalWithCutpi = 0;

static TH1F *hLcdx = 0;
static TH1F *hLcdy = 0;
static TH1F *hLcdz = 0;
static TH1F *hLcdr = 0;
static TH1F *hLcPx = 0;
static TH1F *hLcPy = 0;
static TH1F *hLcPz = 0;
static TH1F *hLcPr = 0;

static TH1F *hFitProb = 0;
static TH1F *hNHFTHits = 0;

static TH1F* hPVError = 0;
static TH2F* hPVErrorVsNTracks = 0;
static TH2F* hPVErrorVsNPVTracks = 0;

static const Char_t *TracksVertices[2] = {"Tracks","Vertices"};
static const Char_t *TitleTrType[kTotalT] = {"Global", "Primary"};
static const Char_t *TitlePiDtype[NoPiDs] = {"dEdxPiD", "ToFPiD"};
static const Char_t *TitleCharge[kTotalSigns] = {"(+)", "(-)"};   
static const Char_t *NamesF[NHYPS]    = {"electron","antiproton","kaon-","pion-","muon-","dbar","tbar","He3Bar","alphabar"
           "positron","proton"    ,"kaon+","pion+","muon+","deuteron"   ,"triton"   ,"He3"    ,"alpha"};
#if 0
static const Char_t *Names[NHYPS]     = {"e-","pbar","K-","pi-","mu-","dbar","tbar","He3Bar","alphabar"
           "e+","p"   ,"K+","pi+","mu+","d"   ,"t"   ,"He3"    ,"alpha"};
#endif
static const Double_t Masses[NHYPS] = {0.51099907e-3,0.93827231,0.493677,0.13956995,0.1056584,1.875613,2.80925, 2.80923,3.727417,
               0.51099907e-3,0.93827231,0.493677,0.13956995,0.1056584,1.875613,2.80925, 2.80923,3.727417};
static const Int_t GEANTiD[NHYPS]    = { 3, 15, 12,  9, 6, 53, 50046, 50049, 50047, // GEANT part Id
           2, 14, 11,  8, 5, 45,    46,    49,    47};
static const Int_t PiDHyp[NHYPS]     = {kPidElectron, kPidProton, kPidKaon, kPidPion, kPidMuon, kPidDeuteron, kPidTriton, kPidHe3, kPidAlpha,
          kPidElectron, kPidProton, kPidKaon, kPidPion, kPidMuon, kPidDeuteron, kPidTriton, kPidHe3, kPidAlpha};
static const Int_t PiDpm[NHYPS]      = {kNegative, kNegative, kNegative, kNegative, kNegative, kNegative, kNegative, kNegative, kNegative,
          kPositive, kPositive, kPositive, kPositive, kPositive, kPositive, kPositive, kPositive, kPositive};
#if 0
static const Char_t *HistNames[NHYPS] = {"eNzB","protonNzB","kaonNzB","piNzB","muNzB","deuteronNzB","tritonNzB","He3NzB","alphaNzB",
          "ePzB","protonPzB","kaonPzB","piPzB","muPzB","deuteronPzB","tritonPzB","HePzB","alphaPzB"};
static const Char_t *HistNames70[NHYPS] = {"eN70B","protonN70B","kaonN70B","piN70B","muN70B","deuteronN70B","tritonN70B","He3N70B","alphaN70B",
            "eP70B","protonP70B","kaonP70B","piP70B","muP70B","deuteronP70B","tritonP70B","He3P70B","alphaP70B"};
static const Char_t *HistNameP[NHYPS] = {"eNzB","protonNzB","kaonNzB","piNzB","muNzB","deuteronNzB","tritonNzB","He3NzB","alphaNzB",
          "ePzB","protonPzB","kaonPzB","piPzB","muPzB","deuteronPzB","tritonPzB","He3PzB","alphaPzB"};
#endif
static const Char_t *HitName = "vs NoFitPnts and Quality";
static const Char_t *KinName = "vs   #eta and pT/|q|";
static const Char_t *KinPionName = "vs   #eta and pT/|q| for pion";
static const Char_t *proj[5] = {"zx","zy","x","y","yx"};
static Int_t nPng = 0;
static ofstream out;
static TString Chapter;
static TString Section;
static TString SubSection;
//________________________________________________________________________________
StMuMcAnalysisMaker::StMuMcAnalysisMaker(const char *name) : StMaker(name), fProcessSignal(kFALSE) {
  memset(mBeg,0,mEnd-mBeg+1);
  fgStMuMcAnalysisMaker = this;
}
//________________________________________________________________________________
StMuMcAnalysisMaker::~StMuMcAnalysisMaker() {
#if 1
  SafeDelete(mStKFParticleInterface);
  SafeDelete(mStKFParticlePerformanceInterface);
#endif
}
//________________________________________________________________________________
TH3F *StMuMcAnalysisMaker::GetTrackHist(UInt_t track, UInt_t match, 
                UInt_t particle, UInt_t charge, 
                UInt_t var, UInt_t i) {
  //  fHistsT[kTotalT][kTotalTkTypes][kPartypeT][kTotalSigns][kVariables][kTotalQAll]
  TH3F *h = 0;
  if (track < kTotalT && match < kTotalTkTypes && particle < kPartypeT && 
      charge < kTotalSigns && var < kVariables && i < kTotalQAll)
    h = fHistsT[track][match][particle][charge][var][i];
  else {
    cout << "Illegal request :" 
   << track << "[" << kTotalT << "]" 
   << match << "[" << kTotalTkTypes << "]"
   << particle << "[" << kPartypeT << "]"
   << charge << "[" << kTotalSigns << "]"
   << var << "[" << kVariables  << "]"
         << i   << "{" << kTotalQAll  << "]" << endl;
  }
  return h;
}
//________________________________________________________________________________
TH3F *StMuMcAnalysisMaker::GetdEdxHist(UInt_t track, UInt_t particle, UInt_t charge, UInt_t var) {
  //  LdEdx[kTotalT][NHypTypes][kTotalSigns][NdEdxPiD]
  TH3F *h = 0;
  if (track < kTotalT && particle < NHypTypes && 
      charge < kTotalSigns && var < NdEdxPiD )
    h =LdEdx [track][particle][charge][var];
  else {
    cout << "Illegal request :" 
   << track << "[" << kTotalT << "]" 
   << particle << "[" << NHypTypes << "]"
   << charge << "[" << kTotalSigns << "]"
   << var << "[" << NdEdxPiD  << "]" << endl;
  }
  return h;
}
//_____________________________________________________________________________
TH3F *StMuMcAnalysisMaker::GetToFHist(UInt_t track, UInt_t particle, UInt_t charge, UInt_t var) {
  //  LToF[kTotalT][NHypTypes][kTotalSigns][NToFPiD]
  TH3F *h = 0;
  if (track < kTotalT && particle < NHypTypes && 
      charge < kTotalSigns && var < NToFPiD )
    h =LToF [track][particle][charge][var];
  else {
    cout << "Illegal request :" 
   << track << "[" << kTotalT << "]" 
   << particle << "[" << NHypTypes << "]"
   << charge << "[" << kTotalSigns << "]"
   << var << "[" << NToFPiD  << "]" << endl;
  }
  return h;
}
//_____________________________________________________________________________
Int_t StMuMcAnalysisMaker::Init(){
  TFile *f = GetTFile();
  if (f) {
    f->cd();
    if (IAttr("TrackPlots"))  BookTrackPlots();
    if (IAttr("VertexPlots")) BookVertexPlots();
  }
  
  //Create file with NTuples for cut optimization
  if(IAttr("StoreCutNTuples"))
  {
    TString ntupleNames[fNNTuples] = {"D0", "DPlus", "Ds", "Lc"};
    TString cutNames[fNNTuples] = {"nHFTHits_K:nTPCHits_K:m2ToF_K:nSigmadEdXPi_K:nSigmadEdXK_K:nSigmadEdXP_K:pt_K:chi2Primary_K:nHFTHits_Pi:nTPCHits_Pi:m2ToF_Pi:nSigmadEdXPi_Pi:nSigmadEdXK_Pi:nSigmadEdXP_Pi:pt_Pi:chi2Primary_Pi:Chi2NDF:LdL:Chi2Topo",
      "nHFTHits_K:nTPCHits_K:m2ToF_K:nSigmadEdXPi_K:nSigmadEdXK_K:nSigmadEdXP_K:pt_K:chi2Primary_K:nHFTHits_Pi1:nTPCHits_Pi1:m2ToF_Pi1:nSigmadEdXPi_Pi1:nSigmadEdXK_Pi1:nSigmadEdXP_Pi1:pt_Pi1:chi2Primary_Pi1:nHFTHits_Pi2:nTPCHits_Pi2:m2ToF_Pi2:nSigmadEdXPi_Pi2:nSigmadEdXK_Pi2:nSigmadEdXP_Pi2:pt_Pi2:chi2Primary_Pi2:Chi2NDF:LdL:Chi2Topo",
      "nHFTHits_KPlus:nTPCHits_KPlus:m2ToF_KPlus:nSigmadEdXPi_KPlus:nSigmadEdXK_KPlus:nSigmadEdXP_KPlus:pt_KPlus:chi2Primary_KPlus:nHFTHits_KMinus:nTPCHits_KMinus:m2ToF_KMinus:nSigmadEdXPi_KMinus:nSigmadEdXK_KMinus:nSigmadEdXP_KMinus:pt_KMinus:chi2Primary_KMinus:nHFTHits_Pi:nTPCHits_Pi:m2ToF_Pi:nSigmadEdXPi_Pi:nSigmadEdXK_Pi:nSigmadEdXP_Pi:pt_Pi:chi2Primary_Pi:Chi2NDF:LdL:Chi2Topo",
      "nHFTHits_P:nTPCHits_P:m2ToF_P:nSigmadEdXPi_P:nSigmadEdXK_P:nSigmadEdXP_P:pt_P:chi2Primary_P:nHFTHits_K:nTPCHits_K:m2ToF_K:nSigmadEdXPi_K:nSigmadEdXK_K:nSigmadEdXP_K:pt_K:chi2Primary_K:nHFTHits_Pi:nTPCHits_Pi:m2ToF_Pi:nSigmadEdXPi_Pi:nSigmadEdXK_Pi:nSigmadEdXP_Pi:pt_Pi:chi2Primary_Pi:Chi2NDF:LdL:Chi2Topo"};
  
    TFile* curFile = gFile;
    TDirectory* curDirectory = gDirectory;
    for(int iNtuple=0; iNtuple<fNNTuples; iNtuple++)
    {
      TString SignalPrefix = "_Signal";
      if(!fProcessSignal) SignalPrefix = "_BG";
      TString currentNTupleFileName = ntupleNames[iNtuple]+SignalPrefix+TString(".root");
      fNTupleFile[iNtuple] = new TFile(currentNTupleFileName.Data(),"RECREATE");
      fCutsNTuple[iNtuple] = new TNtuple(ntupleNames[iNtuple].Data(), ntupleNames[iNtuple].Data(), cutNames[iNtuple].Data());
    }
    gFile = curFile;
    gDirectory = curDirectory;
  }
  return kStOK;
}
//________________________________________________________________________________
Int_t StMuMcAnalysisMaker::Finish() {
  if(IAttr("StoreCutNTuples"))
  {
    TFile* curFile = gFile;
    TDirectory* curDirectory = gDirectory;
    for(int iNtuple=0; iNtuple<fNNTuples; iNtuple++)
    {
      fNTupleFile[iNtuple]->cd();
      fCutsNTuple[iNtuple]->Write();
    }
    gFile = curFile;
    gDirectory = curDirectory;
  }
  return kStOK;
}
//________________________________________________________________________________
Int_t StMuMcAnalysisMaker::InitRun(Int_t runumber) {
  assert(StMuDstMaker::instance());
  if (StMuDstMaker::instance()->IOMode() == StMuDstMaker::ioRead) {
    StMuDstMaker::instance()->SetStatus("*",0);
    const Char_t *ActiveBranches[] = {
      "MuEvent"
      ,"PrimaryVertices"
      ,"PrimaryTracks"
      ,"GlobalTracks"
      ,"StStMuMcVertex"
      ,"StStMuMcTrack"
      ,"CovPrimTrack"
      ,"CovGlobTrack"
      ,"StStMuMcVertex"
      ,"StStMuMcTrack"
      ,"KFTracks"
      ,"KFVertices"
      ,"StBTofHit"
      ,"StBTofHeader"
    }; 
    Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
    for (Int_t i = 0; i < Nb; i++) StMuDstMaker::instance()->SetStatus(ActiveBranches[i],1); // Set Active braches
  }
  return StMaker::InitRun(runumber);
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::PrintMem(const Char_t *opt){
  MemInfo_t info;
  gSystem->GetMemInfo(&info);
  cout << opt 
       << "\tMemory : Total = " << info.fMemTotal 
       << "\tUsed = " << info.fMemUsed
       << "\tFree = " << info.fMemFree
       << "\tSwap Total = " << info.fSwapTotal
       << "\tUsed = " << info.fSwapUsed
       << "\tFree = " << info.fSwapFree << endl;
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::BookTrackPlots(){
#if 1
  enum {npT    = 108};
  //  Double_t pTMax =   10;
  const Double_t ptBins[npT+1] = {
    0.07, 0.08, 0.11, 0.14, 0.16, 0.17, 0.19, 0.21, 0.22, 0.23,
    0.24, 0.26, 0.27, 0.28, 0.29, 0.30, 0.31, 0.32, 0.33, 0.34,
    0.35, 0.36, 0.37, 0.38, 0.39, 0.40, 0.41, 0.42, 0.43, 0.44,
    0.45, 0.46, 0.47, 0.48, 0.49, 0.50, 0.51, 0.53, 0.54, 0.55,
    0.56, 0.57, 0.58, 0.60, 0.61, 0.62, 0.63, 0.65, 0.66, 0.67,
    0.69, 0.70, 0.72, 0.73, 0.75, 0.76, 0.78, 0.80, 0.81, 0.83,
    0.85, 0.87, 0.89, 0.91, 0.93, 0.96, 0.98, 1.01, 1.03, 1.06,
    1.09, 1.12, 1.16, 1.19, 1.23, 1.27, 1.31, 1.35, 1.40, 1.45,
    1.51, 1.57, 1.64, 1.71, 1.80, 1.89, 2.00, 2.11, 2.24, 2.39,
    2.57, 2.78, 3.05, 3.38, 3.80, 4.28, 4.96, 5.88, 7.25,10.00, 
    15.0, 20.0, 25.0, 30., 35., 40., 50., 60., 100.
  };
#else
  Int_t    npT    = 100;
  TArrayD PtBins(npT+1);
  Double_t *ptBins = PtBins.GetArray();
  ptBins[0] = 0;
  for (Int_t i = 1; i <= npT; i++) ptBins[i] = i;
#endif
  enum {nphiM = 10};
  Double_t dphiMask[nphiM] = {0,  5., 10., 12., 14., 15., 16., 18., 20., 25.};
  Int_t    nphi   = 12*nphiM;
  TArrayD PhiBins(nphi+1);
  Double_t *phiBins = PhiBins.GetArray();
  Int_t i = 0;
  for (Int_t sec = 0; sec < 12; sec++) {
    Double_t phi = -180 + 30*sec;
    for (Int_t j = 0; j < nphiM; j++, i++) {
      phiBins[i] = phi +  dphiMask[j];
    }
  }
  phiBins[nphi] = 180.;
  Int_t    neta   = 100; 
  Double_t etamax = 2.5;
  Double_t deta = 2*etamax/neta;
  TArrayD EtaBins(neta+1);
  Double_t *etaBins = EtaBins.GetArray();
  for (i = 0; i <= neta; i++) {etaBins[i] = -etamax + deta*i;}
  PrintMem("");

  TDirectory *dirs[7] = {0};
  dirs[0] = TDirectory::CurrentDirectory(); assert(dirs[0]);
  dirs[0]->cd();
  if (! dirs[0]->GetDirectory(TracksVertices[0])) {
    dirs[0]->mkdir(TracksVertices[0]);
  }
  dirs[1] = dirs[0]->GetDirectory(TracksVertices[0]); assert(dirs[1]);
  dirs[1]->cd();
  for (Int_t gp = kGlobal; gp < kTotalT; gp++) {// TODO
    const  PlotName_t plotNameMatch[kTotalTkTypes] = {
      {kMcTk,       "Mc",       "Mc tracks All"},                        
      {kMcTpcTk,    "Tpc",     Form("Mc tracks which have >= %i Mc Tpc Hits",StMuDst::MinNoTpcMcHits)},        
      {kRecoTk,     "Rec",     "Rc tracks matched with only Mc track"},                
      {kCloneTk,    "Clone",   "Mc tracks matched with > 1 Rc track (Clone)"},             
      {kGhostTk,    "Ghost",   "Rc tracks without Mc partner"},                  
      {kLostTk,     "Lost",    "Mc tracks without reconstructed one"},               
      {kMcToFTk,    "ToF",     Form("Mc tracks which have >= %i Mc Tpc and > 0 ToF Hits",StMuDst::MinNoTpcMcHits)},
      {kRecoToFTk,  "RecToF",   "Rc tracks matched with only Mc track with ToF"},
      {kGhostToFTk, "GhostToF", "Rc tracks without Mc partner with ToF"},
      {kLostToFTk,  "LostToF",  "Mc tracks without reconstructed one in ToF"},
      {kMcHftTk,    "Hft",      "Mc tracks with HFT"},
      {kRecoHftTk,  "RecHft",   "Rc tracks matched with only Mc track with HFT"},
      {kGhostHftTk, "GhostHft", "Rc tracks with HFT without Mc partner with HFT"},
      {kLostHftTk,  "LostHft",  "Mc tracks without reconstructed one in Hft"}
    };
    if (! dirs[1]->GetDirectory(TitleTrType[gp])) {
      dirs[1]->mkdir(TitleTrType[gp]);
    }
    dirs[2] = dirs[1]->GetDirectory(TitleTrType[gp]); assert(dirs[2]);
    dirs[2]->cd();
    PrintMem(dirs[2]->GetPath());
    for (Int_t t = kMcTk; t < kTotalTkTypes; t++) {
      TrackMatchType type = plotNameMatch[t].k;
      if (! dirs[2]->GetDirectory(plotNameMatch[t].Name)) {
 dirs[2]->mkdir(plotNameMatch[t].Name);
      }
      dirs[3] = dirs[2]->GetDirectory(plotNameMatch[t].Name); assert(dirs[3]);
      dirs[3]->cd();
      const Char_t *ParticleType[2] = {"All","Pion"};
      for (Int_t particle = 0; particle < kPartypeT; particle++) {
 if ((type == kGhostTk || type == kGhostHftTk) && particle != kallP) continue;
 if (! dirs[3]->GetDirectory(ParticleType[particle])) {
   dirs[3]->mkdir(ParticleType[particle]);
 }
 dirs[4] = dirs[3]->GetDirectory(ParticleType[particle]); assert(dirs[4]);
 dirs[4]->cd();
 for (Int_t pm = kPositive; pm < kTotalSigns; pm++) {
   if (! dirs[4]->GetDirectory(TitleCharge[pm])) {
     dirs[4]->mkdir(TitleCharge[pm]);
   }
   dirs[5] = dirs[4]->GetDirectory(TitleCharge[pm]); assert(dirs[5]);
   dirs[5]->cd();
   const Char_t *VarSet[kVariables] = {"NoHits","EtapT"};
   for (Int_t x = 0; x < kVariables; x++) {
     if (x == 0 && type != kRecoTk) continue;
     if (! dirs[5]->GetDirectory(VarSet[x])) {
       dirs[5]->mkdir(VarSet[x]);
     }
     dirs[6] = dirs[5]->GetDirectory(VarSet[x]); assert(dirs[6]);
     dirs[6]->cd();
     //                /GlobalTracks/Mc/All/(+)/NoHits
     TString dir(Form("/%s/%s/%s/%s/%s/%s",TracksVertices[0],TitleTrType[gp],
          plotNameMatch[t].Name,ParticleType[particle],TitleCharge[pm],VarSet[x]));
     const VarName_t plotVar[kTotalQAll] = {         //no.fit                      quality,                               
       {"ChiSqXY",   "#chi^{2}_{Track}/NDF",          noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50,  0.,  10., 0.000, 6.000, 1},
       {"ChiSqZ",    "#chi^{2}_{Vx} ",                noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50,  0., 100., 0.000,10.000,-1},
       {"dDcaXY",    "difference in Dca_{XY}",        noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -5.,   5., -.250, 1.500, 0},
       {"dDcaZ",     "difference in Dca_{Z}",         noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -5.,   5., -.250, 1.500, 0},
       {"dPsi",      "difference in  #Psi ",          noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -0.1, 0.1, -.004, 0.040, 1},
       {"dPti" ,     "difference in q/pT",            noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -0.1, 0.1, -.020, 0.200, 1},
       {"dPtiR" ,    "difference in relative q/pT",   noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -0.1, 0.1, -.004, 0.040, 1},
       {"dTanL",     "difference in tan( #lambda )",  noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -0.1, 0.1, -.004, 0.040, 1},
       {"deta",      "difference in  #eta",           noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -0.1, 0.1, -.002, 0.025,-1},
       {"pDcaXY",    "pull in Dca_{XY}",              noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -10., 10., -.300, 3.000, 0},
       {"pDcaZ",     "pull in Dca_{Z}",               noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -10., 10., -.100,10.000, 0},
       {"pPsi",      "pull in  #Psi ",                noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -10., 10., -.300, 3.000, 1},
       {"pPti" ,     "pull in q/pT",                  noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -10., 10., -.250, 2.500, 1},
       {"pPtiR" ,    "pull in relative q/pT",         noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -10., 10., -.230, 3.500, 1},
       {"pTanL",     "pull in tan( #lambda )",        noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -10., 10., -.600, 6.000, 0},
       {"peta",      "pull for  #eta",                noFit-9, 9.5, noFit + 0.5, 22,-5, 105,  50, -10., 10.,-1.000,10.000,-1},
       {"Phi",       "#phi (degrees)",                      0,   0,           0,  0,   0,    0,  60,-180.,180.,-1.000,10.000, 1}
     };
     Int_t i1 = 0;
     if (type != kRecoTk) i1 = kTotalQA;
     for (Int_t i = i1; i < kTotalQAll; i++) {
       if (gp == kGlobal && plotVar[i].GlobalOnly <  0) continue;
       if (gp == kPrimary && plotVar[i].GlobalOnly == 0) continue;
       if ((fHistsT[gp][type][particle][pm][x][i] = (TH3F *) dirs[6]->Get(plotVar[i].Name))) continue;
       if (! x) {// No.Hits
   if (i == kTotalQA) continue;
   fHistsT[gp][type][particle][pm][x][i] = new TH3F(plotVar[i].Name,
                Form("%s for %s %s %s %s %s", plotVar[i].Title,
                     TitleTrType[gp],plotNameMatch[t].Name,ParticleType[particle],TitleCharge[pm],VarSet[x]),
                noFit-9, 9.5, noFit + 0.5, 22,-5, 105,
                plotVar[i].nz, plotVar[i].zmin, plotVar[i].zmax);
   fHistsT[gp][type][particle][pm][x][i]->GetXaxis()->SetTitle("No. of Fit Points");
   fHistsT[gp][type][particle][pm][x][i]->GetYaxis()->SetTitle("Quality");
   fHistsT[gp][type][particle][pm][x][i]->GetZaxis()->SetTitle(plotVar[i].Title);
   fHistsT[gp][type][particle][pm][x][i]->SetMarkerColor(pm+1); 
   fHistsT[gp][type][particle][pm][x][i]->SetLineColor(pm+1); 
   //    for (Int_t l = 1; l < 7; l++) cout << "/" << dirs[l]->GetName();
   //    cout << "\t" << "fHistsT[" << gp << "][" << type << "][" << particle << "][" << pm << "][" << x << "][" << i << "]";
   //    cout << " = " << fHistsT[gp][type][particle][pm][x][i]->GetName() << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetTitle() << endl;
       } else { // eta & pT
   if (i != kTotalQA) {
     TArrayD ZBins(plotVar[i].nz+1);
     Double_t *zBins = ZBins.GetArray();
     Double_t dz = (plotVar[i].zmax - plotVar[i].zmin)/plotVar[i].nz;
     for (Int_t j = 0; j <= plotVar[i].nz; j++) zBins[j] = plotVar[i].zmin + dz*j;
     fHistsT[gp][type][particle][pm][x][i] = new TH3F(plotVar[i].Name,
                  Form("%s for %s %s %s %s %s", plotVar[i].Title,
                     TitleTrType[gp],plotNameMatch[t].Name,ParticleType[particle],TitleCharge[pm],VarSet[x]),
                  neta, etaBins,
                  npT, ptBins,
                  plotVar[i].nz, zBins);
   } else {
     fHistsT[gp][type][particle][pm][x][i] = new TH3F(plotVar[i].Name,
                  Form("%s for %s %s %s %s %s", plotVar[i].Title,
                 TitleTrType[gp],plotNameMatch[t].Name,ParticleType[particle],TitleCharge[pm],VarSet[x]),
                  neta, etaBins,
                  npT, ptBins,
                  nphi, phiBins);
   }
   fHistsT[gp][type][particle][pm][x][i]->GetXaxis()->SetTitle("  #eta");
   fHistsT[gp][type][particle][pm][x][i]->GetYaxis()->SetTitle("pT/|q| (GeV/c)");
   
   fHistsT[gp][type][particle][pm][x][i]->GetZaxis()->SetTitle(plotVar[i].Title);
   fHistsT[gp][type][particle][pm][x][i]->SetMarkerColor(pm+1); 
   fHistsT[gp][type][particle][pm][x][i]->SetLineColor(pm+1); 
   //    for (Int_t l = 1; l < 7; l++) cout << "/" << dirs[l]->GetName();
   //    cout << "\t" << "fHistsT[" << gp << "][" << type << "][" << particle << "][" << pm << "][" << x << "][" << i << "]";
   //    cout << " = " << fHistsT[gp][type][particle][pm][x][i]->GetName() << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetTitle() << endl;
       }
       //    for (Int_t l = 1; l < 7; l++) cout << "/" << dirs[l]->GetName();
       //    cout << "\t" << "fHistsT[" << gp << "][" << type << "][" << particle << "][" << pm << "][" << x << "][" << i << "]";
       //    cout << " = " << fHistsT[gp][type][particle][pm][x][i]->GetName() << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetTitle() << endl;
       ofstream out;
       TString Out("Hist.list");
       if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
       else                              out.open(Out, ios::app);
       out << "\t" << "fHistsT[" << gp << "][" << type << "][" << particle << "][" << pm << "][" << x << "][" << i << "]";
       out << " = " << fHistsT[gp][type][particle][pm][x][i]->GetName() << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetTitle() 
     << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetDirectory()->GetPath() << endl;
       out.close();
     }
   }
 }
      }
    }
  }
  dirs[1] = dirs[0]->GetDirectory(TracksVertices[0]); assert(dirs[1]);
  dirs[1]->cd(); 
  if (IAttr("PiDPlots")) {
  // PiD block
  // dE/dx and ToF block for matched global and  primary tracks
  for (Int_t gp = kGlobal; gp < kTotalT; gp++) { // TODO
    if (! dirs[1]->GetDirectory(TitleTrType[gp])) {
      dirs[1]->mkdir(TitleTrType[gp]);
    }
    dirs[2] = dirs[1]->GetDirectory(TitleTrType[gp]); assert(dirs[2]);
    dirs[2]->cd();
    PrintMem(dirs[2]->GetPath());
    for (Int_t pidType = 0; pidType < NoPiDs; pidType++) {// 
      if (! dirs[2]->GetDirectory(TitlePiDtype[pidType])) {
 dirs[2]->mkdir(TitlePiDtype[pidType]);
      }
      dirs[3] = dirs[2]->GetDirectory(TitlePiDtype[pidType]);
      dirs[3]->cd();
      for (Int_t pm = kPositive; pm < kTotalSigns; pm++) {
 if (! dirs[3]->GetDirectory(TitleCharge[pm])) {
   dirs[3]->mkdir(TitleCharge[pm]);
 }
 dirs[4] = dirs[3]->GetDirectory(TitleCharge[pm]); assert(dirs[4]);
 dirs[4]->cd();
 for (Int_t hyp = 0; hyp < NHypTypes; hyp++) {
   Int_t h = hyp;
   if (pm == kPositive) h += NHypTypes;
   if (! dirs[4]->GetDirectory(NamesF[h])) {
     dirs[4]->mkdir(NamesF[h]);
   }
   dirs[5] = dirs[4]->GetDirectory(NamesF[h]); assert(dirs[5]);
   dirs[5]->cd();
   if (pidType == 0) { // dE/dx
     const Char_t *dEdxTypes[NdEdxPiD] = {"I70","Fit","dNdx"};
     for (i = 0; i < NdEdxPiD; i++) {
       LdEdx[gp][hyp][pm][i] = (TH3F *) dirs[5]->Get(Form("Z%s",dEdxTypes[i]));
       if (LdEdx[gp][hyp][pm][i]) continue;
       LdEdx[gp][hyp][pm][i] = new TH3F(Form("Z%s",dEdxTypes[i]),
                Form(" z_{%s}  versus TpcTrackLength and log_{10} (#beta #gamma) for %s %s",
               dEdxTypes[i],TitleTrType[gp],NamesF[h]),
                 50, 20, 220, 292,-1.6, 5.7, 200, -0.5, 0.5); 
       LdEdx[gp][hyp][pm][i]->GetXaxis()->SetTitle("TpcTrackLength (cm)");
       LdEdx[gp][hyp][pm][i]->GetYaxis()->SetTitle("log_{10} (#beta #gamma)");         
       LdEdx[gp][hyp][pm][i]->GetZaxis()->SetTitle(Form(" z_{%s}",dEdxTypes[i]));   
       LdEdx[gp][hyp][pm][i]->SetMarkerColor(pm+1);
       LdEdx[gp][hyp][pm][i]->SetLineColor(pm+1);
       TString Out("Hist.list");
       if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
       else                              out.open(Out, ios::app);
       out << "\t" << "LdEdx[" << gp << "][" << hyp << "][" << pm << "][" << i << "]";
       out << " = " << LdEdx[gp][hyp][pm][i]->GetName() << "\t" << LdEdx[gp][hyp][pm][i]->GetTitle() 
     << "\t" << LdEdx[gp][hyp][pm][i]->GetDirectory()->GetPath() << endl;
       out.close();
     }
   } else {// Tof
     const Char_t *ToFTypes[NToFPiD] = {"dM2","dBetaInv"};
     Int_t nz = 200;
     Double_t zmax =  0.7;
     Double_t zmin = -0.3;
     TArrayD ZBins(nz+1);
     Double_t *zBins = ZBins.GetArray();
     Double_t dz = (zmax - zmin)/nz;
     for (Int_t j = 0; j <= nz; j++) zBins[j] = zmin + dz*j;
     for (i = 0; i < NToFPiD; i++) {
       LToF[gp][hyp][pm][i] = (TH3F *) dirs[5]->Get(ToFTypes[i]);
       if (LToF[gp][hyp][pm][i]) continue;
       if (i == 0) { // dM2
   LToF[gp][hyp][pm][i] = new TH3F(ToFTypes[i],Form("#Delta M^2 versus #eta and p for %s %s",
                TitleTrType[gp],NamesF[h]),
           neta, etaBins,
           npT, ptBins,
           nz, zBins);
   LToF[gp][hyp][pm][i]->GetZaxis()->SetTitle("#Delta M^2"); 
       } else { // (1/beta^2 - 1/beta_exp^2)/beta^2
   LToF[gp][hyp][pm][i] = new TH3F(ToFTypes[i],Form("#delta 1/#beta versus #eta and p for %s %s ",
                TitleTrType[gp],NamesF[h]),
           neta, etaBins,
           npT, ptBins,
           nz, zBins);
       }
       LToF[gp][hyp][pm][i]->SetMarkerColor(pm+1);
       LToF[gp][hyp][pm][i]->SetLineColor(pm+1);
       LToF[gp][hyp][pm][i]->GetXaxis()->SetTitle("#eta");
       LToF[gp][hyp][pm][i]->GetYaxis()->SetTitle("p[GeV/c]");         
       LToF[gp][hyp][pm][i]->GetZaxis()->SetTitle(Form(" z_{%s}",ToFTypes[i]));   
       TString Out("Hist.list");
       if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
       else                              out.open(Out, ios::app);
       out << "\t" << "LToF[" << gp << "][" << hyp << "][" << pm << "][" << i << "]";
       out << " = " << LToF[gp][hyp][pm][i]->GetName() << "\t" << LToF[gp][hyp][pm][i]->GetTitle() 
     << "\t" << LToF[gp][hyp][pm][i]->GetDirectory()->GetPath() << endl;
       out.close();
     }
   }
 }
      }
    }
  }
  }
  PlotName_t geant[4] = {
    {kNotDefined, "GiD",    "Geant ID for all MC tracks"},
    {kNotDefined, "GiDG",   Form("Geant ID for MC tracks with >= %i Tpc MC hits",StMuDst::MinNoTpcMcHits)},
    {kNotDefined, "GiDPr", "Geant ID for all primary MC tracks"},
    {kNotDefined, "GiDPrG", Form("Geant ID for primary MC tracks with >= %i Tpc MC hits",StMuDst::MinNoTpcMcHits)}
  };
  for (Int_t i = 0; i < 4; i++) {
    GiD[i] = (TH1F *)  dirs[1]->Get(geant[i].Name);
    if (! GiD[i]) {
      dirs[1]->cd();
      GiD[i] = new TH1F(geant[i].Name,geant[i].Title, 50,0.5,50.5); 
      GiD[i]->SetMarkerColor(i%2+1);
      GiD[i]->SetLineColor(i%2+1);
      SetGEANTLabels(GiD[i]->GetXaxis());
    }
  }
  McRcHit = (TH2F *)   dirs[1]->Get("McRcHit");
  if (! McRcHit) McRcHit = new TH2F("McRcHit","No. RC hits in TPC versus No. MC ones",80,-0.5,79.5,80,-0.5,79.5);
  
  hdEdX = (TH2F *)   dirs[1]->Get("hdEdX");
  if (! hdEdX) hdEdX = new TH2F("hdEdX", "hdEdX", 1000, 0, 5, 1000, 0, 10);

  hdEdXwithToF = (TH2F *)   dirs[1]->Get("hdEdXwithToF");
  if (! hdEdXwithToF) hdEdXwithToF = new TH2F("hdEdXwithToF", "hdEdXwithToF", 1000, 0, 5, 1000, 0, 10);
  
  hTofPID = (TH2F *)   dirs[1]->Get("hTofPID");
  if (! hTofPID) hTofPID = new TH2F("hTofPID", "hTofPID", 300, 0, 15, 1100, -1, 10);

  
  double maxDCA = 1.;
  hDCAp = (TH1F *)   dirs[1]->Get("hDCAp");
  if (! hDCAp) hDCAp = new TH1F("hDCAp", "hDCAp",1000, 0, maxDCA);

  hDCApi = (TH1F *)   dirs[1]->Get("hDCApi");
  if (! hDCApi) hDCApi = new TH1F("hDCApi", "hDCApi",1000, 0, maxDCA);

  hDCAK = (TH1F *)   dirs[1]->Get("hDCAK");
  if (! hDCAK) hDCAK = new TH1F("hDCAK", "hDCAK",1000, 0, maxDCA);

  hHFTDCAp = (TH1F *)   dirs[1]->Get("hHFTDCAp");
  if (! hHFTDCAp) hHFTDCAp = new TH1F("hHFTDCAp", "hHFTDCAp",1000, 0, maxDCA);

  hHFTDCApi = (TH1F *)   dirs[1]->Get("hHFTDCApi");
  if (! hHFTDCApi) hHFTDCApi = new TH1F("hHFTDCApi", "hHFTDCApi",1000, 0, maxDCA);

  hHFTDCAK = (TH1F *)   dirs[1]->Get("hHFTDCAK");
  if (! hHFTDCAK) hHFTDCAK = new TH1F("hHFTDCAK", "hHFTDCAK",1000, 0, maxDCA);
  
  hSignalDCAp = (TH1F *)   dirs[1]->Get("hSignalDCAp");
  if (! hSignalDCAp) hSignalDCAp = new TH1F("hSignalDCAp", "hSignalDCAp",1000, 0, maxDCA);

  hSignalDCApi = (TH1F *)   dirs[1]->Get("hSignalDCApi");
  if (! hSignalDCApi) hSignalDCApi = new TH1F("hSignalDCApi", "hSignalDCApi",1000, 0, maxDCA);

  hSignalDCAK = (TH1F *)   dirs[1]->Get("hSignalDCAK");
  if (! hSignalDCAK) hSignalDCAK = new TH1F("hSignalDCAK", "hSignalDCAK",1000, 0, maxDCA); 
  
  hSignalHFTDCAp = (TH1F *)   dirs[1]->Get("hSignalHFTDCAp");
  if (! hSignalHFTDCAp) hSignalHFTDCAp = new TH1F("hSignalHFTDCAp", "hSignalHFTDCAp",1000, 0, maxDCA);

  hSignalHFTDCApi = (TH1F *)   dirs[1]->Get("hSignalHFTDCApi");
  if (! hSignalHFTDCApi) hSignalHFTDCApi = new TH1F("hSignalHFTDCApi", "hSignalHFTDCApi",1000, 0, maxDCA);

  hSignalHFTDCAK = (TH1F *)   dirs[1]->Get("hSignalHFTDCAK");
  if (! hSignalHFTDCAK) hSignalHFTDCAK = new TH1F("hSignalHFTDCAK", "hSignalHFTDCAK",1000, 0, maxDCA);
  
  hLcdx = (TH1F *)   dirs[1]->Get("hLcdx");
  if (! hLcdx) hLcdx = new TH1F("hLcdx", "hLcdx",1000, -2, 2);
  
  hLcdy = (TH1F *)   dirs[1]->Get("hLcdy");
  if (! hLcdy) hLcdy = new TH1F("hLcdy", "hLcdy",1000, -2, 2);
  
  hLcdz = (TH1F *)   dirs[1]->Get("hLcdz");
  if (! hLcdz) hLcdz = new TH1F("hLcdz", "hLcdz",1000, -2, 2);
  
  hLcdr = (TH1F *)   dirs[1]->Get("hLcdr");
  if (! hLcdr) hLcdr = new TH1F("hLcdr", "hLcdr",1000, 0, 2);
  
  hLcPx = (TH1F *)   dirs[1]->Get("hLcPx");
  if (! hLcPx) hLcPx = new TH1F("hLcPx", "hLcPx",1000, -10, 10);
  
  hLcPy = (TH1F *)   dirs[1]->Get("hLcPy");
  if (! hLcPy) hLcPy = new TH1F("hLcPy", "hLcPy",1000, -10, 10);
  
  hLcPz = (TH1F *)   dirs[1]->Get("hLcPz");
  if (! hLcPz) hLcPz = new TH1F("hLcPz", "hLcPz",1000, -10, 10);
  
  hLcPr = (TH1F *)   dirs[1]->Get("hLcPr");
  if (! hLcPr) hLcPr = new TH1F("hLcPr", "hLcPr",1000, 0, 10);
  
  hFitProb = (TH1F *)   dirs[1]->Get("hFitProb");
  if (! hFitProb) hFitProb = new TH1F("hFitProb", "hFitProb",100, 0, 1);

  hNHFTHits = (TH1F *)   dirs[1]->Get("hNHFTHits");
  if (! hNHFTHits) hNHFTHits = new TH1F("hNHFTHits", "hNHFTHits",11, -0.5, 10.5);
  
  hPVError = (TH1F *)   dirs[1]->Get("hPVError");
  if (! hPVError) hPVError = new TH1F("hPVError", "hPVError", 10000, 0, 1);

  hPVErrorVsNTracks = (TH2F *)   dirs[1]->Get("hPVErrorVsNTracks");
  if (! hPVErrorVsNTracks) hPVErrorVsNTracks = new TH2F("hPVErrorVsNTracks", "hPVErrorVsNTracks", 5000, 0, 5000, 5000, 0, 0.5);

  hPVErrorVsNPVTracks = (TH2F *)   dirs[1]->Get("hPVErrorVsNPVTracks");
  if (! hPVErrorVsNPVTracks) hPVErrorVsNPVTracks = new TH2F("hPVErrorVsNPVTracks", "hPVErrorVsNPVTracks", 5000, 0, 5000, 5000, 0, 0.5);
  

  double maxChiPrim = 1000.;
  hChiPrimHFTp = (TH1F *)   dirs[1]->Get("hChiPrimHFTp");
  if (! hChiPrimHFTp) hChiPrimHFTp = new TH1F("hChiPrimHFTp", "hChiPrimHFTp",10000, 0, maxChiPrim);
  hChiPrimHFTK = (TH1F *)   dirs[1]->Get("hChiPrimHFTK");
  if (! hChiPrimHFTK) hChiPrimHFTK = new TH1F("hChiPrimHFTK", "hChiPrimHFTK",10000, 0, maxChiPrim);
  hChiPrimHFTpi = (TH1F *)   dirs[1]->Get("hChiPrimHFTpi");
  if (! hChiPrimHFTpi) hChiPrimHFTpi = new TH1F("hChiPrimHFTpi", "hChiPrimHFTpi",10000, 0, maxChiPrim);
  
  hChiPrimHFTWithCutp = (TH1F *)   dirs[1]->Get("hChiPrimHFTWithCutp");
  if (! hChiPrimHFTWithCutp) hChiPrimHFTWithCutp = new TH1F("hChiPrimHFTWithCutp", "hChiPrimHFTWithCutp",10000, 0, maxChiPrim);
  hChiPrimHFTWithCutK = (TH1F *)   dirs[1]->Get("hChiPrimHFTWithCutK");
  if (! hChiPrimHFTWithCutK) hChiPrimHFTWithCutK = new TH1F("hChiPrimHFTWithCutK", "hChiPrimHFTWithCutK",10000, 0, maxChiPrim);
  hChiPrimHFTWithCutpi = (TH1F *)   dirs[1]->Get("hChiPrimHFTWithCutpi");
  if (! hChiPrimHFTWithCutpi) hChiPrimHFTWithCutpi = new TH1F("hChiPrimHFTWithCutpi", "hChiPrimHFTWithCutpi",10000, 0, maxChiPrim);
  
  hChiPrimHFTSignalp = (TH1F *)   dirs[1]->Get("hChiPrimHFTSignalp");
  if (! hChiPrimHFTSignalp) hChiPrimHFTSignalp = new TH1F("hChiPrimHFTSignalp", "hChiPrimHFTSignalp",10000, 0, maxChiPrim);
  hChiPrimHFTSignalK = (TH1F *)   dirs[1]->Get("hChiPrimHFTSignalK");
  if (! hChiPrimHFTSignalK) hChiPrimHFTSignalK = new TH1F("hChiPrimHFTSignalK", "hChiPrimHFTSignalK",10000, 0, maxChiPrim);
  hChiPrimHFTSignalpi = (TH1F *)   dirs[1]->Get("hChiPrimHFTSignalpi");
  if (! hChiPrimHFTSignalpi) hChiPrimHFTSignalpi = new TH1F("hChiPrimHFTSignalpi", "hChiPrimHFTSignalpi",10000, 0, maxChiPrim);
  
  hChiPrimHFTSignalWithCutp = (TH1F *)   dirs[1]->Get("hChiPrimHFTSignalWithCutp");
  if (! hChiPrimHFTSignalWithCutp) hChiPrimHFTSignalWithCutp = new TH1F("hChiPrimHFTSignalWithCutp", "hChiPrimHFTSignalWithCutp",10000, 0, maxChiPrim);
  hChiPrimHFTSignalWithCutK = (TH1F *)   dirs[1]->Get("hChiPrimHFTSignalWithCutK");
  if (! hChiPrimHFTSignalWithCutK) hChiPrimHFTSignalWithCutK = new TH1F("hChiPrimHFTSignalWithCutK", "hChiPrimHFTSignalWithCutK",10000, 0, maxChiPrim);
  hChiPrimHFTSignalWithCutpi = (TH1F *)   dirs[1]->Get("hChiPrimHFTSignalWithCutpi");
  if (! hChiPrimHFTSignalWithCutpi) hChiPrimHFTSignalWithCutpi = new TH1F("hChiPrimHFTSignalWithCutpi", "hChiPrimHFTSignalWithCutpi",10000, 0, maxChiPrim);
  
  dirs[0]->cd();
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::BookVertexPlots(){
  TDirectory *dirs[2] = {0};
  dirs[0] = TDirectory::CurrentDirectory(); assert(dirs[0]);
  dirs[0]->cd();
  if (! dirs[0]->GetDirectory(TracksVertices[1])) {
    dirs[0]->mkdir(TracksVertices[1]);
  }
  dirs[1] = dirs[0]->GetDirectory(TracksVertices[1]); assert(dirs[1]);
  dirs[1]->cd();
  PrintMem(dirs[1]->GetPath());
#if 0  
  mStKFParticleInterface = new StKFParticleInterface;
  mStKFParticlePerformanceInterface = new StKFParticlePerformanceInterface(mStKFParticleInterface->GetTopoReconstructor());
#endif
  dirs[0]->cd();
  PrintMem(dirs[1]->GetPath());
}
//_____________________________________________________________________________
Int_t StMuMcAnalysisMaker::Make(){
//   if (! GiD[0]) {
//     LOG_ERROR << "StMuMcAnalysisMaker::Make histograms have not been initialized. Probably you have missed TTree file in bfc parameters" << endm;
//     return kStFatal;
//   }
  StMuDstMaker *muDstMaker = StMuDstMaker::instance();
  if (! muDstMaker) return kStFatal;
  muDst = muDstMaker->muDst();
#if 0
  TObjectSet *muSet = (TObjectSet *) GetDataSet("muDst");
  if (! muSet) return kStFatal;
  muDst = (StMuDst *) muSet->GetObject();
#endif
  if (! muDst) return kStOK;
    if (Debug()) {
      muDst->Print();
      muDst->printVertices();
      muDst->printPrimaryTracks();
      muDst->printGlobalTracks();
      muDst->printKFVertices();
      muDst->printKFTracks();
      muDst->printMcVertices();
      muDst->printMcTracks();
    }
  if (IAttr("TrackPlots"))  FillTrackPlots();
  if (IAttr("VertexPlots")) FillVertexPlots();
  return kStOK;
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::FillTrackPlots()
{
#if 1
  if (! muDst || ! muDst->event()) return;
  const Double_t field = muDst->event()->magneticField()*kilogauss;
  //  map<Int_t,Int_t> &IdGlobal2IdPrimaryTrack = muDst->IdGlobal2IdPrimaryTrack(); // map global to primary track Ids from vertex with idTruth == 1
  //  multimap<Int_t,Int_t> &IdMc2IdRcTracks = muDst->IdMc2IdRcTracks(); // map between global and Mc tracks from primary Mc vertex
  // =============  Build map between  Rc and Mc vertices 
  //  multimap<Int_t,Int_t> Mc2RcVertices = muDst->IdMc2IdRcVertices(); // Reconstructable !
  // Loop over Mc Tracks
  for (UInt_t m = 0; m < muDst->numberOfMcTracks(); m++) {
    StMuMcTrack *mcTrack = muDst->MCtrack(m);
    if (! mcTrack) continue;
    // Select only Triggered Mc Vertex
    UInt_t IdVx = mcTrack->IdVx();
    if(IdVx > muDst->numberOfMcVertices()) continue;
    if(IdVx < 1) continue;
    while (IdVx != 1) { // Find parent vertex 
      StMuMcVertex *mcVertex = muDst->MCvertex(IdVx-1);
      Int_t idMcTrack = mcVertex->IdParTrk();
      if (! idMcTrack) break;
      StMuMcTrack *mcTrackP = muDst->MCtrack(idMcTrack-1);
      IdVx = mcTrackP->IdVx();
      if (! IdVx) break;
    }
    if (IdVx != 1) continue; // original vertex 
    IdVx = mcTrack->IdVx(); // for the track
    Bool_t McTpc = mcTrack->No_tpc_hit() >= StMuDst::MinNoTpcMcHits;
    Bool_t McHft = mcTrack->No_pix_hit() >= 2 && mcTrack->No_ist_hit()+mcTrack->No_ssd_hit() >= 1;
    Bool_t McToF = mcTrack->No_tof_hit() > 0;
    GiD[0]->Fill(mcTrack->GePid());
    if (McTpc) GiD[1]->Fill(mcTrack->GePid());
    if (IdVx == 1) {
      GiD[2]->Fill(mcTrack->GePid());
      if (McTpc) GiD[3]->Fill(mcTrack->GePid());
    }
    if (! mcTrack->Charge()) continue;
    EChargeType pm = kPositive;
    if (mcTrack->Charge() < 0) pm = kNegative;
    Int_t NPart = kallP;
    if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) NPart = kPion;
    Double_t eta = mcTrack->Pxyz().pseudoRapidity();
    Double_t pT  = mcTrack->Pxyz().perp();
    Double_t phi = TMath::RadToDeg()*mcTrack->Pxyz().phi();
    Int_t gp2 = kGlobal;
    if (IdVx == 1) gp2 = kPrimary; 
    pair<multimap<StMuMcTrack*,StMuTrack*>::iterator,multimap<StMuMcTrack*,StMuTrack*>::iterator> McTk2RcTk;
    pair<multimap<StMuMcTrack*,KFParticle*>::iterator, multimap<StMuMcTrack*,KFParticle*>::iterator> McTk2KFTk;
    for (Int_t gp = kGlobal; gp <= gp2; gp++) {
      if (gp == kGlobal) McTk2RcTk = muDst->McTrack2GlobalTrack().equal_range(mcTrack);
      else               McTk2RcTk = muDst->McTrack2PrimaryTrack().equal_range(mcTrack);
      Int_t count = 0;
      Int_t countHft = 0;
      Int_t countToF = 0;
      StMuTrack *Track = 0;
      for (auto it = McTk2RcTk.first; it != McTk2RcTk.second; ++it, ++count) {
	auto track = (*it).second; 
	if (! track) continue;
	if (! Track) Track = track;
	StTrackTopologyMap topologyMap = track->topologyMap();
	UInt_t noPxlHits = topologyMap.numberOfHits(kPxlId); // 0-3
	UInt_t noIstHits = topologyMap.numberOfHits(kIstId); // 0-2
	UInt_t noSsdHits = topologyMap.numberOfHits(kSsdId); // 0-2
	//  UInt_t noHftHits = noPxlHits + noIstHits + noSsdHits;
	if (noPxlHits >= 2 && noIstHits + noSsdHits >= 1) {countHft++; Track = track;}
      }
      if (Track && Track->btofPidTraits().matchFlag()) {countToF++;}
      // kNotDefined, kLostTk, kRecoTk, kCloneTk
      TrackMatchType typeTpc = kNotDefined;
      TrackMatchType typeHft = kNotDefined;
      TrackMatchType typeToF = kNotDefined;
      if (! count) { 
	typeTpc =  kLostTk;
      } else if (count == 1) {
	typeTpc = kRecoTk;
	if (McToF) {
	  if (! countToF) typeToF = kLostToFTk;
	  else            typeToF = kRecoToFTk;
	} else {
	  typeToF = kGhostToFTk;
	}
	if (McHft) {
	  if (! countHft) typeHft = kLostHftTk;
	  else            typeHft = kRecoHftTk;
	} else {
	  if (countHft)   typeHft = kGhostHftTk;
	}
      } else {
	typeTpc = kCloneTk;
      }
      for (Int_t particle = 0; particle <= NPart; particle++) {
	fHistsT[gp][kMcTk][particle][pm][1][kTotalQA]->Fill(eta,pT, phi);
	if (! McTpc) continue; 
	fHistsT[gp][kMcTpcTk][particle][pm][1][kTotalQA]->Fill(eta,pT, phi);
	if (typeTpc == kNotDefined) continue;
	if (typeTpc == kGhostTk && particle) continue;
	fHistsT[gp][typeTpc][particle][pm][1][kTotalQA]->Fill(eta,pT, phi);
	if (! McToF) {
	  fHistsT[gp][kMcToFTk][particle][pm][1][kTotalQA]->Fill(eta,pT, phi);
	  if (typeToF != kNotDefined && !( typeToF == kGhostToFTk && particle) )
	    fHistsT[gp][typeToF][particle][pm][1][kTotalQA]->Fill(eta,pT, phi);
	}
	if (! McHft) {
	  fHistsT[gp][kMcHftTk][particle][pm][1][kTotalQA]->Fill(eta,pT, phi);
	  if (typeHft != kNotDefined && !( typeHft == kGhostHftTk && particle) ) 
	    fHistsT[gp][typeHft][particle][pm][1][kTotalQA]->Fill(eta,pT, phi);
	}
      }
#define __NoOfPoint__
#ifdef __NoOfPoint__
      if ((typeTpc == kRecoTk || typeHft == kRecoHftTk) && Track) {
	assert(count == 1);
	// Track QA
	if (gp == kGlobal) {
	  StMuTrack *gTrack = Track;
	  Int_t kgc = gTrack->index2Cov();
	  if (kgc < 0) continue;
	  StDcaGeometry *dcaG = (StDcaGeometry *) muDst->covGlobTrack()->UncheckedAt(kgc);
	  StMuMcVertex *mcVertex = muDst->MCvertex(IdVx-1);
	  if (typeTpc == kRecoTk)    FillQAGl(typeTpc,gTrack, mcTrack, dcaG, mcVertex);
	  if (typeHft == kRecoHftTk) FillQAGl(typeHft,gTrack, mcTrack, dcaG, mcVertex);
	  if (typeToF == kRecoToFTk) FillQAGl(typeToF,gTrack, mcTrack, dcaG, mcVertex);
	} else { // gp = kPrimary
	  StMuTrack *pTrack = Track;
	  Int_t kpc = pTrack->index2Cov();
	  if (kpc >= 0) {
	    StMuPrimaryTrackCovariance *cov = (StMuPrimaryTrackCovariance *) muDst->covPrimTrack()->UncheckedAt(kpc);
	    if (typeTpc == kRecoTk)    FillQAPr(typeTpc, pTrack, mcTrack, cov);
	    if (typeHft == kRecoHftTk) FillQAPr(typeHft, pTrack, mcTrack, cov);
	    if (typeToF == kRecoToFTk) FillQAPr(typeToF, pTrack, mcTrack, cov);
	  } else {
	    McTk2KFTk = muDst->McTrack2KFParticle().equal_range(mcTrack);
	    KFParticle *kfp = 0;
	    for (auto it = McTk2KFTk.first; it != McTk2KFTk.second; ++it, ++count) {
	      auto *p = (*it).second; 
	      if (! p) continue;
	      if (p->Id() == pTrack->id()) {
		kfp = p;
		break;
	      }
	      if (typeTpc == kRecoTk)    FillQAPr(typeTpc,pTrack, mcTrack, kfp);
	      if (typeHft == kRecoHftTk) FillQAPr(typeHft,pTrack, mcTrack, kfp);
	      if (typeToF == kRecoToFTk) FillQAPr(typeToF,pTrack, mcTrack, kfp);
	    }
	  }
	}
	if (IAttr("PiDPlots")) {
	  // dE/dx && ToF block
	  StThreeVectorD momentum = Track->helix().momentum(field);
	  Double_t pMomentum = momentum.mag();
	  Double_t Eta = momentum.pseudoRapidity();
	  const StMuProbPidTraits &PiD = Track->probPidTraits();
	  Double_t I[3] = {PiD.dEdxTruncated(), PiD.dEdxFit(), PiD.dNdxFit()};
	  Double_t TrackLength = PiD.dEdxTrackLength();
	  Int_t Gid = mcTrack->GePid();
	  // ToF
	  const StMuBTofPidTraits &btofPid = Track->btofPidTraits();
	  Float_t pathLength = btofPid.pathLength();
	  //  Float_t timeOfFlight = btofPid.timeOfFlight();
	  Float_t beta = btofPid.beta();
	  
	  //  const StThreeVectorF &pVx  = Track->momentum();
	  for (Int_t h = 0; h < NHYPS; h++) {
	    if (GEANTiD[h] == Gid) {
	      Int_t hyp = PiDHyp[h];
	      Int_t pm  = PiDpm[h];
	      Double_t bg    = pMomentum/Masses[h];
	      Double_t bghyp = TMath::Log10(bg);
	      if (TrackLength > 0) {
		Double_t Pred[3]  = {1.e-6*Bichsel::Instance()->GetI70(bghyp,1.0),
				     1.e-6*TMath::Exp(Bichsel::Instance()->GetMostProbableZ(bghyp,1.0)),
				     StdEdxModel::instance()->dNdx(bg)
		};
		for (Int_t mm = 0; mm < 3; mm++) {
		  if (I[mm] <= 0 || Pred[mm] <= 0) continue;
		  Double_t z = TMath::Log(I[mm]/Pred[mm]);
		  LdEdx[gp][hyp][pm][mm]->Fill(TrackLength, bghyp, z);
		}
	      }
	      if (pathLength > 0) {
		Double_t bg2 = beta*beta/(1. - beta*beta);
		Double_t dM2 = pMomentum*pMomentum/bg2 - Masses[h]*Masses[h];
		Double_t b_exp = bg/TMath::Sqrt(1+ bg*bg);
		Double_t dbInv = 1. - beta/b_exp; // (1./beta - 1./b_exp)/(1./beta)
		LToF[gp][hyp][pm][0]->Fill(Eta,pMomentum, dM2);
		LToF[gp][hyp][pm][1]->Fill(Eta,pMomentum, dbInv);
	      }
	      break;
	    }
	  }
	}
      }
#endif /* __NoOfPoint__ */
    }
  }
  // check for ghost
  for (UInt_t kg = 0; kg < muDst->numberOfGlobalTracks(); kg++) {
    StMuTrack *gTrack = muDst->globalTracks(kg);
    if ( ! muDst->Accept(gTrack)) continue;
    if ( gTrack->idTruth()) continue;
    EChargeType pm = kPositive;
    if (gTrack->charge() < 0) pm = kNegative;
    fHistsT[kGlobal][kGhostTk][kallP][pm][1][kTotalQA]->Fill(gTrack->eta(),(gTrack->charge()*gTrack->pt()),TMath::RadToDeg()*gTrack->phi());
  }
  for (Int_t l = 0; l < (Int_t) muDst->numberOfPrimaryVertices(); l++) {
    StMuPrimaryVertex *Vtx = muDst->primaryVertex(l);
    if (Vtx->idTruth() != 1) continue;
    for (UInt_t k = 0; k < muDst->numberOfPrimaryTracks(); k++) {
      StMuTrack *pTrack = (StMuTrack *) muDst->array(muPrimary)->UncheckedAt(k);
      if (! pTrack) continue;
      if (pTrack->vertexIndex() != l) continue;
      if (! muDst->Accept(pTrack)) continue;
      if ( pTrack->idTruth()) {
  if (pTrack->idParentVx() == 1) continue;
      }
      EChargeType pm = kPositive;
      if (pTrack->charge() < 0) pm = kNegative;
      fHistsT[kPrimary][kGhostTk][kallP][pm][1][kTotalQA]->Fill(pTrack->eta(),pTrack->pt(),TMath::RadToDeg()*pTrack->phi());
    }
  }
#endif
}
#if 1
//_____________________________________________________________________________
void StMuMcAnalysisMaker::FillVertexPlots(){
  if (mStKFParticleInterface) FillKFVertexPlots();
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::FillKFVertexPlots(){
  //   StMuDst::instance()->printKFVertices();
  //   StMuDst::instance()->printKFTracks();
  //  return;
  static Int_t nTracksAll = 0;
#if 0
  static Int_t nTracksGhost = 0;
  static Int_t nKFVertex = 0;
#endif
  static Int_t nStiVertex = 0;
  Int_t NoMuMcVertices = StMuDst::instance()->numberOfMcVertices(); //if (_debugAsk) cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices << std::endl;
  Int_t NoMuMcTracks = StMuDst::instance()->numberOfMcTracks();
//   if (! NoMuMcVertices || ! NoMuMcTracks) {
//     cout << "Ev. has no MC information ==> skip it" << endl;
//     return;
//   }
  vector<KFMCTrack> mcTracks(NoMuMcTracks);
  for (Int_t k = 0; k < NoMuMcTracks; k++) {
    StMuMcTrack *mcTrack = StMuDst::instance()->MCtrack(k);
    if (! mcTrack) continue;
    //       cout << "McTk: " << *mcTrack << endl;
    
    KFMCTrack &mcTrackKF = mcTracks[k];
    mcTrack->FillKFMCTrack(mcTrackKF);
    mcTrackKF.SetNMCPixelPoints(mcTrack->No_ist_hit() + mcTrack->No_ssd_hit() + mcTrack->No_pix_hit());
    
//     std::cout << "mcTrack->GePid() " << mcTrack->GePid() << " pdg " << mcTrack->Pdg()<<  " p " <<  mcTrackKF.P() << std::endl;
//     std::cout << "mcTrackKF.NMCPixelPoints() " << mcTrackKF.NMCPixelPoints() << " pixels " << int(mcTrack->No_ist_hit()) << " " << int(mcTrack->No_ssd_hit()) << " " << int(mcTrack->No_pix_hit()) << " No_tpc_hit() " << int(mcTrack->No_tpc_hit()) << " NoHits() " << int(mcTrack->NoHits()) << std::endl;
//     std::cin.get();
  }
  Int_t NoKFTracks =  StMuDst::instance()->numberOfKFTracks();
  Int_t NoKFVertices = StMuDst::instance()->numberOfKFVertices();
  
  nTracksAll += NoKFTracks;
  nStiVertex += NoKFVertices;
  
  Int_t NoPrimaryVertices = StMuDst::instance()->numberOfPrimaryVertices();  //if (_debugAsk) cout << "\tPrimaryVertices " << NoPrimaryVertices<< std::endl;
  if (! NoPrimaryVertices) return;
  const int NoStVertices = 1; // NoPrimaryVertices;
  //  const int NoStVertices = NoPrimaryVertices;
  vector<KFVertex> PrimVertex(NoStVertices);
  //vector<KFVertex> PrimVertex(NoPrimaryVertices);
  vector< vector<int> > PrimTracks(NoPrimaryVertices);
  
  float bestRank=-1000000;
  int bestPV=-1;
  
  for (Int_t l = 0; l < NoPrimaryVertices; l++) {//NoPrimaryVertices; l++) {
    StMuPrimaryVertex *Vtx = StMuDst::instance()->primaryVertex(l);
    if(!Vtx) continue;
    //       Vtx->Print();
    if (bestRank < Vtx->ranking()) {
      bestRank = Vtx->ranking();
      bestPV = l;
    }
    else continue;
    
    //convert StMuPrimaryVertex to KFVertex
    KFPVertex primVtx_tmp;
//     std::cout << "vtx " << Vtx << std::endl;
    primVtx_tmp.SetXYZ(Vtx->position().x(), Vtx->position().y(), Vtx->position().z());
    double dx = Vtx->posError().x();
    double dy = Vtx->posError().y();
    double dz = Vtx->posError().z();
    primVtx_tmp.SetCovarianceMatrix( dx*dx, 0, dy*dy, 0, 0, dz*dz );
    UShort_t noTracks = Vtx->noTracks();
    primVtx_tmp.SetNContributors(noTracks);
    primVtx_tmp.SetChi2(Vtx->chiSquared());
    PrimVertex[0] = KFVertex(primVtx_tmp);
    Int_t idd = Vtx->idTruth();
    // Check Mc
    if (idd > 0 && idd <= NoMuMcVertices) {
      StMuMcVertex *mcVertex = StMuDst::instance()->MCvertex(idd-1);
      if (mcVertex->Id() != idd) {
  cout << "Mismatched idTruth " << idd << " and mcVertex Id " <<  mcVertex->Id() 
       << " The vertex is ignored" <<  endl;
      }
      //      mcVertex->Print();
      
    }
  }
#ifdef __GoodPV__  
  bool isGoodPV = (PrimVertex[0].X() > -0.3) && (PrimVertex[0].X() < -0.1) &&
                  (PrimVertex[0].Y() > -0.27) && (PrimVertex[0].Y() < -0.13);
  if(!isGoodPV) return;
#endif /*   __GoodPV__ */
  Int_t NoGlobalTracks = StMuDst::instance()->numberOfGlobalTracks();
  
  //find max global track index
  int maxGBTrackIndex = -1;
  for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
    StMuTrack *gTrack = StMuDst::instance()->globalTracks(kg);
    if (! gTrack)            continue;
    
    int index = gTrack->id();
    
    if(index > maxGBTrackIndex)
      maxGBTrackIndex = index;
  }
  
  vector<KFParticle> particles(NoGlobalTracks*3);
  vector<int> nHftHits(NoGlobalTracks*3);
  vector<int> mcIndexes(maxGBTrackIndex+1);
  vector<int> particlesPdg(NoGlobalTracks*3);
  vector< vector<float> > particleCutValues(maxGBTrackIndex+1);
  int nPartSaved = 0;
  
  static int iEvent=0;
  iEvent++;
//   std::cout << "iEvent " << iEvent << std::endl;
  for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
    StMuTrack *gTrack = StMuDst::instance()->globalTracks(kg);
    if (! gTrack)            continue;
    
    //if (! gTrack->idTruth()) return kFALSE;
    if (! gTrack->charge())  continue;
    if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) continue; // bad fit or short track pointing to EEMC
    if (  gTrack->flag() > 1000) continue;  // pile up track in TPC
    if (  gTrack->nHitsFit() < 15) continue;
    if (  gTrack->probPidTraits().dEdxErrorFit() < 0.04 || gTrack->probPidTraits().dEdxErrorFit() > 0.12 ) continue;
//     if (  gTrack->pt() < 0.8 ) continue;
//     if (  gTrack->nHitsFit(kIstId) + gTrack->nHitsFit(kSsdId) + gTrack->nHitsFit(kPxlId) < 3 ) continue;
//     if (  gTrack->qaTruth() < 90) return kFALSE;
    hFitProb->Fill(gTrack->chi2prob());
    const int index = gTrack->id();
    
    Int_t kgc = gTrack->index2Cov();
    if (kgc < 0) continue;
    StDcaGeometry *dcaG = StMuDst::instance()->covGlobTracks(kgc);
    if (! dcaG) continue;
    //       cout << "dcaG:" <<  *dcaG << endl;
    Double_t xyzp[6], CovXyzp[21];
    dcaG->GetXYZ(xyzp,CovXyzp);
    
    bool goodTrack=1;
    for(int iPar=0; iPar<6; iPar++)
      goodTrack = goodTrack && finite(xyzp[iPar]);
    for(int iC=0; iC<21; iC++)
      goodTrack = goodTrack && finite(CovXyzp[iC]);
    goodTrack &= goodTrack && CovXyzp[0]  >=0.f && CovXyzp[0]  < 100.f;
    goodTrack &= goodTrack && CovXyzp[2]  >=0.f && CovXyzp[2]  < 100.f;
    goodTrack &= goodTrack && CovXyzp[5]  >=0.f && CovXyzp[5]  < 100.f;
    goodTrack &= goodTrack && CovXyzp[9]  >=0.f && CovXyzp[9]  < 1.f;
    goodTrack &= goodTrack && CovXyzp[14] >=0.f && CovXyzp[14] < 1.f;
    goodTrack &= goodTrack && CovXyzp[20] >=0.f && CovXyzp[20] < 1.f;
    if(!goodTrack) continue;
    
    static KFPTrack track;
    track.SetParameters(xyzp);
    track.SetCovarianceMatrix(CovXyzp);
    track.SetNDF(1);
    //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
    track.SetID(index);
    Int_t q   = 1;
//     Int_t pdg = -1;
    if (dcaG->charge() < 0) {
      q = -1;
    } 
    track.SetCharge(q);
        
//     bool isSecondary = true;
//     const StMuTrack *primTrack = gTrack->primaryTrack();
    
//     if(primTrack)
//     {
//       const int iPV = primTrack->vertexIndex(); 
//       //continue;
//         //if ((iPV!=bestPV)) continue;
//       {
//         vector<int> &tracksPV = PrimTracks[iPV];
//         tracksPV.push_back(nPartSaved);
//       }
//         
//       if ( iPV==bestPV )
//       {
//         KFParticle particle(track, pdg);
//         particle.SetId(index);
//         
//         particles[nPartSaved] = particle;
//         mcIndexes[index] = gTrack->idTruth()-1;
//         
//         if(mcIndexes[index] > -1)
//         {
//           particlesPdg[nPartSaved] = -1;// mcTracks[mcIndexes[index]].PDG();
//           mcTracks[mcIndexes[index]].SetReconstructed();
//         }
//         else
//         {
//           particlesPdg[nPartSaved] = -1;
//         }
//         nPartSaved++;
//         isSecondary = false;
//       }
//     }
//     
//     if(isSecondary)

    hdEdX->Fill(track.GetP(), gTrack->dEdx()*1.e6);
    
    const StMuBTofPidTraits &btofPid = gTrack->btofPidTraits();
    double timeTof = btofPid.timeOfFlight();
    double lengthTof = btofPid.pathLength();
    if(lengthTof < 0.)
    {
      const StThreeVectorF & tofPoint  = btofPid.position();
      const StThreeVectorF & dcaPoint  = gTrack->dca(0);
      StPhysicalHelixD innerHelix = gTrack->helix();
      double dlDCA = 0;
      if(NoPrimaryVertices > 0)
        dlDCA = fabs( innerHelix.pathLength( StThreeVector<double>(dcaPoint.x(), dcaPoint.y(), dcaPoint.z()) ) );
      StPhysicalHelixD outerHelix = gTrack->outerHelix();
      double dlTOF = fabs( outerHelix.pathLength( StThreeVector<double>(tofPoint.x(), tofPoint.y(), tofPoint.z()) ) );
      
      double l = gTrack->length();
      lengthTof = l + dlDCA + dlTOF;
    }
    double m2tof = -1.e6;
    bool isTofm2 = false;
    if(timeTof > 0. && lengthTof > 0.)
    {
      m2tof = track.GetP()*track.GetP()*(1./((lengthTof/timeTof/29.9792458)*(lengthTof/timeTof/29.9792458))-1.);
      hTofPID->Fill(track.GetP(), m2tof);
      hdEdXwithToF->Fill(track.GetP(), gTrack->dEdx()*1.e6);
      isTofm2 = true;
    }
//     int ToFPDG = -1;
//     if(m2tof > 0.6)
//       ToFPDG = 2212*q;
//     else if(m2tof > 0.14 && m2tof < 0.4)
//       ToFPDG = 321*q;
//     else if(m2tof > -0.5 && m2tof < 0.12)
//       ToFPDG = 211*q;
    
    vector<int> ToFPDG = GetTofPID(m2tof, track.GetP(), q);
    
#if 0
    if(track.GetP() < 0.7)
    {
      float mindEdXSigma = fabs(gTrack->nSigmaPion());
      int dEdXPDG = 211;
      if(fabs(gTrack->nSigmaKaon())   < mindEdXSigma) { mindEdXSigma = fabs(gTrack->nSigmaKaon());   dEdXPDG = 321; }
      if(fabs(gTrack->nSigmaProton()) < mindEdXSigma) { mindEdXSigma = fabs(gTrack->nSigmaProton()); dEdXPDG = 2212; }
      
      if( mindEdXSigma <=3.f )
      {
        dEdXPDG *= q;
        pdg = dEdXPDG;
      }
    }
    else
    {
      if(q < 0)
      {
        if(ToFPDG != -1)
        {
          if(ToFPDG == 211*q && (fabs(gTrack->nSigmaPion())<3))
            pdg = 211*q;
          if(ToFPDG == 321*q && (fabs(gTrack->nSigmaKaon())<3))
            pdg = 321*q;
          if(ToFPDG == 2212*q && (fabs(gTrack->nSigmaProton())<3))
            pdg = 2212*q;    
        }
        else
          if(fabs(gTrack->nSigmaKaon())<3)
            pdg = -321;
      }
      else
      {
        if(ToFPDG != -1)
        {
          if(ToFPDG == 211*q && (fabs(gTrack->nSigmaPion())<3))
            pdg = 211*q;
          if(ToFPDG == 321*q && (fabs(gTrack->nSigmaKaon())<3))
            pdg = 321*q;
          if(ToFPDG == 2212*q && (fabs(gTrack->nSigmaProton())<3))
            pdg = 2212*q;          
        }
        else
        {
          float mindEdXSigma = fabs(gTrack->nSigmaPion());
          int dEdXPDG = 211;
          if(fabs(gTrack->nSigmaProton()) < mindEdXSigma) { mindEdXSigma = fabs(gTrack->nSigmaProton()); dEdXPDG = 2212; }
    
          if( mindEdXSigma <=3.f )
            pdg = dEdXPDG;
        }
      }
    }
#else
    vector<int> dEdXPDG;
    vector<int> dEdXSigma;
    float nSigmaCut = 3.f;
    if(IAttr("StoreCutNTuples"))
      nSigmaCut = 3.e10f;
    if(fabs(gTrack->nSigmaPion())   < nSigmaCut) { dEdXPDG.push_back(211*q);  dEdXSigma.push_back(fabs(gTrack->nSigmaPion()));   }
    
    bool checkKTof = (track.GetP() > 0.5) && (track.GetP() < 2.);
    bool checkKHasTof = 0;
    for(int iTofPDG=0; iTofPDG<ToFPDG.size(); iTofPDG++)
      if(abs(ToFPDG[iTofPDG]) == 321)
        checkKHasTof = 1;
    if(fabs(gTrack->nSigmaKaon())  < 2.f && ((checkKTof && checkKHasTof) || !checkKTof) ) 
    {
      dEdXPDG.push_back(321*q);  
      dEdXSigma.push_back(fabs(gTrack->nSigmaKaon()));
    }
    if(fabs(gTrack->nSigmaProton()) < nSigmaCut) { dEdXPDG.push_back(2212*q); dEdXSigma.push_back(fabs(gTrack->nSigmaProton())); }
    
    float minSigmadEdX = 100;
    //    int iMinSigmadEdX = -1;
    for(UInt_t iPDG=0; iPDG<dEdXSigma.size(); iPDG++)
    {
      if(dEdXSigma[iPDG]<minSigmadEdX)
      {
        minSigmadEdX = dEdXSigma[iPDG];
	//        iMinSigmadEdX = iPDG;
      }
    }
        
    vector<int> totalPDG;
    if(!isTofm2 || IAttr("StoreCutNTuples"))
    {
//       if(minSigmadEdX <= 0.05f && track.GetP() < 0.7)
//         totalPDG.push_back(dEdXSigma[iMinSigmadEdX]);
//       else
        totalPDG = dEdXPDG;
    }
    else
    {
      for(UInt_t iPDG=0; iPDG<dEdXPDG.size(); iPDG++)
	for(int iTofPDG=0; iTofPDG<ToFPDG.size(); iTofPDG++)
	  if(dEdXPDG[iPDG] == ToFPDG[iTofPDG])
            totalPDG.push_back(ToFPDG[iTofPDG]);
          
//       if(totalPDG.size() == 0 && track.GetP() < 0.7)
//       {
//         if(minSigmadEdX <= 0.05f)
//           totalPDG.push_back(dEdXSigma[iMinSigmadEdX]);
//         else
//           totalPDG = dEdXPDG;
//       }
    }
    
    if(totalPDG.size() == 0)
      totalPDG.push_back(-1);
#endif
    for(UInt_t iPDG=0; iPDG<totalPDG.size(); iPDG++)
    {
      int pdg = totalPDG[iPDG];
      
//       if(primTrack)
//       {
//         const int iPV = primTrack->vertexIndex(); 
//         if(iPV==0)
//         {
//           vector<int> &tracksPV = PrimTracks[iPV];
//           tracksPV.push_back(nPartSaved);
//         }
//       }
      
      double dca = sqrt( gTrack->dca(0).x() * gTrack->dca(0).x() + 
                        gTrack->dca(0).y() * gTrack->dca(0).y() + 
                        gTrack->dca(0).z() * gTrack->dca(0).z() );

      KFParticle particle1(track, pdg);
      float chiPrim = particle1.GetDeviationFromVertex(PrimVertex[0]);
      
      if(pdg == -321) hDCAK->Fill(dca);
      if(pdg == 211)  hDCApi->Fill(dca);
      if(pdg == 2212) hDCAp->Fill(dca);
      
      if( sqrt(track.GetPx()*track.GetPx() + track.GetPy()*track.GetPy()) > 0.6f && (gTrack->nHitsFit(kIstId) + gTrack->nHitsFit(kSsdId) + gTrack->nHitsFit(kPxlId)) >=3 )
      {
        if(pdg == -321) 
        {
          hHFTDCAK->Fill(dca); hChiPrimHFTK->Fill(chiPrim);
          if(dca > 75.e-4f) hChiPrimHFTWithCutK->Fill(chiPrim);
        }
        if(pdg == 211)  
        {
          hHFTDCApi->Fill(dca); hChiPrimHFTpi->Fill(chiPrim);
          if(dca > 80.e-4f) hChiPrimHFTWithCutpi->Fill(chiPrim);
        }
        if(pdg == 2212) 
        {
          hHFTDCAp->Fill(dca); hChiPrimHFTp->Fill(chiPrim);
          if(dca > 65.e-4f) hChiPrimHFTWithCutp->Fill(chiPrim);
        }
      }
        
  //     pdg = -1;
  //     pdg = -3; //TODO
      { 
        mcIndexes[index] = gTrack->idTruth()-1;

        if(mcIndexes[index] >= (Int_t) mcTracks.size())
          mcIndexes[index] = -1;
        
        if(mcIndexes[index] > -1)
        {
          mcTracks[mcIndexes[index]].SetReconstructed();
          if(!fProcessSignal)
            pdg = -3;//mcTracks[mcIndexes[index]].PDG(); //TODO
          
          int mcPDG = mcTracks[mcIndexes[index]].PDG();
          if(mcPDG == -321) hSignalDCAK->Fill(dca);
          if(mcPDG == 211)  hSignalDCApi->Fill(dca);
          if(mcPDG == 2212) hSignalDCAp->Fill(dca);
          
          if( sqrt(track.GetPx()*track.GetPx() + track.GetPy()*track.GetPy()) > 0.6f && (gTrack->nHitsFit(kIstId) + gTrack->nHitsFit(kSsdId) + gTrack->nHitsFit(kPxlId)) >=3 )
          {
            if(mcPDG == -321) 
            {
              hSignalHFTDCAK->Fill(dca); hChiPrimHFTSignalK->Fill(chiPrim);
              if(dca > 75.e-4f) hChiPrimHFTSignalWithCutK->Fill(chiPrim);
            }
            if(mcPDG == 211)  
            {
              hSignalHFTDCApi->Fill(dca); hChiPrimHFTSignalpi->Fill(chiPrim);
              if(dca > 80.e-4f) hChiPrimHFTSignalWithCutpi->Fill(chiPrim);
            }
            if(mcPDG == 2212) 
            {
              hSignalHFTDCAp->Fill(dca); hChiPrimHFTSignalp->Fill(chiPrim);
              if(dca > 65.e-4f) hChiPrimHFTSignalWithCutp->Fill(chiPrim);
            }
          }
        }
        else if(fProcessSignal) continue; //TODO
        
        KFParticle particle(track, pdg);
        particle.SetId(index);
        particles[nPartSaved] = particle;
        nHftHits[nPartSaved] = gTrack->nHitsFit(kIstId) + gTrack->nHitsFit(kSsdId) + gTrack->nHitsFit(kPxlId);
        
  // //       if(nHftHits[nPartSaved] < 3)
  // //         pdg = -3;
        
        hNHFTHits->Fill(nHftHits[nPartSaved]);

  //       if(mcIndexes[index] > -1)
  //       {
  //         std::cout << "index " << nPartSaved << std::endl;
  //         std::cout << "pdg   " << pdg << std::endl;
  //         std::cout << "id    " << index << std::endl;
  //         std::cout << "nhft  " << nHftHits[nPartSaved] << std::endl;
  //       }
        
        particlesPdg[nPartSaved] = pdg;
        
        if(IAttr("StoreCutNTuples"))
        {
          particleCutValues[index].resize(8);
          particleCutValues[index][0] = nHftHits[nPartSaved];
          particleCutValues[index][1] = gTrack->nHitsFit();
          particleCutValues[index][2] = m2tof;
          particleCutValues[index][3] = fabs(gTrack->nSigmaPion());
          particleCutValues[index][4] = fabs(gTrack->nSigmaKaon());
          particleCutValues[index][5] = fabs(gTrack->nSigmaProton());
          particleCutValues[index][6] = sqrt(track.GetPx()*track.GetPx() + track.GetPy()*track.GetPy());
          particleCutValues[index][7] = chiPrim;
        }
        
        if(chiPrim < 18.6)
          PrimTracks[0].push_back(nPartSaved);
        
        nPartSaved++;
      }
    }
  }
//   std::cout << "n particles " << nPartSaved << std::endl;
//   std::cin.get();

  particles.resize(nPartSaved);
  particlesPdg.resize(nPartSaved);
  nHftHits.resize(nPartSaved);
  
  const Double_t field = StMuDst::instance()->event()->magneticField();
  
  mStKFParticleInterface->SetField(field);
#if 1
  if(NoKFTracks > 0)
    mStKFParticleInterface->SetBeamLine( *(StMuDst::instance()->KFtrack(0)));
#endif  
  mStKFParticleInterface->SetParticles(particles);
  mStKFParticleInterface->SetParticlesPdg(particlesPdg);
  mStKFParticleInterface->SetHftHits(nHftHits);
  mStKFParticleInterface->CleanPV();
  mStKFParticleInterface->InitParticles();
//   if(NoPrimaryVertices>0)
//   {
//     for(int iPV=0; iPV<NoPrimaryVertices; iPV++)
//     {
//       std::cout << "iPV " << iPV << " ntracks " << PrimTracks[iPV].size() << std::endl;
//       if (iPV==bestPV)
//         mStKFParticleInterface->AddPV(PrimVertex[iPV], PrimTracks[iPV]);
//     }
//     std::cin.get();
//   }
//   else
//   {
    
#if 0
    //find MC PV
    float xPV=0.f, yPV=0.f, zPV=0.f;
    for(unsigned int iTr=0; iTr<mcTracks.size(); iTr++)
    {
      if(mcTracks[iTr].MotherId() < 0)
      {
        xPV = mcTracks[iTr].X();
        yPV = mcTracks[iTr].Y();
        zPV = mcTracks[iTr].Z();
        break;
      }
    }
    
    KFPVertex primVtx_tmp;
    primVtx_tmp.SetXYZ(xPV, yPV, zPV);
    primVtx_tmp.SetCovarianceMatrix( 0, 0, 0, 0, 0, 0 );
    primVtx_tmp.SetNContributors(0);
    primVtx_tmp.SetChi2(-100);
    
    vector<int> tracks;
    KFVertex pv(primVtx_tmp);
    mStKFParticleInterface->AddPV(pv, tracks);
#else

    StMuPrimaryVertex *Vtx = StMuDst::instance()->primaryVertex(0);
    if(!Vtx) return;
    
    KFPVertex primVtx_tmp;
    primVtx_tmp.SetXYZ(Vtx->position().x(), Vtx->position().y(), Vtx->position().z());
    double dx = Vtx->posError().x();
    double dy = Vtx->posError().y();
    double dz = Vtx->posError().z();
    primVtx_tmp.SetCovarianceMatrix( dx*dx, 0, dy*dy, 0, 0, dz*dz );
    primVtx_tmp.SetNContributors(0);
    primVtx_tmp.SetChi2(-100);
    
    hPVError->Fill(sqrt(dx*dx + dy*dy));
    hPVErrorVsNTracks->Fill( nPartSaved, sqrt(dx*dx + dy*dy) );
    hPVErrorVsNPVTracks->Fill( PrimTracks[0].size(), sqrt(dx*dx + dy*dy) );
    
//     if(fabs(dx) > 0.02 || fabs(dy) > 0.02 || fabs(dz) > 0.02 ) return;

    KFVertex pv(primVtx_tmp);
    mStKFParticleInterface->AddPV(pv, PrimTracks[0]);
    
//     std::cout << "PV  x " << pv.X() << " y " << pv.Y() <<" z " << pv.Z() << " C " << 
//                            pv.CovarianceMatrix()[0] << " " << pv.CovarianceMatrix()[1] << " " <<  pv.CovarianceMatrix()[2] << " " <<
//                            pv.CovarianceMatrix()[3] << " " << pv.CovarianceMatrix()[4] << " " <<  pv.CovarianceMatrix()[5] << " " << std::endl;
//     std::cin.get();
  
//     vector<int> tracks;
//     mStKFParticleInterface->AddPV(pv, tracks);
    
    for(unsigned int iTr=0; iTr<mcTracks.size(); iTr++)
    {
      if(mcTracks[iTr].MotherId() < 0)
      {
        float dx = mcTracks[iTr].X() - Vtx->position().x();
        float dy = mcTracks[iTr].Y() - Vtx->position().y();
        float dz = mcTracks[iTr].Z() - Vtx->position().z();
        hLcdx->Fill( dx );
        hLcdy->Fill( dy );
        hLcdz->Fill( dz );
        hLcdr->Fill( sqrt(dx*dx + dy*dy) );
        
        hLcPx->Fill( dx/Vtx->posError().x() );
        hLcPy->Fill( dy/Vtx->posError().y() );
        hLcPz->Fill( dz/Vtx->posError().z() );
        
        break;
      }
    }
    
#endif
//     mStKFParticleInterface->AddPV(pv, tracks);
//   }
//   mStKFParticleInterface->InitParticles();
  
#if 0 /* Maksym reconstruction */
//   mStKFParticleInterface->ReconstructTopology();
   mStKFParticleInterface->ReconstructParticles();
#else
//   for(UInt_t iPart=0; iPart<particles.size(); iPart++)
//   {
//     particles[iPart].SetId(iPart);
//     particles[iPart].AddDaughterId(iPart);
//     mStKFParticleInterface->AddParticle(particles[iPart]);
//   }
  for (Int_t l = 0; l < NoKFVertices; l++)
  {
    KFVertex *vx = StMuDst::instance()->KFvertex(l);
    if (! vx) continue;
//     if( vx->GetNDF() < 0 ) continue;
    if( vx->NDaughters() == 0 ) continue;
    
    KFParticle particle = *vx;
    
//     if(particle.NDaughters() != 2) continue;
//     if(particle.DaughterIds()[0] >= particles.size() || particle.DaughterIds()[1] >= particles.size() ) 
//       continue;
    
//     vector<int> newIds;
//     newIds.push_back( trackIdMap[particle.DaughterIds()[0]]);
//     newIds.push_back( trackIdMap[particle.DaughterIds()[1]]);
//     
//     particle.CleanDaughtersId();
//     particle.AddDaughterId(newIds[0]);
//     particle.AddDaughterId(newIds[1]);
    
    int iPV = -1;
    if(particle.GetParentID() == bestPV)
      iPV = particle.GetParentID();
      
    if(vx->GetNDF() <= 1)
      mStKFParticleInterface->AddParticle(particle);
//     else
//       mStKFParticleInterface->AddCandidate(particle, iPV);
//     if(vx->GetNDF() > 2)
//       mStKFParticleInterface->AddCandidate(particle, 0);
//     else if(vx->GetNDF() > 1)
//       mStKFParticleInterface->AddCandidate(particle, 0);
  }
#endif
  mStKFParticlePerformanceInterface->SetMCTracks(mcTracks);
  mStKFParticlePerformanceInterface->SetMCIndexes(mcIndexes);    
  Int_t nevent = 100;
  mStKFParticlePerformanceInterface->SetPrintEffFrequency(nevent);
  mStKFParticlePerformanceInterface->PerformanceAnalysis();
  
  if(IAttr("StoreCutNTuples"))
  {
    for(int iParticle=0; iParticle<mStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++)
    {
      KFParticle particle;
      bool isMCParticle = mStKFParticlePerformanceInterface->GetParticle(particle, iParticle);
            
      if( !( (fProcessSignal && isMCParticle) || (!fProcessSignal && !isMCParticle) ) ) continue;
        
      vector<float> cutVariables;
      
      int nTuplePDG[fNNTuples] = {421, 411, 431, 4122};
      int nTrackCuts = 8, nParticleCuts = 3;
      for(int iNTuple=0; iNTuple<fNNTuples; iNTuple++)
      {
        if( particle.GetPDG() == nTuplePDG[iNTuple] )
        {
          cutVariables.resize(nTrackCuts*particle.NDaughters() + nParticleCuts);
          for(int iDaughter=0; iDaughter<particle.NDaughters(); iDaughter++)
          {
            const int daughterParticleIndex = particle.DaughterIds()[iDaughter];
            KFParticle daughter;
            mStKFParticlePerformanceInterface->GetParticle(daughter, daughterParticleIndex);
            if(daughter.NDaughters() != 1)
            {
              std::cout << "Error!!! StMuMcAnalysisMaker: save nTuples,   daughter.NDaughters() = " << daughter.NDaughters() << std::endl;
              continue;
            }
            const int daughterTrackIndex = daughter.DaughterIds()[0];
            
            for(int iCut=0; iCut<nTrackCuts; iCut++)
              cutVariables[iDaughter*nTrackCuts + iCut] = particleCutValues[daughterTrackIndex][iCut];
          }
          cutVariables[particle.NDaughters()*nTrackCuts]   = particle.Chi2()/particle.NDF();
          
          KFParticleSIMD tempSIMDParticle(particle);
          float_v l,dl;
          KFParticleSIMD pv(PrimVertex[0]);
          tempSIMDParticle.GetDistanceToVertexLine(pv, l, dl);
          cutVariables[particle.NDaughters()*nTrackCuts+1] = l[0]/dl[0];
          
          tempSIMDParticle.SetProductionVertex(pv);
          cutVariables[particle.NDaughters()*nTrackCuts+2] = double(tempSIMDParticle.Chi2()[0])/double(tempSIMDParticle.NDF()[0]);
          
          fCutsNTuple[iNTuple]->Fill(cutVariables.data());
        }
      }
    }
  }
#if 0  
  double GhostTracksRate = double(nTracksGhost)/double(nTracksAll);
  
  std::cout << "Ghost Tracks: N = " <<nTracksGhost << " All tracks: " <<  nTracksAll << " Ghost Rate: " << GhostTracksRate << std::endl;
  std::cout << "nKFVertex " << nKFVertex << " nStiVertex "  << nStiVertex << std::endl;
#endif
}
#endif
//________________________________________________________________________________
void StMuMcAnalysisMaker::FillQAGl(TrackMatchType type,const StMuTrack *gTrack, const StMuMcTrack *mcTrack, const StDcaGeometry *dcaG, const StMuMcVertex *mcVertex) {
#if 1
  if (! gTrack || ! mcTrack) return;
  if (! dcaG   || ! mcVertex) return;
  EChargeType pm = kPositive;
  if (mcTrack->Charge() < 0) pm = kNegative;
  Double_t eta = mcTrack->Pxyz().pseudoRapidity();
  Double_t pT  = mcTrack->Pxyz().perp();
  Double_t phi = TMath::RadToDeg()*mcTrack->Pxyz().phi();
  Var_t var; memset(&var.ChiSqXY, 0, sizeof(var));
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
  Double_t phiM =  TMath::ATan2(Dir[1],Dir[0]);
  var.dPsi    = phiM - TMath::DegToRad()*phi; 
  var.Phi = phi;
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
  Int_t Npart = 1;
  if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) Npart = 2;
  for (Int_t particle = 0; particle < Npart; particle++) {
    for (Int_t i = 0; i < kTotalQA; i++) {
      if (fHistsT[kGlobal][type][particle][pm][0][i])
	//  fHistsT[kGlobal][type][particle][pm][0][i]->Fill(gTrack->nHitsFit(), gTrack->nHitsFit()*(100.-gTrack->qaTruth())/100., x[i]);
  fHistsT[kGlobal][type][particle][pm][0][i]->Fill(gTrack->nHitsFit(), gTrack->qaTruth(), x[i]);
      if (fHistsT[kGlobal][type][particle][pm][1][i])
  fHistsT[kGlobal][type][particle][pm][1][i]->Fill(eta, pT, x[i]);
    }
  }
  McRcHit->Fill(mcTrack->No_tpc_hit(),gTrack->nHitsFit());
#endif
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::FillQAPr(TrackMatchType type,const StMuTrack *pTrack, const StMuMcTrack *mcTrack, const StMuPrimaryTrackCovariance *cov) {
#if 1
  if (! pTrack || ! mcTrack) return;
  if (! mcTrack->Charge()) return;
  EChargeType pm = kPositive;
  if (mcTrack->Charge() < 0) pm = kNegative;
  Var_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  var.ChiSqXY = pTrack->chi2xy();
  var.ChiSqZ  = pTrack->chi2z();
  Double_t Eta = mcTrack->Pxyz().pseudoRapidity();
  var.deta    = pTrack->eta() - Eta;
  var.dPsi    = pTrack->phi() - mcTrack->Pxyz().phi();
  var.Phi = TMath::RadToDeg()*mcTrack->Pxyz().phi();
  Double_t pTqRC = pTrack->pt()/pTrack->charge();
  Double_t pTqMC = mcTrack->pT()/mcTrack->Charge();
  var.dPti    = 1./pTqRC - 1./pTqMC;
  var.dPtiR   = pTqMC/pTqRC - 1;
  var.peta    = 999.;
  var.pPsi    = 999.;
  var.pPti    = 999.;
  var.pPtiR   = 999.;
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
  Int_t Npart = 1;
  if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) Npart = 2;
  for (Int_t particle = 0; particle < Npart; particle++) {
    for (Int_t i = 0; i < kTotalQAll; i++) {
      if (fHistsT[kPrimary][type][particle][pm][0][i])
  fHistsT[kPrimary][type][particle][pm][0][i]->Fill(pTrack->nHitsFit(), pTrack->qaTruth(), x[i]);
      if (fHistsT[kPrimary][type][particle][pm][1][i])
       fHistsT[kPrimary][type][particle][pm][1][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    }
  }
#endif
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::FillQAPr(TrackMatchType type,const StMuTrack *pTrack, const StMuMcTrack *mcTrack, const KFParticle *particle) {
#if 1
  if (! pTrack || ! mcTrack) return;
  if (! mcTrack->Charge()) return;
  EChargeType pm = kPositive;
  if (mcTrack->Charge() < 0) pm = kNegative;
  Var_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  var.ChiSqXY = pTrack->chi2xy();
  var.ChiSqZ  = pTrack->chi2z();
  Double_t Eta = mcTrack->Pxyz().pseudoRapidity();
  var.deta    = pTrack->eta() - Eta;
  var.dPsi    = pTrack->phi() - mcTrack->Pxyz().phi();
  var.Phi = TMath::RadToDeg()*mcTrack->Pxyz().phi();
  Double_t pTqRC = pTrack->pt()/pTrack->charge();
  Double_t pTqMC = mcTrack->pT()/mcTrack->Charge();
  var.dPti    = 1./pTqRC - 1./pTqMC;
  var.dPtiR   = pTqMC/pTqRC - 1;
  var.peta    = 999.;
  var.pPsi    = 999.;
  var.pPti    = 999.;
  var.pPtiR   = 999.;
  if (particle) {
    var.ChiSqZ  = particle->GetChi2();
    Float_t pT, dpT;
    Float_t Eta, dEta;
    Float_t Phi, dPhi;
    if (! particle->GetPt(pT,dpT) && ! particle->GetEta(Eta,dEta) && ! particle->GetPhi(Phi,dPhi)) {
      Float_t dpTi = dpT/(pTqRC*pTqRC);
      var.peta    = var.deta / dEta;
      var.pPsi    = var.dPsi / dPhi;
      var.pPti    = var.dPti / dpTi;
      var.pPtiR   = var.dPtiR/ dpTi / (mcTrack->pT()/mcTrack->Charge());
    } 
  }
  Double_t *x = &var.ChiSqXY;
  Int_t Npart = 1;
  if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) Npart = 2;
  for (Int_t particle = 0; particle < Npart; particle++) {
    for (Int_t i = 0; i < kTotalQAll; i++) {
      if (fHistsT[kPrimary][type][particle][pm][0][i])
  fHistsT[kPrimary][type][particle][pm][0][i]->Fill(pTrack->nHitsFit(), pTrack->qaTruth(), x[i]);
      if (fHistsT[kPrimary][type][particle][pm][1][i])
  fHistsT[kPrimary][type][particle][pm][1][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    }
  }
#endif
}
//________________________________________________________________________________
TString &StMuMcAnalysisMaker::FormName(const TH1 *hist) {
  static TString Name;
  Name = "";
  if (hist) {
    Name = DirPath(hist);
    Name += "/"; Name += hist->GetName();
    Name.ReplaceAll("/(+)","");
    Name.ReplaceAll("/(-)","");
    Name.ReplaceAll("/","_");
  }
  return *&Name;
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::ForceAnimate(unsigned int times, int msecDelay) {
  unsigned int  counter = times;
  while( (!times || counter) && !gSystem->ProcessEvents()) { --counter; if (msecDelay) gSystem->Sleep(msecDelay);} 
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawPng(TCanvas *c) {
  TString pngName("");
  if (! c) return;
  pngName = c->GetName();
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
  pngName.ReplaceAll("+","");
  pngName.ReplaceAll("-","");
  pngName += ".png"; 
  if (pngName.Contains("_NoHits_ChiSqXY_y.png")) c->SetLogy(1);
  c->Update();
  TVirtualX::Instance()->WritePixmap(c->GetCanvasID(),-1,-1,(Char_t *)pngName.Data());
  nPng++;
  cout << "Draw #\t" << nPng << "\t" << pngName << endl;
  TString FigNo; /* (SubSection); FigNo += ".";*/ 
  FigNo += nPng;
  Bool_t commentIt = kFALSE;
  if (pngName.Contains("_EtapT_ChiSqXY_yx_pfx.png") ||
      pngName.Contains("_EtapT_Phi_zx_1.png") ||
      pngName.Contains("_EtapT_Phi_zy_1.png")) commentIt = kTRUE;
  if (pngName.Contains("Tracks_Primary") &&
      (pngName.Contains("TanL_zx_1.png") ||
       pngName.Contains("TanL_zy_1.png"))) commentIt = kTRUE;
  if (pngName.Contains("Tracks_Primary") &&
      (pngName.Contains("DcaXY_zx_1.png") ||
       pngName.Contains("DcaZ_zx_1.png") ||
       pngName.Contains("EtapT_peta_zx_1.png") ||
       pngName.Contains("EtapT_peta_zy_1.png")
       )
      ) commentIt = kTRUE;
  if (commentIt) out << "<!--" << endl;
  out << "<tr>" << endl; // Fig." << FigNo.Data() << "," << endl;
  out << "<td><a name=\"Fig." <<  FigNo.Data() << "\">Fig." << FigNo.Data() << "</a><img src=\"Sti/" << pngName.Data() << "\" alt=\"\" width=\"400\" border=\"0\"></td>" << endl;
  out << "<td></a><img src=\"StiCA/" << pngName.Data() << "\" alt=\"\" width=\"400\" border=\"0\"></td>" << endl;
  out << "</tr>" << endl;
  if (commentIt) out << "-->" << endl;
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::MinMax(TH1 *h, Double_t &min, Double_t &max, Double_t amax) {
  if (! h) return;
  Int_t n = h->GetNbinsX();
  Int_t imin = 0;
  Int_t imax = n;
  for (Int_t i = 1; i <=n; i++) {
    Double_t y = h->GetBinContent(i);
    Double_t dy = h->GetBinError(i);
    if (dy == 0.0 || TMath::Abs(y) < dy) {
      imin = i + 1;
      continue;
    }
    break;
  }
  Bool_t entry = kFALSE;
  for (Int_t i = n; i >= imin; i--) {
    Double_t y = h->GetBinContent(i);
    Double_t dy = h->GetBinError(i);
    if (dy == 0.0 && ! entry) {
      imax = i;
    } else {
      entry = kTRUE;
    }
    if (TMath::Abs(y+dy) > amax) continue;
    if (TMath::Abs(y-dy) > amax) continue;
    if (y > 0 && y < 3*dy) continue;
    if (y < 0 && y >  -2*dy) continue;
    if (y + dy > max) max = y + dy;
    if (y - dy < min) min = y - dy;
  }
  if (min < -0.5*max) min = -0.5*max;
  if (imax < n) h->GetXaxis()->SetRange(imin,imax);
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawH3s(TH3F *h3[2], Int_t animate, Double_t min, Double_t max, Int_t np) {
  TH3F *h3s[2] = {h3[0], h3[1]};
  if (! h3s[0] && ! h3s[1]) return;
  for (Int_t i = 0; i < 2; i++) {
    if (! h3s[i]) continue;
    if (h3s[i]->GetEntries() < 100) {
      cout << "Histograms " << h3s[i]->GetName() << " has " << h3s[i]->GetEntries() << "entries" << endl;
      h3s[i] = 0;
    }
  }
  if (! h3s[0] && ! h3s[1]) {
    cout << "To few for analysis. Skip" << endl;
    return;
  }
  for (Int_t p = 0; p < np; p++) {// zx, zy, x, y, yx
    TCanvas *cpm[2] = {0};
    TH2 *h2[2] = {0,0};
    TH1 *h1[2] = {0,0};
    TH1 *s1[2] = {0,0};
    static const Char_t *pmNames[2] = {"pos","neg"};
    for (Int_t pm = kPositive; pm < kTotalSigns; pm++) {
      TH3 *h3 = h3s[pm];
      if (! h3) continue;
      h3->GetDirectory()->cd();
      h2[pm] = (TH2 *) h3->Project3D(Form("%s",proj[p]));
      TString Title(h2[pm]->GetTitle());
      Title.ReplaceAll("(+) ","");
      Title.ReplaceAll("(-) ","");
      Title.ReplaceAll(Form("%s projection",proj[p]),"");
      //      Title.ReplaceAll("  Pr"," Primary tracks");
      //      Title.ReplaceAll("  Gl"," Global tracks");
      Title.ReplaceAll(HitName,"");
      Title.ReplaceAll(KinPionName,"");
      Title.ReplaceAll(KinName,"");
      h2[pm]->SetTitle(Title);
      h2[pm]->SetMarkerColor(h3->GetMarkerColor());
      h2[pm]->SetLineColor(h3->GetLineColor());
      TString NameH(h3->GetName());
      cout << "Histogram: " << NameH.Data() << "\t" << Title.Data() << endl;
      if (NameH.Contains("ChiSq",TString::kIgnoreCase)) {
	if (p < 2 || p == 4) {
	  h1[pm] = (TH1 *) h2[pm]->ProfileX();
	  //    h1[pm]->SetTitle("");
	  h1[pm]->GetYaxis()->SetTitle(h2[pm]->GetYaxis()->GetTitle());
	  h1[pm]->SetStats(0);
	  h1[pm]->SetMarkerColor(h2[pm]->GetMarkerColor());
	  h1[pm]->SetLineColor(h2[pm]->GetLineColor());
	  h1[pm]->GetXaxis()->SetTitle(h2[pm]->GetXaxis()->GetTitle());
	} else {
	  h1[pm] = (TH1 *) h2[pm];
	  h2[pm] = 0;
	  h1[pm]->GetYaxis()->SetTitle("");
	  h1[pm]->SetTitle("");
	}
	MinMax(h1[pm],min,max,500);
      } else {
	h2[pm]->FitSlicesY(0,0,-1,10,"qeg3s");
	h1[pm] = (TH1 *) TDirectory::CurrentDirectory()->Get(Form("%s_1",h2[pm]->GetName()));
	if (h1[pm]) {
	  h1[pm]->SetTitle(Form("Fitted %s",Title.Data()));
	  h1[pm]->SetStats(0);
	  h1[pm]->SetMarkerColor(h2[pm]->GetMarkerColor());
	  h1[pm]->SetLineColor(h2[pm]->GetLineColor());
	  h1[pm]->GetXaxis()->SetTitle(h2[pm]->GetXaxis()->GetTitle());
	  h1[pm]->GetYaxis()->SetTitle(h2[pm]->GetYaxis()->GetTitle());
	  MinMax(h1[pm],min,max,10);
	  s1[pm] = (TH1 *) TDirectory::CurrentDirectory()->Get(Form("%s_2",h2[pm]->GetName()));
	  if (s1[pm]) {
	    s1[pm]->SetTitle(Form("#sigma %s",Title.Data()));
	    s1[pm]->SetMarkerStyle(21);
	    s1[pm]->SetMarkerColor(h2[pm]->GetMarkerColor());
	    s1[pm]->SetLineColor(h2[pm]->GetLineColor());
	    MinMax(s1[pm],min,max,10);
	    if (min > -0.1*max) min = -0.1*max;
	  }
	}
      }
    }
    if (h1[0] && h1[1]) {
#if 0
      Double_t yy = 0.3;
      if (! s1[0] || ! s1[1]) yy = 0.2; 
      //      TLegend *l = new TLegend(0.7,0.1,0.9,0.1+yy);
#endif
      TString xName(h1[0]->GetXaxis()->GetTitle());
      if (Debug()) {
	for (Int_t pm = kPositive; pm < kTotalSigns; pm++) {
	  cpm[pm] = new TCanvas(pmNames[pm],pmNames[pm]); //,700,500);
	  if (xName.Contains("pT",TString::kIgnoreCase)) cpm[pm]->SetLogx(1);
	  cpm[p]->SetLogz(1);
	  h2[pm]->Draw("colz");
	  h1[pm]->Draw("same");
	  s1[pm]->Draw("same");
	}
      }
      TString &Name = FormName(h1[0]); 
      TCanvas *c = new TCanvas(Name.Data(),Name.Data()); //,700,500);
      if (max > 0) max *= 1.1;
      else         max *= 0.9;
      if (min > 0) min *= 0.9;
      else         min *= 1.1;
      if (xName.Contains("pT",TString::kIgnoreCase)) c->SetLogx(1);
      if (p < 2) {
	h1[0]->SetMinimum(min);
	h1[0]->SetMaximum(max);
      }
      if (s1[0] && s1[1]) {
	Int_t first = TMath::Min(s1[0]->FindFirstBinAbove(0.0,1), s1[1]->FindFirstBinAbove(0.0,1));
	Int_t last  = TMath::Max(s1[0]->FindLastBinAbove(0.0,1) , s1[1]->FindLastBinAbove(0.0,1));
	if (first > 0 && last > 0 && first < last) h1[0]->GetXaxis()->SetRange(first,last);
      }
      for (Int_t pm = kPositive; pm < kTotalSigns; pm++) {
	if (pm == kPositive) h1[pm]->Draw(); 
	else                 h1[pm]->Draw("same"); 
	if (! s1[pm]) {
	  //    l->AddEntry(h1[pm], Form("averaged %s",TitleCharge[pm]));
	} else {
	  //    l->AddEntry(h1[pm], Form("%s #mu",TitleCharge[pm]));
	  s1[pm]->Draw("same");
	  //    l->AddEntry(s1[pm], Form("%s #sigma",TitleCharge[pm]));
	}
      }
      //      l->Draw();
      if (animate) ForceAnimate(0,200);
      c->Update();
      DrawPng(c);
      delete c;
      for (Int_t i = 0; i < 2; i++) {
	SafeDelete(cpm[i]);
	SafeDelete(h2[1]);
	SafeDelete(h1[1]);
	SafeDelete(s1[1]);
      }
    }
  }
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::Draw(Option_t *option){
  if (! Check()) return;
  TString Out("indexMc.html");
  out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  BeginHtml();
  out << "<H1>1. Tracks</H1>" << endl;
  Chapter = "1.1"; // nPng = 0;
  out << "<H2>" << Chapter.Data() << ". Quality of reconstructed tracks with respect to MC.</H2>" << endl;
  DrawQA();
  Chapter = "1.2"; // nPng = 0;
  out << "<H2>" << Chapter.Data() << ". Track reconstuction efficiencies.</H2>" << endl;
  DrawEff();
  if (IAttr("PiDPlots")) {
  Chapter = "1.3"; // nPng = 0;
  out << "<H2>" << Chapter.Data() << ". TPC dE/dx PiD.</H2>" << endl;
  DrawdEdx();
  Chapter = "1.4"; // nPng = 0;
  out << "<H2>" << Chapter.Data() << ". ToF PiD</H2>" << endl;
  DrawToF();
    }
  out << "<H1>2. Vertices</H1>" << endl;
  Chapter = "2.1"; // nPng = 0;
  EndHtml();
}
//________________________________________________________________________________
TString StMuMcAnalysisMaker::DirPath(const TH1 * hist) {
  TString path;
  if (hist && hist->GetDirectory()) {
    TString FullPath(hist->GetDirectory()->GetPathStatic());
    Int_t index = FullPath.Index(":");
    path = TString(FullPath.Data()+index+2);
  }
  return path;
}
//________________________________________________________________________________
Bool_t StMuMcAnalysisMaker::Check() {
  if (! TDirectory::CurrentDirectory() ) {cout << "There is no input file. Exit" << endl; return kFALSE;}
  if (! fHistsT[kGlobal][kRecoTk][kallP][kPositive][1][0]) {cout << "There are no input histograms. Exit" << endl; return kFALSE;}
  return kTRUE;
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawQA(Int_t gp, Int_t pp, Int_t xx, Int_t ii) {// versus Nhits
  Int_t animate = 0;
  TrackMatchType type = kRecoTk;
  Int_t k1 = kGlobal, k2 = kPrimary;
  if (gp >= 0 && gp <= kPrimary) {k1 =  k2 = gp;}
  for (Int_t k = k1; k <= k2; k++) {
    Section = Chapter; Section += "."; Section += k+1;
    out << "<h3>" << Section.Data() << ". " << TitleTrType[k] << " tracks. </h3>" << endl;
    Int_t i1 = 0, i2 = kTotalQAll - 1;
    TH3F *h3s[2];
    Int_t p1 = 0, p2 = 1;
    if (pp >= 0 && pp < kPartypeT) {p1 = p2 = pp;}
    if (type == kGhostTk || type == kGhostHftTk || type == kGhostToFTk) p2 = 0; // no pion for ghosts
    Int_t x1 = 0; Int_t x2 = kVariables - 1;
    if (xx >= 0 && xx < kVariables) {x1 = x2 = xx;}
    Int_t subsection = 0;
    for (Int_t particle = p1; particle <= p2; particle++) {
      for (Int_t x = x1; x <= x2; x++) {
	if (ii >= 0 && ii < kTotalQAll) {i1 = i2 = ii;}
	TString h4line;
	TString tag;
	if (k == kGlobal) tag += "Global ";
	else              tag += "Primary ";
	tag += " tracks. ";
	if (particle == kallP) tag += " All.";
	else                   tag += " Pions.";
	h4line += tag;
	if (x == 0)            {tag += "NoHits"; h4line += " No. of fit and quality.";}
	else                   {tag += "Kinema"; h4line += "Track parameters";}
	subsection++;
	SubSection = Section; SubSection += "."; SubSection += subsection;
	out << "<h4><a name \"" << tag.Data() << "\">" << SubSection.Data() << ". " <<h4line.Data() << "</a></h4>" << endl;
	BeginTable();
	for (Int_t i = i1; i <= i2; i++) {
	  h3s[0] = fHistsT[k][type][particle][kPositive][x][i];
	  h3s[1] = fHistsT[k][type][particle][kNegative][x][i];
	  if (! h3s[0] || ! h3s[1]) {cout << "No. Plots" << endl; continue;}
	  cout << h3s[0]->GetName() << "\t" << h3s[1]->GetName() << endl;
	  Double_t min =  1e9;
	  Double_t max = -1e9;
#if 0
	  if (k == kPrimary) {min = plotPrVar[i].min;  max = plotPrVar[i].max;}
	  else               {min = plotGlVar[i].min;  max = plotGlVar[i].max;}
#endif
	  if (i == 0) DrawH3s(h3s, animate, min, max, 5);
	  else        DrawH3s(h3s, animate, min, max);
	}
	EndTable();
      }
    }
  }
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawEff(Double_t ymax, Double_t pTmin, Int_t animate) {// Efficiencies
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
    {"GeomA", "Geometrical acceptance",kMcTpcTk, kMcTk,    0.0, 100.0},
    {"EffA",  "Efficiency over all",   kRecoTk,  kMcTk,    0.0, 100.0},
    {"EffG",  "Efficiency wrt Geom",   kRecoTk,  kMcTpcTk, 0.0, 100.0},
    {"CloneA","Clone  over all",       kCloneTk, kMcTk,    0.0,  25.0},
    {"CloneG","Clone wrt Geom",        kCloneTk, kMcTpcTk, 0.0,  60.0},
    {"LostA", "Lost over all",         kLostTk,  kMcTk,    0.0,  50.0},
    {"LostG", "Lost wrt Geom",         kLostTk,  kMcTpcTk, 0.0,  70.0},
    {"GhostA","Ghost over all",        kGhostTk, kMcTk,    0.0, 110.0},
    {"GhostG","Ghost wrt Geom",        kGhostTk, kMcTpcTk, 0.0, 110.0}
  };
  const Double_t pTmins[4] = {0.11, 0.5, 1.01, 2.0};
  TCanvas *c1 = 0;
  if (Debug()) {
    c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
    if (c1) c1->Clear();
    else    c1 = new TCanvas();
  }
  //                        Eta pT  Phi
  const Char_t *proj3[3] = {"x","y","z"};
  
  for (Int_t gp = kGlobal; gp < kTotalT; gp++) {
    Section = Chapter; Section += "."; Section += gp+1;
    out << "<h3>" << Section.Data() << ". " << TitleTrType[gp] << " tracks. </h3>" << endl;
    for (Int_t particle = 0; particle < kPartypeT; particle++) {
      Int_t subsection = 0;
      for (Int_t i = 0; i < kEffTotal; i++) {
	//  TrackMatchType t1 = eff[i].kDividend;
	//  TrackMatchType t2 = eff[i].kDivider;
	TString h4line;
	TString tag = TitleTrType[gp];
	tag += " tracks. ";
	if (particle == kallP) tag += " All.";
	else                   tag += " Pions.";
	tag += " "; tag += eff[i].Title; 
	h4line += tag; h4line += ".";
	subsection++;
	SubSection = Section; SubSection += "."; SubSection += subsection;
	out << "<h4><a name \"" << tag.Data() << "\">" << SubSection.Data() << ". " <<h4line.Data() << "</a></h4>" << endl;
	BeginTable();
	for (Int_t p = 0; p < 3; p++) { // projections
	  TString Name(eff[i].Name);
	  TString Title(eff[i].Title);
	  TH1 *heff[8]; memset(heff, 0, sizeof(heff));
	  Double_t min = eff[i].min;
	  Double_t max = eff[i].max;
	  Int_t NS = kTotalSigns;
	  if (pTmin < 0 && p != 1) NS *= 4;
	  for (Int_t l = kPositive; l < NS; l++) {
	    Int_t pm = l%kTotalSigns;
	    TH3F *Dividend = fHistsT[gp][eff[i].kDividend][particle][pm][1][kTotalQA];
	    TH3F *Divider  = fHistsT[gp][eff[i].kDivider][particle][pm][1][kTotalQA];
	    cout << "Sum " << Divider->GetName() << "\tentries = " << Divider->GetEntries() << endl;
	    if (! Dividend || ! Divider) {
	      cout << "Illegal fHistsT[" << gp << "][" << eff[i].kDividend << "][" << particle << "][" << pm << "][1][" << kTotalQA << "] = " 
		   << fHistsT[gp][eff[i].kDividend][particle][pm][1][kTotalQA]
		   << " or/and fHistsT[" << gp << "][" << eff[i].kDivider << "][" << particle << "][" << pm << "][1][" << kTotalQA << "] = " 
		   << fHistsT[gp][eff[i].kDivider][particle][pm][1][kTotalQA] << endl;
	      continue;
	    }
	    cout << "Eff " << Dividend->GetName() << "\t" << Dividend->GetDirectory()->GetPath() << "\tentries = " << Dividend->GetEntries() << endl;
	    if ( Dividend->GetEntries() < 100) {continue;}
	    Int_t nbinsX = Dividend->GetNbinsX();
	    Int_t nbinsY = Dividend->GetNbinsY();
	    Int_t nbinsZ = Dividend->GetNbinsZ();
	    Int_t binX1 = 1, binX2 = nbinsX;
	    Int_t binY1 = 1, binY2 = nbinsY;
	    Int_t binZ1 = 1, binZ2 = nbinsZ;
	    Double_t Val = 0;
	    Double_t Sum = 0;
	    Double_t val = 0, sum = 0;
	    Int_t bin = 0;
	    TH1 *temp = 0;
	    Double_t err = 0;
	    Dividend->GetXaxis()->SetRange(binX1,binX2);
	    Divider->GetXaxis()->SetRange(binX1,binX2);
	    Dividend->GetYaxis()->SetRange(binY1,binY2);
	    Divider->GetYaxis()->SetRange(binY1,binY2);
	    if (p != 0) { // ! eta
	      binX1 = Dividend->GetXaxis()->FindBin(-ymax);
	      binX2 = Dividend->GetXaxis()->FindBin( ymax);
	      Dividend->GetXaxis()->SetRange(binX1,binX2);
	      Divider->GetXaxis()->SetRange(binX1,binX2);
	    } 
	    if (p != 1) { // ! pT
	      if (NS == kTotalSigns) {
		binY1 = Dividend->GetYaxis()->FindBin(pTmin);
	      } else {
		binY1 = Dividend->GetYaxis()->FindBin(pTmins[l/2]);
	      }
	      Dividend->GetYaxis()->SetRange(binY1,binY2);
	      Divider->GetYaxis()->SetRange(binY1,binY2);
	    }
	    Dividend->GetDirectory()->cd();
	    heff[l] = Dividend->Project3D(proj3[p]); 
	    if (heff[l]->GetEntries() < 100) {SafeDelete(heff[l]); continue;}
	    if (l == 0) heff[l]->SetName(Form("%s%s",eff[i].Name,heff[l]->GetName()));
	    else        heff[l]->SetName(Form("%s%s_%i",eff[i].Name,heff[l]->GetName(),l));
	    heff[l]->SetTitle(Form("%s for %s vs %s",eff[i].Title,TitleTrType[gp],heff[l]->GetXaxis()->GetTitle()));
	    heff[l]->SetYTitle(Form("%s (%%)",eff[i].Title));
	    heff[l]->SetStats(0);
	    heff[l]->SetMarkerColor(l+1);
	    heff[l]->SetLineColor(l+1);
	    Title = heff[l]->GetTitle();
	    if (binX1 != 1)     Title += Form(" at |  #eta | <= %3.1f",ymax);
	    if (binY1 >  1)     Title += Form(" at pT > %3.2f",pTmins[l/2]);
	    heff[l]->SetTitle(Title);   
	    temp =Divider->Project3D(Form("%smc",proj3[p])); 
	    cout << heff[l]->GetName() << "\t" << heff[l]->GetDirectory()->GetPath() << "\t" << heff[l]->GetEntries() << " sum " << temp->GetEntries() << endl;
	    if (c1) {
	      c1->cd(); 
	      temp->Draw();
	      heff[l]->Draw();
	      c1->Update();
	    }
	    if (temp->GetEntries() < 1) {continue;}
	    if (temp->GetNbinsX() != heff[l]->GetNbinsX()) {
	      cout << "No. of bins in " <<  heff[l]->GetName() << " and " << temp->GetName() << " is different. Ignore these histograms" << endl;
	      delete heff[l]; heff[l] = 0;
	      delete temp;
	      continue;
	    }
	    for (bin = heff[l]->GetXaxis()->GetFirst(); bin <= heff[l]->GetXaxis()->GetLast(); bin++) {
	      val = heff[l]->GetBinContent(bin); Val += val;
	      sum = temp->GetBinContent(bin);; Sum += sum;
	      err = 0;
	      if      (sum < 1.e-7 && val < 1.e-7) {val =    0;}
	      else if (val > sum)                  {val = 1.05;}
	      else                                 {val /= sum;     err = TMath::Sqrt(val*(1.-val)/sum);}
	      heff[l]->SetBinContent(bin,100*val);
	      heff[l]->SetBinError(bin,100*err);
	    }
	    cout << heff[l]->GetName() 
		 << "[" << binX1 << "," << binX2 << "]"
		 << "[" << binX1 << "," << binX2 << "]"
		 << "[" << binZ1 << "," << binZ2 << "]"
		 << " Val = " << Val << "\tSum = " << Sum << endl;
	    if (Val < 100) {SafeDelete(heff[l]); continue;}
	    MinMax(heff[l],min,max,200);
	    if (c1) {
	      c1->cd(); 
	      heff[l]->Draw();
	      c1->Update();
	    }
	    SafeDelete(temp);
	  }
	  if (heff[0] || heff[1]) {
	    if (heff[0]) Name = FormName(heff[0]);
	    else         Name = FormName(heff[1]);
	    TCanvas *c = new TCanvas(Name.Data(),Name.Data(),400,400);
	    if (p == 1) c->SetLogx(1);
	    TLegend *l = 0;
	    if (NS > kTotalSigns) l = new TLegend(0.1,0.4,0.4,0.6);
	    TString same;
	    for (Int_t pm = kPositive; pm < NS; pm++) {
              if (! heff[pm]) continue; 
	      heff[pm]->SetMinimum(min); heff[pm]->SetMaximum(max); heff[pm]->Draw(same);
	      same = "same";
	      if (l) l->AddEntry(heff[pm],Form("%s with pT/|q| > %3.1f",TitleCharge[pm%2],pTmins[pm/2]));
	    }
	    if (l) l->Draw();
	    c->Update();
	    if (animate) ForceAnimate(0,200);
	    DrawPng(c);
	    if (Break) return;
	    delete c;
	  }
	  for (Int_t i = 0; i < 8; i++) {SafeDelete(heff[i]);}
	}
	
	EndTable();
      }
    }
  }
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawdEdx(Double_t lenMin) {
  if (! Check()) return;
  for (Int_t gp = 0; gp < kTotalT; gp++) {
    Section = Chapter; Section += "."; Section += gp+1;
    out << "<h3>" << Section.Data() << ". " << TitleTrType[gp] << " tracks. </h3>" << endl;
    Int_t subsection = 0;
    for (Int_t hyp = 0; hyp < NHypTypes; hyp++) {
      TString h4line;
      TString tag;
      if (gp == kGlobal) tag += "Global ";
      else               tag += "Primary ";
      tag += " tracks. ";
      tag += StProbPidTraits::mPidParticleDefinitions[hyp]->name().c_str();
      subsection++;
      SubSection = Section; SubSection += "."; SubSection += subsection;
      out << "<h4><a name \"" << tag.Data() << "\">" << SubSection.Data() << ". " <<h4line.Data() << "</a></h4>" << endl;
      BeginTable();
      for (Int_t var = 0; var < NdEdxPiD; var++) {
	TH3F *h3s[2] = {LdEdx[gp][hyp][0][var], LdEdx[gp][hyp][1][var]};
	if (! h3s[0] || ! h3s[1]) {cout << "No. Plots" << endl; continue;}
	cout << h3s[0]->GetName() << "\t" << h3s[1]->GetName() << endl;
	Int_t animate = 0;
	Double_t min =  1e9;
	Double_t max = -1e9;
	DrawH3s(h3s, animate, min, max);
      }
      EndTable();
    }
  }
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawToF() {
  if (! Check()) return;
  for (Int_t gp = 0; gp < kTotalT; gp++) {
    Section = Chapter; Section += "."; Section += gp+1;
    out << "<h3>" << Section.Data() << ". " << TitleTrType[gp] << " tracks. </h3>" << endl;
    Int_t subsection = 0;
    for (Int_t hyp = 0; hyp < NHypTypes; hyp++) {
      TString h4line;
      TString tag;
      if (gp == kGlobal) tag += "Global ";
      else               tag += "Primary ";
      tag += " tracks. ";
      tag += StProbPidTraits::mPidParticleDefinitions[hyp]->name().c_str();
      subsection++;
      SubSection = Section; SubSection += "."; SubSection += subsection;
      out << "<h4><a name \"" << tag.Data() << "\">" << SubSection.Data() << ". " <<h4line.Data() << "</a></h4>" << endl;
      BeginTable();
      for (Int_t var = 0; var < NToFPiD; var++) {
	TH3F *h3s[2] = {LToF[gp][hyp][0][var], LToF[gp][hyp][1][var]};
	if (! h3s[0] || ! h3s[1]) {cout << "No. Plots" << endl; continue;}
	cout << h3s[0]->GetName() << "\t" << h3s[1]->GetName() << endl;
	Int_t animate = 0;
	Double_t min =  1e9;
	Double_t max = -1e9;
	DrawH3s(h3s, animate, min, max);
      }
      EndTable();
    }
  }
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::BeginHtml() {
  out << "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">" << endl;
  out << "<html> " << endl;
  out << "  <head>" << endl;
  out << "    <title>Reconstruction versus Simulation</title>" << endl;
  out << "  </head>" << endl;
  out << "  <body>" << endl;
  out << "    <h1>Reconstruction versus Simulation</h1>" << endl;
  out << "    <h2>Legend</h2>" << endl;
  out << "<p>The plots are designed to answer on the following questions:" << endl;
  out << "  <ul>" << endl;
  out << "      <li> What tracks (globals and primaries) can be considered as \"good\" ones depending on total no. of fit points and no. of quality ? </li>" << endl;
  out << "      <li> What is the track parameters and their errors dependence on the track kinematics (&phi;, &eta;, 1/pT) ? </li>" << endl;
  out << "      <li> What is the track paramerer pulls dependence on above kinematics ? </li>" << endl;
  out << "      <li> What is the track reconstruction efficiencies for : </li>" << endl;
  out << "      <ul>" << endl;
  out << "           <li> Geometrical acceptance (MC only), </li>" << endl;
  out << "           <li> Reconstruction effiency for track with only match between MC and RC </li>" << endl;
  out << "           <li> Clones, for multiple (>1) match between single MC track to RC one, </li>" << endl;
  out << "           <li> Lost tracks, MC tracks which have no RC partner. </li>" << endl;
  out << "           <li> Ghost tracks, RC tracks which have no MC partner. </li>" << endl;
  out << "      </ul> " << endl;
  out << "      <li> Color scheme: <font color=black>&nbsp;&bull; Positive</font> and <font color=red>&nbsp;&bull; Negative</font> Tracks. </li>" << endl;
  out << "      <li> Results of Gauss fit for slices are presented as &bull; for &mu; and as  box for &sigma;.  </li>" << endl;
  out << "  </ul>" << endl;
}  
//________________________________________________________________________________
void StMuMcAnalysisMaker::BeginTable() {
  out << "    <table width=\"90%\" border=\"1\" cellspacing=\"2\" cellpadding=\"0\">" << endl;
  out << "        <tr>" << endl;
  out << "          <td>Sti</td><td>StiCA</td>" << endl;
  out << "        </tr>" << endl;
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::EndTable() {
  out << "</table>"  << endl;
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::EndHtml() {
  out << "<hr style=\"width: 100%; height: 2px;\">" << endl;
  out << "<hr>" << endl;
  out << "</body>" << endl;
  out << "</html>" << endl;
  
}
//________________________________________________________________________________
#include "TAxis.h"
void StMuMcAnalysisMaker::SetGEANTLabels(TAxis *x) {
  struct Label_t {
    Int_t id;
    const Char_t *name;
  };
  Label_t labels[50] = {
    {      1, "#gamma"},
    {      2, "e^{+}"},
    {      3, "e^{-}"},
    {      4, "#nu"},
    {      5, "#mu^{+}"},
    {      6, "#mu^{-}"},
    {      7, "#pi^{0}"},
    {      8, "#pi^{+}"},
    {      9, "#pi^{-}"},
    {     10, "K^{0}_{L}"},
    {     11, "K^{+}"},
    {     12, "K^{-}"},
    {     13, "n"},
    {     14, "p"},
    {     15, "#bar{p}"},
    {     16, "K^{0}_{S}"},
    {     17, "#eta"},
    {     18, "#Lambda"},
    {     19, "#Sigma^{+}"},
    {     20, "#Sigma^{0}"},
    {     21, "#Sigma^{-}"},
    {     22, "#Xi^{0}"},
    {     23, "#Xi^{-}"},
    {     24, "#Omega^{-}"},
    {     25, "#bar{n}"},
    {     26, "#bar{#Lambda}"},
    {     27, "#bar{#Sigma}^{-}"},
    {     28, "#bar{#Sigma}^{0}"},
    {     29, "#bar{#Sigma}^{+}"},
    {     30, "#bar{#Xi}^{0}"},
    {     31, "#bar{#Xi}^{+}"},
    {     32, "#bar{#Omega}^{+}"},
    {     33, "#tau^{+}"},
    {     34, "#tau^{-}"},
    {     35, "D^{+}"},
    {     36, "D^{-}"},
    {     37, "D^{0}"},
    {     38, "#bar{D^{0}}"},
    {     39, "D^{+}_{S}"},
    {     40, "D^{-}_{S}"},
    {     41, "#Lambda^{+}_{c}"},
    {     42, "W^{+}"},
    {     43, "W^{-}"},
    {     44, "Z^{0}"},
    {     45, "d"},
    {     46, "t"},
    {     47, "#alpha"},
    {     48, "#zeta"},
    {     49, "He^{3}"},
    {     50, "C"}
  };
  x->SetLabelSize(2e-2);
  for (Int_t i = 0; i < 50; i++) {
    x->SetBinLabel(labels[i].id,labels[i].name);
  }
}
//________________________________________________________________________________
std::vector<int> StMuMcAnalysisMaker::GetTofPID(double m2, double p, int q) {
  static const int order = 4;
  static const double parMean[6][order+1] = { { 0.02283190,-0.01482910, 0.01883130,-0.01824250, 0.00409811  }, //pi+
                                              { 0.24842500,-0.00699781,-0.00991387, 0.01327170,-0.00694824  }, //K+
                                              { 0.863211  , 0.0264171 ,-0.0230833 , 0.00239637, 0.000262309 }, //p
                                              { 0.0224095 ,-0.0123235 , 0.0145216 ,-0.0149944 , 0.00325952  }, //pi-
                                              { 0.250696  ,-0.0151308 , 0.00437457, 0.00516669,-0.00529184  }, //K-
                                              { 0.886912  ,-0.0298543 , 0.0449904 ,-0.0286879 , 0.00541963  }};//p-
  static const double parSigma[6][order+1] = { { 0.0112498,-0.0400571, 0.0733615,-0.0316505, 0.00629469 }, //pi+
                                               { 0.0154830,-0.0396312, 0.0719647,-0.0290683, 0.00637164 }, //K+
                                               { 0.114465 ,-0.287213 , 0.356536 ,-0.169257 , 0.0299844  }, //p
                                               { 0.0111682,-0.0394877, 0.0718342,-0.0302914, 0.00587317 }, //pi-
                                               { 0.0157322,-0.0402606, 0.0716639,-0.0272101, 0.00564467 }, //K-
                                               { 0.0899438,-0.211922 , 0.273122 ,-0.129597 , 0.0231844  }};//p-
  double pMax = 2.;
  double nSigmas[3];
  for(int iHypothesys = 0; iHypothesys<3; iHypothesys++)  {
    double x = p;
    if(x>=pMax) x = pMax;
    
    int iSet = iHypothesys;
    if(q<0)
      iSet += 3;
    double mean = 0;
    for(int iTerm=0; iTerm<=order; iTerm++)
      mean += parMean[iSet][iTerm]*TMath::Power(x,iTerm);  
    
    double sigma = 0;
    for(int iTerm=0; iTerm<=order; iTerm++)
      sigma += parSigma[iSet][iTerm]*TMath::Power(x,iTerm);  
    
    nSigmas[iHypothesys] = fabs((m2 - mean)/sigma);
  }
  
  double minNSigma = nSigmas[0];
  int minHypothesis = 0;
  for(int iHypothesys=1; iHypothesys<3; iHypothesys++)  {
    if(minNSigma > nSigmas[iHypothesys])     {
      minNSigma = nSigmas[iHypothesys];
      minHypothesis = iHypothesys;
    }
  }
  
  int pdgHypothesis[3] = {211, 321, 2212};
  vector<int> tofPID;
  if(minNSigma < 3)
    tofPID.push_back(pdgHypothesis[minHypothesis]*q);
  
  //   int pdgHypothesis[3] = {211, 321, 2212};
  //   for(int iHypothesys=0; iHypothesys<3; iHypothesys++)
  //     if(nSigmas[iHypothesys] < 3)
  //       tofPID.push_back(pdgHypothesis[iHypothesys]*q);
  
  return tofPID;
}
// 
// $Id: StMuMcAnalysisMaker.cxx,v 1.18 2007/10/27 17:42:59 fine Exp $
// $Log: StMuMcAnalysisMaker.cxx,v $
