//*-- Author : Yuri Fisyak 02/02/2016
#include "StPicoAnalysisMaker.h"
#include "TDirectory.h"
#include "TROOT.h"
#include "TMath.h"
#include "TNtuple.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "StProbPidTraits.h"
#include "TSystem.h"
#include "TArrayD.h"
//--- KF particle classes ---
#include "KFVertex.h"
#include "KFParticle.h"
#include "KFParticleSIMD.h"
#include "KFPTrack.h"
#include "StKFParticleInterface.h"
#include "KFParticlePerformance/StKFParticlePerformanceInterface.h"
//--- Pico classes ---
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
ClassImp(StPicoAnalysisMaker);
static TH2F *hdEdX = 0;
static TH2F *hdEdXPos = 0;
static TH2F *hdEdXNeg = 0;
static TH2F *hdEdXwithToF = 0;
static TH2F *hTofPID = 0;

// #define PIDHISTO
#ifdef PIDHISTO
static const int NTrackHistoFolders = 18;
static TH2F *hdEdXTracks[NTrackHistoFolders] = {0};
static TH2F *hdEdXwithToFTracks[NTrackHistoFolders] = {0};
static TH2F *hTofPIDTracks[NTrackHistoFolders] = {0};
static TH1F *hMomentumTracks[NTrackHistoFolders] = {0};
static TH2F *hdEdXPull[NTrackHistoFolders] = {0};
static TH2F *hdEdXZ[NTrackHistoFolders] = {0};
static std::map<int, int> trackPdgToHistoIndex;
#endif //PIDHISTO

static TH1F *hChiPrimHFTp = 0;
static TH1F *hChiPrimHFTK = 0;
static TH1F *hChiPrimHFTpi = 0;

static TH1F *hNHFTHits = 0;

static TH1F* hPVError = 0;
static TH2F* hPVErrorVsNTracks = 0;
static TH2F* hPVErrorVsNPVTracks = 0;

//________________________________________________________________________________
StPicoAnalysisMaker::StPicoAnalysisMaker(const char *name) : StMaker(name), fdEdXMode(1) {
  memset(mBeg,0,mEnd-mBeg+1);
}
//________________________________________________________________________________
StPicoAnalysisMaker::~StPicoAnalysisMaker() {
  SafeDelete(mStKFParticleInterface);
  SafeDelete(mStKFParticlePerformanceInterface);
}

//_____________________________________________________________________________
Int_t StPicoAnalysisMaker::Init(){
  TFile *f = GetTFile();
  if (f) {
    f->cd();
    BookTrackPlots();
    BookVertexPlots();
  }
  return kStOK;
}
//________________________________________________________________________________
Int_t StPicoAnalysisMaker::InitRun(Int_t runumber) {
  assert(StPicoDstMaker::instance());
//   if (StPicoDstMaker::instance()->IOMode() == StPicoDstMaker::ioRead) {
    //TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO Ask Yuri
//     StPicoDstMaker::instance()->SetStatus("*",0);
//     const Char_t *ActiveBranches[] = {
//       "MuEvent"
//       ,"PrimaryVertices"
//       ,"PrimaryTracks"
//       ,"GlobalTracks"
//       ,"StStMuMcVertex"
//       ,"StStMuMcTrack"
//       ,"CovPrimTrack"
//       ,"CovGlobTrack"
//       ,"StStMuMcVertex"
//       ,"StStMuMcTrack"
//       ,"KFTracks"
//       ,"KFVertices"
//       ,"StBTofHit"
//       ,"StBTofHeader"
//     }; 
//     Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
//     for (Int_t i = 0; i < Nb; i++) StPicoDstMaker::instance()->SetStatus(ActiveBranches[i],1); // Set Active braches
//   }
  return StMaker::InitRun(runumber);
}
//_____________________________________________________________________________
void StPicoAnalysisMaker::PrintMem(const Char_t *opt){
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
void StPicoAnalysisMaker::BookTrackPlots(){
  PrintMem("");

  TDirectory *dirs[7] = {0};
  dirs[0] = TDirectory::CurrentDirectory(); assert(dirs[0]);
  dirs[0]->cd();
  if (! dirs[0]->GetDirectory("Tracks")) {
    dirs[0]->mkdir("Tracks");
  }
  dirs[1] = dirs[0]->GetDirectory("Tracks"); assert(dirs[1]);
  dirs[1]->cd();
  
  hdEdX = (TH2F *)   dirs[1]->Get("hdEdX");
  if (! hdEdX) hdEdX = new TH2F("hdEdX", "hdEdX", 2000, 0, 10, 3000, 0, 30);

  hdEdXPos = (TH2F *)   dirs[1]->Get("hdEdXPos");
  if (! hdEdXPos) hdEdXPos = new TH2F("hdEdXPos", "hdEdXPos", 2000, 0, 10, 3000, 0, 30);
  
  hdEdXNeg = (TH2F *)   dirs[1]->Get("hdEdXNeg");
  if (! hdEdXNeg) hdEdXNeg = new TH2F("hdEdXNeg", "hdEdXNeg", 2000, 0, 10, 3000, 0, 30);
  
  hdEdXwithToF = (TH2F *)   dirs[1]->Get("hdEdXwithToF");
  if (! hdEdXwithToF) hdEdXwithToF = new TH2F("hdEdXwithToF", "hdEdXwithToF", 2000, 0, 10, 3000, 0, 30);
  
  hTofPID = (TH2F *)   dirs[1]->Get("hTofPID");
  if (! hTofPID) hTofPID = new TH2F("hTofPID", "hTofPID", 300, 0, 15, 1100, -1, 10);

#ifdef PIDHISTO
  int pdgTrackHisto[NTrackHistoFolders] = { 11, -11, 13, -13, 211, -211, 321, -321, 2212, -2212, 
                                            1000010020, -1000010020, 1000010030, -1000010030, 1000020030, -1000020030, 1000020040, -1000020040 };
  TString trackFolderName[NTrackHistoFolders] = {"e-", "e+", "mu-", "mu+", "pi+", "pi-", "K+", "K-", "p", "p-", "d", "d-", "t", "t-", "He3", "He3-", "He4", "He4-"};
                    
  for(int iTrackHisto=0; iTrackHisto<NTrackHistoFolders; iTrackHisto++)
  {
    if (!dirs[1]->GetDirectory(trackFolderName[iTrackHisto].Data()))
      dirs[1]->mkdir(trackFolderName[iTrackHisto].Data());
    
    dirs[2] = dirs[1]->GetDirectory(trackFolderName[iTrackHisto].Data()); assert(dirs[2]);
    dirs[2]->cd();
    
    trackPdgToHistoIndex[ pdgTrackHisto[iTrackHisto] ] = iTrackHisto;
    
    hdEdXTracks[iTrackHisto] = (TH2F *)   dirs[2]->Get("hdEdX");
    if (! hdEdXTracks[iTrackHisto]) hdEdXTracks[iTrackHisto] = new TH2F("hdEdX", "hdEdX", 2000, 0, 10, 3000, 0, 30);

    hdEdXwithToFTracks[iTrackHisto] = (TH2F *)   dirs[2]->Get("hdEdXwithToF");
    if (! hdEdXwithToFTracks[iTrackHisto]) hdEdXwithToFTracks[iTrackHisto] = new TH2F("hdEdXwithToF", "hdEdXwithToF", 2000, 0, 10, 3000, 0, 30);
  
    hTofPIDTracks[iTrackHisto] = (TH2F *)   dirs[2]->Get("hTofPID");
    if (! hTofPIDTracks[iTrackHisto]) hTofPIDTracks[iTrackHisto] = new TH2F("hTofPID", "hTofPID", 300, 0, 15, 1100, -1, 10);
  
    hMomentumTracks[iTrackHisto] = (TH1F *)   dirs[2]->Get("hMomentum");
    if (! hMomentumTracks[iTrackHisto]) hMomentumTracks[iTrackHisto] = new TH1F("hMomentum", "hMomentum", 1000, 0, 10);
    
    hdEdXPull[iTrackHisto] = (TH2F *)   dirs[2]->Get("hdEdXPull");
    if (! hdEdXPull[iTrackHisto]) hdEdXPull[iTrackHisto] = new TH2F("hdEdXPull", "hdEdXPull", 2000, 0, 10, 600, -30, 30);
    
    hdEdXZ[iTrackHisto] = (TH2F *)   dirs[2]->Get("hdEdXZ");
    if (! hdEdXZ[iTrackHisto]) hdEdXZ[iTrackHisto] = new TH2F("hdEdXZ", "hdEdXZ", 2000, -5, 5, 280, -1, 6);
    
    dirs[1]->cd();
  }
#endif //PIDHISTO
  
  double maxDCA = 1.;
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
  
  dirs[0]->cd();
}
//_____________________________________________________________________________
void StPicoAnalysisMaker::BookVertexPlots(){
  TDirectory *dirs[2] = {0};
  dirs[0] = TDirectory::CurrentDirectory(); assert(dirs[0]);
  dirs[0]->cd();
  if (! dirs[0]->GetDirectory("Particles")) {
    dirs[0]->mkdir("Particles");
  }
  dirs[1] = dirs[0]->GetDirectory("Particles"); assert(dirs[1]);
  dirs[1]->cd();
  PrintMem(dirs[1]->GetPath());
  
  mStKFParticleInterface = new StKFParticleInterface;
  const bool storeMCHistograms = false;
  mStKFParticlePerformanceInterface = new StKFParticlePerformanceInterface(mStKFParticleInterface->GetTopoReconstructor(), storeMCHistograms);
  dirs[0]->cd();
  PrintMem(dirs[1]->GetPath());
}
//_____________________________________________________________________________
Int_t StPicoAnalysisMaker::Make(){

//   StPicoDstMaker* picoDstMaker = (StPicoDstMaker *) StMaker::GetTopChain()->Maker("PicoDst");
//   if (! picoDstMaker) return kStFatal;
//   
//   fPicoDst = picoDstMaker->picoDst();
  
#ifdef PIDHISTO
  static int iEvent=0;
  iEvent++;
  if(iEvent%1000 == 0)
    std::cout << "iEvent " << iEvent << std::endl;
#endif
  
  fPicoDst = StPicoDst::instance();
  if (! fPicoDst)  return kStOK;

  RunAnalysis();
  return kStOK;
}
//_____________________________________________________________________________
void StPicoAnalysisMaker::RunAnalysis(){

  vector<KFMCTrack> mcTracks(0);
  
  //read PV from pico Event
  KFVertex picoPrimaryVertex;
  vector<int> primaryTrackList;
    
  StPicoEvent* picoEvent = fPicoDst->event();
  if(!picoEvent) return;
  
  const StThreeVectorF picoPV = picoEvent->primaryVertex();
  const StThreeVectorF picoPVError = picoEvent->primaryVertexError();
  
  KFPVertex primVtx_tmp;
  primVtx_tmp.SetXYZ(picoPV.x(), picoPV.y(), picoPV.z());
  double dx = picoPVError.x();
  double dy = picoPVError.y();
  double dz = picoPVError.z();
  primVtx_tmp.SetCovarianceMatrix( dx*dx, 0, dy*dy, 0, 0, dz*dz );
  picoPrimaryVertex = KFVertex(primVtx_tmp);

  bool isGoodPV = (picoPrimaryVertex.X() > -0.3) && (picoPrimaryVertex.X() < -0.1) &&
                  (picoPrimaryVertex.Y() > -0.27) && (picoPrimaryVertex.Y() < -0.13);
  if(!isGoodPV) return;
  
  Int_t nGlobalTracks = fPicoDst->numberOfTracks( );
  
  //find max global track index
  int maxGBTrackIndex = -1;
  for (Int_t kg = 0; kg < nGlobalTracks; kg++) {
    StPicoTrack *gTrack = fPicoDst->track(kg);
    if (! gTrack)            continue;
    
    int index = gTrack->id();
    
    if(index > maxGBTrackIndex)
      maxGBTrackIndex = index;
  }
  
  vector<KFParticle> particles(nGlobalTracks*7);
  vector<int> nHftHits(nGlobalTracks*7);
  vector<int> mcIndexes(maxGBTrackIndex+1);
  for(unsigned int iIndex=0; iIndex<mcIndexes.size(); iIndex++)
    mcIndexes[iIndex] = -1;
  vector<int> particlesPdg(nGlobalTracks*7);
  int nPartSaved = 0;
  
  for (Int_t kg = 0; kg < nGlobalTracks; kg++) 
  {
//     std::cin.get();
    StPicoTrack *gTrack = fPicoDst->track(kg);
    if (! gTrack)            continue;
    
    if (! gTrack->charge())  continue;
    if (  gTrack->nHitsFit() < 15) continue;
    if (  gTrack->dEdxError() < 0.04 || gTrack->dEdxError() > 0.12 ) continue;
    const int index = gTrack->id();
    
    const StDcaGeometry dcaG = gTrack->dcaGeometry();
    Double_t xyzp[6], CovXyzp[21];
    dcaG.GetXYZ(xyzp,CovXyzp);
    
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
    
    KFPTrack track;
    track.SetParameters(xyzp);
    track.SetCovarianceMatrix(CovXyzp);
    track.SetNDF(1);
    //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
    track.SetID(index);
    Int_t q   = 1;
//     Int_t pdg = -1;
    if (gTrack->charge() < 0) {
      q = -1;
    } 
    track.SetCharge(q);
        
    hdEdX->Fill(track.GetP(), gTrack->dEdx());
    if(q>0)
      hdEdXPos->Fill(track.GetP(), gTrack->dEdx());
    else
      hdEdXNeg->Fill(track.GetP(), gTrack->dEdx());
      
    double m2tof = -1.e6;
    if(gTrack->bTofPidTraitsIndex() > 0)
    {
      const StPicoBTofPidTraits* btofPid = fPicoDst->btofPidTraits(gTrack->bTofPidTraitsIndex());
      double betaTof2 = btofPid->btofBeta() * btofPid->btofBeta();
      if(fabs(betaTof2) > 1.e-6)
        m2tof = track.GetP()*track.GetP()*(1./betaTof2 - 1.);
      
      hTofPID->Fill(track.GetP(), m2tof);
      hdEdXwithToF->Fill(track.GetP(), gTrack->dEdx());
    }
    
    int ToFPDG = -1;
    if(m2tof > 7 && m2tof<9)
      ToFPDG = -1;// 1000010030*q;
    else if(m2tof > 2.8 && m2tof < 4)
      ToFPDG = -1;//1000010020*q;
    else if(m2tof > 1.5 && m2tof < 2.8)
      ToFPDG = 1;//1000020030*q;
    else if(m2tof > 0.6 && m2tof < 1.5)
      ToFPDG = 2212*q;
    else if(m2tof > 0.14 && m2tof < 0.4)
      ToFPDG = 321*q;
    else if(m2tof > -0.5 && m2tof < 0.12)
      ToFPDG = 211*q;
    
//     int ToFPDG = GetTofPID(m2tof, track.GetP(), q);
//     std::cout << " iTr " << kg << " x " << xyzp[0] << " y " << xyzp[1] <<  " z " << xyzp[2] << 
//                                   " px " << xyzp[3] << " py " << xyzp[4] <<  " pz " << xyzp[3] << 
//                                   " nPi " << gTrack->dEdxPullToF(0.139570, fdEdXMode, 1) << " nK " << gTrack->dEdxPullToF(0.493677, fdEdXMode, 1) << " nP " << gTrack->dEdxPullToF(0.938272, fdEdXMode, 1) << " m2tof " << m2tof << " ToFPDG " << ToFPDG<< std::endl;
    
    vector<int> dEdXPDG;
    vector<int> dEdXSigma;
    float nSigmaCut = 3.f;
    if(fabs(gTrack->dEdxPullToF(0.139570, fdEdXMode, 1))   < nSigmaCut) 
    { 
      dEdXPDG.push_back(211*q);  
      dEdXSigma.push_back(fabs(gTrack->dEdxPullToF(0.139570, fdEdXMode, 1)));   
    }
    
    bool checkKTof = (track.GetP() > 0.5) && (track.GetP() < 1.2);
    if(fabs(gTrack->dEdxPullToF(0.493677, fdEdXMode, 1))  < 2.f && ((checkKTof && abs(ToFPDG) == 321) || !checkKTof) ) 
    {
      dEdXPDG.push_back(321*q);  
      dEdXSigma.push_back(fabs(gTrack->dEdxPullToF(0.493677, fdEdXMode, 1)));
    }
    
    //bool checkPTof = (track.GetP() > 0.8) && (track.GetP() < 1.6);
    if(fabs(gTrack->dEdxPullToF(0.938272, fdEdXMode, 1)) < nSigmaCut)    //  && ((checkPTof && abs(ToFPDG) == 2212) || !checkPTof))
    {
      dEdXPDG.push_back(2212*q); 
      dEdXSigma.push_back(fabs(gTrack->dEdxPullToF(0.938272, fdEdXMode, 1)));
    }
    
    float minSigmadEdX = 100;
    int iMinSigmadEdX = -1;
    for(int iPDG=0; iPDG<dEdXSigma.size(); iPDG++)
    {
      if(dEdXSigma[iPDG]<minSigmadEdX)
      {
        minSigmadEdX = dEdXSigma[iPDG];
        iMinSigmadEdX = iPDG;
      }
    }
        
    vector<int> totalPDG;
    if(ToFPDG == -1)
    {
//       if(minSigmadEdX <= 0.05f && track.GetP() < 0.7)
//         totalPDG.push_back(dEdXSigma[iMinSigmadEdX]);
//       else
        totalPDG = dEdXPDG;
    }
    else
    {
      for(int iPDG=0; iPDG<dEdXPDG.size(); iPDG++)
      {
        if(dEdXPDG[iPDG] == ToFPDG)
          totalPDG.push_back(ToFPDG);
      } 
//       if(totalPDG.size() == 0 && track.GetP() < 0.7)
//       {
//         if(minSigmadEdX <= 0.05f)
//           totalPDG.push_back(dEdXSigma[iMinSigmadEdX]);
//         else
//           totalPDG = dEdXPDG;
//       }
    }
    
    if( track.GetP() < 1.5 )
    {
      float nSigmaDeutron = gTrack->dEdxPullToF(1.876124, fdEdXMode, 1);
      
      if(track.GetP() > 0.6)
      {
        float sigmaDCut = -3./0.9*(track.GetP()-0.6) + 3.;
        if(fabs(nSigmaDeutron) < sigmaDCut) 
          totalPDG.push_back(1000010020*q);
      }
      else
      {
        if(fabs(nSigmaDeutron) < 3.) 
          totalPDG.push_back(1000010020*q);
      }
    }
    if( track.GetP() < 1.5 )
    {
      float nSigmaTriton = gTrack->dEdxPullToF(2.809432, fdEdXMode, 1);
      
      if(track.GetP() > 0.6)
      {
        float sigmaTCut = -3./0.9*(track.GetP()-0.6) + 3.;
        if(fabs(nSigmaTriton) < sigmaTCut) 
          totalPDG.push_back(1000010030*q);
      }
      else
      {
        if(fabs(nSigmaTriton) < 3.) 
          totalPDG.push_back(1000010030*q);
      }        
    }
    if(track.GetP() > 1.3)
    {
      float nSigmaHe3 = gTrack->dEdxPullToF(2.809413, fdEdXMode, 2);
      if(fabs(nSigmaHe3) < nSigmaCut) { totalPDG.push_back(1000020030*q); }
      float nSigmaHe4 = gTrack->dEdxPullToF(3.728400, fdEdXMode, 2);
      if(fabs(nSigmaHe4) < nSigmaCut) { totalPDG.push_back(1000020040*q); }
    }
    
    
    if(totalPDG.size() == 0)
      totalPDG.push_back(-1);

    for(int iPDG=0; iPDG<totalPDG.size(); iPDG++)
    {
      int pdg = totalPDG[iPDG];
      
      KFPTrack trackPDG = track;
      if(abs(pdg) == 1000020030 || abs(pdg) == 1000020040)
      {
        trackPDG.SetCharge( trackPDG.Charge()*2.f );
        trackPDG.SetPx( trackPDG.GetPx()*2.f );
        trackPDG.SetPy( trackPDG.GetPy()*2.f );
        trackPDG.SetPz( trackPDG.GetPz()*2.f );

        const int index2[9] = { 6,7,8, 10,11,12, 15,16,17 };
        for(int iIndex=0; iIndex<9; iIndex++)
        {
          const int iC = index2[iIndex];
          trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*2.f );
        }
        const int index4[6] = { 9, 13,14, 18,19,20 };
        for(int iIndex=0; iIndex<6; iIndex++)
        {
          const int iC = index4[iIndex];
          trackPDG.SetCovariance( iC, trackPDG.GetCovariance(iC)*4.f );
        }
      }

      KFParticle particle1(trackPDG, pdg);
      float chiPrim = particle1.GetDeviationFromVertex(picoPrimaryVertex);
      
      if(chiPrim < 18.6)
        primaryTrackList.push_back(nPartSaved);
       
           
      KFParticle particle(trackPDG, pdg);
      particle.SetId(index);
      particles[nPartSaved] = particle;
      
      nHftHits[nPartSaved] = 0;
      if(gTrack->hasPxl1Hit()) nHftHits[nPartSaved]++;
      if(gTrack->hasPxl2Hit()) nHftHits[nPartSaved]++;
      if(gTrack->hasIstHit()) nHftHits[nPartSaved]++;
//       if(gTrack->hasSstHit()) nHftHits[nPartSaved]++;
      
      hNHFTHits->Fill(nHftHits[nPartSaved]);        
      particlesPdg[nPartSaved] = pdg;
              
      nPartSaved++;
      
#ifdef PIDHISTO
      //fill PID histograms
      {
        const int iTrackHisto = trackPdgToHistoIndex[pdg];
        if( ! (iTrackHisto < 0 || iTrackHisto >= NTrackHistoFolders) )
        {
          hMomentumTracks[iTrackHisto] -> Fill(track.GetP());
          hdEdXTracks[iTrackHisto] -> Fill(track.GetP(), gTrack->dEdx());
          if(ToFPDG != -1)
          {
            hdEdXwithToFTracks[iTrackHisto] -> Fill(track.GetP(), gTrack->dEdx());
            hTofPIDTracks[iTrackHisto] -> Fill(track.GetP(), m2tof);
            
            if(abs(ToFPDG)==211)
            {
              hdEdXPull[iTrackHisto] -> Fill(track.GetP(), gTrack->dEdxPullToF(0.139570, fdEdXMode, 1));
              float betaGamma = TMath::Log10(track.GetP()/0.139570);
              float z = gTrack->dEdxPullToF(0.139570, fdEdXMode, 1)*gTrack->dEdxError();
              hdEdXZ[iTrackHisto]->Fill(betaGamma, z);
              
              betaGamma = TMath::Log10(track.GetP()/5.485799e-4);
              z = gTrack->nSigmaElectron()*gTrack->dEdxError();
              hdEdXZ[0]->Fill(betaGamma, z);
            }
            if(abs(ToFPDG)==321)
            {
              hdEdXPull[iTrackHisto] -> Fill(track.GetP(), gTrack->dEdxPullToF(0.493677, fdEdXMode, 1));
              float betaGamma = TMath::Log10(track.GetP()/0.493677);
              float z = gTrack->dEdxPullToF(0.493677, fdEdXMode, 1)*gTrack->dEdxError();
              hdEdXZ[iTrackHisto]->Fill(betaGamma, z);
            }
            if(abs(ToFPDG)==2212)
            {
              hdEdXPull[iTrackHisto] -> Fill(track.GetP(), gTrack->dEdxPullToF(0.938272, fdEdXMode, 1));
              float betaGamma = TMath::Log10(track.GetP()/0.938272);
              float z = gTrack->dEdxPullToF(0.938272, fdEdXMode, 1)*gTrack->dEdxError();
              hdEdXZ[iTrackHisto]->Fill(betaGamma, z);
            }
          }
        }
      }
#endif
    }
  }
//   std::cout << "n particles " << nPartSaved << std::endl;
//   std::cin.get();

  particles.resize(nPartSaved);
  particlesPdg.resize(nPartSaved);
  nHftHits.resize(nPartSaved);
  
  const Double_t field = picoEvent->bField();
  
  mStKFParticleInterface->SetField(field);

  mStKFParticleInterface->SetParticles(particles);
  mStKFParticleInterface->SetParticlesPdg(particlesPdg);
  mStKFParticleInterface->SetHftHits(nHftHits);
  mStKFParticleInterface->CleanPV();
  mStKFParticleInterface->InitParticles();

  //read PV
  hPVError->Fill(sqrt(dx*dx + dy*dy));
  hPVErrorVsNTracks->Fill( nPartSaved, sqrt(dx*dx + dy*dy) );
  hPVErrorVsNPVTracks->Fill( primaryTrackList.size(), sqrt(dx*dx + dy*dy) );
  
//     if(fabs(dx) > 0.02 || fabs(dy) > 0.02 || fabs(dz) > 0.02 ) return;

  mStKFParticleInterface->AddPV(picoPrimaryVertex, primaryTrackList);
  
//   std::cout << "PV  x " << picoPrimaryVertex.X() << " y " << picoPrimaryVertex.Y() <<" z " << picoPrimaryVertex.Z() << " C " << 
//                            picoPrimaryVertex.CovarianceMatrix()[0] << " " << picoPrimaryVertex.CovarianceMatrix()[1] << " " <<  picoPrimaryVertex.CovarianceMatrix()[2] << " " <<
//                            picoPrimaryVertex.CovarianceMatrix()[3] << " " << picoPrimaryVertex.CovarianceMatrix()[4] << " " <<  picoPrimaryVertex.CovarianceMatrix()[5] << " " << std::endl;
//   std::cin.get();
//     vector<int> tracks;
//     mStKFParticleInterface->AddPV(pv, tracks);
  
#ifndef PIDHISTO
  //reconstruct short-lived particles
  mStKFParticleInterface->ReconstructParticles();
  
  //collect histograms
  mStKFParticlePerformanceInterface->SetMCTracks(mcTracks);
  mStKFParticlePerformanceInterface->SetMCIndexes(mcIndexes);    
  Int_t nevent = 100;
  mStKFParticlePerformanceInterface->SetPrintEffFrequency(nevent);
  mStKFParticlePerformanceInterface->PerformanceAnalysis();
#endif
}

int StPicoAnalysisMaker::GetTofPID(double m2, double p, int q)
{
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
  for(int iHypothesys = 0; iHypothesys<3; iHypothesys++)
  {
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
  for(int iHypothesys=1; iHypothesys<3; iHypothesys++)
  {
    if(minNSigma > nSigmas[iHypothesys]) 
    {
      minNSigma = nSigmas[iHypothesys];
      minHypothesis = iHypothesys;
    }
  }
  
  int pdgHypothesis[3] = {211, 321, 2212};
  int tofPID = -1;
  if(minNSigma < 3)
    tofPID = pdgHypothesis[minHypothesis]*q;
  
  return tofPID;
}
