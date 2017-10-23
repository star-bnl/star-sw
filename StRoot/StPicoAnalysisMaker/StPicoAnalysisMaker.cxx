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
static TH2F *hdEdXwithToF = 0;
static TH2F *hTofPID = 0;

static TH1F *hChiPrimHFTp = 0;
static TH1F *hChiPrimHFTK = 0;
static TH1F *hChiPrimHFTpi = 0;

static TH1F *hNHFTHits = 0;

static TH1F* hPVError = 0;
static TH2F* hPVErrorVsNTracks = 0;
static TH2F* hPVErrorVsNPVTracks = 0;

//________________________________________________________________________________
StPicoAnalysisMaker::StPicoAnalysisMaker(const char *name) : StMaker(name) {
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

  dirs[1] = dirs[0]->GetDirectory("Tracks"); assert(dirs[1]);
  dirs[1]->cd(); 
  
  hdEdX = (TH2F *)   dirs[1]->Get("hdEdX");
  if (! hdEdX) hdEdX = new TH2F("hdEdX", "hdEdX", 1000, 0, 5, 1000, 0, 10);

  hdEdXwithToF = (TH2F *)   dirs[1]->Get("hdEdXwithToF");
  if (! hdEdXwithToF) hdEdXwithToF = new TH2F("hdEdXwithToF", "hdEdXwithToF", 1000, 0, 5, 1000, 0, 10);
  
  hTofPID = (TH2F *)   dirs[1]->Get("hTofPID");
  if (! hTofPID) hTofPID = new TH2F("hTofPID", "hTofPID", 300, 0, 15, 1100, -1, 10);

  
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
  mStKFParticlePerformanceInterface = new StKFParticlePerformanceInterface(mStKFParticleInterface->GetTopoReconstructor());
  dirs[0]->cd();
  PrintMem(dirs[1]->GetPath());
}
//_____________________________________________________________________________
Int_t StPicoAnalysisMaker::Make(){
//   if (! GiD[0]) {
//     LOG_ERROR << "StPicoAnalysisMaker::Make histograms have not been initialized. Probably you have missed TTree file in bfc parameters" << endm;
//     return kStFatal;
//   }
  StPicoDstMaker *picoDstMaker = StPicoDstMaker::instance();
  if (! picoDstMaker) return kStFatal;
  fPicoDst = picoDstMaker->picoDst();
#if 0
  TObjectSet *muSet = (TObjectSet *) GetDataSet("muDst");
  if (! muSet) return kStFatal;
  muDst = (StMuDst *) muSet->GetObject();
#endif
  if (! fPicoDst) return kStOK;

  RunAnalysis();
  return kStOK;
}
//_____________________________________________________________________________
void StPicoAnalysisMaker::RunAnalysis(){
  std::cout << "Ololo!!!! " << std::endl;
#if 1
  vector<KFMCTrack> mcTracks(0);
  
  //read PV from pico Event
  KFVertex picoPrimaryVertex;
  vector<int> primaryTrackList;
    
  StPicoEvent* picoEvent = fPicoDst->event();
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
  
  vector<KFParticle> particles(nGlobalTracks*3);
  vector<int> nHftHits(nGlobalTracks*3);
  vector<int> mcIndexes(maxGBTrackIndex+1);
  for(unsigned int iIndex=0; iIndex<mcIndexes.size(); iIndex++)
    mcIndexes[iIndex] = -1;
  vector<int> particlesPdg(nGlobalTracks*3);
  vector< vector<float> > particleCutValues(maxGBTrackIndex+1);
  int nPartSaved = 0;
  
  for (Int_t kg = 0; kg < nGlobalTracks; kg++) 
  {
    StPicoTrack *gTrack = fPicoDst->track(kg);
    if (! gTrack)            continue;
    
    if (! gTrack->charge())  continue;
    if (  gTrack->nHitsFit() < 10) continue;
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
    
    static KFPTrack track;
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
        
    hdEdX->Fill(track.GetP(), gTrack->dEdx()*1.e6);
    
    
    double m2tof = -1.e6;
    if(gTrack->bTofPidTraitsIndex() > 0)
    {
      const StPicoBTofPidTraits* btofPid = fPicoDst->btofPidTraits(gTrack->bTofPidTraitsIndex());
      double betaTof = btofPid->btofBeta();
    }
    
//     if(timeTof > 0. && lengthTof > 0.)
//     {
//       m2tof = track.GetP()*track.GetP()*(1./((lengthTof/timeTof/29.9792458)*(lengthTof/timeTof/29.9792458))-1.);
//       hTofPID->Fill(track.GetP(), m2tof);
//       hdEdXwithToF->Fill(track.GetP(), gTrack->dEdx()*1.e6);
//     }
    int ToFPDG = -1;
    if(m2tof > 0.6)
      ToFPDG = 2212*q;
    else if(m2tof > 0.14 && m2tof < 0.4)
      ToFPDG = 321*q;
    else if(m2tof > -0.5 && m2tof < 0.12)
      ToFPDG = 211*q;
    

    vector<int> dEdXPDG;
    vector<int> dEdXSigma;
    float nSigmaCut = 3.e10f;
    if(fabs(gTrack->nSigmaPion())   < nSigmaCut) { dEdXPDG.push_back(211*q);  dEdXSigma.push_back(fabs(gTrack->nSigmaPion()));   }
    if(fabs(gTrack->nSigmaKaon())   < nSigmaCut) { dEdXPDG.push_back(321*q);  dEdXSigma.push_back(fabs(gTrack->nSigmaKaon()));   }
    if(fabs(gTrack->nSigmaProton()) < nSigmaCut) { dEdXPDG.push_back(2212*q); dEdXSigma.push_back(fabs(gTrack->nSigmaProton())); }
    
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
        if(dEdXPDG[iPDG] == ToFPDG)
          totalPDG.push_back(ToFPDG);
        
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

    for(int iPDG=0; iPDG<totalPDG.size(); iPDG++)
    {
      int pdg = totalPDG[iPDG];
      
      KFParticle particle1(track, pdg);
      float chiPrim = particle1.GetDeviationFromVertex(picoPrimaryVertex);
      
      if(chiPrim < 8)
        primaryTrackList.push_back(nPartSaved);
       
           
      KFParticle particle(track, pdg);
      particle.SetId(index);
      particles[nPartSaved] = particle;
      
      nHftHits[nPartSaved] = 0;
      if(gTrack->hasPxl1Hit()) nHftHits[nPartSaved]++;
      if(gTrack->hasPxl2Hit()) nHftHits[nPartSaved]++;
      if(gTrack->hasIstHit()) nHftHits[nPartSaved]++;
      if(gTrack->hasSstHit()) nHftHits[nPartSaved]++;
      
      hNHFTHits->Fill(nHftHits[nPartSaved]);        
      particlesPdg[nPartSaved] = pdg;
              
      nPartSaved++;
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
//     vector<int> tracks;
//     mStKFParticleInterface->AddPV(pv, tracks);
  
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

