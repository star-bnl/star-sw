//*-- Author : Yuri Fisyak 02/02/2016
#include "StKFParticleAnalysisMaker.h"
#include "TDirectory.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TSystem.h"
//--- KF particle classes ---
#include "KFVertex.h"
#include "KFParticle.h"
#include "KFParticleSIMD.h"
#include "KFPTrack.h"
#include "KFParticleTopoReconstructor.h"
#include "StKFParticleInterface.h"
#include "StKFParticlePerformanceInterface.h"
//--- Pico classes ---
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
//--- Mu classes ---
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
ClassImp(StKFParticleAnalysisMaker);

//________________________________________________________________________________
StKFParticleAnalysisMaker::StKFParticleAnalysisMaker(const char *name) : StMaker(name), fIsPicoAnalysis(true), fdEdXMode(1), 
  fStoreTmvaNTuples(false), fProcessSignal(false), fCollectPIDHistograms(false)
{
  memset(mBeg,0,mEnd-mBeg+1);
}
//________________________________________________________________________________
StKFParticleAnalysisMaker::~StKFParticleAnalysisMaker() 
{
  SafeDelete(fStKFParticleInterface);
  SafeDelete(fStKFParticlePerformanceInterface);
}

//_____________________________________________________________________________
Int_t StKFParticleAnalysisMaker::Init()
{
  TFile *f = GetTFile();
  if (f) 
  {
    f->cd();
    BookVertexPlots();
    fStKFParticleInterface->CollectTrackHistograms();
    if(fCollectPIDHistograms)
      fStKFParticleInterface->CollectPIDHistograms();
  }
  
  //Create file with NTuples for cut optimization
  if(fStoreTmvaNTuples)
  {
    TString ntupleNames[fNNTuples] = {"D0", "DPlus", "Ds", "Lc"};
    TString cutNames[fNNTuples] = {"pt_K:chi2Primary_K:pt_Pi:chi2Primary_Pi:Chi2NDF:LdL:Chi2Topo:refMult",
      "pt_K:chi2Primary_K:pt_Pi1:chi2Primary_Pi1:pt_Pi2:chi2Primary_Pi2:Chi2NDF:LdL:Chi2Topo:refMult",
      "pt_KPlus:chi2Primary_KPlus:pt_KMinus:chi2Primary_KMinus:pt_Pi:chi2Primary_Pi:Chi2NDF:LdL:Chi2Topo:refMult",
      "pt_P:chi2Primary_P:pt_K:chi2Primary_K:pt_Pi:chi2Primary_Pi:Chi2NDF:LdL:Chi2Topo:refMult"};
  
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
Int_t StKFParticleAnalysisMaker::InitRun(Int_t runumber) 
{
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
void StKFParticleAnalysisMaker::PrintMem(const Char_t *opt)
{
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
void StKFParticleAnalysisMaker::BookVertexPlots()
{
  TDirectory *dirs[2] = {0};
  dirs[0] = TDirectory::CurrentDirectory(); assert(dirs[0]);
  dirs[0]->cd();
  if (! dirs[0]->GetDirectory("Particles")) {
    dirs[0]->mkdir("Particles");
  }
  dirs[1] = dirs[0]->GetDirectory("Particles"); assert(dirs[1]);
  dirs[1]->cd();
  PrintMem(dirs[1]->GetPath());
  
  fStKFParticleInterface = new StKFParticleInterface;
  bool storeMCHistograms = false;
  if(!fIsPicoAnalysis && fProcessSignal) storeMCHistograms = true;
  fStKFParticlePerformanceInterface = new StKFParticlePerformanceInterface(fStKFParticleInterface->GetTopoReconstructor(), storeMCHistograms);
  dirs[0]->cd();
  PrintMem(dirs[1]->GetPath());
}
//_____________________________________________________________________________
Int_t StKFParticleAnalysisMaker::Make()
{  
  if(fIsPicoAnalysis)
  {
    fPicoDst = StPicoDst::instance();
    if(!fPicoDst) return kStOK;
  }
  else
  {  
    fMuDst = StMuDst::instance();
    if(!fMuDst) return kStOK;
    else { if(StMuDst::instance()->numberOfPrimaryVertices() == 0 ) return kStOK; }
  }
  
  //find max global track index
  int maxGBTrackIndex = -1;
  if(fIsPicoAnalysis)
  {
    for(unsigned int iTrack = 0; iTrack < fPicoDst->numberOfTracks(); iTrack++) 
    {
      StPicoTrack *gTrack = fPicoDst->track(iTrack);
      if (! gTrack) continue;
      int index = gTrack->id();
      if(index > maxGBTrackIndex)
        maxGBTrackIndex = index;
    }
  }
  else
  {
    for(unsigned int iTrack = 0; iTrack < fMuDst->numberOfGlobalTracks(); iTrack++) 
    {
      StMuTrack *gTrack = fMuDst->globalTracks(iTrack);
      if (! gTrack) continue;
      int index = gTrack->id();
      if(index > maxGBTrackIndex)
        maxGBTrackIndex = index;
    }
  }
  vector<KFMCTrack> mcTracks(0);
  vector<int> mcIndices(maxGBTrackIndex+1);
  for(unsigned int iIndex=0; iIndex<mcIndices.size(); iIndex++)
    mcIndices[iIndex] = -1;
  
//   fStKFParticleInterface->SetTriggerMode();
//   fStKFParticleInterface->SetSoftKaonPIDMode();
//   fStKFParticleInterface->SetSoftTofPidMode();
//   fStKFParticleInterface->SetChiPrimaryCut(8);
//   
//   fStKFParticleInterface->SetPtCutCharm(0.5);
//   fStKFParticleInterface->SetChiPrimaryCutCharm(8);
//   fStKFParticleInterface->SetLdLCutCharmManybodyDecays(3);
//   fStKFParticleInterface->SetChi2TopoCutCharmManybodyDecays(10);
//   fStKFParticleInterface->SetChi2CutCharmManybodyDecays(3);
//   fStKFParticleInterface->SetLdLCutCharm2D(3);
//   fStKFParticleInterface->SetChi2TopoCutCharm2D(10);
//   fStKFParticleInterface->SetChi2CutCharm2D(3);
  
  vector<int> triggeredTracks;
  bool isGoodEvent = false;
  
  //Process the event
  if(fIsPicoAnalysis)
    isGoodEvent = fStKFParticleInterface->ProcessEvent(fPicoDst, triggeredTracks);
  else
  {
    isGoodEvent = fStKFParticleInterface->ProcessEvent(fMuDst, mcTracks, mcIndices, fProcessSignal);
  }
//   bool openCharmTrigger = false;
//   if(isGoodEvent) openCharmTrigger =  fStKFParticleInterface->OpenCharmTrigger();
//   fStKFParticleInterface->OpenCharmTriggerCompression(triggeredTracks.size(), fPicoDst->numberOfTracks(), openCharmTrigger);
  //collect histograms
  
  if(isGoodEvent)
  {
    fStKFParticlePerformanceInterface->SetMCTracks(mcTracks);
    fStKFParticlePerformanceInterface->SetMCIndexes(mcIndices);    
    Int_t nevent = 100;
    fStKFParticlePerformanceInterface->SetPrintEffFrequency(nevent);
    fStKFParticlePerformanceInterface->PerformanceAnalysis();
    
    if(fStoreTmvaNTuples)
    {
      for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++)
      {
        KFParticle particle;
        bool isMCParticle = fStKFParticlePerformanceInterface->GetParticle(particle, iParticle);
              
        if( !( (fProcessSignal && isMCParticle) || (!fProcessSignal && !isMCParticle) ) ) continue;
          
        vector<float> cutVariables;
        
        int nTuplePDG[fNNTuples] = {421, 411, 431, 4122};
        int nTrackCuts = 2, nParticleCuts = 3, nEventCuts = 1;
        for(int iNTuple=0; iNTuple<fNNTuples; iNTuple++)
        {
          if( particle.GetPDG() == nTuplePDG[iNTuple] )
          {
            cutVariables.resize(nTrackCuts*particle.NDaughters() + nParticleCuts + nEventCuts);
            for(int iDaughter=0; iDaughter<particle.NDaughters(); iDaughter++)
            {
              const int daughterParticleIndex = particle.DaughterIds()[iDaughter];
              KFParticle daughter;
              fStKFParticlePerformanceInterface->GetParticle(daughter, daughterParticleIndex);
              if(daughter.NDaughters() != 1)
              {
                std::cout << "Error!!! StMuMcAnalysisMaker: save nTuples,   daughter.NDaughters() = " << daughter.NDaughters() << std::endl;
                continue;
              }
              
              cutVariables[iDaughter*nTrackCuts]   = daughter.GetPt();
              cutVariables[iDaughter*nTrackCuts+1] = daughter.GetDeviationFromVertex(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex());
            }
            cutVariables[particle.NDaughters()*nTrackCuts]   = particle.Chi2()/particle.NDF();
            
            KFParticleSIMD tempSIMDParticle(particle);
            float_v l,dl;
            KFParticleSIMD pv(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex());
            tempSIMDParticle.GetDistanceToVertexLine(pv, l, dl);
            cutVariables[particle.NDaughters()*nTrackCuts+1] = l[0]/dl[0];
            
            tempSIMDParticle.SetProductionVertex(pv);
            cutVariables[particle.NDaughters()*nTrackCuts+2] = double(tempSIMDParticle.Chi2()[0])/double(tempSIMDParticle.NDF()[0]);

            if(fIsPicoAnalysis)
              cutVariables[particle.NDaughters()*nTrackCuts+3] = fPicoDst->event()->refMult();
            else
              cutVariables[particle.NDaughters()*nTrackCuts+3] = fMuDst->event()->refMult();
            fCutsNTuple[iNTuple]->Fill(cutVariables.data());
          }
        }
      }
    }
  }
  
  return kStOK;
}

