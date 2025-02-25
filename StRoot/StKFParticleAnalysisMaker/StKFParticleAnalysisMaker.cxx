//*-- Author : Yuri Fisyak 02/02/2016
#include "StKFParticleAnalysisMaker.h"
#include "TDirectory.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TChain.h"
#include "TNtuple.h"
#include "TTree.h"
#include "TSystem.h"
//--- KF particle classes ---
#include "KFVertex.h"
#include "KFParticle.h"
#include "KFParticleSIMD.h"
#include "KFPTrack.h"
#include "KFParticleTopoReconstructor.h"
#include "KFPartEfficiencies.h"
#include "StKFParticleInterface.h"
#include "StKFParticlePerformanceInterface.h"
//--- Pico classes ---
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
//--- Mu classes ---
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
//--- TMVA classes ---
#include "TMVA/GeneticAlgorithm.h"
#include "TMVA/GeneticFitter.h"
#include "TMVA/IFitterTarget.h"
#include "TMVA/Factory.h"
//--- StRefMult class ---
#include "StRefMultCorr/StRefMultCorr.h"
#include "StRefMultCorr/CentralityMaker.h"
ClassImp(StKFParticleAnalysisMaker);

//________________________________________________________________________________
StKFParticleAnalysisMaker::StKFParticleAnalysisMaker(const char *name) : StMaker(name), fNTrackTMVACuts(0), fIsPicoAnalysis(true), fdEdXMode(1), 
  fStoreTmvaNTuples(false), fProcessSignal(false), fCollectTrackHistograms(false), fCollectPIDHistograms(false),fCollectPVHistograms(false),fTMVAselection(false), 
  fFlowAnalysis(false), fFlowChain(NULL), fFlowRunId(-1), fFlowEventId(-1), fCentrality(-1), fFlowFiles(), fFlowMap(), 
  fRunCentralityAnalysis(0), fRefmultCorrUtil(0), fCentralityFile(""), fAnalyseDsPhiPi(false), fDecays(0), fIsProduce3DEfficiencyFile(false), f3DEfficiencyFile(""), 
  fStoreCandidates(false), fPartcileCandidate(), fIsStoreCandidate(KFPartEfficiencies::nParticles, false), fCandidateFileName("candidates.root"), fCandidateFile(nullptr), fCandidatesTree(nullptr)
{
  memset(mBeg,0,mEnd-mBeg+1);
  
  fNTuplePDG[0] = 421;
  fNTuplePDG[1] = 411;
  fNTuplePDG[2] = 431;
  fNTuplePDG[3] = 4122;
  fNTuplePDG[4] = 426;
  fNTuplePDG[5] = 429;
  fNTuplePDG[6] = 521;
  fNTuplePDG[7] = 511;
  
  fNtupleNames[0] = "D0"; 
  fNtupleNames[1] = "DPlus"; 
  fNtupleNames[2] = "Ds"; 
  fNtupleNames[3] = "Lc";
  fNtupleNames[4] = "D0KK";
  fNtupleNames[5] = "D04";
  fNtupleNames[6] = "BPlus";
  fNtupleNames[7] = "B0";
  
  vector<TString> trackCutNames;
  trackCutNames.push_back("pt_");
  trackCutNames.push_back("chi2Primary_");
  trackCutNames.push_back("dEdXPi_");
  trackCutNames.push_back("dEdXK_");
  trackCutNames.push_back("dEdXP_");
  trackCutNames.push_back("ToFPi_");
  trackCutNames.push_back("ToFK_");
  trackCutNames.push_back("ToFP_");
  fNTrackTMVACuts = trackCutNames.size();
  
  fDaughterNames[0].push_back("K");     fDaughterNames[0].push_back("Pi");                                                                              //D0 -> Kpi
  fDaughterNames[1].push_back("K");     fDaughterNames[1].push_back("Pi1");    fDaughterNames[1].push_back("Pi2");                                      //D+ -> Kpipi
  fDaughterNames[2].push_back("KPlus"); fDaughterNames[2].push_back("KMinus"); fDaughterNames[2].push_back("Pi");                                       //Ds -> KKpi
  fDaughterNames[3].push_back("K");     fDaughterNames[3].push_back("Pi");     fDaughterNames[3].push_back("P");                                        //Lc -> pKpi
  fDaughterNames[4].push_back("KPlus"); fDaughterNames[4].push_back("KMinus");                                                                          //D0 -> KK
  fDaughterNames[5].push_back("K");     fDaughterNames[5].push_back("Pi1");    fDaughterNames[5].push_back("Pi2");  fDaughterNames[5].push_back("Pi3"); //D0 -> Kpipipi
  fDaughterNames[6].push_back("PiD");   fDaughterNames[6].push_back("KD");     fDaughterNames[6].push_back("Pi");                                       //B+ -> D0_bpi
  fDaughterNames[7].push_back("Pi1D");  fDaughterNames[7].push_back("KD");     fDaughterNames[7].push_back("Pi2D"); fDaughterNames[7].push_back("Pi");  //B0 -> D-pi+

  for(int iDecay=0; iDecay<fNNTuples; iDecay++)
  {
    for(unsigned int iDaughter=0; iDaughter<fDaughterNames[iDecay].size(); iDaughter++)
    {
      for(int iTrackTMVACut=0; iTrackTMVACut<fNTrackTMVACuts; iTrackTMVACut++)
      {
        if(iDaughter==0 && iTrackTMVACut==0)
          fNtupleCutNames[iDecay] = trackCutNames[iTrackTMVACut];  
        else
          fNtupleCutNames[iDecay] += trackCutNames[iTrackTMVACut];
        fNtupleCutNames[iDecay] += fDaughterNames[iDecay][iDaughter];
        fNtupleCutNames[iDecay] += ":";
      }
    }
    if(iDecay<6)
      fNtupleCutNames[iDecay] += "Chi2NDF:LdL:Chi2Topo:refMult";
    else if(iDecay>=6 && iDecay<8)
    {
      fNtupleCutNames[iDecay] += "Chi2NDF_D:LdL_D:Chi2Topo_D:Chi2NDF:LdL:Chi2Topo:refMult";
    } 
    
    SetTMVABins(iDecay);
  }
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
  if(f) 
  {
    f->cd();
    BookVertexPlots();
    if(fCollectTrackHistograms)
      fStKFParticleInterface->CollectTrackHistograms();
    if(fCollectPIDHistograms)
      fStKFParticleInterface->CollectPIDHistograms();
    if(fCollectPVHistograms)
      fStKFParticleInterface->CollectPVHistograms();
  }
  
  if(fTMVAselection || fStoreTmvaNTuples)
  {
    for(int iReader=0; iReader<fNNTuples; iReader++)
    {
      TString cutName;
      int firstSymbolOfCutName = 0;
      
      int nCuts = 0;
      while(fNtupleCutNames[iReader].Tokenize(cutName,firstSymbolOfCutName,":"))
        nCuts++;
      fTMVAParticleParameters[iReader].resize(nCuts);
    }
  }
  
  if(fTMVAselection)
  {
    for(int iReader=0; iReader<fNNTuples; iReader++)
    {
      const int nCentralityBins = fTMVACentralityBins[iReader].size() - 1;
      const int nPtBins = fTMVAPtBins[iReader].size() - 1;
      
      for(int iCentralityBin=0; iCentralityBin<nCentralityBins; iCentralityBin++)
      {
        for(int iPtBin=0; iPtBin<nPtBins; iPtBin++)
        {
          fTMVAReader[iReader][iCentralityBin][iPtBin] = new TMVA::Reader("Silent");

          TString cutName;
          int firstSymbolOfCutName = 0;      
          unsigned int iCut = 0;
          while(fNtupleCutNames[iReader].Tokenize(cutName,firstSymbolOfCutName,":"))
          {
            fTMVAReader[iReader][iCentralityBin][iPtBin] -> AddVariable( cutName.Data(), &fTMVAParticleParameters[iReader][iCut] );
            iCut++;
            if(iCut == (fTMVAParticleParameters[iReader].size()-1)) break;
          }
          
          fTMVAReader[iReader][iCentralityBin][iPtBin] -> BookMVA("BDT", fTMVACutFile[iReader][iCentralityBin][iPtBin].Data());
        }
      }
    }
  }
      
  //Create file with NTuples for cut optimization
  if(fStoreTmvaNTuples)
  {  
    TFile* curFile = gFile;
    TDirectory* curDirectory = gDirectory;
    for(int iNtuple=0; iNtuple<fNNTuples; iNtuple++)
    {
      TString SignalPrefix = "_Signal";
      if(!fProcessSignal) SignalPrefix = "_BG";
      TString currentNTupleFileName = fNtupleNames[iNtuple]+SignalPrefix+TString(".root");
      fNTupleFile[iNtuple] = new TFile(currentNTupleFileName.Data(),"RECREATE");
      fCutsNTuple[iNtuple] = new TNtuple(fNtupleNames[iNtuple].Data(), fNtupleNames[iNtuple].Data(), fNtupleCutNames[iNtuple].Data());
    }
    gFile = curFile;
    gDirectory = curDirectory;
  }
  
  if(fStoreCandidates)
  {
    TFile* curFile = gFile;
    TDirectory* curDirectory = gDirectory;
    
    fCandidateFile = new TFile(fCandidateFileName.Data(), "RECREATE");
    fCandidatesTree = new TTree("Candidates", "Candidates");
    
    fCandidatesTree->Branch("Candidates", &fPartcileCandidate, 32000, 0);

    gFile = curFile;
    gDirectory = curDirectory;
  }

  // fRefmultCorrUtil = CentralityMaker::instance()->getgRefMultCorr_P16id();
  // fRefmultCorrUtil->setVzForWeight(6, -6.0, 6.0);
  // fRefmultCorrUtil->readScaleForWeight("/gpfs01/star/pwg/pfederic/qVectors/StRoot/StRefMultCorr/macros/weight_grefmult_VpdnoVtx_Vpd5_Run16.txt"); //for new StRefMultCorr, Run16, SL16j
  
  //Initialise the chain with files containing centrality and reaction plane
  if(fFlowAnalysis)
  {
    std::cout << "StKFParticleAnalysisMaker: run flow analysis. Flow file list:"<<std::endl;
    
    fFlowChain = new TChain("mTree");
    for(unsigned int iFlowFile=0; iFlowFile<fFlowFiles.size(); iFlowFile++)
    {
      std::cout << "      " << fFlowFiles[iFlowFile] << std::endl;
      fFlowChain->Add(fFlowFiles[iFlowFile].Data());
    }
    
    fFlowChain->SetBranchStatus("*",0);
    fFlowChain->SetBranchAddress("runid",   &fFlowRunId);   fFlowChain->SetBranchStatus("runid", 1);
    fFlowChain->SetBranchAddress("eventid", &fFlowEventId); fFlowChain->SetBranchStatus("eventid", 1);
    fFlowChain->SetBranchAddress("cent", &fCentrality);  fFlowChain->SetBranchStatus("cent", 1);
    
    std::cout << "StKFParticleAnalysisMaker: number of entries in the flow chain" << fFlowChain->GetEntries() << std::endl;
    for(int iEntry=0; iEntry<fFlowChain->GetEntries(); iEntry++)
    {
      fFlowChain->GetEvent(iEntry);
      fFlowMap[GetUniqueEventId(fFlowRunId, fFlowEventId)] = iEntry;
    }
  }
  return kStOK;
}
//________________________________________________________________________________
Int_t StKFParticleAnalysisMaker::InitRun(Int_t runumber) 
{
//   assert(StPicoDstMaker::instance());
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
  for(unsigned int iDecay=0; iDecay<fDecays.size(); iDecay++)
    fStKFParticleInterface->AddDecayToReconstructionList( fDecays[iDecay] );
  bool storeMCHistograms = false;
  if(!fIsPicoAnalysis && fProcessSignal) storeMCHistograms = true;
  fStKFParticlePerformanceInterface = new StKFParticlePerformanceInterface(fStKFParticleInterface->GetTopoReconstructor(), storeMCHistograms, fIsProduce3DEfficiencyFile);
  if(!f3DEfficiencyFile.IsNull()) {
    fStKFParticlePerformanceInterface->Set3DEfficiency(f3DEfficiencyFile);
  }
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
#ifdef __TFG__VERSION__
    fMuDst = StMuDst::instance();
#else /* !__TFG__VERSION__ */
  StMuDstMaker *muDstMaker = (StMuDstMaker *)GetTopChain()->GetMakerInheritsFrom("StMuDstMaker");
  if (muDstMaker)  fMuDst = muDstMaker->muDst();
#endif /* __TFG__VERSION__ */
    if(! fMuDst) return kStOK;
    else { if(fMuDst->numberOfPrimaryVertices() == 0 ) return kStOK; }
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
  
  vector<int> triggeredTracks;
  bool isGoodEvent = false;
  
  //Process the event
  if(maxGBTrackIndex > 0)
    fStKFParticleInterface->ResizeTrackPidVectors(maxGBTrackIndex+1);
  if(fIsPicoAnalysis)
    isGoodEvent = fStKFParticleInterface->ProcessEvent(fPicoDst, triggeredTracks);
  else
    isGoodEvent = fStKFParticleInterface->ProcessEvent(fMuDst, mcTracks, mcIndices, fProcessSignal);

  //collect histograms
  
  if(isGoodEvent)
  {
    int centralityBin = -1;
    float centralityWeight = 0.;
    
    if(fRunCentralityAnalysis)
    {
      fRefmultCorrUtil->init(fPicoDst->event()->runId());
      if(! (fRefmultCorrUtil->isBadRun(fPicoDst->event()->runId())) )
      {
        fRefmultCorrUtil->initEvent(fPicoDst->event()->grefMult(), fPicoDst->event()->primaryVertex().z(), fPicoDst->event()->ZDCx()) ;
        centralityBin = fRefmultCorrUtil->getCentralityBin9();
        centralityWeight = fRefmultCorrUtil->getWeight();
      }
//       refmultCor = fRefmultCorrUtil->getRefMultCorr();
    }
    
    if(fTMVAselection)
    {
      for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++)
      {
        KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];
              
        for(int iReader=0; iReader<fNNTuples; iReader++)
        {
          if( abs(particle.GetPDG()) == fNTuplePDG[iReader] )
          {
            GetParticleParameters(iReader, particle);
            
            const int iTMVACentralityBin = GetTMVACentralityBin(iReader, centralityBin);
            const int iTMVAPtBin = GetTMVAPtBin(iReader, particle.GetPt());
            
            if(iTMVACentralityBin<0 || iTMVAPtBin<0) 
            {
              fStKFParticleInterface->RemoveParticle(iParticle);
              continue;
            }
            
            if(fTMVAReader[iReader][iTMVACentralityBin][iTMVAPtBin]->EvaluateMVA("BDT") < fTMVACut[iReader][iTMVACentralityBin][iTMVAPtBin])
              fStKFParticleInterface->RemoveParticle(iParticle);
            
            if(fAnalyseDsPhiPi && abs(fStKFParticleInterface->GetParticles()[iParticle].GetPDG()) == 431)
            {              
              KFParticle phi;
              if(particle.GetPDG() == 431)
                phi += fStKFParticleInterface->GetParticles()[particle.DaughterIds()[0]];
              else
                phi += fStKFParticleInterface->GetParticles()[particle.DaughterIds()[1]];
              phi += fStKFParticleInterface->GetParticles()[particle.DaughterIds()[2]];
              float mass = 0.f, dmass = 0.f;
              phi.GetMass(mass, dmass);
              if( fabs(mass - 1.01946) > 0.015)
                fStKFParticleInterface->RemoveParticle(iParticle);
            }
          }
        }
      }      
    }

#if 1
    //clean clusters for hypernuclei candidates
    int nTracks = 0;
    std::vector<bool> isValidTrack(maxGBTrackIndex+1);
    for(int iTrack=0; iTrack<maxGBTrackIndex+1; iTrack++)
      isValidTrack[iTrack] = true;

    for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++) {
      const KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];

      if(particle.NDaughters() == 1)
        nTracks = iParticle + 1;

      if( (abs(particle.GetPDG()) > 3001) && (abs(particle.GetPDG()) <= 3029) &&
          !(particle.GetPDG() == 3008 || particle.GetPDG() == 3009 || particle.GetPDG() == 3010 || particle.GetPDG() == 3011) ) {

        KFParticle cluster = particle;

        std::vector<int> trackIds;
        bool isValidParticle = true;
        for(int iD=0; iD<particle.NDaughters(); iD++) {
          const int daughterId = particle.DaughterIds()[iD];
          const KFParticle daughter = fStKFParticleInterface->GetParticles()[daughterId];
          const int daughterTrackId = daughter.DaughterIds()[0];
          trackIds.push_back(daughterTrackId);
          isValidParticle &= isValidTrack[daughterTrackId];
        }

        for(int iTrack=0; iTrack<nTracks; iTrack++) {
          KFParticle track = fStKFParticleInterface->GetParticles()[iTrack];
          
          //check that not the same track
          bool isSameTrack = false;
          for(unsigned int iD=0; iD<trackIds.size(); iD++)
            if(trackIds[iD] == track.DaughterIds()[0])
              isSameTrack = true;
          if(isSameTrack) continue;  
          
          // use only secondary tracks
          if(particle.NDaughters() == 2) {
            const float chiprimCut = 8;
            if(track.GetDeviationFromVertex(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex()) < chiprimCut) continue;
          }
          
          // should be close to the initial position
          const float chiSec2 = track.GetDeviationFromVertex(particle);
          if(chiSec2 > 18) continue;

          const float dev = track.GetDeviationFromVertex(cluster);
          if( dev > 10. ) continue;

          //add track to cluster
          KFParticle clusterTmp = cluster;
          clusterTmp += track;
          if(clusterTmp.GetChi2()/float(clusterTmp.GetNDF()) < 3) {
            cluster = clusterTmp;
            trackIds.push_back(track.DaughterIds()[0]);
          }
        }
        
        if(cluster.NDaughters() > 5) {        
          fStKFParticleInterface->RemoveParticle(iParticle);
          for(unsigned int iTrackId=0; iTrackId<trackIds.size(); iTrackId++)
            isValidTrack[trackIds[iTrackId]] = false;
        }
      }
    }
#else
    int nTracks = 0;
    for(; nTracks<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); nTracks++) {
      const KFParticle particle = fStKFParticleInterface->GetParticles()[nTracks];

      if(particle.NDaughters() == 1) {
        nTracks++;
        break;
      }
    }
#endif

    // Clean hypernuclei candidates with large coordinate and mass errors
    for(int iParticle=nTracks; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++) {
      const KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];
      
      if( (abs(particle.GetPDG()) > 3001) && (abs(particle.GetPDG()) <= 3103) ) {

        const float dmCut = (particle.NDaughters() == 2) ? 2.5e-3f : 2.0e-3f;
        if(particle.GetErrMass() > dmCut) {
          fStKFParticleInterface->RemoveParticle(iParticle);
        }
        float l, dl;
        particle.GetDistanceToVertexLine(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex(), l, dl);
        if(dl > 3.f) {
          fStKFParticleInterface->RemoveParticle(iParticle);
          continue;
        }
      }
    }

    //clean H3L, H4L, Ln, Lnn
    for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++)
    {
      KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];
      if((abs(particle.GetPDG()) > 3002) && (abs(particle.GetPDG()) <= 3203))
      {        
        for(int iD=0; iD<particle.NDaughters(); iD++)
        {
          const int daughterId = particle.DaughterIds()[iD];
          const KFParticle daughter = fStKFParticleInterface->GetParticles()[daughterId];

          if(abs(daughter.GetPDG())!=211 && daughter.GetP() < 0.5) //TODO remove me
            fStKFParticleInterface->RemoveParticle(iParticle);
        }
      }

      if((abs(particle.GetPDG()) >= 1003004) && (abs(particle.GetPDG()) <= 1003007))
      {
        const float dmCut = 3.0e-3f;
        if(particle.GetErrMass() > dmCut) {
          fStKFParticleInterface->RemoveParticle(iParticle);
        }
      }
    }


    if(fStoreCandidates) {
      KFPartEfficiencies parteff;
      for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++) {
        const KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];
        if(particle.GetPDG() == -1) continue;
        const int particleIndex = parteff.GetParticleIndex(particle.GetPDG());
        if(particleIndex == -1) continue;
        
        if(!fIsStoreCandidate[particleIndex]) continue;

        fPartcileCandidate = fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex();
        fCandidatesTree->Fill();

        fPartcileCandidate = particle;
        fCandidatesTree->Fill();

        if(particle.NDaughters() == 1) continue;
        
        for(int iDaughter=0; iDaughter<particle.NDaughters(); iDaughter++) {
          const KFParticle daughter = fStKFParticleInterface->GetParticles()[particle.DaughterIds()[iDaughter]];
          fPartcileCandidate = daughter;
          fCandidatesTree->Fill();

          if(daughter.NDaughters() > 1)
          {
            for(int iGrandDaughter=0; iGrandDaughter<daughter.NDaughters(); iGrandDaughter++) {
              const KFParticle grandDaughter = fStKFParticleInterface->GetParticles()[daughter.DaughterIds()[iGrandDaughter]];
              fPartcileCandidate = grandDaughter;
              fCandidatesTree->Fill();
            }
          }
        }
      }
    }
    int eventId = -1;
    int runId = -1;
    
    if(fFlowAnalysis)
    {
      if(fIsPicoAnalysis) 
      {
        runId   = fPicoDst->event()->runId();
        eventId = fPicoDst->event()->eventId();
      }
      else
      {
        runId   = fMuDst->event()->runId();
        eventId = fMuDst->event()->eventId();
      }
    
      long entryId = GetUniqueEventId(runId, eventId);
      std::map<long,int>::iterator flowMapIterator = fFlowMap.find(entryId);
      if (flowMapIterator != fFlowMap.end())
      {
        fFlowChain->GetEvent(fFlowMap[GetUniqueEventId(runId, eventId)]);
        centralityBin = fCentrality;
      }
    }
    
    centralityWeight = 1;
    fStKFParticlePerformanceInterface->SetMCTracks(mcTracks);
    fStKFParticlePerformanceInterface->SetMCIndexes(mcIndices);    
    fStKFParticlePerformanceInterface->SetCentralityBin(centralityBin);
    fStKFParticlePerformanceInterface->SetCentralityWeight(centralityWeight);
    Int_t nevent = 100000;
    fStKFParticlePerformanceInterface->SetPrintEffFrequency(nevent);
    fStKFParticlePerformanceInterface->PerformanceAnalysis();
    if(fStoreTmvaNTuples)
    {
      for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++)
      {
        KFParticle particle;
        bool isMCParticle = fStKFParticlePerformanceInterface->GetParticle(particle, iParticle);
              
        if( !( (fProcessSignal && isMCParticle) || (!fProcessSignal && !isMCParticle) ) ) continue;
                  
        for(int iNTuple=0; iNTuple<fNNTuples; iNTuple++)
        {
          if( particle.GetPDG() == fNTuplePDG[iNTuple] )
          {
            GetParticleParameters(iNTuple, particle);
            fCutsNTuple[iNTuple]->Fill(fTMVAParticleParameters[iNTuple].data());
          }
        }
      }
    }
  }
  
  return kStOK;
}

void StKFParticleAnalysisMaker::GetDaughterParameters(const int iReader, int& iDaughterTrack, int& iDaughterParticle, KFParticle& particle)
{
  if(particle.NDaughters() == 1)
  {
    fTMVAParticleParameters[iReader][iDaughterTrack*fNTrackTMVACuts]   = particle.GetPt();
    fTMVAParticleParameters[iReader][iDaughterTrack*fNTrackTMVACuts+1] = particle.GetDeviationFromVertex(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex());
    int trackId = particle.DaughterIds()[0];
    fTMVAParticleParameters[iReader][iDaughterTrack*fNTrackTMVACuts+2]   = fStKFParticleInterface->GetdEdXNSigmaPion(trackId);
    fTMVAParticleParameters[iReader][iDaughterTrack*fNTrackTMVACuts+3]   = fStKFParticleInterface->GetdEdXNSigmaKaon(trackId);
    fTMVAParticleParameters[iReader][iDaughterTrack*fNTrackTMVACuts+4]   = fStKFParticleInterface->GetdEdXNSigmaProton(trackId);
    fTMVAParticleParameters[iReader][iDaughterTrack*fNTrackTMVACuts+5]   = fStKFParticleInterface->GetTofNSigmaPion(trackId);
    fTMVAParticleParameters[iReader][iDaughterTrack*fNTrackTMVACuts+6]   = fStKFParticleInterface->GetTofNSigmaKaon(trackId);
    fTMVAParticleParameters[iReader][iDaughterTrack*fNTrackTMVACuts+7]   = fStKFParticleInterface->GetTofNSigmaProton(trackId);
    
    iDaughterTrack++;
  }
  else if(particle.NDaughters() > 1)
  {
    int order[4] = {0, 1, 2, 3};
    if( particle.GetPDG() == -421 || particle.GetPDG() == -411 || particle.GetPDG() == -431 ||   
        particle.GetPDG() == -429 || particle.GetPDG() == -4122) 
    { 
      order[0] = 1; 
      order[1] = 0; 
    }
    
    for(int iDaughter=0; iDaughter<particle.NDaughters(); iDaughter++)
    {
      const int daughterParticleIndex = particle.DaughterIds()[order[iDaughter]];
      KFParticle daughter = fStKFParticleInterface->GetParticles()[daughterParticleIndex];
      //set pdg for correct order of cuts
      if(particle.GetPDG() == 521 && daughter.GetPDG() == -1) daughter.SetPDG(-421);
      if(particle.GetPDG() ==-521 && daughter.GetPDG() == -1) daughter.SetPDG( 421);
      if(particle.GetPDG() == 511 && daughter.GetPDG() == -1) daughter.SetPDG(-411);
      if(particle.GetPDG() ==-511 && daughter.GetPDG() == -1) daughter.SetPDG( 411);
        
      GetDaughterParameters(iReader, iDaughterTrack, iDaughterParticle, daughter);
    }
    
    fTMVAParticleParameters[iReader][fDaughterNames[iReader].size()*fNTrackTMVACuts + iDaughterParticle*3] = particle.Chi2()/particle.NDF();  
    
    KFParticleSIMD tempSIMDParticle(particle);
    float32_v l,dl;
    KFParticleSIMD pv(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex());
    tempSIMDParticle.GetDistanceToVertexLine(pv, l, dl);
    fTMVAParticleParameters[iReader][fDaughterNames[iReader].size()*fNTrackTMVACuts + iDaughterParticle*3 + 1] = l[0]/dl[0];
    
    tempSIMDParticle.SetProductionVertex(pv);
    fTMVAParticleParameters[iReader][fDaughterNames[iReader].size()*fNTrackTMVACuts + iDaughterParticle*3 + 2] = 
      double(tempSIMDParticle.Chi2()[0])/double(tempSIMDParticle.NDF()[0]);
    
    iDaughterParticle++;
  }
}

void StKFParticleAnalysisMaker::GetParticleParameters(const int iReader, KFParticle& particle)
{
  bool isBMeson = abs(particle.GetPDG()) == 511 || abs(particle.GetPDG()) == 521;
//   if( !isBMeson ) return;
  
  int iDaughterTrack = 0;
  int iDaughterParticle = 0;
  GetDaughterParameters(iReader, iDaughterTrack, iDaughterParticle, particle);

  int nDaughterParticleCut = 0;
  if(isBMeson) nDaughterParticleCut += 3;
  nDaughterParticleCut += fDaughterNames[iReader].size()*fNTrackTMVACuts;
  
  fTMVAParticleParameters[iReader][nDaughterParticleCut]   = particle.Chi2()/particle.NDF();  
  
  KFParticleSIMD tempSIMDParticle(particle);
  float32_v l,dl;
  KFParticleSIMD pv(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex());
  tempSIMDParticle.GetDistanceToVertexLine(pv, l, dl);
  fTMVAParticleParameters[iReader][nDaughterParticleCut + 1] = l[0]/dl[0];
  
  tempSIMDParticle.SetProductionVertex(pv);
  fTMVAParticleParameters[iReader][nDaughterParticleCut + 2] = double(tempSIMDParticle.Chi2()[0])/double(tempSIMDParticle.NDF()[0]);

  if(fIsPicoAnalysis)
    fTMVAParticleParameters[iReader][nDaughterParticleCut + 3] = fPicoDst->event()->refMult();
  else
    fTMVAParticleParameters[iReader][nDaughterParticleCut + 3] = fMuDst->event()->refMult();
}

Int_t StKFParticleAnalysisMaker::Finish() 
{
  if(fStoreTmvaNTuples)
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
  
  if(fStoreCandidates)
  {
    TFile* curFile = gFile;
    TDirectory* curDirectory = gDirectory;
    fCandidateFile->cd();
    fCandidatesTree->Write();
    gFile = curFile;
    gDirectory = curDirectory;
  }
  
  return kStOK;
}

long StKFParticleAnalysisMaker::GetUniqueEventId(const int iRun, const int iEvent) const
{
  long id = 1000000000;
  return id*(iRun%1000) + iEvent;
}

int StKFParticleAnalysisMaker::GetTMVACentralityBin(int iReader, int centrality)
{
  for(unsigned int iBin=0; iBin<fTMVACentralityBins[iReader].size()-1; iBin++)
    if(centrality >= fTMVACentralityBins[iReader][iBin] && centrality < fTMVACentralityBins[iReader][iBin+1])
      return iBin;
  return -1;
}

int StKFParticleAnalysisMaker::GetTMVAPtBin(int iReader, double pt)
{
  for(unsigned int iBin=0; iBin<fTMVAPtBins[iReader].size()-1; iBin++)
    if(pt >= fTMVAPtBins[iReader][iBin] && pt < fTMVAPtBins[iReader][iBin+1])
      return iBin;
  return -1;
}

void StKFParticleAnalysisMaker::SetTMVACentralityBins(int iReader, TString bins)
{
  fTMVACentralityBins[iReader].clear();
  TString value; int firstSymbol = 0;      
  while(bins.Tokenize(value,firstSymbol,":"))
    fTMVACentralityBins[iReader].push_back(value.Atoi());
}

void StKFParticleAnalysisMaker::SetTMVAPtBins(int iReader, TString bins)
{
  fTMVAPtBins[iReader].clear();
  TString value; int firstSymbol = 0;      
  while(bins.Tokenize(value,firstSymbol,":"))
    fTMVAPtBins[iReader].push_back(value.Atof());
}

void StKFParticleAnalysisMaker::SetTMVABins(int iReader, TString centralityBins, TString ptBins)
{
  SetTMVACentralityBins(iReader, centralityBins);
  SetTMVAPtBins(iReader, ptBins);
  
  const int nCentralityBins = fTMVACentralityBins[iReader].size() - 1;
  const int nPtBins = fTMVAPtBins[iReader].size() - 1;
  
  fTMVACutFile[iReader].resize(nCentralityBins);
  fTMVACut[iReader].resize(nCentralityBins);
  fTMVAReader[iReader].resize(nCentralityBins);
  
  for(int iCentralityBin=0; iCentralityBin<nCentralityBins; iCentralityBin++)
  {
    fTMVACutFile[iReader][iCentralityBin].resize(nPtBins);
    fTMVACut[iReader][iCentralityBin].resize(nPtBins);
    fTMVAReader[iReader][iCentralityBin].resize(nPtBins);
  }
}

void StKFParticleAnalysisMaker::AddDecayToReconstructionList( int iDecay ) { fDecays.push_back(iDecay); }

void StKFParticleAnalysisMaker::AddCandidateToStore(int pdg) {
  KFPartEfficiencies parteff;
  const int particleIndex = parteff.GetParticleIndex(pdg);
  fIsStoreCandidate[particleIndex] = true;
  fStoreCandidates = true;
}
