//*-- Author : Yuri Fisyak 02/02/2016
#include "StKFParticleAnalysisMaker.h"
#include "TDirectory.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TChain.h"
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
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
//--- Mu classes ---
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
#include "StDetectorDbMaker/St_beamInfoC.h"
ClassImp(StKFParticleAnalysisMaker);

//________________________________________________________________________________
StKFParticleAnalysisMaker::StKFParticleAnalysisMaker(const char *name) : StMaker(name), fNTrackTMVACuts(0), fIsPicoAnalysis(true), fdEdXMode(1), 
  fStoreTmvaNTuples(false), fProcessSignal(false), fCollectTrackHistograms(false), fCollectPIDHistograms(false),fTMVAselection(false), 
  fFlowAnalysis(false), fFlowChain(NULL), fFlowRunId(-1), fFlowEventId(-1), fCentrality(-1), fFlowFiles(), fFlowMap(), 
  fRunCentralityAnalysis(0), fRefmultCorrUtil(0), fCentralityFile(""), fAnalyseDsPhiPi(false), fDecays(0)
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
#if 0
  const Char_t *path  = "/gpfs01/star/pwg/pfederic/qVectors/StRoot/StRefMultCorr/macros";
  const Char_t *File  = "weight_grefmult_VpdnoVtx_Vpd5_Run16.txt";
  Char_t *file = gSystem->Which(path,File,kReadPermission);
  if (! file) {
    Warning("StKFParticleAnalysisMaker::Init","File %s has not been found in path %s. Ignore ScaleForWeight",File,path);
  } else {
  fRefmultCorrUtil = CentralityMaker::instance()->getgRefMultCorr_P16id();
  fRefmultCorrUtil->setVzForWeight(6, -6.0, 6.0);
    fRefmultCorrUtil->readScaleForWeight("/gpfs01/star/pwg/pfederic/qVectors/StRoot/StRefMultCorr/macros/weight_grefmult_VpdnoVtx_Vpd5_Run16.txt"); //for new StRefMultCorr, Run16, SL16j
  }
  delete [] file;
#endif
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
  return StMaker::Init();
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
  StKFParticleInterface::instance()->SetFixedTarget(St_beamInfoC::instance()->IsFixedTarget());
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
//   fStKFParticleInterface->SetChiPrimaryCut(10);
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
  if(maxGBTrackIndex > 0)
    fStKFParticleInterface->ResizeTrackPidVectors(maxGBTrackIndex+1);
  if(fIsPicoAnalysis)
    isGoodEvent = fStKFParticleInterface->ProcessEvent(fPicoDst, triggeredTracks);
  else
    isGoodEvent = fStKFParticleInterface->ProcessEvent(fMuDst, mcTracks, mcIndices, fProcessSignal);

//   bool openCharmTrigger = false;
//   if(isGoodEvent) openCharmTrigger =  fStKFParticleInterface->OpenCharmTrigger();
//   fStKFParticleInterface->OpenCharmTriggerCompression(triggeredTracks.size(), fPicoDst->numberOfTracks(), openCharmTrigger);
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

#if 0
    //clean clusters
    std::vector< std::vector<int> > vertexIds(fStKFParticlePerformanceInterface->GetNReconstructedParticles());
    
    for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++) {
      KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];
      if( (abs(particle.GetPDG()) > 3001) && (abs(particle.GetPDG()) < 3028) ) {
        for(int iD=0; iD<particle.NDaughters(); iD++) {
          const int daughterId = particle.DaughterIds()[iD];
          const KFParticle daughter = fStKFParticleInterface->GetParticles()[daughterId];
          if(abs(daughter.GetPDG()) > 1000000000) {
            vertexIds[daughterId].push_back(iParticle);
          }
        }
      }
    }
    
    for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++) {
      const int nVertices = vertexIds[iParticle].size();
      if(nVertices < 2) continue;
      //collect indices of all tracks
      vector<int> trackIndices;
      trackIndices.push_back(iParticle);
      for(int iVertex=0; iVertex<nVertices; iVertex++) {
        const int iCurrentVertex = vertexIds[iParticle][iVertex];
        const KFParticle currentVertex = fStKFParticleInterface->GetParticles()[iCurrentVertex];
        for(int iD=0; iD<currentVertex.NDaughters(); iD++) {
          const int daughterId = currentVertex.DaughterIds()[iD];
          bool isNewTrack = true;
          for(unsigned int iTrack=0; iTrack<trackIndices.size(); iTrack++) {
            if(daughterId == trackIndices[iTrack]) {
              isNewTrack = false;
              break;
            }
          }
          if(isNewTrack)
            trackIndices.push_back(daughterId);
        }
      }
      //construct a cluster from these tracks
      KFParticle cluster;
      cluster += fStKFParticleInterface->GetParticles()[trackIndices[0]];
      for(unsigned int iTrack=1; iTrack<trackIndices.size(); iTrack++) {
        KFParticle clusterTmp = cluster;
        clusterTmp += fStKFParticleInterface->GetParticles()[trackIndices[iTrack]];
        if(clusterTmp.GetChi2()/float(clusterTmp.GetNDF()) < 3)
          cluster = clusterTmp;
      }
      //if cluster contains more then 3 tracks - remove all corresponding particles
      if(cluster.NDaughters() > 3) {
        for(int iVertex=0; iVertex<nVertices; iVertex++) {
          const int iCurrentVertex = vertexIds[iParticle][iVertex];
          fStKFParticleInterface->RemoveParticle(iCurrentVertex);
        }
      }
    }
#endif    
    
    
    
    
//rotational background
#if 0
    const int nParticles0 = fStKFParticleInterface->GetParticles().size();
    for(int iParticle=0; iParticle<nParticles0; iParticle++) {
      KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];
      if(particle.GetPDG()==3006 || 
         particle.GetPDG()==3007 || 
         particle.GetPDG()==3012 || 
         particle.GetPDG()==3013)
      {
        KFParticle pion     = fStKFParticleInterface->GetParticles()[particle.DaughterIds()[0]];
        KFParticle fragment = fStKFParticleInterface->GetParticles()[particle.DaughterIds()[1]];
        KFParticle proton   = fStKFParticleInterface->GetParticles()[particle.DaughterIds()[2]];
        
        KFParticle ppi;
        ppi += pion;
        ppi += proton;
        
        fStKFParticleInterface->RemoveParticle(iParticle);
        
        float vtx[3] = { ppi.X(), ppi.Y(), ppi.Z() };
//         const int nRotate = 3;
//         for(int i=1; i<nRotate+1; i++) 
        {
          KFParticle fragment_copy = fragment;
          KFParticle ppi_copy = ppi;
          
          fragment_copy.TransportToPoint(vtx);
//           fragment_copy.Rotate(2.*TMath::Pi()/(nRotate+1)*i, particle);
          fragment_copy.Rotate(TMath::Pi(), particle);
          fragment_copy.SetId(fStKFParticleInterface->GetParticles().size());
          fStKFParticleInterface->AddParticle(fragment_copy);         
          
          ppi_copy += fragment_copy;
          ppi_copy.SetPDG(particle.GetPDG());
          ppi_copy.SetId(fStKFParticleInterface->GetParticles().size());
          
          ppi_copy.CleanDaughtersId();
          ppi_copy.AddDaughterId(pion.Id());
          ppi_copy.AddDaughterId(fragment_copy.Id());
          ppi_copy.AddDaughterId(proton.Id());
          
          fStKFParticleInterface->AddParticle(ppi_copy);          
        }
      }
    }
#endif
//check resonans
#if 0
    const int nParticles0 = fStKFParticleInterface->GetParticles().size();
    for(int iParticle=0; iParticle<nParticles0; iParticle++) {
      KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];
      if(particle.GetPDG()==3006 || 
         particle.GetPDG()==3007 || 
         particle.GetPDG()==3012 || 
         particle.GetPDG()==3013)
      {
        KFParticle pion     = fStKFParticleInterface->GetParticles()[particle.DaughterIds()[0]];
        KFParticle fragment = fStKFParticleInterface->GetParticles()[particle.DaughterIds()[1]];
        KFParticle proton   = fStKFParticleInterface->GetParticles()[particle.DaughterIds()[2]];

        float m, dm, l, dl;
        bool ok = true;
        
        KFParticle ppi;
        ppi += pion;
        ppi += proton;
        ppi.GetMass(m,dm);
//         ppi.GetDistanceToVertexLine(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex(), l, dl);
//         ok &= l/dl > 5.f;
//         
//         KFParticle pf;
//         pf += proton;
//         pf += fragment;
//         pf.GetDistanceToVertexLine(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex(), l, dl);
//         ok &= l/dl > 5.f;
//         
//         KFParticle fpi;
//         fpi += fragment;
//         fpi += pion;
//         fpi.GetDistanceToVertexLine(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex(), l, dl);
//         ok &= l/dl > 5.f;
        
        if(fabs(m - 1.115683f) < 3.f*0.0015f){
          ppi.SetNonlinearMassConstraint(1.115683);
          
          KFParticle resonance;
          resonance += ppi;
          resonance += fragment;
          
          resonance.GetDistanceToVertexLine(fStKFParticleInterface->GetTopoReconstructor()->GetPrimVertex(), l, dl);
          ok &= l/dl > 3.f;          
        }
        
        if(!ok)
          fStKFParticleInterface->RemoveParticle(iParticle);
      }
    }
#endif

    int nHe5L = 0;
    int nHe4L = 0;
    //clean H3L, H4L, Ln, Lnn
    for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++)
    {
      KFParticle particle = fStKFParticleInterface->GetParticles()[iParticle];
      if( abs(particle.GetPDG())==3003 || abs(particle.GetPDG())==3103 || abs(particle.GetPDG())==3004 || abs(particle.GetPDG())==3005)
      {        
        for(int iD=0; iD<particle.NDaughters(); iD++)
        {
          const int daughterId = particle.DaughterIds()[iD];
          const KFParticle daughter = fStKFParticleInterface->GetParticles()[daughterId];
          if(abs(daughter.GetPDG())==211 && daughter.GetP() > 0.7)
            fStKFParticleInterface->RemoveParticle(iParticle);
          if(abs(daughter.GetPDG())!=211 && daughter.GetP() < 0.5) //TODO remove me
            fStKFParticleInterface->RemoveParticle(iParticle);
        }
        
//         float l = sqrt(particle.X()*particle.X() + particle.Y()*particle.Y() + particle.Z()*particle.Z());
        float r = sqrt(particle.X()*particle.X() + particle.Y()*particle.Y());
        if(r > 50)// || (r>2.5 && r<3.6) || (r>7.5&&r<8.8))
          fStKFParticleInterface->RemoveParticle(iParticle);
      }

#if 0
      //clean He4L and He5L
      if( (abs(particle.GetPDG()) == 3006) || 
          (abs(particle.GetPDG()) == 3007) ||
          (abs(particle.GetPDG()) == 3027) ||
          
          (abs(particle.GetPDG()) == 3012) || 
          (abs(particle.GetPDG()) == 3013) || 
          (abs(particle.GetPDG()) == 3014) || 
          (abs(particle.GetPDG()) == 3015) || 
          (abs(particle.GetPDG()) == 3017) || 
          (abs(particle.GetPDG()) == 3018) || 
          (abs(particle.GetPDG()) == 3020) || 
          (abs(particle.GetPDG()) == 3021) || 
          (abs(particle.GetPDG()) == 3023) || 
          (abs(particle.GetPDG()) == 3024) || 
          (abs(particle.GetPDG()) == 3026)
        ) 
      {
        const int iFragment1 = particle.DaughterIds()[1];
        const KFParticle fragment1 = fStKFParticleInterface->GetParticles()[iFragment1];
        const int iFragment2 = particle.DaughterIds()[2];
        const KFParticle fragment2 = fStKFParticleInterface->GetParticles()[iFragment2];
        const KFParticle* vDaughters[2] = {&fragment1, &fragment2};
        KFParticle resonance;
        resonance.Construct(vDaughters, 2, 0);
        float massResonance, massResonanceError;
        resonance.GetMass(massResonance, massResonanceError);
        if( (abs(particle.GetPDG()) == 3006) && massResonance > 3.777)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3007) && massResonance > 4.675)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3027) && fabs(massResonance - 7.4548501) > 0.008)
          fStKFParticleInterface->RemoveParticle(iParticle);
        
        
//         if( (abs(particle.GetPDG()) == 3012) && massResonance > 2.84)
        if( (abs(particle.GetPDG()) == 3012) && massResonance > 2.835)
          fStKFParticleInterface->RemoveParticle(iParticle);
//         if( (abs(particle.GetPDG()) == 3013) && massResonance > 3.76)
        if( (abs(particle.GetPDG()) == 3013) && massResonance > 3.755)
          fStKFParticleInterface->RemoveParticle(iParticle);        
        if( (abs(particle.GetPDG()) == 3014) && massResonance > 3.775)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3015) && massResonance > 4.71)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3017) && massResonance > 5.65)
          fStKFParticleInterface->RemoveParticle(iParticle); 
        if( (abs(particle.GetPDG()) == 3018) && massResonance > 4.72)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3020) && massResonance > 5.651)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3021) && massResonance > 5.615)
          fStKFParticleInterface->RemoveParticle(iParticle); 
        if( (abs(particle.GetPDG()) == 3023) && massResonance > 6.55)
          fStKFParticleInterface->RemoveParticle(iParticle); 
        if( (abs(particle.GetPDG()) == 3024) && (massResonance < 5.62 || massResonance > 5.65))
          fStKFParticleInterface->RemoveParticle(iParticle); 
        if( (abs(particle.GetPDG()) == 3026) && (massResonance < 6.536 || massResonance > 6.552))
          fStKFParticleInterface->RemoveParticle(iParticle);
      }
#endif

#if 0
      //clean He4L and He5L
      if( (abs(particle.GetPDG()) == 3006) || 
          (abs(particle.GetPDG()) == 3007) ||
          (abs(particle.GetPDG()) == 3027) ||
          
          (abs(particle.GetPDG()) == 3012) || 
          (abs(particle.GetPDG()) == 3013) || 
          (abs(particle.GetPDG()) == 3014) || 
          (abs(particle.GetPDG()) == 3015) || 
          (abs(particle.GetPDG()) == 3017) || 
          (abs(particle.GetPDG()) == 3018) || 
          (abs(particle.GetPDG()) == 3020) || 
          (abs(particle.GetPDG()) == 3021) || 
          (abs(particle.GetPDG()) == 3023) || 
          (abs(particle.GetPDG()) == 3024) || 
          (abs(particle.GetPDG()) == 3026)
        ) 
      {
        const int iFragment1 = particle.DaughterIds()[1];
        const KFParticle fragment1 = fStKFParticleInterface->GetParticles()[iFragment1];
        const int iFragment2 = particle.DaughterIds()[2];
        const KFParticle fragment2 = fStKFParticleInterface->GetParticles()[iFragment2];
        const KFParticle* vDaughters[2] = {&fragment1, &fragment2};
        KFParticle resonance;
        resonance.Construct(vDaughters, 2, 0);
        float massResonance, massResonanceError;
        resonance.GetMass(massResonance, massResonanceError);
        if( (abs(particle.GetPDG()) == 3006) && massResonance > 3.76)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3007) && massResonance > 4.675)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3027) && fabs(massResonance - 7.4548501) > 0.008)
          fStKFParticleInterface->RemoveParticle(iParticle);
        
        if( (abs(particle.GetPDG()) == 3012) && massResonance > 2.825)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3013) && massResonance > 3.755)
          fStKFParticleInterface->RemoveParticle(iParticle);        
        if( (abs(particle.GetPDG()) == 3014) && massResonance > 3.762)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3015) && massResonance > 4.71)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3017) && massResonance > 5.65)
          fStKFParticleInterface->RemoveParticle(iParticle); 
        if( (abs(particle.GetPDG()) == 3018) && massResonance > 4.72)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3020) && massResonance > 5.651)
          fStKFParticleInterface->RemoveParticle(iParticle);
        if( (abs(particle.GetPDG()) == 3021) && massResonance > 5.615)
          fStKFParticleInterface->RemoveParticle(iParticle); 
        if( (abs(particle.GetPDG()) == 3023) && massResonance > 6.55)
          fStKFParticleInterface->RemoveParticle(iParticle); 
        if( (abs(particle.GetPDG()) == 3024) && (massResonance < 5.62 || massResonance > 5.65))
          fStKFParticleInterface->RemoveParticle(iParticle); 
        if( (abs(particle.GetPDG()) == 3026) && (massResonance < 6.536 || massResonance > 6.552))
          fStKFParticleInterface->RemoveParticle(iParticle);
      }
#endif

#if 0
      if( (abs(particle.GetPDG()) == 3006) || 
          (abs(particle.GetPDG()) == 3007) ||
          (abs(particle.GetPDG()) == 3012) ||
          (abs(particle.GetPDG()) == 3013) ||
          (abs(particle.GetPDG()) == 3014) ||
          (abs(particle.GetPDG()) == 3015) ||
          (abs(particle.GetPDG()) == 3017) ||
          (abs(particle.GetPDG()) == 3018) ||
          (abs(particle.GetPDG()) == 3020) ||
          (abs(particle.GetPDG()) == 3021) ||
          (abs(particle.GetPDG()) == 3023) ||
          (abs(particle.GetPDG()) == 3024) ||
          (abs(particle.GetPDG()) == 3026) ||
          (abs(particle.GetPDG()) == 3027) )
      {
        //clean high momentum pions
        for(int iD=0; iD<particle.NDaughters(); iD++)
        {
          const int daughterId = particle.DaughterIds()[iD];
          const KFParticle daughter = fStKFParticleInterface->GetParticles()[daughterId];
          if(abs(daughter.GetPDG())==211 && daughter.GetP() > 0.7)
            fStKFParticleInterface->RemoveParticle(iParticle);
        }
        //clean gamma and clones
        for(int iD0=0; iD0<particle.NDaughters(); iD0++)
        {
          for(int iD1=0; iD1<iD0; iD1++)
          {
            int index0 = particle.DaughterIds()[iD0];
            int index1 = particle.DaughterIds()[iD1];
            KFParticle d0 = fStKFParticleInterface->GetParticles()[index0];
            KFParticle d1 = fStKFParticleInterface->GetParticles()[index1];
            float vertex[3] = {particle.GetX(), particle.GetY(), particle.GetZ()};
            d0.TransportToPoint(vertex);
            d1.TransportToPoint(vertex);
            float qtAlpha[2];
            KFParticle::GetArmenterosPodolanski(d0, d1, qtAlpha );
            if(qtAlpha[0] < 0.005)
              fStKFParticleInterface->RemoveParticle(iParticle);
          }
        }

        if(particle.NDaughters() == 3) {
          const KFParticle d[3] = {
            fStKFParticleInterface->GetParticles()[particle.DaughterIds()[0]],
            fStKFParticleInterface->GetParticles()[particle.DaughterIds()[1]],
            fStKFParticleInterface->GetParticles()[particle.DaughterIds()[2]]
          };
          KFParticle v[3];
          int index[3][2] = { {1,2}, {0,2}, {0,1} }; 
          
          bool ok = true;
          for(int iD=0; iD<3; iD++){
            
            const KFParticle* vd[2] = {&d[index[iD][0]], &d[index[iD][1]]};
            v[iD].Construct(vd, 2);

            float q1q2 = vd[0]->Px()*vd[1]->Px() + vd[0]->Py()*vd[1]->Py() + vd[0]->Pz()*vd[1]->Pz();
            float q12  = vd[0]->Px()*vd[0]->Px() + vd[0]->Py()*vd[0]->Py() + vd[0]->Pz()*vd[0]->Pz();
            float q22  = vd[1]->Px()*vd[1]->Px() + vd[1]->Py()*vd[1]->Py() + vd[1]->Pz()*vd[1]->Pz();
            ok &= q1q2 > -q12;
            ok &= q1q2 > -q22;
            

            float p1p2 = d[iD].Px()*v[iD].Px() + d[iD].Py()*v[iD].Py() + d[iD].Pz()*v[iD].Pz();
            float p12  = d[iD].Px()*d[iD].Px() + d[iD].Py()*d[iD].Py() + d[iD].Pz()*d[iD].Pz();
            float p22  = v[iD].Px()*v[iD].Px() + v[iD].Py()*v[iD].Py() + v[iD].Pz()*v[iD].Pz();
            ok &= p1p2 > -p12;
            ok &= p1p2 > -p22;
            
            v[iD] += d[iD];
            ok &= v[iD].Chi2()/float(v[iD].NDF()) < 3;
            
            float m=0.f, dm=1e6f;
            ok &= (v[iD].GetMass(m, dm) == 0);
            
            float l=0.f, dl=1e6f;
            v[iD].GetDistanceToVertexLine(particle, l, dl);
            ok &= l/dl < 3.f;
          }
          
          if(!ok)
            fStKFParticleInterface->RemoveParticle(iParticle);
        }
      }
#endif
      
      if(particle.GetPDG() == 3006) nHe4L++;
      if(particle.GetPDG() == 3007) nHe5L++;
    }

//     if(nHe5L>10 || nHe4L>10) return kStOK;
    
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

//_____________________________________________________________________________
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
    float_v l,dl;
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
  float_v l,dl;
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

void StKFParticleAnalysisMaker::GetParticles(std::vector<KFParticle>* particles, const int pdg) const
{
  for(int iParticle=0; iParticle<fStKFParticlePerformanceInterface->GetNReconstructedParticles(); iParticle++)
  {
    const KFParticle& particle = fStKFParticleInterface->GetParticles()[iParticle];
    if(particle.GetPDG() == pdg)
      particles->push_back(particle);
  }
}

void StKFParticleAnalysisMaker::AddDecayToReconstructionList( int iDecay ) { fDecays.push_back(iDecay); }
