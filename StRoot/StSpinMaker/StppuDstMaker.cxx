/***************************************************************************
 *
 * $Id: StppuDstMaker.cxx,v 1.9 2003/09/11 05:49:22 perev Exp $
 * 
 * Author: Akio Ogawa June 2001
 ***************************************************************************
 *
 * Description:  TTree uDst for spin-pp
 *
 ***************************************************************************
 *
 * $Log: StppuDstMaker.cxx,v $
 * Revision 1.9  2003/09/11 05:49:22  perev
 * ansi corrs
 *
 * Revision 1.8  2003/09/02 17:59:02  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.7  2003/08/04 19:25:10  perev
 * warnOff
 *
 * Revision 1.6  2003/05/14 18:00:25  akio
 * New addition for 2003 data ntuple prodction
 * Also fix a problem with MuTrack creating from StEvent tracks.
 *
 * Revision 1.5  2003/02/04 21:57:10  akio
 * Improvments on pi0 reconstruction code and ntuple
 *
 * Revision 1.4  2002/12/04 20:28:09  thenry
 * StppuDstMaker was modified to allow multiple jet analysis modules to be
 * run simultaneosly with various parameters while the Maker loads the events
 * and analyses them.  Four different jet analyzers exist:
 *
 * Konstanin's Analyzers:
 *     Kt type: StppKonstKtJetAnalyzer
 *     Cone type: StppKonstConeJetAnalyzer
 *
 * Mike's Analyzers:
 *     Kt type: StppMikeKtJetAnalyzer
 *     Cone type: StppMikeConeJetAnalyzer
 *
 * These modules all require the StJetFinder modules.
 *
 * Revision 1.3  2002/06/24 13:22:59  akio
 * numerous bug fix & updates
 *
 * Revision 1.2  2002/02/11 20:30:48  akio
 * Many updates, including very first version of jet finder.
 *
 * Revision 1.1  2002/01/16 20:22:54  akio
 * First version
 *
 **************************************************************************/
#include <string.h>
#include <Stiostream.h>

#include "TFile.h"
#include "TTree.h"

#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/EMC/StEmcMicroEvent.h"

#include "StppuDstMaker.h"
#include "StppEvent.h"
#include "StppGeant.h"
#include "StTriggerDetectorCollection.h"
#include "StBbcTriggerDetector.h"
#include "StFpdCollection.h"
#include "St_trg_Maker/St_trg_Maker.h"
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"

#ifdef _2002ntuple_
extern "C" void initfpdpi0_(const char*);
extern "C" void finishfpdpi0_();
#endif
#ifdef _2003ntuple_
extern "C" void initntp2003_(const char*);
extern "C" void finishntp2003_();
#endif

ClassImp(StppuDstMaker)
  
StppuDstMaker::StppuDstMaker(const Char_t *name) 
  : StMaker(name), mGoodCounter(0), mBadCounter(0){
    printf("***StppuDstMaker: Constractor*** name=%s\n",name);
    numJetBranches = 0;
    typedef StppJetAnalyzer* StppJetAnalyzerP;
    jetBranches = new StppJetAnalyzerP[MAXANALYZERS];
    typedef char* charP;
    names = new charP[MAXANALYZERS];
    infoLevel = 0;
    mudst=0;
    saveEventWithNoJets = true;
    ppEvent=0;
#ifdef _EMC_
    storeEMC = true;
#endif
}

StppuDstMaker::~StppuDstMaker(){
  delete [] jetBranches;
  delete [] names;
}

void StppuDstMaker::SetSaveEventWithNoJets(bool saveIt)
{
  saveEventWithNoJets = saveIt;
}

void StppuDstMaker::addAnalyzer(StppJetAnalyzer* a, const char * name)
{
    char* branchName = new char[strlen(name)+2];
    strcpy(branchName, name);
    //this worked for Thomas, but not for Mike
    //strcat(branchName, ".");
    
    names[numJetBranches] = branchName;
    jetBranches[numJetBranches] = a;
    numJetBranches++;
}


Int_t StppuDstMaker::Init(){
    return StMaker::Init();
}

Int_t StppuDstMaker::Init(const Char_t *filename) 
{
    printf("***StppuDstMaker::Init***\n");
    // creating uDst file name
    TString uDstFileName(filename);
    StIOMaker* pIOMaker = (StIOMaker*)GetMaker("IO");
    if(!pIOMaker){ pIOMaker = (StIOMaker*)GetMaker("inputStream"); }
    if(pIOMaker){
	uDstFileName = pIOMaker->GetFile() ;
	char* ccc = "/" ;
	Ssiz_t slashPosition = uDstFileName.Last(*ccc) ;
	if ( slashPosition != -1 &&
	     slashPosition < uDstFileName.Length() )uDstFileName.Remove(0,slashPosition+1);
    }
    uDstFileName.ReplaceAll(".dst.root",".spinDst.root");
    uDstFileName.ReplaceAll(".event.root",".spinDst.root");
    uDstFileName.ReplaceAll(".MuDst.root",".spinDst.root");
    uDstFileName.ReplaceAll(".daq",".spinDst.root");
    uDstFileName.ReplaceAll(":MuDst",".spinDst.root");
    cout << "StppuDstMaker: spiunDst output file: " << uDstFileName << endl;
    
    //open udst file
    m_outfile = new TFile(uDstFileName,"recreate");
    //  m_outfile->SetFormat(1);
    m_outfile->SetCompressionLevel(1);
    
    //create udst & its branches
    ppuDst  = new TTree("uDst","ppSpinuDst",99);
    printf("***StppuDstMaker::Init*** Creating TTree\n");
    ppEvent = new StppEvent(); ppEvent->setInfoLevel(infoLevel);
    printf("***StppuDstMaker::Init*** Creating StppEvent\n");
    ppuDst->Branch ("Event","StppEvent",&ppEvent,64000,99);
    for(int i = 0; i < numJetBranches; i++)
	{
	    jetBranches[i]->addBranch(names[i], ppuDst);
	    ppEvent->addAnalyzer(jetBranches[i]);
	}
#ifdef _GEANT_
    ppGeant = new StppGeant(); ppGeant->setInfoLevel(infoLevel);
    ppuDst->Branch ("Geant","StppGeant",&ppGeant,64000,99);
#endif
#ifdef _BBC_data_
    bbc     = new StBbcTriggerDetector();
    ppuDst->Branch ("Bbc","StBbcTriggerDetector",&bbc,64000,99);
#endif
#ifdef _FPD_data_
    fpd     = new StFpdCollection();
    ppuDst->Branch ("Fpd","StFpdCollection",&fpd,64000,99);
#endif
#ifdef _EMC_
    if(storeEMC)
      {
	emcEvent = new StEmcMicroEvent();  //will get this data from the EmcMicroEvent
	ppuDst->Branch ("EmcMuEvent", "StEmcMicroEvent", &emcEvent, 64000, 99);
      }
#endif
#ifdef _EMC_CLUSTERS_
    emcClusters[0]=new StEmcClusterCollection();
    emcClusters[1]=new StEmcClusterCollection();
    emcClusters[2]=new StEmcClusterCollection();
    //  cout << "********* adding emc cluster branch" << endl; 
    //  ppuDst->Branch ("EmcClusterTower","StEmcClusterCollection",&emcClusters[0],64000,99);
    //  ppuDst->Branch ("EmcClusterSMDX","StEmcClusterCollection",&emcClusters[1],64000,99);
    //  ppuDst->Branch ("EmcClusterSMDY","StEmcClusterCollection",&emcClusters[2],64000,99);
#endif
#ifdef _EMC_POINTS_
    ppEmcPoints = new StppEmcPoints();
    //emcPoints = new TClonesArray("EmcPoints", MAX_EMC_POINTS);
    cout << "*********  adding stppemcpoint branch" << endl; 
    ppuDst->Branch ("EmcPoints","StppEmcPoints",&ppEmcPoints,64000,99);
#endif  
    
    uDstFileName.ReplaceAll(".spinDst.root",".spinDst.ntp");
#ifdef _2002ntuple_
    initfpdpi0_(uDstFileName.Data());    
#endif
#ifdef _2003ntuple_
    initntp2003_(uDstFileName.Data());
#endif
    return 0;
}

void StppuDstMaker::Clear(Option_t *opt){  
  StMaker::Clear();
}

Int_t StppuDstMaker::Make() {
  cout <<" Start StppuDstMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

  if(ppEvent==0) Init("spindst");

  ppEvent->clear();
#ifdef _GEANT_
  ppGeant->clear();
#endif
#ifdef _EMC_POINTS_
  //emcPoints->Clear();
  ppEmcPoints->Clear();
#endif
  
  // Get MuDst, or if it's not there, get StEvent
  StEvent* event;
  if(mudst) {
    ppEvent->setMuDst(mudst);
    event = 0;
  }
  else{
    event = (StEvent *)GetInputDS("StEvent");
    if(!event){
      mBadCounter++;
      return kStOK;
    }
  }
  
  // fill ppEvent 
  int res;
  res = ppEvent->fill(event);
  if(res<0){
    mBadCounter++;
    return kStOK;
  }
  
  // Get geant info, if any, and fill geant branch
#ifdef _GEANT_
  TDataSet *geantBranch = GetInputDS("geantBranch");
  if(geantBranch){
    ppGeant->fill(geantBranch);
  }
#endif
  
  // Get FPD & BBC infos
#ifdef _BBC_data_
  if(event) bbc = &(event->triggerDetectorCollection()->bbc());
  if(mudst) bbc = &(mudst->event()->bbcTriggerDetector());
#endif
#ifdef _FPD_data_
  if(event) fpd = event->fpdCollection();
  if(mudst) fpd = &(mudst->event()->fpdCollection());
#endif

  //Get EMC info
#ifdef _EMC_CLUSTERS_
  //cout << "filling emc cluster branch" << endl; 
  if(event){
    emcClusters[0] = event->emcCollection()->detector(kBarrelEmcTowerId)->cluster();
    emcClusters[1] = event->emcCollection()->detector(kBarrelSmdEtaStripId)->cluster();
    emcClusters[2] = event->emcCollection()->detector(kBarrelSmdPhiStripId)->cluster();
  }
#endif
#ifdef _EMC_POINTS_
  cout << "filling emcpoint branch" << endl; 
  if(event){
    StSPtrVecEmcPoint& bemcp = event->emcCollection()->barrelPoints();
    ppEmcPoints->setNumPoints(bemcp.size());
    for (int i=0; i<bemcp.size(); i++){
      ppemcPoints->Fill(i, *bemcp[i]);
      //cout << i << endl;
    }
    ppemcPoints->ConstructNbrArray();
  }
#endif
  
  //write out to uDst
  if(saveEventWithNoJets)
  {
    ppuDst->Fill();
  }
  else if(ppEvent->hasJets())
  {
    ppuDst->Fill();
  }
  
  mGoodCounter++;

  return kStOk;
}

Int_t StppuDstMaker::Finish()
{
  m_outfile->Write();
  m_outfile->Close();

#ifdef _2002ntuple_
  finishfpdpi0_();
#endif
#ifdef _2003ntuple_
  finishntp2003_();
#endif
  cout << "=================================================================\n";
  cout << "StppuDstger statistics:\n";
  cout << "events with StppuDstger data: " << mGoodCounter << endl;
  cout << "events without StppuDstger data: " << mBadCounter << endl;
  cout << "=================================================================\n";    
  StMaker::Finish();
  return kStOK;
}

