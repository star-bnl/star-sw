/***************************************************************************
 *
 * $Id: StJetMaker.cxx,v 1.2 2003/04/04 21:25:56 thenry Exp $
 * 
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Jet Nano-Dst Creator
 *
 ***************************************************************************
 *
 * Revision 1.0  2003/02/20 thenry
 * StJetMaker was modified and adapted from Akio Ogawa's StppuDstMaker
 * to allow multiple jet analysis modules to be
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
 **************************************************************************/
#include <string.h>
#include <iostream.h>

#include "TFile.h"
#include "TTree.h"

#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/EMC/StEmcMicroEvent.h"

#include "StJetMaker.h"
#define _NoJet_
#include "../StSpinMaker/StppEvent.h"
#include "../StSpinMaker/StppGeant.h"
#include "StTriggerDetectorCollection.h"
#include "StBbcTriggerDetector.h"
#include "StFpdCollection.h"
#include "St_trg_Maker/St_trg_Maker.h"
#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/EMC/StEmcMicroCollection.h"
#include "../StSpinMaker/StJet.h"
#include "StEmcPoint.h"
#include "StFourPMaker.h"

ClassImp(StJetMaker)
  
StJetMaker::StJetMaker(const Char_t *name, StFourPMaker* fPMaker, 
  StMuDstMaker* uDstMaker, const char *outputName) 
  : StMaker(name), fourPMaker(fPMaker), muDstMaker(uDstMaker),
    outName(outputName), mGoodCounter(0), mBadCounter(0){
    numJetBranches = 0;
    jetBranches = new (StppJetAnalyzer*)[MAXANALYZERS];
    names = new (char*)[MAXANALYZERS];
    infoLevel = 0;
    mudst=0;
    saveEventWithNoJets = true;
    maxEventsPerFile = DMAXEVENTSPERFILE;
    eventFileCounter = 0;
    fileCounter = 0;
    saveEMC = false;
    muEmcCol = new StMuEmcCollection();
}

void StJetMaker::SetSaveEventWithNoJets(bool saveIt)
{
  saveEventWithNoJets = saveIt;
}

void StJetMaker::addAnalyzer(StppJetAnalyzer* a, const char * name)
{
    char* branchName = new char[strlen(name)+2];
    strcpy(branchName, name);
    //this worked for Thomas, but not for Mike
    //strcat(branchName, ".");
    
    names[numJetBranches] = branchName;
    jetBranches[numJetBranches] = a;
    numJetBranches++;
}

void StJetMaker::InitFile(void) 
{
    // creating Jet nanoDst file name
    TString jetFileName(outName);
    jetFileName += fileCounter;
    jetFileName += ".root";
    cout << "StJetMaker: jet output file: " << jetFileName << endl;
    
    //open udst file
    m_outfile = new TFile(jetFileName,"recreate");
    //  m_outfile->SetFormat(1);
    m_outfile->SetCompressionLevel(1);

    jetTree->SetDirectory(m_outfile);    
}

Int_t StJetMaker::Init() 
{
    //create udst & its branches    
    jetTree  = new TTree("jet","jetTree",99);
    jetEvent = new StppEvent(); //jetEvent->setInfoLevel(infoLevel);
    jetTree->Branch ("Event","StppEvent",&jetEvent,64000,99);
    for(int i = 0; i < numJetBranches; i++)
	{
	    jetBranches[i]->addBranch(names[i], jetTree);
	}
#ifdef _GEANT_
    ppGeant = new StppGeant(); ppGeant->setInfoLevel(infoLevel);
    jetTree->Branch ("Geant","StppGeant",&ppGeant,64000,99);
#endif
#ifdef _BBC_data_
    bbc     = new StBbcTriggerDetector();
    jetTree->Branch ("Bbc","StBbcTriggerDetector",&bbc,64000,99);
#endif
#ifdef _FPD_data_
    fpd     = new StFpdCollection();
    jetTree->Branch ("Fpd","StFpdCollection",&fpd,64000,99);
#endif
    if(saveEMC)
    {
      jetTree->Branch ("Emc", "StMuEmcCollection", &muEmcCol, 64000, 99);
    }

    InitFile();
    return StMaker::Init();
}

Int_t StJetMaker::Make() {
  cout <<" Start StJetMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

  if(eventFileCounter > maxEventsPerFile)
  {
    cout << "Max Events reached.  New File being created..." << endl;
    eventFileCounter = 0;
    FinishFile();
    InitFile();
  }

  jetEvent->clear();
#ifdef _GEANT_
  ppGeant->clear();
#endif
  
  // Get MuDst, or if it's not there, get StEvent
  StEvent* event;
  if(muDstMaker != NULL)  
  {
    mudst = muDstMaker->muDst();
    if(mudst == NULL) cout << "NULL mudst!" << endl;
  }
  if(mudst != NULL) 
  {
    jetEvent->setMuDst(mudst);
    event = NULL;
  }
  else{
    event = (StEvent *)GetInputDS("StEvent");
    if(event == NULL){
      mBadCounter++;
      return kStOK;
    }
  }
  jetEvent->setMuDst(mudst);
  while(mudst != jetEvent->getMuDst())
  {
    cout << "Failed to set muDst in event.  Retrying!" << endl; 
    jetEvent->mudst = mudst;
  }
  
  // fill jetEvent 
  int res;
  res = jetEvent->fill(event, mudst);
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

  //EMC info is retrieved at this point:
  if(saveEMC) 
  {
    muEmcCol = fourPMaker->getStMuEmcCollection();  
  }  

  //Find the Jets, using the fourPMaker information:
  bool hadJets = false;
  for(Int_t jbNum = 0; jbNum < numJetBranches; jbNum++)
  {
    StppJetAnalyzer* thisAna = jetBranches[jbNum];
    if(!thisAna)
    {
      cout << "StJetMaker::Make() ERROR:\tjetBranches[" << jbNum << "]==0. abort()" << endl;
      abort();
    }
    StMuTrackFourVec* tracks = fourPMaker->getTracks();
    thisAna->setFourVec(tracks, fourPMaker->numTracks());
    //cout << "AnaNum = " << jbNum << " Tracks = " << fourPMaker->numTracks() << endl;
    thisAna->findJets();

    typedef StppJetAnalyzer::JetList JetList;
    JetList &cJets = thisAna->getJets();

    StJets *muDstJets = thisAna->getmuDstJets();
    muDstJets->Clear();

    if (cJets.size() > 0) hadJets = true;
    
    for(JetList::iterator it=cJets.begin(); it!=cJets.end(); ++it)
    {
      muDstJets->addProtoJet(*it);
    }
    cout << "Number Jets Found: " << muDstJets->nJets() << endl;
    for(int i = 0; i < muDstJets->nJets(); i++)
    {
      StJet* jet = (StJet*) muDstJets->jets()->At(i);
      cout << "Pt of Jet " << i << " = " << jet->Pt() << endl;
      cout << "Eta of Jet " << i << " = " << jet->Eta() << endl;
      cout << "Phi of Jet " << i << " = " << jet->Phi() << endl;
    }
  }

  //write out to file
  if(saveEventWithNoJets)
  {
    jetTree->Fill();
  }
  else if(hadJets)
  {
    jetTree->Fill();
  } else
  {
    mBadCounter++;
    return kStOk;
  }
  
  mGoodCounter++;

  return kStOk;
}

void StJetMaker::FinishFile(void) 
{
    fileCounter++;
    
    //close file
    m_outfile->Write();
    m_outfile->Close();
    delete m_outfile;
}

Int_t StJetMaker::Finish()
{
  FinishFile();
  cout << "=================================================================\n";
  cout << "StJetMaker statistics:\n";
  cout << "events with StJetMaker data: " << mGoodCounter << endl;
  cout << "events without StJetMaker data: " << mBadCounter << endl;
  cout << "=================================================================\n";    
  StMaker::Finish();
  return kStOK;
}

