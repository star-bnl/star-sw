/***************************************************************************
 *
 * $Id: StJetMaker.cxx,v 1.1 2004/07/08 15:41:03 mmiller Exp $
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

//root
#include "TFile.h"
#include "TTree.h"

//St_base
#include "StChain.h"

//StEvent
#include "StEvent.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//StJetMaker
#include "StJetMaker.h"
#include "StJet.h"
#include "StFourPMaker.h"

ClassImp(StJetMaker)
  
    StJetMaker::StJetMaker(const Char_t *name, StFourPMaker* fPMaker, 
			   StMuDstMaker* uDstMaker, const char *outputName) 
	: StMaker(name), fourPMaker(fPMaker), muDstMaker(uDstMaker),
	  outName(outputName), mGoodCounter(0), mBadCounter(0)
{
    infoLevel = 0;
    mudst=0;
}
/*!
  Constructing a new jet analysis requires three elements:
  (1) An instance of StppAnaPars that defines the track and jet cuts used in the analysis.
  See StRoot/StJetMaker/StppJetAnalyzer.h for specifics
  (2) A derived instance of StJetPars (i.e., StConePars or StKtPars) that defines the internal
  parameters used in the jet finding algorithm (e.g., cone radius, etc).  See
  StRoot/StJetFinder/StConeJetFinder.h and StKtCluJetFinder.h for specifics
  (3) A unique character string which is used to identify this branch in the jets TTree

*/
void StJetMaker::addAnalyzer(const StppAnaPars* ap, const StJetPars* jp, const char* name)
{
    jetBranches[name] = new StppJetAnalyzer(ap, jp);
}

void StJetMaker::InitFile(void)
{
    // creating Jet nanoDst file name
    TString jetFileName(outName);
    if(jetFileName == "/dev/null") {return; }
    jetFileName += ".root";
    cout << "StJetMaker: jet output file: " << jetFileName << endl;
    
    //open udst file
    m_outfile = new TFile(jetFileName,"recreate");
    m_outfile->SetCompressionLevel(1);
    
    jetTree->SetDirectory(m_outfile);
}

Int_t StJetMaker::Init() 
{
    //create udst & its branches    
    jetTree  = new TTree("jet","jetTree",99);
    for(jetBranchesMap::iterator i = jetBranches.begin(); i != jetBranches.end(); i++)	{
	(*i).second->addBranch((*i).first.c_str(), jetTree);
    }
    
    InitFile();
    return StMaker::Init();
}

Int_t StJetMaker::Make()
{
    cout <<" Start StJetMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

    if(muDstMaker != NULL) {
	mudst = muDstMaker->muDst();
    }
    
    //Find the Jets, using the fourPMaker information:
    bool hadJets = false;
    for(jetBranchesMap::iterator jb = jetBranches.begin(); jb != jetBranches.end(); jb++) {
	StppJetAnalyzer* thisAna = (*jb).second;
	if(!thisAna) {
	    cout << "StJetMaker::Make() ERROR:\tjetBranches[" << (*jb).first << "]==0. abort()" << endl;
	    abort();
	}

	if(fourPMaker == NULL) {
	    cout << "StJetMaker::Make() ERROR:\tfourPMaker is NULL! abort()" << endl;
	    abort();
	}
	FourList &tracks = fourPMaker->getTracks();
	thisAna->setFourVec(tracks);
	//cout << "AnaNum = " << (*jb).first << " Tracks = " << fourPMaker->numTracks() << endl;
	thisAna->findJets();
	
	typedef StppJetAnalyzer::JetList JetList;
	JetList &cJets = thisAna->getJets();
	
	StJets *muDstJets = thisAna->getmuDstJets();
	muDstJets->Clear();

	if (mudst) {
	    muDstJets->setMuDst(mudst);
	}
	
	if (cJets.size() > 0) hadJets = true;
	
	for(JetList::iterator it=cJets.begin(); it!=cJets.end(); ++it) {
	    muDstJets->addProtoJet(*it);
	}
	cout << "Number Jets Found: " << muDstJets->nJets() << endl;
	
	for(int i = 0; i < muDstJets->nJets(); i++) {
	    StJet* jet = (StJet*) muDstJets->jets()->At(i);
	    cout<<"jet "<<i<<"\t\t"<<jet->E()<<"\t\t"<<jet->Phi()<<"\t\t"<<jet->Eta()<<endl;
	}
    }
    
    jetTree->Fill();
    
    return kStOk;
}

void StJetMaker::FinishFile(void) 
{
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










