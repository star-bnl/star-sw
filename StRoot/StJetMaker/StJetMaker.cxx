/***************************************************************************
 *
 * $Id: StJetMaker.cxx,v 1.4 2004/09/14 17:27:15 mmiller Exp $
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

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"

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

double gDeltaPhi(double p1, double p2);
double gDeltaR(const TLorentzVector* jet, const StThreeVectorF& track);

ClassImp(StJetMaker)
  
    StJetMaker::StJetMaker(const Char_t *name, StFourPMaker* fPMaker, 
			   StMuDstMaker* uDstMaker, const char *outputName) 
	: StMaker(name), fourPMaker(fPMaker), muDstMaker(uDstMaker),
	  outName(outputName), mGoodCounter(0), mBadCounter(0), mEventCounter(0)
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
    ++mEventCounter;
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

	//clear...
	thisAna->clear();
	
	FourList &tracks = fourPMaker->getTracks();

	thisAna->setFourVec(tracks);
	cout << "call:\t" << (*jb).first <<".findJets() with:\t" << tracks.size() << "\t protoJets"<<endl;
	thisAna->findJets();
	
	typedef StppJetAnalyzer::JetList JetList;
	JetList &cJets = thisAna->getJets();
	
	StJets *muDstJets = thisAna->getmuDstJets();
	muDstJets->Clear();

	muDstJets->setMuDst(mudst);
	
	if (cJets.size() > 0) hadJets = true;
	
	for(JetList::iterator it=cJets.begin(); it!=cJets.end(); ++it) {
	    muDstJets->addProtoJet(*it);
	}
	cout << "Number Jets Found: " << muDstJets->nJets() << endl;
	
	for(int i = 0; i < muDstJets->nJets(); i++) {
	    StJet* jet = (StJet*) muDstJets->jets()->At(i);
	    cout<<"jet "<<i<<"\t\t"<<jet->E()<<"\t\t"<<jet->Phi()<<"\t\t"<<jet->Eta()<<endl;

	    /*
	    // begin test......................................................................................................
	    StJet* j = jet;
	    //Get pointers to retreive the emc info
	    StEmcGeom* geom = StEmcGeom::getEmcGeom(detname[0].Data());
	    StEmcADCtoEMaker* adc2e =dynamic_cast<StEmcADCtoEMaker*>( GetMaker("Eread") );
	    assert(adc2e);
	    StBemcData* data = adc2e->getBemcData();
	    
	    StJets* stjets = muDstJets;
	    typedef StJets::TrackVec TrackVec;
	    StMuDst* muDst = muDstMaker->muDst();
	    TrackVec tracks = stjets->jetParticles(muDst, i);
	    
	    int itrack = 0;
	    for (TrackVec::iterator tit = tracks.begin(); tit!=tracks.end(); ++tit) {
		StMuTrack *muTrack = *tit;
		
		//cout <<"\t--track "<<itrack<<endl;
		const StThreeVectorF& mom = muTrack->momentum();
		double dR = gDeltaR(j, mom);
		cout <<"\tPt_track:\t"<<mom.perp()
		     <<"\tEta_track:\t"<<mom.pseudoRapidity()
		     <<"\tPhi_track:\t"<<mom.phi()
		     <<"\tdR:\t"<<dR<<endl;
		
		++itrack;
	    }
	    //now get the bemc info:
	    vector<int> towerIndices = stjets->jetBemcTowerIndices(i);
	    const int maxHits = 4800;

	    for (vector<int>::iterator bit=towerIndices.begin(); bit!=towerIndices.end(); ++bit) {
		int towerIndex = (*bit);
		if (towerIndex>maxHits) {
		    cout <<"StJetReader::exampleEventAna(). ERROR:\ttowerIndex out of bounds. abort()"<<endl;
		    abort();
		}
		float eta, phi;
		geom->getEtaPhi(towerIndex, eta, phi);
		double e = data->TowerEnergy[towerIndex];
		double dphi = gDeltaPhi(j->Phi(), phi);
		double deta = j->Eta()-eta;
		double dR = sqrt( dphi*dphi  +  deta*deta );
		cout <<"\tE_tower:\t"<<e<<"\tEta_tower:\t"<<eta<<"\tPhi_tower:\t"<<phi<<"\tdR:\t"<<dR<<endl;
	    }
	    //end test.............................................................................................................
	    */
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










