/***************************************************************************
 *
 * $Id: StJetMaker.cxx,v 1.26 2008/03/27 00:41:09 tai Exp $
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

#include "StJetMaker.h"

//root
#include "TFile.h"
#include "TTree.h"

//St_base
#include "StMessMgr.h"

//StEvent
#include "StEvent.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//StJetFinder
#include "StJetFinder/StProtoJet.h"

//StJetMaker
#include "StJetMaker/StJet.h"
#include "StJetMaker/StFourPMakers/StFourPMaker.h"
#include "StJetMaker/StFourPMakers/StBET4pMaker.h"
#include "StFourPMakers/StMuEmcPosition.h"

using namespace std;

ClassImp(StJetMaker)
  
StJetMaker::StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputName) 
  : StMaker(name)
  , mMuDstMaker(uDstMaker)
  , mOutName(outputName)
  , mJetTree(0)
{

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
void StJetMaker::addAnalyzer(const StppAnaPars* ap, const StJetPars* jp, StFourPMaker* fp, const char* name)
{
  mJetBranches[name] = new StppJetAnalyzer(ap, jp, fp);
}

Int_t StJetMaker::Init() 
{
  cout << "StJetMaker: jet output file: " << mOutName << endl;
    
  mOutFile = new TFile(mOutName.c_str(), "recreate");
    
  mJetTree  = new TTree("jet", "jetTree");
  for(jetBranchesMap::iterator i = mJetBranches.begin(); i != mJetBranches.end(); ++i) {
    (*i).second->addBranch((*i).first.c_str(), mJetTree);
  }
    
  return StMaker::Init();
}

Int_t StJetMaker::Make()
{
  LOG_DEBUG << " Start StJetMaker :: " << GetName() << " mode=" << m_Mode << endm;

  for(jetBranchesMap::iterator jb = mJetBranches.begin(); jb != mJetBranches.end(); ++jb) {

    StppJetAnalyzer* thisAna = (*jb).second;
    if(!thisAna) {
      cout << "StJetMaker::Make() ERROR:\tjetBranches[" << (*jb).first << "]==0. abort()" << endl;
      abort();
    }

    StFourPMaker* fourPMaker = thisAna->fourPMaker();
    if(!fourPMaker) {
      cout << "StJetMaker::Make() ERROR:\tfourPMaker is NULL! abort()" << endl;
      abort();
    }

    thisAna->clear();
	
    FourList &tracks = fourPMaker->getTracks();

    thisAna->setFourVec(tracks);
    LOG_DEBUG << "call:\t" << (*jb).first <<".findJets() with:\t" << tracks.size() << "\t protoJets" << endm;
    thisAna->findJets();
	
    typedef StppJetAnalyzer::JetList JetList;
    JetList &cJets = thisAna->getJets();
	
    StJets *muDstJets = thisAna->getmuDstJets();
    muDstJets->Clear();
    muDstJets->setBemcCorrupt(fourPMaker->bemcCorrupt() );

    StMuEvent* event = mMuDstMaker->muDst()->event();
    muDstJets->seteventId(event->eventId());
    muDstJets->seteventNumber(event->eventNumber());
    muDstJets->setrunId(event->runId());
    muDstJets->setrunNumber(event->runNumber());

    //Addd some info from StBet4pMaker
    StBET4pMaker* bet4p = dynamic_cast<StBET4pMaker*>(fourPMaker);
    if (bet4p) {
      LOG_DEBUG << "StJetMaker::Make()\tfound 4pmaker in chain" << endm;
      muDstJets->setDylanPoints( bet4p->nDylanPoints() );
      muDstJets->setSumEmcE( bet4p->sumEmcEt() );
    }
	
    int ijet(0);
	
    LOG_DEBUG << "Number Jets Found(a):\t" << cJets.size() << endm;
    for(JetList::iterator it = cJets.begin(); it != cJets.end(); ++it) {
	    
      StProtoJet& pj = (*it);
      LOG_DEBUG << "jet " << ijet << "\t\t" << pj.pt() << "\t" << pj.phi() << "\t" << pj.eta() << endm;

      addProtoJet(*muDstJets, *it);

      ++ijet;
    }
	
    LOG_DEBUG << "Number Jets Found (b): " << muDstJets->nJets() << endm;	
    for(int i = 0; i < muDstJets->nJets(); i++) {
      StJet* jet = (StJet*) muDstJets->jets()->At(i);
      LOG_DEBUG << "jet " << i << "\t\t" << jet->Pt() << "\t\t" << jet->Phi() << "\t\t" << jet->Eta() << endm;
    }
  }
    
  mJetTree->Fill();
    
  return kStOk;
}

void StJetMaker::addProtoJet(StJets &jets, StProtoJet& pj)
{
  StMuDst *muDst = mMuDstMaker->muDst();

  StProtoJet::FourVecList &trackList = pj.list();
	
  StJet tempJet( pj.e(), pj.px(), pj.py(), pj.pz(), 0, 0 );
  tempJet.jetEt = pj.eT();
  tempJet.jetPt = tempJet.Pt();
  tempJet.jetEta = tempJet.Eta();
  tempJet.jetPhi = tempJet.Phi();
  
  StMuEvent* event = muDst->event();
  StThreeVectorF vPos = event->primaryVertexPosition();
  tempJet.zVertex = vPos.z();

  for(StProtoJet::FourVecList::iterator it2=trackList.begin(); it2!=trackList.end(); ++it2)  {
    StMuTrackFourVec *track = dynamic_cast<StMuTrackFourVec*>(*it2);
    if (!track) {
      cout <<"StJets::addProtoJet(). ERROR:\tcast to StMuTrackFourVecFailed.  no action"<<endl;
      return;
    }
    int muTrackIndex = track->getIndex();
    if (muTrackIndex <0) {
      cout <<"Error, muTrackIndex<0. abort()"<<endl;
      abort();
    }
      
    TrackToJetIndex t2j( jets.nJets(), muTrackIndex, track->detectorId() );
    t2j.SetPxPyPzE(track->px(), track->py(), track->pz(), track->e() );
      
    //and cache some properties if it really came from a StMuTrack:
    StMuTrack* muTrack = track->particle();
    if (muTrack) {  //this will fail for calorimeter towers ;)

      //----------------------------------------------
      double bField = 0.5; //to put it in Tesla
      double rad=238.6;//geom->Radius()+5.;
      StThreeVectorD momentumAt,positionAt;

      StMuEmcPosition mMuPosition;
      mMuPosition.trackOnEmc(&positionAt, &momentumAt, muTrack, bField, rad );

      t2j.setCharge( muTrack->charge() );
      t2j.setNhits( muTrack->nHits() );
      t2j.setNhitsPoss( muTrack->nHitsPoss() );
      t2j.setNhitsDedx( muTrack->nHitsDedx() );
      t2j.setNhitsFit( muTrack->nHitsFit() );
      t2j.setNsigmaPion( muTrack->nSigmaPion() );
      t2j.setTdca ( muTrack->dcaGlobal().mag() ); //jan 27, 2007
      t2j.setTdcaz ( muTrack->dcaZ() ); //jan 27, 2007
      t2j.setTdcaxy ( muTrack->dcaD() ); //jan 27, 2007
      t2j.setetaext ( positionAt.pseudoRapidity() );
      t2j.setphiext ( positionAt.phi() );
    }
     
    jets.addTrackToIndex(t2j);

    //ok, get track/tower properties here:
    StDetectorId mDetId = track->detectorId();
    if (mDetId==kTpcId) {
      tempJet.nTracks++;
      tempJet.tpcEtSum += track->eT();
    }
    else if (mDetId==kBarrelEmcTowerId) {
      tempJet.nBtowers++;
      tempJet.btowEtSum += track->eT();
    }
    else if (mDetId==kEndcapEmcTowerId) {
      tempJet.nEtowers++;
      tempJet.etowEtSum += track->eT();
    }
  }

  jets.addJet(tempJet);

}

Int_t StJetMaker::Finish()
{
  mOutFile->Write();
  mOutFile->Close();
  delete mOutFile;

  StMaker::Finish();
  return kStOK;
}
