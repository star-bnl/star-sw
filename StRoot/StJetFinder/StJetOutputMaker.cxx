/***************************************************************************
 *
 * $Id: StJetOutputMaker.cxx,v 1.1 2003/05/15 18:11:19 thenry Exp $
 * 
 * Author: Thomas Henry May 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a binary output file of the jets and
 * some other basic information like trigger and event ids.  It utilizes
 * the jetevent class.
 *
 ***************************************************************************
 */

//#include "StSpinMaker/StJet.h"
#include "TLorentzVector.h"
#include "StSpinMaker/StJets.h"
#include "StJetMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StJetOutputMaker.h"

ClassImp(StJetOutputMaker)
  
StJetOutputMaker::StJetOutputMaker(const char* name, StMuDstMaker* uDstMaker,
				   StJetMaker *stjetmaker, 
				   const char *outputName, 
				   StEmcTpcFourPMaker* emcTpcFourPMaker = NULL)
  : StMaker(name), muDstMaker(uDstMaker), jetMaker(stjetmaker), 
    ofilename(outputName), fourPMaker(emcTpcFourPMaker), ofile(NULL){
}

Int_t StJetOutputMaker::Init() 
{
  if(ofile == NULL)
    ofile = new ofstream(ofilename.c_str(), ofstream::out | ofstream::trunc);
  return StMaker::Init();
}

Int_t StJetOutputMaker::Make() {
  cout <<" Start StJetOutputMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

  oJetEvent.clear();
  // First basic event information:
  StMuDst* muDst = muDstMaker->muDst();
  StMuEvent* muEvent = muDst->event();
  oJetEvent.eventId = muEvent->eventId();
  const vector<UInt_t> &triggers = 
    muEvent->triggerIdCollection().nominal().triggerIds();
  oJetEvent.triggers.clear();
  for(vector<UInt_t>::const_iterator it = triggers.begin(); 
      it != triggers.end(); ++it)
    oJetEvent.triggers.push_back(static_cast<int>(*it));
  oJetEvent.xVertex = muEvent->primaryVertexPosition().x();
  oJetEvent.yVertex = muEvent->primaryVertexPosition().y();
  oJetEvent.zVertex = muEvent->primaryVertexPosition().z();
  int numberPrimaryTracks = muDst->numberOfPrimaryTracks();
  oJetEvent.npTracks = numberPrimaryTracks;
  StMuEmcCollection* muEmc = NULL;
  if(fourPMaker != NULL)
    muEmc = fourPMaker->getMuEmcCollection();

  // Then the jets (if any)
  StJetMaker::jetBranchesMap &jetAnalyzers = jetMaker->getJets();
  for(StJetMaker::jetBranchesMap::iterator it = jetAnalyzers.begin(); 
      it != jetAnalyzers.end(); ++it)
    {
      oJetEvent.jetname = (*it).first;
      StppJetAnalyzer* ana = (*it).second;
      StJets* muDstJets = ana->getmuDstJets();
      int nJets = muDstJets->nJets();
      for(int i = 0; i < nJets; ++i)
	{
	  vector<int> trackIndices = muDstJets->jetTrackIndices(i);
	  TLorentzVector* jet = dynamic_cast<TLorentzVector*> 
	    ((muDstJets->jets())->At(i));
	  oJetEvent.energy = jet->E();
	  oJetEvent.px = jet->Px();
	  oJetEvent.py = jet->Py();
	  oJetEvent.pz = jet->Pz();
	  oJetEvent.eta = jet->Eta();
	  oJetEvent.phi = jet->Phi();
	  //oJetEvent.numCharges = jet->nCell;
	  //oJetEvent.charge = jet->charge;
	  oJetEvent.numCharges = muDstJets->nCell(i);
	  oJetEvent.charge = muDstJets->charge(i);
	  for(vector<int>::iterator trackit = trackIndices.begin();
	      trackit != trackIndices.end(); ++trackit)
	    {
	      if(*trackit >= numberPrimaryTracks)
		if(muEmc)
		  {
		    StMuEmcPoint* muPoint = muEmc->getPoint(i);
		    StCorrectedEmcPoint &point = fourPMaker->binmap.
		      moddPoints[muPoint];
		    oJetEvent.trackE = point.E();
		    oJetEvent.trackPhi = point.Phi();
		    oJetEvent.trackEta = point.Eta();
		    oJetEvent.isTpcTrack = false;
		    oJetEvent.push_track();
		    continue;
		  }
	      StMuTrack* muTrack = muDst->primaryTracks(i);
	      if(muEmc)
		tempTrack = fourPMaker->binmap.moddTracks[muTrack];
	      else
		tempTrack.init(muTrack, muEvent->primaryVertexPosition());
	      oJetEvent.trackE = tempTrack.E();
	      oJetEvent.trackPhi = tempTrack.Phi();
	      oJetEvent.trackEta = tempTrack.Eta();
	      oJetEvent.isTpcTrack = true;
	      oJetEvent.push_track();
	    }
	}
      oJetEvent.push_jet();
    }
  ofstream &jetFile = *ofile;
  cout << oJetEvent;
  write(jetFile, oJetEvent);

  return kStOk;
}

Int_t StJetOutputMaker::Finish()
{
  ofile->close();
  delete ofile;
  return StMaker::Finish();
}









