#include "StRareEvent.h"
#include "StRareTrack.h"
#include "StL3RareTrack.h"
#include "StEventTypes.h"
#include "StThreeVectorF.hh"
#include "StEventUtilities/StuRefMult.hh"

ClassImp(StRareEvent)

TClonesArray* StRareEvent::fgRareTracks = 0;
TClonesArray* StRareEvent::fgL3RareTracks = 0;

StRareEvent::StRareEvent() {
  if (!fgRareTracks) fgRareTracks = new TClonesArray("StRareTrack",4000);
  fRareTracks = fgRareTracks;
  fNRareTrack = 0;
  if (!fgL3RareTracks) fgL3RareTracks = new TClonesArray("StL3RareTrack",4000);
  fL3RareTracks = fgL3RareTracks;
  fNL3RareTrack = 0;
}


StRareEvent::~StRareEvent() { 
  fgRareTracks->Delete();
  fgL3RareTracks->Delete();
}


void StRareEvent::fillRareEvent(StEvent* event){
  fRunNumber = event->runId();
  fEventNumber = event->id();
  fmagneticField = event->summary()->magneticField();
  //  fmagneticField = 0.25;
  //  fnumberOfGoodPrimaryTracks = event->summary()->numberOfGoodPrimaryTracks();
  StEvent& evt = *event;
  fnumberOfGoodPrimaryTracks = uncorrectedNumberOfNegativePrimaries(evt);
  if (event->primaryVertex()) {
        fVertexZ = event->primaryVertex()->position().z();
  }
  else {
        fVertexZ = -999.0;
  }

  StL0Trigger* myL0Trigger = event->l0Trigger();
  if (!myL0Trigger) {
        fTriggerWord = 0;
  }
  else fTriggerWord = myL0Trigger->triggerWord();
}


void StRareEvent::fillL3Info(StL3Trigger *l3trigger)
{
  if (l3trigger->primaryVertex()) {
        fL3VertexZ = l3trigger->primaryVertex()->position().z();
  }
  else fL3VertexZ = -999;

  // get event summary
  const StL3EventSummary* l3EventSummary = l3trigger->l3EventSummary();
  if (!l3EventSummary) {
        cout << "No l3 event summary found." << endl;
	return;
  }

  fL3Unbiased = l3EventSummary->unbiasedTrigger();

  fL3Flag = kFALSE;

  // fill counters for anti-nucleus trigger
  const StPtrVecL3AlgorithmInfo& algInfo = l3EventSummary->algorithmsAcceptingEvent();
  for (unsigned int i=0; i<algInfo.size(); i++) {
        if (algInfo[i]->id() == 6) {
	      fL3Flag = kTRUE;
	      fNProcessed = algInfo[i]->numberOfProcessedEvents();
	      fNAccept = algInfo[i]->numberOfAcceptedEvents();
	      fNBuild = algInfo[i]->numberOfBuildEvents();
	      for (int j=0; j<algInfo[i]->dataSize(); j++)
		    fTriggerData[j] = algInfo[i]->data(j);
	}
  }

}


void StRareEvent::clear(Option_t *option) {
  fgRareTracks->Clear(option);
  fNRareTrack = 0;
  fgL3RareTracks->Clear(option);
  fNL3RareTrack = 0;  
}


void StRareEvent::addTrack(StPrimaryTrack* track) {
  TClonesArray &trks = *fRareTracks;
  new (trks[fNRareTrack++]) StRareTrack(track);
}


void StRareEvent::addL3Track(StGlobalTrack* l3track) {
  TClonesArray &l3trks = *fgL3RareTracks;
//   // temporary fix for dip angle:
//   StPhysicalHelixD oldhelix = l3track->geometry()->helix();
//   StHelixD l3helix(0.001*oldhelix.curvature(), atan(oldhelix.dipAngle()),
// 		   // oldhelix.phase()+TMath::Pi(), oldhelix.origin(), -1*oldhelix.h());
// 		   oldhelix.phase(), oldhelix.origin(), oldhelix.h());
//   StThreeVectorD vertex(0, 0, zVertex);
//   double dca2d = l3helix.distance(vertex);
//   // fix end
//   //cout << zVertex << " ==> dca 2d : " << dca2d << endl;
  new (l3trks[fNL3RareTrack++]) StL3RareTrack(l3track);
}

