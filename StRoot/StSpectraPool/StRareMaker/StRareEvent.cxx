# include "StRareEvent.h"
# include "StRareTrack.h"
# include "StEventTypes.h"
# include "StEventUtilities/StuRefMult.hh"

ClassImp(StRareEvent)

TClonesArray *StRareEvent::fgRareTracks = 0;

StRareEvent::StRareEvent(){
if(!fgRareTracks) fgRareTracks = new TClonesArray("StRareTrack",4000);
 fRareTracks = fgRareTracks;
 fNRareTrack = 0;
}
void StRareEvent::FillRareEvent(StEvent* event){
  fRunNumber = event->runId();
  fEventNumber = event->id();
  //  fmagneticField = event->summary()->magneticField();
  fmagneticField = 0.25;
  //  fnumberOfGoodPrimaryTracks = event->summary()->numberOfGoodPrimaryTracks();
  StEvent& evt = *event;
  fnumberOfGoodPrimaryTracks = uncorrectedNumberOfNegativePrimaries(evt);
  if (event->primaryVertex()) {
    fVertexZ = event->primaryVertex()->position().z();
  }
  else{
    fVertexZ = -999.0;
  }
}

void StRareEvent::Clear(Option_t *option){fgRareTracks->Clear();fNRareTrack=0;}

StRareEvent::~StRareEvent(){ fgRareTracks->Delete();}


void StRareEvent::AddTrack(StPrimaryTrack* track){
  TClonesArray &trks = *fRareTracks;
  if(fNRareTrack<4000){
   new (trks[fNRareTrack++]) StRareTrack(track);
  }
  else {
    cout << "StRareEvent::Too Many Rare Particle Candidates = " << fNRareTrack << endl;}
}



