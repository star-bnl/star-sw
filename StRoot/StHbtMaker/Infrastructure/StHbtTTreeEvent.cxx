#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTTreeEvent.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeTrack.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeV0.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeXi.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeKink.h"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtXiCut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"

#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StHbtMaker/Infrastructure/StHbtXiCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtKinkCollection.hh"

#include "StExceptions.hh"
#include "StarClassLibrary/SystemOfUnits.h"

ClassImp(StHbtTTreeEvent)

int StHbtTTreeEvent::mDebug = 0;
TClonesArray *StHbtTTreeEvent::fgTracks = 0;
TClonesArray *StHbtTTreeEvent::fgV0s = 0;
TClonesArray *StHbtTTreeEvent::fgXis = 0;
TClonesArray *StHbtTTreeEvent::fgKinks = 0;

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtTTreeEvent::StHbtTTreeEvent() { 
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent()" << endl;
  initClonesArrays();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtTTreeEvent::~StHbtTTreeEvent(){
  if (mDebug) cout << "StHbtTTreeEvent::~StHbtTTreeEvent()" << endl;
  fgTracks->Clear("");
  fgV0s->Clear("");
  fgXis->Clear("");
  fgKinks->Clear("");
  mNtracks=0;
  mNv0s=0;
  mNxis=0;
  mNkinks=0;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtTTreeEvent::initClonesArrays(){
  if (mDebug) cout << "StHbtTTreeEvent::initClonesArrays()" << endl;
  /* *** array to hold tracks *** */
  if (!fgTracks) {
    if (mDebug) cout << "StHbtTTreeEvent::initClonesArrays() create fgTracks" << endl;
    fgTracks = new TClonesArray("StHbtTTreeTrack", 4000);
    mNtracks=0;
  }
  fTracks = fgTracks;
  /* *** array to hold V0s *** */
  if (!fgV0s) {
    if (mDebug) cout << "StHbtTTreeEvent::initClonesArrays() create fgV0s" << endl;
    fgV0s = new TClonesArray("StHbtTTreeV0", 4000);
    mNv0s=0;
  }
  fV0s = fgV0s;
  /* *** array to hold Xis *** */
  if (!fgXis) {
    if (mDebug) cout << "StHbtTTreeEvent::initClonesArrays() create fgXis" << endl;
    fgXis = new TClonesArray("StHbtTTreeXi", 4000);
    mNxis=0;
  }
  fXis = fgXis;
  /* *** array to hold Kinks *** */
  if (!fgKinks) {
    if (mDebug) cout << "StHbtTTreeEvent::initClonesArrays() create fgKinks" << endl;
    fgKinks = new TClonesArray("StHbtTTreeKink", 4000);
    mNkinks=0;
  }
  fKinks = fgKinks;
  
  if (mDebug) cout << "StHbtTTreeEvent::initClonesArrays() leaving" << endl;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtTTreeEvent::StHbtTTreeEvent(const StHbtEvent* event, StHbtTrackCut* trackCut, StHbtV0Cut* v0Cut, StHbtXiCut* xiCut, StHbtKinkCut* kinkCut){
  try {
    initClonesArrays();
    fillEventInfo(event);
  }
  catch (StException e) {
    throw e;
  }

  // loop over tracks
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent(...) - now fill " << event->TrackCollection()->size() << " tracks" << endl;
  for (StHbtTrackIterator iter=event->TrackCollection()->begin(); iter != event->TrackCollection()->end(); iter++){
    if (!trackCut || trackCut->Pass(*iter)) addTrack(event,*iter);
  }
  // loop over v0s
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent(...) - now fill " << event->V0Collection()->size() << " v0s" << endl;
  for (StHbtV0Iterator iter=event->V0Collection()->begin(); iter != event->V0Collection()->end(); iter++){
    if (!v0Cut || v0Cut->Pass(*iter)) addV0(event,*iter);
  }
  // loop over xis
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent(...) - now fill " << event->XiCollection()->size() << " xis" << endl;
  for (StHbtXiIterator iter=event->XiCollection()->begin(); iter != event->XiCollection()->end(); iter++){
    if (!xiCut || xiCut->Pass(*iter)) addXi(event,*iter);
  }
  // loop over kinks
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent(...) - now fill " << event->KinkCollection()->size() << " kinks" << endl;
  for (StHbtKinkIterator iter=event->KinkCollection()->begin(); iter != event->KinkCollection()->end(); iter++){
    if (!kinkCut || kinkCut->Pass(*iter)) addKink(event,*iter);
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtTTreeEvent::fill(const StHbtEvent* event, StHbtTrackCut* trackCut, StHbtV0Cut* v0Cut, StHbtXiCut* xiCut, StHbtKinkCut* kinkCut){
  clear();
  try {
    fillEventInfo(event);
  }
  catch (StException e) {
    throw e;
  }
  // loop over tracks
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent(...) - now fill " << event->TrackCollection()->size() << " tracks" << endl;
  for (StHbtTrackIterator iter=event->TrackCollection()->begin(); iter != event->TrackCollection()->end(); iter++){
    if (!trackCut || trackCut->Pass(*iter)) addTrack(event,*iter);
  }
  // loop over v0s
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent(...) - now fill " << event->V0Collection()->size() << " v0s" << endl;
  for (StHbtV0Iterator iter=event->V0Collection()->begin(); iter != event->V0Collection()->end(); iter++){
    if (!v0Cut || v0Cut->Pass(*iter)) addV0(event,*iter);
  }
  // loop over xis
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent(...) - now fill " << event->XiCollection()->size() << " xis" << endl;
  for (StHbtXiIterator iter=event->XiCollection()->begin(); iter != event->XiCollection()->end(); iter++){
    if (!xiCut || xiCut->Pass(*iter)) addXi(event,*iter);
  }
  // loop over kinks
  if (mDebug) cout << "StHbtTTreeEvent::StHbtTTreeEvent(...) - now fill " << event->KinkCollection()->size() << " kinks" << endl;
  for (StHbtKinkIterator iter=event->KinkCollection()->begin(); iter != event->KinkCollection()->end(); iter++){
    if (!kinkCut || kinkCut->Pass(*iter)) addKink(event,*iter);
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtTTreeEvent::fillEventInfo(const StHbtEvent* event){
  if (mDebug) cout << "StHbtTTreeEvent::fillEventInfo(StEvent* event)" << endl;
  if (!event) 
    throw StExceptionNullPointer("StHbtTTreeEvent::fillEventInfo(StHbtEvent* event) - event");
  mEventNumber = event->mEventNumber;
  mRunNumber = event->mRunNumber;                
  mTpcNhits = event->mTpcNhits;
  mNumberOfTracks = event->mNumberOfTracks;
  mNumberOfGoodTracks= event->mNumberOfGoodTracks;
  mUncorrectedNumberOfPositivePrimaries = event->mUncorrectedNumberOfPositivePrimaries;
  mUncorrectedNumberOfNegativePrimaries = event->mUncorrectedNumberOfNegativePrimaries;
  mReactionPlane[0] = event->mReactionPlane[0];              
  mReactionPlane[1] = event->mReactionPlane[1];              
  mReactionPlanePtWgt[0] = event->mReactionPlanePtWgt[0];              
  mReactionPlanePtWgt[1] = event->mReactionPlanePtWgt[1];              
  mVertexX = event->mPrimVertPos.x();
  mVertexY = event->mPrimVertPos.y();
  mVertexZ = event->mPrimVertPos.z();
  mCtbMultiplicity  = event->mCtbMultiplicity;
  mZdcAdc[0] = event->mZdcAdc[0];
  mZdcAdc[1] = event->mZdcAdc[1];
  mMagneticField = event->mMagneticField;
  mTriggerWord = event->mTriggerWord;
  mTriggerActionWord = event->mTriggerActionWord;
  //for ( int i=0; i<4; i++) mL3TriggerAlgorithm[i] = event->mL3TriggerAlgorithm[i];
  mL3TriggerAlgorithm = event->mL3TriggerAlgorithm[0];

  if (mDebug) cout << "StHbtTTreeEvent::fillEventInfo(StEvent* event) - leaving" << endl;
} 
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtTTreeEvent::clear(){
  if (mDebug) cout << "StHbtTTreeEvent::clear(...)" << endl;
//  for (unsigned short i=0; i<mNtracks; i++) delete (StHbtTTreeTrack*)fTracks->UncheckedAt(i);
//  for (unsigned short i=0; i<mNv0s; i++) delete (StHbtTTreeV0*)fV0s->UncheckedAt(i);
//  for (unsigned short i=0; i<mNxis; i++) delete (StHbtTTreeXi*)fXis->UncheckedAt(i);
//  for (unsigned short i=0; i<mNkinks; i++) delete (StHbtTTreeKink*)fKinks->UncheckedAt(i);

  fTracks->Clear();
  fV0s->Clear();
  fXis->Clear();
  fKinks->Clear();

  mNtracks=0;
  mNv0s=0;
  mNxis=0;
  mNkinks=0;
}
//---------------------------------------------------------------------
//---------------------------------------------------------------------
//---------------------------------------------------------------------
void StHbtTTreeEvent::addTrack(const StHbtEvent* event, const StHbtTrack* track) {
  if (mDebug>1) cout << "StHbtTTreeEvent::addTrack(...) " << endl;
  TClonesArray &tracks = *fTracks;
  try{
    new(tracks[mNtracks++]) StHbtTTreeTrack(event, track);
  }
  catch (StException e) {
    if (mDebug>1) e.print();
  }
}
//---------------------------------------------------------------------
//---------------------------------------------------------------------
//---------------------------------------------------------------------
void StHbtTTreeEvent::addV0(const StHbtEvent* event, const StHbtV0* v0) {
  if (mDebug>1) cout << "StHbtTTreeEvent::addV0(...) " << endl;
  TClonesArray &v0s = *fV0s;
  try {
    new(v0s[mNv0s++]) StHbtTTreeV0(event, v0);
  }
  catch (StException e) {
    e.print();
  }
}
//---------------------------------------------------------------------
//---------------------------------------------------------------------
//---------------------------------------------------------------------
void StHbtTTreeEvent::addXi(const StHbtEvent* event, const StHbtXi* xi) {
  if (mDebug>1) cout << "StHbtTTreeEvent::addXi(...) " << endl;
  TClonesArray &xis = *fXis;
  try {
    new(xis[mNxis++]) StHbtTTreeXi(event, xi);
  }
  catch (StException e) {
    e.print();
  }
}
//---------------------------------------------------------------------
//---------------------------------------------------------------------
//---------------------------------------------------------------------
void StHbtTTreeEvent::addKink(const StHbtEvent* event, const StHbtKink* kink) {
  TClonesArray &kinks = *fKinks;
  try {
    new(kinks[mNkinks++]) StHbtTTreeKink(event, kink);
  }
  catch (StException e) {
    e.print();
  }
}


void StHbtTTreeEvent::SetMagneticField(double m) {mMagneticField=m;}


