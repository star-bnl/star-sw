#include "StChargedPionEvent.h"

#include "StMessMgr.h"

#include "StChargedPionVertex.h"
#include "StChargedPionTrack.h"
#include "StChargedPionJet.h"

ClassImp(StChargedPionEvent)

StChargedPionEvent::StChargedPionEvent() : TObject(), mSpinQA(0), mTriggerBits(0), mSimuTriggerBits(0) {
    mTriggerLookup[ 96011] = 0x00000001;
    mTriggerLookup[ 96201] = 0x00000002;
    mTriggerLookup[ 96211] = 0x00000004;
    mTriggerLookup[ 96221] = 0x00000008;
    mTriggerLookup[ 96233] = 0x00000010;
    mTriggerLookup[117001] = 0x00000020;
    mTriggerLookup[137213] = 0x00000040;
    mTriggerLookup[137221] = 0x00000080;
    mTriggerLookup[137222] = 0x00000100;
    mTriggerLookup[137585] = 0x00000200;
    mTriggerLookup[137611] = 0x00000400;
    mTriggerLookup[137622] = 0x00000800;
    mTriggerLookup[106011] = 0x00001000;
    mTriggerLookup[106201] = 0x00002000;
    mTriggerLookup[106211] = 0x00004000;
    mTriggerLookup[106221] = 0x00008000;
    mTriggerLookup[106233] = 0x00010000;
    
    mVertices = new TClonesArray("StChargedPionVertex", 20);
    mTracks = new TClonesArray("StChargedPionTrack", 50);
    mJets = new TClonesArray("StChargedPionJet", 50);
    
    StChargedPionEvent::Class()->IgnoreTObjectStreamer();
    StChargedPionVertex::Class()->IgnoreTObjectStreamer();
    StChargedPionTrack::Class()->IgnoreTObjectStreamer();
    StChargedPionJet::Class()->IgnoreTObjectStreamer();
}

StChargedPionEvent::~StChargedPionEvent() { 
    if(mVertices) delete mVertices;
    if(mTracks) delete mTracks;
    if(mJets) delete mJets;
}

StChargedPionEvent::StChargedPionEvent(const StChargedPionEvent & e) : TObject(e),
    mRunId(e.mRunId), mEventId(e.mEventId), mBx7(e.mBx7), mBbcTimeBin(e.mBbcTimeBin),
    mSpinBit(e.mSpinBit), mSpinQA(e.mSpinQA), mTriggerLookup(e.mTriggerLookup),
    mTriggerBits(e.mTriggerBits), mSimuTriggerBits(e.mSimuTriggerBits) {
        
        for(unsigned i=0; i<e.nVertices(); ++i) {
            addVertex(e.vertex(i));
        }
        
        for(unsigned i=0; i<e.nTracks(); ++i) {
            addTrack(e.track(i));
        }
        
        for(unsigned i=0; i<e.nJets(); ++i) {
            addJet(e.jet(i));
        }
}

StChargedPionEvent& StChargedPionEvent::operator=(const StChargedPionEvent & e) {
    if (this == &e) return *this;
    
    this->Clear();
    mRunId              = e.mRunId;
    mEventId            = e.mEventId;
    mBx7                = e.mBx7;
    mBbcTimeBin         = e.mBbcTimeBin;
    mSpinBit            = e.mSpinBit;
    mSpinQA             = e.mSpinQA;
    mTriggerLookup      = e.mTriggerLookup;
    mTriggerPrescales   = e.mTriggerPrescales;
    mTriggerBits        = e.mTriggerBits;
    mSimuTriggerBits    = e.mSimuTriggerBits;
    
    for(unsigned i=0; i<e.nVertices(); ++i) {
        addVertex(e.vertex(i));
    }
    
    for(unsigned i=0; i<e.nTracks(); ++i) {
        addTrack(e.track(i));
    }
    
    for(unsigned i=0; i<e.nJets(); ++i) {
        addJet(e.jet(i));
    }
    
    return *this;
}

void StChargedPionEvent::copy(const StChargedPionEvent& e) {
    *this = e;
}

void StChargedPionEvent::Clear(Option_t* o) {
    mSpinQA = 0;
    mTriggerBits = 0;
    mSimuTriggerBits = 0;
    mVertices->Clear();
    mTracks->Clear();
    
    // use ::Delete here so ROOT doesn't hold onto memory allocated for jet particles
    mJets->Delete();
    
    mTriggerPrescales.clear();
    mHighTowers.clear();
    mTriggerPatches.clear();
    mJetPatches.clear();
}

bool StChargedPionEvent::isSpinValid() const { 
    return (isPolValid() && isPolLong() && !isPolTrans() && !isBxingMasked() && isNullOffset());
}

bool StChargedPionEvent::isTrigger(unsigned int trigId) const {
    map<unsigned int, unsigned int>::const_iterator it = mTriggerLookup.find(trigId);
    if(it==mTriggerLookup.end()) return false;
    return mTriggerBits & it->second;
}

bool StChargedPionEvent::isSimuTrigger(unsigned int trigId) const {
    map<unsigned int, unsigned int>::const_iterator it = mTriggerLookup.find(trigId);
    if(it==mTriggerLookup.end()) return false;
    return mSimuTriggerBits & it->second;
}

float StChargedPionEvent::prescale(unsigned int trigId) const { 
    map<unsigned int, float>::const_iterator it = mTriggerPrescales.find(trigId);
    if(it==mTriggerPrescales.end()) return -1.0;
    return it->second;
}

int StChargedPionEvent::highTowerAdc(short towerId) const {
    map<short, int>::const_iterator it = mHighTowers.find(towerId);
    if(it==mHighTowers.end()) return -1;
    return it->second;
}

int StChargedPionEvent::triggerPatchAdc(short patchId) const {
    map<short, int>::const_iterator it = mTriggerPatches.find(patchId);
    if(it==mTriggerPatches.end()) return -1;
    return it->second;
}

int StChargedPionEvent::jetPatchAdc(short patchId) const {
    map<short, int>::const_iterator it = mJetPatches.find(patchId);
    if(it==mJetPatches.end()) return -1;
    return it->second;
}

StChargedPionVertex* StChargedPionEvent::vertex(int i) {
    return static_cast<StChargedPionVertex*>(mVertices->At(i));
}

const StChargedPionVertex* StChargedPionEvent::vertex(int i) const {
    return static_cast<StChargedPionVertex*>(mVertices->At(i));
}

StChargedPionTrack* StChargedPionEvent::track(int i) {
    return static_cast<StChargedPionTrack*>(mTracks->At(i));
}

const StChargedPionTrack* StChargedPionEvent::track(int i) const {
    return static_cast<StChargedPionTrack*>(mTracks->At(i));
}

StChargedPionJet* StChargedPionEvent::jet(int i) {
    return static_cast<StChargedPionJet*>(mJets->At(i));
}

const StChargedPionJet* StChargedPionEvent::jet(int i) const {
    return static_cast<StChargedPionJet*>(mJets->At(i));
}

void StChargedPionEvent::addTrigger(unsigned int trigId) {
    map<unsigned int, unsigned int>::const_iterator it = mTriggerLookup.find(trigId);
    if(it!=mTriggerLookup.end()) {
        mTriggerBits |= it->second;
    }
}

void StChargedPionEvent::addSimuTrigger(unsigned int trigId) {
    map<unsigned int, unsigned int>::const_iterator it = mTriggerLookup.find(trigId);
    if(it!=mTriggerLookup.end()) {
        mSimuTriggerBits |= it->second;
    }
}

void StChargedPionEvent::addVertex(const StChargedPionVertex* v) {
    new ( (*mVertices)[mVertices->GetEntriesFast()] ) StChargedPionVertex(*v);
}

void StChargedPionEvent::addTrack(const StChargedPionTrack* t) {
    new ( (*mTracks)[mTracks->GetEntriesFast()] ) StChargedPionTrack(*t);
}

void StChargedPionEvent::addJet(const StChargedPionJet* j) {
    new ( (*mJets)[mJets->GetEntriesFast()] ) StChargedPionJet(*j);
}

