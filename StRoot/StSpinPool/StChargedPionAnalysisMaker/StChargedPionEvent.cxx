#include "StChargedPionEvent.h"

#include "StMessMgr.h"

#include "StChargedPionVertex.h"
#include "StChargedPionTrack.h"
#include "StChargedPionJet.h"
//#include "StChargedPionJetParticle.h"

ClassImp(StChargedPionEvent)

StChargedPionEvent::StChargedPionEvent() : TObject(), mSpinQA(0), mTriggerBits(0), mSimuTriggerBits(0) {
    mTriggerLookup[ 96011] = 0x00000001;
    mTriggerLookup[ 96201] = 0x00000002;
    mTriggerLookup[ 96211] = 0x00000004;
    mTriggerLookup[ 96221] = 0x00000008;
    mTriggerLookup[ 96233] = 0x00000010;
    
    mVertices = new TClonesArray("StChargedPionVertex", 20);
    mTracks = new TClonesArray("StChargedPionTrack", 50);
    mJets = new TClonesArray("StChargedPionJet", 50);
    //mJetParticles = new TClonesArray("StChargedPionJetParticles", 200);
    
    StChargedPionEvent::Class()->IgnoreTObjectStreamer();
    StChargedPionVertex::Class()->IgnoreTObjectStreamer();
    StChargedPionTrack::Class()->IgnoreTObjectStreamer();
    StChargedPionJet::Class()->IgnoreTObjectStreamer();
    
    //StChargedPionJetParticle::Class()->IgnoreTObjectStreamer();
}

StChargedPionEvent::~StChargedPionEvent() { 
    if(mVertices) delete mVertices;
    if(mTracks) delete mTracks;
    if(mJets) delete mJets;
    //if(mJetParticles) delete mJetParticles; 
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
    mRunId          = e.mRunId;
    mEventId        = e.mEventId;
    mBx7            = e.mBx7;
    mBbcTimeBin     = e.mBbcTimeBin;
    mSpinBit        = e.mSpinBit;
    mSpinQA         = e.mSpinQA;
    mTriggerLookup  = e.mTriggerLookup;
    mTriggerBits    = e.mTriggerBits;
    mSimuTriggerBits= e.mSimuTriggerBits;
    
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
    
    //mJetParticles->Clear();
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

//StChargedPionJetParticle* StChargedPionEvent::jetParticle(int i) {
//    return static_cast<StChargedPionJetParticle*>(mJetParticles->At(i));
//}
//
//const StChargedPionJetParticle* StChargedPionEvent::jetParticle(int i) const {
//    return static_cast<StChargedPionJetParticle*>(mJetParticles->At(i));
//}

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

//void StChargedPionEvent::addJetParticle(StChargedPionJetParticle* j) {
//    new ( (*mJetParticles)[mJetParticles->GetEntriesFast()] ) StChargedPionJetParticle(*j);
//}
