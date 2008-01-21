#include "StChargedPionEvent.h"

#include <utility>
using std::make_pair;

#include "StMessMgr.h"

#include "StChargedPionVertex.h"
#include "StChargedPionTrack.h"
#include "StChargedPionJet.h"

#include "StTriggerUtilities/StTriggerSimuResult.h"

ClassImp(StChargedPionEvent)

// static member initialization
std::pair<unsigned int, unsigned int> a[] = {
    make_pair( 96011, 0x00000001),
    make_pair( 96201, 0x00000002),
    make_pair( 96211, 0x00000004),
    make_pair( 96221, 0x00000008),
    make_pair( 96233, 0x00000010),
    make_pair(117001, 0x00000020),
    make_pair(137213, 0x00000040),
    make_pair(137221, 0x00000080),
    make_pair(137222, 0x00000100),
    make_pair(137585, 0x00000200),
    make_pair(137611, 0x00000400),
    make_pair(137622, 0x00000800),
    make_pair(106011, 0x00001000),
    make_pair(106201, 0x00002000),
    make_pair(106211, 0x00004000),
    make_pair(106221, 0x00008000),
    make_pair(106233, 0x00010000),
    make_pair(117402, 0x00020000),
    make_pair(117211, 0x00040000),
    make_pair(117212, 0x00080000),
    make_pair(137262, 0x00100000),
    make_pair(137271, 0x00200000),
    make_pair(137272, 0x00400000),
    make_pair(137273, 0x00800000),
    make_pair(137641, 0x01000000),
    make_pair(137652, 0x02000000),
    // reuse some bits for transverse and first longitudinal running
    make_pair(127212, 0x00080000),
    make_pair(127213, 0x00000040),
    make_pair(117221, 0x00000080),
    make_pair(127221, 0x00000080),
    make_pair(117585, 0x00000200),
    make_pair(127585, 0x00000200),
    make_pair(117611, 0x00000400),
    make_pair(127611, 0x00000400),
    make_pair(117622, 0x00000800),
    make_pair(127622, 0x00000800),
    make_pair(117262, 0x00100000),
    make_pair(127262, 0x00100000),
    make_pair(117271, 0x00200000),
    make_pair(127271, 0x00200000),
    make_pair(117641, 0x01000000),
    make_pair(127641, 0x01000000),
    make_pair(117652, 0x02000000),
    make_pair(127652, 0x02000000)
};

map<unsigned int, unsigned int> 
StChargedPionEvent::mTriggerLookup(a, a + sizeof(a)/sizeof(a[0]) );

unsigned int StChargedPionEvent::triggerBit(unsigned int trigId) {
    map<unsigned int, unsigned int>::const_iterator it = mTriggerLookup.find(trigId);
    if(it==mTriggerLookup.end()) return 0;
    return it->second;
}

StChargedPionEvent::StChargedPionEvent() : TObject(), mSpinQA(0), mTriggerBits(0), mSimuTriggerBits(0) {
    mVertices = new TClonesArray("StChargedPionVertex", 20);
    mTracks = new TClonesArray("StChargedPionTrack", 50);
    mJets = new TClonesArray("StChargedPionJet", 50);
    
    memset(mL2Result, 0, 36);
    memset(mL2ResultEmulated, 0, 36);
        
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
    mSpinBit(e.mSpinBit), mSpinQA(e.mSpinQA),
    mTriggerBits(e.mTriggerBits), mSimuTriggerBits(e.mSimuTriggerBits) {
        
        memcpy(mL2Result, e.mL2Result, 36);
        memcpy(mL2ResultEmulated, e.mL2ResultEmulated, 36);
        
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
    mTriggerPrescales   = e.mTriggerPrescales;
    mTriggerBits        = e.mTriggerBits;
    mSimuTriggerBits    = e.mSimuTriggerBits;
    
    memcpy(mL2Result, e.mL2Result, 36);
    memcpy(mL2ResultEmulated, e.mL2ResultEmulated, 36);
    
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
    return (isPolValid() && !isBxingMasked() && isNullOffset());
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

void StChargedPionEvent::setL2Result(const void *address, bool emulated) {
    if(emulated) {
        memcpy(mL2ResultEmulated, address, 20);
        memcpy(mL2ResultEmulated+5, (int*)address+6, 16);
    }
    else {
        memcpy(mL2Result, address, 20);
        memcpy(mL2Result+5, (int*)address+6, 16);
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
