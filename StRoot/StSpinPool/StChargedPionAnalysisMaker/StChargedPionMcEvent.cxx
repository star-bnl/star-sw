// $Id: StChargedPionMcEvent.cxx,v 1.3 2009/01/04 17:42:58 kocolosk Exp $

#include "StChargedPionMcEvent.h"

#include <utility>
using std::make_pair;

#include "TMath.h"
#include "TLorentzVector.h"
#include "TClass.h"

#include "StChargedPionTypes.h"
#include "StChargedPionTrack.h"
#include "StChargedPionJet.h"
#include "StChargedPionVertex.h"

ClassImp(StChargedPionMcEvent)

StChargedPionMcEvent::StChargedPionMcEvent() : mSimuTriggerBits(0) {
    mVertices = new TClonesArray("StChargedPionVertex", 20);
    mTracks = new TClonesArray("StChargedPionTrack", 50);
    mJets = new TClonesArray("StChargedPionJet", 50);
    
    memset(mL2ResultEmulated, 0, 36);
        
    StChargedPionMcEvent::Class()->IgnoreTObjectStreamer();
    StChargedPionVertex::Class()->IgnoreTObjectStreamer();
    StChargedPionTrack::Class()->IgnoreTObjectStreamer();
    StChargedPionJet::Class()->IgnoreTObjectStreamer();
    StChargedPionMcTrack::Class()->IgnoreTObjectStreamer();
    StChargedPionMcJet::Class()->IgnoreTObjectStreamer();
}

StChargedPionMcEvent::~StChargedPionMcEvent() {
    if(mVertices) delete mVertices;
    if(mTracks) delete mTracks;
    if(mJets) delete mJets;
}

void StChargedPionMcEvent::Clear(Option_t* o) {
    mSimuTriggerBits = 0;
    mVertices->Clear();
    mTracks->Clear();
    
    // use ::Delete here so ROOT doesn't hold onto memory allocated for jet particles
    mJets->Delete();
    
    mHighTowers.clear();
    mTriggerPatches.clear();
    mJetPatches.clear();
    
    mMcTracks.clear();
    mMcJets.clear();
    mMatchedPairs.clear();
    mMergedPairs.clear();
    mSplitPairs.clear();
    mContamPairs.clear();
    mGhostPairs.clear();
    
    mPythiaRecord.clear();
}

StChargedPionLorentzVector
StChargedPionMcEvent::parton(int id, StChargedPionMcEvent::Frame frame) const { 
    const StChargedPionLorentzVector *v;
    switch(id) {
        case 1: v = &mParton1; break;
        case 2: v = &mParton2; break;
        case 3: v = &mParton3; break;
        case 4: v = &mParton4; break;
    }
    
    if(frame == CM) {
        Boost boost( (mParton1+mParton2).BoostToCM() );
        LorentzRotation rotate( RotationY(-1.0 * boost(mParton1).Theta()) );
        rotate *= RotationZ(-1.0 * boost(mParton1).Phi());
        return rotate(boost(*v));
    }
    else {
        return *v;        
    } 
}

TLorentzVector StChargedPionMcEvent::lv(const StChargedPionLorentzVector& v) {
    return TLorentzVector(v.X(), v.Y(), v.Z(), v.E());
}

double StChargedPionMcEvent::beta34() const {
    return TMath::Sqrt( pow(1 - mParton3.mass2()/s() - mParton4.mass2()/s(), 2) -
        4 * mParton3.mass2()/s() * mParton4.mass2()/s());
}

double StChargedPionMcEvent::s() const {
    return (mParton1 + mParton2).Dot(mParton1 + mParton2);
}

/// this version neglects outgoing parton masses, and matches g2t_pythia->mand_t
double StChargedPionMcEvent::t() const {
    return -0.5 * s() * (1-cosTheta());
}

/// this version neglects outgoing parton masses, and matches g2t_pythia->mand_u
double StChargedPionMcEvent::u() const {
    return -0.5 * s() * (1+cosTheta());
}

/// doesn't quite match hard_p in the case of b quark stuff
double StChargedPionMcEvent::pt() const {
    return parton(3, StChargedPionMcEvent::CM).Pt();
}

/// boost to CM, rotate so p1-p2 collide on z-axis, then get Theta() of p3
/// this matches g2t_pythia->cos_theta
double StChargedPionMcEvent::cosTheta() const {
    return ::cos(parton(3, StChargedPionMcEvent::CM).Theta());
}

/// this one neglects outgoing parton masses, and matches g2t_pythia->hard_p**2
double StChargedPionMcEvent::Q2() const {
    return t() * u() / s();
}

double StChargedPionMcEvent::x1() const {
    return mX1;
}

double StChargedPionMcEvent::x2() const {
    return tau()/x1();
}

double StChargedPionMcEvent::tau() const {
    return s() / (200*200);
}

double StChargedPionMcEvent::y() const {
    return 0.5 * TMath::Log(x1()/x2());
}

double StChargedPionMcEvent::xF() const {
    return (x1() - x2());
}

// differences between the versions that neglect parton masses and the versions that do
// not are typically very small -- even in the case of b-quark production they are generally
// less then 0.5%
double StChargedPionMcEvent::t_alternative() const {
    return (mParton1 - mParton3).Dot(mParton1 - mParton3);
}


double StChargedPionMcEvent::u_alternative() const {
    return (mParton1 - mParton4).Dot(mParton1 - mParton4);
}

/// eqn 83, Chapter 7 of PYTHIA 6.400 manual
double StChargedPionMcEvent::Q2_alternative() const {
    return (mParton3.mass2() + mParton4.mass2()) / 2 + 
           (t()*u() - mParton3.mass2()*mParton4.mass2()) / s();
}

// don't understand why masses need to be included here, but it matches Q2()
double StChargedPionMcEvent::Q2_alternative2() const {
    return ::pow((parton(3, StChargedPionMcEvent::CM).Et() + 
        parton(4, StChargedPionMcEvent::CM).Et()) / 2, 2);
}

bool StChargedPionMcEvent::isSimuTrigger(unsigned int trigId) const {
    map<unsigned int, unsigned int>::const_iterator it = mTriggerLookup.find(trigId);
    if(it==mTriggerLookup.end()) return false;
    return mSimuTriggerBits & it->second;
}

int StChargedPionMcEvent::highTowerAdc(short towerId) const {
    map<short, int>::const_iterator it = mHighTowers.find(towerId);
    if(it==mHighTowers.end()) return -1;
    return it->second;
}

int StChargedPionMcEvent::triggerPatchAdc(short patchId) const {
    map<short, int>::const_iterator it = mTriggerPatches.find(patchId);
    if(it==mTriggerPatches.end()) return -1;
    return it->second;
}

int StChargedPionMcEvent::jetPatchAdc(short patchId) const {
    map<short, int>::const_iterator it = mJetPatches.find(patchId);
    if(it==mJetPatches.end()) return -1;
    return it->second;
}

StChargedPionVertex* StChargedPionMcEvent::vertex(int i) {
    return static_cast<StChargedPionVertex*>(mVertices->At(i));
}

const StChargedPionVertex* StChargedPionMcEvent::vertex(int i) const {
    return static_cast<StChargedPionVertex*>(mVertices->At(i));
}

StChargedPionTrack* StChargedPionMcEvent::track(int i) {
    return static_cast<StChargedPionTrack*>(mTracks->At(i));
}

const StChargedPionTrack* StChargedPionMcEvent::track(int i) const {
    return static_cast<StChargedPionTrack*>(mTracks->At(i));
}

StChargedPionJet* StChargedPionMcEvent::jet(int i) {
    return static_cast<StChargedPionJet*>(mJets->At(i));
}

const StChargedPionJet* StChargedPionMcEvent::jet(int i) const {
    return static_cast<StChargedPionJet*>(mJets->At(i));
}

void StChargedPionMcEvent::addSimuTrigger(unsigned int trigId) {
    map<unsigned int, unsigned int>::const_iterator it = mTriggerLookup.find(trigId);
    if(it!=mTriggerLookup.end()) {
        mSimuTriggerBits |= it->second;
    }
}

void StChargedPionMcEvent::setL2Result(const void *address, bool emulated) {
    if(emulated) {
        memcpy(mL2ResultEmulated, address, 20);
        memcpy(mL2ResultEmulated+5, (int*)address+6, 16);
    }
    else { /* no-op, should throw an error */ }
}

void StChargedPionMcEvent::addVertex(const StChargedPionVertex* v) {
    new ( (*mVertices)[mVertices->GetEntriesFast()] ) StChargedPionVertex(*v);
}

void StChargedPionMcEvent::addTrack(const StChargedPionTrack* t) {
    new ( (*mTracks)[mTracks->GetEntriesFast()] ) StChargedPionTrack(*t);
}

void StChargedPionMcEvent::addJet(const StChargedPionJet* j) {
    new ( (*mJets)[mJets->GetEntriesFast()] ) StChargedPionJet(*j);
}

/*****************************************************************************
 * $Log: StChargedPionMcEvent.cxx,v $
 * Revision 1.3  2009/01/04 17:42:58  kocolosk
 * extra includes for standalone builds against ROOT 5.16+
 *
 * Revision 1.2  2008/12/29 15:58:30  kocolosk
 * removed commented code and added Id and Log as needed
 *
 * Revision 1.1  2008/07/17 17:06:31  kocolosk
 * big-bang integration StChargedPionMcEvent framework
 *
 *****************************************************************************/
