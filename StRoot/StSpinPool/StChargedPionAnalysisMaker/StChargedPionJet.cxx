#include "StChargedPionJet.h"

#include "StChargedPionJetParticle.h"

ClassImp(StChargedPionJet)

StChargedPionJet::StChargedPionJet() : TLorentzVector() { 
    //mParticles = new TClonesArray("StChargedPionJetParticle", 200);
}

StChargedPionJet::StChargedPionJet(const StChargedPionJet& t) : TLorentzVector(t), mParticles(t.mParticles) {
    mCharge     = t.mCharge;
    mTpcCount   = t.mTpcCount;
    mBtowCount  = t.mBtowCount;
    mEtowCount  = t.mEtowCount;
    mTpcEtSum   = t.mTpcEtSum;
    mBtowEtSum  = t.mBtowEtSum;
    mEtowEtSum  = t.mEtowEtSum;
    mVertexZ    = t.mVertexZ;
    
    //mParticles = new TClonesArray("StChargedPionJetParticle", t.mParticles->GetEntriesFast());
    //for(int i=0; i<t.mParticles->GetEntriesFast(); i++) {
    //    StChargedPionJetParticle *p = static_cast<StChargedPionJetParticle*>(t.mParticles->At(i));
    //    addParticle(p);
    //}
    /*for(int i=0; i<t.mParticles.size(); i++) {
        StChargedPionJetParticle p(mParticles[i]);
        mParticles.push_back(p);
    }*/
}

StChargedPionJet::~StChargedPionJet() { /* no-op */ }

void StChargedPionJet::Clear(Option_t * o) {
    //mParticles->Clear(o);
    mParticles.clear();
}

/*StChargedPionJetParticle* StChargedPionJet::particle(int i) {
    //return static_cast<StChargedPionJetParticle*>(mParticles->At(i));
    return &(mParticles[i]);
}

const StChargedPionJetParticle* StChargedPionJet::particle(int i) const {
    //return static_cast<StChargedPionJetParticle*>(mParticles->At(i));    
    return &(mParticles[i]);
}*/

void StChargedPionJet::addParticle(StChargedPionJetParticle* p) {
    mParticles.push_back(*p);
}
