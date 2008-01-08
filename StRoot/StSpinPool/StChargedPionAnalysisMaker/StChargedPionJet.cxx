#include "StChargedPionJet.h"

#include "TMath.h"
#include "StChargedPionJetParticle.h"

ClassImp(StChargedPionJet)

StChargedPionJet::StChargedPionJet() : TLorentzVector() { }

StChargedPionJet::StChargedPionJet(const StChargedPionJet& t) : TLorentzVector(t), mParticles(t.mParticles) {
    mCharge     = t.mCharge;
    mTpcCount   = t.mTpcCount;
    mBtowCount  = t.mBtowCount;
    mEtowCount  = t.mEtowCount;
    mTpcEtSum   = t.mTpcEtSum;
    mBtowEtSum  = t.mBtowEtSum;
    mEtowEtSum  = t.mEtowEtSum;
    mVertexZ    = t.mVertexZ;
}

StChargedPionJet::~StChargedPionJet() { /* no-op */ }

void StChargedPionJet::Clear(Option_t * o) {
    mParticles.clear();
}

float StChargedPionJet::detectorEta() const { 
    return this->detectorEta(mVertexZ); 
}

float StChargedPionJet::detectorEta(float vz, float r) const {
    float hold(0.),denom(0.);
    if (Theta()==TMath::PiOver2()) { // if Jet Theta = 90 then tan is undefined
        if (vz==0) {hold = TMath::PiOver2();}
        else {hold = atan2(r,vz);}
    }
    else
    {
        denom = (r/tan(Theta()))+vz;
        if (denom==0.) {hold = TMath::PiOver2();}
        if (denom!=0.) {hold = atan2(r,denom);}
    }
    return -TMath::Log(TMath::Tan(hold/2));
}

void StChargedPionJet::addParticle(StChargedPionJetParticle* p) {
    mParticles.push_back(*p);
}
