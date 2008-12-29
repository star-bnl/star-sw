// $Id: StChargedPionJet.cxx,v 1.5 2008/12/29 15:58:29 kocolosk Exp $

#include "StChargedPionJet.h"

#include "TMath.h"
#include "StChargedPionEvent.h"
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

StChargedPionJet::~StChargedPionJet() { 
    this->Clear();
}

void StChargedPionJet::Clear(Option_t * o) {
    mParticles.clear();
    mGeomTriggers = 0;
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

bool StChargedPionJet::isTrigger(unsigned int trigId) const {
    return mGeomTriggers & StChargedPionEvent::triggerBit(trigId);
}

void StChargedPionJet::addTrigger(unsigned int trigId) {
    mGeomTriggers |= StChargedPionEvent::triggerBit(trigId);
}

void StChargedPionJet::addParticle(StChargedPionJetParticle* p) {
    mParticles.push_back(*p);
}

const StChargedPionJetParticle& StChargedPionJet::leadingParticle() const {
    const StChargedPionJetParticle *p = NULL;
    for(unsigned i=0; i< particles().size(); i++) {
        if(!p) p = &(particles()[i]);
        else {
            if(p->z(*this) < particles()[i].z(*this) ) 
                p = &(particles()[i]);
        }
    }
    return *p;
}

/*****************************************************************************
 * $Log: StChargedPionJet.cxx,v $
 * Revision 1.5  2008/12/29 15:58:29  kocolosk
 * removed commented code and added Id and Log as needed
 *
 *****************************************************************************/
