#include "StChargedPionJetParticle.h"

ClassImp(StChargedPionJetParticle)

StChargedPionJetParticle::StChargedPionJetParticle() : LorentzVector< PtEtaPhiE4D<Double32_t> >() { /* no-op */ }

StChargedPionJetParticle::~StChargedPionJetParticle() { /* no-op */ }

double StChargedPionJetParticle::z(const TLorentzVector& jet) const {
    return (X()*jet.X() + Y()*jet.Y() + Z()*jet.Z()) / ::pow(jet.P(),2);
}
