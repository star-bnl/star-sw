// $Id: StChargedPionJetParticle.cxx,v 1.3 2008/12/29 15:58:29 kocolosk Exp $

#include "StChargedPionJetParticle.h"

ClassImp(StChargedPionJetParticle)

StChargedPionJetParticle::StChargedPionJetParticle() : LorentzVector< PtEtaPhiE4D<Double32_t> >() { /* no-op */ }

StChargedPionJetParticle::~StChargedPionJetParticle() { /* no-op */ }

double StChargedPionJetParticle::z(const TLorentzVector& jet) const {
    return (X()*jet.X() + Y()*jet.Y() + Z()*jet.Z()) / ::pow(jet.P(),2);
}

/*****************************************************************************
 * $Log: StChargedPionJetParticle.cxx,v $
 * Revision 1.3  2008/12/29 15:58:29  kocolosk
 * removed commented code and added Id and Log as needed
 *
 *****************************************************************************/
