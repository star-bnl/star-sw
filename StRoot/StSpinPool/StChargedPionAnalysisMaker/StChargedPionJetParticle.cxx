// $Id: StChargedPionJetParticle.cxx,v 1.4 2012/11/09 03:31:34 perev Exp $

#include "StChargedPionJetParticle.h"

ClassImp(StChargedPionJetParticle)

StChargedPionJetParticle::StChargedPionJetParticle() : LorentzVector< PtEtaPhiE4D<Double32_t> >() { /* no-op */ }

StChargedPionJetParticle::~StChargedPionJetParticle() { /* no-op */ }

double StChargedPionJetParticle::z(const TLorentzVector& jet) const {
    return (X()*jet.X() + Y()*jet.Y() + Z()*jet.Z()) / ::pow(jet.P(),2);
}

/*****************************************************************************
 * $Log: StChargedPionJetParticle.cxx,v $
 * Revision 1.4  2012/11/09 03:31:34  perev
 * Cleanup
 *
 * Revision 1.3  2008/12/29 15:58:29  kocolosk
 * removed commented code and added $Id: StChargedPionJetParticle.cxx,v 1.4 2012/11/09 03:31:34 perev Exp $/$Log: StChargedPionJetParticle.cxx,v $
 * removed commented code and added $Id$/Revision 1.4  2012/11/09 03:31:34  perev
 * removed commented code and added $Id$/Cleanup
 * removed commented code and added $Id$/ as needed
 *
 *****************************************************************************/
