// $Id: StMuTrackEmuFactory.cxx,v 1.4 2008/06/01 18:01:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StMuTrackEmuFactory.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include <StFourPMakers/StMuEmcPosition.h>

#include "StMuTrackEmu.h"

namespace StSpinJet {

StMuTrackEmu* StMuTrackEmuFactory::createStMuTrackEmu(const StMuTrack* track) const
{
  if(!track) return 0;

  StMuTrackEmu* trackEmu = new StMuTrackEmu();

  trackEmu->_flag       = track->flag();
  trackEmu->_nHits      = track->nHits(); 
  trackEmu->_charge     = track->charge();
  trackEmu->_nHitsPoss  = track->nHitsPoss();
  trackEmu->_nHitsDedx  = track->nHitsDedx();
  trackEmu->_nHitsFit   = track->nHitsFit();
  trackEmu->_nSigmaPion = track->nSigmaPion();
  trackEmu->_Tdca       = track->dcaGlobal().mag();
  trackEmu->_dcaZ       = track->dcaZ();
  trackEmu->_dcaD       = track->dcaD();

  double bField(0.5); // to put it in Tesla
  double rad(238.6);// geom->Radius()+5.;
  StThreeVectorD momentumAt, positionAt;
  StMuEmcPosition EmcPosition;
  EmcPosition.trackOnEmc(&positionAt, &momentumAt, track, bField, rad);

  trackEmu->_etaext = positionAt.pseudoRapidity();
  trackEmu->_phiext = positionAt.phi();

  trackEmu->_dEdx = track->dEdx();

  return trackEmu;
}

}
