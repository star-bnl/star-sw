// $Id: StMuTrackEmuFactory.cxx,v 1.1 2008/07/21 02:00:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StMuTrackEmuFactory.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include <StFourPMakers/StMuEmcPosition.h>
#include <StEmcUtil/geometry/StEmcGeom.h>

#include "StMuTrackEmu.h"

namespace StSpinJet {

StMuTrackEmu* StMuTrackEmuFactory::createStMuTrackEmu(const StMuTrack* track, int trackIndex, double BTesla)
{
  if(!track) return 0;

  StMuTrackEmu* trackEmu = new StMuTrackEmu();

  trackEmu->_px         = track->momentum().x();
  trackEmu->_py         = track->momentum().y();
  trackEmu->_pz         = track->momentum().z();
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

  trackEmu->_BField      = BTesla;
  trackEmu->_bemcRadius = StEmcGeom::instance("bemc")->Radius() + 5;

  StThreeVectorD momentumAt, positionAt;
  StMuEmcPosition EmcPosition;
  if (EmcPosition.trackOnEmc(&positionAt, &momentumAt, track, trackEmu->_BField, trackEmu->_bemcRadius) ||
      EmcPosition.trackOnEEmc(&positionAt, &momentumAt, track))
    {
      trackEmu->_etaext = positionAt.pseudoRapidity();
      trackEmu->_phiext = positionAt.phi();
    }
  else
    {
      trackEmu->_etaext = -999;
      trackEmu->_phiext = -999;
    }


  trackEmu->_dEdx = track->dEdx();

  trackEmu->_trackIndex = trackIndex;

  trackEmu->_id = track->id();

  return trackEmu;
}

}
