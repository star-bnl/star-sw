// $Id: StMuTrackEmuFactory.cxx,v 1.2 2008/05/09 02:11:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StMuTrackEmuFactory.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StMuTrackEmu.h"

namespace StSpinJet {

StMuTrackEmu* StMuTrackEmuFactory::createStMuTrackEmu(const StMuTrack* track) const
{
  if(!track) return 0;

  StMuTrackEmu* trackEmu = new StMuTrackEmu();

  trackEmu->_flag  = track->flag();
  trackEmu->_nHits = track->nHits(); 

  return trackEmu;
}

}
