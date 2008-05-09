// $Id: StMuTrackEmuFactory.cxx,v 1.1 2008/05/09 00:54:37 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StMuTrackEmuFactory.h"

#include "StMuTrackEmu.h"

namespace StSpinJet {

StMuTrackEmu* StMuTrackEmuFactory::createStMuTrackEmu(const StMuTrack* track) const
{
  StMuTrackEmu* trackEmu = new StMuTrackEmu;

  return trackEmu;
}

}
