// $Id: TrackToTLorentzVectorWithId.cxx,v 1.1 2008/07/15 04:44:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "TrackToTLorentzVectorWithId.h"

#include <TrackList.h>

namespace StSpinJet {

TLorentzVectorWithId TrackToTLorentzVectorWithId::operator()(const Track& track)
{
  TLorentzVectorWithId p4(_track2tlorentzvector(track));
  p4.runNumber   = track.runNumber;
  p4.eventId     = track.eventId;
  p4.type        = 1;     
  p4.detectorId  = 1;
  p4.trackId     = track.id;
  p4.towerId     = 0;
  return p4;
}

}
