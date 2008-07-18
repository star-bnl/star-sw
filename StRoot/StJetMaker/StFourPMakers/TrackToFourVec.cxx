// $Id: TrackToFourVec.cxx,v 1.2 2008/07/18 04:11:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "TrackToFourVec.h"

#include "TrackList.h"
#include "FourVecList.h"

namespace StSpinJet {

FourVec TrackToFourVec::operator()(const Track& track)
{
  FourVec ret;
  ret.runNumber   = track.runNumber;
  ret.eventId     = track.eventId;
  ret.type        = 1;     
  ret.detectorId  = track.detectorId;
  ret.trackId     = track.id;
  ret.towerId     = 0;
  ret.vertexZ     = track.vertexZ;

  TLorentzVector p4(_track2tlorentzvector(track));
  ret.pt  = p4.Pt();
  ret.eta = p4.Eta();
  ret.phi = p4.Phi();
  ret.m   = p4.M();
  return ret;
}

}
