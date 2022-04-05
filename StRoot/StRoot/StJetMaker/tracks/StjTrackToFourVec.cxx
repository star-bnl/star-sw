// $Id: StjTrackToFourVec.cxx,v 1.1 2008/11/27 07:09:37 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrackToFourVec.h"

ClassImp(StjTrackToFourVec)

#include "StjTrackList.h"
#include "StjFourVecList.h"

StjFourVec StjTrackToFourVec::operator()(const StjTrack& track)
{
  StjFourVec ret;
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
