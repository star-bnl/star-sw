// $Id: StjTrackToTLorentzVector.cxx,v 1.3 2008/08/03 00:26:38 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrackToTLorentzVector.h"

#include <StjTrackList.h>

TLorentzVector StjTrackToTLorentzVector::operator()(const StjTrack& track)
{
  TLorentzVector p4;
  p4.SetPtEtaPhiM(track.pt, track.eta, track.phi, _mass);
  return p4;
}
