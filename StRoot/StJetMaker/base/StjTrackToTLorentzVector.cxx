// $Id: StjTrackToTLorentzVector.cxx,v 1.2 2008/08/02 19:22:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrackToTLorentzVector.h"

#include <StjTrackList.h>

namespace StSpinJet {

TLorentzVector StjTrackToTLorentzVector::operator()(const StjTrack& track)
{
  TLorentzVector p4;
  p4.SetPtEtaPhiM(track.pt, track.eta, track.phi, _mass);
  return p4;
}

}
