// $Id: StjTrackToTLorentzVector.cxx,v 1.1 2008/08/02 04:16:39 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrackToTLorentzVector.h"

#include <StjTrackList.h>

namespace StSpinJet {

TLorentzVector TrackToTLorentzVector::operator()(const Track& track)
{
  TLorentzVector p4;
  p4.SetPtEtaPhiM(track.pt, track.eta, track.phi, _mass);
  return p4;
}

}
