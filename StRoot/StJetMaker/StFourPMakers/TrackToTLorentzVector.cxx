// $Id: TrackToTLorentzVector.cxx,v 1.1 2008/07/15 04:19:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "TrackToTLorentzVector.h"

#include <TrackList.h>

namespace StSpinJet {

TLorentzVector TrackToTLorentzVector::operator()(const Track& track)
{
  TLorentzVector p4;
  p4.SetPtEtaPhiM(track.pt, track.eta, track.phi, _mass);
  return p4;
}

}
