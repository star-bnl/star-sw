// $Id: StjTrackToTLorentzVector.cxx,v 1.1 2008/11/27 07:09:38 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrackToTLorentzVector.h"

#include <StjTrackList.h>

ClassImp(StjTrackToTLorentzVector)

TLorentzVector StjTrackToTLorentzVector::operator()(const StjTrack& track)
{
  TLorentzVector p4;
  p4.SetPtEtaPhiM(track.pt, track.eta, track.phi, _mass);
  return p4;
}
