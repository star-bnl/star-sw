// $Id: StjTPCTxt.cxx,v 1.1 2008/11/27 07:09:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTPCTxt.h"

#include <iostream>
#include <string>
#include <sstream>

#include <TVector3.h>

ClassImp(StjTPCTxt)

using namespace std;

StjTPCTxt::StjTPCTxt(const char* path)
 : _currentEvent(-1)
 , _oldLine("")
{
  _dataFile.open(path);
}

StjTrackList StjTPCTxt::getTrackList()
{
  ++_currentEvent;

  string line;

  vector<string> currentLines;

  while(!_dataFile.eof()) {

    if(_oldLine.size()) {
      line = _oldLine;
      _oldLine = "";
    } else {
      getline(_dataFile, line);
    }

    if(0 == line.size()) break; 

    istringstream ist(line);
    long i;
    ist >> i;

    if (_currentEvent != i) {
      _oldLine = line;
      break;
    }
    currentLines.push_back(line);
  }

  StjTrackList ret;

  for(vector<string>::const_iterator it = currentLines.begin(); it != currentLines.end(); ++it) {
    istringstream ist(*it);
    long i;

    StjTrack track;

    double px, py, pz;

    ist >> i
	>> px
	>> py
	>> pz
	>> track.flag
	>> track.nHits
	>> track.charge
	>> track.nHitsPoss
	>> track.nHitsDedx
	>> track.nHitsFit
	>> track.nSigmaPion
	>> track.Tdca
	>> track.dcaZ
	>> track.dcaD
	>> track.BField
	>> track.bemcRadius
	>> track.exitEta
	>> track.exitPhi
	>> track.dEdx
	>> track.trackIndex
	>> track.id;

    TVector3 p(px, py, pz);
    track.pt  = p.Pt();
    track.eta = p.Eta();
    track.phi = p.Phi();

    ret.push_back(track);
  }

  return ret;
}
