// $Id: StJetTPCTxt.cxx,v 1.1 2008/07/21 17:24:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetTPCTxt.h"

#include <iostream>
#include <string>
#include <sstream>

#include <TVector3.h>

using namespace std;

namespace StSpinJet {

StJetTPCTxt::StJetTPCTxt(const char* path)
 : _currentEvent(-1)
 , _oldLine("")
{
  _dataFile.open(path);
}

TrackList StJetTPCTxt::getTrackList()
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

  TrackList ret;

  for(vector<string>::const_iterator it = currentLines.begin(); it != currentLines.end(); ++it) {
    istringstream ist(*it);
    long i;

    Track track;

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


}
