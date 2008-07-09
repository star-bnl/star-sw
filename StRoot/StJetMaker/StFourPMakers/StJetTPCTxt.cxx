// $Id: StJetTPCTxt.cxx,v 1.3 2008/07/09 08:16:05 tai Exp $
#include "StJetTPCTxt.h"

#include "../StMuTrackEmu.h"

#include <iostream>
#include <string>
#include <sstream>

using namespace std;

namespace StSpinJet {

StJetTPCTxt::StJetTPCTxt(const char* path)
 : _currentEvent(-1)
 , _oldLine("")
{
  _dataFile.open(path);
}

StJetTPCTxt::TrackList StJetTPCTxt::getTrackList()
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

    StMuTrackEmu* track = new StMuTrackEmu();

    ist >> i
	>> track->_px
	>> track->_py
	>> track->_pz
	>> track->_flag
	>> track->_nHits
	>> track->_charge
	>> track->_nHitsPoss
	>> track->_nHitsDedx
	>> track->_nHitsFit
	>> track->_nSigmaPion
	>> track->_Tdca
	>> track->_dcaZ
	>> track->_dcaD
	>> track->_BField
	>> track->_bemcRadius
	>> track->_etaext
	>> track->_phiext
	>> track->_dEdx
	>> track->_trackIndex
	>> track->_id;

    ret.push_back(track);
  }

  return ret;
}


}
