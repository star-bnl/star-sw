// $Id: StJetEEMCTxt.cxx,v 1.2 2008/07/09 08:16:05 tai Exp $
#include "StJetEEMCTxt.h"

#include <iostream>
#include <string>
#include <sstream>

using namespace std;

namespace StSpinJet {

StJetEEMCTxt::StJetEEMCTxt(const char* path)
  : _currentEvent(-1)
  , _oldLine("")
{
  _dataFile.open(path);
}

TowerEnergyDepositList StJetEEMCTxt::getEnergyList()
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

  TowerEnergyDepositList ret;

  for(vector<string>::const_iterator it = currentLines.begin(); it != currentLines.end(); ++it) {
    istringstream ist(*it);
    long i;

    TowerEnergyDeposit dep;

    ist >> i
	>> dep.towerId
	>> dep.towerX
	>> dep.towerY
	>> dep.towerZ
	>> dep.vertexX
	>> dep.vertexY
	>> dep.vertexZ
	>> dep.energy
	>> dep.adc
	>> dep.pedestal
	>> dep.rms
	>> dep.status;

    ret.push_back(dep);
  }

  return ret;
}


}
