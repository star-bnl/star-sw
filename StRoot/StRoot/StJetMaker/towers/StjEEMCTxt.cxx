// $Id: StjEEMCTxt.cxx,v 1.1 2008/11/27 07:35:24 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjEEMCTxt.h"

#include <iostream>
#include <string>
#include <sstream>

#include <TVector3.h>

ClassImp(StjEEMCTxt)

using namespace std;

StjEEMCTxt::StjEEMCTxt(const char* path)
  : _currentEvent(-1)
  , _oldLine("")
{
  _dataFile.open(path);
}

StjTowerEnergyList StjEEMCTxt::getEnergyList()
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

  StjTowerEnergyList ret;

  for(vector<string>::const_iterator it = currentLines.begin(); it != currentLines.end(); ++it) {
    istringstream ist(*it);
    long i;

    StjTowerEnergy dep;

    dep.detectorId = 13;

    double x, y, z;

    ist >> i
	>> dep.towerId
	>> x
	>> y
	>> z
	>> dep.vertexX
	>> dep.vertexY
	>> dep.vertexZ
	>> dep.energy
	>> dep.adc
	>> dep.pedestal
	>> dep.rms
	>> dep.status;

    TVector3 tower(x, y, z);
    dep.towerR = tower.Perp();
    dep.towerEta = tower.Eta();
    dep.towerPhi = tower.Phi();

    ret.push_back(dep);
  }

  return ret;
}
