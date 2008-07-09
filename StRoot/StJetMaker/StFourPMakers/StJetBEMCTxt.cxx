// $Id: StJetBEMCTxt.cxx,v 1.1 2008/07/09 05:35:58 tai Exp $
#include "StJetBEMCTxt.h"

#include <iostream>
#include <string>
#include <sstream>

using namespace std;

namespace StSpinJet {

StJetBEMCTxt::StJetBEMCTxt(const char* path)
{
  _dataFile.open(path);
}

TowerEnergyDepositList StJetBEMCTxt::getEnergyList()
{
  static long currentEvent(-1);
  ++currentEvent;

  static string oldLine("");
  string line;

  vector<string> currentLines;

  while(!_dataFile.eof()) {

    if(oldLine.size()) {
      line = oldLine;
      oldLine = "";
    } else {
      getline(_dataFile, line);
    }

    istringstream ist(line);
    long i;
    ist >> i;

    if (currentEvent != i) {
      oldLine = line;
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
