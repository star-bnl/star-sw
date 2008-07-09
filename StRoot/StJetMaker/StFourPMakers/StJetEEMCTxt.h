// -*- mode: c++;-*-
// $Id: StJetEEMCTxt.h,v 1.2 2008/07/09 08:16:05 tai Exp $
#ifndef STJETEEMCTXT_H
#define STJETEEMCTXT_H

#include "StJetEEMC.h"

#include <string>
#include <fstream>

namespace StSpinJet {

class StJetEEMCTxt : public StJetEEMC {

public:
  StJetEEMCTxt(const char* path);
  virtual ~StJetEEMCTxt() { }

  TowerEnergyDepositList getEnergyList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;

};

}

#endif // STJETEEMCTXT_H
