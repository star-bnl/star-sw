// -*- mode: c++;-*-
// $Id: StJetBEMCTxt.h,v 1.2 2008/07/09 08:16:04 tai Exp $
#ifndef STJETBEMCTXT_H
#define STJETBEMCTXT_H

#include "StJetBEMC.h"

#include <string>
#include <fstream>

namespace StSpinJet {

class StJetBEMCTxt : public StJetBEMC {

public:
  StJetBEMCTxt(const char* path);
  virtual ~StJetBEMCTxt() { }

  TowerEnergyDepositList getEnergyList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;
  
};

}

#endif // STJETBEMCTXT_H
