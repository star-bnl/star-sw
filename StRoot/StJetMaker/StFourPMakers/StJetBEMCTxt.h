// -*- mode: c++;-*-
// $Id: StJetBEMCTxt.h,v 1.3 2008/07/10 20:15:21 tai Exp $
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

  TowerEnergyList getEnergyList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;
  
};

}

#endif // STJETBEMCTXT_H
