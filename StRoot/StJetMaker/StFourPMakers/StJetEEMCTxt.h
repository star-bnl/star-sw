// -*- mode: c++;-*-
// $Id: StJetEEMCTxt.h,v 1.3 2008/07/10 20:15:22 tai Exp $
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

  TowerEnergyList getEnergyList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;

};

}

#endif // STJETEEMCTXT_H
