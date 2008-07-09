// -*- mode: c++;-*-
// $Id: StJetEEMCTxt.h,v 1.1 2008/07/09 07:06:44 tai Exp $
#ifndef STJETEEMCTXT_H
#define STJETEEMCTXT_H

#include "StJetEEMC.h"

#include <fstream>

namespace StSpinJet {

class StJetEEMCTxt : public StJetEEMC {

public:
  StJetEEMCTxt(const char* path);
  virtual ~StJetEEMCTxt() { }

  TowerEnergyDepositList getEnergyList();

private:

  std::ifstream _dataFile;

};

}

#endif // STJETEEMCTXT_H
