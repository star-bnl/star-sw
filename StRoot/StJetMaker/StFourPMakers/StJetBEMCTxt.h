// -*- mode: c++;-*-
// $Id: StJetBEMCTxt.h,v 1.1 2008/07/09 05:35:58 tai Exp $
#ifndef STJETBEMCTXT_H
#define STJETBEMCTXT_H

#include "StJetBEMC.h"

#include <fstream>

namespace StSpinJet {

class StJetBEMCTxt : public StJetBEMC {

public:
  StJetBEMCTxt(const char* path);
  virtual ~StJetBEMCTxt() { }

  TowerEnergyDepositList getEnergyList();

private:

  std::ifstream _dataFile;

};

}

#endif // STJETBEMCTXT_H
