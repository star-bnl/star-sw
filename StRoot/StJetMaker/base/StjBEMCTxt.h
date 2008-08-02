// -*- mode: c++;-*-
// $Id: StjBEMCTxt.h,v 1.3 2008/08/02 22:43:15 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJBEMCTXT_H
#define STJBEMCTXT_H

#include "StjBEMC.h"

#include <string>
#include <fstream>

namespace StSpinJet {

class StjBEMCTxt : public StjBEMC {

public:
  StjBEMCTxt(const char* path);
  virtual ~StjBEMCTxt() { }

  StjTowerEnergyList getEnergyList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;
  
};

}

#endif // STJBEMCTXT_H
