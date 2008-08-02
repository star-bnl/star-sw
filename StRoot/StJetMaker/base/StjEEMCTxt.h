// -*- mode: c++;-*-
// $Id: StjEEMCTxt.h,v 1.2 2008/08/02 19:22:42 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETEEMCTXT_H
#define STJETEEMCTXT_H

#include "StjEEMC.h"

#include <string>
#include <fstream>

namespace StSpinJet {

class StjEEMCTxt : public StjEEMC {

public:
  StjEEMCTxt(const char* path);
  virtual ~StjEEMCTxt() { }

  StjTowerEnergyList getEnergyList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;

};

}

#endif // STJETEEMCTXT_H
