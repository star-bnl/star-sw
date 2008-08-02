// -*- mode: c++;-*-
// $Id: StjEEMCTxt.h,v 1.3 2008/08/02 22:43:15 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEEMCTXT_H
#define STJEEMCTXT_H

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

#endif // STJEEMCTXT_H
