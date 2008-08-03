// -*- mode: c++;-*-
// $Id: StjBEMCTxt.h,v 1.4 2008/08/03 00:26:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJBEMCTXT_H
#define STJBEMCTXT_H

#include "StjBEMC.h"

#include <string>
#include <fstream>

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

#endif // STJBEMCTXT_H
