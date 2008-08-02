// -*- mode: c++;-*-
// $Id: StjBEMCTxt.h,v 1.1 2008/08/02 04:15:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMCTXT_H
#define STJETBEMCTXT_H

#include "StjBEMC.h"

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
