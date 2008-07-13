// -*- mode: c++;-*-
// $Id: StJetBEMCTxt.h,v 1.4 2008/07/13 10:02:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
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
