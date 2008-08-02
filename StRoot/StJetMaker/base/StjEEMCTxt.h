// -*- mode: c++;-*-
// $Id: StjEEMCTxt.h,v 1.1 2008/08/02 04:15:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETEEMCTXT_H
#define STJETEEMCTXT_H

#include "StjEEMC.h"

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
