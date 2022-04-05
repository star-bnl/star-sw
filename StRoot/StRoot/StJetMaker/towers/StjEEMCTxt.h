// -*- mode: c++;-*-
// $Id: StjEEMCTxt.h,v 1.1 2008/11/27 07:35:24 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEEMCTXT_H
#define STJEEMCTXT_H

#include "StjEEMC.h"

#include <string>
#include <fstream>

class StjEEMCTxt : public StjEEMC {

public:
  StjEEMCTxt(const char* path);
  virtual ~StjEEMCTxt() { }

  StjTowerEnergyList getEnergyList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;

  ClassDef(StjEEMCTxt, 1)

};

#endif // STJEEMCTXT_H
