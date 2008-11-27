// -*- mode: c++;-*-
// $Id: StjBEMCTxt.h,v 1.1 2008/11/27 07:35:23 tai Exp $
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
  
  ClassDef(StjBEMCTxt, 1)

};

#endif // STJBEMCTXT_H
