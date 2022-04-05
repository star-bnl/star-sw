// -*- mode: c++;-*-
// $Id: StjTrgSoftPass.h,v 1.1 2008/08/18 08:51:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGSOFTPASS_H
#define STJTRGSOFTPASS_H

#include "StjTrgSoft.h"

class StjTrgSoftPass : public StjTrgSoft {

public:
  StjTrgSoftPass() { }
  virtual ~StjTrgSoftPass() { }

  bool soft() { return true; }

private:

  ClassDef(StjTrgSoftPass, 1)

};

#endif // STJTRGSOFTPASS_H
