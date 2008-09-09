// -*- mode: c++;-*-
// $Id: StjDijetListTriggerBHT.h,v 1.1 2008/09/09 00:12:44 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETLISTTRIGGERBHT2_H
#define STJDIJETLISTTRIGGERBHT2_H

#include "StjDijetListTrigger.h"

class StjTrg;

class StjDijetListTriggerBHT : public StjDijetListTrigger {

public:
  StjDijetListTriggerBHT(StjTrg* trg) { }
  virtual ~StjDijetListTriggerBHT() { }

private:

  ClassDef(StjDijetListTriggerBHT, 1)

};

#endif // STJDIJETLISTTRIGGERBHT2_H
