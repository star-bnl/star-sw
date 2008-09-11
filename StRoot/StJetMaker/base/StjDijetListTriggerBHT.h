// -*- mode: c++;-*-
// $Id: StjDijetListTriggerBHT.h,v 1.2 2008/09/11 22:24:36 tai Exp $
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

  StjDijetList processOneItem(const StjDijetList::value_type& item);

  ClassDef(StjDijetListTriggerBHT, 1)

};

#endif // STJDIJETLISTTRIGGERBHT2_H
