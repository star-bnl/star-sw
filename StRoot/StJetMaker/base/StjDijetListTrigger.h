// -*- mode: c++;-*-
// $Id: StjDijetListTrigger.h,v 1.2 2008/09/11 22:24:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETLISTTRIGGER_H
#define STJDIJETLISTTRIGGER_H

#include <TObject.h>

#include "StjDijetList.h"

class StjDijetListTrigger : public TObject {

public:
  StjDijetListTrigger() { }
  virtual ~StjDijetListTrigger() { }

  StjDijetList operator()(const StjDijetList& inList);

private:

  virtual StjDijetList processOneItem(const StjDijetList::value_type& item) = 0;

  ClassDef(StjDijetListTrigger, 1)

};

#endif // STJDIJETLISTTRIGGER_H
