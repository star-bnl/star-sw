// -*- mode: c++;-*-
// $Id: StjDijetListTrigger.h,v 1.1 2008/09/09 00:00:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETLISTTRIGGER_H
#define STJDIJETLISTTRIGGER_H

#include <TObject.h>

#include "StjDijetList.h"

class StjDijetListTrigger : public TObject {

public:
  StjDijetListTrigger() { }
  virtual ~StjDijetListTrigger() { }

  //  StjDijetList operator()(const StjDijetList& dijetList);

private:

  ClassDef(StjDijetListTrigger, 1)

};

#endif // STJDIJETLISTTRIGGER_H
