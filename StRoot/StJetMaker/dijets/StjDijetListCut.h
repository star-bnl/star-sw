// -*- mode: c++;-*-
// $Id: StjDijetListCut.h,v 1.1 2008/09/11 23:34:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETLISTCUT_H
#define STJDIJETLISTCUT_H

#include <TObject.h>

#include "StjDijetCut.h"

#include "StjDijetList.h"

class StjDijetListCut : public TObject {

public:
  StjDijetListCut() { }
  virtual ~StjDijetListCut() { }
  
  StjDijetList operator()(const StjDijetList& dijetList);

  void addCut(StjDijetCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StjDijetCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjDijet& p4);

  CutList _cutList;

  ClassDef(StjDijetListCut, 1)

};

#endif // STJDIJETLISTCUT_H
