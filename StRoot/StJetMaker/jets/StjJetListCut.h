// -*- mode: c++;-*-
// $Id: StjJetListCut.h,v 1.1 2008/09/12 00:32:56 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETLISTCUT_H
#define STJJETLISTCUT_H

#include <TObject.h>

#include "StjJetCut.h"

#include "StjJetList.h"

class StjJetListCut : public TObject {

public:
  StjJetListCut() { }
  virtual ~StjJetListCut() { }
  
  StjJetList operator()(const StjJetList& fourList);

  void addCut(StjJetCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StjJetCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjJet& p4);

  CutList _cutList;

  ClassDef(StjJetListCut, 1)

};

#endif // STJJETLISTCUT_H
