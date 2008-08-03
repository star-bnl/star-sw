// -*- mode: c++;-*-
// $Id: StjJetListCut.h,v 1.4 2008/08/03 00:26:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETLISTCUT_H
#define STJJETLISTCUT_H

#include "StjJetCut.h"

#include "StjJetList.h"

class StjJetListCut {

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

};

#endif // STJJETLISTCUT_H
