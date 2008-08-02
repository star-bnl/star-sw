// -*- mode: c++;-*-
// $Id: StjJetListCut.h,v 1.2 2008/08/02 19:22:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETJETLISTCUT_H
#define STJETJETLISTCUT_H

#include "StjJetCut.h"

#include "StjJetList.h"

namespace StSpinJet {

class StjJetListCut {

public:
  StjJetListCut() { }
  virtual ~StjJetListCut() { }
  
  StjJetList operator()(const StjJetList& fourList);

  void addCut(StJetJetCut::StjJetCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetJetCut::StjJetCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjJet& p4);

  CutList _cutList;

};

}

#endif // STJETJETLISTCUT_H
