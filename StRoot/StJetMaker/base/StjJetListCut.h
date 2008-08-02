// -*- mode: c++;-*-
// $Id: StjJetListCut.h,v 1.1 2008/08/02 04:15:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETJETLISTCUT_H
#define STJETJETLISTCUT_H

#include "StjJetCut.h"

#include "StjJetList.h"

namespace StSpinJet {

class StJetJetListCut {

public:
  StJetJetListCut() { }
  virtual ~StJetJetListCut() { }
  
  JetList operator()(const JetList& fourList);

  void addCut(StJetJetCut::JetCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetJetCut::JetCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const Jet& p4);

  CutList _cutList;

};

}

#endif // STJETJETLISTCUT_H
