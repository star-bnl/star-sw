// -*- mode: c++;-*-
// $Id: StJetJetListCut.h,v 1.1 2008/07/18 01:39:56 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETJETLISTCUT_H
#define STJETJETLISTCUT_H

#include "JetCut.h"

#include "JetList.h"

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
