// -*- mode: c++;-*-
// $Id: StjFourVecListCut.h,v 1.2 2008/08/02 19:22:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURVECLISTCUT_H
#define STJETFOURVECLISTCUT_H

#include "StjFourVecCut.h"

#include "StjFourVecList.h"

namespace StSpinJet {

class StjFourVecListCut {

public:
  StjFourVecListCut() { }
  virtual ~StjFourVecListCut() { }
  
  StjFourVecList operator()(const StjFourVecList& fourList);

  void addCut(StJetFourVecCut::StjFourVecCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetFourVecCut::StjFourVecCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjFourVec& p4);

  CutList _cutList;

};

}

#endif // STJETFOURLISTCUT_H
