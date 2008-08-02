// -*- mode: c++;-*-
// $Id: StjFourVecListCut.h,v 1.3 2008/08/02 22:43:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECLISTCUT_H
#define STJFOURVECLISTCUT_H

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

#endif // STJFOURVECLISTCUT_H
