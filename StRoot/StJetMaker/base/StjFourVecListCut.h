// -*- mode: c++;-*-
// $Id: StjFourVecListCut.h,v 1.1 2008/08/02 04:15:21 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURVECLISTCUT_H
#define STJETFOURVECLISTCUT_H

#include "StjFourVecCut.h"

#include "StjFourVecList.h"

namespace StSpinJet {

class StJetFourVecListCut {

public:
  StJetFourVecListCut() { }
  virtual ~StJetFourVecListCut() { }
  
  FourVecList operator()(const FourVecList& fourList);

  void addCut(StJetFourVecCut::FourVecCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetFourVecCut::FourVecCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const FourVec& p4);

  CutList _cutList;

};

}

#endif // STJETFOURLISTCUT_H
