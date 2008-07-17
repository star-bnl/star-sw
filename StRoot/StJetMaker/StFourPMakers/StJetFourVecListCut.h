// -*- mode: c++;-*-
// $Id: StJetFourVecListCut.h,v 1.1 2008/07/17 19:06:26 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURVECLISTCUT_H
#define STJETFOURVECLISTCUT_H

#include "FourVecCut.h"

#include "FourVecList.h"

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
