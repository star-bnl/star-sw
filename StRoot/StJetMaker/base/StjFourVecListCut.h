// -*- mode: c++;-*-
// $Id: StjFourVecListCut.h,v 1.4 2008/08/03 00:26:28 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECLISTCUT_H
#define STJFOURVECLISTCUT_H

#include "StjFourVecCut.h"

#include "StjFourVecList.h"

class StjFourVecListCut {

public:
  StjFourVecListCut() { }
  virtual ~StjFourVecListCut() { }
  
  StjFourVecList operator()(const StjFourVecList& fourList);

  void addCut(StjFourVecCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StjFourVecCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjFourVec& p4);

  CutList _cutList;

};

#endif // STJFOURVECLISTCUT_H
