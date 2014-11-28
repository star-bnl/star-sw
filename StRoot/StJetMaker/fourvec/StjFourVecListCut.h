// -*- mode: c++;-*-
// $Id: StjFourVecListCut.h,v 1.1 2008/11/27 07:29:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECLISTCUT_H
#define STJFOURVECLISTCUT_H

#include <TObject.h>

#include "StjFourVecCut.h"

#include "StjFourVecList.h"

class StjFourVecListCut : public TObject {

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

  ClassDef(StjFourVecListCut, 1)

};

#endif // STJFOURVECLISTCUT_H
