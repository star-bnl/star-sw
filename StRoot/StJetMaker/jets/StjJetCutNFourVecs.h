// -*- mode: c++;-*-
// $Id: StjJetCutNFourVecs.h,v 1.1 2008/09/12 00:32:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTNFOURVECS_H
#define STJJETCUTNFOURVECS_H

#include "StjJetCut.h"

class StjJetCutNFourVecs : public StjJetCut {

public:
  StjJetCutNFourVecs(size_t min = 0, size_t max = 60000)
    :_min(min), _max(max) { }
  virtual ~StjJetCutNFourVecs() { }

  bool operator()(const StjJet& jet)
  {
    if(jet.fourVecList.size() < _min) return true;

    if(jet.fourVecList.size() > _max) return true;

    return false;
  }

private:

  size_t  _min;
  size_t  _max;

  ClassDef(StjJetCutNFourVecs, 1)

};

#endif // STJJETCUTNFOURVECS_H
