// -*- mode: c++;-*-
// $Id: StjJetCutNFourVecs.h,v 1.3 2008/08/02 22:43:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTNFOURVECS_H
#define STJJETCUTNFOURVECS_H

#include "StjJetCut.h"

namespace StJetJetCut {

class StjJetCutNFourVecs : public StjJetCut {

public:
  StjJetCutNFourVecs(size_t min = 0, size_t max = std::numeric_limits<size_t>::max())
    :_min(min), _max(max) { }
  virtual ~StjJetCutNFourVecs() { }

  bool operator()(const StSpinJet::StjJet& jet)
  {
    if(jet.fourVecList.size() < _min) return true;

    if(jet.fourVecList.size() > _max) return true;

    return false;
  }

private:

  size_t  _min;
  size_t  _max;

};

}

#endif // STJJETCUTNFOURVECS_H
