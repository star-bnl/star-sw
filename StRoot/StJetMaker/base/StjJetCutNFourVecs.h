// -*- mode: c++;-*-
// $Id: StjJetCutNFourVecs.h,v 1.2 2008/08/02 19:22:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUTNFOURVECS_H
#define JETCUTNFOURVECS_H

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

#endif // JETCUTNFOURVEC_H
