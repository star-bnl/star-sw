// -*- mode: c++;-*-
// $Id: StjJetCutNFourVecs.h,v 1.1 2008/08/02 04:15:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUTNFOURVECS_H
#define JETCUTNFOURVECS_H

#include "StjJetCut.h"

namespace StJetJetCut {

class JetCutNFourVecs : public JetCut {

public:
  JetCutNFourVecs(size_t min = 0, size_t max = std::numeric_limits<size_t>::max())
    :_min(min), _max(max) { }
  virtual ~JetCutNFourVecs() { }

  bool operator()(const StSpinJet::Jet& jet)
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
