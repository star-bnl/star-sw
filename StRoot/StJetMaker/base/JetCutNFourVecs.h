// -*- mode: c++;-*-
// $Id: JetCutNFourVecs.h,v 1.1 2008/07/21 17:24:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUTNFOURVECS_H
#define JETCUTNFOURVECS_H

#include "JetCut.h"

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
