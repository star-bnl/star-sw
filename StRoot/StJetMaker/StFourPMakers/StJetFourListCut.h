// -*- mode: c++;-*-
// $Id: StJetFourListCut.h,v 1.1 2008/07/17 02:19:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURLISTCUT_H
#define STJETFOURLISTCUT_H

#include "FourCut.h"

#include <TObjArray.h>

namespace StSpinJet {

class StJetFourListCut {

public:
  StJetFourListCut() { }
  virtual ~StJetFourListCut() { }
  
  TObjArray operator()(const TObjArray& fourList);

  void addCut(StJetFourCut::FourCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetFourCut::FourCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const TLorentzVectorWithId& p4);

  CutList _cutList;

};

}

#endif // STJETFOURLISTCUT_H
