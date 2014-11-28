// -*- mode: c++;-*-
// $Id: StjJetListVariation.h,v 1.1 2008/09/12 22:32:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETLISTVARIATION_H
#define STJJETLISTVARIATION_H

#include <TObject.h>

#include "StjJetList.h"
#include "StjJetVariation.h"

#include <vector>

class StjJetListVariation : public TObject {

public:
  StjJetListVariation() { }
  virtual ~StjJetListVariation() { }
  
  StjJetList operator()(const StjJetList& inList);

  void addVariation(StjJetVariation* var) {
    _varList.push_back(var);
  }

  typedef std::vector<StjJetVariation*> VarList;
  VarList getVariationList() { return _varList; }

private:

  StjJet vary(const StjJet& item);

  VarList _varList;

  ClassDef(StjJetListVariation, 1)

};

#endif // STJJETLISTVARIATION_H
