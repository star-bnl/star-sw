// -*- mode: c++;-*-
// $Id: StjFormDijet.h,v 1.1 2008/09/11 23:34:56 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFORMDIJET_H
#define STJFORMDIJET_H

#include <StjDijetList.h>
#include <StjJetList.h>

#include <TObject.h>

class StjFormDijet : public TObject {

public:
  StjFormDijet() { }
  virtual ~StjFormDijet() { }

  StjDijetList operator()(StjJetList jetList);

private:

  class pt_more {
  public:
    bool operator()(const StjJet& jet1, const StjJet& jet2) const
    {
      return jet1.pt > jet2.pt;
    }
  };


  ClassDef(StjFormDijet, 1)

};

#endif // STJFORMDIJET_H
