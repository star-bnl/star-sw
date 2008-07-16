// -*- mode: c++;-*-
// $Id: TLorentzVectorForJet.h,v 1.2 2008/07/16 05:36:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TLORENTZVECTORFORJET_H
#define TLORENTZVECTORFORJET_H

#include "TLorentzVectorWithId.h"

#include <TObjArray.h>

class TLorentzVectorForJet : public TLorentzVector {

public:
  TLorentzVectorForJet(const TLorentzVectorForJet& p) 
    : TLorentzVector(p)
    , runNumber(p.runNumber), eventId(p.eventId), jetId(p.jetId)
    , particleList(p.particleList)
  {
    particleList.SetOwner(kTRUE);
  }

  TLorentzVectorForJet()
    : particleList(1000)
  {
    particleList.SetOwner(kTRUE);
  }

  TLorentzVectorForJet(const TLorentzVector& p)
    : TLorentzVector(p)
    , particleList(1000)
  {
    particleList.SetOwner(kTRUE);
  }

  virtual ~TLorentzVectorForJet()
  {
    //    particleList.Delete();
  }

  Int_t    runNumber;
  Int_t    eventId;
  Int_t    jetId;

  TObjArray particleList;

  ClassDef(TLorentzVectorForJet, 1)
};


#endif // TLORENTZVECTORFORJET_H
