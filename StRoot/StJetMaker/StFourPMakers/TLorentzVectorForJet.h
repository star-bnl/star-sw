// -*- mode: c++;-*-
// $Id: TLorentzVectorForJet.h,v 1.1 2008/07/16 03:54:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TLORENTZVECTORFORJET_H
#define TLORENTZVECTORFORJET_H

#include "TLorentzVectorWithId.h"

#include <TClonesArray.h>

class TLorentzVectorForJet : public TLorentzVector {

public:
  TLorentzVectorForJet(const TLorentzVectorForJet& p) 
    : TLorentzVector(p)
    , runNumber(p.runNumber), eventId(p.eventId), jetId(p.jetId)
    , particleList(p.particleList) { }

  TLorentzVectorForJet() 
    : particleList("TLorentzVectorWithId", 100000) { }

  TLorentzVectorForJet(const TLorentzVector& p)
    : TLorentzVector(p)
    , particleList("TLorentzVectorWithId", 100000) { }

  virtual ~TLorentzVectorForJet()
  {
    //    particleList.Delete();
  }

  Int_t    runNumber;
  Int_t    eventId;
  Int_t    jetId;

  TClonesArray particleList;

  ClassDef(TLorentzVectorForJet, 1)
};


#endif // TLORENTZVECTORFORJET_H
