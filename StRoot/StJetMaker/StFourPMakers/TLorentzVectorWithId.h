// -*- mode: c++;-*-
// $Id: TLorentzVectorWithId.h,v 1.5 2008/07/16 03:54:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TLORENTZVECTORWITHID_H
#define TLORENTZVECTORWITHID_H

#include <TLorentzVector.h>

#include <iostream>

class TLorentzVectorWithId : public TLorentzVector {

public:
  TLorentzVectorWithId()
  {
    ++n;
    //    std::cout << "constructor 1 : " << n << std::endl;
  }

  TLorentzVectorWithId(const TLorentzVector& p)
    : TLorentzVector(p)
  {
    ++n;
    //    std::cout << "constructor 2 : " << n << std::endl;
  }

  TLorentzVectorWithId(const TLorentzVectorWithId& p)
    : TLorentzVector(p)
    , runNumber(p.runNumber), eventId(p.eventId)
    , particleId(p.particleId), type(p.type)
    , detectorId(p.detectorId)
    , trackId(p.trackId), towerId(p.towerId)
  {
    ++n;
    //    std::cout << "constructor 3 : " << n << std::endl;
  }

  virtual ~TLorentzVectorWithId()
  {
    //    std::cout << "destractor    : " << n << std::endl;
    --n;
  }

  Int_t    runNumber;
  Int_t    eventId;
  Int_t    particleId;
  Int_t    type;        // 0: mc, 1: track, 2: tower energy
  Int_t    detectorId;  // 1: TPC, 9: BEMC, 13: EEMC
  Short_t  trackId;
  Int_t    towerId;

  static int n;

  ClassDef(TLorentzVectorWithId, 1)
};


#endif // TLORENTZVECTORWITHID_H
