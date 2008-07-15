// -*- mode: c++;-*-
// $Id: TLorentzVectorWithId.h,v 1.4 2008/07/15 07:10:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TLORENTZVECTORWITHID_H
#define TLORENTZVECTORWITHID_H

#include <TLorentzVector.h>

class TLorentzVectorWithId : public TLorentzVector {

public:
  TLorentzVectorWithId(const TLorentzVector& lorentzvector)
    : TLorentzVector(lorentzvector) { }

  Int_t    runNumber;
  Int_t    eventId;
  Int_t    particleId;
  Int_t    type;        // 0: mc, 1: track, 2: tower energy
  Int_t    detectorId;  // 1: TPC, 9: BEMC, 13: EEMC
  Short_t  trackId;
  Int_t    towerId;

  ClassDef(TLorentzVectorWithId, 1)
};


#endif // TLORENTZVECTORWITHID_H
