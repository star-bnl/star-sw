// -*- mode: c++;-*-
// $Id: TLorentzVectorWithId.h,v 1.1 2008/07/21 17:24:55 tai Exp $
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


inline bool operator==(const TLorentzVectorWithId& v1, const TLorentzVectorWithId& v2)
{
  if(v1.runNumber   != v2.runNumber ) return false;
  if(v1.eventId     != v2.eventId   ) return false;
  if(v1.particleId  != v2.particleId) return false;
  if(v1.type        != v2.type      ) return false;
  if(v1.detectorId  != v2.detectorId) return false;
  if(v1.trackId     != v2.trackId   ) return false;
  if(v1.towerId     != v2.towerId   ) return false;
  if(v1.Pt()        != v2.Pt()      ) return false;
  if(v1.Eta()       != v2.Eta()     ) return false;
  if(v1.Phi()       != v2.Phi()     ) return false;
  if(v1.M()         != v2.M()       ) return false;
  return true;
}

inline std::ostream& operator<<(std::ostream& out, const TLorentzVectorWithId& p)
{
  out << "Pt: " << p.Pt() << ", Eta: " << p.Eta() << ", Phi: " << p.Phi() << ", M: " << p.M() << ", .... ";
  return out;
}

#endif // TLORENTZVECTORWITHID_H
