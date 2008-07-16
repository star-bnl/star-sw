// -*- mode: c++;-*-
// $Id: FourVecWithId.h,v 1.1 2008/07/16 03:54:22 tai Exp $
#ifndef FOURVECWITHID_H
#define FOURVECWITHID_H

#include "StJetFinder/AbstractFourVec.h"

#include <TLorentzVectorWithId.h>

class FourVecWithId : public AbstractFourVec {

public:
    
  FourVecWithId() { }

  FourVecWithId(const TLorentzVectorWithId& p)
    : _vec(p)
  { }

  virtual ~FourVecWithId() { }
    
  double pt() const { return _vec.Pt(); }
  double px() const { return _vec.Px(); } 
  double py() const { return _vec.Py(); }
  double pz() const { return _vec.Pz(); }

  double phi()      const { return _vec.Phi(); }
  double eta()      const { return _vec.Eta(); }
  
  double eT()   const { return _vec.Et(); }

  double e()    const { return _vec.E(); }
  double mass() const { return _vec.M(); }

  double charge() const { return 0; }

  TLorentzVectorWithId vec() const { return _vec; }

private:

  TLorentzVectorWithId _vec;
};

#endif // FOURVECWITHID_H

