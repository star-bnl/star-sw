// -*- mode: c++;-*-
// $Id: FourVecForJetFinder.h,v 1.1 2008/07/29 00:16:44 tai Exp $
#ifndef FOURVECFORJETFINDER_H
#define FOURVECFORJETFINDER_H

#include "StJetFinder/AbstractFourVec.h"

#include "FourVecList.h"
#include "TLorentzVector.h"

class FourVecForJetFinder : public AbstractFourVec {

public:
    
  FourVecForJetFinder() { }

  FourVecForJetFinder(const StSpinJet::FourVec& p)
    : _fourVec(p)
  { 
    _vec.SetPtEtaPhiM(p.pt, p.eta, p.phi, p.m);
  }

  virtual ~FourVecForJetFinder() { }
    
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

  StSpinJet::FourVec fourVec() const { return _fourVec; }

private:

  TLorentzVector _vec;
  StSpinJet::FourVec _fourVec;

};

#endif // FOURVECWITHID_H

