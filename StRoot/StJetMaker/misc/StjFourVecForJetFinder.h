// -*- mode: c++;-*-
// $Id: StjFourVecForJetFinder.h,v 1.2 2008/08/02 19:23:14 tai Exp $
#ifndef FOURVECFORJETFINDER_H
#define FOURVECFORJETFINDER_H

#include "StJetFinder/AbstractFourVec.h"

#include "StjFourVecList.h"
#include "TLorentzVector.h"

class StjFourVecForJetFinder : public AbstractFourVec {

public:
    
  StjFourVecForJetFinder() { }

  StjFourVecForJetFinder(const StSpinJet::StjFourVec& p)
    : _fourVec(p)
  { 
    _vec.SetPtEtaPhiM(p.pt, p.eta, p.phi, p.m);
  }

  virtual ~StjFourVecForJetFinder() { }
    
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

  StSpinJet::StjFourVec fourVec() const { return _fourVec; }

private:

  TLorentzVector _vec;
  StSpinJet::StjFourVec _fourVec;

};

#endif // FOURVECWITHID_H

