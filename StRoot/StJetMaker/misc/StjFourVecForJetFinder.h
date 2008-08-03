// -*- mode: c++;-*-
// $Id: StjFourVecForJetFinder.h,v 1.4 2008/08/03 00:28:58 tai Exp $
#ifndef STJFOURVECFORJETFINDER_H
#define STJFOURVECFORJETFINDER_H

#include "StJetFinder/AbstractFourVec.h"

#include "StjFourVecList.h"
#include "TLorentzVector.h"

class StjFourVecForJetFinder : public AbstractFourVec {

public:
    
  StjFourVecForJetFinder() { }

  StjFourVecForJetFinder(const StjFourVec& p)
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

  StjFourVec fourVec() const { return _fourVec; }

private:

  TLorentzVector _vec;
  StjFourVec _fourVec;

};

#endif // STJFOURVECFORJETFINDER_H

