/***************************************************************************
 *
 * $Id: StHbtPair.hh,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Brian Laziuk, Yale University
 *         slightly modified by Mike Lisa
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    the Pair object is passed to the PairCuts for verification, and
 *    then to the AddRealPair and AddMixedPair methods of the
 *    Correlation Functions
 *
 ***************************************************************************
 *
 * $Log: StHbtPair.hh,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef ST_HBT_PAIR_HH
#define ST_HBT_PAIR_HH

#include <utility>

#include "StHbtMaker/Infrastructure/StHbtParticle.hh"

class StHbtPair {
public:
  StHbtPair();
  StHbtPair(StHbtParticle&, StHbtParticle&);
  

  ~StHbtPair();
  //StHbtPair(const StHbtPair&);
  //StHbtPair& operator=(const StHbtPair&);

  StLorentzVector<double> fourMomentum() const;
  StHbtParticle& track1() const;
  StHbtParticle& track2() const;

  double qInv() const;
  double kT()   const;
  double qOut() const;
  double qSide() const;
  double mInv() const;

private:
  StHbtParticle& mTrack1;
  StHbtParticle& mTrack2;

};

#endif
