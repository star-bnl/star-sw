/***************************************************************************
 *
 * $Id: StHbtPair.hh,v 1.3 1999/07/22 18:49:10 lisa Exp $
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
 * Revision 1.3  1999/07/22 18:49:10  lisa
 * Implement idea of Fabrice to not create and delete StHbtPair all the time
 *
 * Revision 1.2  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef ST_HBT_PAIR_HH
#define ST_HBT_PAIR_HH

#include <utility>

#include "StHbtMaker/Infrastructure/StHbtParticle.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtPair {
public:
  StHbtPair();
  StHbtPair(StHbtParticle*, StHbtParticle*);
  

  ~StHbtPair();
  //StHbtPair(const StHbtPair&);
  //StHbtPair& operator=(const StHbtPair&);

  StHbtLorentzVector fourMomentum() const;
  // track Gets:
  StHbtParticle* track1() const;
  StHbtParticle* track2() const;
  // track Sets:
  void SetTrack1(const StHbtParticle* trkPtr);
  void SetTrack2(const StHbtParticle* trkPtr);

  double qInv() const;
  double kT()   const;
  double qOut() const;
  double qSide() const;
  double mInv() const;

private:
  StHbtParticle* mTrack1;
  StHbtParticle* mTrack2;

};

inline void StHbtPair::SetTrack1(const StHbtParticle* trkPtr){mTrack1=trkPtr;}
inline void StHbtPair::SetTrack2(const StHbtParticle* trkPtr){mTrack2=trkPtr;}

inline StHbtParticle* StHbtPair::track1() const {return mTrack1;}
inline StHbtParticle* StHbtPair::track2() const {return mTrack2;}

#endif
