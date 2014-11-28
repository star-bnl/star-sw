/***************************************************************************
 *
 * $Id: StHbtTriplet.hh,v 1.3 2001/06/05 00:59:30 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *    The triplet object is passed to the TripletCuts for verification, and
 *    then to the AddRealTriplet and AddMixedTriplet methods of the
 *    three-particle correlation functions.
 *
 ***************************************************************************
 *
 * $Log: StHbtTriplet.hh,v $
 * Revision 1.3  2001/06/05 00:59:30  willson
 * Added entrance separation and quality methods
 *
 * Revision 1.2  2000/04/12 01:56:03  willson
 * Qinv Correlation Functions corrected
 *
 *
 ***************************************************************************/

#ifndef ST_HBT_TRIPLET_HH
#define ST_HBT_TRIPLET_HH

#include <utility>

#include "StHbtMaker/Infrastructure/StHbtParticle.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtTriplet {
public:
  StHbtTriplet();
  StHbtTriplet(StHbtParticle*, StHbtParticle*, StHbtParticle*);
  

  ~StHbtTriplet();
  //StHbtTriplet(const StHbtTriplet&);
  //StHbtTriplet& operator=(const StHbtTriplet&);

  // track Gets:
  StHbtParticle* track1() const;
  StHbtParticle* track2() const;
  StHbtParticle* track3() const;
  // track Sets:
  void SetTrack1(const StHbtParticle* trkPtr);
  void SetTrack2(const StHbtParticle* trkPtr);
  void SetTrack3(const StHbtParticle* trkPtr);

  StHbtLorentzVector fourMomentum() const;
  double qInv() const;
  double qInv12() const;
  double qInv23() const;
  double qInv31() const;
  double kT()   const;
  double mInv() const;

  double quality() const;
  
  // the following two methods calculate the "nominal" separation of the tracks 
  // at the inner field cage (EntranceSeparation) and when they exit the TPC,
  // which may be at the outer field cage, or at the endcaps.
  // "nominal" means that the tracks are assumed to start at (0,0,0).  Making this
  // assumption is important for the Event Mixing-- it is not a mistake. - MALisa
  double NominalTpcExitSeparation() const;
  double NominalTpcEntranceSeparation() const;
  double NominalTpcAverageSeparation() const;

private:
  StHbtParticle* mTrack1;
  StHbtParticle* mTrack2;
  StHbtParticle* mTrack3;

};

inline void StHbtTriplet::SetTrack1(const StHbtParticle* trkPtr){mTrack1=(StHbtParticle*)trkPtr;}
inline void StHbtTriplet::SetTrack2(const StHbtParticle* trkPtr){mTrack2=(StHbtParticle*)trkPtr;}
inline void StHbtTriplet::SetTrack3(const StHbtParticle* trkPtr){mTrack3=(StHbtParticle*)trkPtr;}

inline StHbtParticle* StHbtTriplet::track1() const {return mTrack1;}
inline StHbtParticle* StHbtTriplet::track2() const {return mTrack2;}
inline StHbtParticle* StHbtTriplet::track3() const {return mTrack3;}

#endif
