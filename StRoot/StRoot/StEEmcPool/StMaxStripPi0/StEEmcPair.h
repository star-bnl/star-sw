#ifndef __StEEmcPair_h__
#define __StEEmcPair_h__

#include "TObject.h"
#include "StEEmcPoint.h"
#include "TVector3.h"
#include <vector>

class StEEmcPair : public TObject {

 public:

  /// Default constructor (don't use except for placeholder)
  StEEmcPair();
  /// Reconstruct point pairs where vertex doesn't matter or
  /// isn't available (i.e. in production)
  StEEmcPair( StEEmcPoint p1, StEEmcPoint p2 );
  /// Reconstruct a pair of real photons with a common vertex
  StEEmcPair( StEEmcPoint p1, StEEmcPoint p2, TVector3 vertex );
  /// Reconstruct a mixed-event pair with vertex from each event
  StEEmcPair( StEEmcPoint p1, StEEmcPoint p2, TVector3 vertex1, TVector3 vertex2 );
  /// Destructor
  ~StEEmcPair(){ /* nada */ };

  /// Copy constructor
  StEEmcPair( const StEEmcPair &old );

  /// Returns specified point
  /// \parm index [0,1]
  StEEmcPoint point(Int_t index){ return mPoint[index]; } 

  /// Returns invariant mass of pair
  Float_t mass();
  /// Returns energy of pair
  Float_t energy();
  /// Returns energy-sharing of pair
  Float_t zgg();
  /// Returns opening-angle of pair
  Float_t phigg();
  /// Returns pt of pair
  Float_t pt();
  /// Returns momentum of pair
  TVector3 momentum();
  /// Returns vertex of pair
  TVector3 vertex();

 private:
 protected:

  void Kinematics();

  Float_t  mMass;     // invariant mass
  Float_t  mEnergy;   // total energy
  Float_t  mZgg;      // energy-sharing
  Float_t  mPhigg;    // opening-angle
  TVector3 mMomentum; // self-evident

  TVector3 mVertex1;  // vertex of first point
  TVector3 mVertex2;  // vertex of second point (different for background mix)
  TVector3 mVertex;   // event vertex (real pi0), average vertex (mixed)

  StEEmcPoint mPoint[2];
  
  ClassDef(StEEmcPair,1);

};

inline Float_t StEEmcPair::mass(){ return mMass; }
inline Float_t StEEmcPair::energy(){ return mEnergy; }
inline Float_t StEEmcPair::zgg(){ return mZgg; }
inline Float_t StEEmcPair::phigg(){ return mPhigg; }
inline Float_t StEEmcPair::pt(){ return mMomentum.Perp(); }
inline TVector3 StEEmcPair::momentum(){ return mMomentum; }
inline TVector3 StEEmcPair::vertex(){ return mVertex; }

typedef std::vector< StEEmcPair > StEEmcPairVec_t;

#endif
