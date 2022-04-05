#ifndef __StEEmcIUPair_h__
#define __StEEmcIUPair_h__

#include "TObject.h"
#include "StEEmcIUPoint.h"
#include "TVector3.h"
#include <vector>

class StEEmcIUPair : public TObject {

 public:

  /// Default constructor (don't use except for placeholder)
  StEEmcIUPair();
  /// Reconstruct point pairs where vertex doesn't matter or
  /// isn't available (i.e. in production)
  StEEmcIUPair( StEEmcIUPoint p1, StEEmcIUPoint p2 );
  /// Reconstruct a pair of real photons with a common vertex
  StEEmcIUPair( StEEmcIUPoint p1, StEEmcIUPoint p2, TVector3 vertex );
  /// Reconstruct a mixed-event pair with vertex from each event
  StEEmcIUPair( StEEmcIUPoint p1, StEEmcIUPoint p2, TVector3 vertex1, TVector3 vertex2 );
  /// Destructor
  ~StEEmcIUPair(){ /* nada */ };

  /// Copy constructor
  StEEmcIUPair( const StEEmcIUPair &old );

  /// Returns specified point
  /// \param index [0,1]
  StEEmcIUPoint point(Int_t index){ return mPoint[index]; } 

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
  Float_t pz();
  /// Returns momentum of pair
  TVector3 momentum();
  /// Returns vertex of pair
  TVector3 vertex();

  /// Prints a one-line summary of the pair
  void print();

 private:
 protected:

  /// Called by constructor to reconstruct two-body kinematics
  void Kinematics();

  Float_t  mMass;     /**<- invariant mass */
  Float_t  mEnergy;   /**<- total energy */
  Float_t  mZgg;      /**<- energy-sharing */
  Float_t  mPhigg;    /**<- opening-angle */ 
  TVector3 mMomentum; /**<- self-evident*/

  TVector3 mVertex1;  /**<- vertex of first point */
  TVector3 mVertex2;  /**<- vertex of second point (different for background mix)*/
  TVector3 mVertex;   /**<- event vertex (real pi0), average vertex (mixed)*/

  StEEmcIUPoint mPoint[2]; /**<- points */

  /// Makes class available in root
  ClassDef(StEEmcIUPair,1);

};

inline Float_t StEEmcIUPair::mass(){ return mMass; }
inline Float_t StEEmcIUPair::energy(){ return mEnergy; }
inline Float_t StEEmcIUPair::zgg(){ return mZgg; }
inline Float_t StEEmcIUPair::phigg(){ return mPhigg; }
inline Float_t StEEmcIUPair::pt(){ return mMomentum.Perp(); }
inline Float_t StEEmcIUPair::pz(){ return mMomentum.Pz(); }
inline TVector3 StEEmcIUPair::momentum(){ return mMomentum; }
inline TVector3 StEEmcIUPair::vertex(){ return mVertex; }

typedef std::vector< StEEmcIUPair > StEEmcIUPairVec_t;

#endif
