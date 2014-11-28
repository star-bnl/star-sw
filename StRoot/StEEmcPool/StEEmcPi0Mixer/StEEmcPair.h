#ifndef __StEEmcPair_h__
#define __StEEmcPair_h__

#include "TObject.h"
#include "StEEmcPool/StEEmcPointMaker/StEEmcPoint.h"
#include "TVector3.h"
#include <vector>

class StEEmcPair : public TObject {

 public:

  /// Default constructor (don't use except for placeholder)
  StEEmcPair();
  /// Reconstruct point pairs where vertex doesn't matter or
  /// isn't available (i.e. in production)
  StEEmcPair(const StEEmcPoint &p1, const StEEmcPoint &p2);
  /// Reconstruct a pair of real photons with a common vertex
  StEEmcPair(const StEEmcPoint &p1, const StEEmcPoint &p2, const TVector3 &vertex );
  /// Reconstruct a mixed-event pair with vertex from each event
  StEEmcPair(const StEEmcPoint &p1, const StEEmcPoint &p2, const TVector3 &vertex1, const TVector3 &vertex2 );
  /// Destructor
  virtual ~StEEmcPair(){ /* nada */ };

  /// Copy constructor
  StEEmcPair( const StEEmcPair &old );

  /// Returns specified point
  /// \param index [0,1]
  const StEEmcPoint &point(Int_t index) const { return mPoint[index]; } 

  /// Returns invariant mass of pair
  Float_t mass() const;
  /// Returns energy of pair
  Float_t energy() const;
  /// Returns energy-sharing of pair
  Float_t zgg() const;
  /// Returns opening-angle of pair
  Float_t phigg() const;
  /// Returns pt of pair
  Float_t pt() const;
  /// Returns momentum of pair
  const TVector3 &momentum() const;
  /// Returns vertex of pair
  const TVector3 &vertex() const;

  /// Prints a one-line summary of the pair
  void print() const;

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

  StEEmcPoint mPoint[2]; /**<- points */

  /// Makes class available in root
  ClassDef(StEEmcPair,1);

};

inline Float_t StEEmcPair::mass() const { return mMass; }
inline Float_t StEEmcPair::energy() const { return mEnergy; }
inline Float_t StEEmcPair::zgg() const { return mZgg; }
inline Float_t StEEmcPair::phigg() const { return mPhigg; }
inline Float_t StEEmcPair::pt() const { return mMomentum.Perp(); }
inline const TVector3 &StEEmcPair::momentum() const { return mMomentum; }
inline const TVector3 &StEEmcPair::vertex() const { return mVertex; }

typedef std::vector< StEEmcPair > StEEmcPairVec_t;

#endif
