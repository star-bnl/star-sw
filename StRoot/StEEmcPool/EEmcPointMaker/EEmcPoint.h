#ifndef __EEmcPoint__
#define __EEmcPoint__

/*
 *
 * \class EEmcPoint
 * \author Jason C. Webb <jwebb@iucf.indiana.edu>
 *
 */

class EEmcPoint;
class EEezCluster;

#include <TObject.h>
#include <TVector3.h>
#include <TMath.h>

#include "StEEmcPool/EEmcClusterMaker/EEmcCluster.h"
#include "EEmcSmdPoint.h" 


#include <vector>
typedef std::vector<EEmcPoint*>     EEmcPointPtrVec_t;
typedef EEmcPointPtrVec_t::iterator EEmcPointPtrVecIter_t;
typedef std::vector<EEmcPoint>      EEmcPointVec_t;
typedef EEmcPointVec_t::iterator    EEmcPointVecIter_t; 

class EEmcPoint : public TObject {

 public:

  EEmcPoint();
  ~EEmcPoint(){ /* nada */ };

  /// Create EEmcPoint from an "ez" cluster
  EEmcPoint ( EEezCluster *c );

  /// Sets the sector [0..11] where this possible reconstruction
  /// point is located.
  void setSector( Int_t sec );


  /// Returns the sector in which this point lies 
  Int_t    sector();
  /// Returns the position of this point 
  TVector3 getPosition();
  
  /// Sets pointer to the tower cluster
  void setCluster ( EEmcCluster c ); 
  /// Copies the SMD point to local variable  
  void setSmdPoint ( EEmcSmdPoint p ); 
  /// Sets the fraction of energy which this SMD point
  /// carries within the tower cluster
  void setFraction( Float_t f ); 
  /// Set the energy of this point
  void setEnergy( Float_t e ); 
  /// Set the position of this point
  void setPosition( TVector3 p ); 

  /// Prints information about this point
  void print();

  /// Returns the energy
  Float_t energy();
  /// Returns a copy of the cluster
  EEmcCluster cluster();
  /// Returns a copy of the smd point
  EEmcSmdPoint smdpoint();
  /// Returns the position of the hit
  TVector3 position();

  /// Returns the mean U of the smd point
  Float_t umean();
  /// Returns the mean V of the smd point
  Float_t vmean();

  /// Sets the mean U of the point (if not derived
  /// from an SMD point).
  void setUmean(Float_t);
  /// Sets the mean V of the point (if not derived
  /// from an SMD point).
  void setVmean(Float_t);

  /// Does this point match a given tower
  Bool_t   match ( EEezTower *tower );
  /// Does this point match a given cluster
  Bool_t   match ( EEezCluster *cluster );
  /// Does this point match a given patch
  Bool_t   match ( EEezPatch *patch );

  /// Set a user-defined analysis flag
  void setFlag(Int_t f);
  /// Reset flag to 0
  void resetFlag(){ setFlag(0); }
  /// Logical or of flag
  void addFlag(Int_t f){ mFlag |= f; }
  /// Get the user-defined analysis flag
  Int_t flag();
  /// Returns the logical or with the arguement
  Int_t flag( Int_t f );

 private:
 protected:

  Int_t         mSector;   // Sector in which this point lies 
  EEmcSmdPoint  mSmdPoint; 
  EEmcCluster   mCluster; 
  Float_t       mFraction; // Fraction of the cluster energy for this point

  Float_t  mEnergy;
  TVector3 mPosition; 

  Int_t mFlag; // user-defined analysis flag

  Float_t mUmean;
  Float_t mVmean;

  ClassDef(EEmcPoint,1);

};

inline Float_t  EEmcPoint::umean(){ return mUmean; }
inline Float_t  EEmcPoint::vmean(){ return mVmean; }
inline void     EEmcPoint::setUmean(Float_t u){ mUmean=u; }
inline void     EEmcPoint::setVmean(Float_t v){ mVmean=v; }

inline void     EEmcPoint::setSector( Int_t sec ) { mSector = sec; }
inline TVector3 EEmcPoint::getPosition(){ return mPosition; }  
inline void     EEmcPoint::setCluster( EEmcCluster c ){ mCluster = c; } 
inline void     EEmcPoint::setEnergy( Float_t e ) { mEnergy = e; } 
inline void     EEmcPoint::setPosition( TVector3 p ) { mPosition = p; } 
inline void     EEmcPoint::setFraction( Float_t f ) { mFraction = f; }

inline Float_t  EEmcPoint::energy(){ return mEnergy; }
inline EEmcCluster EEmcPoint::cluster(){ return mCluster; }
inline EEmcSmdPoint EEmcPoint::smdpoint(){ return mSmdPoint; }
inline TVector3 EEmcPoint::position(){ return mPosition; }
inline Int_t EEmcPoint::sector(){ return mSector; }

inline Bool_t EEmcPoint::match( EEezTower *tower ) { return mSmdPoint.match(tower); }
inline Bool_t EEmcPoint::match( EEezCluster *cluster ) { return mSmdPoint.match(cluster); }
inline Bool_t EEmcPoint::match( EEezPatch *patch ) { return mSmdPoint.match(patch); }

inline void EEmcPoint::setFlag(Int_t f){ mFlag = f; }
inline Int_t EEmcPoint::flag(){ return mFlag; }
inline Int_t EEmcPoint::flag( Int_t f ){ return mFlag&f; }


#endif
