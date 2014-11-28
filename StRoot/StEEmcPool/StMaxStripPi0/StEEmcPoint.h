#ifndef __StEEmcPoint_h__
#define __StEEmcPoint_h__

#include "TObject.h"
#include "TVector3.h"

#include "StEEmcTower.h"
#include "StEEmcSmdCluster.h"

class StEmcPoint;

class StEEmcPoint : public TObject {

 public:

  StEEmcPoint();
  ~StEEmcPoint(){ /* nada */ };

  /// Copy constructor
  StEEmcPoint( const StEEmcPoint &p );

  /// Set the position of this point at the SMD plane
  void position( TVector3 p ) { mPosition=p; }
  /// Set the energy of this point
  void energy( Float_t e ) { mEnergy=e; }
  /// Set the fraction of the tower energy used
  void fraction ( Float_t f ) { mFraction=f; }
  /// Add a tower with specified weight to the point
  void tower( StEEmcTower t, Float_t w=1. ) { mTowers.push_back(t); mWeights.push_back(w); }
  /// Add an smd cluster to this point
  void cluster( StEEmcSmdCluster c, Int_t plane ){mSmdClusters[plane]=c;} 

  /// Set the number of other points which share tower energy 
  void numberOfRelatives( Int_t r ){ mRelatives=r; }

  /// Get the position of this point
  TVector3 position(){ return mPosition; }
  /// Get the energy of this point
  Float_t  energy(){ return mEnergy; }
  /// Get the fraction of tower energy associated with this point
  Float_t  fraction(){ return mFraction; }
  /// Get the energy of this point
  Float_t  energy() const { return mEnergy; }
  /// Gets the number of towers
  Int_t numberOfTowers(){ return (Int_t)mTowers.size(); }
  /// Gets a specific tower
  StEEmcTower tower(Int_t t){ return mTowers[t]; }
  /// Gets the weight associated with a specific tower
  Float_t weight(Int_t t){ return mWeights[t]; }
  /// Gets the specified cluster.
  /// \param c: 0=U, 1=V
  StEEmcSmdCluster cluster(Int_t c){ return mSmdClusters[c]; } 

  /// Return the number of "relative" points.  A point is related if
  /// it lies within the 3x3 patch of towers centered on this point.
  Int_t numberOfRelatives(){ return mRelatives; }

  /// Returns a pointer to a new StEmcPoint, basically a
  /// 1:1 copy of this point.
  StEmcPoint *stemc();

  /// Returns the sector 
  Int_t sector();

  void print();
  
  Bool_t operator<( const StEEmcPoint &other ) const; 
  Bool_t chiSquare( const StEEmcPoint &other ) const;

 private:
 protected:

  /// Position and energy of the point
  TVector3 mPosition;
  Float_t mEnergy;
  Float_t mFraction;
 
  /// Smd clusters associated with this point
  StEEmcSmdCluster mSmdClusters[2];

  /// Towers associated with this point, and associated
  /// weights for each tower.
  StEEmcTowerVec_t mTowers;       //!
  std::vector<Float_t> mWeights;  //! 

  Int_t mRelatives;

  StEmcPoint *mEmcPoint;

  ClassDef(StEEmcPoint,1);

};

typedef std::vector<StEEmcPoint> StEEmcPointVec_t;

inline Bool_t StEEmcPoint::operator<( const StEEmcPoint &other ) const { return this->energy() < other.energy(); }  
inline Bool_t chiSquare( const StEEmcPoint &me, const StEEmcPoint &other ) { return me.chiSquare( other ); }

inline Int_t StEEmcPoint::sector(){ if ( mSmdClusters[0].sector()==mSmdClusters[1].sector() ) return mSmdClusters[0].sector(); else return -1; }

#endif
