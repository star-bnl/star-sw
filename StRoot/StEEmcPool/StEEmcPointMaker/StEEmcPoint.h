#ifndef __StEEmcPoint_h__
#define __StEEmcPoint_h__
/*!
 *
 * \class StEEmcPoint
 * \date 1/1/05
 * \author Jason C. Webb <Jason.Webb@Valpo.edu>
 *
 * Base class representing an EEMC point.  Points store the energy and position
 * of gamma and electron candidates, along with additional information provided
 * by the cluster and points makers.
 *
 */

#include "TObject.h"
#include "TVector3.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcTower.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcCluster.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcSmdCluster.h"

class StEmcPoint;

class StEEmcPoint : public TObject {
public:

  StEEmcPoint();
  StEEmcPoint( const StEEmcPoint &p );
  virtual ~StEEmcPoint(){ /* nada */ };

  /// Set the position of this point at the SMD plane
  void position( const TVector3 &p ) { mPosition=p; }
  /// Set the energy of this point
  void energy( Float_t e, Int_t layer=0 ) { mEnergy[layer]=e; }
  /// Set the fraction of the tower energy used
  void fraction ( Float_t f ) { mFraction=f; }
  /// Add a tower with specified weight to the point
  void tower( const StEEmcTower &t, Float_t w=1. ) { mTowers.push_back(t); mWeights.push_back(w); }
  /// Add an smd cluster to this point
  void cluster( const StEEmcSmdCluster &c, Int_t plane ) {
    mSmdClusters[plane]=c;
    mSector=c.sector();
    if (plane==0)
	mU=c.mean();
    else
	mV=c.mean();
  }

  /// Add a tower cluster to this point
  void cluster( const StEEmcCluster &c, Int_t layer){ mTowerClusters[layer].push_back(c); }

  /// Returns tower clusters for the specified layer 0=T, 1=P, 2=Q, 3=R, 4+=crash
  StEEmcClusterVec_t &clusters(Int_t layer){ return mTowerClusters[layer]; }
  const StEEmcClusterVec_t &clusters(Int_t layer) const { return mTowerClusters[layer]; }

  /// Set the number of other points which share tower energy 
  void numberOfRelatives( Int_t r ) { mRelatives=r; }

  /// Get the position of this point
  const TVector3 &position() const { return mPosition; }
  /// Get the fraction of tower energy associated with this point
  Float_t  fraction() const { return mFraction; }
  /// Get the energy of this point
  Float_t  energy(Int_t layer=0) const { return mEnergy[layer]; }
  /// Gets the number of towers
  Int_t numberOfTowers() const { return (Int_t)mTowers.size(); }
  /// Gets a specific tower
  const StEEmcTower &tower(Int_t t) const { return mTowers[t]; }
  /// Gets the weight associated with a specific tower
  Float_t weight(Int_t t) const { return mWeights[t]; }
  /// Gets the specified cluster.
  /// \param c: 0=U, 1=V
  StEEmcSmdCluster &cluster(Int_t c) { return mSmdClusters[c]; } 
  const StEEmcSmdCluster &cluster(Int_t c) const { return mSmdClusters[c]; } 

  /// Return the number of "relative" points.  A point is related if
  /// it lies within the 3x3 patch of towers centered on this point.
  Int_t numberOfRelatives() const { return mRelatives; }

  /// Returns a pointer to a new StEmcPoint, basically a
  /// 1:1 copy of this point.
  StEmcPoint *stemc();

  /// Returns the sector 
  Int_t sector() const { return mSector; }
  /// Sets the sector
  void sector(Int_t s){ mSector=s; }

  /// Returns the width
  Float_t sigma() const { return mSigma; }
  /// Sets the width
  void sigma(Float_t s) { mSigma=s; }
  
  /// Returns mean U position
  Float_t u() const { return mU; }
  /// Sets mean U position
  void u(Float_t uu){ mU=uu; }

  /// Returns mean V position
  Float_t v() const { return mV; }
  /// Sets mean U position
  void v(Float_t vv){ mV=vv; }

  /// Get the residual in the U plane
  Float_t residueU() const { return mResidueU; }
  /// Set the residual in the U plane
  void residueU(Float_t r){ mResidueU=r; }
  /// Get the residual in the V plane
  Float_t residueV() const { return mResidueV; }
  /// Set the residual in the V plane
  void residueV(Float_t r){ mResidueV=r; }
  
  /// print
  void print() const;
  
  /// Point is less than another based on energy
  Bool_t operator<( const StEEmcPoint &other ) const { return this->energy() < other.energy(); }  
  /// Chi2 sort method
  Bool_t chiSquare( const StEEmcPoint &other ) const;

  Int_t key() const { return mKey; }
  void  key( Int_t k ) { mKey = k; }

  Float_t asymmetry() const {
    Float_t esum=cluster(0).energy()+cluster(1).energy();
    Float_t edif=cluster(0).energy()-cluster(1).energy();
    if(esum>0.)
	return edif/esum;
    return 9.0E9;
  }

  Float_t chisquared() const {
    Float_t edif = ( mSmdClusters[0].energy() - mSmdClusters[1].energy() );
    Float_t esum = ( mSmdClusters[0].energy() + mSmdClusters[1].energy() );
    Float_t nmip = 1000.0*esum / 1.3;
    if ( nmip > 0. ) return (edif*edif)/nmip;
    return -1.0;
  }

protected:
  /// Position of the point
  TVector3 mPosition;
  /// Energy of the point
  Float_t mEnergy[4];
  /// ...
  Float_t mFraction;

  /// Sector
  Int_t   mSector;
  /// Width
  Float_t mSigma;
  /// Mean U
  Float_t mU;
  /// Mean V
  Float_t mV;
  /// Residual in U
  Float_t mResidueU;
  /// Residual in V
  Float_t mResidueV;
 
  /// Smd clusters associated with this point
  StEEmcSmdCluster mSmdClusters[2];

  /// Tower clusters in each layer
  std::vector< StEEmcClusterVec_t > mTowerClusters;

  /// Towers associated with this point, and associated
  /// weights for each tower.
  StEEmcTowerVec_t mTowers;       //!
  /// Vector of weights
  std::vector<Float_t> mWeights;  //! 

  /// Number of points which share tower energy with this point
  Int_t mRelatives;

  /// Pointer to corresponding StEmcPoint (StEvent only)
  StEmcPoint *mEmcPoint;

  Int_t mKey;

  /// Makes class available to root
  ClassDef(StEEmcPoint,1);

};

typedef std::vector<StEEmcPoint> StEEmcPointVec_t;

inline Bool_t chiSquare( const StEEmcPoint &me, const StEEmcPoint &other ) { return me.chiSquare( other ); }

inline Bool_t Asymmetry( const StEEmcPoint &me, const StEEmcPoint &other ){ 
  return TMath::Abs(me.asymmetry()) < TMath::Abs(other.asymmetry()); 
}

#endif
