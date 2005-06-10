#ifndef __StEEmcCluster_h__
#define __StEEmcCluster_h__

#include <TObject.h>
#include <TVector3.h>
#include "StEEmcTower.h"

class StEmcCluster;

class StEEmcCluster : public TObject {

 public:

  StEEmcCluster();
  ~StEEmcCluster();

  /// add a tower to this cluster.  The code assumes that
  /// the first tower added is the seed tower, and hence
  /// is the most energetic.
  void add( StEEmcTower, Float_t weight=1.0 );
  
  /// Get energy of this cluster
  Float_t energy();
  /// Get the energy of the seed tower
  Float_t seedEnergy();
  /// Get the momentum of this cluster
  TVector3 momentum();
  /// Get the number of towers in cluster
  Int_t numberOfTowers(); 
  /// Get the specified tower
  StEEmcTower tower(Int_t t); 
  StEEmcTower tower(Int_t t) const; 

  /// Get the weight associated with tower
  Float_t weight(Int_t t); 
  /// Get the vector of towers in this cluster
  StEEmcTowerVec_t towers();

  /// Set the momentum of this cluster
  void momentum( TVector3 p);

  /// Create and return an StEmcCluster using this cluster's
  /// hits, energy, etc...  The StEmcCluster will be deleted
  /// when this cluster's destructor is called.
  StEmcCluster *stemc();

  void stemc(StEmcCluster *c){ mEmcCluster = c; }

  Int_t key(){ return mKey; }
  void  key(Int_t k){ mKey=k; }

  Bool_t operator==( const StEEmcCluster &other ) const { return this->tower(0).index() == other.tower(0).index(); }

  void print();
  
 private:
 protected:

  Int_t mKey;

  /// Vector of towers
  StEEmcTowerVec_t mTowers;
  std::vector<Float_t> mWeights;

  /// Energy
  Float_t mEnergy;
  /// Momentum
  TVector3 mMomentum;

  StEmcCluster *mEmcCluster;

  ClassDef(StEEmcCluster,1);

};

inline Float_t StEEmcCluster::energy(){ return mEnergy; }
inline Float_t StEEmcCluster::seedEnergy(){ return (mTowers.size())?mTowers[0].energy():0.; }

inline void StEEmcCluster::momentum(TVector3 p){mMomentum=p;}
inline TVector3 StEEmcCluster::momentum(){ return mMomentum;}
inline StEEmcTower StEEmcCluster::tower(Int_t t){ return mTowers[t]; } 
inline StEEmcTower StEEmcCluster::tower(Int_t t)const{ return mTowers[t]; } 
inline Int_t StEEmcCluster::numberOfTowers(){ return (Int_t)mTowers.size(); } 
inline StEEmcTowerVec_t StEEmcCluster::towers(){ return mTowers; } 
inline Float_t StEEmcCluster::weight( Int_t t ) { return mWeights[t]; } 

typedef std::vector<StEEmcCluster> StEEmcClusterVec_t;

#endif
