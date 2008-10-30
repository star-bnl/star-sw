#ifndef __StEEmcIUCluster_h__
#define __StEEmcIUCluster_h__

#include <TObject.h>
#include <TVector3.h>
#include "StEEmcPool/StEEmcA2EMaker/StEEmcTower.h"

class StEmcCluster;

class StEEmcIUCluster : public TObject {

 public:

  StEEmcIUCluster();
  ~StEEmcIUCluster();

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
  /// Get the specified tower
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

  /// Pointer to StEmcCluster for embedding
  void stemc(StEmcCluster *c){ mEmcCluster = c; }

  /// Returns unique id of the cluster
  Int_t key(){ return mKey; }
  /// Sets the unique id of the cluster
  void  key(Int_t k){ mKey=k; }

  /// Tests whether the cluster has the same seed tower as another cluster.
  /// If so, these clusters are considered equal
  Bool_t operator==( const StEEmcIUCluster &other ) const { return this->tower(0).index() == other.tower(0).index(); }

  /// Prints cluster data
  void print();
  
 private:
 protected:

  /// Unique cluster ID
  Int_t mKey;

  /// Vector of towers
  StEEmcTowerVec_t mTowers;
  /// Vector of tower weights
  std::vector<Float_t> mWeights;

  /// Energy
  Float_t mEnergy;
  /// Momentum
  TVector3 mMomentum;

  /// Pointer to EMC cluster
  StEmcCluster *mEmcCluster;

  /// Makes class available to root
  ClassDef(StEEmcIUCluster,1);

};

inline Float_t StEEmcIUCluster::energy(){ return mEnergy; }
inline Float_t StEEmcIUCluster::seedEnergy(){ return (mTowers.size())?mTowers[0].energy():0.; }

inline void StEEmcIUCluster::momentum(TVector3 p){mMomentum=p;}
inline TVector3 StEEmcIUCluster::momentum(){ return mMomentum;}
inline StEEmcTower StEEmcIUCluster::tower(Int_t t){ return mTowers[t]; } 
inline StEEmcTower StEEmcIUCluster::tower(Int_t t)const{ return mTowers[t]; } 
inline Int_t StEEmcIUCluster::numberOfTowers(){ return (Int_t)mTowers.size(); } 
inline StEEmcTowerVec_t StEEmcIUCluster::towers(){ return mTowers; } 
inline Float_t StEEmcIUCluster::weight( Int_t t ) { return mWeights[t]; } 

typedef std::vector<StEEmcIUCluster> StEEmcIUClusterVec_t;

#endif
