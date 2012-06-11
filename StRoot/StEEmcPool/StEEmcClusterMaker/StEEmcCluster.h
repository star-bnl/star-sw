#ifndef __StEEmcCluster_h__
#define __StEEmcCluster_h__

/*!
 *
 * \class StEEmcCluster
 * \author Jason C. Webb <Jason.Webb@Valpo.edu>
 *
 * Class which represents a tower, preshower or postshwoer cluster.  This
 * class derives from StEEmcBaseCluster, which allows matching of clusters
 * between layers by storing a unique cluster ID.
 *
 * To build a cluster, one uses something like the following recipe
 *
 * \code
 *
 * StEEmcClusterVec_t list_of_clusters;
 * StEEmcCluster cluster;
 *
 * // Get the seed tower from StEEmcA2EMaker 0=tower 1=pre1 2=pre2 3=post
 * Int_t layer = 0;
 * StEEmcTower tower = mEEanalysis -> tower( index_of_seed_tower, layer );
 *
 * // Loop over neighboring towers and add them to the cluster if they exceed
 * // a specified threshold
 * for ( Int_t i=0;i<tower.numberOfNeighbors();i++ )
 * {
 *    StEEmcTower neighbor=tower.neighbor(i);
 *    if ( tower.energy() > threshold_to_add_tower )
 *    {
 *       cluster.add(neighbor);
 *    }
 * }
 *
 * // Add our cluster to the list of clusters
 * list_of_clusters.push_back( cluster );
 *
 * \endcode
 *
 */

#include <TObject.h>
#include <TVector3.h>
#include <TMath.h>
#include "StEEmcPool/StEEmcA2EMaker/StEEmcTower.h"
#include "StEEmcBaseCluster.h"

class StEmcCluster;

class StEEmcCluster : public StEEmcBaseCluster {
public:
  StEEmcCluster();
  StEEmcCluster ( const StEEmcCluster &other );
  virtual ~StEEmcCluster();

  /// add a tower to this cluster.  The code assumes that
  /// the first tower added is the seed tower, and hence
  /// is the most energetic.
  void add(const StEEmcTower &t, Float_t weight=1.0 );
  
  /// Get energy of this cluster
  Float_t energy() const {return mEnergy;}

  /// Get the energy of the seed tower
  Float_t seedEnergy() const {return (mTowers.size())?mTowers[0].energy():0.;}

  /// Get the momentum of this cluster.  The internally calculated
  /// momentum assumes z=0.
  TVector3 momentum() const {return mMomentum;}

  /// Get the position of this cluster on the endcap.  But see note
  /// for StEEmcCluster::momentum( TVector3 p ).
  TVector3 position() const {return mPosition;}

  /// Get the number of towers in cluster
  Int_t numberOfTowers() const { return (Int_t)mTowers.size(); }

  /// Get the specified tower within the cluster
  StEEmcTower &tower(Int_t t) { return mTowers[t]; }
  const StEEmcTower &tower(Int_t t) const { return mTowers[t]; }

  /// Returns the std. deviation in the energy of the towers
  /// which make up the cluster.
  Float_t sigmaE() const;

  /// Get the weight associated with tower
  Float_t weight(Int_t t) const { return mWeights[t]; }
  /// Get the vector of towers in this cluster
  StEEmcTowerVec_t &towers() { return mTowers; }
  const StEEmcTowerVec_t &towers() const { return mTowers; }

  /// Set the momentum of this cluster.  This overrides the momentum
  /// computed internally as towers are added to the cluster.  If the
  /// momentum is changed before all towers are added, the position()
  /// of the cluster will become unreliable.
  void momentum(const TVector3 &p) {mMomentum=p;}

  /// Create and return an StEmcCluster using this cluster's
  /// hits, energy, etc...  The StEmcCluster will be deleted
  /// when this cluster's destructor is called.
  StEmcCluster *stemc();

  /// Pointer to StEmcCluster for embedding
  void stemc(StEmcCluster *c){ mEmcCluster = c; }

  /// Tests whether the cluster has the same seed tower as another cluster.
  /// If so, these clusters are considered equal
  Bool_t operator==( const StEEmcCluster &other ) const { return this->tower(0).index() == other.tower(0).index(); }

  /// Prints cluster data
  void print() const;

  void printLine(Bool_t Endl=false) const;

  /// Returns true if tower is adjacent to any tower in the cluster
  Bool_t isNeighbor(const StEEmcTower &tower) const;

  /// Returns the fractional mean etabin
  Float_t fracEtabin() const;
  /// Returns the fractional mean phibin
  Float_t fracPhibin() const;
  
  /// Returns the sigma (sqrt variance) in units of etabins
  Float_t sigmaEtabin() const { return TMath::Sqrt(mSumEta2W/mEnergy-mSumEtaW*mSumEtaW/mEnergy/mEnergy); }
  Float_t sigmaPhibin() const { return TMath::Sqrt(mSumPhi2W/mEnergy-mSumPhiW*mSumPhiW/mEnergy/mEnergy); }

  /// Returns true if the specified tower is in the cluster
  Bool_t hasTower(const StEEmcTower &tower) const;

  Bool_t operator<( const StEEmcCluster &other ) const { return this->energy() < other.energy(); }
  Bool_t operator>( const StEEmcCluster &other ) const { return this->energy() > other.energy(); }

  Int_t numberOfEtabins() const;
  Int_t numberOfPhibins() const;

protected:
  /// Vector of towers
  StEEmcTowerVec_t mTowers;
  /// Vector of tower weights
  std::vector<Float_t> mWeights;

  /// Momentum
  TVector3 mMomentum;
  TVector3 mPosition;

  /// Pointer to EMC cluster
  StEmcCluster *mEmcCluster;

  Float_t mfEtabin; /**< Fractional mean etabin * energy */
  Float_t mfPhibin; /**< Fractional mean phibin * energy */

  Float_t mSumEta2W;
  Float_t mSumEtaW;
  Float_t mSumPhi2W;
  Float_t mSumPhiW;
  
  ClassDef(StEEmcCluster,1);
};

typedef std::vector<StEEmcCluster> StEEmcClusterVec_t;

#endif
