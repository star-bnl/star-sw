#ifndef __StEEmcSmdCluster_h__
#define __StEEmcSmdCluster_h__

/*!
 *
 * \class StEEmcSmdCluster
 * \author Jason C. Webb <Jason.Webb@Valpo.edu>
 *
 * Class which represents an SMD cluster.  This
 * class derives from StEEmcBaseCluster, which allows matching of clusters
 * between layers by storing a unique cluster ID.
 *
 * To build a cluster, one uses something like the following recipe
 *
 * \code
 *
 * StEEmcSmdClusterVec_t list_of_clusters;
 * StEEmcSmdCluster cluster;
 *
 * // Get the seed strip from StEEmcA2EMaker 0=tower 1=pre1 2=pre2 3=post
 * Int_t plane = 0;
 * StEEmcStrip seed = mEEanalysis -> strip( sector, plane, index_of_seed_strip );
 *
 * cluster.add( seed );
 *
 * // Next, loop over all hit strips and add any which satisfy your clustering
 * // criteria by using cluster.add(strip).
 *
 * // Finally, add the cluster to storage
 * list_of_clusters.push_back(cluster);
 *
 * \endcode
 *
 * The energy, mean and sigma of the cluster are calculated in flight.
 * One _can_ override this by calling the energy(e), mean(m) and sigma(s)
 * methods.
 *
 * Strips can be added with a weight.
 *
 */

#include <cassert>
#include <TObject.h>
#include "StEEmcPool/StEEmcA2EMaker/StEEmcStrip.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcTower.h"

#include "StEEmcBaseCluster.h"

class TH1F;

class StEmcCluster;

class StEEmcSmdCluster : public StEEmcBaseCluster {

 public:

  StEEmcSmdCluster();
  ~StEEmcSmdCluster(){ /* nada */ };

  /// copy constructor
  StEEmcSmdCluster ( const StEEmcSmdCluster &c );

  /// Add an SMD strip to this cluster with the
  /// specified weight
  void add( StEEmcStrip strip, Float_t weight=1.0 );

  void add( StEEmcStripVec_t &strips );

 
  /// Return the energy of this cluster
  Float_t energy();
  /// Return the energy of this cluster
  Float_t energy()const;
  /// Return the mean strip number of this cluster
  Float_t mean();
   /// Return the mean strip number of this cluster
  Float_t mean()const;

  /// Return the sigma -- sqrt(variance) -- of the cluster
  Float_t sigma();
  /// Return the sigma -- sqrt(variance) -- of the cluster
  Float_t sigma()const;

  /// Set the mean of the cluster
  void mean(Float_t x){ mMean=x; }
  /// Set the width of the cluster
  void sigma(Float_t s){ mSigma=s; }
  /// Set the energy of the cluster
  void energy( Float_t e){ mEnergy=e; }

  /// Return the energy of this cluster summed over 
  /// +/- nmax strips about either the mean (opts=="mean")
  /// or seed (opts=="seed") smd strip.
  Float_t energy( Int_t nmax, Option_t *opts="mean" );
  /// As above, but returns sigma instead of energy
  Float_t sigma( Int_t nmax, Option_t *opts="mean" );

  /// Return the size (number of strips) of the cluster
  Int_t size();
  Int_t size()const;

  /// Return list of towers matching this cluster
  StEEmcTowerVec_t towers();
  /// Return number of towers matching this cluster
  Int_t numberOfMatchedTowers();
  /// Return a specific tower matching this SMD cluster
  StEEmcTower tower(Int_t t);

  /// One cluster is greater than another if energy
  /// is greater than the other.
  Bool_t operator<( const StEEmcSmdCluster &other ) const { return this->energy() < other.energy(); }
  Bool_t operator>( const StEEmcSmdCluster &other ) const { return this->energy() > other.energy(); }


  /// Returns the number of SMD strips in the cluster
  Int_t numberOfStrips(){ return (Int_t)mStrips.size(); }
  Int_t numberOfStrips()const{ return (Int_t)mStrips.size(); }

  /// Returns the specified smd strip w/in the cluster
  StEEmcStrip strip(Int_t s){ return mStrips[s]; }
  StEEmcStrip strip(Int_t s)const{ return mStrips[s]; }

  Float_t weight(Int_t s){ return mWeights[s]; }
  Float_t weight(Int_t s)const{ return mWeights[s]; } 

  /// Returns the seed strip (by convention, the first
  /// strip added to the cluster).
  StEEmcStrip seed(){ assert(mStrips.size()>0); return mStrips[0]; } 

  /// Return the plane of the cluster
  Int_t plane(){ return mPlane; }
  /// Return the sector of the cluster
  Int_t sector(){ return mSector; }
  /// Set the sector
  void sector( Int_t s ) { mSector=s; }
  /// Set the plane
  void plane( Int_t p ){ mPlane = p; }

  /// Returns a new StEmcCluster or the corresponding StEmcCluster if
  /// it already exists
  StEmcCluster *stemc();
  /// Set pointer to StEmcCluster
  void stemc( StEmcCluster *c ){ mEmcCluster = c; }
  /// print
  void print(Option_t *opts="");
  void printLine(Bool_t endline=false);

  /// return the index of the next strip in the specified direction.
  /// \param: direct, negative is left, positive is right
  Int_t next(Int_t direct){ if(direct>0) return mRight; else if(direct<0) return mLeft; else return -999; }

  void copy( TH1F *h );

 private:
 protected:
  
  /// Vector of strips belonging to this SMD cluster
  StEEmcStripVec_t mStrips;         //!
  /// Vector of strip weights
  std::vector<Float_t> mWeights;    //!

  /// Kludge so that root will store number of smd strips 
  Int_t mSize;  

  /// Running sums to calculate mean, sigma of cluster
  Float_t mSumXW;
  /// Running sums to calculate mean, sigma of cluster
  Float_t mSumX2W;

  /// Mean and sigma computed after each strip is added
  Float_t mMean;
  /// Sigma
  Float_t mSigma;
  /// Plane
  Int_t   mPlane;
  /// Sector
  Int_t   mSector;

  /// Vector of hit towers above this SMD cluster
  StEEmcTowerVec_t mMatchedTowers;  //! 

  /// index of next strip to the left
  Int_t mLeft;
  /// index of next strip to the right
  Int_t mRight;

  /// POinter to EMC cluster
  StEmcCluster *mEmcCluster;

  /// Makes class available to root
  ClassDef(StEEmcSmdCluster,1);

};

inline Float_t StEEmcSmdCluster::energy(){ return mEnergy; }
inline Float_t StEEmcSmdCluster::mean(){ return mMean; }
inline Float_t StEEmcSmdCluster::sigma(){ return mSigma; }

inline Float_t StEEmcSmdCluster::energy()const{ return mEnergy; }
inline Float_t StEEmcSmdCluster::mean()const{ return mMean; }
inline Float_t StEEmcSmdCluster::sigma()const{ return mSigma; }

inline Int_t StEEmcSmdCluster::size(){ return (Int_t)mSize; }
inline Int_t StEEmcSmdCluster::size()const{ return (Int_t)mSize; }

inline StEEmcTowerVec_t StEEmcSmdCluster::towers(){ return mMatchedTowers; }
inline Int_t            StEEmcSmdCluster::numberOfMatchedTowers(){ return (Int_t)mMatchedTowers.size(); }
inline StEEmcTower      StEEmcSmdCluster::tower(Int_t t){ return mMatchedTowers[t]; }
//inline Bool_t           StEEmcSmdCluster::operator>(StEEmcSmdCluster &other){ return (energy() > other.energy()); }


/// use to sort smd clusters from inner to outer
inline Bool_t inner( const StEEmcSmdCluster &me, const StEEmcSmdCluster &you) { return me.mean()<you.mean(); } 
/// from outer to inner
inline Bool_t outer( const StEEmcSmdCluster &me, const StEEmcSmdCluster &you) { return me.mean()>you.mean(); }
/// based on energy
inline Bool_t Energy( const StEEmcSmdCluster &me, const StEEmcSmdCluster &you) { return me.energy()>you.energy(); }
/// based on key value
inline Bool_t Key( const StEEmcSmdCluster &me, const StEEmcSmdCluster &you) { return me.key()<you.key(); }

typedef std::vector<StEEmcSmdCluster> StEEmcSmdClusterVec_t;

ostream& operator<<(ostream &out, const StEEmcSmdCluster &c );

#endif
