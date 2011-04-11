#ifndef __StEEmcIUSmdCluster_h__
#define __StEEmcIUSmdCluster_h__

#include <cassert>
#include <TObject.h>
#include "StEEmcPool/StEEmcA2EMaker/StEEmcStrip.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcTower.h"

class StEmcCluster;

class StEEmcIUSmdCluster : public TObject {

 public:

  StEEmcIUSmdCluster();
  ~StEEmcIUSmdCluster(){ /* nada */ };

  /// copy constructor
  StEEmcIUSmdCluster ( const StEEmcIUSmdCluster &c );

  /// Add an SMD strip to this cluster with the
  /// specified weight
  void add( StEEmcStrip strip, Float_t weight=1.0 );

  /// Return a unique key assigned by the cluster maker
  Int_t key(){ return mKey; }
  /// Return a unique key assigned by the cluster maker
  Int_t key()const { return mKey; }

  /// Set a unique id for this cluster... k>=0 is intended to
  /// be assigned within the cluster maker.  k<0 intended for
  /// error flags to kill off clusters.
  void  key(Int_t k){ mKey=k; }
  
  /// Return the energy of this cluster
  Float_t energy();
  /// Return the energy of this cluster
  Float_t energy()const;
  /// Return the mean strip number of this cluster
  Float_t mean();
   /// Return the mean strip number of this cluster
  Float_t mean()const;
  /// Return the sigma -- TMath::Sqrt(variance) -- of the cluster
  Float_t sigma();
  /// Return the sigma -- TMath::Sqrt(variance) -- of the cluster
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

  /*
  Float_t energy3(){ return energy(3,"seed"); }
  Float_t energy5(){ return energy(5,"seed"); }
  Float_t sigma3(){ return sigma(3,"seed"); }
  Float_t sigma5(){ return sigma(5,"seed"); }
  */

  /// Return the size (number of strips) of the cluster
  Int_t size();

  /// Return list of towers matching this cluster
  StEEmcTowerVec_t towers();
  /// Return number of towers matching this cluster
  Int_t numberOfMatchedTowers();
  /// Return a specific tower matching this SMD cluster
  StEEmcTower tower(Int_t t);

  /// One cluster is greater than another if energy
  /// is greater than the other.
  Bool_t operator>( StEEmcIUSmdCluster &other );

  /// Returns the number of SMD strips in the cluster
  Int_t numberOfStrips(){ return (Int_t)mStrips.size(); }

  /// Returns the specified smd strip w/in the cluster
  StEEmcStrip strip(Int_t s){ return mStrips[s]; }

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
  void print();

 private:
 protected:
  
  /// Unique key
  Int_t mKey;

  /// Vector of strips belonging to this SMD cluster
  StEEmcStripVec_t mStrips;         //!
  /// Vector of strip weights
  std::vector<Float_t> mWeights;    //!

  /// Kludge so that root will store number of smd strips 
  Int_t mSize;  

  /// Energy of this SMD cluster
  Float_t mEnergy;

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

  /// POinter to EMC cluster
  StEmcCluster *mEmcCluster;

  /// Makes class available to root
  ClassDef(StEEmcIUSmdCluster,1);

};

inline Float_t StEEmcIUSmdCluster::energy(){ return mEnergy; }
inline Float_t StEEmcIUSmdCluster::mean(){ return mMean; }
inline Float_t StEEmcIUSmdCluster::sigma(){ return mSigma; }

inline Float_t StEEmcIUSmdCluster::energy()const{ return mEnergy; }
inline Float_t StEEmcIUSmdCluster::mean()const{ return mMean; }
inline Float_t StEEmcIUSmdCluster::sigma()const{ return mSigma; }

inline Int_t StEEmcIUSmdCluster::size(){ return (Int_t)mSize; }

inline StEEmcTowerVec_t StEEmcIUSmdCluster::towers(){ return mMatchedTowers; }
inline Int_t            StEEmcIUSmdCluster::numberOfMatchedTowers(){ return (Int_t)mMatchedTowers.size(); }
inline StEEmcTower      StEEmcIUSmdCluster::tower(Int_t t){ return mMatchedTowers[t]; }
inline Bool_t           StEEmcIUSmdCluster::operator>(StEEmcIUSmdCluster &other){ return (this->energy() > other.energy()); }


/// use to sort smd clusters from inner to outer
inline Bool_t inner( const StEEmcIUSmdCluster &me, const StEEmcIUSmdCluster &you) { return me.mean()<you.mean(); } 
/// from outer to inner
inline Bool_t outer( const StEEmcIUSmdCluster &me, const StEEmcIUSmdCluster &you) { return me.mean()>you.mean(); }
/// based on energy
inline Bool_t Energy( const StEEmcIUSmdCluster &me, const StEEmcIUSmdCluster &you) { return me.energy()>you.energy(); }
/// based on key value
inline Bool_t Key(const StEEmcIUSmdCluster &me, const StEEmcIUSmdCluster &you) { return me.key()<you.key(); }



typedef std::vector<StEEmcIUSmdCluster> StEEmcIUSmdClusterVec_t;

#endif
