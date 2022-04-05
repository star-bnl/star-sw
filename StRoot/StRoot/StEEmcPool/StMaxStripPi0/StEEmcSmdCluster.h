#ifndef __StEEmcSmdCluster_h__
#define __StEEmcSmdCluster_h__

#include <TObject.h>
#include "StEEmcStrip.h"
#include "StEEmcTower.h"

class StEmcCluster;

class StEEmcSmdCluster : public TObject {

 public:

  StEEmcSmdCluster();
  ~StEEmcSmdCluster(){ /* nada */ };

  /// copy constructor
  StEEmcSmdCluster ( const StEEmcSmdCluster &c );

  /// Add an SMD strip to this cluster with the
  /// specified weight
  void add( StEEmcStrip strip, Float_t weight=1.0 );

  /// Return a unique key assigned by the cluster maker
  Int_t key(){ return mKey; }
  Int_t key()const { return mKey; }

  /// Set a unique id for this cluster... k>=0 is intended to
  /// be assigned within the cluster maker.  k<0 intended for
  /// error flags to kill off clusters.
  void  key(Int_t k){ mKey=k; }
  
  /// Return the energy of this cluster
  Float_t energy();
  Float_t energy()const;
  /// Return the mean strip number of this cluster
  Float_t mean();
  Float_t mean()const;
  /// Return the sigma -- sqrt(variance) -- of the cluster
  Float_t sigma();
  Float_t sigma()const;

  /// Return the energy of this cluster summed over 
  /// +/- nmax strips about either the mean (opts=="mean")
  /// or seed (opts=="seed") smd strip.
  Float_t energy( Int_t nmax, Option_t *opts="mean" );
  /// As above, but returns sigma instead of energy
  Float_t sigma( Int_t nmax, Option_t *opts="mean" );

  /// For interactive root sessions...
  Float_t energy3(){ return energy(3,"seed"); }
  Float_t energy5(){ return energy(5,"seed"); }
  Float_t sigma3(){ return sigma(3,"seed"); }
  Float_t sigma5(){ return sigma(5,"seed"); }

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
  Bool_t operator>( StEEmcSmdCluster &other );

  /// Returns the number of SMD strips in the cluster
  Int_t numberOfStrips(){ return (Int_t)mStrips.size(); }

  /// Returns the specified smd strip w/in the cluster
  StEEmcStrip strip(Int_t s){ return mStrips[s]; }

  Int_t plane(){ return mStrips[0].plane(); }
  Int_t sector(){ return mStrips[0].sector(); }

  /// Returns a new StEmcCluster
  StEmcCluster *stemc();

  void stemc( StEmcCluster *c ){ mEmcCluster = c; }

  void print();

 private:
 protected:
  
  Int_t mKey;

  /// Vector of strips belonging to this SMD cluster
  StEEmcStripVec_t mStrips;         //!
  std::vector<Float_t> mWeights;    //!

  /// Kludge so that root will store number of smd strips 
  Int_t mSize;  

  /// Energy of this SMD cluster
  Float_t mEnergy;

  /// Running sums to calculate mean, sigma of cluster
  Float_t mSumXW;
  Float_t mSumX2W;

  /// Mean and sigma computed after each strip is added
  Float_t mMean;
  Float_t mSigma;

  /// Vector of hit towers above this SMD cluster
  StEEmcTowerVec_t mMatchedTowers;  //! 

  StEmcCluster *mEmcCluster;

  ClassDef(StEEmcSmdCluster,1);

};

inline Float_t StEEmcSmdCluster::energy(){ return mEnergy; }
inline Float_t StEEmcSmdCluster::mean(){ return mMean; }
inline Float_t StEEmcSmdCluster::sigma(){ return mSigma; }

inline Float_t StEEmcSmdCluster::energy()const{ return mEnergy; }
inline Float_t StEEmcSmdCluster::mean()const{ return mMean; }
inline Float_t StEEmcSmdCluster::sigma()const{ return mSigma; }

inline Int_t StEEmcSmdCluster::size(){ return (Int_t)mSize; }

inline StEEmcTowerVec_t StEEmcSmdCluster::towers(){ return mMatchedTowers; }
inline Int_t            StEEmcSmdCluster::numberOfMatchedTowers(){ return (Int_t)mMatchedTowers.size(); }
inline StEEmcTower      StEEmcSmdCluster::tower(Int_t t){ return mMatchedTowers[t]; }
inline Bool_t           StEEmcSmdCluster::operator>(StEEmcSmdCluster &other){ return (this->energy() > other.energy()); }


/// use to sort smd clusters from inner to outer
inline Bool_t inner( const StEEmcSmdCluster &me, const StEEmcSmdCluster &you) { return me.mean()<you.mean(); } 
/// from outer to inner
inline Bool_t outer( const StEEmcSmdCluster &me, const StEEmcSmdCluster &you) { return me.mean()>you.mean(); }
/// based on energy
inline Bool_t Energy( const StEEmcSmdCluster &me, const StEEmcSmdCluster &you) { return me.energy()>you.energy(); }
/// based on key value
inline Bool_t Key( const StEEmcSmdCluster &me, const StEEmcSmdCluster &you) { return me.key()<you.key(); }



typedef std::vector<StEEmcSmdCluster> StEEmcSmdClusterVec_t;

#endif
