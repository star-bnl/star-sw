#ifndef __StEEmcBaseCluster_h__
#define __StEEmcBaseCluster_h__

/*!
 ****************************************************************
 *
 * \class StEEmcBaseCluster
 * \author Jason C. Webb <Jason.Webb@Valpo.edu>
 *
 * Base representation of and EEMC cluster, holding information which
 * is common between tower- and smd-clusters.  This class provides
 * storage of a unique id for the cluster, and stores the unique id's
 * of clusters in other layers which "match" this cluster (as determined
 * by the cluster maker).
 *
 * StEEmcCluster and StEEmcSmdCluster represent tower/pre/post and
 * smd clusters, and derive from this class.
 *
 ****************************************************************
 */

#include "TObject.h"
#include <vector>

class StEEmcBaseCluster : public TObject {
public:
  StEEmcBaseCluster();
  virtual ~StEEmcBaseCluster(){ /* nada */ }

  Int_t key() const {return mKey;}   // returns the unique id for this cluster
  void  key(Int_t k) {mKey = k;} // sets the unique id for this cluster

  Bool_t split() const { return mSplit; }
  void   split(Bool_t s){ mSplit=s; }

  void  addMatch( Int_t key, Int_t layer ) { mMatched[layer].push_back(key); } // associates another cluster with specified key in the specified layer
  Int_t numberOfMatchingClusters( Int_t layer ) const { return (Int_t)mMatched[layer].size(); } // returns the number of matching clusters in the specified layer
  Int_t getMatch( Int_t ith, Int_t layer ) const { return mMatched[layer][ith]; } // returns the key of the ith matching cluster in the specified layer

  Float_t energy() const { return mEnergy; } // returns the energy (or energy deposit) of the cluster
  void energy( Float_t e){ mEnergy=e; } // Set the energy of the cluster
  Int_t   numberOfElements() const { return mNumberOfElements; } // returns the number of detector elements

protected:
  Int_t mKey;                                        // a unique id assigned to each cluster, (initialized to -1 to indicate an invalid cluster
  Bool_t mSplit;                                     // set true if the cluster is split from another cluster, false otherwise
  std::vector< std::vector< Int_t > > mMatched;      // keys for matching clusters in each layer
  Float_t mEnergy; // energy of the cluster
  Int_t   mNumberOfElements; // number of detector elements

  ClassDef(StEEmcBaseCluster,1);
};

#endif


