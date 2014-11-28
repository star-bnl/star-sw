#ifndef __StEEmc2x2ClusterMaker_h__
#define __StEEmc2x2ClusterMaker_h__

/*
 ***************************************************************************
 *
 * \class StEEmc2x2ClusterMaker
 * \author Jason C. Webb
 * \brief an example clustering algorithm
 *
 * StEEmc2x2ClusterMaker is an implementation of the StEEmcGenericClusterMaker.
 * It provides a clustering algorithm for all layers of the endcap.  The 
 * algorithms work as follows:
 *
 * \section towers Tower/Preshower/Postshower cluster finder
 *
 * The tower cluster finder starts by identifying all towers (or pre/post
 * elements) which exceed a user-specified seed threshold.  The list of
 * seed
 * towers is then sorted by energy.  We loop over all seed towers starting
 * with the most energetic and proceeding to the least energetic.
 *
 * For each seed tower, we create a new cluster.  We add all neighboring 
 * towers which exceed a 
 * specified minimum energy.  These towers are flagged and will not be
 * added to any other clusters.  We then treat all towers within the
 * cluster as a seed tower, and add any neighboring towers above threshold
 * to the cluster.  This procedure repeats until no new towers can be added
 * to the cluster, or a user-specified size limit (dEta,dPhi) is reached.
 *
 * The cluster is then added to the list of clusters, and the next seed 
 * tower (not previously flagged) is processed.
 *
 * \section smd SMD cluster finder
 *
 * The SMD cluster finder starts by identifying all SMD strips in a given
 * plane which are above a user-specified seed threshold.  The list of 
 * seed strips is then sorted descending in energy.  We loop over all seeds
 * starting with the most energetic and proceeding to the least energetic.
 *
 * For each seed strip, we create a new cluster and add the strip to it.
 * Neighboring strips are added until one of the following conditions
 * are met:
 *
 * 1. A strip falls below a user-defined minimum
 * 2. The next SMD strip exceeds the energy of the current strip
 * 3. The cluster fails to grow by a user-specified amount
 * 4. We have reached a user-specified size limit
 *
 * Each strip added to a cluster is flagged and will not be used by any other
 * clusters.
 *
 * The SMD cluster is added to the list of clusters if it has more than a user-
 * specified number of strips, and the next seed strip
 * (not previously flagged) is processed.
 *
 *
 ***************************************************************************
 */

#include "StEEmcGenericClusterMaker.h"
#include "TTree.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "TString.h"
#include "TFile.h"
#include "TObjArray.h"

#include "TClonesArray.h"

#include "TH1F.h"
#include "TH2F.h"

#include <map>


class StEEmc2x2ClusterParams 
{
 public:
  StEEmc2x2ClusterParams(){ /* nada */ };
  ~StEEmc2x2ClusterParams(){ /* nada */ };
};


class StEEmc2x2ClusterMaker : public StEEmcGenericClusterMaker, public StEEmc2x2ClusterParams
{
public:
  StEEmc2x2ClusterMaker(const Char_t *name, const StEEmcA2EMaker *a2e, StMuDstMaker * /*mumk*/);
  virtual ~StEEmc2x2ClusterMaker(){ /* nada */ }

  virtual Int_t Make();

  /// Sets SMD seed threshold
  void setSmdSeedEnergy( Float_t e, Float_t s=0. ) { seed_threshold=e; seed_slope=s; }

  /// Sets minimum energy to add an SMD strip to a cluster
  void setSmdMinimumEnergy( Float_t e ) { mSmdMinEnergy = e; } 

  /// Sets maximum number of strips to add on each side of the cluster
  void setSmdMaximumSize( Int_t m ) { mMaxExtent = m; } 
  /// Clusters are truncated if the energy of the cluster does not incrase
  /// by a factor "t".
  void setSmdTruncationRatio( Float_t t ) { mTruncateRatio = t; } 
  /// Set the minimum number of SMD strips to accept a cluster
  void setSmdMinimumStrips( Int_t m ) { mMinStrips = m; }
  /// Sets inflection-point break
  void setDoBreakInflection( Bool_t b ){ mBreakInflection=b; }
  /// Sets energy ratio break
  void setDoBreakTruncation( Bool_t b ){ mBreakTruncation=b; }
  /// Sets maximimum size truncation
  void setDoBreakSize( Bool_t b ){ mBreakSize=b; }

  /// Sets the maximum distance (in eta bins) from the seed tower
  void setEtaCut( Int_t nbins ){ mEtaCut = nbins; }
  /// Sets the maximum distance (in phi bins) from the seed tower
  void setPhiCut( Int_t nbins ){ mPhiCut = nbins; }
  /// Sets the minimum seed energy for each tower layer
  void setSeedEnergy( Float_t e, Int_t layer=0 ){ mSeedEnergy[layer]=e; }
  /// Sets the minimum energy required to add a tower
  void setMinimumEnergy( Float_t e, Int_t layer=0 ){ mMinEnergy[layer]=e; }

  /// Sets the (gaussian) parameters of the "floor" and sets the 
  /// control flag which activates the cut.
  void setFloor( Float_t relative_yield, Float_t width ){ mFloorParams[0]=relative_yield; mFloorParams[1]=width; }

    virtual const char* GetCVS() const
	        {static const char cvs[]="Tag $Name:  $ $Id: StEEmc2x2ClusterMaker.h,v 1.4 2014/08/06 11:42:59 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}


protected:

  //StMuDstMaker *mMuDst;
  //TFile *mFile;

  Float_t seed_threshold;
  Float_t seed_slope;

  Float_t mSmdSeedEnergy; /**< SMD seed threshold   */
  Float_t mSmdMinEnergy;  /**< SMD minimum energy   */

  Int_t   mMaxExtent;     /**< Maximum cluster size */
  Float_t mTruncateRatio; /**< Cluster truncated when \f$\frac{E_{cluster+2}}/E_{cluster} < ratio\f$ */
  Int_t   mMinStrips;     /**< Minimum number of smd strips required to form a cluster */

  Int_t mEtaCut; /**< Maximum number of eta bins around seed tower */
  Int_t mPhiCut; /**< Maximum number of phi bins around seed tower */

  // builder for tower clusters
  virtual Int_t buildTowerClusters();
  // builder for preshower clusters (both layers)
  virtual Int_t buildPreshowerClusters();
  // builder for postshower clusters
  virtual Int_t buildPostshowerClusters();
  // builder for smd clusters
  virtual Int_t buildSmdClusters();

  // builder generic builder for tower, pre and post clusters
  Int_t buildLayer( Int_t layer );

  Float_t mSeedEnergy[6]; /**< Seed energy in each layer    */
  Float_t mMinEnergy[6];  /**< Minimum energy in each layer */

  Bool_t mBreakSize;
  Bool_t mBreakTruncation;
  Bool_t mBreakInflection;
  Bool_t mUseFloor;
  Float_t mFloorParams[2];

  ClassDef(StEEmc2x2ClusterMaker,1);
};


#endif
