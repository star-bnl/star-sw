#ifndef __StMyPointMaker_h__
#define __StMyPointMaker_h__

/*!
 * \class StMyPointMaker.h
 * \date 11/21/2006
 * \author Jason C. Webb <Jason.Webb@Valpo.edu>
 *
 * StMyPointMaker is a point-making algorithm designed to find photon candidates
 * for pi0 reconstruction.  It is a derived class of the StEEmcGenericPointMaker.
 *
 * Example macro:
 *
 * \example macros/runEEmcPointMaker.C
 *
 * The pointmaking algorithm works as follows.  Clusters are input from the user's
 * algorithm (see StEEmcGenericClusterMaker).  Tower clusters are ranked, descending
 * in energy.  For each tower cluster, we look at the SMD clusters which were matched
 * to it by the cluster maker and find the best matches between the u, v planes by
 * minimizing the following sum
 *
 * \f[
 * \chi^{2} = \sum_{i}^{MIN(Nu,Nv)} \frac{E_{u}^{i}-E_{v}^{i}}{{Nmips}^{i}}
 * \f]
 *
 * After all points have been found, we attempt to split points in the case
 * where 2u + 1v or 1u + 2v clusters were found matched to a tower cluster.  When this
 * is the case, we will split the singleton cluster only if it improves the &chi;<sup>2</sup>
 * equation above.
 *
 * The algorithm which is used to split the cluster works as follows.  The two clusters
 * in the resolved view (R1 and R2) act as the fitting functions in the merged view.  The
 * seed strip of R1 is aligned with the seed strip of the cluster in the merged view (M).
 * The seed strip of R2 is aligned with the leftmost strip of M, and we compute a strip-by-strip
 * &chi;<sup>2</sup>.  The seed strip of R2 is scanned accross M and a &chi;<sup>2</sup>
 * determined for each position.  We then swap R1 for R2 and repeat.  The position and
 * ordering which minimizes &chi;<sup>2</sup> is then used to split M.  
 *
 * In the case where an SMD cluster has been split, the resulting pair of clusters will have the same
 * "unique" ID as the parent cluster.
 *
 */

#include "StEEmcGenericPointMaker.h"

class StMyPointMaker : public StEEmcGenericPointMaker
{

 public:
  
  StMyPointMaker(const Char_t *name="EEmcPointMaker", StEEmcA2EMaker *a2e=NULL, StEEmcGenericClusterMaker *cl=NULL );
  ~StMyPointMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  void setSplit(){ mAllowSplitting=true; }
  void setSplitMinimumET( Float_t et ){ mSplitMinimumET=et; }
  
  void setSmdMinFraction( Float_t f ){ mSmdMinFrac=f; }

 private:
 protected:

  Bool_t mAllowSplitting;
  Float_t mSplitMinimumET;

  Float_t mSmdMinFrac; /**< minumum fractional energy for SMD clusters to form points w/in tower clusters (e.g. cut points if (E_u+E_v)/E_towers ) < mSmdMinFrac */

  /// Given three clusters in1, in2 and out1, the code will determine the best possible 
  /// division for out1 and return the split in out1 + out2.
  Bool_t split( StEEmcSmdCluster &in1, StEEmcSmdCluster &in2, StEEmcSmdCluster &out1, StEEmcSmdCluster &out2, Float_t &chi2 );

  /// Given a vector of clusters in each SMD plane, find the permutation of
  /// c2 such that c1[i], c2[i] forms the best set of candidate points.
  /// @param c1 a list of clusters in the u or v plane, whichever is smaller
  /// @param c2 a list of clusters in the u or v plane, whichever is larger
  Bool_t AssociateClusters( StEEmcSmdClusterVec_t &c1, StEEmcSmdClusterVec_t &c2 );


  Bool_t SplitClusters( StEEmcSmdClusterVec_t &c1, StEEmcSmdClusterVec_t &c2 );

  /// Given two clusters, return (e1-e2)^2/nmips
  Float_t energyChi2( StEEmcSmdCluster &c1, StEEmcSmdCluster &c2 );
  /// Given three clusters, return (e1-e2-e3)^2/nmips
  Float_t energyChi2( StEEmcSmdCluster &c1, StEEmcSmdCluster &c2, StEEmcSmdCluster &c3 );

  ClassDef(StMyPointMaker,1);

};

#endif
