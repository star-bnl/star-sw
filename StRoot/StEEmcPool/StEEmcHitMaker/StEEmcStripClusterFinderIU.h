/*!
 * \class StripClusterFinderIU_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * The method used in the dissertation of Weighong He's.  This
 * implementation produces the same results as
 * StEEmcIUClusterMaker::buildSmdClusters(), but includes several
 * improvements in the implementation
 *
 * One significant change in the implementation exists: whereas the
 * StEEmcIUClusterMaker implementation prevents a strip from being a
 * seed if it is already a member of another cluster, this
 * implementation instead allows one to ensure that the seeds are at
 * least a minimum number of strips apart.  Setting
 *
 * "mMinSeedDistance = maxExtent"
 *
 * yeilds the same result as the StEEmcIUClusterMaker implementation.
 *
 */

#ifndef _ST_STRIP_CLUSTER_FINDER_IU_H_
#define _ST_STRIP_CLUSTER_FINDER_IU_H_

#include <Rtypes.h>

#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StSimpleCluster.h"
#include "StEEmcStripClusterFinder.h"
class EEmcElement_t;

class StEEmcStripClusterFinderIU_t : public StEEmcStripClusterFinder_t {

 public:
   StEEmcStripClusterFinderIU_t();
   virtual ~StEEmcStripClusterFinderIU_t() { /* */ };

   /// clear between events
   virtual void clear();

   /// find some clusters
   virtual Int_t find( const ESmdLayer_t& smdLayer, StSimpleClusterVec_t& cluster );

   // modifiers
   void setIgnoreCorners( Bool_t ignore = true ){ mIgnoreCorners = ignore; };
   void setUseNaiveFloorShape( Bool_t useNaive = true ){ mUseNaiveFloorShape = useNaive; };
   void setApplyClusterSplitting( Bool_t apply = true ){ mApplyClusterSplitting = apply; };
   void setMaxExtent( Int_t maxExtent );
   void setMinStripsPerCluster( Int_t min_strips );
   void setMinSeedDistance( Int_t min ){ mMinSeedDistance = min; };
   void setSeedFloorConst( Float_t seedFloor ){ mSeedFloor = seedFloor; };
   void setSeedEnergyThres( LayerOfSMD_t layer, Float_t thres ){ mSeedEnergyThres[ layer ] = thres; };

 protected:
   // variables for user options
   Bool_t mIgnoreCorners;
   Bool_t mUseNaiveFloorShape;
   Bool_t mApplyClusterSplitting;
   Int_t mMaxExtent;
   Int_t mMinStripsPerCluster;
   Int_t mMinSeedDistance;
   Float_t mSeedFloor;
   Float_t mSeedEnergyThres[2];

   // variable internal to the class
   Bool_t mNeedsToBeCleared;
   Float_t mSeedFloorArray[ kEEmcNumStrips ];
   Int_t mFirstClusterIDForEachStrip[ kEEmcNumStrips ];
   Int_t mClosestClusterIDForEachStrip[ kEEmcNumStrips ];

   // for sorting
   static Bool_t energyGreaterThan( const EEmcElement_t *s1, const EEmcElement_t *s2 );

 private:
   ClassDef( StEEmcStripClusterFinderIU_t, 1 );

};

#endif

/*
 * $Id: StEEmcStripClusterFinderIU.h,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinderIU.h,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
