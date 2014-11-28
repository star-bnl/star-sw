/*!
 * \class StEEmcStripClusterFinderTSIU_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * This is the ANL modified version of the IU algorithm, and can be
 * thought of as a combination of the IU and TSP algorithms.
 *
*/

#ifndef _ST_STRIP_CLUSTER_FINDER_TSIU_H_
#define _ST_STRIP_CLUSTER_FINDER_TSIU_H_


#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StEEmcStripClusterFinder.h"

class StEEmcStripClusterFinderTSIU_t : public StEEmcStripClusterFinder_t {

 public:
   StEEmcStripClusterFinderTSIU_t( );
   virtual ~StEEmcStripClusterFinderTSIU_t();

   /// find some clusters
   virtual Int_t find( const ESmdLayer_t& stripArray, StSimpleClusterVec_t& cluster );

   // cuts for smoothing and finding seeds
   void setNumSmoothIters( UInt_t val ){ mNumSmoothIters = val; };
   void setSeedAbsThres( Double_t val ){ mSeedAbsThres = val; };
   void setSeedRelThres( Double_t val ){ mSeedRelThres = val; };

   // cuts for building clusters around seeds
   void setNumStripsPerSide( UInt_t val ){ mNumStripsPerSide = val; };

   // cuts on whether to keep the cluster
   void setMinStripsPerCluster( UInt_t val ){ mMinStripsPerCluster = val; };
   void setMinEnergyPerCluster( Double_t val ){ mMinEnergyPerCluster = val; };

 protected:
   // parameters
   UInt_t mNumSmoothIters, mNumStripsPerSide, mMinStripsPerCluster;
   Double_t mSeedAbsThres, mSeedRelThres, mMinEnergyPerCluster;

   // temp storage
   Double_t mStripEnergyArray[ kEEmcNumStrips ], mSmoothedEnergyArray[ kEEmcNumStrips ];

 private:
   ClassDef( StEEmcStripClusterFinderTSIU_t, 1 );
};


#endif

/*
 * $Id: StEEmcStripClusterFinderTSIU.h,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinderTSIU.h,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
