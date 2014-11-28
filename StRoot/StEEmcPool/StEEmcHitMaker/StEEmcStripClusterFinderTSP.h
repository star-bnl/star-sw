/*!
 * \class StEEmcStripClusterFinderTSP_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * This algorithm applies the smoother, attributed J.W. Tukey, and then identifies clusters which each peak above threshold.
 * The Tukey-smoother is already programmed in TH1::SmoothArray(...).  
 *
 * Note: TSP stands for "apply (T)ukey-(S)moother and find (P)eaks".
 *
*/

#ifndef _ST_STRIP_CLUSTER_FINDER_TSP_H_
#define _ST_STRIP_CLUSTER_FINDER_TSP_H_


#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StEEmcStripClusterFinder.h"

class StEEmcStripClusterFinderTSP_t : public StEEmcStripClusterFinder_t {

 public:
   StEEmcStripClusterFinderTSP_t( );
   virtual ~StEEmcStripClusterFinderTSP_t();

   /// find some clusters
   virtual Int_t find( const ESmdLayer_t& stripArray, StSimpleClusterVec_t& cluster );

   void setNumSmoothIters( UInt_t val ){ mNumSmoothIters = val; };
   void setMinStripsPerCluster( UInt_t val ){ mMinStripsPerCluster = val; };
   void setMaxDist( UInt_t val ){ mMaxDist = val; };
   void setSearchMargin( UInt_t val ){ mSearchMargin = val; };
   void setSeedAbsThres( Double_t val ){ mSeedAbsThres = val; };
   void setSeedRelThres( Double_t val ){ mSeedRelThres = val; };
   void setAbsPeakValleyThres( Double_t val ){ mAbsPeakValleyThres = val; };
   void setAnomalySubFactor( Double_t val ){ mAnomalySupFactor = val; };   // 0.5 here to avoid dividing by half to take the average

 protected:
   // typedef
   typedef std::vector< Int_t > IntVec_t;

   // parameters
   UInt_t mNumSmoothIters, mMinStripsPerCluster, mMaxDist, mSearchMargin;
   Double_t mSeedAbsThres, mSeedRelThres, mAbsPeakValleyThres, mAnomalySupFactor;

   // temp storage
   Double_t mStripEnergyArray[ kEEmcNumStrips ], mSmoothedEnergyArrayA[ kEEmcNumStrips ], mSmoothedEnergyArrayB[ kEEmcNumStrips ];
   Int_t mSmallestIdx, mLargestIdx;

 private:
   ClassDef( StEEmcStripClusterFinderTSP_t, 1 );
};

#endif

/*
 * $Id: StEEmcStripClusterFinderTSP.h,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinderTSP.h,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
