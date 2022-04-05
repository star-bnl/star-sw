/*!
 * \class StEEmcStripClusterFinderGMM_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Use GMM with the AIC criterea for picking the number of clusters.
 *
*/

#ifndef _ST_STRIP_CLUSTER_FINDER_MORHAC_H_
#define _ST_STRIP_CLUSTER_FINDER_MORHAC_H_

#include <TRandom3.h>
#include <TSpectrum.h>


#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StEEmcStripClusterFinder.h"

class StEEmcStripClusterFinderMorhac_t : public StEEmcStripClusterFinder_t {

 public:
   StEEmcStripClusterFinderMorhac_t( Int_t maxNumPoints = 5, Float_t resolution = 1 );
   virtual ~StEEmcStripClusterFinderMorhac_t();

   /// find some clusters
   virtual Int_t find( const ESmdLayer_t& stripArray, StSimpleClusterVec_t& cluster );

   void setDoMarkov( Bool_t val ){ mDoMarkov = val; };
   void setRemoveBkg( Bool_t val ){ mRemoveBkg = val; };
   void setMinStripsPerCluster( Int_t val ){ mMinStripsPerCluster = val; };
   void setNumDeconIters( Int_t val ){ mNumDeconIters = val; };
   void setAverWindow( Int_t val ){ mAverWindow = val; };
   void setWidth( Float_t val ){ mWidth = val; };
   void setThreshold( Float_t val ){ mThreshold = val; };
   void setMinPeakEnergy( Float_t val ){ mMinPeakEnergy = val; };
   void setMinClusterEnergy( Float_t val ){ mMinClusterEnergy = val; };
   void setNumSmoothIters( Int_t num ){ mNumSmoothIters = num; };

 protected:
   Bool_t mRemoveBkg, mDoMarkov;
   Int_t mMaxNumPoints, mMinStripsPerCluster, mNumDeconIters, mAverWindow, mNumSmoothIters;
   Float_t mWidth, mThreshold, mMinPeakEnergy, mMinClusterEnergy;

   Float_t mStripEnergyArray[kEEmcNumStrips], mSmoothedEnergyArray[kEEmcNumStrips];
   Double_t mStripEnergyArrayTemp[kEEmcNumStrips];
   Float_t mDeconvoluted[kEEmcNumStrips];

   TSpectrum *peakFinderPtr;

 private:
   ClassDef( StEEmcStripClusterFinderMorhac_t, 1 );
};


#endif

/*
 * $Id: StEEmcStripClusterFinderMorhac.h,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinderMorhac.h,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
