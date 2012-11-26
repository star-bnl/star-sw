/*!
 * \class StEEmcStripClusterFinder_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Abstract base class for various (SMD) strip clustering algrothims
*/

#ifndef _ST_STRIP_CLUSTER_FINDER_H_
#define _ST_STRIP_CLUSTER_FINDER_H_

#include <Rtypes.h>

#include "StSimpleCluster.h"
#include "StFinderAlg.h"

class ESmdLayer_t;

class StEEmcStripClusterFinder_t : public StFinderAlg_t {

 public:
   StEEmcStripClusterFinder_t() : StFinderAlg_t() { /* */ };
   virtual ~StEEmcStripClusterFinder_t() { /* */ };

   /// clear things before doing the finding for the next sector or layer
   virtual void clear(){ /* */ };

   /// reset the count
   void resetClusterCount(){ mLastClusterID = -1; };

   /// find some clusters
   virtual Int_t find( const ESmdLayer_t& smdLayer, StSimpleClusterVec_t& cluster ) = 0;

   /// enum to keep track of layers
   enum LayerOfSMD_t { U_LAYER = 0, V_LAYER = 1};

   /// Set current layer and sector
   void setCurrentLayer( LayerOfSMD_t layer ){ mLayer = layer; };
   void setCurrentSector( Int_t sector ){ mSector = sector; };


 protected:
   LayerOfSMD_t mLayer;
   Int_t mSector;
   Int_t mLastClusterID;

 private:
   ClassDef( StEEmcStripClusterFinder_t, 1 );
};


#endif

/*
 * $Id: StEEmcStripClusterFinder.h,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinder.h,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
