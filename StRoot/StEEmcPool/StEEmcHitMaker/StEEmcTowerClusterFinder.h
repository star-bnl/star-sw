/*!
 * \class StEEmcTowerClusterFinder_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Abstract base class for various tower clustering algorithms.
*/

#ifndef _ST_TOWER_CLUSTER_FINDER_H_
#define _ST_TOWER_CLUSTER_FINDER_H_

#include <Rtypes.h>

#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StSimpleCluster.h"

#include "StFinderAlg.h"

class ETowEnergy_t;

// global (ugh!) variable
// number of towers (equals 720)
const Int_t kEEmcNumTowers = kEEmcNumSectors*kEEmcNumSubSectors*kEEmcNumEtas;

class StEEmcTowerClusterFinder_t : public StFinderAlg_t {

 public:
   StEEmcTowerClusterFinder_t() : StFinderAlg_t() { /* */ };
   virtual ~StEEmcTowerClusterFinder_t() { /* */ };

   /// clear things before doing the finding for the next event
   virtual void clear() = 0;

   /// find some clusters
   virtual Int_t find( ETowEnergy_t& towEnergy, StSimpleClusterVec_t& cluster ) = 0;

 private:
   ClassDef( StEEmcTowerClusterFinder_t, 1 );

};

#endif

/*
 * $Id: StEEmcTowerClusterFinder.h,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcTowerClusterFinder.h,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
