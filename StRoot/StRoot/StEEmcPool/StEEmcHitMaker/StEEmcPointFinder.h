/*!
 * \class StEEmcStEEmcPointFinder_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Abstract base class for various point finding algorithms.
*/

#ifndef _ST_POINT_FINDER_H_
#define _ST_POINT_FINDER_H_

#include "StFinderAlg.h"
#include "StSimpleCluster.h"
#include "StEEmcHit.h"
#include "StESMDClustersPerSector.h"

class EEmcEnergy_t;

class StEEmcPointFinder_t : public StFinderAlg_t {

 public:
   StEEmcPointFinder_t() : StFinderAlg_t() { /* */ };
   virtual ~StEEmcPointFinder_t() { /* */ };

   /// clear things before doing the finding for the next event
   virtual void clear() = 0;

   /// find some some points
   virtual Int_t find( const EEmcEnergy_t& eemcEnergy,
                       const StSimpleClusterVec_t &towerClusterVec,
                       const StESMDClustersVec_t &smdClusterVec,
                       const Double_t* smdEuEvRatio,
                       StEEmcHitVec_t& hitVec ) = 0;
 private:
   ClassDef( StEEmcPointFinder_t, 1 );

};

#endif

/*
 * $Id: StEEmcPointFinder.h,v 1.2 2013/02/21 22:00:44 sgliske Exp $ 
 * $Log: StEEmcPointFinder.h,v $
 * Revision 1.2  2013/02/21 22:00:44  sgliske
 * general update
 *
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
