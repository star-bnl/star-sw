/*!
 * \class StEEmcEnergyApportioner_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Abstract base class for various energy apportioning (sharing) algorithms.
*/

#ifndef _ST_EEMC_ENERGY_APPORTIONER_H_
#define _ST_EEMC_ENERGY_APPORTIONER_H_

#include <Rtypes.h>

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "StFinderAlg.h"
#include "StSimpleCluster.h"
#include "StEEmcHit.h"
#include "StESMDClustersPerSector.h"

class EEmcEnergy_t;

class StEEmcEnergyApportioner_t : public StFinderAlg_t {

 public:
   StEEmcEnergyApportioner_t() : StFinderAlg_t() { /* */ };
   virtual ~StEEmcEnergyApportioner_t() { /* */ };

   /// clear things before doing the finding for the next event
   virtual void clear() = 0;

   /// find some some points
   virtual Int_t find( EEmcEnergy_t* eemcEnergyPtr,
                       const StSimpleClusterVec_t &towerClusterVec,
                       const StESMDClustersVec_t &smdClusterVec,
                       StEEmcHitVec_t& hitVec ) = 0;

 protected:

 private:
   ClassDef( StEEmcEnergyApportioner_t, 1 );
};

#endif

/*
 * $Id: StEEmcEnergyApportioner.h,v 1.1 2012/11/26 19:05:54 sgliske Exp $ 
 * $Log: StEEmcEnergyApportioner.h,v $
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
