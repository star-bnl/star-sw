/*!
 *
 * \class EEmcHitMakerSimple_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Connects the three basic tasks of hit finding, specifically
 * clustering, point finding, and energy sharing.  One can
 * independently set the methods used for each of the three tasks.
 * Conglomerate methods where the programming flow is not simply
 *
 *      cluster -> find points -> finalize energy sharing
 *
 * are children of the StEEmcHitMakerConglomerate_t class
 *
 *
 */

#ifndef _EEMC_HIT_MAKER_SIMPLE_H_
#define _EEMC_HIT_MAKER_SIMPLE_H_

/// Include StRoot headers
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

/// Include related classes
#include "StEEmcHitMaker.h"
#include "StEEmcTowerClusterFinder.h"
#include "StEEmcStripClusterFinder.h"
#include "StEEmcPointFinder.h"
#include "StEEmcEnergyApportioner.h"

class StEEmcHitMakerSimple_t : public StEEmcHitMaker_t {
 public:
   /// Construtor
   StEEmcHitMakerSimple_t( const Char_t *myName,          // name of this maker in the StMaker hash table
                           const Char_t *inputMakerName,  // name of the keeper in charge of the response
                           StEEmcTowerClusterFinder_t* towerClusterFinder,
                           StEEmcStripClusterFinder_t* stripClusterFinder,
                           StEEmcPointFinder_t *pointFinder,
                           StEEmcEnergyApportioner_t *energyApportioner );

   /// Deconstructor
   virtual ~StEEmcHitMakerSimple_t();

   /// Initialize
   Int_t Init();

   /// Find all the hits
   Int_t Make();

   /// Clear for next event
   void  Clear(Option_t *opts="");

   /// modifier
   void setClusterStripsFirst( Bool_t doItFirst = 1 ){ mClusterStripsFirst = doItFirst; };

 protected:
   /// pointers to finders for subtasks
   StEEmcTowerClusterFinder_t *mTowerClusterFinder;
   StEEmcStripClusterFinder_t *mStripClusterFinder;
   StEEmcPointFinder_t *mPointFinder;
   StEEmcEnergyApportioner_t *mEnergyApportioner;

   Int_t clusterTowers();
   Int_t clusterStrips();

   // whether to cluster towers before or after the SMD strips
   Bool_t mClusterStripsFirst;

   // to keep track of total energy per layer
   Double_t mSmdEuEvRatio[kEEmcNumSectors];

 private:
   ClassDef( StEEmcHitMakerSimple_t, 1 );
};

#endif

/*
 * $Id: StEEmcHitMakerSimple.h,v 1.2 2013/02/21 22:00:44 sgliske Exp $ 
 * $Log: StEEmcHitMakerSimple.h,v $
 * Revision 1.2  2013/02/21 22:00:44  sgliske
 * general update
 *
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
