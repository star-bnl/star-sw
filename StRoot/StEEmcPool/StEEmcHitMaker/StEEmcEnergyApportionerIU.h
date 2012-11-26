/*!
 * \class StEEmcEnergyApportionerIU_t
 *
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Weihong/Jason's energy sharing methods: weight the surrounding
 * towers by either the sum of the SMD cluster energies, or this sum
 * times a "leakage" factore (determined by Jason's spline function.)
 *
*/

#ifndef _ST_EEMC_ENERGY_APPORTIONER_IU_H_
#define _ST_EEMC_ENERGY_APPORTIONER_IU_H_

#include "StSimpleCluster.h"
#include "StEEmcHit.h"
#include "StEEmcHitMaker.h"
#include "StEEmcTowerClusterFinder.h"
#include "StEEmcEnergyApportioner.h"
#include "StESMDClustersPerSector.h"

class StEEmcEnergyApportionerIU_t : public StEEmcEnergyApportioner_t {

 public:
   enum WeightFunction_t { SMD_SUM, SMD_SUM_AND_LEAKAGE };

   StEEmcEnergyApportionerIU_t();
   virtual ~StEEmcEnergyApportionerIU_t() { /* */ };

   virtual void clear(){ /* */ };

   /// apportion the energy
   virtual Int_t find( EEmcEnergy_t* eemcEnergyPtr,
                       const StSimpleClusterVec_t& towerClusterVec,
                       const StESMDClustersVec_t &smdClusterVec,
                       StEEmcHitVec_t& hitVec );

   // modifier
   void setWeightFunction( WeightFunction_t funcType );
   void setCheckTowerBits( Bool_t doCheck ){ mCheckTowerBits = doCheck; };

 protected:
   typedef std::map< Int_t, Float_t > sparseVec_t;

   // option of whether to include towers in the energy which have
   // fail bits or bad status bits.
   Bool_t mCheckTowerBits;

   // geom class
   EEmcGeomSimple mEEmcGeomSimple;

   /// pointer to which function to use for weights
   Float_t (StEEmcEnergyApportionerIU_t::*weightFunc)( const StEEmcHit_t &hit, Int_t thisTowerIdx );

   /// Use just sum of SMD energies
   Float_t smdSumWeightFunc( const StEEmcHit_t &hit, Int_t thisTowerIdx );

   /// Use sum of SMD energies times leakage function (spline), i.e. the eeTowerFunction
   Float_t smdSumAndLeakageWeightFunc( const StEEmcHit_t &hit, Int_t thisTowerIdx );

 private:
   ClassDef( StEEmcEnergyApportionerIU_t, 1 );

};

#endif

/*
 * $Id: StEEmcEnergyApportionerIU.h,v 1.1 2012/11/26 19:05:54 sgliske Exp $ 
 * $Log: StEEmcEnergyApportionerIU.h,v $
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
