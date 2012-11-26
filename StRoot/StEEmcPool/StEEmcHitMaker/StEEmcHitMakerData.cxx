/*!
 * \class StEEmcHitMakerData_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See description in StEEmcHitMakerData_t.h
 *
*/

/// Includes

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"

#include "StEEmcHitMakerData.h"

/// get reference to clusers from a given layer
StSimpleClusterVec_t& StEEmcHitMakerData_t::getTowerClusterVec( TowerLayer_t layer ) {
   if( layer > NUM_TOW_LAYERS ){
      LOG_FATAL << "StEEmcHitMaker_t::getTowerClusterVec( TowerLayer_t layer ) given invalid layer: " << layer << endm;
      return mTowerClusterVec[ 0 ];
   };
   return mTowerClusterVec[ layer ];
};

void StEEmcHitMakerData_t::clearData(){
   for( Int_t i = 0; i<NUM_TOW_LAYERS; ++i )
      mTowerClusterVec[i].clear();
   mESMDClusterVec.clear();
   mHitVec.clear();

   //LOG_INFO << "SMD clusters now have " << mESMDClusterVec.size() << " clusters" << endm;
};

ClassImp( StEEmcHitMakerData_t );

/*
 * $Id: StEEmcHitMakerData.cxx,v 1.1 2012/11/26 19:05:54 sgliske Exp $ 
 * $Log: StEEmcHitMakerData.cxx,v $
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
