/*!
 *
 * \class StEEmcHitMakerSimple_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See description in StEEmcHitMakerSimple.h
 *
*/

/// Include StRoot headers
#include "StRoot/St_base/Stypes.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"

/// Include related classes
#include "StEEmcHitMakerSimple.h"
#include "StEEmcTowerClusterFinder.h"
#include "StEEmcStripClusterFinder.h"
#include "StEEmcPointFinder.h"
#include "StEEmcEnergyApportioner.h"

StEEmcHitMakerSimple_t::StEEmcHitMakerSimple_t( const Char_t *myName,          // name of this maker
                                                const Char_t *inputMakerName,  // maker from which to get input data
                                                StEEmcTowerClusterFinder_t* towerClusterFinder,
                                                StEEmcStripClusterFinder_t* stripClusterFinder,
                                                StEEmcPointFinder_t *pointFinder,
                                                StEEmcEnergyApportioner_t *energyApportioner ) :
   StEEmcHitMaker_t(    myName, inputMakerName ),
   mTowerClusterFinder( towerClusterFinder ),
   mStripClusterFinder( stripClusterFinder ),
   mPointFinder(        pointFinder ),
   mEnergyApportioner(  energyApportioner ),
   mClusterStripsFirst( 0 ) {

   // will check all input pointers during Init()
};

StEEmcHitMakerSimple_t::~StEEmcHitMakerSimple_t(){
   /* nothing to do */
};


Int_t StEEmcHitMakerSimple_t::Init(){
   Int_t ierr = kStOK;

   // check parent
   ierr = StEEmcHitMaker_t::Init();

   if( !ierr && !mTowerClusterFinder && ( mDoClusterTower[ TOWER ] || mDoClusterTower[ PRESHOWER1 ] || mDoClusterTower[ PRESHOWER2 ] || mDoClusterTower[ POSTSHOWER ] ) ){
      LOG_FATAL << "StEEmcHitMakerSimple_t passed null pointer for TowerClusterFinder" << endm;
      ierr = kStFatal;
   };

   if( !ierr && !mStripClusterFinder && mDoClusterSMDStrips ){
      LOG_FATAL << "StEEmcHitMakerSimple_t passed null pointer for StripClusterFinder" << endm;
      ierr = kStFatal;
   };

   if( !ierr && !mPointFinder ){
      LOG_FATAL << "StEEmcHitMakerSimple_t passed null pointer for PointFinder" << endm;
      ierr = kStFatal;
   };

   if( !ierr && !mEnergyApportioner ){
      LOG_FATAL << "StEEmcHitMakerSimple_t passed null pointer for EneryApportioner" << endm;
      ierr = kStFatal;
   };

   if( ierr )
      return ierr;

   // check readiness
   if( mTowerClusterFinder && !mTowerClusterFinder->isReady() ){
      LOG_FATAL << "sub-system TowerClusterFinder is not ready" << endm;
      ierr = kStFatal;
   };

   if( mStripClusterFinder && !mStripClusterFinder->isReady() ){
      LOG_FATAL << "sub-system StripClusterFinder is not ready" << endm;
      ierr = kStFatal;
   };

   if( !mPointFinder->isReady() ){
      LOG_FATAL << "sub-system PointFinder is not ready" << endm;
      ierr = kStFatal;
   };

   if( !mEnergyApportioner->isReady() ){
      LOG_FATAL << "sub-system EnergyApportioner is not ready" << endm;
      ierr = kStFatal;
   };

   if( ierr )
      LOG_FATAL << "StEEmcHitMakerSimple_t cannot initialize" << endm;

   // initialize finer event number
   StFinderAlg_t::setEventNum( -1 );

   return ierr;
};


/// Make Hits.  It is assumed that classes called by this function
/// will display their own log messages for all errors
Int_t StEEmcHitMakerSimple_t::Make(){
   //LOG_INFO << "==========> " << ClassName() << "::Make(), Event " << GetEventNumber() << endm;

   Int_t evNum = GetEventNumber();
   if( evNum == -1 ){
      evNum = StFinderAlg_t::getEventNum();
      ++evNum;
   };
   StFinderAlg_t::setEventNum( evNum );

   // ----------> LOAD DATA <----------
   Int_t ierr = StEEmcHitMaker_t::Make();

   if( !ierr ){
      // ----------> CLUSTERING <----------

      //LOG_INFO << "----------> Clustering <----------" << endm;

      if( mClusterStripsFirst ){
         if( !ierr && mDoClusterSMDStrips )
            ierr = clusterStrips();

         if( !ierr )
            ierr = clusterTowers();
      } else {
         if( !ierr )
            ierr = clusterTowers();

         if( !ierr && mDoClusterSMDStrips )
            ierr = clusterStrips();
      };

      // ----------> POINT FINDING <----------

      //LOG_INFO << "----------> Point Finding <----------" << endm;

      if( !ierr )
         ierr = mPointFinder->find( *mEEmcEnergy, mTowerClusterVec[ TOWER ], mESMDClusterVec, mSmdEuEvRatio, mHitVec );

      //LOG_INFO << "-----> Found " << mHitVec.size() << " points" << endm;

      // ----------> ENERGY APPORTIONING (SHARING) <----------

      //LOG_INFO << "----------> Energy Apportioning <----------" << endm;

      if( !ierr )
         mEnergyApportioner->find( mEEmcEnergy, mTowerClusterVec[ TOWER ], mESMDClusterVec, mHitVec );


      // ----------> OUTPUT <----------

      if( ierr ){
         LOG_WARN << "StEEmcHitMakerSimple_t::find() returning error code of '" << ierr << "'" << endm;
      };
   };

   //static_cast< StEEmcHitMaker_t* >( this )->getHitVecSize();
   //LOG_INFO << "==========> Make done, mHitVec.size() = " << mHitVec.size() << endm;

   return ierr;
};

/// clear to prepare for next event
void StEEmcHitMakerSimple_t::Clear( Option_t* opts ){

   // Clear all classes.  Clear regardless of whether clustering, just
   // in case someone changes the "do cluster" options
   if( mTowerClusterFinder )
      mTowerClusterFinder->clear();
   if( mStripClusterFinder )
      mStripClusterFinder->clear();
   if( mPointFinder )
      mPointFinder->clear();
   if( mEnergyApportioner )
      mEnergyApportioner->clear();

   // clear the parent
   StEEmcHitMaker_t::Clear( opts );
};

Int_t StEEmcHitMakerSimple_t::clusterTowers(){
   Int_t ierr = kStOK;

   if( !ierr && mDoClusterTower[ TOWER ] )
      ierr = mTowerClusterFinder->find( mEEmcEnergy->eTow, mTowerClusterVec[ TOWER ] );

   if( !ierr && mDoClusterTower[ PRESHOWER1 ] )
      ierr = mTowerClusterFinder->find( mEEmcEnergy->ePre1, mTowerClusterVec[ PRESHOWER1 ] );

   if( !ierr && mDoClusterTower[ PRESHOWER2 ] )
      ierr = mTowerClusterFinder->find( mEEmcEnergy->ePre2, mTowerClusterVec[ PRESHOWER2 ] );

   if( !ierr && mDoClusterTower[ POSTSHOWER ] )
      ierr = mTowerClusterFinder->find( mEEmcEnergy->ePost, mTowerClusterVec[ POSTSHOWER ] );

   return ierr;
};

Int_t StEEmcHitMakerSimple_t::clusterStrips(){
   Int_t ierr = kStOK;

   mStripClusterFinder->resetClusterCount();
   for( Int_t sector = 0; sector < kEEmcNumSectors && !ierr; ++sector ){

      //LOG_INFO << "sector = " << sector << ", hit strips = " << mHitUStripPtr[ sector ]->size() << ", " << mHitVStripPtr[ sector ]->size() << endm;

      // set current sector
      mStripClusterFinder->setCurrentSector( sector );

      ESmdEnergy_t& eSMD = mEEmcEnergy->eSmd;

      mSmdEuEvRatio[sector] = 1;

      // make sure there are at least some hit strips in this sector
      if( mEEmcEnergy->nStrips ){
         // add a new element for clusters from this sector
         mESMDClusterVec.push_back( StESMDClustersPerSector_t( sector ) );

         // get references, to make code easier to read
         StSimpleClusterVec_t &uClusVec = mESMDClusterVec.back().getClusterVecU();
         StSimpleClusterVec_t &vClusVec = mESMDClusterVec.back().getClusterVecV();

         // U layer
         mStripClusterFinder->setCurrentLayer( StEEmcStripClusterFinder_t::U_LAYER );
         ierr = mStripClusterFinder->find( eSMD.sec[sector].layer[0], uClusVec );

         //LOG_INFO << "-----> found " << uClusVec.size() << " u clusters." << endm;

         // V layer
         mStripClusterFinder->clear();
         mStripClusterFinder->setCurrentLayer( StEEmcStripClusterFinder_t::V_LAYER );
         ierr = mStripClusterFinder->find( eSMD.sec[sector].layer[1], vClusVec );

         //LOG_INFO << "-----> found " << vClusVec.size() << " v clusters." << endm;
         //LOG_INFO << "zzz\tSec " << sector << ": uSMD clus = " << uClusVec.size() << ", vSMD clus = " << vClusVec.size() << endm;

         if( uClusVec.empty() && vClusVec.empty() ){
            // delete the last element if there were no clusters found in either layer
            mESMDClusterVec.pop_back();
         } else {

            Double_t E[2] = { 0, 0 };

            // compute total energy in each layer
            for( Int_t layer = 0; layer < 2; ++layer ){
               ESmdLayer_t &smdLayer = eSMD.sec[sector].layer[layer];

               for( Int_t strip=0; strip<288; ++strip ){
                  EEmcElement_t& elem = smdLayer.strip[strip];

                  if( !elem.fail )
                     E[layer] += elem.energy;
               };
            };

            mSmdEuEvRatio[sector] = ( E[1] != 0 ? E[0] / E[1] : 1e50 );
         };
      };
   };

   return ierr;
};

ClassImp( StEEmcHitMakerSimple_t );

/*
 * $Id: StEEmcHitMakerSimple.cxx,v 1.2 2013/02/21 22:00:44 sgliske Exp $ 
 * $Log: StEEmcHitMakerSimple.cxx,v $
 * Revision 1.2  2013/02/21 22:00:44  sgliske
 * general update
 *
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
