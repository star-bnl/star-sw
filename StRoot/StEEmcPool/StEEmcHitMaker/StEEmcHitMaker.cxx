/*!
 * \class StEEmcHitMaker_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See description in StEEmcHitMaker.h
 *
*/

/// Include the header
#include "StEEmcHitMaker.h"

/// Include StRoot classes
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"
#include "StRoot/StEEmcPool/./StEEmcTreeMaker/StEEmcTreeMaker.h"
#include "StRoot/StEEmcPool/./StEEmcTreeMaker/StEEmcEnergyMaker.h"

const Int_t StEEmcHitMaker_t::kEEmcNumPhiBins = kEEmcNumSectors*kEEmcNumSubSectors;

/// Construtor
StEEmcHitMaker_t::StEEmcHitMaker_t( const Char_t *myName, const Char_t *inputMkrName )
   : StMaker( myName ),
     mInputMakerName( inputMkrName ),
     mDoClusterSMDStrips(0),
     mConstructedOK(1)  // default is OK -- child class should reset
{
   // default to doing nothing
   for( Int_t i = 0; i < NUM_TOW_LAYERS; ++i )
      mDoClusterTower[i] = 0;
};

/// deconstructor
StEEmcHitMaker_t::~StEEmcHitMaker_t(){
   // nothing to do
};

/// Initialize
Int_t StEEmcHitMaker_t::Init(){
   Int_t ierr = kStOK;

   // check if constructed OK
   if( !mConstructedOK ){
      LOG_FATAL << "StEEmcHitMaker had an error during construction phase.  Init() returning kStFatal." << endm;
      ierr = kStFatal;
   };

   TObject *inputMaker = GetMaker( mInputMakerName.data() );
   if( !inputMaker ){
      LOG_FATAL << "::Init() could not get pointer to a maker with name '" << mInputMakerName << "'" << endm;
      ierr = kStFatal;
   };

   if( !ierr && !inputMaker->InheritsFrom( "StEEmcTreeMaker_t" ) && !inputMaker->InheritsFrom( "StEEmcEnergyMaker_t" ) ){
      LOG_FATAL << "Input maker '" << mInputMakerName << "' does not inherit from "
                << "StEEmcTreeMaker_t nor StEEmcEnergyMaker_t" << endm;
      ierr = kStFatal;
   };

   return ierr;
};

/// need so that data is all loaded properly
Int_t StEEmcHitMaker_t::Make(){
   Int_t ierr = kStOk;

   TObject *inputMaker = GetMaker( mInputMakerName.data() );
   if( !inputMaker ){
      LOG_FATAL << "::Init() could not get pointer to a maker with name '" << mInputMakerName << "'" << endm;
      ierr = kStFatal;
   };

   mEEmcEnergy = 0;
   if( !ierr ){
      if( inputMaker->InheritsFrom( "StEEmcTreeMaker_t" ) ){
         StEEmcTreeMaker_t *treeMkr = static_cast< StEEmcTreeMaker_t* >( inputMaker );
         mEEmcEnergy = treeMkr->getEEmcEnergy();
      } else if ( inputMaker->InheritsFrom( "StEEmcEnergyMaker_t" ) ){
         StEEmcEnergyMaker_t *energyMkr = static_cast< StEEmcEnergyMaker_t* >( inputMaker );
         mEEmcEnergy = energyMkr->getEEmcEnergyPtr();
      } else {
         LOG_FATAL << "Input maker '" << mInputMakerName << "' does not inherit from "
                   << "StEEmcTreeMaker_t nor StEEmcEnergyMaker_t" << endm;
         ierr = kStFatal;
      };        

      if( !mEEmcEnergy ){
         LOG_ERROR << "Error getting EEmcEnergy_t pointer from '" << mInputMakerName << "'" << endm;
         ierr = kStErr;
      };
   };

   return ierr;
};

/// Clear for next event
void StEEmcHitMaker_t::Clear(Option_t *){
   clearData();
};

Bool_t StEEmcHitMaker_t::getIfClusteredTower( TowerLayer_t layer ) const{
   Bool_t didIt = 0;

   if( layer < NUM_TOW_LAYERS )
      didIt = mDoClusterTower[ layer ];

   return didIt;
};

Int_t StEEmcHitMaker_t::getNumSMDClusters(){
   Int_t n = 0;

   for( UInt_t i = 0; i<mESMDClusterVec.size(); ++i ){
      n += mESMDClusterVec[i].getClusterVecU().size();
      n += mESMDClusterVec[i].getClusterVecV().size();
   };

   return n;
};

ClassImp( StEEmcHitMaker_t );

/*
 * $Id: StEEmcHitMaker.cxx,v 1.1 2012/11/26 19:05:54 sgliske Exp $ 
 * $Log: StEEmcHitMaker.cxx,v $
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
