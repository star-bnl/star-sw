/***************************************************************************
 *
 * $Id: StEEmcRawMapMaker.cxx,v 1.3 2012/05/09 21:11:58 sgliske Exp $
 * Author: S. Gliske, April 2012
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StEEmcRawMapMaker.cxx,v $
 * Revision 1.3  2012/05/09 21:11:58  sgliske
 * updates
 *
 * Revision 1.2  2012/04/13 15:08:43  sgliske
 * updates
 *
 * Revision 1.1  2012/04/12 17:11:16  sgliske
 * creation
 *
 *
 **************************************************************************/

#include <map>

#include "StMaker.h"
#include "StEEmcRawMapMaker.h"

#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StEvent/StEvent.h"
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcModule.h"
#include "StEvent/StEmcRawHit.h"
#include "StEvent/StEmcCollection.h"

#include "StMessMgr.h"

#define TRIG
//#define DEBUG

#ifdef TRIG
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"
#endif

// constructors
StEEmcRawMapMaker::StEEmcRawMapMaker( const Char_t* name ) : StMaker( name ), mInputType(-1), mInputName(""), mEEmcDb(0) { /* */ };

// deconstructor
StEEmcRawMapMaker::~StEEmcRawMapMaker(){ /* */ };

Int_t StEEmcRawMapMaker::setInput( const Char_t *name, Int_t type ){
   Int_t ierr = kStOk;

   if( type == 0 || type == 1){
      mInputType = type;
      mInputName = name;
   } else {
      LOG_ERROR << "Invalid input type" << endm;
      ierr = kStFatal;
   };
   return ierr;
};

Int_t StEEmcRawMapMaker::Init(){
   Int_t ierr = kStOk;

   if( mInputType != 0 && mInputType != 1){
      LOG_ERROR << "Input mInputType not set to valid value" << endm;
      ierr = kStFatal;
   };

   return ierr;
};

Int_t StEEmcRawMapMaker::Make(){
   StEEmcRawMapMaker::Clear();

   Int_t ierr = kStOk;

   mEEmcDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
   if( !mEEmcDb ){
      LOG_ERROR << "Error finding EEMC DB" << endm;
      this->ls();
      ierr = kStFatal;
   };

   if( !ierr ){
      if( mInputType ){
         ierr = loadFromMuDst();
      } else {
         ierr = loadFromStEvent();
      };
   };

   return ierr;
};

Int_t StEEmcRawMapMaker::loadFromMuDst(){
   Int_t ierr = kStOk;

   const StMuDst* muDst = (const StMuDst*)GetInputDS( mInputName.data() );
   const StMuEmcCollection* emc = 0;
   if( muDst )
      emc = muDst->muEmcCollection();

   if( !emc ){
      ierr = kStFatal;
      LOG_ERROR << "Error finding MuEmc Collection from MuDst" << endm;
   };

   if( !ierr ){

#ifdef DEBUG2
      LOG_INFO << "Number of towers in MuDst " << emc->getNEndcapTowerADC() << endm;
#endif

      /// Loop over all towers
      for ( Int_t i = 0; i < emc->getNEndcapTowerADC(); ++i ){
         Int_t adc, sec, sub, eta;
         emc->getEndcapTowerADC( i, adc, sec, sub, eta );

         // need to switch from indexing from 1 to indexing from 0
         sec--;
         sub--;
         eta--;

         if( (sec >= 0) && (sec < kEEmcNumSectors ) && (sub >= 0) && (sub < kEEmcNumSubSectors ) && (eta >= 0) && (eta < kEEmcNumEtas) )
            addHitTower(sec,sub,eta,adc,0);
      };

      /// Loop over all pre/postshower elements
      for ( Int_t i = 0; i < emc->getNEndcapPrsHits(); i++ ){

         Int_t adc, sec, sub, eta, det;
         const StMuEmcHit *hit = emc->getEndcapPrsHit(i, sec, sub, eta, det);
         if ( hit ){
            adc = hit -> getAdc();

            // need to switch from indexing from 1 to indexing from 0
            sec--;
            sub--;
            eta--;

            if( (sec >= 0) && (sec < kEEmcNumSectors ) && (sub >= 0) && (sub < kEEmcNumSubSectors ) && (eta >= 0) && (eta < kEEmcNumEtas) )
               addHitTower(sec,sub,eta,adc,det);

         };
      };
   };

   //
   // LOAD SMD DATA
   //
   if( !ierr ){
      Char_t cpl[] = { 'U','V' };
      for ( Int_t plane = 0; plane < 2; plane++ ){
         for ( Int_t ihit = 0; ihit < emc->getNEndcapSmdHits(cpl[plane]); ihit++ ) {
            Int_t sec, strip, adc;
            const StMuEmcHit *hit = emc->getEndcapSmdHit( cpl[plane], ihit, sec, strip );
            adc = hit->getAdc();

            sec--;   // adjust indexing
            strip--; // 

            if ((sec >= 0) && (sec < 12) && (strip >= 0) && (strip < 288))
               addHitStrip(sec,plane,strip,adc);
         };
      };
   };


#ifdef TRIG
   StMuEvent *event = muDst->event();
   if( event ){
      const StTriggerId& l1trig = event->triggerIdCollection().l1();
      cout << "Passed trigger? " << l1trig.isTrigger( 380301 ) << ' ' << l1trig.isTrigger( 380302 ) << endl;
   };
#endif

#ifdef DEBUG
   LOG_INFO << "Number of towers in MuDst " << emc->getNEndcapTowerADC() << " vs in raw Map " << mMap[0].size() << endm;
#endif

   return ierr;
};

Int_t StEEmcRawMapMaker::loadFromStEvent(){
   Int_t ierr = kStOk;

   const StEvent *event = (const StEvent*)GetInputDS( mInputName.data() );
   const StEmcCollection *emc = 0;

   if( event )
      emc = event->emcCollection();

   if( !emc ){
      LOG_ERROR << "Error reading from StEvent" << endm;
      ierr = kStFatal;
   };

   const StEmcDetector *detector = 0;
   StDetectorId detectorIds[] = { kEndcapEmcTowerId, kEndcapEmcPreShowerId };

   //
   // TOWERS, PRESHOWERS and POSTSHOWER
   //

   if( !ierr ){
      for( Int_t iDet = 0; iDet < 2; ++iDet ){
         detector = emc->detector( detectorIds[iDet] );

         if ( !detector ){
            LOG_ERROR << "Error finding EEMC detector in EEmc collection" << endm;
            ierr = kStFatal;
         } else {
            // loop over towers
            for ( UInt_t sec = 0; sec < detector->numberOfModules(); sec++ ){

               /// Comment from StEEmcA2EMaker: "Remember to watch out for
               /// fortran holdovers in the numbering scheme"
               const StEmcModule *sector = detector->module( sec+1 );
               if( sector ){
                  const StSPtrVecEmcRawHit &hits = sector->hits();

                  /// Loop over all raw hits in this sector
                  for ( UInt_t ihit=0; ihit<hits.size(); ihit++ ){
                     if (!hits[ihit]) {

                        /// Switch from indexing from 1 to indexing from 0
                        Int_t sec = hits[ihit]->module() - 1;
                        Int_t sub = hits[ihit]->sub() - 1;
                        Int_t eta = hits[ihit]->eta() - 1;
                        Int_t adc = hits[ihit]->adc();

                        Int_t layer = (hits[ihit]->sub() - 1) / 5 + 1;
                        if( (layer >= 0) && (layer < 4) && (sec >= 0) && (sec < kEEmcNumSectors) && (sub >= 0) && (sub < kEEmcNumSubSectors ) && (eta >= 0) && (eta < kEEmcNumEtas) )
                           addHitTower(sec,sub,eta,adc,layer);
                     };
                  };
               };
            };
         };
      };
   };

   //
   // SMD STRIPS
   //

   if( !ierr ){
      StDetectorId ids[] = { kEndcapSmdUStripId, kEndcapSmdVStripId };
      /// Loop over U & V planes
      for( Int_t iplane=0; iplane<2; iplane++ ){

         /// Get the eemc smd collections
         detector=emc->detector( ids[iplane] );
         assert( detector );

         for( UInt_t sec = 0; sec<detector->numberOfModules(); sec++ ){
            /// Remember to watch out for fortran holdovers in
            /// the numbering scheme
            const StEmcModule *sector = detector->module( sec+1 );
            const StSPtrVecEmcRawHit &hits = sector->hits();
                    
            /// Loop over all raw hits in this sector
            for ( UInt_t ihit=0; ihit<hits.size(); ihit++ ){
               Int_t isector = hits[ihit]->module()-1;
               Int_t istrip  = hits[ihit]->eta()-1;
               Int_t adc     = hits[ihit]->adc();
               if( (isector >= 0) && (isector < 12) && (istrip >= 0) && (istrip < 288) )
                  addHitStrip( isector, iplane, istrip, adc);
            };
         };
      };
   };

   return ierr;
};

void StEEmcRawMapMaker::addHitTower( Int_t sec, Int_t sub, Int_t eta, Int_t adc, Int_t layer ){

   Int_t index = eta + kEEmcNumEtas*( sub + kEEmcNumSubSectors*sec );

// #ifdef DEBUG2
//    if( GetEventNumber() == 868734 && index == 336 ){
//       LOG_INFO << "Event " << GetEventNumber() << " adding tower " << index << ' ' << layer << ' ' << adc << endm;
//    };
// #endif

   StEEmcRawMapData& data = mMap[layer][index];
   data.rawAdc = adc;

   assert( mEEmcDb );
   static const Char_t subsectors[] = { 'A','B','C','D','E' };
   static const Char_t detectors[] = { 'T', 'P', 'Q', 'R' };
   const EEmcDbItem *dbitem = mEEmcDb->getTile( sec+1,subsectors[sub],eta+1, detectors[layer] );
   assert( dbitem );

   data.fail = dbitem->fail;
   data.stat = dbitem->stat;
   data.ped = dbitem->ped;
   data.pedSigma = dbitem->sigPed;
   data.gain = dbitem->gain;
};

void StEEmcRawMapMaker::addHitStrip( Int_t sec, Bool_t layerIsV, Int_t strip, Int_t adc ){

   Int_t index = strip + kEEmcNumStrips*( layerIsV + kEEmcNumSmdUVs*sec );

   StEEmcRawMapData& data = mMap[ESMD][index];
   data.rawAdc = adc;

   assert( mEEmcDb );
   const EEmcDbItem *dbitem = mEEmcDb->getByStrip ( sec+1, (layerIsV ? 'V' : 'U'), strip+1 );
   assert( dbitem );

   data.fail = dbitem->fail;
   data.stat = dbitem->stat;
   data.ped = dbitem->ped;
   data.pedSigma = dbitem->sigPed;
   data.gain = dbitem->gain;
};

void StEEmcRawMapMaker::Clear( Option_t *opt ){
   mMap[0].clear();
   mMap[1].clear();
   mMap[2].clear();
   mMap[3].clear();
};

ClassImp(StEEmcRawMapMaker);
