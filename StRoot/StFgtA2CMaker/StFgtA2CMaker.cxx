/***************************************************************************
 *
 * $Id: StFgtA2CMaker.cxx,v 1.13 2012/01/30 10:42:22 sgliske Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtA2CMaker.cxx,v $
 * Revision 1.13  2012/01/30 10:42:22  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.12  2012/01/28 20:10:12  avossen
 * addec cluster uncertainty
 *
 * Revision 1.11  2012/01/28 11:22:53  sgliske
 * changed status check to status map
 * changed setDb to setFgtDb
 * cleaned up few other minor things
 *
 * Revision 1.10  2012/01/27 13:38:29  sgliske
 * updated to correspond with new Status/Ped readers,
 * Now keyed by elecId
 *
 * Revision 1.9  2012/01/24 06:52:45  sgliske
 * made status cuts optional
 * and updated status to a fail condition--
 * i.e. status == 0x0 is good, otherwise is bad.
 * WARNING--this may be different than that used at first
 * in for the cosmic test stand.
 *
 * Revision 1.8  2012/01/24 05:52:13  sgliske
 * Surrounded printf's with #ifdef,
 * cleaned up a little whitespace,
 * added strip->SetType( 1 );
 *
 * Revision 1.7  2012/01/04 20:23:02  sgliske
 * fixed spelling of iDsic to iDisc
 *
 * Revision 1.6 2011/12/01 00:13:23 avossen
 * included use of db. Note: For DB use it hast to be set with setDb.
 * Instantiate StFgtDBMaker, get the StFgtDb from the getTables method
 * and give the pointer to the A2C maker
 *
 * Revision 1.5  2011/11/25 20:24:13  ckriley
 * added statusmaker functionality
 *
 * Revision 1.4  2011/11/17 18:40:40  sgliske
 * Bug fixed: need to always call stripCollectionPtr->removeFlagged();
 * Also implemented check to invalidate strip if weird ped. value
 *
 * Revision 1.3  2011/11/04 17:01:06  balewski
 * added printouts
 *
 * Revision 1.2  2011/11/01 18:46:14  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.1  2011/10/28 14:58:49  sgliske
 * replacement to StFgtCorAdcMaker
 *
 *
 **************************************************************************/

#include <string>

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StFgtPedMaker/StFgtPedReader.h"
#include "StRoot/StFgtStatusMaker/StFgtStatusReader.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"
#include "StFgtA2CMaker.h"


// constructors
StFgtA2CMaker::StFgtA2CMaker( const Char_t* name )
   : StMaker( name ), mPedReader(0), mStatusReader(0), useStatusFile(false),
     mAbsThres(-10000), mRelThres(5), usePedFile(false), mDb(0) { /* */ };


Int_t StFgtA2CMaker::Init(){
  Int_t ierr = kStOk;
  if( usePedFile ) {
      if( mPedFile.empty()){
	LOG_FATAL << "no ped file but told to use" << endm;
	ierr = kStFatal;
      };
      if( !ierr ){
	mPedReader = new StFgtPedReader( mPedFile.data() );
	mPedReader->setTimeBinMask( 0xFF );
	ierr = mPedReader->Init();
      }
    } else {
     if(!mDb) {
	LOG_FATAL << "No DB nor pedestal file specified--cannot proceed" << endm;
	ierr = kStFatal;
      }
  }
  if( useStatusFile ) {
      if( mStatusFile.empty()){
	LOG_FATAL << "no status file but told to use" << endm;
	ierr = kStFatal;
      };
      if( !ierr ){
	mStatusReader = new StFgtStatusReader( mStatusFile.data() );
	ierr = mStatusReader->Init();
      }
    } else {
     if(!mDb) {
	LOG_FATAL << "No DB nor status file specified--cannot proceed" << endm;
	ierr = kStFatal;
      }
  };

  return ierr;
};


Int_t StFgtA2CMaker::Make(){
   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   StFgtCollection* fgtCollectionPtr = 0;

   if( eventPtr ) {
      fgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !fgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   if( !mDb && mStatusFile.empty() && mStatusMask != 0x0 ){
	LOG_FATAL << "No DB nor status file specified--cannot proceed" << endm;
	ierr = kStFatal;
   };

   if( !mDb && mPedFile.empty() ){
	LOG_FATAL << "No DB nor pedestal file specified--cannot proceed" << endm;
	ierr = kStFatal;
   };

   if( !ierr ){
      for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
         StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;

#ifdef DEBUG
	    printf("A2C for iDisc=%d\n",discIdx);
#endif
            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
               StFgtStrip *strip = *stripIter;
               if( strip ){
                  Int_t totalADC = 0;
                  Int_t pedErrSumSq = 0, nPed = 0;
                  Int_t nTbAboveThres = 0;

                  Int_t geoId = strip->getGeoId();
                  // switch geoId to elec id lookups, as soon as available
                  // also clean up later computations of elecId at the same time

                  // subtract the pedestal from each time bin
                  for( Int_t timebin = 0; timebin < kFgtNumTimeBins && strip->getGeoId() > -1; ++timebin ){

                     // get the pedestal
                     Float_t ped = 0, err = 0;
		     if(usePedFile){
                        Int_t rdo, arm, apv, channel;
                        strip->getElecCoords( rdo, arm, apv, channel );
                        Int_t elecId = StFgtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel );
                        mPedReader->getPed( elecId, timebin, ped, err );
		     } else if( mDb ){
                        ped = mDb->getPedestalFromGeoId( geoId );
                        err = mDb->getPedestalSigmaFromGeoId( geoId );
                     };

#ifdef DEBUG
		     printf(" inp strip geoId=%d adc=%d ped=%f pedErr=%f\n",geoId,adc,ped,err);
#endif
                     // subract the ped or invalidate the strip
                     if( ped > 4096 || ped < 0 ){
                        strip->setGeoId( -1 );
                     } else {
                        Int_t adcMinusPed = strip->getAdc() - ped;

                        strip->setAdc( adcMinusPed );
                        strip->setType( 1 );

                        totalADC += adcMinusPed;
                        pedErrSumSq += err*err;
                        ++nPed;

                        if( (mRelThres && adcMinusPed > mRelThres*err) || (mAbsThres>-4096 && adcMinusPed > mAbsThres) )
                           ++nTbAboveThres;
                     };
                  };

                  // check if any signal here
                  if( !nTbAboveThres && (mRelThres || mAbsThres>-4096) ){
                     strip->setGeoId( -1 );
                  } else {
                     // there's a signal here, or we aren't cutting on signals.

		     Double_t gain = mDb ? mDb->getGainFromGeoId(geoId) : 1;

                     strip->setCharge( gain*totalADC );  // later change totalADC to a fit parameter
                     strip->setChargeUncert( nPed ?
                                             gain*sqrt( pedErrSumSq )/nPed :
                                             10000 );

#ifdef DEBUG
                     printf("    out  adc=%d charge=%f\n",strip->getAdc(),strip->getCharge());
#endif
                     if( mStatusMask != 0x0 ){
                        UInt_t status = 0x0;  // assume strip is good
                        if( useStatusFile ){
                           Int_t rdo, arm, apv, channel;
                           Int_t elecId = StFgtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel );
                        
                           status = mStatusReader->getStatus( elecId );
                        } else if( mDb ){
                           status=mDb->getStatusFromGeoId(geoId);
                        };

                        if( status & mStatusMask )
                           strip->setGeoId( -1 );
                     };
                  };
               };
            };

            // always check if any need removed, as it is possible
            // some ``bad'' strips may have abnormally large st. dev.
            stripCollectionPtr->removeFlagged();
         };
      };
   };

   return ierr;
};


ClassImp(StFgtA2CMaker);
