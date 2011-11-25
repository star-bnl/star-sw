/***************************************************************************
 *
 * $Id: StFgtA2CMaker.cxx,v 1.5 2011/11/25 20:24:13 ckriley Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtA2CMaker.cxx,v $
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
#include "StFgtA2CMaker.h"


// constructors
StFgtA2CMaker::StFgtA2CMaker( const Char_t* name )
   : StMaker( name ), mPedReader(0), mStatusReader(0), checkStatus(false),
     mTimeBinMask(0x10), mDoRemoveOtherTimeBins(0),
     mAbsThres(-10000), mRelThres(5) { /* */ };

Int_t StFgtA2CMaker::Init(){
   Int_t ierr = kStOk;

   if( mPedFile.empty() ){
      LOG_FATAL << "Cannot subtract peds--database not yet implemented" << endm;
      ierr = kStFatal;
   };

   // now the ped reader and status reader, if needed
   if( !ierr ){
      mPedReader = new StFgtPedReader( mPedFile.data() );
      mPedReader->setTimeBinMask( mTimeBinMask );
      ierr = mPedReader->Init();
      if( !(mStatusFile.empty()) ){
        mStatusReader = new StFgtStatusReader( mStatusFile.data() );
        ierr = mStatusReader->Init();
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

   if( !ierr ){
      for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
         StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;

	    printf("A2C for iDsic=%d\n",discIdx);
            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
               StFgtStrip *strip = *stripIter;
               if( strip ){
                  if( 1<<strip->getTimeBin() & mTimeBinMask ){
                     Int_t geoId = strip->getGeoId();
                     Int_t timebin = strip->getTimeBin();
                     Int_t adc = strip->getAdc();

                     Float_t ped, err;
                     mPedReader->getPed( geoId, timebin, ped, err );
		     printf(" inp strip geoId=%d adc=%d ped=%f pedErr=%f\n",geoId,adc,ped,err);

                     if( ped > 4096 || ped < 0 ){
                        strip->setGeoId( -1 );
                     } else {
                        // subtract the pedistal
                        adc -= ped;

                        // set the value
                        strip->setAdc( adc );

                        // no DB yet, so no gains.  Default to unitary gain
                        strip->setCharge( adc );
                        printf("    out  adc=%d charge=%f\n",strip->getAdc(),strip->getCharge());

                        // flag whether to cut
                        if(checkStatus) {
                          Int_t status;
                          mStatusReader->getStatus( geoId, status );
                          if(!status)
                            strip->setGeoId( -1 );}

                        if( (mRelThres && adc < mRelThres*err) || (mAbsThres>-4096 && adc < mAbsThres) )
                           strip->setGeoId( -1 );
                     };
                  } else if ( mDoRemoveOtherTimeBins ){
                     // flag to cut
                     strip->setGeoId( -1 );
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
