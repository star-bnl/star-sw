/***************************************************************************
 *
 * $Id: StFgtA2CMaker.cxx,v 1.1 2011/10/28 14:58:49 sgliske Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtA2CMaker.cxx,v $
 * Revision 1.1  2011/10/28 14:58:49  sgliske
 * replacement to StFgtCorAdcMaker
 *
 *
 **************************************************************************/

#include <string>

#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"
#include "StRoot/StFgtPedMaker/StFgtPedReader.h"
#include "StFgtA2CMaker.h"
#include "StRoot/StEvent/StEvent.h"

// constructors
StFgtA2CMaker::StFgtA2CMaker( const Char_t* name )
   : StMaker( name ),mFgtEventPtr(0), mPedReader(0),
     mTimeBinMask(0x10), mDoRemoveOtherTimeBins(0),
     mAbsThres(-10000), mRelThres(5) { /* */ };

Int_t StFgtA2CMaker::Init(){
   Int_t ierr = kStOk;

   if( mPedFile.empty() ){
      LOG_FATAL << "Cannot subtract peds--database not yet implemented" << endm;
      ierr = kStFatal;
   };

   // now the ped reader, if needed
   if( !ierr ){
      mPedReader = new StFgtPedReader( mPedFile.data() );
      mPedReader->setTimeBinMask( mTimeBinMask );
      ierr = mPedReader->Init();
   };

   return ierr;
};

Int_t StFgtA2CMaker::Make(){
   Int_t ierr = kStOk;

   StEvent* mEvent=0;
   mEvent=(StEvent*)GetInputDS("StEvent");

   mFgtEventPtr=NULL;
   if(mEvent) {
      mFgtEventPtr=mEvent->fgtEvent();
   };

   if( !mFgtEventPtr) {
      LOG_ERROR << "could not find StFgtEvent in '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   if( !ierr ){
      for( Int_t discIdx=0; discIdx<mFgtEventPtr->getNumDiscs(); ++discIdx ){
         StFgtDisc *discPtr = mFgtEventPtr->getDiscPtr( discIdx );

         if( discPtr ){
            StFgtRawHitArray &hitArray = discPtr->getRawHitArray();

            for( Int_t hitIdx = 0; hitIdx < hitArray.getEntries() && !ierr; ++hitIdx ){
               StFgtRawHit *hit = hitArray.getRawHitPtr( hitIdx );
               if( hit ){
                  if( 1<<hit->getTimeBin() & mTimeBinMask ){
                     Int_t geoId = hit->getGeoId();
                     Int_t timebin = hit->getTimeBin();
                     Int_t adc = hit->getAdc();

                     Float_t ped, err;
                     mPedReader->getPed( geoId, timebin, ped, err );

                     // subtract the pedistal
                     adc -= ped;

                     // set the value
                     hit->setAdc( adc );

                     // no DB yet, so no gains.  Default to unitary gain
                     hit->setCharge( adc );

                     // flag whether to cut
                     if( (mRelThres && adc < mRelThres*err) || (mAbsThres>-4096 && adc < mAbsThres) )
                        hit->setGeoId( -1 );

                  } else if ( mDoRemoveOtherTimeBins ){
                     // flag to cut
                     hit->setGeoId( -1 );
                  };
               };
            };

            if( mDoRemoveOtherTimeBins || mRelThres || mAbsThres>-4096 )
               hitArray.removeFlagged();
         };
      };
   };

   return ierr;
};


ClassImp(StFgtA2CMaker);
