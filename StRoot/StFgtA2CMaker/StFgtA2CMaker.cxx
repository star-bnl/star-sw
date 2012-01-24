/***************************************************************************
 *
 * $Id: StFgtA2CMaker.cxx,v 1.9 2012/01/24 06:52:45 sgliske Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtA2CMaker.cxx,v $
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
 * Revision 1.6  2011/12/01 00:13:23  avossen
 * included use of db. Note: For DB use it hast to be set with setDb. Instantiate StFgtDBMaker, get the StFgtDb from the getTables method and give the pointer to the A2C maker
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
#include "StFgtA2CMaker.h"


// constructors
StFgtA2CMaker::StFgtA2CMaker( const Char_t* name )
   : StMaker( name ), mPedReader(0), mStatusReader(0), useStatusFile(false),
     mTimeBinMask(0x10), mDoRemoveOtherTimeBins(0),
     mAbsThres(-10000), mRelThres(5), usePedFile(false), mDb(0) { /* */ };


Int_t StFgtA2CMaker::Init(){
  Int_t ierr = kStOk;
  if(usePedFile)
    {
      if( mPedFile.empty()){
	LOG_FATAL << "no ped file but told to use" << endm;
	ierr = kStFatal;
      };
      if( !ierr ){
	mPedReader = new StFgtPedReader( mPedFile.data() );
	mPedReader->setTimeBinMask( mTimeBinMask );
	ierr = mPedReader->Init();
      }
    }
  else{
    if(!mDb)
      {
	LOG_FATAL << "cannot find db that I am supposed to use!" << endm;
	ierr = kStFatal;
      }
  }
  if( useStatusFile && !(mStatusFile.empty()) ){
	mStatusReader = new StFgtStatusReader( mStatusFile.data() );
	ierr = mStatusReader->Init();
  }
  
  // now the ped reader and status reader, if needed
  
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

#ifdef DEBUG
	    printf("A2C for iDisc=%d\n",discIdx);
#endif
            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
               StFgtStrip *strip = *stripIter;
               if( strip ){
                  if( 1<<strip->getTimeBin() & mTimeBinMask ){
                     Int_t geoId = strip->getGeoId();
                     Int_t timebin = strip->getTimeBin();
                     Int_t adc = strip->getAdc();
		     Double_t gain= mDb ? mDb->getGainFromGeoId(geoId) : 1;

                     Float_t ped, err;
		     if(usePedFile)
		       mPedReader->getPed( geoId, timebin, ped, err );
		     else
		       {
			 ped=mDb->getPedestalFromGeoId(geoId);
			 err=mDb->getPedestalSigmaFromGeoId(geoId);
		       }
#ifdef DEBUG
		     printf(" inp strip geoId=%d adc=%d ped=%f pedErr=%f\n",geoId,adc,ped,err);
#endif

                     if( ped > 4096 || ped < 0 ){
                        strip->setGeoId( -1 );
                     } else {
                        // subtract the pedistal
                        adc -= ped;

                        // set the values
                        strip->setAdc( adc );
                        strip->setType( 1 );
                        strip->setCharge( gain*adc );
#ifdef DEBUG
                        printf("    out  adc=%d charge=%f\n",strip->getAdc(),strip->getCharge());
#endif
                        if( mCutBadStatus ){
                           UInt_t status=0;
                           if(useStatusFile)
                              mStatusReader->getStatus( geoId, status );
                           if(!useStatusFile && mDb)
                              status=mDb->getStatusFromGeoId(geoId);
                           if(status)
                              strip->setGeoId( -1 );
                        };
		     
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
