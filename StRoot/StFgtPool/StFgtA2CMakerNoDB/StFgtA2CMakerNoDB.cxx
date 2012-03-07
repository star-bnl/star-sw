/***************************************************************************
 *
 * $Id: StFgtA2CMakerNoDB.cxx,v 1.4 2012/03/07 17:53:53 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtA2CMakerNoDB.cxx,v $
 * Revision 1.4  2012/03/07 17:53:53  sgliske
 * Added options for not removing strips to StFgtStripCollection
 *
 * Revision 1.3  2012/03/07 03:57:23  avossen
 * various updates
 *
 * Revision 1.2  2012/01/31 09:15:34  sgliske
 * includes updated since status and Ped makers moved to Pool
 *
 * Revision 1.1  2012/01/31 08:41:45  sgliske
 * split from the StFgtA2CMaker
 *
 *
 **************************************************************************/

#include <string>
#include <TH1.h>
#include <TF1.h>

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedReader.h"
#include "StRoot/StFgtPool/StFgtStatusMaker/StFgtStatusReader.h"

#include "StFgtA2CMakerNoDB.h"

// constructors
StFgtA2CMakerNoDB::StFgtA2CMakerNoDB( const Char_t* name )
   : StMaker( name ), mPedReader(0), mStatusReader(0),
     mAbsThres(-10000), mRelThres(5) {

   mPulseShapePtr = new TF1( "pulseShape", "[0]*(x>[4])*(x-[4])**[1]*exp(-[2]*(x-[4]))+[3]", 0, kFgtNumTimeBins );
   mPulseShapePtr->SetParName( 0, "C" );
   mPulseShapePtr->SetParName( 1, "a" );
   mPulseShapePtr->SetParName( 2, "b" );
   mPulseShapePtr->SetParName( 3, "ped" );
   mPulseShapePtr->SetParName( 4, "t0" );
   mPulseShapePtr->SetParameter( 0, 1000 );
   mPulseShapePtr->SetParameter( 1, 2 );
   mPulseShapePtr->SetParameter( 2, 2 );
   mPulseShapePtr->SetParameter( 3, 0 );
   mPulseShapePtr->SetParameter( 4, 0.1 );

   mHistPtr = new TH1F( (std::string( name ) + "_hist").data(), "temp hist", kFgtNumTimeBins, 0, kFgtNumTimeBins );
};


Int_t StFgtA2CMakerNoDB::Init(){
  Int_t ierr = kStOk;

  if( mPedFile.empty()){
     LOG_FATAL << "No pedestal file provided" << endm;
     ierr = kStFatal;
  };

  if( !ierr ){
     mPedReader = new StFgtPedReader( mPedFile.data() );
     mPedReader->setTimeBinMask( 0xFF );
     ierr = mPedReader->Init();
  };

  if( mStatusFile.empty() && mStatusMask ){
     LOG_FATAL << "No status file but nonzero status mask" << endm;
     ierr = kStFatal;
  };

  if( !ierr ){
     mStatusReader = new StFgtStatusReader( mStatusFile.data() );
     ierr = mStatusReader->Init();
  };

  return ierr;
};


Int_t StFgtA2CMakerNoDB::Make(){
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

   if( mStatusFile.empty() && mStatusMask != 0x0 ){
      LOG_FATAL << "No status file specified--cannot proceed" << endm;
      ierr = kStFatal;
   };

   if( mPedFile.empty() ){
      LOG_FATAL << "No pedestal file specified--cannot proceed" << endm;
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
                  Int_t nTbAboveThres = 0;

                  Int_t adc = strip->getAdc();
                  Int_t rdo, arm, apv, channel;
                  strip->getElecCoords( rdo, arm, apv, channel );
                  Int_t elecId = StFgtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel );

                  // subtract the pedestal from each time bin
                  for( Int_t timebin = 0; timebin < kFgtNumTimeBins && strip->getGeoId() > -1; ++timebin ){
                     mHistPtr->SetBinContent( timebin+1, 0 );
                     mHistPtr->SetBinError( timebin+1, 10000 );

                     // get the pedestal
                     Float_t ped = 0, pedErr = 0;
                     mPedReader->getPed( elecId, timebin, ped, pedErr );
#ifdef DEBUG
                     Int_t geoId = strip->getGeoId();
		     printf(" inp strip geoId=%d adc=%d ped=%f pedErr=%f\n",geoId,adc,ped,pedErr);
#endif

                     // subract the ped or invalidate the strip
                     if( ped > 4096 || ped < 0 ){
                        strip->setGeoId( -1 );
                     } else {
                        Int_t adcMinusPed = adc - ped;

                        strip->setAdc( adcMinusPed );
			//                        strip->setType( 1 );

                        mHistPtr->SetBinContent( timebin+1, adcMinusPed );
                        mHistPtr->SetBinError( timebin+1, pedErr );

                        if( (mRelThres && adcMinusPed > mRelThres*pedErr) || (mAbsThres>-4096 && adcMinusPed > mAbsThres) )
                           ++nTbAboveThres;
                     };
                  };

                  // check if any signal here
                  if( !nTbAboveThres && (mRelThres || mAbsThres>-4096) ){
                     strip->setGeoId( -1 );
                  } else {
                     mHistPtr->Fit( mPulseShapePtr );
                     strip->setFitParam( 
                                        mPulseShapePtr->GetParameter( 0 ),
                                        mPulseShapePtr->GetParameter( 1 ),
                                        mPulseShapePtr->GetParameter( 2 ),
                                        mPulseShapePtr->GetParameter( 3 ),
                                        mPulseShapePtr->GetParameter( 4 )   );

                     Double_t fitC = mPulseShapePtr->GetParameter( 0 );
                     Double_t errC = mPulseShapePtr->GetParError( 0 );
		     Double_t gain = 1; // no file for gains (yet)

                     strip->setCharge( gain ? fitC/gain : 0 );
                     strip->setChargeUncert( gain ? sqrt(errC*errC + adc)/gain : 10000 );

#ifdef DEBUG
                     printf("    out  adc=%d charge=%f\n",strip->getAdc(),strip->getCharge());
#endif
                     if( mStatusMask != 0x0 ){
                        UInt_t status = mStatusReader->getStatus( elecId );

                        if( status & mStatusMask )
                           strip->setGeoId( -1 );
                     };
                  };
               };
            };

            // always check if any need removed, as it is possible
            // some ``bad'' strips may have abnormally large st. dev.
            stripCollectionPtr->removeFlagged(0);
         };
      };
   };

   return ierr;
};


ClassImp(StFgtA2CMakerNoDB);
