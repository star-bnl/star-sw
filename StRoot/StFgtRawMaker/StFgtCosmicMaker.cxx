#include "StFgtCosmicMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeomDefs.h"
#include "StRoot/StFgtUtil/geometry/StFgtCosmicTestStandGeom.h"
#include "RTS/src/DAQ_FGT/daq_fgt.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"

StFgtCosmicMaker::StFgtCosmicMaker( const Char_t* name, const Char_t *daqFileName ) :
   StFgtRawBase(), StMaker( name ), mCutShortEvents(0), mDaqFileName( daqFileName ), mRdr(0)
{
   //LOG_INFO << "OK?" << endm;

   // nothing else to do
};

StFgtCosmicMaker::~StFgtCosmicMaker(){
   if( mRdr )
      delete mRdr;
};


//in the cosmic maker the prepareEnvironment should only be called once (in init), so everything is constructed
Int_t StFgtCosmicMaker::prepareEnvironment()
{
 
  if( mFgtEventPtr )
      delete mFgtEventPtr;

   mFgtEventPtr = new StFgtEvent( mNumDiscs, mNumRawHits, mNumClusters, mNumPoints );
   StEvent* mEvent=0;
   //   mEvent=(StEvent*)GetInputDS("StEvent");
   //we assume that for the cosmic stand this is where the event is created
   if(!mEvent)
     {
       mEvent=new StEvent();
       AddData(mEvent);
       cout <<" attaching..." <<endl;
     }


   if(mEvent&& mFgtEventPtr)
     {
       mEvent->setFgtEvent(mFgtEventPtr);
       LOG_DEBUG<<"cosmic maker added new event and fgtEvent" <<endm;
     }
   else
     {
       LOG_ERROR<<"cosmic maker could not create events" <<endl;
     }

   return ( mFgtEventPtr ? kStOk : kStErr );
};

Int_t StFgtCosmicMaker::Init(){
   //LOG_INFO << "initializing" << endm;

   Int_t ierr = prepareEnvironment();

   //LOG_INFO << "event constructed" << endm;

   if( ierr || !mFgtEventPtr )
      {
         LOG_FATAL << "Error constructing FgtEvent" << endm;
         ierr = kStFatal;
      };

   //LOG_INFO << "constructing daqReader" << endm;

  // unfortunately, the daqReader has some constness issues to be
  // fixed.  Until they are, must remove constness of the filename.
  mRdr = new daqReader( const_cast< Char_t* >( mDaqFileName.data() ) ); 	

  //LOG_INFO << "done with init" << endm;

  return ierr;
};

//read next event from daq file and fill the fgtevent
Int_t StFgtCosmicMaker::Make()
{

  ///clear should be called separately, but in case it is not, empty the fgtevent by hand:
  if(mFgtEventPtr)
    mFgtEventPtr->Clear();

   //Short_t quadrant=0;      
   //Char_t layer=0;
   //Double_t ordinate=0;
   //Double_t lowerSpan=0;
   //Double_t upperSpan=0;
  Int_t rdo=0;
  Int_t arm=0;
  Int_t apv=0;
  Int_t channel=0;
  Short_t adc=0;
  Short_t timebin=0;

  //char *ret =
  mRdr->get(0,EVP_TYPE_ANY);
  if(mRdr->status == EVP_STAT_EOR) {
     LOG_DEBUG <<"End of File reached..."<<endm;
     return kStEOF;	
  }
  daq_dta *dd = 0;
  dd = mRdr->det("fgt")->get("adc");
      
  //LOG_INFO << "z starting event " << GetEventNumber() << ", dd at " << dd << endm;

  while(dd && dd->iterate()) 
    {
      fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

//       LOG_INFO << "y Arm, Apv, Rdo = " << arm << ' ' << apv << ' ' << rdo
//            << ", number of hits " << dd->ncontent << endm;

      for(u_int i=0;i<dd->ncontent;i++) 
	{
	  channel=f[i].ch;
	  adc=f[i].adc;
	  arm=dd->sec;
	  apv=dd->pad;
	  rdo=dd->rdo;
	  timebin=f[i].tb;
          Short_t discIdx=0;  // will be set with getNaivePhysCoordFromElecCoord

	  Short_t geoId = StFgtCosmicTestStandGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);

          Short_t quad, strip;
          Char_t layer;
          StFgtGeom::decodeGeoId( geoId, discIdx, quad, layer, strip );

          /* DEBUGGING
             Short_t quad=0;
             Short_t strip=0;
             Char_t layer=0;

             //Double_t ordinate, lowerSpan, upperSpan;
             //StFgtGeom::getNaivePhysCoordFromElecCoord(rdo,arm,apv,channel,discIdx,quad,layer,ordinate,lowerSpan,upperSpan);
             Int_t geoId2 = StFgtGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);
             StFgtGeom::decodeGeoId( geoId2, discIdx, quad, layer, strip );
             if( arm == 0 ){
             discIdx = 0;
             } else if ( apv < 12 ) {
             discIdx = 1;
             } else {
             discIdx = 2;
             apv -= 12;
             };
             geoId2 = StFgtGeom::encodeGeoId( discIdx, 0, layer, strip );

             if( geoId != geoId2 ){
             StFgtGeom::decodeGeoId( geoId, discIdx, quad, layer, strip );
             LOG_INFO << "geom: " << geoId << ' ' << discIdx << ' ' << quad << ' ' << layer << ' ' << strip << endm;

             StFgtGeom::decodeGeoId( geoId2, discIdx, quad, layer, strip );
             LOG_INFO << "hack: " << geoId2 << ' ' << discIdx << ' ' << quad << ' ' << layer << ' ' << strip << endm;
             };
          */

          Char_t type = 0;    // raw adc, no correction yet.
	  StFgtRawHit hit(geoId,adc,type,timebin);
	  hit.setCharge(adc);
	  StFgtDisc* pDisc=mFgtEventPtr->getDiscPtr(discIdx);

	  if(pDisc)
	    pDisc->getRawHitArray().pushBack(hit);
	  else
	    { LOG_WARN <<"Could not access disc "<<endm; }
	}
    }


  if( mCutShortEvents ){
     // clear events that do not have a complete set of channels
     Bool_t eventOK = 1;
     for( UInt_t i = 0; i < mNumDiscs && eventOK; ++i ){
        StFgtDisc* pDisc=mFgtEventPtr->getDiscPtr( i );
        if( pDisc )
           eventOK = ( pDisc->getNumRawHits() == 7*1280 );
     };
     if( !eventOK ){
        for( UInt_t i = 0; i < mNumDiscs; ++i ){
           StFgtDisc* pDisc=mFgtEventPtr->getDiscPtr( i );
           if( pDisc )
              pDisc->ClearRawHitArray();
        };
     };
  };

  // debug
//   for( UInt_t i = 0; i < mNumDiscs; ++i ){
//      StFgtDisc* pDisc=mFgtEventPtr->getDiscPtr( i );
//      if( pDisc )
//         LOG_INFO << "mkr: disc " << i << " nhits " << pDisc->getNumRawHits() << endm;
//   };

  return kStOk;
};

void StFgtCosmicMaker::Clear( Option_t *opts )
{
   if( mFgtEventPtr )
      mFgtEventPtr->Clear( opts );
};


ClassImp(StFgtCosmicMaker);
