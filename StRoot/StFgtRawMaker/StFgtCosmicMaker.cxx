#include "StFgtCosmicMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeomDefs.h"
#include "StRoot/StFgtUtil/geometry/StFgtCosmicTestStandGeom.h"
#include "RTS/src/DAQ_FGT/daq_fgt.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"

StFgtCosmicMaker::StFgtCosmicMaker( const Char_t* name, const Char_t *daqFileName ) :
   StMaker( name ), mCutShortEvents(0), mFgtCollectionPtr(0), mDaqFileName( daqFileName ), mRdr(0)
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
  StEvent* eventPtr=0;
  eventPtr= (StEvent*)GetInputDS("StEvent");

  mFgtCollectionPtr=NULL;
  if(eventPtr)
    {
      mFgtCollectionPtr=eventPtr->fgtCollection();
    }
  else
    {
      eventPtr=new StEvent();
      AddData(eventPtr);
      mFgtCollectionPtr=eventPtr->fgtCollection();
    }
  if(!mFgtCollectionPtr)
    {
      mFgtCollectionPtr=new StFgtCollection();
      eventPtr->setFgtCollection(mFgtCollectionPtr);
      LOG_DEBUG <<"::prepareEnvironment() has added a non existing StFgtCollection()"<<endm;
    }
  else
    {
      //this should be unncessary if the member clear function is called
      mFgtCollectionPtr->Clear();
    }
  return kStOK;
};

Int_t StFgtCosmicMaker::Init(){
   //LOG_INFO << "initializing" << endm;

   Int_t ierr = prepareEnvironment();

   //LOG_INFO << "event constructed" << endm;

   if( ierr || !mFgtCollectionPtr )
      {
         LOG_FATAL << "Error constructing FgtCollection" << endm;
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
  if(mFgtCollectionPtr)
    mFgtCollectionPtr->Clear();

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
      
  while(dd && dd->iterate()) 
    {
      fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

      for(u_int i=0;i<dd->ncontent;i++) 
	{
	  channel=f[i].ch;
	  adc=f[i].adc;
	  arm=dd->sec;
	  apv=dd->pad;
	  rdo=dd->rdo;
	  timebin=f[i].tb;
          Short_t discIdx=0;  // will be set with getNaivePhysCoordFromElecCoord

	  Int_t geoId = StFgtCosmicTestStandGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);

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


          StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
          if( stripCollectionPtr )
             {
                StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
                stripVec.push_back( new StFgtStrip( geoId,rdo,arm,apv,channel,adc,type,timebin) );
             }
	  else
             { LOG_WARN << "StFgtCosmicMaker::Make() -- Could not access disc " << discIdx << endm; }
	}
    }


  if( mCutShortEvents ){
     // clear events that do not have a complete set of channels
     Bool_t eventOK = 1;
     UInt_t numDiscs = mFgtCollectionPtr->getNumDiscs();

     for( UInt_t discIdx = 0; discIdx < numDiscs && eventOK; ++discIdx ){
        StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
        if( stripCollectionPtr )
           eventOK = ( stripCollectionPtr->getNumStrips() == 7*1280 );
     };
     if( !eventOK ){
        for( UInt_t discIdx = 0; discIdx < numDiscs && eventOK; ++discIdx ){
          StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
          if( stripCollectionPtr )
             stripCollectionPtr->Clear();
        };
     };
  };

  return kStOk;
};

void StFgtCosmicMaker::Clear( Option_t *opts )
{
   if( mFgtCollectionPtr )
      mFgtCollectionPtr->Clear( opts );
};


ClassImp(StFgtCosmicMaker);
