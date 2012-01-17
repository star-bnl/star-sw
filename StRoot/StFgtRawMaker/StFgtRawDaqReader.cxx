/*
 *
 * \class StFgtRawMaker
 * \author S. Gliske (sgliske@anl.gov) based on StFgtComsicReader,
 * v1.15 written by A. Vossen (avossen@indiana.edu)
 *
 * See header for description.
 *
 */

#include "StFgtRawDaqReader.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeomDefs.h"
#include "StRoot/StFgtUtil/geometry/StFgtCosmicTestStandGeom.h"
#include "StRoot/St_base/StMessMgr.h"
#include "RTS/src/DAQ_FGT/daq_fgt.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "RTS/src/DAQ_READER/daqReader.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
//#include "StRoot/St_db_Maker/St_db_Maker.h"

#include <string.h>

StFgtRawDaqReader::StFgtRawDaqReader( const Char_t* name, const Char_t *daqFileName, const Char_t* dbMkrName ) :
   StMaker( name ), mCutShortEvents(0), mIsCosmic(0), mFgtCollectionPtr(0), mDaqFileName( daqFileName ), mDbMkrName( dbMkrName ), mRdr(0), mFgtDbMkr(0) {

   // set to being cosmic if filename ends in .sfs
   // otherwise, assume is not
   std::string daqFileNameS( daqFileName );
   std::string::size_type pos = daqFileNameS.find_last_of(".");

   if( pos != std::string::npos && daqFileNameS.substr( pos ) == ".sfs" )
      mIsCosmic = 1;
};

StFgtRawDaqReader::~StFgtRawDaqReader(){
   if( mRdr )
      delete mRdr;
};


//in the cosmic maker the prepareEnvironment should only be called once (in init), so everything is constructed
Int_t StFgtRawDaqReader::prepareEnvironment(){
   StEvent* eventPtr=0;
   eventPtr= (StEvent*)GetInputDS("StEvent");

   mFgtCollectionPtr=NULL;
   if(eventPtr) {
      mFgtCollectionPtr=eventPtr->fgtCollection();
   } else {
      eventPtr=new StEvent();
      AddData(eventPtr);
      mFgtCollectionPtr=eventPtr->fgtCollection();
   };
   if(!mFgtCollectionPtr) {
      mFgtCollectionPtr=new StFgtCollection();
      eventPtr->setFgtCollection(mFgtCollectionPtr);
      LOG_DEBUG <<"::prepareEnvironment() has added a non existing StFgtCollection()"<<endm;
   } else {
      //this should be unncessary if the member clear function is called
      mFgtCollectionPtr->Clear();
   };
   return kStOK;
};

Int_t StFgtRawDaqReader::Init(){
   //LOG_INFO << "initializing" << endm;

   GetEvtHddr()->SetEventNumber(1);

   Int_t ierr = prepareEnvironment();

   //LOG_INFO << "event constructed" << endm;

   if( ierr || !mFgtCollectionPtr ) {
      LOG_FATAL << "Error constructing FgtCollection" << endm;
      ierr = kStFatal;
   };

   if( !ierr && !mIsCosmic ){
      // get the maker pointer
      mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMaker( mDbMkrName.data() ) );

       if( !ierr && !mFgtDbMkr ){
          LOG_FATAL << "Error finding FgtDbMkr" << endm;
          ierr = kStFatal;
       };

      if( !mFgtDbMkr->InheritsFrom("StFgtDbMaker") ){
         LOG_FATAL << "StFgtDbMkr does not inherit from StFgtDbMaker" << endm;
         LOG_FATAL << "Name is '" << mFgtDbMkr->GetName() << "', class is '" << mFgtDbMkr->ClassName() << endm;
         ierr = kStFatal;
      };

      LOG_INFO << "Using date and time " << mFgtDbMkr->GetDateTime().GetDate() << ", " << mFgtDbMkr->GetDateTime().GetTime() << endm;
   };

   if( !ierr ){
      //LOG_INFO << "constructing daqReader" << endm;

      // unfortunately, the daqReader has some constness issues to be
      // fixed.  Until they are, must remove constness of the filename.
      mRdr = new daqReader( const_cast< Char_t* >( mDaqFileName.data() ) ); 	
   };

   //LOG_INFO << "done with init" << endm;

  return ierr;
};

//read next event from daq file and fill the fgtevent
Int_t StFgtRawDaqReader::Make() {
   Int_t ierr = kStOk;

   StFgtDb *fgtTables = 0;
   if( !mIsCosmic ){
      if( !mFgtDbMkr ){
         LOG_FATAL << "Pointer to Fgt DB Maker is null" << endm;
         ierr = kStFatal;
      };
      if( !ierr ){
         fgtTables = mFgtDbMkr->getDbTables();

         if( !fgtTables ){
            LOG_FATAL << "Pointer to Fgt DB Tables is null" << endm;
            ierr = kStFatal;
         };
      };
   };

   if( !ierr ){
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
      
      while(dd && dd->iterate()) {
         fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

         for(u_int i=0;i<dd->ncontent;i++) {
            channel=f[i].ch;
            adc=f[i].adc;
            arm=dd->sec;
            apv=dd->pad;
            rdo=dd->rdo;
            timebin=f[i].tb;
            Short_t discIdx=0;  // will be set with getNaivePhysCoordFromElecCoord


            Short_t quad, strip;
            Char_t layer;
            Short_t geoId = ( mIsCosmic
                              ? StFgtCosmicTestStandGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel)
                              : fgtTables->getGeoIdFromElecCoord(rdo, arm, apv, channel) 
                              );
            StFgtGeom::decodeGeoId( geoId, discIdx, quad, layer, strip );


            /* DEBUGGING
               if( timebin == 1 ){
               cout << "AAA " << GetEventNumber() << " | " << rdo << ' ' << arm << ' ' << apv << ' ' << channel << " | " << geoId << ' ' << discIdx << ' ' << quad << ' ' << layer << ' ' << strip << endl;

               //                ( mIsCosmic
               //                  ? StFgtCosmicTestStandGeom::getNaiveElecCoordFromGeoId(geoId, rdo,arm,apv,channel)
               //                  : fgtTables->getElecCoordFromGeoId(geoId, rdo, arm, apv, channel) 
               //                  );
               //                cout << " | " << rdo << ' ' << arm << ' ' << apv << ' ' << channel << endl;
               };
            */

            Char_t type = 0;    // raw adc, no correction yet.

            StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
            if( stripCollectionPtr ) {
               StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
               stripVec.push_back( new StFgtStrip( geoId,adc,type,timebin) );
            } else {
               LOG_WARN <<"Could not access disc "<<endm;
            };
         };
      };
   };

  if( mCutShortEvents ){
     // clear events that do not have a complete set of channels
     Bool_t eventOK = 1;
     UInt_t numDiscs = mFgtCollectionPtr->getNumDiscs();

     for( UInt_t discIdx = 0; discIdx < numDiscs && eventOK; ++discIdx ){
        StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
        if( stripCollectionPtr ){
           Int_t remainder = stripCollectionPtr->getNumStrips() % 1280;
           Int_t estTimeBins = stripCollectionPtr->getNumStrips() / 1280;
           eventOK = ( remainder == 0 && ( estTimeBins == 5 || estTimeBins == 7 || estTimeBins == 1 ) );
        };
     };
     if( !eventOK ){
        for( UInt_t discIdx = 0; discIdx < numDiscs && eventOK; ++discIdx ){
          StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
          if( stripCollectionPtr )
             stripCollectionPtr->Clear();
        };
     };
  };

  return ierr;
};

void StFgtRawDaqReader::Clear( Option_t *opts )
{
   if( mFgtCollectionPtr )
      mFgtCollectionPtr->Clear( opts );
};


ClassImp(StFgtRawDaqReader);
