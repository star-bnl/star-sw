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

#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"
#include "StRoot/St_base/StMessMgr.h"
#include "RTS/src/DAQ_FGT/daq_fgt.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "RTS/src/DAQ_READER/daqReader.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

#include <string.h>
#include <time.h>

StFgtRawDaqReader::StFgtRawDaqReader( const Char_t* name, const Char_t *daqFileName, const Char_t* dbMkrName ) :
  StMaker( name ), mCutShortEvents(0), mIsCosmic(0), mFgtCollectionPtr(0), mDaqFileName( daqFileName ), mDbMkrName( dbMkrName ), 
  mRdr(0), mFgtDbMkr(0), mDataType(0), mStartTbin(0), mNumTbin(15) {

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

   if( !ierr ){
      //LOG_INFO << "constructing daqReader" << endm;

      // unfortunately, the daqReader has some constness issues to be
      // fixed.  Until they are, must remove constness of the filename.
      mRdr = new daqReader( const_cast< Char_t* >( mDaqFileName.data() ) ); 	
   };
   mRdr->get(0,EVP_TYPE_ANY);
   int unixtime=mRdr->evt_time;
   struct tm* local = localtime((const time_t*)&unixtime);
   int date=(local->tm_year+1900)*10000 + (local->tm_mon+1)*100 + local->tm_mday;
   int time=local->tm_hour*10000 + local->tm_min*100 + local->tm_sec;
   printf("Event Unix Time = %d %08d %06d\n",mRdr->evt_time,date,time);
   
   if( !ierr && !mIsCosmic ){
      if( !mDbMkrName.empty() ){
         // get the maker pointer
         mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMaker( mDbMkrName.data() ) );

         if( !ierr && !mFgtDbMkr ){
            LOG_FATAL << "Error finding FgtDbMkr named '" << mDbMkrName << "'" << endm;
            ierr = kStFatal;
         };

         if( !mFgtDbMkr->InheritsFrom("StFgtDbMaker") ){
            LOG_FATAL << "StFgtDbMkr does not inherit from StFgtDbMaker" << endm;
            LOG_FATAL << "Name is '" << mFgtDbMkr->GetName() << "', class is '" << mFgtDbMkr->ClassName() << endm;
            ierr = kStFatal;
         };
      };

      if( !mFgtDbMkr ){
         mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
         if( !mFgtDbMkr ){
            LOG_FATAL << "StFgtDbMaker name not provided and error finding StFgtDbMaker" << endm;
            ierr = kStFatal;
         };
      };

      if(date<21000000 && date>19990000) mFgtDbMkr->SetDateTime(date,time);
      LOG_INFO << "Using date and time " << mFgtDbMkr->GetDateTime().GetDate() << ", "
               << mFgtDbMkr->GetDateTime().GetTime() << endm;
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
      if(mDataType==0){
	dd = mRdr->det("fgt")->get("adc");
	if(!dd) dd = mRdr->det("fgt")->get("zs");
      }else if(mDataType==1){
	dd = mRdr->det("fgt")->get("adc");
      }else if(mDataType==2){
	dd = mRdr->det("fgt")->get("zs");
      }else if(mDataType==3){
	dd = mRdr->det("fgt")->get("zs");
	if(!dd) dd = mRdr->det("fgt")->get("adc");
      }

      int ntimebin=0;
      if(dd && dd->meta){
	apv_meta_t *meta = (apv_meta_t *)dd->meta;
	for(int r=1;r<=FGT_RDO_COU;r++) {
	  if(meta->arc[r].present == 0) continue ;
	  for(int arm=0;arm<FGT_ARM_COU;arm++) {
	    if(meta->arc[r].arm[arm].present == 0) continue ;
	    for(int apv=0;apv<FGT_APV_COU;apv++) {
	      if(meta->arc[r].arm[arm].apv[apv].present == 0) continue ;
	      int nt=meta->arc[r].arm[arm].apv[apv].ntim;
	      //printf("RDO=%1d ARM=%1d APV=%02d Number of time bin =%d\n",r,arm,apv,nt);
	      if(ntimebin!=0 && nt!=0 && ntimebin!=nt) printf("Different number of timebins in different APV!!! Taking larger one!!!\n");
	      if(ntimebin<nt) ntimebin=nt;
	    }
	  }
	}
      }
      mFgtCollectionPtr->setNumTimeBins(ntimebin);      
      if(mNumTbin < ntimebin) mFgtCollectionPtr->setNumTimeBins(mNumTbin);
      //printf("Max Number of Timebin=%d\n",ntimebin);

      while(dd && dd->iterate()) {
         fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

         for(u_int i=0;i<dd->ncontent;i++) {
            timebin=f[i].tb - mStartTbin;
	    if(timebin<0 || timebin>=mNumTbin) continue;

            channel=f[i].ch;
            adc=f[i].adc;
            arm=dd->sec;
            apv=dd->pad;
            rdo=dd->rdo;	    

	    int flag=0;
	    //corrupted data in non existing channels
	    if(rdo<1 || rdo > kFgtNumRdos)	                flag=1;
	    if(arm<0 || arm>=kFgtNumArms)	                flag=1;
	    if(apv>=22 || apv <  0 || apv ==10|| apv==11)   	flag=1;
	    if(channel<0 || channel>=kFgtNumChannels)	        flag=1;
	    if(timebin<0 || timebin>=kFgtNumTimeBins)	        flag=1;
	    //LOG_INFO<< "rdo: " << rdo <<" arm: " << arm <<" apv: " << apv <<" channel: " << channel <<" tbin: "<<timebin<<endm;
	    if(flag==1){
	      LOG_INFO<< "Corrupt data  rdo: " << rdo <<" arm: " << arm <<" apv: " << apv <<" channel: " << channel <<" tbin: "<<timebin<<endm;
	      continue;
	    }
	    
#if 0
	    // year 2012 exclusions -- adjusted (bug fixed) 02/29/12 by sgliske
	    //            if( ( (rdo==1 && arm==1) || (rdo==2 && arm==2) || (rdo==1 && arm==4) ) && apv>4 && apv<10 ) continue;
	    //            if( ( (rdo==2 && arm==1) || (rdo==1 && arm==3) || (rdo==2 && arm==4) ) && apv<5 ) continue;
	    //            if( ( (rdo==2 && arm==1) || (rdo==1 && arm==3) || (rdo==2 && arm==4) ) && apv>16 ) continue;
	    //            if( ( (rdo==1 && arm==2) || (rdo==2 && arm==3) || (rdo==2 && arm==4) ) && apv>9 && apv<17) continue;
            // end of 2012 exclusions
#endif 
            Short_t discIdx=0;  // will be set with getNaivePhysCoordFromElecCoord
            Short_t quad, strip;
            Char_t layer;
            Int_t geoId = ( mIsCosmic
			    ? StFgtCosmicTestStandGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel)
			    : fgtTables->getGeoIdFromElecCoord(rdo, arm, apv, channel) 
			    );
            StFgtGeom::decodeGeoId( geoId, discIdx, quad, layer, strip );
            if(discIdx < 0 || discIdx >= kFgtNumDiscs)  flag=1;
            if(quad <0 || quad >= kFgtNumQuads)         flag=1;
            if(layer != 'P' && layer != 'R')            flag=1;
	    if(strip < 0 || strip >= kFgtNumStrips)     flag=1;
	    if(flag==1){
	      LOG_INFO<<"Wrong geoId: " << geoId <<" discIdx: " << discIdx << " quad: " << quad << " layer: " << layer <<" strip: " << strip
		      <<" is cosmic: " << mIsCosmic << " rdo: " << rdo <<" arm: " << arm <<" apv: " << apv <<" channel: " << channel <<endm;
	      continue;
	    }
	      
	    // cout << "AAA " << GetEventNumber() << " | " << rdo << ' ' << arm << ' ' << apv << ' ' << channel << " | " << geoId << ' ' << discIdx << ' ' << quad << ' ' << layer << ' ' << strip << ' '<<timebin<<endl;
            /* DEBUGGING
               if( timebin == 1 ){
               
               //                ( mIsCosmic
               //                  ? StFgtCosmicTestStandGeom::getNaiveElecCoordFromGeoId(geoId, rdo,arm,apv,channel)
               //                  : fgtTables->getElecCoordFromGeoId(geoId, rdo, arm, apv, channel) 
               //                  );
               //                cout << " | " << rdo << ' ' << arm << ' ' << apv << ' ' << channel << endl;
               };
            */

            StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
            if( stripCollectionPtr ) {
               Int_t elecId =  StFgtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel );
               StFgtStrip* stripPtr = stripCollectionPtr->getStrip( elecId );
               stripPtr->setAdc( adc, timebin );
               stripPtr->setGeoId( geoId );
               stripPtr->setElecCoords( rdo, arm, apv, channel );
            } else {
               LOG_WARN << "StFgtRawDaqReader::Make() -- Could not access stripCollection for disc " << discIdx << endm;
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
           Int_t chanPerQuad = kFgtApvsPerQuad*kFgtNumChannels; // 1280
           Int_t remainder = stripCollectionPtr->getNumStrips() % chanPerQuad;
           eventOK = (remainder == 0);
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
     {
       //       LOG_DEBUG<<"info" <<endl;
       //       cout <<"clearing collectons...  " <<endl;

      mFgtCollectionPtr->Clear( opts );
      //      cout <<"done " <<endl;
     }
};


ClassImp(StFgtRawDaqReader);

/*
 * $Id: StFgtRawDaqReader.cxx,v 1.18 2013/03/19 03:24:19 akio Exp $
 * $Log: StFgtRawDaqReader.cxx,v $
 * Revision 1.18  2013/03/19 03:24:19  akio
 * remove static from numtimebin
 *
 * Revision 1.17  2013/03/10 05:45:29  akio
 * added option to limit timebins to feed rest of makers
 *
 * Revision 1.16  2013/03/03 04:59:12  akio
 * less printing
 *
 * Revision 1.15  2013/02/21 20:30:26  akio
 * added ZS data first option
 *
 * Revision 1.14  2013/02/06 18:10:54  akio
 * getting date & time from data
 *
 * Revision 1.13  2013/01/31 20:44:55  akio
 * Adding StFgtCollection::setNumTimeBins()
 *
 * Revision 1.12  2013/01/31 20:00:32  akio
 * adding obtaining number of timebins from meta data
 * adding options for zero suppressed data
 *
 * Revision 1.11  2012/11/26 15:20:35  akio
 * remove some hardcoded numbers, and use StEnumeration
 *
 * Revision 1.10  2012/08/23 20:05:35  avossen
 * commented out year 12 exclusions
 *
 * Revision 1.9  2012/03/07 15:23:52  sgliske
 * StFgtStrip no longer has a type field
 *
 * Revision 1.8  2012/02/29 18:23:46  sgliske
 * fixed bug in 2012 exclusions
 *
 * Revision 1.7  2012/02/07 05:33:25  balewski
 * *** empty log message ***
 *
 * Revision 1.6  2012/02/06 04:17:32  balewski
 * added 2012 APV exclusions
 *
 * Revision 1.5  2012/02/05 21:19:22  avossen
 * added check for invalid elec coordinates
 *
 * Revision 1.4  2012/01/31 16:48:56  wwitzke
 * Changed for cosmic test stand.
 *
 * Revision 1.3  2012/01/31 11:23:34  sgliske
 * No longer requires passing name of StFgtDbMaker
 *
 * Revision 1.2  2012/01/31 09:16:55  sgliske
 * fixed cvs caption
 *
 * Revision 1.1  2012/01/31 09:15:34  sgliske
 * Moved to StFgtPool
 *
 */
