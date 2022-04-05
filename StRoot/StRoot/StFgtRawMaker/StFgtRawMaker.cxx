//
// \class StFgtRawMaker
// \author Anselm Vossen
//
//   $Id: StFgtRawMaker.cxx,v 1.38 2013/12/20 05:19:46 xuanli Exp $
//
//  $Log: StFgtRawMaker.cxx,v $
//  Revision 1.38  2013/12/20 05:19:46  xuanli
//  Update FgtRawMaker for run13 test production.
//
//  Revision 1.37  2013/06/03 15:47:52  avossen
//  changed return code from fatal to warn if #tb is to high in meta data
//
//  Revision 1.36  2013/05/25 17:34:52  avossen
//  checked for maximum allowed timebin
//
//  Revision 1.35  2013/05/25 17:18:51  avossen
//  checked for maximum allowed timebin
//
//  Revision 1.34  2013/02/19 20:57:01  akio
//  Added getting timebin from meta data, and also support for zero suppresed data
//
//  Revision 1.33  2012/07/31 18:25:53  jeromel
//  Remove virtual + add InitRun to get Db point (previous method implied passing from outside a pointer to a maker (sigh!) not appropriate)
//
//  Revision 1.32  2012/03/07 03:57:23  avossen
//  various updates
//
//  Revision 1.31  2012/02/21 19:44:45  avossen
//  implementing reviewers comments take 2
//
//  Revision 1.30  2012/02/21 04:41:57  avossen
//  *** empty log message ***
//
//  Revision 1.29  2012/02/20 23:56:39  avossen
//  addressing reviewers comments take 1
//
//  Revision 1.28  2012/02/06 04:17:45  balewski
//  added 2012 APV exclusions
//
//  Revision 1.27  2012/02/05 21:19:22  avossen
//  added check for invalid elec coordinates
//
//  Revision 1.26  2012/01/30 10:42:23  sgliske

//  all time bins.  Also fixed bug where setType modified the timebin
//  rather than the type.
//
//  Revision 1.25  2012/01/28 18:54:47  avossen
//  removed last naive call and reference to StFgtGeom.h
//
//  Revision 1.24  2012/01/26 13:13:12  sgliske
//  Updated to use StFgtConsts, which
//  replaces StFgtEnums and StFgtGeomDefs
//
//  Revision 1.23  2012/01/18 17:48:31  sgliske
//  StEvent/StFgtStrip now contains rdo/arm/apv/channel
//
//  Revision 1.22  2012/01/18 03:28:25  avossen
//  removed naive calls
//
//  Revision 1.21  2012/01/18 03:10:51  avossen
//  added db access to the raw maker
//
//  Revision 1.20  2012/01/17 21:56:26  sgliske
//  Short_t geoId -> Int_t geoId
//
//  Revision 1.19  2011/11/01 18:45:32  sgliske
//  Updated to correspond with StEvent containers, take 2.
//  Note: new FGT containers (and StEvent access) no longer
//  motivate the use of a common base class
//
//  Revision 1.18  2011/10/27 21:09:47  jeromel
//  Small info added in Init() + ident
//
//  Revision 1.17  2011/10/26 21:32:01  avossen
//  fixed mFgtEvent pointer name
//
//  Revision 1.16  2011/10/26 20:57:48  avossen
//  hopefully made cosmic and raw maker compatible with bfc (again), added clear in make. Unnecessary if member fkt clear() is called after every event
//
//  Revision 1.15  2011/10/06 15:19:43  sgliske
//  StFgtRawHitArray::PushBack -> pushBack
//
//  Revision 1.14  2011/09/26 14:23:06  sgliske
//  Update for new 'Char_t mType' field in StFgtRawHit
//
//  Revision 1.13  2011/09/21 17:49:34  sgliske
//  alternate base class with more
//   functionality and not an StMaker
//
//  Revision 1.11  2011/09/20 15:53:09  sgliske
//  Update so that everything compiles nicely
//  and so that one can execute the macro/simpleTestStandTest.C file
//
//  Revision 1.10  2011/09/19 21:12:36  sgliske
//  update
//
//  Revision 1.9  2011/09/14 17:21:19  avossen
//  using dev allows cint to compile.
//
//  Revision 1.8  2011/09/14 15:44:11  avossen
//  took out the root cint stuff so it compiles
//
//  Revision 1.7  2011/09/13 18:35:42  avossen
//  added RTS header files
//
//  Revision 1.6  2011/09/13 10:06:43  avossen
//  *** empty log message ***
//
//  Revision 1.5  2011/09/11 08:06:36  avossen
//  added cosmic maker
//
//  Revision 1.4  2011/08/24 14:30:44  avossen
//  Continued raw maker development
//
//

#include "St_base/StMessMgr.h"
#include "St_base/Stypes.h"

#include "StChain/StRtsTable.h"
#include "StEvent/StEvent.h"
#include "DAQ_FGT/daq_fgt.h"
#include "DAQ_READER/daq_dta.h"

#include "StEvent/StFgtCollection.h"
#include "StEvent/StFgtStripCollection.h"
#include "StEvent/StFgtStrip.h"
#include "StFgtDbMaker/StFgtDbMaker.h"
#include "St_base/StMessMgr.h"
#include "St_base/Stypes.h"
#include <string>
#include <sstream>
#include "StFgtRawMaker.h"


/**
Function to get pointer to StEvent datastructures. Creates them if they do not exist already.
*/
Int_t StFgtRawMaker::prepareEnvironment()
{
  StEvent* eventPtr=0;
  eventPtr= (StEvent*)StRTSBaseMaker::GetInputDS("StEvent");

  mFgtCollectionPtr=NULL;
  if(eventPtr)
    {
      mFgtCollectionPtr=eventPtr->fgtCollection();
    }
  else
    {
      eventPtr=new StEvent();
      if(!eventPtr)
	{
	  LOG_DEBUG <<"::prepareEnvironment could not create StFgtEvent" <<endm;
	  return kStFatal;
	}
      StRTSBaseMaker::AddData(eventPtr);
      mFgtCollectionPtr=eventPtr->fgtCollection();
    }
  if(!mFgtCollectionPtr)
    {
      mFgtCollectionPtr=new StFgtCollection();
      if(!mFgtCollectionPtr)
	{
	  LOG_DEBUG <<"::prepareEnvironment could not create StFgtCollection" <<endm;
	  return kStFatal;
	}
      eventPtr->setFgtCollection(mFgtCollectionPtr);
      LOG_DEBUG <<"::prepareEnvironment() has added a non existing StFgtCollection()"<<endm;
    }
  else
    {
      //this should be unncessary if the member clear function is called
      mFgtCollectionPtr->Clear();
    }
  return kStOK;
}

/**
Maker main function. Getting pointer to StEvent and fills the event structure
*/
Int_t StFgtRawMaker::Make()
{

  LOG_DEBUG <<"StEmcRawMaker::Make()******************************************************************"<<endm;

  if( prepareEnvironment()!=kStOK )
    {
      LOG_ERROR << "Error preparing enviroment" << endm;
      return kStFatal;
    }
  else
    {
      mEvent++;
      try{
	return fillHits();
      }
      catch(string s)
	{
	  LOG_ERROR << s << endm;
	  return kStWarn;
	}
    }
}


/**
utility function to get the data from the daq file and fill the StEvent structure
*/
Int_t StFgtRawMaker::fillHits()
{

  Short_t quadrant=0;      
  Char_t layer=0;
  Double_t ordinate=0;
  Double_t lowerSpan=0;
  Double_t upperSpan=0;
  Int_t rdo=0;
  Int_t arm=0;
  Int_t apv=0;
  Int_t channel=0;
  Short_t adc=0;
  Short_t timebin=0;
  Short_t discIdx=0;

  //now grab the constants from the header file, loop over the raw data and fill the hits...
  StRtsTable* rts_tbl=0;
  int flag=0;
  int ntimebin=0;
  while(1){
      if(flag==0){
	if(mDataType==0){
	  rts_tbl = GetNextDaqElement("fgt/adc"); flag=1;
	  if(!rts_tbl) { rts_tbl = GetNextDaqElement("fgt/zs"); flag=2; }
	}else if(mDataType==1){
	  rts_tbl = GetNextDaqElement("fgt/adc"); flag=1;
	}else if(mDataType==2){
	  rts_tbl = GetNextDaqElement("fgt/zs");  flag=2;
	}
      }
      else if(flag==1){ rts_tbl = GetNextDaqElement("fgt/adc"); }
      else if(flag==2){ rts_tbl = GetNextDaqElement("fgt/zs"); }
      if(!rts_tbl) break;
      
      apv_meta_t *meta = (apv_meta_t *)rts_tbl->Meta();
      if(meta){
	for(int r=1;r<=FGT_RDO_COU;r++) {
	  if(meta->arc[r].present == 0) continue ;
	  for(int arm=0;arm<FGT_ARM_COU;arm++) {
	    if(meta->arc[r].arm[arm].present == 0) continue ;
	    for(int apv=0;apv<FGT_APV_COU;apv++) {
	      if(meta->arc[r].arm[arm].apv[apv].present == 0) continue ;
	      int nt=meta->arc[r].arm[arm].apv[apv].ntim;
	      //printf("RDO=%1d ARM=%1d APV=%02d Number of time bin =%d\n",r,arm,apv,nt);
	      if(ntimebin!=0 && nt!=0 && ntimebin!=nt) {
		LOG_ERROR << "Different number of timebins in different APV!!! Taking larger one!!!" << endm;
	      }
	      if(ntimebin<nt) ntimebin=nt;
	      if(ntimebin>kFgtNumTimeBins)
		{
		  stringstream ss;
		  ss<<"timebin nr read from APV ("<<ntimebin<<") larger than allocated space ("<<kFgtNumTimeBins<<")";
		  throw ss.str();
		}
	    }
	  }
	}
      }
      
      //works because '*' operator is giving your the row
      for(StRtsTable::iterator it=rts_tbl->begin();it!=rts_tbl->end();it++)
	{
	  fgt_adc_t *mFgtRawData=(fgt_adc_t*)*it;
	  rdo=rts_tbl->Rdo();
	  //this is different from rts_example
	  channel=mFgtRawData->ch;
	  timebin=mFgtRawData->tb;
	  //look at rts_example for the mapping 
	  adc=mFgtRawData->adc;
	  arm=rts_tbl->Sector();
	  apv=rts_tbl->Pad();
	  //printf("rdo=%1d arm=%1d apv=%2d ch=%3d tb=%1d adc=%4d\n",rdo,arm,apv,channel,timebin,adc);

	  if(apv>=22 || apv <  0 || apv ==10|| apv==11)	 continue;
	  if(arm<0 || arm>5)		    continue;
	  if(timebin<0 || timebin>ntimebin || timebin>kFgtNumTimeBins) continue;
	  if(channel<0 || channel>=128)	    continue;
	  if(rdo<1 || rdo>2)		    continue;

	  // year 2012 exclusions
	  //	  if( ( (rdo==1 && arm==1) || (rdo==2 && arm==2) || (rdo==1 && arm==4)) && apv>4 && apv<10 ) continue;
	  //	  if( ((rdo==2 && arm==1) ||(rdo==1 && arm==3) ||(rdo==2 && arm==4) ) && apv<5 ) continue;
	  //	  if( rdo==2 && arm==4)  continue;
	  //	  if( ( (rdo==2 && arm==1) ||(rdo==1 && arm==3)  ) && apv>16 ) continue;
	  //	  if( ((rdo==1 && arm==2) ||(rdo==2 && arm==3)  ) && apv>10 && apv<17) continue;
	  // end of 2012 exclusions


	  Int_t geoId=-1;
	  if(!mFgtDb)
	    //		 geoId=StFgtGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);
	    {
	      LOG_FATAL<<Form("StFgtRawMaker: No DB available")<<endm;
	      return kStFatal;
	    }    
	  else
	    {
	      geoId=mFgtDb->getGeoIdFromElecCoord(rdo, arm, apv, channel);
	      mFgtDb->getPhysCoordFromElecCoord(rdo,arm,apv,channel,discIdx,quadrant,layer,ordinate,lowerSpan,upperSpan);
	    }

	  //               StFgtGeom::getNaivePhysCoordFromElecCoord(rdo,arm,apv,channel,discIdx,quadrant,layer,ordinate,lowerSpan,upperSpan);

	  //	  Char_t type = kFgtRawAdc;
	  //	  Char_t type = 0;

	  StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
	  if( stripCollectionPtr )
	    {
	      Int_t elecId =  StFgtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel );
	      StFgtStrip* stripPtr = stripCollectionPtr->getStrip( elecId );
	      stripPtr->setAdc( adc, timebin );
	      //	      stripPtr->setType( type );
	      stripPtr->setGeoId( geoId );
	      stripPtr->setElecCoords( rdo, arm, apv, channel );
	    }
	  else
	    { LOG_WARN << "StFgtRawMaker::Make() -- Could not access disc " << discIdx << endm; }
	}
      }
      
  if(mEvent<3) LOG_INFO << "StFgtRawMaker:: Number of Timebin from meta data = " << Form("%d",ntimebin) << endm;
  mFgtCollectionPtr->setNumTimeBins(ntimebin);

  return kStOK;
}

/**
Init function. Not doing anything at the moment.
*/
Int_t StFgtRawMaker::Init()
{
  LOG_INFO << "StFgtRawMaker::Init we are named "  << GetName() << endm;
  return kStOk;
}


/// InitRun will be called after FgtDb due to chain dependencies
Int_t StFgtRawMaker::InitRun(Int_t runNumber)
{
  LOG_INFO << "StFgtRawMaker::InitRun with run = "  << runNumber << endm;
  if (! mFgtDb ){
    StFgtDbMaker * fgtMaker;
    fgtMaker = (StFgtDbMaker *) GetMaker("fgtDb");
    if ( fgtMaker ){
      // NOTE JL 2012/07
      // we can at least return a point to the expected struct to
      // get back to our feet but this is non-standard
      mFgtDb = fgtMaker->getDbTables();
      LOG_INFO   << "::InitRun Getting fgtDb info" << endm;
    } else {
      LOG_ERROR  << "::InitRun we will fail as fgtDb is not present" << endm;
    }
  }
  return kStOK;
}



/**
constructor. 
*/
StFgtRawMaker::StFgtRawMaker(const Char_t* name) :
  StRTSBaseMaker( "adc", name ),
  mFgtCollectionPtr(0), mFgtDb(0), mEvent(0), mDataType(0)
{
}


StFgtRawMaker::~StFgtRawMaker()
{
  // nothing to do
}

void StFgtRawMaker::Clear( Option_t *opts )
{
  if( mFgtCollectionPtr )
    mFgtCollectionPtr->Clear( opts );
  mFgtCollectionPtr=NULL;
}


//StFgtRawMaker& StFgtRawMaker::operator=(const StFgtRawMaker &source)
//{
//  StRTSBaseMaker::operator=(source);
//  return *this;
//}

//StFgtRawMaker::StFgtRawMaker(const StFgtRawMaker &source):StRTSBaseMaker(source)
//{
//}

ClassImp(StFgtRawMaker)
