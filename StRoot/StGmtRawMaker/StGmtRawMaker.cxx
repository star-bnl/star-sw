//
// \class StGmtRawMaker
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtRawMaker
//

#include "St_base/StMessMgr.h"
#include "St_base/Stypes.h"

#include "StChain/StRtsTable.h"
#include "StEvent/StEvent.h"
#include "DAQ_FGT/daq_fgt.h"
#include "DAQ_READER/daq_dta.h"
// #include "DAQ_GMT/daq_gmt.h"
// #include "DAQ_READER/daq_dta.h"

#include "StEvent/StGmtCollection.h"
#include "StEvent/StGmtStripCollection.h"
#include "StEvent/StGmtStrip.h"
// #include "StGmtDbMaker/StGmtDbMaker.h"
#include "StGmtUtil/geometry/StGmtGeom.h"
#include "St_base/StMessMgr.h"
#include "St_base/Stypes.h"

#include "StGmtRawMaker.h"

const Int_t mChIdToSeqId[128] = {
	0,16,32,48,64,80,96,112,4,20,36,52,68,84,100,116,8,
	24,40,56,72,88,104,120,12,28,44,60,76,92,108,124,1,
	17,33,49,65,81,97,113,5,21,37,53,69,85,101,117,9,
	25,41,57,73,89,105,121,13,29,45,61,77,93,109,125,2,
	18,34,50,66,82,98,114,6,22,38,54,70,86,102,118,10,
	26,42,58,74,90,106,122,14,30,46,62,78,94,110,126,3,
	19,35,51,67,83,99,115,7,23,39,55,71,87,103,119,11,
	27,43,59,75,91,107,123,15,31,47,63,79,95,111,127
};


/**
Function to get pointer to StEvent datastructures. Creates them if they do not exist already.
*/
Int_t StGmtRawMaker::prepareEnvironment()
{
  StEvent* eventPtr=0;
  eventPtr= (StEvent*)StRTSBaseMaker::GetInputDS("StEvent");

  mGmtCollectionPtr=NULL;
  if(eventPtr)
    {
      mGmtCollectionPtr=eventPtr->gmtCollection();
    }
  else
    {
      eventPtr=new StEvent();
      if(!eventPtr)
	{
	  LOG_DEBUG <<"::prepareEnvironment could not create StGmtEvent" <<endm;
	  return kStFatal;
	}
      StRTSBaseMaker::AddData(eventPtr);
      mGmtCollectionPtr=eventPtr->gmtCollection();
    }
  if(!mGmtCollectionPtr)
    {
      mGmtCollectionPtr=new StGmtCollection();
      if(!mGmtCollectionPtr)
	{
	  LOG_DEBUG <<"::prepareEnvironment could not create StGmtCollection" <<endm;
	  return kStFatal;
	}
      eventPtr->setGmtCollection(mGmtCollectionPtr);
      LOG_DEBUG <<"::prepareEnvironment() has added a non existing StGmtCollection()"<<endm;
    }
  else
    {
      //this should be unncessary if the member clear function is called
      mGmtCollectionPtr->Clear();
    }
  return kStOK;
}

/**
Maker main function. Getting pointer to StEvent and fills the event structure
*/
Int_t StGmtRawMaker::Make()
{

  LOG_DEBUG <<"StGmtRawMaker::Make()******************************************************************"<<endm;

  if( prepareEnvironment()!=kStOK )
    {
      LOG_ERROR << "Error preparing enviroment" << endm;
      return kStFatal;
    }
  else
    {
      return fillHits();
    }
}


/**
utility function to get the data from the daq file and fill the StEvent structure
*/
Int_t StGmtRawMaker::fillHits()
{

  Char_t layer=0;
  Int_t rdo=0;
  Int_t arm=0;
  Int_t apv=0;
  Int_t port=-999;
  Int_t channel=0;
  Short_t adc=0;
  Short_t timebin=0;
  Short_t moduleIdx=0;
  Short_t coordNum=0;
  Double_t position=0;

//   LOG_INFO << "StGmtRawMaker::fillHits() Trying to find fgt/adc... " << endm;
  LOG_INFO << "StGmtRawMaker::fillHits() Trying to find gmt/adc... " << endm;
  
	  ////// FIX ME!!!!!!!!!!!!!!!!!!
  //now grab the constants from the header file, loop over the raw data and fill the hits...
//   while(GetNextDaqElement("fgt/adc"))
  while(GetNextDaqElement("gmt/adc"))
    {
      StRtsTable* rts_tbl=DaqDta();
      //works because '*' operator is giving your the row
      //LOG_INFO << "StGmtRawMaker::fillHits() Looks like there are : " <<  rts_tbl->GetTableSize() <<
//	 " entries in the rts_tbl..." << endm;

	 Int_t count = 0;
      for(StRtsTable::iterator it=rts_tbl->begin();it!=rts_tbl->end();it++)
	{
	  fgt_adc_t *mGmtRawData=(fgt_adc_t*)*it;
	  rdo=rts_tbl->Rdo();
	  Int_t seqId = count/7;
	  Int_t chanTmp=mGmtRawData->ch;
	  channel=mChIdToSeqId[chanTmp];	
	  //this is different from rts_example
	  timebin=mGmtRawData->tb;
	  //look at rts_example for the mapping 
	  adc=mGmtRawData->adc;
	  arm=rts_tbl->Sector();
	  apv=rts_tbl->Pad();

	  // the next segment is needed because of a lack of ARM port information
	  if ( (apv >= 0) && (apv <= 3)) {
	    port = 0; 
	  }
	  else if ( (apv >= 12) && (apv <= 15) ) {
//		  cout<<"APV "<<apv<<endl;
	    port = 1;
	  }

	  if(Debug()){
		  LOG_INFO << "StGmtRawMaker::fillHits() Got: \n" <<
			  "rdo: " << rdo <<
			  "  arm: " << arm <<
			  "  apv: " << apv <<
			  "  port: " << port <<
			  "  seq: " << seqId <<
			  "  channel: " << channel <<
			  "  timebin: " << timebin <<
			  "  adc: " << adc <<
			  endm;
	  }


// 	  LOG_INFO << "StGmtRawMaker::fillHits() Got: \n" << 
// 	  "rdo: " << rdo << 
// 	  "  arm: " << arm << 
// 	  "  apv: " << apv << 
// 	  "  port: " << port << 
// 	  "  channel: " << channel << 
// 	  "  timebin: " << timebin << 
// 	  "  adc: " << adc << 
// 	  endm;

// 	  if( apv < 0 || apv > 15 || (apv > 3 && apv < 12) ) continue;
// 	  if( arm < 0 || arm > 1 ) continue;
// 	  if( timebin >= 15 ) continue;
// 	  if( channel >= 128 ) continue;
// 	  if( rdo != 1) continue;

	  ////// FIX ME!!!!!!!!!!!!!!!!!!
	  Int_t geoId=-1;
	  Int_t layer=-1;
	  Short_t strip=-1;
// 	  if(!mGmtDb)
// 	    //		 geoId=StGmtGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);
// 	    {
// 	      LOG_FATAL<<Form("StGmtRawMaker: No DB available")<<endm;
// 	      return kStFatal;
// 	    }    
// 	  else
// 	    {
// 	      geoId=mGmtDb->getGeoIdFromElecCoord(rdo, arm, apv, channel);
// 	      mGmtDb->getPhysCoordFromElecCoord(rdo,arm,apv,channel,moduleIdx,layer);
// 	    }

	  // StGmtGeom::getNaivePhysCoordFromElecCoord(rdo,arm,apv,channel,quadrant,layer,ordinate,lowerSpan,upperSpan);

	  //	  Char_t type = kGmtRawAdc;
	  //	  Char_t type = 0;

	  moduleIdx = StGmtGeom::getModuleIdFromElecCoord( rdo, arm, apv );
	  coordNum = StGmtGeom::getCoordNumFromElecCoord( rdo, arm, apv, channel );
	  position = StGmtGeom::getPositionFromElecCoord( rdo, arm, apv, channel );

	  // the next segment is needed because of a lack of ARM port information
// 	  if ( (apv >= 12) && (apv <= 15) ) {
// 	    apv  = apv - 12; // just so we get a useful apv number in this port
// 	  }
	  
	  geoId = StGmtGeom::encodeGeoId( rdo, arm, apv, channel ); // geoId same as elecId for now (RW 3/29/2013)
	  StGmtGeom::decodeGeoId( geoId, moduleIdx, layer, strip );

// 	  if( timebin == 0)
// 	  {
// 	    LOG_INFO << "\t geoId is: " << geoId << " elecId is: " << 
// 	    StGmtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel ) << " counter: " << count << endm;
// 	    LOG_INFO << "\t\t module " << moduleIdx << 
// 	    " layer " << layer << " strip " << strip << endm;
// 	    LOG_INFO << "\t\t arm " << arm << 
// 	    " apv " << apv << " channel " << channel << endm;
// 	    count++;
// 	  }
	  
	  StGmtStripCollection *stripCollectionPtr = mGmtCollectionPtr->getStripCollection( moduleIdx );
	  if( stripCollectionPtr )
	    {
	      geoId =  StGmtGeom::encodeGeoId( rdo, arm, apv, channel );
	      Int_t elecId =  StGmtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel );
// 	      LOG_INFO << "StGmtRawMaker::fillHits() elecId is: " << elecId << endm;
// 	      StGmtStrip* stripPtr = stripCollectionPtr->getStrip( elecId );
	      StGmtStrip* stripPtr = stripCollectionPtr->getStrip( geoId );
	      if( coordNum == 999 ) // these are not connected
	      {
		stripPtr->setAdc( 0, timebin );
		stripPtr->setCharge( 0 ); // was done in separate maker for FGT (StFgtA2CMaker), assume gain=1 for now
		stripPtr->setChargeUncert( 0 ); // was done in separate maker for FGT (StFgtA2CMaker), assume gain=1 for now
		stripPtr->setGeoId( geoId );
		stripPtr->setModule( moduleIdx );
		stripPtr->setIsY( layer );
		stripPtr->setPosition( position );
		stripPtr->setElecCoords( rdo, arm, apv, channel );
		stripPtr->setCoordNum( coordNum );
		stripPtr->setPed( 0 );
		stripPtr->setPedStdDev( 0 );
		stripPtr->setPedErr( 0 );
	      }
	      else  // these are connected (mapping in StGmtGeom.cxx)
	      {
		if(layer) // layer here is just an indicator for either a X ( i.e. strip (=0) ) or Y ( i.e. pad (=1) ) element
		{
		  if( timebin == 0)
		  {
// 		    LOG_INFO << "\t\t will set coordinates " << coordNum + kGmtNumStrips << endm;
		  }
		  stripPtr->setCoordNum( coordNum + kGmtNumStrips ); // map Y into 128-255
		  //never returns more certain ids
		  if(channel==100) LOG_INFO << "Str.=" << channel << "\tLay0\tgeoid=" << geoId<< "\tposition=" << position << endl;
				}
		else
		{
		  if( timebin == 0)
		  {
// 		    LOG_INFO << "\t\t will set coordinates " << coordNum << endm;
		  }
		  stripPtr->setCoordNum( coordNum );  // map X into 0-127
		  if(channel==50) LOG_INFO << "Str.=" << channel << "\tLay0\tgeoid=" << geoId<< "\tposition=" << position << endl;
		}
		stripPtr->setAdc( adc, timebin );
		stripPtr->setCharge( adc ); // was done in separate maker for FGT (StFgtA2CMaker), assume gain=1 for now
		stripPtr->setChargeUncert( sqrt(adc) ); // was done in separate maker for FGT (StFgtA2CMaker), assume gain=1 for now
		stripPtr->setGeoId( geoId );
		stripPtr->setModule( moduleIdx );
		stripPtr->setIsY( layer );
		stripPtr->setPosition( position );
		stripPtr->setElecCoords( rdo, arm, apv, channel );
	
	      }
	    }
	  else
	    { LOG_WARN << "StGmtRawMaker::Make() -- Could not access module " << moduleIdx << endm; }
	}
        
        //LOG_INFO << "StGmtRawMaker::Make() -- Set information for " << count << " strips." << endm;

    }

  return kStOK;
}

/**
Init function. Not doing anything at the moment.
*/
Int_t StGmtRawMaker::Init()
{
  LOG_INFO << "StGmtRawMaker::Init we are named "  << GetName() << endm;
  return kStOk;
}

////////// FIX ME!!!!!!!!!!!!!!!!!!
/// InitRun will be called after GmtDb due to chain dependencies
Int_t StGmtRawMaker::InitRun(Int_t runNumber)
{
  LOG_INFO << "StGmtRawMaker::InitRun with run = "  << runNumber << endm;
//   if (! mGmtDb ){
//     StGmtDbMaker * gmtMaker;
//     gmtMaker = (StGmtDbMaker *) GetMaker("gmtDb");
//     if ( gmtMaker ){
//       // NOTE JL 2012/07
//       // we can at least return a point to the expected struct to
//       // get back to our feet but this is non-standard
//       mGmtDb = gmtMaker->getDbTables();
//       LOG_INFO   << "::InitRun Getting gmtDb info" << endm;
//     } else {
//       LOG_ERROR  << "::InitRun we will fail as gmtDb is not present" << endm;
//     }
//   }
  return kStOK;
}



/**
constructor. 
*/
StGmtRawMaker::StGmtRawMaker(const Char_t* name) :
  StRTSBaseMaker( "adc", name ),
//   mGmtCollectionPtr(0), mGmtDb(0)
  mGmtCollectionPtr(0)
{
}


StGmtRawMaker::~StGmtRawMaker()
{
  // nothing to do
}

void StGmtRawMaker::Clear( Option_t *opts )
{
  if( mGmtCollectionPtr )
    mGmtCollectionPtr->Clear( opts );
}


//StGmtRawMaker& StGmtRawMaker::operator=(const StGmtRawMaker &source)
//{
//  StRTSBaseMaker::operator=(source);
//  return *this;
//}

//StGmtRawMaker::StGmtRawMaker(const StGmtRawMaker &source):StRTSBaseMaker(source)
//{
//}

ClassImp(StGmtRawMaker)
