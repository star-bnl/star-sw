#include "StTriggerDataMaker.h"
#include <stdlib.h>
//#include "StEventTypes.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StTRGReader.h"
#include "StDaqLib/TRG/trgStructures2003.h"
#include "StEvent/StTriggerData2003.h"
#include "StDaqLib/TRG/trgStructures2004.h"
#include "StEvent/StTriggerData2004.h"
#include "StDaqLib/TRG/trgStructures2005.h"
#include "StEvent/StTriggerData2005.h"
#include "StDaqLib/TRG/trgStructures2007.h"
#include "StEvent/StTriggerData2007.h"
#include "StDaqLib/TRG/trgStructures2008.h"
#include "StEvent/StTriggerData2008.h"
#include "StDaqLib/TRG/trgStructures2009.h"
#include "StEvent/StTriggerData2009.h"
#include "StDaqLib/TRG/trgStructures2012.h"
#include "StEvent/StTriggerData2012.h"
#include "StDaqLib/TRG/trgStructures2013.h"
#include "StEvent/StTriggerData2013.h"
#include "StDaqLib/TRG/trgStructures2016.h"
#include "StEvent/StTriggerData2016.h"
#include "StDaqLib/TRG/trgStructures2017.h"
#include "StEvent/StTriggerData2017.h"
#include "StDaqLib/TRG/trgStructures2018.h"
#include "StEvent/StTriggerData2018.h"
#include "StDaqLib/TRG/trgStructures2019.h"
#include "StEvent/StTriggerData2019.h"
#include "TFile.h"
#include "TH1.h"

ClassImp(StTriggerDataMaker)

//_____________________________________________________________________________

StTriggerDataMaker::StTriggerDataMaker(const char *name):StRTSBaseMaker("trg",name)
{
  LOG_INFO << "Constructing StTriggerDataMaker with name=" << name << endm;
  mDebug=0;
}

//_____________________________________________________________________________

Int_t StTriggerDataMaker::Make()
{
  LOG_DEBUG << "StTriggerDataMaker Make() starting..........Run=" 
        << GetRunNumber() << " : Event=" << GetEventNumber() << endm;

  int year=0, run=0;
  const TrgDataType2003    *trgdata2003=0;
  const TrgDataType2004    *trgdata2004=0;
  const TrgDataType2005    *trgdata2005=0;
  const TrgDataType2007    *trgdata2007=0;
  const TrgDataType2008    *trgdata2008=0;
  const TriggerDataBlk2009 *trgdata2009=0;
  const TriggerDataBlk2012 *trgdata2012=0;
  const TriggerDataBlk2013 *trgdata2013=0;
  const TriggerDataBlk2016 *trgdata2016=0;
  const TriggerDataBlk2017 *trgdata2017=0;
  const TriggerDataBlk2018 *trgdata2018=0;
  const TriggerDataBlk2019 *trgdata2019=0;

  St_DataSet* daqReaderDS = GetDataSet("StDAQReader");
  if (!daqReaderDS) return kStWarn;
  StDAQReader* daqReader = (StDAQReader*)(daqReaderDS->GetObject());
  if (!daqReader)   return kStWarn;

  run = daqReader->getRunNumber();

  StTRGReader* trgReader = daqReader->getTRGReader();
  if (trgReader  && ( year = trgReader->getYear()) ){ 
    
    LOG_INFO << "StTriggerDataMaker Make() found old data for year " << year << endm;  
    switch(year){
    case 2003:
      trgdata2003=trgReader->getDataType2003();
      if (!trgdata2003) return kStWarn;
      AddData(new TObjectSet("StTriggerData",new StTriggerData2003(trgdata2003,run),kTRUE));
      break;
    case 2004:
      trgdata2004=trgReader->getDataType2004();
      if (!trgdata2004) return kStWarn;
      AddData(new TObjectSet("StTriggerData",new StTriggerData2004(trgdata2004,run),kTRUE));
      break;
    case 2005:
      trgdata2005=trgReader->getDataType2005();
      if (!trgdata2005) return kStWarn;
      AddData(new TObjectSet("StTriggerData",new StTriggerData2005(trgdata2005,run),kTRUE));
      break;
    case 2007:
      trgdata2007=trgReader->getDataType2007();
      if (!trgdata2007) return kStWarn;
      AddData(new TObjectSet("StTriggerData",new StTriggerData2007(trgdata2007,run),kTRUE));
      break;
    case 2008:
      trgdata2008=trgReader->getDataType2008();
      if (!trgdata2008) return kStWarn;
      AddData(new TObjectSet("StTriggerData",new StTriggerData2008(trgdata2008,run),kTRUE));
      break;
    }
  } else {
    if(mDebug>1) LOG_INFO << "StTriggerDataMaker Make() found no old format data, trying to get new data format" << endm;

    StRtsTable *daqData = GetNextRaw();
    if ( daqData ){
      char* data = daqData->GetTable();
      if(data){
	char version = data[3];
	{ 
	  LOG_INFO << Form("StTriggerDataMaker Make() found new data formt with version = %02x%02x%02x%02x\n",
			   data[0],data[1],data[2],data[3]);
	}
	switch(version){
	case 0x40:
	  year=2009;
	  trgdata2009 = (TriggerDataBlk2009*)data;
	  AddData(new TObjectSet("StTriggerData",new StTriggerData2009(trgdata2009,run,1,mDebug),kTRUE));	
	  break;	
	case 0x41:
	  year=2012;
	  trgdata2012 = (TriggerDataBlk2012*)data;
	  AddData(new TObjectSet("StTriggerData",new StTriggerData2012(trgdata2012,run,1,mDebug),kTRUE));	
	  break;	
	case 0x42:
	  year=2013;
	  trgdata2013 = (TriggerDataBlk2013*)data;
	  AddData(new TObjectSet("StTriggerData",new StTriggerData2013(trgdata2013,run,1,mDebug),kTRUE));	
	  break;	
	case 0x43:
	  year=2016;
	  trgdata2016 = (TriggerDataBlk2016*)data;
	  AddData(new TObjectSet("StTriggerData",new StTriggerData2016(trgdata2016,run,1,mDebug),kTRUE));	
	  break;	
	case 0x44:
	  year=2017;
	  trgdata2017 = (TriggerDataBlk2017*)data;
	  AddData(new TObjectSet("StTriggerData",new StTriggerData2017(trgdata2017,run,1,mDebug),kTRUE));	
	  break;	
	case 0x45:
	  year=2018;
	  trgdata2018 = (TriggerDataBlk2018*)data;
	  AddData(new TObjectSet("StTriggerData",new StTriggerData2018(trgdata2018,run,1,mDebug),kTRUE));	
	  break;	
	case 0x46:
	  year=2019;
	  trgdata2019 = (TriggerDataBlk2019*)data;
	  AddData(new TObjectSet("StTriggerData",new StTriggerData2019(trgdata2019,run,1,mDebug),kTRUE));	
	  break;	
	default:
	  LOG_INFO << "StTriggerDataMaker Make() found new data but with unknown version = " << version << endm;
	}
      } else {
	LOG_INFO << "StTriggerDataMaker Make() found no new data format neither" << endm;  
      }      
    } else {
      LOG_WARN << "StTriggerDataMaker Make() - GetNextRaw() returned nothing" << endm;
    }
  }

  if(year==0){
    LOG_INFO << "StTriggerDataMaker Make() finished. Found no trigger data" << endm;  
    return kStWarn;
  }else{
    TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
    if (os) {
      StTriggerData* pTrg = (StTriggerData*)os->GetObject();
      if(pTrg){
	//if(mDebug>0) pTrg->dump();
	unsigned int err = pTrg->errorFlag();
	LOG_DEBUG << "StTriggerDataMaker Make() finished. Found trigger data for year "<< year <<" mErrorFlag="<<err<<endm;  
	if(err==0){
	  return kStOK;
	}else{
	  LOG_INFO << "StTriggerDataMaker Make() found fatal decording error "<< endm;	  
	  pTrg->setDebug(1);
	  if(year>=2009) pTrg->readData();
	  pTrg->setDebug(mDebug);
	  return kStWarn;
	}
      }    
    }
    LOG_INFO << "StTriggerDataMaker Make() finished. Failed to addData trigger data" << endm;  
    return kStWarn;
  }
}
