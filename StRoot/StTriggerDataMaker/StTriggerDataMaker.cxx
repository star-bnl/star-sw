#include "StTriggerDataMaker.h"
#include <stdlib.h>
#include "StEventTypes.h"
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
#include "TFile.h"
#include "TH1.h"

ClassImp(StTriggerDataMaker)

//_____________________________________________________________________________

StTriggerDataMaker::StTriggerDataMaker(const char *name):StRTSBaseMaker("trg",name)
{
  LOG_INFO << "Constructing StTriggerDataMaker with name=" << name << endm;
}

//_____________________________________________________________________________

Int_t StTriggerDataMaker::Make(){
  cout << "StTriggerDataMaker Make() starting..................................."  << endl;

  int year=0, run=0;
  const TrgDataType2003    *trgdata2003=0;
  const TrgDataType2004    *trgdata2004=0;
  const TrgDataType2005    *trgdata2005=0;
  const TrgDataType2007    *trgdata2007=0;
  const TrgDataType2008    *trgdata2008=0;
  const TriggerDataBlk2009 *trgdata2009=0;

  St_DataSet* daqReaderDS = GetDataSet("StDAQReader");
  if (!daqReaderDS) return kStWarn;
  StDAQReader* daqReader = (StDAQReader*)(daqReaderDS->GetObject());
  if (!daqReader)   return kStWarn;

  run = daqReader->getRunNumber();

  StTRGReader* trgReader = daqReader->getTRGReader();
  if (trgReader){ 
    year = trgReader->getYear();
    cout << "StTriggerDataMaker Make() found old data for year " << year << endl;  
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
  }
  if (!(trgReader && year) ){
    LOG_INFO << "StTriggerDataMaker Make() found no old format data, trying to get new data format" << endm;

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
	  AddData(new TObjectSet("StTriggerData",new StTriggerData2009(trgdata2009,run),kTRUE));	
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
    LOG_INFO << "StTriggerDataMaker Make() finished. Found trigger data for year "<< year << endm;  
    return kStOK;
  }
}
