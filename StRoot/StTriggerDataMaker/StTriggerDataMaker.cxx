#include "StTriggerDataMaker.h"
#include <stdlib.h>
#include "StEventTypes.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StTRGReader.h"
#include "StDaqLib/TRG/trgStructures2003.h"
#include "StEvent/StTriggerData2003.h"
#include "StDaqLib/TRG/trgStructures2004.h"
#include "StEvent/StTriggerData2004.h"
#include "TFile.h"
#include "TH1.h"

ClassImp(StTriggerDataMaker)

//_____________________________________________________________________________

StTriggerDataMaker::StTriggerDataMaker(const char *name):StMaker(name)
{
  cout << "Constructing StTriggerDataMaker with name=" << name << endl;
}

//_____________________________________________________________________________

Int_t StTriggerDataMaker::Make(){

  cout << "StTriggerDataMaker Make() starting..................................."  << endl;
  int year=0;
  St_DataSet* daqReaderDS = GetDataSet("StDAQReader");
  if (!daqReaderDS)				return kStWarn;
  StDAQReader* daqReader = (StDAQReader*)(daqReaderDS->GetObject());
  if (!daqReader  ) 				return kStWarn;
//VP  if (!(daqReader->TRGDetectorsPresent()))	return kStWarn;
  StTRGReader* trgReader = daqReader->getTRGReader();
  if (!trgReader) 				return kStWarn;
  year = trgReader->getYear();
  const TrgDataType2003 *trgdata2003=0;
  const TrgDataType2004 *trgdata2004=0;
  switch(year){
  case 2003:
    trgdata2003=trgReader->getDataType2003();
    if (!trgdata2003) return kStWarn;
    AddData(new TObjectSet("StTriggerData",new StTriggerData2003(trgdata2003),kTRUE));
    break;
  case 2004:
    trgdata2004=trgReader->getDataType2004();
    if (!trgdata2004) return kStWarn;
    AddData(new TObjectSet("StTriggerData",new StTriggerData2004(trgdata2004),kTRUE));
    break;
  }	      

  if(year==0){
    cout << "StTriggerDataMaker Make() finished. Found no trigger data" << endl;  
  }else{
    cout << "StTriggerDataMaker Make() finished. Found trigger data for year "<<year<< endl;  
  }
  return kStOK;
}
