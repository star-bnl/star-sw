#include "StTriggerDataMaker.h"
#include <stdlib.h>
#include "StEventTypes.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StTRGReader.h"
#include "StDaqLib/TRG/trgStructures2003.h"
#include "StEvent/StTriggerData2003.h"
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
  switch(year){
    case 2003:{
      const TrgDataType2003 *trgdata=trgReader->getDataType2003();
      if (!trgdata) 				return kStWarn;
      AddData(new TObjectSet("StTriggerData",new StTriggerData2003(trgdata),kTRUE));
      }
      break;
  }	      

  if(year==0){
    cout << "StTriggerDataMaker Make() finished. Found no trigger data" << endl;  
  }else{
    cout << "StTriggerDataMaker Make() finished. Found trigger data for year "<<year<< endl;  
  }
  return kStOK;
}
