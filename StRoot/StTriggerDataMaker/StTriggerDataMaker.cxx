#include "StTriggerDataMaker.h"
#include <stdlib.h>
#include "StEventTypes.h"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StTRGReader.h"
#include "trgStructures2003.h"
#include "StEvent/StTriggerData2003.h"
#include "TFile.h"
#include "TH1.h"

ClassImp(StTriggerDataMaker)

//_____________________________________________________________________________

StTriggerDataMaker::StTriggerDataMaker(const char *name):StMaker(name) {
  cout << "Constructing StTriggerDataMaker with name=" << name << endl;
}

//_____________________________________________________________________________

StTriggerDataMaker::~StTriggerDataMaker(){}

//_____________________________________________________________________________

Int_t StTriggerDataMaker::Init(){    
  return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StTriggerDataMaker::Make(){

  cout << "StTriggerDataMaker Make() starting..................................."  << endl;
  int year=0;
  mTrgData=0;
  St_DataSet* daqReaderDS = GetDataSet("StDAQReader");
  if(daqReaderDS) {
    StDAQReader* daqReader = (StDAQReader*)(daqReaderDS->GetObject());
    if(daqReader) {
      if (!(daqReader->TRGDetectorsPresent())) {
	StTRGReader* trgReader = daqReader->getTRGReader();
	if(trgReader){
	  TRG_Reader* reader = trgReader->fTRGImpReader;
	  if(reader){
	    char* trgdata = (char*) reader->pBankTRGD + 40;
	    year = reader->YearOfData(trgdata);
	    switch(year){
	    case 2003:
	      StTriggerData2003* trg2003 = new StTriggerData2003(trgdata);
	      mTrgData = (StTriggerData*) trg2003;
	      break;
	    }	      
	  }
	}
      }
    }
  }
  if(year==0){
    cout << "StTriggerDataMaker Make() finished. Found no trigger data" << endl;  
  }else{
    cout << "StTriggerDataMaker Make() finished. Found trigger data for year "<<year<< endl;  
  }
  return kStOK;
}

//_____________________________________________________________________________
Int_t StTriggerDataMaker::Finish(){
  // TFile theFile("fpd.root","RECREATE","fpdstudy");
  // theFile.cd();
  // nAdcHitHisto->Write();
  // nTdcHitHisto->Write();
  return StMaker::Finish();
}





