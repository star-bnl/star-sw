/***************************************************************************
 *
 * $Id: StFpdMaker.cxx,v 1.4 2005/11/26 01:27:00 perev Exp $
 *
 * Author: AKIO OGAWA
 *
 ***************************************************************************
 *
 * Description: FPD offline software: maker to insert FPDp raw data onto StEvent
 *
 ***************************************************************************
 *
 * $Log: StFpdMaker.cxx,v $
 * Revision 1.4  2005/11/26 01:27:00  perev
 * created StEvent added to data. LeakOff
 *
 * Revision 1.3  2003/07/18 18:31:47  perev
 * test for nonexistance of XXXReader added
 *
 * Revision 1.2  2002/10/17 02:03:10  akio
 * modification along with StEvent modification for 2003
 *
 * Revision 1.1  2002/01/16 20:21:07  akio
 * first version
 *
 * 
 **************************************************************************/

#include "StFpdMaker.h"
#include <stdlib.h>
#include "StEventTypes.h"
//VP#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/FPD/FPD_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "TFile.h"
#include "TH1.h"

ClassImp(StFpdMaker)

//_____________________________________________________________________________

StFpdMaker::StFpdMaker(const char *name):StMaker(name) {}

//_____________________________________________________________________________

StFpdMaker::~StFpdMaker(){}

//_____________________________________________________________________________

Int_t StFpdMaker::Init(){    
  return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StFpdMaker::Make(){

#ifdef FPDP_DEBUG
  cout << "===========FPDP_DEBUG=========================================" << endl;
#endif
  //cout << "StFpdMaker Make() starting..................................."  << endl;

  StFpdCollection* fpdCollection;
  //check for existence of collections
  mEvent = (StEvent *) GetInputDS("StEvent");
  if(!mEvent){
    //create own StEvent
    cout << "StFpdMaker found no StEvent... Create local StEvent" << endl;
    mEvent = new StEvent;
    AddData(mEvent);
  }
  //check fpdCollection in StEvent.
  //If not there, create it and set 
  //cout << "StFpdMaker Checking for Fpd collection in StEvent..." << endl;
  fpdCollection = mEvent->fpdCollection();
  if(fpdCollection != 0) {
    //cout << "StFpdMaker fpdCollection Exists" << endl;
  } else {
    cout << "StFpdMaker fpdCollection does not exist" << endl;
    cout << "StFpdMaker creating fpdCollection in StEvent ... ";
    fpdCollection = new StFpdCollection();
    mEvent->setFpdCollection(fpdCollection);
    fpdCollection = mEvent->fpdCollection();
    if (fpdCollection) cout << "OK"<< endl;
    else{
      cout << "FAILED" << endl;
      return kStWarn;
    }
  }
  
  //check bbcTriggerDetector in StEvent->triggerDetectorCollection. 
  //If not there, create it and set 
  //cout << "StFpdMaker Checking for TriggerDetector collection in StEvent..." << endl;
  StTriggerDetectorCollection* trg = mEvent->triggerDetectorCollection();
  if(trg == 0){
    cout << "StFpdMaker found no StTriggerDetectorCollection... Create and add" << endl;
    trg = new StTriggerDetectorCollection;
    mEvent->setTriggerDetectorCollection(trg);
  }
  StBbcTriggerDetector& bbcTriggerDetector = trg->bbc();

  // Get Daq reader
  cout << "StFapdMaker getting data from FPDDAQ Reader ... " << endl;
  St_DataSet* daqReaderDS = GetDataSet("StDAQReader");
  if (!daqReaderDS) {
    cout << "StFpdMaker No StDAQReader dataset. Event skipped" << endl;
    return kStWarn;
  }
  StDAQReader* daqReader = (StDAQReader*)(daqReaderDS->GetObject()); 
  if (!daqReader) {
      cout << "StFpdMaker No StDAQReader object. Event skipped" << endl;
      return kStWarn;
  }
  if (!(daqReader->FPDPresent())) {
    cout << "StFpdMaker FPD is not in datastream. Event skipped" << endl;
    return kStWarn;
  }
  StFpdReaderInterface* fpdReader = daqReader->getFPDReader();
  if (!fpdReader){
    cout << "StFpdMaker Failed to getFpdReader()...." << endl;
  }
  //  fpdReader->printRawData();

  //copy daq data to FPD and BBC collections
  //cout << "StFpdMaker Unpacking data..." << endl;
  fpdCollection->setToken(fpdReader->GetEventNumber());
  for (unsigned int i=0;i<fpdCollection->numberOfADC();i++){
    fpdCollection->setAdc(i,fpdReader->GetAdc(i));
  }
  for (unsigned int i=0;i<fpdCollection->numberOfTDC();i++){
    fpdCollection->setTdc(i,fpdReader->GetTdc(i));
  }
  for (unsigned int i=0;i<fpdCollection->numberOfRegisters();i++){
    fpdCollection->setRegister(i,fpdReader->GetReg(i));
  }
  for (unsigned int i=0;i<fpdCollection->numberOfPedestal();i++){
    fpdCollection->setPedestal(i,fpdReader->GetPed(i));
  }
  for (unsigned int i=0;i<fpdCollection->numberOfScalers();i++){
    fpdCollection->setScaler(i,fpdReader->GetScl(i));
  }
  //  for (unsigned int i=0;i<bbcTriggerDetector.numberOfPMTs();i++){
  for (unsigned int i=0;i<32;i++){
    bbcTriggerDetector.setAdc(i,fpdReader->GetBbcAdc(i));
    bbcTriggerDetector.setTdc(i,fpdReader->GetBbcAdc(i+32));
  }
  for (unsigned int i=0;i<bbcTriggerDetector.numberOfRegisters();i++){
    bbcTriggerDetector.setRegister(i,fpdReader->GetBbcAdc(i+64));
  }
  for (unsigned int i=0;i<bbcTriggerDetector.numberOfPedestalData();i++){
    bbcTriggerDetector.setPedestal(i,fpdReader->GetBbcPed(i));
  }
  for (unsigned int i=0;i<bbcTriggerDetector.numberOfScalars();i++){
    bbcTriggerDetector.setScalar(i,fpdReader->GetBbcScl(i));
  }
  //cout << "StFpdMaker Make() finished..................................." << endl;
  //fpdCollection->dump();
  //bbcTriggerDetector.dump();
  return kStOK;
}

//_____________________________________________________________________________
Int_t StFpdMaker::Finish(){
  // TFile theFile("fpd.root","RECREATE","fpdstudy");
  // theFile.cd();
  // nAdcHitHisto->Write();
  // nTdcHitHisto->Write();
  return StMaker::Finish();
}
