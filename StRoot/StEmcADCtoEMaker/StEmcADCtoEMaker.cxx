#include "StEmcADCtoEMaker.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "Stiostream.h"
#include <math.h>
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "TStopwatch.h"
#include "TString.h"
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"


ClassImp(StEmcADCtoEMaker)

//_____________________________________________________________________________
/* 
   Default constructor. Set Initial values for some variables
*/
StEmcADCtoEMaker::StEmcADCtoEMaker(const char *name):StMaker(name)
{
  mEvent = 0;
  mEmbed = kFALSE;
  mBemcData = new StBemcData();  
}
//_____________________________________________________________________________
/*! 
   Default destructor
*/
StEmcADCtoEMaker::~StEmcADCtoEMaker()
{
}
//_____________________________________________________________________________
/*! 
   Init function. Initializes the histograms and all other variables used by the
   program
*/
Int_t StEmcADCtoEMaker::Init()
{     
  mBemcData->initHisto();
  return StMaker::Init();
}
Int_t StEmcADCtoEMaker::InitRun(Int_t run)
{
  // Load DB and create decoder for the BEMC
  gMessMgr->Info() <<"Getting database tables for the BEMC detector "<<endm;
  mBemcData->createDecoder(GetDate(),GetTime());
  mBemcData->getTables()->loadTables((StMaker*)this);

  return StMaker::InitRun(run);
}
//_____________________________________________________________________________
/*!
  This method creates mean ADC and RMS histograms. It runs only in the end of the job
*/
Int_t StEmcADCtoEMaker::Finish()
{
  return kStOk;
}
//_____________________________________________________________________________
/*!
  Process the event. 
*/
Int_t StEmcADCtoEMaker::Make()
{    
  TStopwatch clock;
  clock.Start();
  gMessMgr->Info() <<"StEmcADCtoEMaker::Make()******************************************************************"<<endm;  
  if(!prepareEnvironment()) gMessMgr->Warning()<<"Could not prepare the proper environment"<<endm;    
  if(!makeBemc()) gMessMgr->Warning()<<"Could not make BEMC detector"<<endm;
  fillHistograms();
  clock.Stop();
  gMessMgr->Info() <<"Time to run StEmcADCtoEMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<endm;
  gMessMgr->Info() <<"*******************************************************************************************"<<endm;

  return kStOK;
}
//_____________________________________________________________________________
/*!
  Prepare the environment for filling the EMC information 
*/
Bool_t StEmcADCtoEMaker::prepareEnvironment()
{
  mEvent = 0;
  gMessMgr->Info() <<"Get StEvent pointer and make it ready for filling"<<endm;
  ////////////////////////////////////////////////////////////
  // Get StEvent pointer and make it ready for filling
  //
  mEvent = (StEvent*)GetInputDS("StEvent");
  StEmcCollection *emc = NULL;
  mMyStEvent = kFALSE;
  
  if(mEvent) emc = mEvent->emcCollection();
  else
  {
    mEvent = new StEvent();
    AddData(mEvent);
    emc = mEvent->emcCollection();
    mMyStEvent = kTRUE;
  }
  if(!emc) 
  {
    emc = new StEmcCollection();
    mEvent->setEmcCollection(emc);
  }
  if(mMyStEvent)
  {
    StEmcRawData *BemcData = emc->bemcRawData();
    if(!BemcData)
    {
      BemcData = new StEmcRawData();
      emc->setBemcRawData(BemcData);
    }
  }
  //
  ////////////////////////////////////////////////////////////
  
  return kTRUE;  
}

//_____________________________________________________________________________
/*!
  make the barrel EMC
*/
Bool_t StEmcADCtoEMaker::makeBemc()
{
  mBemcData->setDate(GetDate());
  
  // Try DAQ First  
  TDataSet* TheData   = GetDataSet("StDAQReader");
  if(TheData) 
  {
    gMessMgr->Info() <<"Copying EMC information from DAQ structure "<<endm;
    return mBemcData->make(TheData,mEvent);
  }
  // Try StEvent
  if(mEvent && !mMyStEvent)
  {
    StEmcCollection *emc = mEvent->emcCollection();
    if(emc)
    {
      if(!mEmbed) // try StEmcRawData
      {
        StEmcRawData *BemcData = emc->bemcRawData();
        gMessMgr->Info() <<"Copying EMC information from StEmcRadData "<<endm;
        if(BemcData) return mBemcData->make(BemcData,mEvent);
      }
      gMessMgr->Info() <<"Copying EMC information from StEmcCollection "<<endm;      
      return mBemcData->make(emc,mEvent);
    }
  }
  
  // try MuDst as last option
  StMuDst* muDst = (StMuDst*)GetInputDS("MuDst");
  if(muDst)
  {
    StMuEmcCollection *muEmc = muDst->muEmcCollection();
    if(muEmc)       
    {
      gMessMgr->Info() <<"Copying EMC information from StEmcCollection "<<endm;      
      Bool_t ok = mBemcData->make(muEmc,mEvent);
      if(ok) 
      {
        StEmcCollection *emc = mEvent->emcCollection();
        muDst->setEmcCollection(emc);
      } else muDst->setEmcCollection(NULL);
      return ok;
    }
  }
  return kFALSE;
}
//_____________________________________________________________________________
/*!
  Fill QA histograms
*/
void StEmcADCtoEMaker::fillHistograms()
{
  if(mBemcData) mBemcData->fillHisto();
}
void StEmcADCtoEMaker::setPrint(Bool_t a)
{
  if(a)
  {
     gMessMgr->SwitchOn("D");
     gMessMgr->SwitchOn("I");
     gMessMgr->SwitchOn("W");
     gMessMgr->SwitchOn("E");
  }
  else
  {
     gMessMgr->SwitchOff("D");
     gMessMgr->SwitchOff("I");
     gMessMgr->SwitchOff("W");
     gMessMgr->SwitchOff("E");
  }
}
void StEmcADCtoEMaker::printMap(Int_t detector,char*file)
{
    if(!mBemcData->getDecoder()) return;
    ofstream f(file);
    if(detector==BTOW)  mBemcData->getDecoder()->PrintTowerMap(&f);
    if(detector==BPRS)  mBemcData->getDecoder()->PrintPsdMap(&f);
    if(detector==BSMDE) mBemcData->getDecoder()->PrintSmdMap(&f);
    if(detector==BSMDP) mBemcData->getDecoder()->PrintSmdMap(&f);
    f.close();
    return;
}
StEmcCollection*  StEmcADCtoEMaker::getEmcCollection()  
{ 
  if(mEvent) return mEvent->emcCollection(); 
  return 0;
} 
