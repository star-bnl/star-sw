// 
// $Id: StEmcRawMaker.cxx,v 1.3 2004/10/19 23:48:49 suaide Exp $
// $Log: StEmcRawMaker.cxx,v $
// Revision 1.3  2004/10/19 23:48:49  suaide
// Initial implementation of the endcap detector done by Jan Balewski
//
// Revision 1.2  2004/10/19 17:53:00  suaide
// code clean up
//
// Revision 1.1  2004/10/18 18:20:07  suaide
// New Maker. Will replace StEmcADCtoEMaker in production.
// It reads only DAQ structures. Output is StEvent.
//
#include "StEmcRawMaker.h"
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

//EEMC ........
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEemcRaw.h"
//#include "StL0Trigger.h"

ClassImp(StEmcRawMaker)

//_____________________________________________________________________________
/* 
   Default constructor. Set Initial values for some variables
*/
StEmcRawMaker::StEmcRawMaker(const char *name):StMaker(name)
{
  mPrint = kTRUE;
  mEvent = 0;
  mBemcRaw = new StBemcRaw();  
  mEemcRaw = new StEemcRaw();  
  eeStDb=0;

}

//_____________________________________________________________________________
/*! 
   Default destructor
*/
StEmcRawMaker::~StEmcRawMaker()
{
}

//_____________________________________________________________________________
/*! 
   Init function. Initializes the histograms and all other variables 
*/
Int_t StEmcRawMaker::Init()
{     
  mBarrelNHitHist         = new TH2F("BarrelNHit","BarrelNHit",500,0.0,18000.0,4,0.5,4.5);            
  mBarrelEtotHist         = new TH2F("BarrelEtot","BarrelEtot",500,0.0,10000.0,4,0.5,4.5);           
  mBarrelAdcSumHist       = new TH2F("BarrelAdcSum","BarrelAdcSum",500,0.0,1000000.0,4,0.5,4.5);           
  mBarrelNCratesHist      = new TH2F("BarrelNCrates","BarrelNCrates",31,0.0,31.0,4,0.5,4.5);           
  mBarrelCrateStatusHist  = new TH2F("BarrelCrateStatus","BarrelCrateStatus",6,-0.5,5.5,30,0.5,30.5);;            

  //................EEMC stuff ..............
  eeStDb= (StEEmcDbMaker*) GetMaker("eeDb");
  
  if(eeStDb==0){
    gMessMgr->Message("","W") <<  GetName()<<"::Init(),  FATAL !!! \n   did not found \"eeDb-maker\", all EEMC data will be ignored\n fix it, JB\n" <<endm;
  }
  
  return StMaker::Init();
}

//____________________________________________________
//____________________________________________________
/*!
  Refresh DB info for new run
*/
Int_t StEmcRawMaker::InitRun(Int_t runNumber)
{         
  // Load DB and create decoder for the BEMC
  if(mPrint) gMessMgr->Info() <<"Getting database tables for the BEMC detector "<<endm;
  mBemcRaw->createDecoder(GetDate(),GetTime());  	
  mBemcRaw->getTables()->loadTables((StMaker*)this);

  //................EEMC stuff ..............

  gMessMgr->Message("","I") << GetName()<<"::InitRun("<< runNumber<<") list  DB content"<<endm;
  if(eeStDb==0){
    gMessMgr->Message("","W") << GetName()<<"::InitRun() did not found \"eeDb-maker\", all EEMC data will be ignored\n\n"<<endm;
  } else if ( eeStDb->valid()==0 ) {
    gMessMgr->Message("","W") << GetName()<<"::InitRun()  found \"eeDb-maker\", but without any DB data, all EEMC data will be ignored\n\n"<<endm;
    eeStDb=0;
  } else {// all is OK, no warning
    //eeStDb->exportAscii();
  }
  mEemcRaw->setDb(eeStDb);
  
  return StMaker::InitRun(runNumber);
}
//_____________________________________________________________________________
/*!
  This method creates mean ADC and RMS histograms. It runs only in the end of the job
*/
Int_t StEmcRawMaker::Finish()
{
  return kStOk;
}
//_____________________________________________________________________________
/*!
  Process the event. 
*/
Int_t StEmcRawMaker::Make()
{    
  TStopwatch clock;
  clock.Start();
  if(mPrint) gMessMgr->Info() <<"StEmcRawMaker::Make()******************************************************************"<<endm;  
  if(!prepareEnvironment()) if(mPrint) gMessMgr->Warning()<<"Could not prepare the environment to process the event "<<endm;    
  printf("JB start BTowRaw\n");
  if(!makeBemc()) if(mPrint) gMessMgr->Warning()<<"Could not process BEMC information properly "<<endm;
  printf("JB start ETowRaw\n");
  if(!makeEemc())  gMessMgr->Warning()<<"Could not process EEMC information properly "<<endm;
  printf("JB stop Raw\n");
  fillHistograms();
  clock.Stop();
  if(mPrint) gMessMgr->Info() <<"Time to run StEmcRawMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<endm;
  if(mPrint) gMessMgr->Info() <<"*******************************************************************************************"<<endm;

  return kStOK;
}
//_____________________________________________________________________________
/*!
  Prepare the environment for filling the EMC information 
*/
Bool_t StEmcRawMaker::prepareEnvironment()
{
  mEvent = 0;

  if(mPrint) gMessMgr->Info() <<"Get StEvent pointer and make it ready for filling"<<endm;
  ////////////////////////////////////////////////////////////
  // Get StEvent pointer and make it ready for filling
  //
  mEvent = (StEvent*)GetInputDS("StEvent");
  StEmcCollection *emc = NULL;

  if(mEvent) {
    emc = mEvent->emcCollection();
  } else  {
    mEvent = new StEvent();
    AddData(mEvent);
    emc = mEvent->emcCollection();
  }

  if(!emc) 
  {
    emc = new StEmcCollection();
    mEvent->setEmcCollection(emc);
    gMessMgr->Message("","D") << GetName()<<"::prepareEnvironment() has added a non existing StEmcCollection()"<<endm;
  }

  StEmcRawData *bemcRaw = emc->bemcRawData();
  if(!bemcRaw)
  {
    bemcRaw = new StEmcRawData();
    emc->setBemcRawData(bemcRaw);
  }

  StEmcRawData *eemcRaw = emc->eemcRawData();
  if(eemcRaw)  {
    gMessMgr->Message("","W") << GetName()<<"::prepareEnvironment() found old StEmcRawData *eemcRaw, TOTAL failure,\n STOP this chain, JB"<<endm;
    delete eemcRaw;
  }
  eemcRaw = new StEmcRawData(); // always create new collection for EEMC
  emc->setEemcRawData(eemcRaw);
  

  //
  ////////////////////////////////////////////////////////////
  
  return kTRUE;  
}

//_____________________________________________________________________________
/*!
  make the barrel EMC
*/
Bool_t StEmcRawMaker::makeBemc()
{
  if(mPrint) gMessMgr->Info() <<"Copying EMC information from DAQ structure "<<endm;
  TDataSet* TheData   = GetDataSet("StDAQReader");
  if(!TheData)
  {
    if(mPrint) gMessMgr->Warning() <<"Could not find DAQ Reader "<<endm;
    return kFALSE;
  }    
  mBemcRaw->setDate(GetDate());
  return mBemcRaw->make(TheData,mEvent);
}

//_____________________________________________________________________________
/*!
  make the Endcap EMC
*/
Bool_t StEmcRawMaker::makeEemc()
{
  gMessMgr->Message("","D") << GetName()<<"::makeEemc() DO: daqReader->StEvent"<<endm;

  St_DataSet *daq = GetDataSet("StDAQReader");

  if (! daq) {
    gMessMgr->Message("","W") << GetName()<<"::makeEemc() , StDAQReader not  available" << endm;
    return false;
  }

  StDAQReader *fromVictor = (StDAQReader*) (daq->GetObject());

  if (!fromVictor ) {
    gMessMgr->Message("","W") <<  GetName()<<"::makeEemc() , daq->GetObject() failed" << endm;
    return false;
  }

  StEEMCReader *eeReader  = fromVictor->getEEMCReader();
  if(!eeReader) return false ;

  if (! eeReader) {
    gMessMgr->Message("","W") <<  GetName()<<"::makeEemc() , fromVictor->getEEMCReader() failed" << endm;
    return false;
  }

 

  return mEemcRaw->make( eeReader,mEvent ); //eeReader,eemcRaw, token);
}

//_____________________________________________________________________________
/*!
  Fill QA histograms
*/
void StEmcRawMaker::fillHistograms()
{
  if(mBemcRaw)
  {
    for(Int_t det = 1;det<=MAXDETBARREL; det++)
    {
      mBarrelNHitHist->Fill(mBemcRaw->getTotalSaved(det),det);
      mBarrelEtotHist->Fill(mBemcRaw->getTotalE(det),det);
      mBarrelAdcSumHist->Fill(mBemcRaw->getTotalADC(det),det);
      mBarrelNCratesHist->Fill(mBemcRaw->getNCratesOK(det),det);
    }
    for(Int_t crate = 1;crate<=MAXCRATES; crate++)
    {
      mBarrelCrateStatusHist->Fill(mBemcRaw->getCrateStatus(BTOW,crate),crate);
    }
  }
}
