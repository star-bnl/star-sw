// 
// $Id: StEmcRawMaker.cxx,v 1.8 2004/12/14 11:32:11 suaide Exp $

#include <math.h>

#include <StEventTypes.h>
#include <StEvent.h>
#include <TStopwatch.h>
#include <TString.h>

#include <StEmcUtil/others/emcDetectorName.h>
#include <StEmcUtil/geometry/StEmcGeom.h>

// DAQ Libraries
#include <StDaqLib/GENERIC/EventReader.hh>
#include <StDaqLib/EMC/EMC_Reader.hh>
#include <StDAQMaker/StDAQReader.h>
#include <StDaqLib/EMC/StEmcDecoder.h>
#include <StMessMgr.h>

//EEMC ........
#include <StEEmcDbMaker/StEEmcDbMaker.h>
#include <StEEmcDbMaker/EEmcDbCrate.h> // for print outs

#include "StEmcRawMaker.h"
#include "StEemcRaw.h"

ClassImp(StEmcRawMaker)

//_____________________________________________________________________________
/* 
   Default constructor. Set Initial values for some variables
*/
StEmcRawMaker::StEmcRawMaker(const char *name):StMaker(name)
{
  m_Mode = 0;
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
  //................BEMC stuff ..............
  mBemcRaw->initHisto();
  mBemcRaw->printConf();
  if(m_Mode&0x1==1) 
  {
    gMessMgr->Info()<<"Setting BEMC debug Mode -> save all hits into StEvent"<<endm;
    mBemcRaw->saveAllStEvent(kTRUE);
    mBemcRaw->initQAHisto();
  }
  //................EEMC stuff ..............
  eeStDb= (StEEmcDbMaker*) GetMaker("eeDb");
  
  if(eeStDb==0){
    gMessMgr->Message("","W") <<  GetName()<<"::Init(),  FATAL !!! \n   did not found \"eeDb-maker\", all EEMC data will be ignored\n fix it, JB\n" <<endm;
  }

  mEemcRaw->initHisto(); // EEMC histos to monitor corruption

  if (IAttr(".histos")) {
    /* EEMC and BEMC  has no unneeded histos to skip during production,
       all monitor corruption, 
       require  ~1000 float bins in total, JB
    */
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
  gMessMgr->Info() <<"Getting database tables for the BEMC detector "<<endm;
  mBemcRaw->createDecoder(GetDate(),GetTime());  	
  mBemcRaw->getTables()->loadTables((StMaker*)this);

  //................EEMC stuff ..............

  gMessMgr->Message("","I") << GetName()<<"::InitRun("<< runNumber<<")" <<endm;
  if(eeStDb==0){
    gMessMgr->Message("","W") << GetName()<<"::InitRun() did not found \"eeDb-maker\", all EEMC data will be ignored\n\n"<<endm;
  } else if ( eeStDb->valid()==0 ) {
    gMessMgr->Message("","W") << GetName()<<"::InitRun()  found \"eeDb-maker\", but without any DB data, all EEMC data will be ignored\n\n"<<endm;
    eeStDb=0;
  } else {// all is OK, no warning
    //eeStDb->exportAscii();
  }
  mEemcRaw->setDb(eeStDb);
  
  if(eeStDb)
  {
    int icr;
    for(icr=0;icr<eeStDb->getNFiber();icr++) {
      const EEmcDbCrate *fiber=eeStDb-> getFiber(icr);
      printf(" eemcDB : ");fiber->print();
    }
  }

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
//____________________________________
void StEmcRawMaker::setPrint(Bool_t a) 
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
//_____________________________________________________________________________
/*!
  Process the event. 
*/
Int_t StEmcRawMaker::Make()
{    
  TStopwatch clock;
  clock.Start();
  gMessMgr->Info() <<"StEmcRawMaker::Make()******************************************************************"<<endm;  
  if(!prepareEnvironment())  gMessMgr->Warning()<<"Could not prepare the environment to process the event "<<endm;    
  
  if(!makeBemc())  gMessMgr->Warning()<<"Could not process BEMC information properly "<<endm;

  if(!makeEemc())  gMessMgr->Warning()<<"Could not process EEMC information properly "<<endm;

  fillHistograms();
  clock.Stop();
  gMessMgr->Info() <<"Time to run StEmcRawMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<endm;
  gMessMgr->Info() <<"*******************************************************************************************"<<endm;

  return kStOK;
}
//_____________________________________________________________________________
/*!
  Prepare the environment for filling the EMC information 
*/
Bool_t StEmcRawMaker::prepareEnvironment()
{
  mEvent = 0;

  gMessMgr->Info() <<"Get StEvent pointer and make it ready for filling"<<endm;
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
  if(bemcRaw)
  {
    gMessMgr->Message("","W") << GetName()<<"::prepareEnvironment() found old StEmcRawData *bemcRaw. Will delete it and create a new one"<<endm;
    delete bemcRaw;
  }
  bemcRaw = new StEmcRawData();
  emc->setBemcRawData(bemcRaw);

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
  gMessMgr->Info() <<"Copying BEMC information from DAQ structure "<<endm;
  TDataSet* TheData   = GetDataSet("StDAQReader");
  if(!TheData)
  {
    gMessMgr->Warning() <<"Could not find DAQ Reader "<<endm;
    return kFALSE;
  }    
  mBemcRaw->setDate(GetDate());
  mBemcRaw->getTables()->loadTables((StMaker*)this);
  return mBemcRaw->make(TheData,mEvent);
}

//_____________________________________________________________________________
/*!
  make the Endcap EMC
*/
Bool_t StEmcRawMaker::makeEemc()
{
  gMessMgr->Info() <<"Copying EEMC information from daqReader->StEvent "<<endm;
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
  if(mBemcRaw) mBemcRaw->fillHisto();
}

// $Log: StEmcRawMaker.cxx,v $
// Revision 1.8  2004/12/14 11:32:11  suaide
// added histograms for status tables creation
//
// Revision 1.7  2004/11/24 00:12:19  suaide
// added check in for m_Mode&0x1==1 for bemcDebug flag in bfc
//
// Revision 1.6  2004/11/22 12:46:22  suaide
// added new flags for hit reconstruction. Status are not checked
// dureing production anymore in order to avoid bad status loaded in
// DB
//
// Revision 1.5  2004/11/02 03:23:09  suaide
// small changes in order to fix a bug
//
// Revision 1.4  2004/10/21 00:01:50  suaide
// small changes in histogramming and messages for BEMC
// Complete version for EEMC done by Jan Balewski
//
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
