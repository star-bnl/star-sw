// 
// $Id: StEmcRawMaker.cxx,v 1.1 2004/10/18 18:20:07 suaide Exp $
// $Log: StEmcRawMaker.cxx,v $
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

#define STATUS_OK 1
#define CAP1 124
#define CAP2 125

ClassImp(StEmcRawMaker)

//_____________________________________________________________________________
/* 
   Default constructor. Set Initial values for some variables
*/
StEmcRawMaker::StEmcRawMaker(const char *name):StMaker(name)
{
  mPrint = kTRUE;
  mRun = 0;
  mDBRun = -1;
  mEvent = 0;
  mBemcRaw = new StBemcRaw();  
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
   Init function. Initializes the histograms and all other variables used by the
   program
*/
Int_t StEmcRawMaker::Init()
{     
  mBarrelNHitHist         = new TH2F("BarrelNHit","BarrelNHit",500,0.0,18000.0,4,0.5,4.5);            
  mBarrelEtotHist         = new TH2F("BarrelEtot","BarrelEtot",500,0.0,10000.0,4,0.5,4.5);           
  mBarrelAdcSumHist       = new TH2F("BarrelAdcSum","BarrelAdcSum",500,0.0,1000000.0,4,0.5,4.5);           
  mBarrelNCratesHist      = new TH2F("BarrelNCrates","BarrelNCrates",31,0.0,31.0,4,0.5,4.5);           
  mBarrelCrateStatusHist  = new TH2F("BarrelCrateStatus","BarrelCrateStatus",6,-0.5,5.5,30,0.5,30.5);;            
  
  return StMaker::Init();
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
  if(mPrint) cout <<"\n\nStEmcRawMaker::Make()******************************************************************\n";  
  prepareEnvironment();    
  makeBemc();
  fillHistograms();
  clock.Stop();
  if(mPrint) cout <<"Time to run StEmcRawMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
  if(mPrint) cout <<"*******************************************************************************************\n\n\n";

  return kStOK;
}
//_____________________________________________________________________________
/*!
  Prepare the environment for filling the EMC information 
*/
Bool_t StEmcRawMaker::prepareEnvironment()
{
  mDate = GetDate();
  mTime = GetTime();
  mEvent = 0;

  if(mPrint) cout <<"Get StEvent pointer and make it ready for filling"<<endl;
  ////////////////////////////////////////////////////////////
  // Get StEvent pointer and make it ready for filling
  //
  mEvent = (StEvent*)GetInputDS("StEvent");
  StEmcCollection *emc = NULL;
  
  if(mEvent) emc = mEvent->emcCollection();
  else
  {
    mEvent = new StEvent();
    AddData(mEvent);
    emc = mEvent->emcCollection();
  }
  if(!emc) 
  {
    emc = new StEmcCollection();
    mEvent->setEmcCollection(emc);
  }
  StEmcRawData *bemcRaw = emc->bemcRawData();
  if(!bemcRaw)
  {
    bemcRaw = new StEmcRawData();
    emc->setBemcRawData(bemcRaw);
  }
  //
  ////////////////////////////////////////////////////////////
  
  if(mRun==mDBRun) return kTRUE;
  
  if(mPrint) cout <<"Getting database tables for the BEMC detector \n";
  mDBRun = mRun;
  mBemcRaw->createDecoder(mDate,mTime);  	
  mBemcRaw->getTables()->loadTables((StMaker*)this);

  return kTRUE;  
}

//_____________________________________________________________________________
/*!
  make the barrel EMC
*/
Bool_t StEmcRawMaker::makeBemc()
{
  if(mPrint) cout <<"Copying EMC information from DAQ structure "<<endl;
  TDataSet* TheData   = GetDataSet("StDAQReader");
  if(!TheData)
  {
    if(mPrint) cout <<"Could not find DAQ Reader "<<endl;
    return kFALSE;
  }    
  StDAQReader* TheDataReader=(StDAQReader*)(TheData->GetObject());
  if(TheDataReader) mRun = TheDataReader->getRunNumber();
  mBemcRaw->setDate(mDate);
  return mBemcRaw->make(TheData,mEvent);
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
    for(Int_t crate = 1;crate<=30; crate++)
    {
      mBarrelCrateStatusHist->Fill(mBemcRaw->getCrateStatus(1,crate),crate);
    }
  }
}
