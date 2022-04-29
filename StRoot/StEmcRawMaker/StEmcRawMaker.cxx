//
// $Id: StEmcRawMaker.cxx,v 1.24 2010/12/22 22:58:57 stevens4 Exp $

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
#include <StEmcUtil/database/StEmcDecoder.h>
#include <StMessMgr.h>

//EEMC ........
#include <StEEmcUtil/database/StEEmcDb.h>
#include <StEEmcUtil/database/EEmcDbCrate.h> // for print outs

#include "StEmcRawMaker.h"
#include "StEemcRaw.h"

ClassImp(StEmcRawMaker)

//_____________________________________________________________________________
/*
   Default constructor. Set Initial values for some variables
*/
StEmcRawMaker::StEmcRawMaker(const char *name):StRTSBaseMaker(name)
{
    m_Mode = 0;
    mEvent = 0;
    mBemcRaw = new StBemcRaw();
    // does not correct for tower map bug at production level by default
    // this assures consistency between the muDst/StEvent files and database
    mBemcRaw->towerMapBug(kFALSE);
    mBemcRaw->psdMapBug2(kFALSE);
    mBemcRaw->smdMapBug(kFALSE);

    mEemcRaw = new StEemcRaw();
    eeStDb=0;
}

//_____________________________________________________________________________
/*!
   Default destructor
*/
StEmcRawMaker::~StEmcRawMaker()
{}

//_____________________________________________________________________________
/*!
   Init function. Initializes the histograms and all other variables 
*/
Int_t StEmcRawMaker::Init()
{
    //................BEMC stuff ..............
    if(IAttr("BEmcCheckStatus")) {
        LOG_INFO << "StEmcRawMaker will suppress hits from towers with bad status" << endm;
        mBemcRaw->getControlTable()->CheckStatus[0] = 1;
    }

    TString EmcOpts = SAttr(".gopt.emc");
    if (EmcOpts.IsHex()) {
        Int_t optionMap[] = {0, 2, 4, 4, 1, 3, 5, 5};
        controlADCtoE_st* tab = mBemcRaw->getControlTable();
        for(Int_t i=0; i<MAXDETBARREL; i++) {
            Int_t optI;
            TString optIs = EmcOpts(optionMap[i],1);
            sscanf(optIs.Data(),"%x",&optI); // convert hex to decimal
            tab->CheckStatus[i] = (optI>>0) & 1;
            tab->CutOffType[i] = (optI>>1) & 1;
            // etc.
        }
    }

    mBemcRaw->initHisto();
    mBemcRaw->printConf();
    if((m_Mode&0x1)==1)
    {
		LOG_INFO << "Setting BEMC debug Mode -> save all hits into StEvent"<<endm;
        mBemcRaw->saveAllStEvent(kTRUE);
        mBemcRaw->initQAHisto();
    }
    //................EEMC stuff ..............
    eeStDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
    if(eeStDb==0)
    {
        LOG_ERROR << "FATAL !!! \n   did not found \"eeDb-maker\", all EEMC data will be ignored\n fix it, JB\n" <<endm;
    }

    mEemcRaw->initHisto(); // EEMC histos to monitor corruption
 
    if (IAttr(".histos"))
    {
        /* EEMC and BEMC  has no unneeded histos to skip during production,
           all monitor corruption, 
           require  ~1000 float bins in total, JB
        */
    }


    //never change control table after here
    assert(mBemcRaw->getControlTable()->CutOffType[2]==mBemcRaw->getControlTable()->CutOffType[3]);

    if(mBemcRaw->getControlTable()->CutOffType[2]){
      mBemcRaw->getControlTable()->DeductPedestal[2] = 0;
      mBemcRaw->getControlTable()->DeductPedestal[3] = 0;
    }
    if(mBemcRaw->getControlTable()->CutOffType[1]){
      mBemcRaw->getControlTable()->DeductPedestal[1] = 0;
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
    LOG_INFO <<"Getting database tables for the BEMC detector "<<endm;
    mBemcRaw->createDecoder(GetDate(),GetTime());
    mBemcRaw->getTables()->loadTables((StMaker*)this);

    //................EEMC stuff ..............

    LOG_INFO << "(" << runNumber << ")" << endm;

    if(eeStDb==0)
    {
        LOG_ERROR << "did not found \"eeDb-maker\", all EEMC data will be ignored\n\n"<<endm;
    }
    else if ( eeStDb->valid()==0 )
    {
        LOG_ERROR << "found \"eeDb-maker\", but without any DB data, all EEMC data will be ignored\n\n"<<endm;
        eeStDb=0;
    }
    else
    {// all is OK, no warning
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
//____________________________________
void StEmcRawMaker::setPrint(Bool_t a)
{
    LOG_INFO << "this function is obsolete.  Use logger config file to set verbosity instead." << endm;
}
//_____________________________________________________________________________
/*!
  Process the event. 
*/
Int_t StEmcRawMaker::Make()
{
    TStopwatch clock;
    clock.Start();
    LOG_DEBUG <<"StEmcRawMaker::Make()******************************************************************"<<endm;
    if(!prepareEnvironment())
	{ LOG_WARN <<"Could not prepare the environment to process the event "<<endm; }

    if(!makeBemc())
    { LOG_WARN <<"Could not process BEMC information properly "<<endm; }

    if(!makeEemc())
    { LOG_WARN <<"Could not process EEMC information properly "<<endm; }

    fillHistograms();
    clock.Stop();
    LOG_DEBUG <<"Time to run StEmcRawMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<endm;
    LOG_DEBUG <<"*******************************************************************************************"<<endm;

    //cleanup B+EmcRawCollection
    
     if( mEvent->id()%555 ) {// clear raw data from StEvent for most of events
      int i;
      StEmcRawData *eemcRaw = mEvent->emcCollection()->eemcRawData();
      for (i=0; i<eemcRaw->getNBlocks();i++) eemcRaw->deleteBank(i);
      StEmcRawData *bemcRaw = mEvent->emcCollection()->bemcRawData();
      for (i=0; i<bemcRaw->getNBlocks();i++) bemcRaw->deleteBank(i);
    }  else {
      LOG_INFO<< Form("B+EmcRawData are retained for eve=%d\n",mEvent->id())<<endm;
    }		      
    return kStOK;
}
//_____________________________________________________________________________
/*!
  Prepare the environment for filling the EMC information 
*/
Bool_t StEmcRawMaker::prepareEnvironment()
{
    mEvent = 0;

    LOG_DEBUG <<"Get StEvent pointer and make it ready for filling"<<endm;
    ////////////////////////////////////////////////////////////
    // Get StEvent pointer and make it ready for filling
    //
    mEvent = (StEvent*)GetInputDS("StEvent");
    StEmcCollection *emc = NULL;

    if(mEvent)
    {
        emc = mEvent->emcCollection();
    }
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
        LOG_DEBUG << "::prepareEnvironment() has added a non existing StEmcCollection()"<<endm;
    }

    StEmcRawData *bemcRaw = emc->bemcRawData();
    if(bemcRaw)
    {
        LOG_WARN << "::prepareEnvironment() found old StEmcRawData *bemcRaw. Will delete it and create a new one"<<endm;
        delete bemcRaw;
    }
    bemcRaw = new StEmcRawData();
    emc->setBemcRawData(bemcRaw);

    StEmcRawData *eemcRaw = emc->eemcRawData();
    if(eemcRaw)
    {
        LOG_FATAL << "::prepareEnvironment() found old StEmcRawData *eemcRaw, TOTAL failure,\n STOP this chain, JB"<<endm;
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
    LOG_DEBUG <<"Copying BEMC information from DAQ structure "<<endm;

    if(GetDate() < 20081101){
      TDataSet* TheData   = GetDataSet("StDAQReader");
      if(!TheData){
        LOG_ERROR <<"Could not find DAQ Reader "<<endm;
        return kFALSE;
      }
      mBemcRaw->setDate(GetDate());
      mBemcRaw->getTables()->loadTables((StMaker*)this);
      return mBemcRaw->make(TheData,mEvent);
   }
    mBemcRaw->setDate(GetDate());
    mBemcRaw->getTables()->loadTables((StMaker*)this);
    return mBemcRaw->make((StEmcRawMaker*)this,mEvent);
}

//_____________________________________________________________________________
/*!
  make the Endcap EMC
*/
Bool_t StEmcRawMaker::makeEemc()
{
    LOG_DEBUG <<"Copying EEMC information from daqReader->StEvent "<<endm;

    if(GetDate() < 20081101){
      St_DataSet *daq = GetDataSet("StDAQReader");

      if (! daq){
        LOG_ERROR << "::makeEemc() , StDAQReader not  available" << endm;
        return false;
      }

      StDAQReader *fromVictor = (StDAQReader*) (daq->GetObject());

      if (!fromVictor ){
        LOG_ERROR << "::makeEemc() , daq->GetObject() failed" << endm;
        return false;
      }

      StEEMCReader *eeReader  = fromVictor->getEEMCReader();
      if(!eeReader)
        return false ;

      if (! eeReader){
        LOG_ERROR << "::makeEemc() , fromVictor->getEEMCReader() failed" << endm;
        return false;
      }
      return mEemcRaw->make(eeReader,mEvent);
    }
    return mEemcRaw->make( (StEmcRawMaker*)this,mEvent ); //eeReader,eemcRaw, token);
}

//_____________________________________________________________________________
/*!
  Fill QA histograms
*/
void StEmcRawMaker::fillHistograms()
{
    if(mBemcRaw)
        mBemcRaw->fillHisto();
}

// $Log: StEmcRawMaker.cxx,v $
// Revision 1.24  2010/12/22 22:58:57  stevens4
// Patch for BSMDE mapping problem in P10ih and P10ij productions (RT #2043)
//
// Revision 1.23  2010/08/27 22:31:34  ogrebeny
// *** empty log message ***
//
// Revision 1.22  2009/11/17 15:55:48  mattheww
// fixed a bunch of warnings
//
// Revision 1.21  2009/02/11 22:38:56  mattheww
// fixed a bug in getting CAP
//
// Revision 1.20  2009/02/04 21:05:42  kocolosk
// Refactor StEEmcDb(Maker), new location for StEmcDecoder. Fixes RT #1388.
//
// Revision 1.19  2009/02/01 02:27:55  mattheww
// fixed behavior for older data
//
// Revision 1.18  2009/01/27 19:58:36  mattheww
// Updates to StEmcRawMaker to be compatible with 2009 DAQ Format
//
// Revision 1.17  2008/03/27 19:54:16  genevb
// Utilize new BFC option for GoptEMC for controlADCtoE table
//
// Revision 1.16  2007/12/26 17:47:35  kocolosk
// added support for "BEmcCheckStatus" attribute to suppress hot towers in fastOffline
//
// Revision 1.15  2007/09/10 22:21:42  kocolosk
// Support for new BPRS swap fixes (off by default for 06/07 production, on for analysis).
// StBemcTables now matches map fixes in case end users want to use this copy.
//
// Revision 1.14  2007/04/11 03:29:11  balewski
// undo hacks,
// Endcap code is now in default configuration
//
// Revision 1.13  2007/03/27 16:53:42  balewski
// disable RawEndcap hits filtering
//
// Revision 1.12  2007/01/22 19:13:37  kocolosk
// use STAR logger for all output
//
// Revision 1.11  2006/12/12 20:29:17  balewski
// added hooks for Endcap embedding
//
// Revision 1.10  2006/08/01 14:45:31  balewski
// clear EmcRawData for eveID%555!=0 to reduce footprint of StEvent, for B+E-EMC
//
// Revision 1.9  2006/01/16 11:12:00  suaide
// tower map bug fixed and astyle run
//
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
