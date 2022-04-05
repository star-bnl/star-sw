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
#include "StEnumerations.h"


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
  mBemcData->towerMapBug(kTRUE); // corrects for tower map bug at analysis level by default
  mBemcData->psdMapBug2(kTRUE);
  mBemcData->smdMapBug(kTRUE);
  mBemcData->setCrateVeto(1);
  //status checking for all tables at analysis level by default
  StDetectorId bemcid = static_cast<StDetectorId>(kBarrelEmcTowerId);
  setCheckStatus(bemcid,1);
  StDetectorId bprsid = static_cast<StDetectorId>(kBarrelEmcPreShowerId);
  setCheckStatus(bprsid,1);
  StDetectorId bsmdeid = static_cast<StDetectorId>(kBarrelSmdEtaStripId);
  setCheckStatus(bsmdeid, 1);
  StDetectorId bsmdpid = static_cast<StDetectorId>(kBarrelSmdPhiStripId);
  setCheckStatus(bsmdpid, 1);
}
//_____________________________________________________________________________
/*!
  Default destructor
  */
StEmcADCtoEMaker::~StEmcADCtoEMaker()
{}
//_____________________________________________________________________________
/*!
  Init function. Initializes the histograms and all other variables used by the
  program
  */
Int_t StEmcADCtoEMaker::Init()
{
  mBemcData->initHisto();
  mBemcData->printConf();
  return StMaker::Init();
}
Int_t StEmcADCtoEMaker::InitRun(Int_t run)
{
  // Load DB and create decoder for the BEMC
  LOG_INFO <<"Getting database tables for the BEMC detector "<<endm;

  StMuDst* muDst = (StMuDst*)GetDataSet("MuDst");
  if(muDst && muDst->event())
	mBemcData->setProdVer(muDst->event()->runInfo().productionVersion().Data());
  else
	mBemcData->setProdVer("");
  LOG_INFO <<"Setting done prod version"<<endm;

  mBemcData->createDecoder(GetDate(),GetTime());
  mBemcData->getTables()->loadTables(this);
  LOG_INFO <<"loading table done"<<endm;

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
  LOG_DEBUG <<"StEmcADCtoEMaker::Make()******************************************************************"<<endm;
  mTestedCorruption = kFALSE;
  if(!prepareEnvironment())
	LOG_WARN <<"Could not prepare the proper environment"<<endm;
  if(!makeBemc())
	LOG_WARN <<"Could not make BEMC detector"<<endm;
  fillHistograms();
  clock.Stop();
  LOG_DEBUG <<"Time to run StEmcADCtoEMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<endm;
  LOG_DEBUG <<"*******************************************************************************************"<<endm;

  return kStOK;
}
//_____________________________________________________________________________
/*!
  Prepare the environment for filling the EMC information 
  */
Bool_t StEmcADCtoEMaker::prepareEnvironment()
{
  mEvent = 0;
  LOG_DEBUG <<"Get StEvent pointer and make it ready for filling"<<endm;
  ////////////////////////////////////////////////////////////
  // Get StEvent pointer and make it ready for filling
  //
  mEvent = (StEvent*)GetDataSet("StEvent");
  StEmcCollection *emc = NULL;
  mMyStEvent = kFALSE;

  if(mEvent)
	emc = mEvent->emcCollection();
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
  mBemcData->getTables()->loadTables(this);

  // Try DAQ First
  TDataSet* TheData   = GetDataSet("StDAQReader");
  if(TheData)
  {
	LOG_DEBUG <<"Copying EMC information from DAQ structure "<<endm;
	return mBemcData->make(TheData,mEvent);
  }
  // Try StEvent
  if(mEvent && !mMyStEvent)
  {
	StEmcCollection *emc = mEvent->emcCollection();
	if(emc)
	{
	  LOG_DEBUG <<"Copying EMC information from StEmcCollection "<<endm;
	  return mBemcData->make(emc,mEvent);
	}
  }

  // try MuDst as last option
  StMuDst* muDst = (StMuDst*)GetDataSet("MuDst");
  if(muDst)
  {
	StMuEmcCollection *muEmc = muDst->muEmcCollection();
	if(muEmc)
	{
	  LOG_DEBUG <<"Copying EMC information from StEmcCollection "<<endm;
	  Bool_t ok = mBemcData->make(muEmc,mEvent);
	  if(ok)
	  {
		StEmcCollection *emc = mEvent->emcCollection();
		muDst->setEmcCollection(emc);
	  }
	  else
		muDst->setEmcCollection(NULL);
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
  if(mBemcData)
	mBemcData->fillHisto();
}
void StEmcADCtoEMaker::setPrint(Bool_t a)
{
  LOG_INFO << "::setPrint() is obsolete.  Use logger config file to set verbosity instead." << endm;
}
void StEmcADCtoEMaker::printMap(Int_t detector,char*file)
{
  if(!mBemcData->getDecoder())
	return;
  ofstream f(file);
  if(detector==BTOW)
	mBemcData->getDecoder()->PrintTowerMap(&f);
  if(detector==BPRS)
	mBemcData->getDecoder()->PrintPsdMap(&f);
  if(detector==BSMDE)
	mBemcData->getDecoder()->PrintSmdMap(&f);
  if(detector==BSMDP)
	mBemcData->getDecoder()->PrintSmdMap(&f);
  f.close();
  return;
}
StEmcCollection*  StEmcADCtoEMaker::getEmcCollection()
{
  if(mEvent)
	return mEvent->emcCollection();
  return 0;
}
Bool_t  StEmcADCtoEMaker::isCorrupted()
{
  if(!mTestedCorruption)
	testCorruption();
  return mIsCorrupted;
}
void  StEmcADCtoEMaker::testCorruption()
{
  mIsCorrupted = kFALSE;
  mTestedCorruption = kTRUE;

  StEmcCollection *emc = mEvent->emcCollection();
  if(!emc)
	return;

  StEmcDetector* det = emc->detector(kBarrelEmcTowerId);
  if(!det)
  {
	mIsCorrupted = kTRUE;
	return;
  }

  Bool_t flagsAreOk = kFALSE;

  //-------------------------- Check for P04k and later productions (this is easy) ----
  for(int crate = 1; crate<=MAXCRATES; crate++)
  {
	StEmcCrateStatus crateStatus = det->crateStatus(crate);
	if (crateStatus==crateHeaderCorrupt)
	  mIsCorrupted = kTRUE;
	if (crateStatus==crateHeaderCorrupt || crateStatus==crateOK)
	  flagsAreOk = kTRUE;
  }
  if(flagsAreOk)
	return;

  //-------------------------- Check for pre P04k productions (this is not so easy) ---

  // If corrupt header is detected for a crate, then hits for all
  // modules hooked up to this crate are set to zero.
  // Then, StEmcAdcToEMaker doesn't save hits with ADC<=0 to StEvent
  // nModulesWithNoHits together with nModulesOff (if one is not
  // equal another) can be used for ghost pedestal removal.

  /*   turns out that we don't yet have status tables for pp data, so we need to take a different
	   tack.  Thorsten writes:

	   But normaly a large number of crates is corrupted (in FY04 Au+Au express
	   data I've seen never less than 4, also this is just based on a very
	   limited statistics, I just had a look at some log files...). So given
	   the 15 working crates in FY04 you could set a cut at 12, which leaves
	   enough room for failing crates and will still detect corruption.

*/
  StBemcTables* tables = this->getBemcData()->getTables();
  StEmcGeom* geom = StEmcGeom::instance("bemc");

  Int_t nModulesWithNoHits = 0;
  Int_t nModulesOff = 0;

  for(UInt_t imod = 1; imod <= det->numberOfModules(); ++imod)
  {
	StEmcModule* module = det->module(imod);
	Int_t NADCZero = 0;
	StSPtrVecEmcRawHit& hits = module->hits();
	for(UInt_t i = 0;i<hits.size();i++)
	  if(hits[i]->adc()==0)
		NADCZero++;
	if(NADCZero >=20)
	{
	  nModulesWithNoHits += 1;
	  // now find out whether this module is off in status tables
	  Int_t nTowersNoGood = 0;
	  for(Int_t eta = 1; eta<=20; eta++)
	  {
		for(Int_t sub = 1; sub<=2; sub++)
		{
		  Int_t tower_id;
		  geom->getId(imod, eta, sub, tower_id);
		  Int_t status;
		  tables->getStatus(BTOW, tower_id, status);
		  if(status!=1)
			nTowersNoGood += 1; // 0 is off, 1 is good, >1 is bad
		}
	  }
	  if(nTowersNoGood>=20)
		nModulesOff += 1;
	}
  }
  if(nModulesWithNoHits != nModulesOff)
	mIsCorrupted = kTRUE;
  return;
}

void StEmcADCtoEMaker::setCheckStatus(StDetectorId det, int flag, const char* option)
{
  mBemcData->setCheckStatus(det-kBarrelEmcTowerId, flag, option);
  return;
}
