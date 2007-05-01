#include "StPi0TriggerSimulatorMaker.h"

#include <St_base/StMessMgr.h>
#include <StEvent/StEvent.h>
#include <StEvent/StEventTypes.h>
#include <StEvent/StTriggerId.h>
#include <StEvent/StTriggerDetectorCollection.h>
#include <StEvent/StEmcTriggerDetector.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StEmcUtil/projection/StEmcPosition.h>
#include <StDaqLib/EMC/StEmcDecoder.h>
#include <tables/St_emcStatus_Table.h>
#include <tables/St_smdStatus_Table.h>
#include <St_db_Maker/St_db_Maker.h>
#include <StMcEventTypes.hh>
#include <StMcEvent.hh>
#include <StMcVertex.hh>
#include <StMcTrack.hh>
#include <StMcEventMaker/StMcEventMaker.h>
#include <StEmcADCtoEMaker/StEmcADCtoEMaker.h>
#include <StEmcADCtoEMaker/StBemcData.h>
#include <StDetectorDbMaker/StDetectorDbTriggerID.h>
#include <StEmcUtil/database/StBemcTables.h>
#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

#include <StEmcPool/StPi0DataMaker/StPi0DataMakerUtil.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(StPi0TriggerSimulatorMaker);

//_____________________________________________________________________________
StPi0TriggerSimulatorMaker::StPi0TriggerSimulatorMaker(const Char_t *name)
    : inherited(name)
    , datasetNameStEvent("StEvent")
    , isSimulation(false)
    , HT1Threshold(5)
    , HT2Threshold(10)
    , TriggerADC(32)
    , triggersHT1(0)
    , triggersHT2(0)
    , useFullEmcTriggerSimulator(false)
    , triggerFullSimulatorName("")
    , doTowerSwapFix(true)
    , mData(0)
    , mEmcGeom(0)
    , mEmcDecoder(0)
    , mEmcTables(0)
{
}

//_____________________________________________________________________________
StPi0TriggerSimulatorMaker::~StPi0TriggerSimulatorMaker() {
}

//_____________________________________________________________________________
Int_t StPi0TriggerSimulatorMaker::Init() {
  {LOG_DEBUG << "Starting Init()" << endm;}
  Int_t result = this->inherited::Init();
  this->mData = new TMyTriggerSimulatedData();
  this->mEmcGeom = new StEmcGeom("bemc");
  this->mEmcDecoder = new StEmcDecoder();
  this->mEmcTables = new StBemcTables();
  {LOG_DEBUG << "Finished Init()" << endm;}
  return result;
}

//_____________________________________________________________________________
Int_t StPi0TriggerSimulatorMaker::Make() {
    {LOG_DEBUG << "Starting Make()" << endm;}
    Int_t result = this->inherited::Make();
    const StEvent *event = (const StEvent*)GetInputDS(this->datasetNameStEvent);
    if (!event) {
        {LOG_ERROR << "Cannot find StEvent at " << this->datasetNameStEvent << endm;}
        result = kStWarn;
    }
    const StEmcCollection *emcCollection = event ? (const StEmcCollection *)event->emcCollection() : 0;
    if (event && !emcCollection) {LOG_ERROR << "Cannot find StEmcCollection" << endm;}

    if (this->mEmcDecoder) {
	this->mEmcDecoder->SetFixTowerMapBug(this->doTowerSwapFix);
	this->mEmcDecoder->SetDateTime(this->GetDate(), this->GetTime());
    }
    if (this->mEmcTables) this->mEmcTables->loadTables(this);

    const StEmcDetector *bemcDet = emcCollection ? emcCollection->detector(kBarrelEmcTowerId) : 0;

    if (this->mData) {
	this->mData->highestAdcHit.adc = 0;
        this->mData->trigger.triggered = 0;
    }
    for (Int_t i = 0;i < 4800;i++) {
    	this->towerTriggeredHT1[i] = false;
	this->towerTriggeredHT2[i] = false;
    }
    if (this->mData && bemcDet) {
        Float_t highestEtHitEt = -1000;
	Bool_t isHT1 = false;
	Bool_t isHT2 = false;
	Int_t numMod = bemcDet->numberOfModules();
	{LOG_DEBUG << "Tower detector present: " << numMod << " modules" << endm;}

	for (Int_t imod = 1;imod <= numMod;imod++) {
    	    const StEmcModule *module = bemcDet->module(imod);
    	    if (module) {
		for (StPtrVecEmcRawHitConstIterator it = module->hits().begin();it != module->hits().end();it++) {
		    const StEmcRawHit *hit = *it;
		    if (hit) {
			Int_t id = -1;
			this->mEmcGeom->getId(hit->module(), hit->eta(), hit->sub(), id);
			Int_t id_shift = 0;
			if (this->mEmcDecoder && this->mEmcDecoder->GetTowerBugCorrectionShift(id, id_shift)) id += id_shift;
			Int_t status = 0;
			this->mEmcTables->getStatus(BTOW, id, status);
			Int_t adc = hit->adc();
			Float_t energy = hit->energy();
			Int_t triggerAdc = (Int_t)adc / this->TriggerADC;
			Float_t theta = 0;
			this->mEmcGeom->getTheta(hit->module(), hit->eta(), theta);
			Float_t eta = 0, phi = 0;
			this->mEmcGeom->getEtaPhi(id, eta, phi);
			Float_t Et = energy * sin(theta);

			if (adc > this->mData->highestAdcHit.adc) {
			    getHitData(hit, this->mEmcGeom, 0, 0, 0, this->mEmcTables, this->mData->highestAdcHit);
			}
			if (Et > highestEtHitEt) {
			    highestEtHitEt = Et;
			    getHitData(hit, this->mEmcGeom, 0, 0, 0, this->mEmcTables, this->mData->highestEtHit);
	    		}

			if (triggerAdc > this->HT1Threshold) {
			    {LOG_DEBUG << "Tower HT1 " << id << " status=" << status << ", adc=" << hit->adc() << endm;}
	    		    isHT1 = true;
	    		    this->towerTriggeredHT1[id - 1] = true; 
			}
			if (triggerAdc > this->HT2Threshold) {
			    {LOG_DEBUG << "Tower HT2 " << id << " status=" << status << ", adc=" << hit->adc() << endm;}
	    		    isHT2 = true;
	    		    this->towerTriggeredHT2[id - 1] = true; 
			}
		    }
		}
    	    }
	}
	if (isHT1) this->mData->trigger.triggered |= this->triggersHT1;
	if (isHT2) this->mData->trigger.triggered |= this->triggersHT2;
    } else {LOG_WARN << "No kBarrelEmcTowerId detector!" << endm;}

    if (this->useFullEmcTriggerSimulator && this->mData) {
	UInt_t runId = event->runId();
        UInt_t year = (runId / 1000000) - 1;
	this->mData->trigger.triggered = 0;
	if (year >= 5) for (Int_t i = 0;i < 4800;i++) {
	    this->towerTriggeredHT1[i] = false;
	    this->towerTriggeredHT2[i] = false;
	}
	Bool_t isHT1 = false;
	Bool_t isHT2 = false;
	StEmcTriggerMaker *triggerMaker = dynamic_cast<StEmcTriggerMaker*>(this->GetMaker(this->triggerFullSimulatorName));
	if (triggerMaker) {
	    Int_t tmp = -1;
	    if (year == 3) {
		isHT1 = (triggerMaker->is2003HT1() == 1);
	    } else if (year == 4) {
		isHT1 = (triggerMaker->is2004HT1() == 1);
		isHT2 = (triggerMaker->is2004HT2() == 1);
	    } else if (year == 5) {
		isHT1 = (triggerMaker->is2005HT1() == 1);
		isHT2 = (triggerMaker->is2005HT2() == 1);
		if (isHT1) for (UInt_t i = triggerMaker->get2005HT1_NTOWS();i;--i) {
		    triggerMaker->get2005HT1_TOWS(i - 1, &tmp);
		    if ((tmp >= 1) && (tmp <= 4800)) this->towerTriggeredHT1[tmp - 1] = true;
		}
		if (isHT2) for (UInt_t i = triggerMaker->get2005HT2_NTOWS();i;--i) {
		    triggerMaker->get2005HT2_TOWS(i - 1, &tmp);
		    if ((tmp >= 1) && (tmp <= 4800)) this->towerTriggeredHT2[tmp - 1] = true;
		}
	    }/* else if (year == 6) {
		isHT2 = (triggerMaker->is2006HT2() == 1);
		if (isHT2) for (UInt_t i = triggerMaker->get2006HT2_NTOWS();i;--i) {
		    triggerMaker->get2006HT2_TOWS(i - 1, &tmp);
		    if ((tmp >= 1) && (tmp <= 4800)) this->towerTriggeredHT2[tmp - 1] = true;
		}
	    }*/
	} else {LOG_ERROR << "Cannot find StEmcTriggerMaker " << this->triggerFullSimulatorName <<  endm;}
	if (isHT1) this->mData->trigger.triggered |= this->triggersHT1;
        if (isHT2) this->mData->trigger.triggered |= this->triggersHT2;
    }
    {LOG_DEBUG << "Finished Make(), maxAdc=" << (this->mData ? this->mData->highestAdcHit.adc : -1) << endm;}
    return result;
}

//______________________________________________________________________________
Int_t StPi0TriggerSimulatorMaker::Finish() {
  {LOG_DEBUG << "Starting Finish()" << endm;}
  Int_t result = this->inherited::Finish();
  if (this->mData) delete this->mData; this->mData = 0;
  {LOG_DEBUG << "Deleted mData" << endm;}
  if (this->mEmcGeom) delete this->mEmcGeom; this->mEmcGeom = 0;
  {LOG_DEBUG << "Deleted mEmcGeom" << endm;}
  if (this->mEmcDecoder) delete this->mEmcDecoder; this->mEmcDecoder = 0;
  {LOG_DEBUG << "Deleted mEmcDecoder" << endm;}
  if (this->mEmcTables) delete this->mEmcTables; this->mEmcTables = 0;
  {LOG_DEBUG << "Deleted mEmcTables" << endm;}
  {LOG_DEBUG << "Finished Finish()" << endm;}
  return result;
}
