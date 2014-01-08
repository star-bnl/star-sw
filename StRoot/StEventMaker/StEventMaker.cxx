/*!
   StEventMaker

   Description: Setup of StEvent

   Author: Original version by T. Wenaus, BNL
           Revised version for new StEvent by T. Ullrich, Yale

*/

#include <vector>
#include <algorithm>
#include <utility>
#include <cstdlib>
#include "TError.h"
#include "TString.h"
#include "StEventMaker/StEventMaker.h"
#include "StEventMaker/StRootEventManager.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"
#include "StGlobals.hh"
#include "StEvtHddr.h"
#include "StTpcDb/StTpcDb.h"
#include "StDetectorDbMaker/StDetectorDbRichScalers.h"
#include "StDetectorDbMaker/StDetectorDbBeamInfo.h"
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "tables/St_trgOfflineFilter_Table.h"
#include "StDAQMaker/StDAQReader.h"
#include "StPrompt.hh"
#include "StMath.hh"
#include <typeinfo>
#include <map>
#include "StarMagField.h"
#include "TUnixTime.h"
#if !defined(ST_NO_NAMESPACES)
using std::vector;
using std::max;
using std::pair;
using std::map;
#endif

#if defined(ST_NO_TEMPLATE_DEF_ARGS)
#define StVector(T) vector<T, allocator<T> >
#else
#define StVector(T) vector<T>
#endif

static const char rcsid[] = "$Id: StEventMaker.cxx,v 2.97 2014/01/08 21:10:22 fisyak Exp $";
//______________________________________________________________________________
ClassImp(StEventMaker)
    //______________________________________________________________________________

    StEventMaker::StEventMaker(const char *name, const char *title) : StMaker(name)
{
    if(title) SetTitle(title);
    mEventManager = new StRootEventManager();
    mEventManager->setMaker(this);
    mCurrentEvent = 0;
    doLoadTpcHits     = kTRUE;
    doLoadFtpcHits    = kTRUE;
    doLoadSvtHits     = kTRUE;
    doLoadSsdHits     = kTRUE;
    doLoadTptTracks   = kFALSE;
    doLoadEstTracks   = kTRUE;
    doPrintEventInfo  = kFALSE;
    doPrintMemoryInfo = kFALSE;
    doPrintCpuInfo    = kFALSE;
    mCreateEmptyInstance = kFALSE;
}

StEventMaker::~StEventMaker()
{
    delete mEventManager;
}

void
StEventMaker::Clear(const char*)
{
    mCurrentEvent=0;
    StMaker::Clear();
}

StEventManager*
StEventMaker::eventManager() {return mEventManager;};
StEvent*
StEventMaker::event() { return mCurrentEvent;};

void
StEventMaker::setEventManager(StEventManager* mgr)
{
    mEventManager = mgr;
}
Int_t
StEventMaker::Init()
{
    return StMaker::Init();
}

Int_t
StEventMaker::Make()
{
    //
    // In this method we actually do not create anything but call
    // other methods which do that for us:
    // makeEvent()    creates StEvent and all its dependent classes
    //
    // Since this Maker should also work without any 'dst' dataset
    // we create *always* an instance of StEvent, even if it is empty.
    //

    //
    //  Init timing and memory snapshots
    //
    StTimer timer;
    if (doPrintCpuInfo) timer.start();
    if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

    //
    //  The current event is deleted automatically before every
    //  new event. It is added by using AddData().
    //  We do not need to delete it ourself.
    //
    //  If no DST dataset is available we cannot setup StEvent
    //  properly. Nevertheless we will create an empty instance.
    //
    if (TString(GetName()) == "0Event") mCreateEmptyInstance = kTRUE;
    else                                mCreateEmptyInstance = kFALSE;
    //
    //  Setup the event (StEvent and all subclasses)
    //
    Int_t status = makeEvent();
    if (status != kStOK)
        gMessMgr->Warning() << "StEventMaker::Make(): error in makeEvent(), no StEvent object created." << endm;

    mEventManager->closeEvent();
    //
    //  Print out some timing, memory usage and StEvent
    //  info if requested
    //
    if (doPrintEventInfo) printEventInfo();
    if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
    }
    if (doPrintCpuInfo) {
        timer.stop();
        LOG_DEBUG  << "CPU time for StEventMaker::Make(): "
             << timer.elapsedTime() << " sec\n" << endm;
    }

    return status;
}

StEvent*
StEventMaker::getStEventInstance()
{
    StEvent* exist = (StEvent*) GetInputDS("StEvent");
    if (exist) {
        gMessMgr->Info() << "StEventMaker::getStEventInstance(): existing instance found, no new object created." << endm;
        return exist;
    }
    else {
        exist = new StEvent;
	AddData(exist);
        return exist;
    }
}

Int_t
StEventMaker::makeEvent()
{
    //
    //  In case there's nothing to fill (no DST dataset) we
    //  create an empty instance only. This is OK in this
    //  case and therefore we do not return a warning or an
    //  error message.
    //
    if (! mCreateEmptyInstance &&  mCurrentEvent) return kStOK;
    //
    //  Create AND setup/fill StEvent.
    //
    long nrows;
    //
    //  Create instance of StEvent, using whatever we got so far.
    //
    mCurrentEvent = getStEventInstance();

    //
    //        Load trigger & trigger detector data
    //
    dst_TrgDet_st* dstTriggerDetectors = mEventManager->returnTable_dst_TrgDet(nrows);
    dst_L0_Trigger_st* dstL0Trigger    = mEventManager->returnTable_dst_L0_Trigger(nrows);
    dst_L1_Trigger_st* dstL1Trigger    = mEventManager->returnTable_dst_L1_Trigger(nrows);
    StEventInfo *evinfo = new StEventInfo();
    evinfo->setType("Collision");
    evinfo->setRunId(GetRunNumber());
    evinfo->setId(GetEventNumber());
    evinfo->setTime(TUnixTime::Convert(GetDateTime(),1));
    mCurrentEvent->setInfo(evinfo);

    Float_t   xlocal[3] = {0,0,0}, bfield[3] = {0,0,0};
    if (StarMagField::Instance()) StarMagField::Instance()->BField(xlocal,bfield);
    StEventSummary *evsum = new StEventSummary();
    evsum->setMagneticField(bfield[2]);
    mCurrentEvent->setSummary(evsum);
    //
    // Get trgStructure structures from StTriggerDataMaker
    // and store them in StEvent. Note that they are need
    // pior to creating the old StTriggerDetectorCollection
    // since the latter uses StTriggerData at construction.
    //
    if (!mCurrentEvent->triggerData()) {
        TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
        if (os) {
	  StTriggerData* pTrg = (StTriggerData*)os->GetObject();
	  assert(pTrg); 		// wrong, empty data
	  assert(os->IsOwner()); 	// wrong, data allready taken
	  os->DoOwner(0); 	          //change ownership
	  mCurrentEvent->setTriggerData(pTrg);
	  StEventInfo *theInfo = mCurrentEvent->info();
	  if (theInfo) {
	    theInfo->setBunchCrossingNumber(pTrg->bunchCounterLow(),0);
	    theInfo->setBunchCrossingNumber(pTrg->bunchCounterHigh(),1);
	  }
	  StEvtHddr* header = dynamic_cast<StEvtHddr*>(GetInputDS("EvtHddr"));
	  if (header) {
	    header->SetBunchCrossingNumber(pTrg->bunchCounterLow(),pTrg->bunchCounterHigh());
	  }
        }
    }

    //
    //  year 2001, 2002: use TrgDet tables
    //  year >= 2003: use info from StTriggerData
    //  Long term: get rid of StTriggerDetectorCollection
    //             and use StTriggerData
    //
    if (!mCurrentEvent->triggerDetectorCollection()) {
        if (mCurrentEvent->triggerData() && mCurrentEvent->triggerData()->year() >= 2003)
	  mCurrentEvent->setTriggerDetectorCollection(new StTriggerDetectorCollection(*(mCurrentEvent->triggerData())));
        else if (dstTriggerDetectors)
	  mCurrentEvent->setTriggerDetectorCollection(new StTriggerDetectorCollection(*dstTriggerDetectors));
    }

    StL0Trigger *l0t = mCurrentEvent->l0Trigger();
    if (!l0t) mCurrentEvent->setL0Trigger((l0t = new StL0Trigger()));
    if (mCurrentEvent->triggerData() && mCurrentEvent->triggerData()->year() >= 2003){
        l0t->set(mCurrentEvent->triggerData());
    }
    else {
        l0t->set(dstL0Trigger       );
        l0t->set(dstTriggerDetectors);
    }

    if (dstL0Trigger && dstL1Trigger && !mCurrentEvent->l1Trigger())
        mCurrentEvent->setL1Trigger(new StL1Trigger(*dstL0Trigger, *dstL1Trigger));
    //
    //  Trigger ID summary
    //
    Int_t idx;
    StTriggerIdCollection* triggerIdColl = mCurrentEvent->triggerIdCollection();
    if (!triggerIdColl) {
        mCurrentEvent->setTriggerIdCollection((triggerIdColl =
				       new StTriggerIdCollection()));
    }
    StTriggerId* trigId[3];
    triggerIdColl->setL1((trigId[0] = new StTriggerId()));
    triggerIdColl->setL2((trigId[1] = new StTriggerId()));
    triggerIdColl->setL3((trigId[2] = new StTriggerId()));
    // copy trigDetSums table to StEvent
    St_trigDetSums *table = 0;
    // reuse table from StDetectorDbMaker
    if ( St_trigDetSumsC::GetInstance()) {
      table = new St_trigDetSums(*((St_trigDetSums *)St_trigDetSumsC::GetInstance()->Table()));
      table->Shunt(mCurrentEvent);
    }
    if (! table) {
      TDataSet *set = GetDataSet("inputStream_DAQ");
      if (set) table = (St_trigDetSums *) set->Find("trigDetSums");
      if ( table) { 
	gMessMgr->Info("get trigDetSums from inputStream_DAQ");
	St_trigDetSums *copytable = new St_trigDetSums(*table);
	copytable->Shunt(mCurrentEvent); //  keep table from daq in StEvent
	table = copytable;
      }  else        {
	table = (St_trigDetSums *) GetDataBase("Calibrations/rich/trigDetSums");
	if ( table) { gMessMgr->Info("get trigDetSums from Calibrations/rich/trigDetSums");}
      }
      if (table) {
	StMaker::GetChain()->AddData(new St_trigDetSumsC(table));
      }
    }
    St_DataSet *daqReaderSet=GetDataSet("StDAQReader");
    if (daqReaderSet) { 
      StTrigSummary* trigSummary =
            ((StDAQReader*) (daqReaderSet->GetObject()))->getTrigSummary();
        if (!trigSummary) gMessMgr->Warning("StEventMaker: No StTrigSummary found");

        StDetectorDbTriggerID* dbTriggerId = StDetectorDbTriggerID::instance();
        if (!dbTriggerId) gMessMgr->Warning("StEventMaker: No StDetectorDbTriggerID found");

        if (trigSummary && dbTriggerId) {

	    // The nominal is a pointer to one of the above.
	    // 64 bits for Run 11 and beyond
	    if (mCurrentEvent->runId()> 12000000) {

		uint64_t mask = 0;
		mask  = (unsigned int) trigSummary->L1summary[1];
		mask <<= 32;
		mask += (unsigned int) trigSummary->L1summary[0];
		trigId[0]->setMask(mask);

		mask  = (unsigned int) trigSummary->L2summary[1];
		mask <<= 32;
		mask += (unsigned int) trigSummary->L2summary[0];
		trigId[1]->setMask(mask);

		mask  = (unsigned int) trigSummary->L3summary[1];
		mask <<= 32;
		mask += (unsigned int) trigSummary->L3summary[0];
		trigId[2]->setMask(mask);


	    }
	    else {

		trigId[0]->setMask(trigSummary->L1summary[0]);
		trigId[1]->setMask(trigSummary->L2summary[0]);
		trigId[2]->setMask(trigSummary->L3summary[0]);
	    }


	    // Loop over trigger level
	    for(unsigned int trglevel=0 ; trglevel < 3 ; trglevel++){
		StTriggerId* whichTrig =  trigId[trglevel];

		// Loop over the triggers within this level
		for (unsigned int iTrg = 0; iTrg < dbTriggerId->getIDNumRows() ; iTrg++){
		    // Shift the mask by daqTrigId bits to examine that bit
		    if ( whichTrig->mask() &  ((uint64_t)1 << (dbTriggerId->getDaqTrgId(iTrg)) )  ) {

		        //Check if StTriggerData found (partial) data corruption
		        //and if so, add 9000 to offline trigger Id
  		        UInt_t offlineId = dbTriggerId->getOfflineTrgId(iTrg);
                            if (mCurrentEvent->triggerData()){
			  if (mCurrentEvent->triggerData()->errorFlag()>0) {
                                    offlineId += 9000;
                                    printf("StEventMaker: StTriggerData found partial corruption, thus adding 9000 to offline trigger id =%d\n",offlineId);
			  }
                            }

                            whichTrig->addTrigger(
                                                  //dbTriggerId->getOfflineTrgId(iTrg),
                                                  offlineId,
                                                  dbTriggerId->getTrgVersion(iTrg),
                                                  dbTriggerId->getTrgNameVersion(iTrg),
                                                  dbTriggerId->getThreashVersion(iTrg),
                                                  dbTriggerId->getPsVersion(iTrg)
                                                  );
		    }
		}

		// Add in additional trigger ids to all levels; hack to fix prescale problem in runs 3-6
		for (unsigned int iTrg = 0; iTrg < dbTriggerId->getAdditionalTriggerIDNumRows() ; iTrg++){
		    // Shift the mask by daqTrigId bits to examine that bit
		    // Tweak for warnings; saved the run/event number as an unsigned int, but for some reason StEvent is int
		    int checkRun = dbTriggerId->getAdditionalTriggerIDRunNumber(iTrg);
		    int checkEvent = dbTriggerId->getAdditionalTriggerIDEventNumber(iTrg);

		    if (mCurrentEvent->runId() == checkRun &&
			mCurrentEvent->id() == checkEvent )
		    {
			    whichTrig->addTrigger(
				dbTriggerId->getAdditionalTriggerIDOfflineTrgId(iTrg),
				dbTriggerId->getAdditionalTriggerIDTrgVersion(iTrg),
				dbTriggerId->getAdditionalTriggerIDTrgNameVersion(iTrg),
				dbTriggerId->getAdditionalTriggerIDThreashVersion(iTrg),
				dbTriggerId->getAdditionalTriggerIDPsVersion(iTrg)
				);
		    }
		}
	    }

	    // This just puts the pointer, not a deep copy
	    if ( (idx=dbTriggerId->getDefaultTriggerLevel() ) != kDbTriggerBadID ){
		triggerIdColl->setNominal(trigId[idx-1]);
	    }
	    // Now hack up the offline trigger ids for year 2006


	    if (mCurrentEvent->triggerData() && mCurrentEvent->runId()>7000000 && mCurrentEvent->runId()<8000000) {
		gMessMgr->Info("StEventMaker::Run 6, expanding L3 trigger id");

		// Hack for mapping of StDetectorDb to StL2TriggerResultType
		map<string,StL2TriggerResultType> mapDbToStL2TriggerResultType;

		mapDbToStL2TriggerResultType["l2Trg2006BEMCGammaPi"] = l2Trg2006BEMCGammaPi;
		mapDbToStL2TriggerResultType["l2Trg2006BEMCGammaPiRandom"] = l2Trg2006BEMCGammaPiRandom;
		mapDbToStL2TriggerResultType["l2Trg2006EEMCGammaPi"] = l2Trg2006EEMCGammaPi;
		mapDbToStL2TriggerResultType["l2Trg2006EEMCGammaPiRandom"] = l2Trg2006EEMCGammaPiRandom;
		mapDbToStL2TriggerResultType["l2Trg2006MonoJet"] = l2Trg2006MonoJet;
		mapDbToStL2TriggerResultType["l2Trg2006DiJet"] = l2Trg2006DiJet;
		mapDbToStL2TriggerResultType["l2Trg2006RandomJet"] = l2Trg2006RandomJet;


		// Do a deep copy of the l3 into l3Expanded
		StTriggerId *whichTrig = new StTriggerId(*triggerIdColl->l3());

		triggerIdColl->setL3Expanded(whichTrig);
		// Reset the nominal pointer
		triggerIdColl->setNominal(whichTrig);
		// Expand up the l3Expanded.
		for (unsigned int irow=0; irow<dbTriggerId->getTrigL3ExpandedNumRows(); ++irow) {
		    unsigned int oldtid = dbTriggerId->getTrigL3ExpandedL3TrgId(irow);
		    unsigned int newtid = dbTriggerId->getTrigL3ExpandedL3ExpandedTrgId(irow);


		    string testString = string(dbTriggerId->getTrigL3ExpandedL2TriggerResultType(irow));


		    map<string,StL2TriggerResultType>::const_iterator p =
			mapDbToStL2TriggerResultType.find(
			    testString);

		    if (p != mapDbToStL2TriggerResultType.end()) {
			StL2TriggerResultType l2Test = (*p).second;
			if (whichTrig->isTrigger(oldtid) &&
			    mCurrentEvent->triggerData()->isL2Triggered(l2Test)) {

			    whichTrig->addTrigger(newtid,
						  whichTrig->version(oldtid),
						  whichTrig->nameVersion(oldtid),
						  whichTrig->thresholdVersion(oldtid),
						  whichTrig->prescaleVersion(oldtid)
				);
			}
		    }
		}
	    }
// We need to make a copy of the nominal to avoid problems in delete??
	    if (triggerIdColl->nominal()) triggerIdColl->setNominal(new StTriggerId(*(triggerIdColl->nominal())));
	    if (StTpcDb::instance()) {
	      Int_t TriggerId = -1; // Unknown
	      if (triggerIdColl) {
		static Int_t goodIds[5] = {9200,9201,310811,310812,310813};
		const StTriggerId *nominal = triggerIdColl->nominal();
		TriggerId = 0;
		if (nominal) {
		  for (Int_t i = 0; i < 5; i++) {
		    if (nominal->isTrigger(goodIds[i])) {TriggerId = goodIds[i]; break;}
		  }
		}
	      }
	      StTpcDb::instance()->SetTriggerId(TriggerId);
	    }

            // Allow trigger filtering to skip events
            Int_t FiltTrg = IAttr("FiltTrg");
            if (FiltTrg != 0) {
              bool unmatched = true;
	      St_trgOfflineFilter* flaggedTrgs =
                (St_trgOfflineFilter *) GetDataBase("Calibrations/trg/trgOfflineFilter");
              if (!flaggedTrgs) {
                LOG_ERROR << "Could not find Calibrations/trg/trgOfflineFilter in database" << endm;
                return kStErr;
              }
              const StTriggerId* tr = triggerIdColl->nominal();
              if (tr) {
                vector<unsigned int> idVec = tr->triggerIds();
                long nFlaggedTrgs = flaggedTrgs->GetNRows();
                for (unsigned int iTrg = 0;
                     unmatched && (iTrg < idVec.size()) ; iTrg++) {
                  trgOfflineFilter_st* flaggedTrg = flaggedTrgs->GetTable();
                  for (long iFlaggedTrg = 0;
                       unmatched && iFlaggedTrg < nFlaggedTrgs; iFlaggedTrg++, flaggedTrg++) {
                    unmatched = (idVec[iTrg] != flaggedTrg->trigid);
                  }
                }
              }
              if ((unmatched && FiltTrg>0) || // Include if trigger is matched
                 (!unmatched && FiltTrg<0)) { // Exclude if trigger is matched
                LOG_INFO << "Event failed trigger filter...skipping" << endm;
                return kStSkip;
              }
            }
	}
    }


    else {
        gMessMgr->Warning("StEventMaker: No StDAQReader found");
    }



    //
    //  Complete information in StEventInfo
    //
    StEventInfo *theInfo = mCurrentEvent->info();
    if (theInfo && dstTriggerDetectors) {
        theInfo->setBunchCrossingNumber(dstTriggerDetectors->bunchXing_lo,0);
        theInfo->setBunchCrossingNumber(dstTriggerDetectors->bunchXing_hi,1);
    }
    //
    //  Add data from StEvtHddr we cannot get elsewhere
    //
    StEvtHddr* header = dynamic_cast<StEvtHddr*>(GetInputDS("EvtHddr"));
    if (header) {
        mCurrentEvent->setTriggerMask(header->GetTriggerMask());
        if (mCurrentEvent->info())
	  mCurrentEvent->info()->setEventSize(header->GetEventSize());
    }

    //
    //  Fill StRunInfo
    //
    StRunInfo* mCurrentRunInfo = new StRunInfo;
    StDetectorDbBeamInfo *dbBeamInfo = StDetectorDbBeamInfo::instance();
    StDetectorDbRichScalers* richScalers = StDetectorDbRichScalers::instance();

    mCurrentRunInfo->setRunId(mCurrentEvent->runId());
    mCurrentRunInfo->setProductionTime(time(0));
    mCurrentRunInfo->setProductionVersion(getenv("STAR_VERSION"));
    if (mCurrentEvent->summary())
        mCurrentRunInfo->setMagneticField(mCurrentEvent->summary()->magneticField());
    if (gStTpcDb) {
        mCurrentRunInfo->setTpcDriftVelocity(east, gStTpcDb->DriftVelocity());
        mCurrentRunInfo->setTpcDriftVelocity(west, gStTpcDb->DriftVelocity());
    }
    if (dbBeamInfo) {
        mCurrentRunInfo->setCenterOfMassEnergy(dbBeamInfo->getBlueEnergy() + dbBeamInfo->getYellowEnergy());
        mCurrentRunInfo->setBeamMassNumber(blue, dbBeamInfo->getBlueMassNumber());
        mCurrentRunInfo->setBeamMassNumber(yellow, dbBeamInfo->getYellowMassNumber());
        mCurrentRunInfo->setBeamEnergy(blue, dbBeamInfo->getBlueEnergy());
        mCurrentRunInfo->setBeamEnergy(yellow, dbBeamInfo->getYellowEnergy());
        mCurrentRunInfo->setInitialBeamIntensity(blue, dbBeamInfo->getBlueIntensity());
        mCurrentRunInfo->setInitialBeamIntensity(yellow, dbBeamInfo->getYellowIntensity());
        mCurrentRunInfo->setBeamLifeTime(blue, dbBeamInfo->getBlueLifeTime());
        mCurrentRunInfo->setBeamLifeTime(yellow, dbBeamInfo->getYellowLifeTime());
        mCurrentRunInfo->setBeamFillNumber(blue, dbBeamInfo->getBlueFillNumber());
        mCurrentRunInfo->setBeamFillNumber(yellow, dbBeamInfo->getYellowFillNumber());
    }
    if (richScalers) {
        mCurrentRunInfo->setZdcWestRate(richScalers->getZDCWest());
        mCurrentRunInfo->setZdcEastRate(richScalers->getZDCEast());
        mCurrentRunInfo->setZdcCoincidenceRate(richScalers->getZDCX());
        mCurrentRunInfo->setBackgroundRate(richScalers->getMult());
        mCurrentRunInfo->setL0RateToRich(richScalers->getL0());
        mCurrentRunInfo->setBbcCoincidenceRate(richScalers->getBBCX());
        mCurrentRunInfo->setBbcEastRate(richScalers->getBBCEast());
        mCurrentRunInfo->setBbcWestRate(richScalers->getBBCWest());
        mCurrentRunInfo->setBbcBlueBackgroundRate(richScalers->getBBCBlueBkg());
        mCurrentRunInfo->setBbcYellowBackgroundRate(richScalers->getBBCYellowBkg());
    }

    if (mCurrentRunInfo)
        mCurrentEvent->setRunInfo(mCurrentRunInfo);

    //
    //  Detector States
    //
    if (richScalers)
        mCurrentEvent->addDetectorState(new StDetectorState(kRichId, richScalers->getRichHVStatus()));

    return kStOK;
}

void
StEventMaker::printEventInfo()
{
    LOG_INFO << "*********************************************************" << endm;
    LOG_INFO << "*                  StEvent Information                  *" << endm;
    LOG_INFO << "*********************************************************" << endm;

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StEvent at " << (void*) mCurrentEvent                      << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent)
	 mCurrentEvent->Dump();
    else
        return;

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StRunInfo at " << (void*) mCurrentEvent->runInfo()         << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->runInfo())
	 mCurrentEvent->runInfo()->Dump();

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StEventInfo at " << (void*) mCurrentEvent->info()          << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->info())
	 mCurrentEvent->info()->Dump();

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StEventSummary at " << (void*) (mCurrentEvent->summary())  << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->summary()) mCurrentEvent->summary()->Dump();
    if (mCurrentEvent->summary()) {
        unsigned int k;
 StEventSummary *evtsum = mCurrentEvent->summary();
    LOG_INFO << "--> StEventSummary quasi-histograms" << endm;
    LOG_INFO << "--> StEventSummary quasi-histogram -> # of tracks vs. eta" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
           LOG_INFO << k << "\t[" << evtsum->lowerEdgeEtaBin(k)
                 << " - "      << evtsum->upperEdgeEtaBin(k)
                 << "] : \t"  <<  evtsum->tracksInEtaBin(k) << endm;
        }
        LOG_INFO << "--> StEventSummary quasi-histogram -> # of tracks vs. phi" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
           LOG_INFO << k << "\t[" << evtsum->lowerEdgePhiBin(k)
                 << " - "      << evtsum->upperEdgePhiBin(k)
                 << "] : \t"   << evtsum->tracksInPhiBin(k) << endm;
        }
        LOG_INFO << "--> StEventSummary quasi-histogram -> # of tracks vs. pt" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
           LOG_INFO << k << "\t[" << evtsum->lowerEdgePtBin(k)
                 << " - "      << evtsum->upperEdgePtBin(k)
                 << "] : \t"   << evtsum->tracksInPtBin(k) << endm;
        }
        LOG_INFO << "--> StEventSummary quasi-histogram -> energy vs. eta" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
            LOG_INFO << k << "\t[" << evtsum->lowerEdgeEtaBin(k)
                 << " - "      << evtsum->upperEdgeEtaBin(k)
                 << "] : \t"   << evtsum->energyInEtaBin(k) << endm;
        }
        LOG_INFO << "--> StEventSummary quasi-histogram -> energy vs. phi" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
        LOG_INFO << k << "\t[" << evtsum->lowerEdgePhiBin(k)
                 << " - "      << evtsum->upperEdgePhiBin(k)
                 << "] : \t"   << evtsum->energyInPhiBin(k) << endm;
        }
    }
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StL0Trigger at "
         << (void*) (mCurrentEvent->l0Trigger())                        << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->l0Trigger()) mCurrentEvent->l0Trigger()->Dump();

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StTriggerDetectorCollection at "
         << (void*) (mCurrentEvent->triggerDetectorCollection())        << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->triggerDetectorCollection())
        mCurrentEvent->triggerDetectorCollection()->Dump();

    if (mCurrentEvent->triggerDetectorCollection()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StCtbTriggerDetector"                                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->triggerDetectorCollection()->ctb().Dump();

        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StMwcTriggerDetector"                                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->triggerDetectorCollection()->mwc().Dump();

        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StVpdTriggerDetector"                                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->triggerDetectorCollection()->vpd().Dump();

        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StZdcTriggerDetector"                                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->triggerDetectorCollection()->zdc().Dump();
    }

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecTrackDetectorInfo"                                << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->trackDetectorInfo().size() << endm;

    if (mCurrentEvent->trackDetectorInfo().size()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StTrackDetectorInfo at "
             << (void*) mCurrentEvent->trackDetectorInfo()[0]               << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->trackDetectorInfo()[0]->Dump();
    }

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecTrackNode"                                        << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "All tracks in the first node are printed separately  "     << endm;
    LOG_INFO << "after the node info.                                     " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->trackNodes().size() << endm;

    unsigned int i;
    if (mCurrentEvent->trackNodes().size()) {
        LOG_INFO << "# tracks in first element = "
             << mCurrentEvent->trackNodes()[0]->entries() << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StTrackNode at "
             << (void*) mCurrentEvent->trackNodes()[0]                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->trackNodes()[0]->Dump();
        for (i=0; i<mCurrentEvent->trackNodes()[0]->entries(); i++)
            printTrackInfo(mCurrentEvent->trackNodes()[0]->track(i));
    }

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecPrimaryVertex"                                    << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "The first daughter track (primary track) in the first    " << endm;
    LOG_INFO << "vertex is printed separately after the vertex info.      " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->numberOfPrimaryVertices() << endm;

    if (mCurrentEvent->numberOfPrimaryVertices()) {
        LOG_INFO << "# primary tracks in first element = "
             << mCurrentEvent->primaryVertex()->numberOfDaughters() << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StPrimaryVertex at "
             << (void*) mCurrentEvent->primaryVertex()                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->primaryVertex()->Dump();
        if (mCurrentEvent->primaryVertex()->numberOfDaughters())
            printTrackInfo(mCurrentEvent->primaryVertex()->daughter(0));
    }

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecCalibrationVertex"                                << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->numberOfCalibrationVertices() << endm;

    if (mCurrentEvent->numberOfCalibrationVertices())
        mCurrentEvent->calibrationVertex(0)->Dump();

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecV0Vertex"                                         << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->v0Vertices().size() << endm;

    if (mCurrentEvent->v0Vertices().size()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StV0Vertex at "
             << (void*) mCurrentEvent->v0Vertices()[0]                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->v0Vertices()[0]->Dump();
    }

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecXiVertex"                                         << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->xiVertices().size() << endm;

    if (mCurrentEvent->xiVertices().size()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StXiVertex at "
             << (void*) mCurrentEvent->xiVertices()[0]                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->xiVertices()[0]->Dump();
    }

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecKinkVertex"                                       << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->kinkVertices().size() << endm;

    if (mCurrentEvent->kinkVertices().size()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StKinkVertex at "
             << (void*) mCurrentEvent->kinkVertices()[0]                    << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->kinkVertices()[0]->Dump();
    }

    unsigned int       j=0, k=0, nhits=0;
    Bool_t             gotOneHit;
    StTpcHitCollection *tpcColl = mCurrentEvent->tpcHitCollection();
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StTpcHitCollection at " << (void*) tpcColl                 << endm;
    LOG_INFO << "Dumping collection size and one hit only."                 << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (tpcColl) {
        nhits = tpcColl->numberOfHits();
        LOG_INFO << "# of hits in collection = " << nhits << endm;
        gotOneHit = kFALSE;
        for (k=0; !gotOneHit && k<tpcColl->numberOfSectors(); k++)
            for (j=0; !gotOneHit && j<tpcColl->sector(k)->numberOfPadrows(); j++)
                if (tpcColl->sector(k)->padrow(j)->hits().size()) {
                    tpcColl->sector(k)->padrow(j)->hits()[0]->Dump();
                    gotOneHit = kTRUE;
                }
    }

    StFtpcHitCollection *ftpcColl = mCurrentEvent->ftpcHitCollection();
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StFtpcHitCollection at " << (void*) ftpcColl               << endm;
    LOG_INFO << "Dumping collection size and one hit only."                 << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (ftpcColl) {
        nhits = ftpcColl->numberOfHits();
        LOG_INFO << "# of hits in collection = " << nhits << endm;
        gotOneHit = kFALSE;
        for (k=0; !gotOneHit && k<ftpcColl->numberOfPlanes(); k++)
            for (j=0; !gotOneHit && j<ftpcColl->plane(k)->numberOfSectors(); j++)
                if (ftpcColl->plane(k)->sector(j)->hits().size()) {
                    ftpcColl->plane(k)->sector(j)->hits()[0]->Dump();
                    gotOneHit = kTRUE;
                }
    }

    StSvtHitCollection *svtColl = mCurrentEvent->svtHitCollection();
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSvtHitCollection at " << (void*) svtColl                 << endm;
    LOG_INFO << "Dumping collection size and one hit only."                 << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (svtColl) {
        nhits = svtColl->numberOfHits();
        LOG_INFO << "# of hits in collection = " << nhits << endm;
        gotOneHit = kFALSE;
        for (k=0; !gotOneHit && k<svtColl->numberOfBarrels(); k++)
            for (j=0; !gotOneHit && j<svtColl->barrel(k)->numberOfLadders(); j++)
                for (i=0; !gotOneHit && i<svtColl->barrel(k)->ladder(j)->numberOfWafers(); i++)
                    if (svtColl->barrel(k)->ladder(j)->wafer(i)->hits().size()) {
                        svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[0]->Dump();
                        gotOneHit = kTRUE;
                    }
    }

    StSsdHitCollection *ssdColl = mCurrentEvent->ssdHitCollection();
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSsdHitCollection at " << (void*) ssdColl                 << endm;
    LOG_INFO << "Dumping collection size and one hit only."                 << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (ssdColl) {
        nhits = ssdColl->numberOfHits();
        LOG_INFO << "# of hits in collection = " << nhits << endm;
        gotOneHit = kFALSE;
        for (k=0; !gotOneHit && k<ssdColl->numberOfLadders(); k++)
	    for (i=0; !gotOneHit && i<ssdColl->ladder(j)->numberOfWafers(); i++)
		if (ssdColl->ladder(j)->wafer(i)->hits().size()) {
		    ssdColl->ladder(j)->wafer(i)->hits()[0]->Dump();
		    gotOneHit = kTRUE;
		}
    }

    LOG_INFO << endm;

    //
    //   Info from some tables for comparisons.
    //   Only tables with varying # of rows are listed.
    //
    LOG_INFO << "*********************************************************" << endm;
    LOG_INFO << "*                   Table Information                   *" << endm;
    LOG_INFO << "*********************************************************" << endm;
    LOG_INFO << endm;
}

void
StEventMaker::printTrackInfo(StTrack* track)
{
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StTrack (" << (track ? track->GetName() : "n/a")
         << ") at " << (void*) track                                    << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (track) {
        track->Dump();
        LOG_INFO << "covariantMatrix():" << track->fitTraits().covariantMatrix() << endm;

        LOG_INFO << "---> StTrack -> StGeometry ("<< track->geometry()->GetName()
             << ") at " << (void*) (track->geometry()) << endm;
        if (track->geometry()) track->geometry()->Dump();

        LOG_INFO << "---> StTrack -> StGeometry (outer) ("<< track->outerGeometry()->GetName()
             << ") at " << (void*) (track->outerGeometry()) << endm;
        if (track->outerGeometry()) track->outerGeometry()->Dump();

        LOG_INFO << "---> StTrack -> StDetectorInfo at "
             << (void*) (track->detectorInfo()) << endm;
        if (track->detectorInfo()) track->detectorInfo()->Dump();

        LOG_INFO << "---> StTrack -> StTrackNode at "
             << (void*) (track->node()) << endm;
        if (track->node()) track->node()->Dump();

        LOG_INFO << "---> StTrack -> StPidTraits ("
             << (track->pidTraits().size() ? 1 : 0 ) << " of "
             <<  track->pidTraits().size() << " entries shown)" << endm;
        if (track->pidTraits().size()) track->pidTraits()[0]->Dump();
    }
}

/**************************************************************************
 * $Id: StEventMaker.cxx,v 2.97 2014/01/08 21:10:22 fisyak Exp $
 * $Log: StEventMaker.cxx,v $
 * Revision 2.97  2014/01/08 21:10:22  fisyak
 * Move St_trigDetSumsC under top maker for consistency with StDetectorDbMaker
 *
 * Revision 2.96  2013/12/20 18:41:41  genevb
 * Add event filtering by trigger (offline id)
 *
 * Revision 2.95  2013/12/17 15:48:19  fisyak
 * Copy trigDetSums table to StEvent
 *
 * Revision 2.94  2013/04/11 18:57:36  jeromel
 * cast to unsigned as mask is 2x32 in a unint64
 *
 * Revision 2.93  2012/05/07 14:44:50  fisyak
 * Give StTpcDb hit about TriggerId
 *
 * Revision 2.92  2011/02/02 20:21:06  ullrich
 * Changes die to modified StTriggerId (32 -> 64 bit) (Jamie)
 *
 * Revision 2.91  2009/11/23 16:37:08  fisyak
 * Clean up, fix problem with bunch crossing information in StEventInfo and StHddr
 *
 * Revision 2.89  2009/11/19 16:54:09  fisyak
 * Clean up
 *
 * Revision 2.88  2009/11/10 21:59:30  fisyak
 * Fix
 *
 * Revision 2.87  2009/11/10 20:45:08  fisyak
 * pams Cleanup
 *
 * Revision 2.86  2009/11/10 03:55:49  perev
 * Remove redundant printing
 *
 * Revision 2.85  2009/08/24 23:06:11  ullrich
 * Added checks for corruption in StTriggerData.
 *
 * Revision 2.84  2008/01/29 18:45:01  perev
 * WarnOff
 *
 * Revision 2.83  2007/08/24 17:26:29  fine
 * replace cout with LOG_INFO
 *
 * Revision 2.82  2007/08/24 15:07:43  perev
 * No print mem info by default
 *
 * Revision 2.81  2007/05/11 23:17:11  jeromel
 * Addition by J.Dunlop of random trigger (from db)
 *
 * Revision 2.80  2007/04/28 20:36:16  perev
 * Redundant StChain.h removed
 *
 * Revision 2.79  2006/09/14 00:08:04  ullrich
 * StTriggerDetectorCollection constructor changed and L2 \ninterface in StTriggerData (no run number needed.
 *
 * Revision 2.78  2006/08/21 20:58:20  ullrich
 * Pass run number to constructor of StTriggerDetectorCollection (Akio).
 *
 * Revision 2.77  2006/08/10 03:32:47  perev
 * Assert==>assert
 *
 * Revision 2.76  2006/05/04 19:15:40  ullrich
 * Added L3 expanded (Jamie).
 *
 * Revision 2.75  2005/12/07 19:38:19  perev
 * simplified logic if StEvent already exists
 *
 * Revision 2.74  2005/06/15 01:00:53  ullrich
 * Use abs() for SVT hit track ID
 *
 * Revision 2.73  2004/10/20 21:18:41  ullrich
 * Removed checks that prevented storage of calib. vertices.
 *
 * Revision 2.72  2004/08/28 18:52:02  fisyak
 * Replace StEvent Hit containers if there are entries in the corrensponding tables
 *
 * Revision 2.71  2004/08/13 18:45:44  ullrich
 * Set vertex-used-in-fit flag for primary tracks.
 *
 * Revision 2.70  2004/08/03 17:26:05  ullrich
 * Update of trigger part by Akio.
 *
 * Revision 2.69  2004/07/07 20:58:51  ullrich
 * Corrected way the SVT drift velocity scaler is obtained.
 *
 * Revision 2.68  2004/07/06 23:06:05  ullrich
 * Fill SVT drift velocity scaler into StRunInfo.
 *
 * Revision 2.67  2004/04/12 16:48:21  ullrich
 * Fixed bug in creating StTriggerDetectorCollection. StTriggerData
 * needs to be instantiated and added before StTriggerDetectorCollection
 * gets constructed.
 *
 * Revision 2.66  2004/02/14 19:25:27  perev
 * More strong check for secondaries objects
 *
 * Revision 2.65  2004/02/11 02:20:27  ullrich
 * Load StTriggerDetectorCollection using StTriggerData for runs
 * in 2003 and later. For 2001 and 2002 stick to TrgDet table.
 *
 * Revision 2.64  2004/01/22 23:35:55  ullrich
 * Fill new member of StRunInfo (RICH scaler).
 *
 * Revision 2.63  2003/12/29 00:00:34  perev
 * More debug prints
 *
 * Revision 2.62  2003/12/04 03:54:13  perev
 * Set empty, instead of crazy outer geometry
 *
 * Revision 2.61  2003/12/01 18:22:31  perev
 * Fix bug in bad track filter. Thanks to Helen
 *
 * Revision 2.60  2003/11/25 04:13:25  perev
 * FPE protection
 *
 * Revision 2.59  2003/10/13 04:38:35  perev
 * Bug with different collection lenth fixed
 *
 * Revision 2.58  2003/07/16 19:58:32  perev
 * Cleanup of StTriggerData2003 at all
 *
 * Revision 2.57  2003/04/16 17:52:28  ullrich
 * Cleaned up section where trigger structures are added,
 * that us removed debud print-outs.
 *
 * Revision 2.56  2003/04/15 22:37:54  jeromel
 * StTriggerDataMaker added
 *
 * Revision 2.55  2003/04/13 23:07:01  jeromel
 * Use of access method on null already fixed. Additional dbTriggerId fix closes bug # 85
 *
 * Revision 2.54  2003/04/08 18:44:03  ullrich
 * Added protection for cases where StDetectorDbTriggerID and
 * StTrigSummary couldn't be obtained (see Trigger Id summary).
 *
 * Revision 2.53  2003/02/19 16:35:04  jeromel
 * $LINK mechanism removed
 *
 * Revision 2.52  2003/02/18 23:25:55  jeromel
 * Loop over trigger blabla corrected
 *
 * Revision 2.51  2003/02/15 20:19:31  genevb
 * Added Trigger ID Summary
 *
 * Revision 2.50  2002/11/26 02:19:48  perev
 * StEventMaker ITTF modif
 *
 * Revision 2.49  2002/05/02 03:07:18  ullrich
 * Changed mechanism to reject EST tracks without SVT hits.
 *
 * Revision 2.48  2002/05/01 01:08:31  ullrich
 * Add SVT dE/dx only to EST tracks.
 *
 * Revision 2.47  2002/04/18 23:29:34  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.46  2002/02/25 19:34:14  ullrich
 * Fill parts of StRunInfo from StDetectorDbBeamInfo.
 *
 * Revision 2.45  2002/02/15 23:06:58  ullrich
 * Fill detector state for RICH.
 *
 * Revision 2.44  2002/01/31 23:50:28  ullrich
 * More filling of StRunInfo (by J. Gans).
 *
 * Revision 2.43  2002/01/11 16:44:12  ullrich
 * Fill bunch crossing numbers in StEventInfo.
 *
 * Revision 2.42  2001/12/21 22:39:32  ullrich
 * Disabled filling parts of StRunInfo.
 *
 * Revision 2.41  2001/12/21 21:13:03  ullrich
 * Fixed bug: loading multiple primary vertices.
 *
 * Revision 2.40  2001/11/10 23:54:21  ullrich
 * Added calibration vertices.
 *
 * Revision 2.39  2001/11/07 21:20:46  ullrich
 * Added L1 trigger.
 *
 * Revision 2.38  2001/09/28 22:22:05  ullrich
 * Load helix geometry at last point of each track.
 *
 * Revision 2.37  2001/09/19 04:49:05  ullrich
 * Set event size in StEventInfo.
 *
 * Revision 2.36  2001/09/18 00:16:06  ullrich
 * Fill and add StRunInfo.
 *
 * Revision 2.35  2001/09/12 23:49:22  ullrich
 * Removed code to build StRun and StRunSummary.
 *
 * Revision 2.34  2001/07/19 00:05:28  ullrich
 * New StL0Trigger needs additional table in constructor.
 *
 * Revision 2.33  2001/07/17 22:21:50  ullrich
 * Use B from event summary to set helicity of tracks.
 *
 * Revision 2.32  2001/05/17 22:46:37  ullrich
 * Removed loading of event summary params.
 *
 * Revision 2.31  2001/02/22 05:02:40  ullrich
 * Added new protected method getStEventInstance().
 * Modified maker to allow multiple calls of Make() within
 * one event. If instance already it is re-used and the data
 * from existing tables gets added.
 *
 * Revision 2.30  2000/11/02 16:33:28  ullrich
 * Fixed tiny memory leak.
 *
 * Revision 2.29  2000/08/30 05:37:02  ullrich
 * Obtain trigger mask from StEvtHddr dataset.
 *
 * Revision 2.28  2000/08/17 00:38:48  ullrich
 * Allow loading of tpt tracks.
 *
 * Revision 2.27  2000/05/26 11:36:19  ullrich
 * Default is to NOT print event info (doPrintEventInfo  = kFALSE).
 *
 * Revision 2.26  2000/05/26 11:34:08  ullrich
 * Skip the attempt of creating an instance of StRun in case
 * no dst dataset is available.
 *
 * Revision 2.25  2000/05/25 14:44:43  ullrich
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.24  2000/05/24 15:48:15  ullrich
 * Instance of StEvent now also created if no DST dataset
 * is available.
 *
 * Revision 2.23  2000/05/22 21:53:41  ullrich
 * No more copying of RICH tables. RICH now writes directly
 * to StEvent. printEventInfo() and makeEvent() modified.
 *
 * Revision 2.22  2000/04/26 20:29:13  ullrich
 * Create instance of StEvent not StBrowsableEvent.
 *
 * Revision 2.21  2000/03/22 17:11:20  ullrich
 * Added further checks for case were tables exist but have
 * zero length. Added for primary and global tracks.
 *
 * Revision 2.20  2000/02/23 12:11:49  ullrich
 * Added printout of covariant matrix to printTrackInfo().
 *
 * Revision 2.19  2000/02/17 18:19:05  ullrich
 * Adapted new SVT hit storage layout. Barrels instead of layers.
 *
 * Revision 2.18  2000/02/11 16:12:33  ullrich
 * Modified check for valid primary vertices.
 *
 * Revision 2.17  2000/02/08 21:14:16  genevb
 * Handle cases with no tracks.
 *
 * Revision 2.16  2000/01/25 20:11:11  ullrich
 * Fixed bug in loading the Xi vertices.
 *
 * Revision 2.15  2000/01/14 18:51:06  ullrich
 * Added printout of quasi-histos in the event summary
 * to printEventInfo().
 *
 * Revision 2.14  2000/01/14 13:58:03  ullrich
 * Create and fill the RICH pixel collection. Added also
 * the debug output for the RICH to printEventInfo().
 *
 * Revision 2.13  2000/01/11 16:05:34  ullrich
 * With Victors help now possible to read the dst_summary_param
 * table from the runco branch and build StEventSummary objects.
 *
 * Revision 2.12  2000/01/10 18:20:32  ullrich
 * Create new StTrackDetectorInfo object for primary tracks if
 * first or last points differ from the referring global track.
 *
 * Revision 2.11  2000/01/05 16:07:44  ullrich
 * Added loading of SSD hits and handling of runco branch.
 *
 * Revision 2.10  1999/12/21 15:13:13  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.9  1999/12/07 18:58:39  ullrich
 * Modified to get rid of some warnings on Linux
 *
 * Revision 2.8  1999/11/23 17:14:19  ullrich
 * Forgot to fill PID traits. Fixed now.
 *
 * Revision 2.7  1999/11/17 14:10:27  ullrich
 * Added more checks to protect from corrupted table data.
 *
 * Revision 2.6  1999/11/11 17:46:30  ullrich
 * Added more checks and warning messages. Handling
 * of primary vertices made safer
 *
 * Revision 2.5  1999/11/11 10:02:58  ullrich
 * Added warning message in case some hits cannot be stored.
 *
 * Revision 2.4  1999/11/10 22:40:27  ullrich
 * Delete hit if it cannot be added to collection.
 *
 * Revision 2.3  1999/11/08 17:04:59  ullrich
 * Hits now allocated individually.
 *
 * Revision 2.2  1999/11/05 18:35:54  ullrich
 * Added methods and flags for debugging and monitoring.
 *
 * Revision 2.1  1999/11/04 19:55:37  ullrich
 * Corrected typo.
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
