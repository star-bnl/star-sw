//StJetSkimEventMaker.cxx
//M.L. Miller (MIT)
//12/06

//root
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"

//St_base
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"

//StEvent
#include "StEvent.h"

//
#include "St_db_Maker/St_db_Maker.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//StSpinDb
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

//StJetMaker
#include "StJetMaker/StJetSkimEventMaker.h"

//StSpinPool
#include "StSpinPool/StJetSkimEvent/StJetSkimEvent.h"

//StMCAsymMaker
#include "StSpinPool/StMCAsymMaker/StPythiaEvent.h"
#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"

//StTriggerUtilities
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StTriggerUtilities/Emc/StEmcTriggerSimu.h"

// StBtofHeader
#include "StBTofHeader.h"
#include "StEnumerations.h"

void copyVertex(StMuPrimaryVertex& v, StJetSkimVert& sv);

ClassImp(StJetSkimEventMaker)

	StJetSkimEventMaker::StJetSkimEventMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputName) 
: StMaker(name), muDstMaker(uDstMaker), outName(outputName), mOutfile(0), mTree(0), mEvent(0), isRealData(true)
{

}

StJetSkimEventMaker::~StJetSkimEventMaker()
{
}

Int_t StJetSkimEventMaker::Init()
{
	//open file, book tree
	assert(outName!=0);
	printf("StJetSkimEventMaker::Init(): open file:\t%s\t for writing\n",outName);
	mOutfile = new TFile(outName,"RECREATE");

	//here's the object that will be stored in the tree
	mEvent = new StJetSkimEvent();

	//skipping TObject data members if possible can save significant space
	StJetSkimTrig::Class()->IgnoreTObjectStreamer();
	StPythiaEvent::Class()->IgnoreTObjectStreamer();

	//now we build the tree
	mTree = new TTree("jetSkimTree","StJetSkimEvent Tree",99);

	//and add a branch for our event objects
	mTree->Branch ("skimEventBranch", "StJetSkimEvent", &mEvent, 64000, 99);

	//if this is a simulation we'll pick up all the info we need right here
	mcAsymMaker  = dynamic_cast<StMCAsymMaker*>(GetMaker("MCAsym"));
	if(mcAsymMaker != NULL) {
		mEvent->setMcEvent(mcAsymMaker->pythiaEvent());
		isRealData = false;
	}

	return kStOk;
}

Int_t StJetSkimEventMaker::InitRun(int runnumber) {
	//store prescale and threshold info for all active triggers in this run in a TClonesArray
	//TClonesArray has unique ID == runnumber and is stored in the UserInfo of the tree
	//each event has a TRef pointing to its trigger header array

	TList *headerList = mTree->GetUserInfo();
	bool insertHeader = true;

	//look for an existing trigger header array for this run
	for(int i=0; i<headerList->GetEntries(); i++) {
		TClonesArray *tmp = (TClonesArray*)headerList->At(i);
		StJetSkimTrigHeader *h = (StJetSkimTrigHeader*)tmp->At(0);
		if(h->runId == runnumber){
			insertHeader = false;
			mCurrentHeaderRef = tmp;
		}
	}

	if(insertHeader) {
		TClonesArray *trigHeaderArray = new TClonesArray("StJetSkimTrigHeader",50);
		StJetSkimTrigHeader *header = new StJetSkimTrigHeader();

		if(isRealData) {
			StDetectorDbTriggerID *v = StDetectorDbTriggerID::instance();
			map<int,float> prescaleMap = v->getTotalPrescales();
			for(map<int,float>::const_iterator it=prescaleMap.begin(); it!=prescaleMap.end(); it++) {
				header->runId = runnumber;
				header->trigId = it->first;
				header->prescale = it->second;
				fillThresholds(*header);
				new( (*trigHeaderArray)[trigHeaderArray->GetLast()+1] ) StJetSkimTrigHeader(*header);
			}
		}
		else {
			for(unsigned i=0; i<mSimuTrigIds.size(); i++) {
				header->runId = runnumber;
				header->trigId = mSimuTrigIds[i];
				header->prescale = 1.0;
				fillThresholds(*header);
				new( (*trigHeaderArray)[trigHeaderArray->GetLast()+1] ) StJetSkimTrigHeader(*header);
			}
		}

		headerList->Add(trigHeaderArray);
		mCurrentHeaderRef = headerList->Last();
	}

	return StMaker::InitRun(runnumber);
}

Int_t StJetSkimEventMaker::Make()
{
	//clear info
	mEvent->clear();

	//useful pointers
	assert(muDstMaker);
	StMuDst* muDst = muDstMaker->muDst();
	assert(muDst);
	StMuEvent* muEvent = muDst->event();
	assert(muEvent);

	StBbcTriggerDetector* bbc = &(muEvent->bbcTriggerDetector());
	assert(bbc);
	StRunInfo* runInfo = &(muEvent->runInfo()); assert(runInfo);
	assert(runInfo);
	StDetectorDbTriggerID* v = StDetectorDbTriggerID::instance();
	assert(v);

	//first cycle through triggers:
	mEvent->setTrigHeaderArray(mCurrentHeaderRef);
	if(isRealData) {
		map<int,float> prescaleMap = v->getTotalPrescales();
		StJetSkimTrig skimTrig;
		for (map<int,float>::iterator it=prescaleMap.begin(); it!=prescaleMap.end(); ++it) {
			skimTrig.setTrigId((*it).first);
			if (muEvent->triggerIdCollection().nominal().isTrigger(skimTrig.trigId())) {
				skimTrig.setDidFire(true);
			}
			else {
				skimTrig.setDidFire(false);
			}
			fillTriggerSimulationInfo(skimTrig);
			if(skimTrig.didFire() || (skimTrig.shouldFire() > 0)) mEvent->setTrig(skimTrig);
			skimTrig.clear();
		}

		St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("StarDb");
		assert(mydb);
		int theYear=mydb->GetDateTime().GetYear();

		if(theYear >= 2006)
		{
			mEvent->setL2Result(muEvent->L2Result().GetArray());
		}
	}
	else {
		StJetSkimTrig skimTrig;
		for(unsigned i=0; i<mSimuTrigIds.size(); i++) {
			skimTrig.setTrigId(mSimuTrigIds[i]);
			skimTrig.setDidFire(false);
			fillTriggerSimulationInfo(skimTrig);
			if(skimTrig.shouldFire() > 0) mEvent->setTrig(skimTrig);
			skimTrig.clear();
		}
	}

	//basic event/run info
	mEvent->setFill( runInfo->beamFillNumber(blue));
	mEvent->setRunId( muEvent->runId() );
	mEvent->setEventId( muEvent->eventId() );
	TChain* chain = muDstMaker->chain(); 
	assert(chain);
	TObjString inputfile(chain->GetFile()->GetName());
	mEvent->setMudstFileName(inputfile);
	mEvent->setDateTime(GetDateTime());

	//bbc info:
	mEvent->setBbcTimeBin( muEvent->bbcTriggerDetector().onlineTimeDifference() );
	int Npmt=bbc->numberOfPMTs();
	for (int pmt=0;pmt<Npmt;pmt++){
		if(bbc->adc(pmt) > 5) {
			if(pmt<16) mEvent->setEbbc(1);
			if(pmt>23 && pmt<40) mEvent->setWbbc(1);
		}
	}

	//ZDC and BBC rates:
	mEvent->setZdcWestRate(muEvent->runInfo().zdcWestRate());
	mEvent->setZdcEastRate(muEvent->runInfo().zdcEastRate());
	mEvent->setZdcCoincidenceRate(muEvent->runInfo().zdcCoincidenceRate());
	mEvent->setBbcWestRate(muEvent->runInfo().bbcWestRate());
	mEvent->setBbcEastRate(muEvent->runInfo().bbcEastRate());
	mEvent->setBbcCoincidenceRate(muEvent->runInfo().bbcCoincidenceRate());

	// VPD Stuff:
	if (muDst->btofHeader()){
		mEvent->setVpdTdiff(muDst->btofHeader()->tDiff());
		mEvent->setVpdTstart(muDst->btofHeader()->tStart());
		mEvent->setVpdZvertex(muDst->btofHeader()->vpdVz());
		mEvent->setVpdEastHits(muDst->btofHeader()->numberOfVpdHits(east));
		mEvent->setVpdWestHits(muDst->btofHeader()->numberOfVpdHits(west));
	}
	else { // No StBTofHeader
		cout << "BAD BTOFHEADER" << endl;
		mEvent->setVpdTdiff(-999.);
		mEvent->setVpdTstart(-999.);
		mEvent->setVpdZvertex(-999.);
		mEvent->setVpdEastHits(-1);
		mEvent->setVpdWestHits(-1);
	}

	//spin specific info from Mudst:
	int bx7 = muEvent->l0Trigger().bunchCrossingId7bit(muEvent->runId());
	int bx48 =  muEvent->l0Trigger().bunchCrossingId();
	mEvent->setBx7( bx7 );
	mEvent->setBx48( bx48 );
	mEvent->setSpinBits( muEvent->l0Trigger().spinBits( muEvent->runId() ) );

	//get spin info (Yellow,Blue): Up,Up = 5; Down,Up = 6; Up,Down = 9; Down,Down = 10;
	if(isRealData) {
		StSpinDbMaker* spDbMaker = dynamic_cast<StSpinDbMaker*>(GetMakerInheritsFrom("StSpinDbMaker"));
		assert(spDbMaker);
		mEvent->setIsValid( spDbMaker->isValid() );
		mEvent->setIsPolLong( spDbMaker->isPolDirLong() );
		mEvent->setIsPolTrans( spDbMaker->isPolDirTrans() );
		int isMasked = (spDbMaker->isMaskedUsingBX48(bx48)) ? 1 : 0;
		mEvent->setIsMaskedUsingBx48( isMasked );   
		mEvent->setOffsetBx48minusBX7( spDbMaker->offsetBX48minusBX7(bx48, bx7) );  
		mEvent->setSpin4UsingBx48( spDbMaker->spin4usingBX48(bx48) );
	}
	//cout <<"sdb:\t"<<mEvent->isValid()<<"\t"<<mEvent->isPolLong()<<"\t"<<mEvent->isPolTrans()<<"\t"<<mEvent->isMaskedUsingBx48()<<"\t"<<mEvent->offsetBx48minusBX7()<<endl;

	//vertex information:
	int nVertices = muDst->numberOfPrimaryVertices();
	for(int i=0; i<nVertices; ++i){
		assert(muDst->primaryVertex(i));
		StMuPrimaryVertex* muVert = muDst->primaryVertex(i);
		assert(muVert);

		StJetSkimVert skimVert;
		copyVertex(*muVert, skimVert);
		mEvent->setVert(skimVert);
	}

	//highest ranking vertex is always in position 0
	mEvent->setBestVert(0);

	// Changes to account for the 4th threshold for di-jet triggers
	// Modified by Danny

	// EMC info
	if (StTriggerSimuMaker* trigSimu = dynamic_cast<StTriggerSimuMaker*>(GetMakerInheritsFrom("StTriggerSimuMaker"))) {
		if (trigSimu->bemc) {
			//for (int i = 0; i < 3; ++i) mEvent->setBarrelJetPatchTh(i,trigSimu->bemc->barrelJetPatchTh(i));
			// Setting the 4th threshold for BEMC
			// i = 0 (JP0)
			// i = 1 (JP1)
			// i = 2 (JP2)
			// i = 4 (dijet)
			for (int i = 0; i < 4; ++i)
			{
				mEvent->setBarrelJetPatchTh(i,trigSimu->bemc->barrelJetPatchTh(i));
				if(i == 3) mEvent->setBarrelJetPatchTh(i,trigSimu->bemc->barrelJetPatchTh(i+1));
			}
			for (int i = 0; i < 4; ++i) mEvent->setBarrelHighTowerTh(i,trigSimu->bemc->barrelHighTowerTh(i));
			for (int jp = 0; jp < 18; ++jp) mEvent->setBarrelJetPatchAdc(jp,trigSimu->bemc->barrelJetPatchAdc(jp));
		}

		if (trigSimu->eemc) {
			//for (int i = 0; i < 3; ++i) mEvent->setEndcapJetPatchTh(i,trigSimu->eemc->endcapJetPatchTh(i));
			// Setting the 4th threshold for EEMC
			// i = 0 (JP0)
			// i = 1 (JP1)
			// i = 2 (JP2)
			// i = 3 (dijet)
			for (int i = 0; i < 4; ++i)
			{
				mEvent->setEndcapJetPatchTh(i,trigSimu->eemc->endcapJetPatchTh(i));
			}
			for (int i = 0; i < 2; ++i) mEvent->setEndcapHighTowerTh(i,trigSimu->eemc->endcapHighTowerTh(i));
			for (int jp = 0; jp < 6; ++jp) mEvent->setEndcapJetPatchAdc(jp,trigSimu->eemc->endcapJetPatchAdc(jp));
		}

		if (trigSimu->emc) {
			for (int i = 0; i < 3; ++i) mEvent->setOverlapJetPatchTh(i,trigSimu->emc->overlapJetPatchTh(i));
			for (int i = 0; i < 2; ++i) {
				int jp, adc;
				trigSimu->emc->getOverlapJetPatchAdc(i,jp,adc);
				mEvent->setOverlapJetPatchAdc(jp,adc);
			}
		}

		mEvent->setEmcLayer2(trigSimu->emc->EM201output());
	}

	//fill tree
	mTree->Fill();

	return kStOk;
}

void copyVertex(StMuPrimaryVertex& v, StJetSkimVert& sv)
{
	float pos[3];
	float err[3];
	StThreeVectorF pos3 = v.position();
	StThreeVectorF err3 = v.posError();
	pos[0] = pos3.x();
	pos[1] = pos3.y();
	pos[2] = pos3.z();
	err[0] = err3.x();
	err[1] = err3.y();
	err[2] = err3.z();
	sv.setPosition(pos);
	sv.setError(err);

	//then a brute force copy:
	sv.setVertexFinderId( v.vertexFinderId());
	sv.setRanking( v.ranking() );
	sv.setNTracksUsed( v.nTracksUsed());
	sv.setNBTOFMatch( v.nBTOFMatch());
	sv.setNCTBMatch( v.nCTBMatch());
	sv.setNBEMCMatch( v.nBEMCMatch());
	sv.setNEEMCMatch( v.nEEMCMatch());
	sv.setNCrossingCentralMembrane( v.nCrossCentralMembrane() );
	sv.setSumTrackPt( v.sumTrackPt());
	sv.setMeanDip( v.meanDip() );
	sv.setChiSquared( v.chiSquared() );
	sv.setRefMultNeg( v.refMultNeg() );
	sv.setRefMultPos( v.refMultPos() );
	sv.setRefMultFtpcWest( v.refMultFtpcWest() );
	sv.setRefMultFtpcEast( v.refMultFtpcEast() );

}

void StJetSkimEventMaker::fillTriggerSimulationInfo(StJetSkimTrig &skimTrig)
{

	StTriggerSimuMaker* trigSimu = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu"));
	StEmcTriggerMaker *emcTrigMaker = dynamic_cast<StEmcTriggerMaker*>(GetMaker("bemctrigger"));

	if (trigSimu) {
		const StTriggerSimuResult& trigResult = trigSimu->detailedResult(skimTrig.trigId());
		skimTrig.setShouldFire(trigSimu->isTrigger(skimTrig.trigId()));
		skimTrig.setShouldFireBBC(trigResult.bbcDecision());
		skimTrig.setShouldFireBemc(trigResult.bemcDecision());
		skimTrig.setShouldFireEemc(trigResult.eemcDecision());
		skimTrig.setShouldFireL2(trigResult.l2Decision());

		if (trigResult.bemcDecision()==1){
			vector<short> towerId = trigResult.highTowerIds();
			for (unsigned i=0; i<towerId.size(); i++) {
				skimTrig.addTowerAboveThreshold(0,towerId[i],trigResult.highTowerAdc(towerId[i]));
			}

			vector<short> tpId = trigResult.triggerPatchIds();
			for (unsigned i=0; i<tpId.size(); i++) {
				skimTrig.addTriggerPatchAboveThreshold(0,tpId[i],trigResult.triggerPatchAdc(tpId[i]));
			}

			vector<short> jpId = trigResult.jetPatchIds();
			for (unsigned i=0; i<jpId.size(); i++) {
				skimTrig.addJetPatchAboveThreshold(0,jpId[i],trigResult.jetPatchAdc(jpId[i]));
			}
		}

		if (trigResult.l2Decision()==1) {
			// Needs to be able to handle more than just jet triggers...
			int year = GetDBTime().GetYear();
			const int* L2ResultEmulated = (int*)trigResult.l2Result(kJet,year);
			if (L2ResultEmulated) {
				skimTrig.setL2ResultEmulated(L2ResultEmulated);
			}
			else {
				LOG_WARN << "No emulated L2 result for trigger ID = " << skimTrig.trigId() << " of year = " << year << endm;
			}
		}
	} else if (emcTrigMaker) {
		skimTrig.setShouldFire(emcTrigMaker->isTrigger(skimTrig.trigId()));

		map<int,int> towerMap = emcTrigMaker->barrelTowersAboveThreshold(skimTrig.trigId());
		for(map<int,int>::const_iterator it=towerMap.begin(); it!=towerMap.end(); it++) {
			skimTrig.addTowerAboveThreshold(0,it->first, it->second);
		}

		towerMap = emcTrigMaker->endcapTowersAboveThreshold(skimTrig.trigId());
		for(map<int,int>::const_iterator it=towerMap.begin(); it!=towerMap.end(); it++) {
			skimTrig.addTowerAboveThreshold(1,it->first, it->second);
		}

		map<int,int> triggerPatchMap = emcTrigMaker->barrelTriggerPatchesAboveThreshold(skimTrig.trigId());
		for(map<int,int>::const_iterator it=triggerPatchMap.begin(); it!=triggerPatchMap.end(); it++) {
			skimTrig.addTriggerPatchAboveThreshold(0,it->first, it->second);
		}

		triggerPatchMap = emcTrigMaker->endcapTriggerPatchesAboveThreshold(skimTrig.trigId());
		for(map<int,int>::const_iterator it=triggerPatchMap.begin(); it!=triggerPatchMap.end(); it++) {
			skimTrig.addTriggerPatchAboveThreshold(1,it->first, it->second);
		}

		map<int,int> jetPatchMap = emcTrigMaker->barrelJetPatchesAboveThreshold(skimTrig.trigId());
		for(map<int,int>::const_iterator it=jetPatchMap.begin(); it!=jetPatchMap.end(); it++) {
			skimTrig.addJetPatchAboveThreshold(0,it->first, it->second);
		}

		jetPatchMap = emcTrigMaker->endcapJetPatchesAboveThreshold(skimTrig.trigId());
		for(map<int,int>::const_iterator it=jetPatchMap.begin(); it!=jetPatchMap.end(); it++) {
			skimTrig.addJetPatchAboveThreshold(1,it->first, it->second);
		}

		skimTrig.setTotalEnergy(emcTrigMaker->totalEnergy());

	} else {
		LOG_WARN << "StJetSkimEventMaker::fillTriggerSimulationInfo --- no StTriggerSimuMaker or StEmcTriggerMaker" << endm;
	}

}

void StJetSkimEventMaker::fillThresholds(StJetSkimTrigHeader &header) {

	StTriggerSimuMaker* trigSimu = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu"));
	StEmcTriggerMaker *emcTrigMaker = dynamic_cast<StEmcTriggerMaker*>(GetMaker("bemctrigger")); 

	if (trigSimu && trigSimu->bemc) { 
		header.eastBarrelTowerThreshold         = (trigSimu->bemc)->getTowerThreshold(header.trigId,15);
		header.eastBarrelTriggerPatchThreshold  = (trigSimu->bemc)->getTriggerPatchThreshold(header.trigId,15);
		header.eastBarrelJetPatchThreshold      = (trigSimu->bemc)->getJetPatchThreshold(header.trigId,15);
		header.westBarrelTowerThreshold         = (trigSimu->bemc)->getTowerThreshold(header.trigId,0);
		header.westBarrelTriggerPatchThreshold  = (trigSimu->bemc)->getTriggerPatchThreshold(header.trigId,0);
		header.westBarrelJetPatchThreshold      = (trigSimu->bemc)->getJetPatchThreshold(header.trigId,0);
	} else if (emcTrigMaker) {
		header.eastBarrelTowerThreshold         = emcTrigMaker->barrelTowerThreshold(header.trigId,2401);
		header.eastBarrelTriggerPatchThreshold  = emcTrigMaker->barrelTriggerPatchThreshold(header.trigId,150);
		header.eastBarrelJetPatchThreshold      = emcTrigMaker->barrelJetPatchThreshold(header.trigId,6);
		header.westBarrelTowerThreshold         = emcTrigMaker->barrelTowerThreshold(header.trigId);
		header.westBarrelTriggerPatchThreshold  = emcTrigMaker->barrelTriggerPatchThreshold(header.trigId);
		header.westBarrelJetPatchThreshold      = emcTrigMaker->barrelJetPatchThreshold(header.trigId);
		header.endcapTowerThreshold             = emcTrigMaker->endcapTowerThreshold(header.trigId);
		header.endcapTriggerPatchThreshold      = emcTrigMaker->endcapTriggerPatchThreshold(header.trigId);
		header.endcapJetPatchThreshold          = emcTrigMaker->endcapJetPatchThreshold(header.trigId);
		header.totalEnergyThreshold             = emcTrigMaker->totalEnergyThreshold(header.trigId);
	} else {
		LOG_WARN << "StTriggerSimuMaker/StEmcTriggMaker not in Chain..." << endm;
	}

}

Int_t StJetSkimEventMaker::Finish()
{
	assert(mOutfile);
	mOutfile->Write();
	mOutfile->Close();

	return kStOK;
}

void StJetSkimEventMaker::Clear(const Option_t* c)
{
}
