//StJetSkimEventMaker.cxx
//M.L. Miller (MIT)
//12/06

//root
#include "TFile.h"
#include "TTree.h"

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"

//St_base
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "StChain.h"

//StEvent
#include "StEvent.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

//StSpinDb
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

//StJetMaker
#include "StJetMaker/StJetSkimEvent.h"
#include "StJetMaker/StJetSkimEventMaker.h"

void copyVertex(StMuPrimaryVertex& v, StJetSkimVert& sv);

ClassImp(StJetSkimEventMaker)

StJetSkimEventMaker::StJetSkimEventMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputName) 
: StMaker(name), muDstMaker(uDstMaker), outName(outputName), mOutfile(0), mTree(0), mEvent(0)
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
	
	//now we build the tree
	mTree = new TTree("jetSkimTree","StJetSkimEvent Tree",99);
	
	//and add a branch for our event objects
	mTree->Branch ("skimEventBranch", "StJetSkimEvent", &mEvent, 64000, 99);
	
    return kStOk;
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
	StSpinDbMaker* spDbMaker = dynamic_cast<StSpinDbMaker*>(GetMaker("spinDb"));
	assert(spDbMaker);

	StBbcTriggerDetector* bbc = &(muEvent->bbcTriggerDetector());
	assert(bbc);
	StRunInfo* runInfo = &(muEvent->runInfo()); assert(runInfo);
	assert(runInfo);
	StDetectorDbTriggerID* v = StDetectorDbTriggerID::instance();
	assert(v);

	//first cycle through triggers:
	map<int,float> prescaleMap = v->getTotalPrescales();
	for (map<int,float>::iterator it=prescaleMap.begin(); it!=prescaleMap.end(); ++it) {
		
		StJetSkimTrig skimTrig;
		skimTrig.trigId = (*it).first;
		skimTrig.prescale = v->getTotalPrescaleByTrgId(skimTrig.trigId);
		if (muEvent->triggerIdCollection().nominal().isTrigger(skimTrig.trigId)) {
			skimTrig.isSatisfied = 1;
		}
		else {
			skimTrig.isSatisfied = 0;
		}
		mEvent->setTrig(skimTrig);
		//cout <<"filled with:\t"<<skimTrig.trigId<<"\t"<<skimTrig.prescale<<"\t"<<skimTrig.isSatisfied<<endl;
	}
	
	//then get L2Results:
	TArrayI& l2Array = muEvent->L2Result();
	cout <<"l2Size:\t"<<l2Array.GetSize()<<endl;
	/*
	assert(l2Array.GetSize()==32);
	mEvent->setL2Result(l2Array.GetArray());
	 */
	mEvent->setL2Result(l2Array);

		
	//basic event/run info
	mEvent->setFill( runInfo->beamFillNumber(blue));
    mEvent->setRunId( muEvent->runId() );
	mEvent->setEventId( muEvent->eventId() );
	
	//bbc info:
	mEvent->setBbcTimeBin( muEvent->bbcTriggerDetector().onlineTimeDifference() );
	int Npmt=bbc->numberOfPMTs();
	for (int pmt=0;pmt<Npmt;pmt++){
		if(bbc->adc(pmt) > 5) {
			if(pmt<16) mEvent->setEbbc(1);
			if(pmt>23 && pmt<40) mEvent->setWbbc(1);
		}
	}
	
	//spin specific info from Mudst:
	int bx7 = muEvent->l0Trigger().bunchCrossingId7bit(muEvent->runId());
	int bx48 =  muEvent->l0Trigger().bunchCrossingId();
	mEvent->setBx7( bx7 );
	mEvent->setBx48( bx48 );
	mEvent->setSpinBits( muEvent->l0Trigger().spinBits( muEvent->runId() ) );
	
	//get spin info (Yellow,Blue): Up,Up = 5; Down,Up = 6; Up,Down = 9; Down,Down = 10;
	mEvent->setIsValid( spDbMaker->isValid() );
	mEvent->setIsPolLong( spDbMaker->isPolDirLong() );
	mEvent->setIsPolTrans( spDbMaker->isPolDirTrans() );
	int isMasked = (spDbMaker->isMaskedUsingBX48(bx48)) ? 1 : 0;
	mEvent->setIsMaskedUsingBx48( isMasked );	
	mEvent->setOffsetBx48minusBX7( spDbMaker->offsetBX48minusBX7(bx48, bx7) );	
	mEvent->setSpin4UsingBx48( spDbMaker->spin4usingBX48(bx48) );
	
	cout <<"sdb:\t"<<mEvent->isValid()<<"\t"<<mEvent->isPolLong()<<"\t"<<mEvent->isPolTrans()<<"\t"<<mEvent->isMaskedUsingBx48()<<"\t"<<mEvent->offsetBx48minusBX7()<<endl;
	
	//vertex information:
	int nVertices = muDst->numberOfPrimaryVertices();
	for(int i=0; i<nVertices; ++i){
		assert(muDst->primaryVertex(i));
		StMuPrimaryVertex* muVert = muDst->primaryVertex(i);
		assert(muVert);
		
		if (i==0) {//best vertex:
			StJetSkimVert* bestVert = mEvent->bestVert();
			assert(bestVert);
			copyVertex(*muVert, *bestVert);
		}
		
		StJetSkimVert skimVert;
		copyVertex(*muVert, skimVert);
		
		mEvent->setVert(skimVert);
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



//extra:
/*
//check the TClonesArray:
const TClonesArray* array = mEvent->triggers();
int ntotal = array->GetLast()+1;
for (int i=0; i<ntotal; ++i) {
	TObject* temp = (*array)[i];
	StJetSkimTrig& skimTrig = *(static_cast<StJetSkimTrig*>(temp));
	cout <<"retrieved with:\t"<<skimTrig.trigId<<"\t"<<skimTrig.prescale<<"\t"<<skimTrig.isSatisfied<<endl;
}
*/



