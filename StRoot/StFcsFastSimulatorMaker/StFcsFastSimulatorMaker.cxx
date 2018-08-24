// $Id: StFcsFastSimulatorMaker.cxx,v 1.1.2.1 2018/08/24 15:15:44 jwebb Exp $                                            
//                                                                                                                     
// $Log: StFcsFastSimulatorMaker.cxx,v $
// Revision 1.1.2.1  2018/08/24 15:15:44  jwebb
// Fast simulator for the forward calorimeter system.  Handles both EM and
// hadronic calorimeters.
//
//                                                                                                                     
//       Implementation of StFcsFastSimulatorMaker, the FCS fast simulator
//

#include "StFcsFastSimulatorMaker/StFcsFastSimulatorMaker.h"

//#include <algorithm>  // For std::fill(), std::max(), std::min()

#include "St_base/StMessMgr.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsHit.h"
#include "tables/St_g2t_emc_hit_Table.h"
//#include "StFcsDbMaker/StFcsDbMaker.h"

static const int N=2;     //0=Wcal, 1=Hcal
static const int NDET=2;  //0=North, 1=South
static const int nid=750;
static const char name[N][4]={"wca","hca"};

StFcsFastSimulatorMaker::StFcsFastSimulatorMaker(const Char_t* name) : StMaker(name) {
    mZSch[0]=1;
    mZSch[1]=1;
    mSF[0]=0.18;
    mSF[1]=0.01;
    mFixedGain[0]=1.0;
    mFixedGain[1]=1.0;
}

Int_t StFcsFastSimulatorMaker::Make() {
    LOG_DEBUG << "StFcsFastSimulatorMaker::Make" << endm;
    
    //if (!GetMaker("fcsDb")) {
    //  LOG_ERROR << "No StFcsDbMaker. StFcsDbMaker library not loaded?" << endm;
    //  return kStErr;
    //}  // if
    
    // Get the existing StEvent, or add one if it doesn't exist.
    StEvent* event = static_cast<StEvent*>(GetDataSet("StEvent"));
    if (!event) {        
	event = new StEvent;
	AddData(event);
	LOG_DEBUG << "Creating StEvent" << endm;
    }  
    // Add an FCS collection to the event if one does not already exist.
    if (!event->fcsCollection()) {
	event->setFcsCollection(new StFcsCollection);
	LOG_DEBUG << "Creating StFcsCollection" << endm;
    }  
    
    fillStEvent(event);
    return kStOk;
}

/* Fill an event with StFcsHits. */
void StFcsFastSimulatorMaker::fillStEvent(StEvent* event) {
    //StFcsDbMaker* dbMaker = static_cast<StFcsDbMaker*>(GetMaker("fcsDb"));
    StFcsCollection * fcscollection = event->fcsCollection();
    
    // Read the g2t table for FCS Wcal and Hcal
    for(int wh=0; wh<N; wh++){
	St_g2t_emc_hit* hitTable = static_cast<St_g2t_emc_hit*>(GetDataSet(Form("g2t_%3s_hit",name[wh])));
	if (!hitTable) {
	    LOG_INFO << Form("g2t_%3s_hit table is empty",name[wh]) << endm;
	    continue;
	}
	const Int_t nHits = hitTable->GetNRows();
	LOG_INFO << Form("g2t_%s_hit table has %d hit",name[wh],nHits) << endm;
	if(nHits<=0) continue;
	const g2t_emc_hit_st* hit = hitTable->GetTable();
	if(!hit){
	    LOG_INFO << Form("g2t_%3s_hit GetTable failed",name[wh]) << endm;
	    continue;
	}
	StPtrVecFcsHit hits; //temp storage for hits
	//table to keep pointer to hit for each det & channel
	auto map = new StFcsHit*[NDET][nid](); //no need for memset with ()
	for (Int_t i=0; i < nHits; ++i) {
	    if (!hit) continue;
	    const Int_t det = hit->volume_id / 1000 - 1;
	    const Int_t id  = hit->volume_id % 1000;
	    LOG_INFO << Form("%3s volid=%8d det=%2d ch=%4d e=%f\n",name[wh],hit->volume_id,det,id,hit->de);
	    if(det<0 || det>=NDET || id<=0 || id>=nid){
		LOG_INFO << Form("FCS %s det or id out of range det=%d id=%d (limit %d)" ,name[wh],det,id,nid) << endm;
		continue;
	    }
	    Float_t de = hit->de;
	    StFcsHit* fcshit=0;
	    int timebin=0; //for now timebin=0 for sum is assumed
	    if(map[det][id]==0){ // New hit
		Int_t crate=0, slot=0, ch=0, adc=0;
		//dbMaker->getMap(det, id, &crate, &slot, &ch);
		fcshit = new StFcsHit(det, id, crate, slot, ch, timebin, adc, de);
		hits.push_back(fcshit);
		map[det][id]=fcshit;
	    }else{ // Adding energy to old hit
		fcshit = map[det][id];
		fcshit->setEnergy(fcshit->energy() + de);
	    }
	    hit++;
	}
	delete [] map;
	int nhit=hits.size();
	float etot=0.0;
	// Loop over hits and digitize
	for(int i=0; i<nhit; i++){
	    const Int_t det = hits[i]->detectorId();
	    const Int_t id = hits[i]->id();
	    float de=hits[i]->energy();
	    float sf=mSF[wh];
	    float gain=100.0/4096.0;  //for now hardcoded to 100GeV/4095
	    float gainCorr=1.0; //for now fardcoded to be 1.0
	    //sf = dbMaker->getSamplingFraction(det, id);
	    //gain = dbMaker->getGain(det, id);
	    //gainCorr = dbMaker->getGainCorrection(det, id);
	    if(mFixedGain[wh]>0.0) gainCorr=mFixedGain[wh];
	    int adc = static_cast<Int_t>(de / (sf * gain * gainCorr));
	    adc = std::max(adc, 0);  // Prevent negative ADC
	    adc = std::min(adc, 4095*14);  // Cap maximum ADC =12bit * 14 timebin???
	    if(adc>mZSch[wh]){ // zero suppress low ADC
		float digi_energy = adc * sf * gain * gainCorr;
		hits[i]->setAdc(adc);
		hits[i]->setEnergy(digi_energy);
		fcscollection->addHit(wh,hits[i]); 
		etot += digi_energy;
		LOG_DEBUG << Form("%3s Det=%1d id=%3d dE=%8.3f SF+%6.3f gain=%6.3f corr=%6.3f ADC=%4d digiE=%8.3f\n",
				  name[wh],det,id,de,sf,gain,gainCorr,adc,digi_energy);
	    }else{
		delete hits[i];
	    }	    
	}
	LOG_INFO << Form("%s Found %d g2t hits in %d cells, created %d hits with ADC>ZS(%d) and Etot=%8.3f",
			 name[wh],nHits,nhit,fcscollection->numberOfHits(wh),mZSch[wh],etot)<<endm;
    }
}
