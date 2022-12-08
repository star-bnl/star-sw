/***************************************************************************
 * Author: Akio Ogawa, Minho Kim, Seunghwan Lee
 ***************************************************************************
 *
 * Description: This is the Hit maker for the RHICf data. 
 *
 ***************************************************************************
 * $Id: StRHICfRawHitMaker.cxx,v 1.4 2017/01/30 18:10:16 akio Exp $
 * $Log: StRHICfRawHitMaker.cxx,v $
 ***************************************************************************/

#include "StRHICfRawHitMaker.h"

#include "StEvent/StEventTypes.h"
#include "St_base/StMessMgr.h"
#include "St_base/Stypes.h"
#include "RTS/src/DAQ_RHICF/daq_rhicf.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StChain/StRtsTable.h"
#include "StEvent/StEvent.h"

#include "StEvent/StEnumerations.h"
#include "StRHICfDbMaker/StRHICfDbMaker.h"
#include "StEvent/StRHICfCollection.h"
#include "StEvent/StRHICfRawHit.h"

StRHICfRawHitMaker::StRHICfRawHitMaker(const Char_t* name) : StRTSBaseMaker( "rhicf", name ) 
{
}

StRHICfRawHitMaker::~StRHICfRawHitMaker()
{
}

Int_t StRHICfRawHitMaker::InitRun(Int_t runNumber)
{
	LOG_INFO << "StRHICfRawHitMaker::InitRun with run = "  << runNumber << endm;
	checkRunTypeForRHICf2017(runNumber);

	mRHICfDbMaker = static_cast<StRHICfDbMaker*>(GetMaker("rhicfDb")); 
	if(!mRHICfDbMaker){
		LOG_ERROR  << "StRHICfRawHitMaker::InitRun Failed to get StRHICfDbMaker" << endm;
		return kStFatal;
	}

	return kStOK;
}

Int_t StRHICfRawHitMaker::Make()
{
	LOG_INFO <<"StRHICfRawHitMaker::Make()" <<endm;

	StEvent* eventPtr=0;
	eventPtr= (StEvent*)StRTSBaseMaker::GetInputDS("StEvent");

	if(eventPtr){
		LOG_INFO <<"found StEvent" <<endm;
		mRHICfCollection=eventPtr->rhicfCollection();
		if(mRHICfCollection==0) LOG_INFO <<"No RHICfCollection in StEvent" <<endm;
		else{setupRHICfCollection();}
	}
	else{
		LOG_ERROR <<"found no StEvent"<<endm;
		return kStFatal;
	}
	if(!mRHICfCollection){
		LOG_INFO <<"creating new StRHICfCollection" <<endm;
		mRHICfCollection=new StRHICfCollection;
		if(!mRHICfCollection){
			LOG_ERROR <<"could not create StRHICfCollection" <<endm;
			return kStFatal;
		}
		LOG_INFO <<"setting new StRHICfCollection to StEvent" <<endm;

		setupRHICfCollection();
		eventPtr->setRHICfCollection(mRHICfCollection);
	}

	if(mRHICfCollection==0) LOG_INFO <<"No RHICfCollection in StEvent" <<endm; 

	StRtsTable *daqData = GetNextRaw();
	if(daqData){
		unsigned int* gendata = (unsigned int*) daqData->GetTable();
		unsigned short* rawdata = (unsigned short*) daqData->GetTable();

		// Insert general data
		mRHICfCollection -> setRunNumber(gendata[0]);
		mRHICfCollection -> setEventNumber(gendata[1]);
		mRHICfCollection -> setRunType(getRunType());

		unsigned int bunchIdx = mRHICfDbMaker->getBunchNumberAddress();
		unsigned int triggerIdx = mRHICfDbMaker->getTriggerAddress();
		unsigned int runTRGMIdx = mRHICfDbMaker->getRunTRGMAddress();
		
		mRHICfCollection -> setBunchNumber(gendata[bunchIdx]);
		mRHICfCollection -> setTriggerNumber(gendata[triggerIdx]);
		mRHICfCollection -> setRunTRGM(gendata[runTRGMIdx]);

		for(int i=0; i<2; i++){
			unsigned int runTimeIdx = mRHICfDbMaker->getRunTimeAddress(i);
			mRHICfCollection -> setRunTime(i, gendata[runTimeIdx]);
		}

		// Insert raw data for Plate, GSOBar
		for(int it=0; it<kRHICfNtower; it++){
			for(int ip=0; ip<kRHICfNplate; ip++){
				for(int ir=0; ir<kRHICfNrange; ir++){
					unsigned int plateADCIdx = mRHICfDbMaker -> getAdcAddress(it, ip, ir);
					unsigned int plateADCDIdx = mRHICfDbMaker -> getAdcDAddress(it, ip, ir);
					mRHICfRawHitColl -> setPlateADC(it, ip, ir, rawdata[plateADCIdx]);
					mRHICfRawHitColl -> setPlateADCDelay(it, ip, ir, rawdata[plateADCDIdx]);
				}
			}

			for(int il=0; il<kRHICfNlayer; il++){
				for(int ixy=0; ixy<kRHICfNxy; ixy++){
					Int_t gsobarSize = checkGSOBarSize(it);
					for(int ich=0; ich<gsobarSize; ich++){
						unsigned int gsobarADCIdx = mRHICfDbMaker -> getScifiAddress(it, il, ixy, ich);
						mRHICfRawHitColl -> setGSOBarADC(it, il, ixy, ich, rawdata[gsobarADCIdx]);
					}
				}
			}
		}

		// Insert raw data
		for(Int_t idx=0; idx<kRHICfNtdc; idx++){
			if(idx<kRHICfNcad0){
				// for CAD0
				unsigned int cad0Idx = mRHICfDbMaker->getCAD0Address(idx);
				mRHICfRawHitColl -> setCAD0(idx, gendata[cad0Idx]);
			}
			if(idx<kRHICfNgpi0){
				// for GPI0
				unsigned int gpi0Idx = mRHICfDbMaker->getGPI0Address(idx);
				mRHICfRawHitColl -> setGPI0(idx, gendata[gpi0Idx]);
			}
			if(idx<kRHICfNgpi1){
				// for GPI1
				unsigned int gpi1Idx = mRHICfDbMaker->getGPI1Address(idx);
				mRHICfRawHitColl -> setGPI1(idx, gendata[gpi1Idx]);
			}
			// for TDC
			unsigned int tdcIdx = mRHICfDbMaker->getTDCAddress(idx);
			mRHICfRawHitColl -> setTDC(idx, gendata[tdcIdx]);
		}
	}
	else{LOG_ERROR <<"No RHICf data found" <<endm;}

	return kStOK;
}

Int_t StRHICfRawHitMaker::Finish()
{
	return kStOk;
}

Int_t StRHICfRawHitMaker::setupRHICfCollection()
{
	mRHICfRawHitColl = mRHICfCollection -> rawHitCollection();
	if(!mRHICfRawHitColl){return kStFatal;}
	return kStOk;
}

ClassImp(StRHICfRawHitMaker)