#include "StStgcQAMaker.h"

#include "StEvent/StEvent.h"
#include "StEvent/StEventInfo.h"
#include "StEvent/StStgcCollection.h"
#include "StEvent/StFtsStgcHit.h"


#include "StChain/StChainOpt.h" // for renaming the histogram file

#include <iostream>
// ROOT
#include "TH1F.h"
#include "TH2F.h"

Int_t StStgcQAMaker::Make(){

	StEvent* event = (StEvent*)GetInputDS("StEvent");
	if(!event) {
		LOG_ERROR << "StStgcQAMaker::Make did not find StEvent"<<endm; 
		return kStErr;
	}
	
	StStgcCollection * stgcColl = event->stgcCollection();
	if(!stgcColl) {
		LOG_ERROR << "StStgcQAMaker::Make did not find StEvent->StStgcCollection"<<endm; 
		return kStErr;
	}

	
	LOG_INFO << "Processing Run : " << event->runId() << ", event : " << event->info()->id() << endm; 
	LOG_INFO << "FOUND " << stgcColl->numberOfHits(0) << " sTGC Hits" << endm;


	for ( auto h : stgcColl->hits(0) ){
		LOG_INFO << "hit : " << h->rdo() << endm;

		UInt_t dId = mStgcDbMaker->toId( h->fee(), h->altro(), h->ch() );
		LOG_INFO << "dId = " << dId << endm;
		Int_t stripIndex = mStgcDbMaker->stripIndex( dId );
		LOG_INFO << "stripIndex = " << stripIndex << endm;

		auto stripDir = mStgcDbMaker->stripOrientation( dId );

		string sstripDir = "H";
		if ( kStgcVerticalStrips == stripDir )
			sstripDir = "V";

		string prefix = Form( "det%d", 0 );

		string name = prefix + sstripDir + "_adcVsTb";

		for ( int tb_index = 0; tb_index < h->nTimebins(); tb_index++ ){
			mHistograms[ name ]->Fill( h->timebin( tb_index ), h->adc( tb_index ) );

			name = prefix + sstripDir + "_TbVsStrip";
			static_cast<TH2*>(mHistograms[ name ])->Fill( stripIndex, h->timebin( tb_index ) );
		}
	}


	return kStOK;
}


void StStgcQAMaker::setHistFileName( std::string extension )
{
	if( GetChainOpt()->GetFileOut() != nullptr ) {
		TString outFile = GetChainOpt()->GetFileOut();

		mQAHistFileName = ( string ) outFile;

		// get rid of .root
		size_t lastindex = mQAHistFileName.find_last_of( "." );
		mQAHistFileName = mQAHistFileName.substr( 0, lastindex );

		// get rid of .MuDst or .event if necessary
		lastindex = mQAHistFileName.find_last_of( "." );
		mQAHistFileName = mQAHistFileName.substr( 0, lastindex );

		// get rid of directories
		lastindex = mQAHistFileName.find_last_of( "/" );
		mQAHistFileName = mQAHistFileName.substr( lastindex + 1, mQAHistFileName.length() );

		mQAHistFileName = mQAHistFileName + extension;
		LOG_INFO << "Set HistFilename: " << mQAHistFileName << endm;
	}
	else {
		LOG_ERROR << "Cannot set the output filename for histograms" << endm;
	}
}


void StStgcQAMaker::BookHistograms(){

	for ( int det_index = 0; det_index < kStgcNDet; det_index++ ){
		string prefix = Form( "det%d", det_index );
		string name = prefix + "H_adcVsTb";
		mHistograms[ name ] = new TH2F( name.c_str(), ";TB;ADC", 50, 0, 50, 1024, 0, 1024 );
		name = prefix + "V_adcVsTb";
		mHistograms[ name ] = new TH2F( name.c_str(), ";TB;ADC", 50, 0, 50, 1024, 0, 1024 );

		name = prefix + "V_TbVsStrip";
		mHistograms[ name ] = new TH2F( name.c_str(), ";Strip;TB", 100, 0, 100, 60, 0, 60 );
		name = prefix + "H_TbVsStrip";
		mHistograms[ name ] = new TH2F( name.c_str(), ";Strip;TB", 100, 0, 100, 60, 0, 60 );

	}
	
}