#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include <DAQ_ETOF/daq_etof.h>
#include <SFS/sfs_index.h>

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1D.h>
#include <TH2D.h>

#include <TMath.h>
#include <math.h>
#include "etofBuilder.h"
#include <RTS/include/rtsLog.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include "etofMessageFormat.h"

#include <assert.h>



ClassImp(etofBuilder);

typedef JevpPlot * ptrJevpPlot;


etofBuilder::etofBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"etof";

  int np = sizeof(contents) / sizeof(TH1 *);

  plots = new ptrJevpPlot[np]();
  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(plots, 0, sizeof(JevpPlot *) * np);
}

etofBuilder::~etofBuilder() {
  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(plots[i]) delete plots[i];
  }
  delete plots;
}

void etofBuilder::initialize(int argc, char *argv[]) {

	// Build Root Histograms...
	LOG(NOTE, "etofBuilder::initialize");

	rocMap[ 0x0b59 ] = 0; 
	rocMap[ 0x1898 ] = 1;
	rocMap[ 0x18f6 ] = 2;
	rocMap[ 0x5f64 ] = 3;
	rocMap[ 0x18e6 ] = 4;

	for ( auto kv : rocMap ){
		LOG( INFO, "%hu = %#06x", kv.second , kv.first );
		rocrMap[ kv.second ] = kv.first;
	}

	contents.nHits = new TH1F( "nHits", "ETOF # hits; # hits", 100, 0, 100 );
	contents.nEpochMismatch = new TH1F( "nEpochMismatch", "ETOF # of Epoch Mismatch / roc; roc;# Epoch Mismatch", 5, 0, 5 );
	contents.nEpochLoss = new TH1F( "nEpochLoss", "ETOF # of Epoch loss / roc; roc;# Epoch Lost", 5, 0, 5 );
	contents.nEpochSync = new TH1F( "nEpochSync", "ETOF # of Epoch Sync / roc; roc;# Epoch Sync", 5, 0, 5 );
	contents.nEpochDataLoss = new TH1F( "nEpochDataLoss", "ETOF # of Epoch Data loss / roc; roc;# Epoch Data Lost", 5, 0, 5 );
	contents.totAll = new TH1F( "totAll", "ETOF ToT of all hits; ToT", 60, 0, 60 );
	contents.hitMap = new TH2F( "hitMap", "ETOF hit map;roc;channel", 5, 0, 5, 4, 0, 4 );
	for ( size_t i = 0; i < 20; i++ ){
		int roc = i / 4;
		int chan = i % 4;
		contents.fineTime[i] = new TH1F( TString::Format("fineTime_%lu", (long unsigned int) i ), TString::Format( "ETOF Fine Time ROC %#06x, chan %d", rocrMap[ roc ], chan ), 130, 0, 130 );
	}
	
	// size_t i = 1;
	LOG(INFO, "rocrMap.size() = %lu", rocrMap.size());
	for ( size_t i = 0; i < rocrMap.size(); i++ ){
		char buf[50];
		LOG( INFO, "%#06x", rocrMap[i] );
		sprintf( buf, "%#06x", rocrMap[i] );
		contents.hitMap->GetXaxis()->SetBinLabel( i+1, buf );
		contents.nEpochMismatch->GetXaxis()->SetBinLabel( i+1, buf );
		contents.nEpochLoss->GetXaxis()->SetBinLabel( i+1, buf );
		contents.nEpochSync->GetXaxis()->SetBinLabel( i+1, buf );
		contents.nEpochDataLoss->GetXaxis()->SetBinLabel( i+1, buf );
	}
	
	int np = sizeof(contents) / sizeof(TH1 *);
	// JevpPlot *plots[np];

	// int n=0;

	for ( int i = 0; i < np; i++ ){
		contents.array[i]->SetLineColor( kBlue );		
		
		if (contents.array[i] == contents.nEpochMismatch || contents.array[i] == contents.nEpochLoss ||
		    contents.array[i] == contents.nEpochLoss     || contents.array[i] == contents.nEpochDataLoss )
			contents.array[i]->SetFillColor( kRed );

		JevpPlot *jp =  new JevpPlot( contents.array[i] );
		jp->logy = 0;
		if (contents.array[i] == contents.nHits || contents.array[i] == contents.totAll)
			jp->logy = 1;
		
		addPlot( jp );
	}

  resetAllPlots();
}

void etofBuilder::event(daqReader *rdr) {
    rtsLogLevel(INFO);
    // if(disable_builder) return;
    // assert(0);

	LOG( DBG, "-------------START EVENT----------" );
	daq_dta *dd;
	daq_etof *etof = (daq_etof*)rdr->det("etof");
	dd = etof->get("raw");

	if(!dd) return;


	

	size_t nHits = 0;
	//size_t nEpochMismatch = 0;
	while ( dd->iterate() ){

		int iInputSizeBytes = dd->ncontent;
		Int_t iInputSzLg = iInputSizeBytes / sizeof( ULong64_t );

		LOG(DBG, "iInputSizeBytes=%d", iInputSizeBytes );
		LOG(DBG, "iInputSzLg=%d = iInputSizeBytes / %d", iInputSzLg, sizeof( ULong64_t ) );

		ULong64_t * pulLongBuff = static_cast< ULong64_t * >(dd->Void);
		ULong64_t ulTrgGdpbFullTs = pulLongBuff[0];
		ULong64_t ulTrgStarFullTs = pulLongBuff[1];
		UInt_t    uStarToken      = (pulLongBuff[2] >> 32) & 0xFFF;
		UInt_t    uStarDaqCmdIn   = (pulLongBuff[2] >> 16) & 0x00F;
		UInt_t    uStarTrigCmdIn  = (pulLongBuff[2]      ) & 0x00F;
		ULong64_t fullEventStatusFlag = pulLongBuff[3];

		LOG(DBG, "ulTrgGdpbFullTs=%lu", ulTrgGdpbFullTs );
		LOG(DBG, "ulTrgStarFullTs=%lu", ulTrgStarFullTs );
		LOG(DBG, "uStarToken=%lu", uStarToken );
		LOG(DBG, "uStarDaqCmdIn=%lu", uStarDaqCmdIn );
		LOG(DBG, "uStarTrigCmdIn=%lu", uStarTrigCmdIn );
		LOG(DBG, "fullEventStatusFlag=%lu", fullEventStatusFlag );

		if ( iInputSzLg > 4){
			UInt_t uMsgsToRead = iInputSzLg - 4;
			for( UInt_t uMsgIdx = 0; uMsgIdx < uMsgsToRead; uMsgIdx++)
			{
				gdpb::FullMessage mess( pulLongBuff[4 + 2 * uMsgIdx], pulLongBuff[4 + 2 * uMsgIdx + 1] );
				// mess.PrintMessage( gdpb::msg_print_Prefix | gdpb::msg_print_Data);

				if ( mess.isGet4Hit32Msg() ){
					// roc.insert( mess.getRocNumber() );
					contents.totAll->Fill( mess.getGdpbHit32Tot() );
					nHits ++;

					if ( rocMap.count( mess.getRocNumber() ) > 0 ){
						int rocN = rocMap[mess.getRocNumber()];
						contents.hitMap->Fill( rocN, mess.getGdpbHitChanId() );
						contents.fineTime[ rocN * 4 + mess.getGdpbHitChanId() ]->Fill( mess.getGdpbHitFineTs() );
					}

				}

				if ( mess.isEpoch2Msg() ){
				  // JML  Messages every event kills the logs!


				  //LOG( INFO, "getEpoch2EpochLost=%d", mess.getEpoch2EpochLost() );
				  //	LOG( INFO, "getEpoch2DataLost=%d", mess.getEpoch2DataLost() );
				  //	LOG( INFO, "getEpoch2Sync=%d", mess.getEpoch2Sync() );
				  //	LOG(INFO, "roc=%08x, chan=%d", mess.getRocNumber(), mess.getGdpbHitChanId());


					int rocN = rocMap[mess.getRocNumber()];
					if ( mess.getEpoch2EpochMissmatch() )
						contents.nEpochMismatch->Fill( rocN );
					if ( mess.getEpoch2EpochLost() )
						contents.nEpochLoss->Fill( rocN );
					if ( mess.getEpoch2DataLost() )
						contents.nEpochDataLoss->Fill( rocN );
					if ( mess.getEpoch2Sync() )
						contents.nEpochSync->Fill( rocN );

				}

			} // message loop
		} // check if there are messages
	} // dd->iterate
		

	contents.nHits->Fill( nHits );
	// contents.nEpochMismatch->Fill( nEpochMismatch );

	rtsLogLevel(INFO);
}

void etofBuilder::startrun(daqReader *rdr) {
  resetAllPlots();
}

void etofBuilder::stoprun(daqReader *rdr) {
  
}

void etofBuilder::main(int argc, char *argv[])
{
  etofBuilder me;
  

  me.Main(argc, argv);
}

