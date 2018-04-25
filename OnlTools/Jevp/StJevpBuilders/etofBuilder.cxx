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

	LOG(NOTE, "etofBuilder::initialize");

	// initialize rocMap
	rocMap[ 0x18f6 ] = 0; 
	rocMap[ 0x0b59 ] = 1;
	rocMap[ 0x1898 ] = 2;
	rocMap[ 0x5f64 ] = 3;
	rocMap[ 0x18e6 ] = 4;

	for ( auto kv : rocMap ){
		LOG( INFO, "%hu = %#06x", kv.second , kv.first );
		rocrMap[ kv.second ] = kv.first;
	}
	
	// initialize Get4 to PADI conversion
        int channelMap[32] = {
		3,  2,  1,  0, 
		23, 22, 21, 20,
                 7,  6,  5,  4,
                27, 26, 25, 24,
                11, 10,  9,  8,
                31, 30, 29, 28,
                15, 14, 13, 12,
                19, 18, 17, 16 };
	
	
	for(int i=0; i<32; i++) {
		Get4ToPadi.push_back( channelMap[i] );
		LOG( INFO, "channelMap (%d) = %d", i, Get4ToPadi[i]);                        
        }

        NrOfChannelsPerGet4 = 4;
        NrOfGet4PerFeb      = 8;
        NrOfChannelsPerCard = 32;                                               

		
	// initialize counter geometry map
	counterPlotMap[ 0 ] = 3;	
	counterPlotMap[ 1 ] = 0;	
	counterPlotMap[ 2 ] = 6;	
	counterPlotMap[ 3 ] = 4;	
	counterPlotMap[ 4 ] = 1;	
	counterPlotMap[ 5 ] = 7;	
	counterPlotMap[ 6 ] = 5;	
	counterPlotMap[ 7 ] = 2;	
	counterPlotMap[ 8 ] = 8;	

	for ( auto kv : counterPlotMap ){
		counterPlotrMap[ kv.second ] = kv.first;
	}

	
	// Build Root Histograms...
	contents.nHits             = new TH1D( "nHits",             "ETOF # hits; # hits",                                     250,  0, 250 );
	contents.nHitsVsTofTrgMult = new TH2D( "nHitsVsTofTrgMult", "ETOF # hits vs Tof trigger data multiplicity; # hits in eTOF; Tof mult in trigger data", 100, 0, 200, 100, 0, 800 );
        
	contents.nEpochMismatch    = new TH1D( "nEpochMismatch",    "ETOF # of Epoch Mismatch / roc; roc; # Epoch Mismatch",     5,  0,   5 );
        contents.nEpochLoss        = new TH1D( "nEpochLoss",        "ETOF # of Epoch loss / roc; roc; # Epoch Lost",             5,  0,   5 );
        contents.nEpochSync        = new TH1D( "nEpochSync",        "ETOF # of Epoch Sync / roc; roc; # Epoch Sync",             5,  0,   5 );
        contents.nEpochDataLoss    = new TH1D( "nEpochDataLoss",    "ETOF # of Epoch Data loss / roc; roc; # Epoch Data Lost",   5,  0,   5 );
        
	contents.totAll            = new TH1D( "totAll",            "ETOF ToT of all hits; ToT",                               256,  0, 256 );
        contents.hitTimeToTrigger  = new TH1D( "hitTimeToTrigger",  "ETOF hit time to Trigger; t_{hit} - t_{trigger} (#mus)",  600, -3,   3 );
        
	contents.hitMap            = new TH2D( "hitMap",            "ETOF hit map; roc; get4Id",                                 5,  0,   5, 32, 0, 32 );
        contents.hitMapChannelId   = new TH2D( "hitMapChannelId",   "ETOF hit map channel; roc; channelId",                      5,  0,   5,  4, 0,  4 );

	for( int i = 0; i < 9; i++) {
                int module  = counterPlotMap[ i ] / 3;
                int counter = counterPlotMap[ i ] % 3;
                contents.hitMapCounter[ i ] = new TH2D( TString::Format( "hitMapCounter_%d", i ), TString::Format( "ETOF hit map module %d  counter %d", module, counter ), 2, 0, 2, 32, 0, 32 );
                contents.hitMapCounter[ i ]->GetXaxis()->SetTitle( "side" );
                contents.hitMapCounter[ i ]->GetYaxis()->SetTitle( "31 - strip" );
        }

	contents.fineTimeAll   = new TH1D( "fineTimeAll",  "ETOF Fine Ts",   115, 0,  115 );
	contents.coarseTimeAll = new TH1D( "coarseTimeAll","ETOF Coarse Ts", 300, 0, 4200 );
	
	for ( size_t i = 0; i < 18; i++ ) {
		int roc  = i / 4;
		int card = i % 4;
		contents.fineTimeGet4card  [ i ] = new TH1D( TString::Format("fineTimeGet4card_%u",   (unsigned int) i ), TString::Format( "ETOF Fine Time ROC %#06x, Get4 card %d",   rocrMap[ roc ], card ), 115, 0,  115 );
		contents.coarseTimeGet4card[ i ] = new TH1D( TString::Format("coarseTimeGet4card_%u", (unsigned int) i ), TString::Format( "ETOF Coarse Time ROC %#06x, Get4 card %d", rocrMap[ roc ], card ), 300, 0, 4200 );
	}

	for ( size_t i = 0; i < 20; i++ ) {
		int roc  = i / 4;
		int chan = i % 4;
		contents.fineTime[i] = new TH1D( TString::Format("fineTime_%u", (unsigned int) i ), TString::Format( "ETOF Fine Time ROC %#06x, chan %d", rocrMap[ roc ], chan ), 115, 0, 115 );
	}
	
	
	LOG(INFO, "rocrMap.size() = %lu", rocrMap.size());
	for ( size_t i = 0; i < rocrMap.size(); i++ ){
		char buf[50];
		LOG( INFO, "%#06x", rocrMap[i] );
		sprintf( buf, "%#06x", rocrMap[i] );
                
		contents.hitMap         ->GetXaxis()->SetBinLabel( i+1, buf );
                contents.hitMapChannelId->GetXaxis()->SetBinLabel( i+1, buf );
                
		contents.nEpochMismatch ->GetXaxis()->SetBinLabel( i+1, buf );
                contents.nEpochLoss     ->GetXaxis()->SetBinLabel( i+1, buf );
                contents.nEpochSync     ->GetXaxis()->SetBinLabel( i+1, buf );
                contents.nEpochDataLoss ->GetXaxis()->SetBinLabel( i+1, buf );
	}
	
	int np = sizeof(contents) / sizeof(TH1 *);
	// JevpPlot *plots[np];
	
	for ( int i = 0; i < np; i++ ){
		contents.array[i]->SetLineColor( kBlue );		
		
		if (contents.array[i] == contents.nEpochMismatch || contents.array[i] == contents.nEpochLoss ||
		    contents.array[i] == contents.nEpochLoss     || contents.array[i] == contents.nEpochDataLoss )
			contents.array[i]->SetFillColor( kRed );

		JevpPlot *jp =  new JevpPlot( contents.array[i] );
		jp->logy = 0;

		if (contents.array[i] == contents.nHits || contents.array[i] == contents.hitTimeToTrigger || contents.array[i] == contents.totAll)
			jp->logy = 1;
		if (contents.array[i] == contents.nHitsVsTofTrgMult) {
			jp->optlogz = 1;
		}
		if (contents.array[i] == contents.hitMapCounter[0] ||
		    contents.array[i] == contents.hitMapCounter[1] ||
		    contents.array[i] == contents.hitMapCounter[2] ||
		    contents.array[i] == contents.hitMapCounter[3] ||
		    contents.array[i] == contents.hitMapCounter[4] ||
		    contents.array[i] == contents.hitMapCounter[5] ||
		    contents.array[i] == contents.hitMapCounter[6] ||
		    contents.array[i] == contents.hitMapCounter[7] ||
	    	    contents.array[i] == contents.hitMapCounter[8]
		 )	
			jp->gridx = 0;

	
		addPlot( jp );
	}

  resetAllPlots();
}

void etofBuilder::event(daqReader *rdr) {
  //rtsLogLevel(INFO);    // Nope!  This changes the log level for the server!  Don't do it!!!!!!!!!!!
  // if you wan't to modify log levels in debug mode, use the -loglevel command line argument!


    // if(disable_builder) return;
    // assert(0);

	LOG( DBG, "-------------START EVENT----------" );

        StTriggerData* trgd = getStTriggerData( rdr );        
        if( !trgd ) return;
        float TofTrgMult = (float) trgd->tofMultiplicity( 0 );


	daq_dta *dd;
	daq_etof *etof = (daq_etof*)rdr->det("etof");
	dd = etof->get("raw");

	if(!dd) {
	    if( trgd ) delete trgd;
	    return;
	}

	

	size_t nHits = 0;
	while ( dd->iterate() ){

		int iInputSizeBytes = dd->ncontent;
		Int_t iInputSzLg = iInputSizeBytes / sizeof( ULong64_t );

		LOG(DBG, "iInputSizeBytes=%d", iInputSizeBytes );
		LOG(DBG, "iInputSzLg=%d = iInputSizeBytes / %d", iInputSzLg, sizeof( ULong64_t ) );

		ULong64_t * pulLongBuff      = static_cast< ULong64_t * >(dd->Void);
		ULong64_t ulTrgGdpbFullTs    = pulLongBuff[0];
		ULong64_t ulTrgStarFullTs    = pulLongBuff[1];
		UInt_t    uStarToken         = (pulLongBuff[2] >> 32) & 0xFFF;
		UInt_t    uStarDaqCmdIn      = (pulLongBuff[2] >> 16) & 0x00F;
		UInt_t    uStarTrigCmdIn     = (pulLongBuff[2]      ) & 0x00F;
		ULong64_t fulEventStatusFlag = pulLongBuff[3];

		LOG(DBG, "ulTrgGdpbFullTs=%lu",    ulTrgGdpbFullTs );
		LOG(DBG, "ulTrgStarFullTs=%lu",    ulTrgStarFullTs );
		LOG(DBG, "uStarToken=%lu",         uStarToken );
		LOG(DBG, "uStarDaqCmdIn=%lu",      uStarDaqCmdIn );
		LOG(DBG, "uStarTrigCmdIn=%lu",     uStarTrigCmdIn );
		LOG(DBG, "fulEventStatusFlag=%lu", fulEventStatusFlag );

		if ( iInputSzLg > 4){
			UInt_t uMsgsToRead = iInputSzLg - 4;
			for( UInt_t uMsgIdx = 0; uMsgIdx < uMsgsToRead / 2; uMsgIdx++)
			{
				gdpb::FullMessage mess( pulLongBuff[4 + 2 * uMsgIdx], pulLongBuff[4 + 2 * uMsgIdx + 1] );
				// mess.PrintMessage( gdpb::msg_print_Prefix | gdpb::msg_print_Data);

				if ( mess.isGet4Hit32Msg() ){	
					unsigned int rocId      = mess.getRocNumber();
                                        unsigned int get4ChipId = mess.getGdpbGenChipId();
                                        unsigned int get4ChanId = mess.getGdpbHitChanId();

                                        // check if the reported rocId, ChipId and ChannelId make sense
                                        if( rocMap.count( rocId ) > 0 && get4ChipId < 32 && get4ChanId < 4 ) {
						nHits ++;

						contents.totAll->Fill( mess.getGdpbHit32Tot() );
					
						// calculate time difference to trigger	
						double dTriggerTime = static_cast< double_t > ( ulTrgGdpbFullTs );
                                                double dMessageFullTime = mess.GetFullTimeNs();
                                                double dTimeToTrg = ( dMessageFullTime - dTriggerTime ) / 1000.;

                                                contents.hitTimeToTrigger->Fill( dTimeToTrg );

						 
						// do some mapping from rocId, get4ChipId, get4ChanId to module, counter, Get4 cardNr ...
						//unsigned int channelNr         = get4ChipId * NrOfChannelsPerGet4 + get4ChanId;
                                                unsigned int channelNrInCard   = (get4ChipId % NrOfGet4PerFeb) * NrOfChannelsPerGet4 + get4ChanId;
                                                unsigned int cardNr            = (get4ChipId / NrOfGet4PerFeb);
                                                //unsigned int remappedChannelNr = cardNr * NrOfChannelsPerCard + Get4ToPadi[ channelNrInCard ];	

						unsigned int strip = Get4ToPadi[ channelNrInCard ];
                                                unsigned int side  = 0;
                                                
						if( cardNr % 2 == 1) {
                                                        strip = 31 - strip;
                                                        side  = 1;
                                                }
						
						unsigned int counterIndex; 						
						if( cardNr / 2 == 0 ) {
							if( rocMap[ rocId ] == 0 )      counterIndex = 2;
							else if( rocMap[ rocId ] == 1 ) counterIndex = 0;
							else if( rocMap[ rocId ] == 2 ) counterIndex = 5;
							else if( rocMap[ rocId ] == 3 ) counterIndex = 8;
							else if( rocMap[ rocId ] == 4 ) counterIndex = 6;
						}
						else {
							if( rocMap[ rocId ] == 0 )      counterIndex = 1;
							else if( rocMap[ rocId ] == 1 ) counterIndex = 3;
							else if( rocMap[ rocId ] == 2 ) counterIndex = 4;
							else if( rocMap[ rocId ] == 3 ) counterIndex = 7;
						}

						contents.hitMap         ->Fill( rocMap[ rocId ], get4ChipId );
                                                contents.hitMapChannelId->Fill( rocMap[ rocId ], get4ChanId );
						
						contents.hitMapCounter[ counterPlotrMap[counterIndex] ]->Fill( side, 31 - strip );
						

						// fine & coarse time histograms
						int fineTs   = mess.getGdpbHitFullTs() % 112;
						int coarseTs = mess.getGdpbHitFullTs() / 112;
						contents.fineTimeAll  ->Fill( fineTs   );
						contents.coarseTimeAll->Fill( coarseTs );

						contents.fineTimeGet4card  [ rocMap[ rocId ] * 4 + cardNr ]->Fill( fineTs );
						contents.coarseTimeGet4card[ rocMap[ rocId ] * 4 + cardNr ]->Fill( coarseTs );
						
						contents.fineTime[ rocMap[ rocId ] * 4 + get4ChanId ]->Fill( fineTs );
					} 
					else {
						LOG( DBG, "some id is out of range: rocId %d    get4ChipId %d    get4ChannelId %d", rocId, get4ChipId, get4ChanId );
						//mess.PrintMessage( gdpb::msg_print_Prefix | gdpb::msg_print_Data);
					}
				} // end of things to do with hit message

				if ( mess.isEpoch2Msg() ){
				  // JML  Messages every event kills the logs!


				  //LOG( INFO, "getEpoch2EpochLost=%d", mess.getEpoch2EpochLost() );
				  //LOG( INFO, "getEpoch2DataLost=%d", mess.getEpoch2DataLost() );
				  //LOG( INFO, "getEpoch2Sync=%d", mess.getEpoch2Sync() );
				  //LOG( INFO, "roc=%08x, chan=%d", mess.getRocNumber(), mess.getGdpbHitChanId());


					int rocN = rocMap[mess.getRocNumber()];
					if ( mess.getEpoch2EpochMissmatch() )
						contents.nEpochMismatch->Fill( rocN );
					if ( mess.getEpoch2EpochLost() )
						contents.nEpochLoss->Fill( rocN );
					if ( mess.getEpoch2DataLost() )
						contents.nEpochDataLoss->Fill( rocN );
					if ( mess.getEpoch2Sync() )
						contents.nEpochSync->Fill( rocN );

				} // end of things to do with epoch2 message

			} // end message loop
		} // check if there are messages
	} // dd->iterate
	if( trgd ) delete trgd;
			
	contents.nHits->Fill( nHits );
	contents.nHitsVsTofTrgMult->Fill( nHits, TofTrgMult );

	//rtsLogLevel(INFO);    // Nope!  This changes the log level for the server.  Don't do it!!!!!!!
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

