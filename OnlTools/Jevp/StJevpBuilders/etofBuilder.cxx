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



ClassImp( etofBuilder );

typedef JevpPlot* ptrJevpPlot;


etofBuilder::etofBuilder( JevpServer *parent ) : JevpBuilder( parent ) {
  plotsetname = ( char* ) "etof";

  int np = sizeof( contents ) / sizeof( TH1* );

  plots = new ptrJevpPlot[ np ]();

  // start with histograms undefined...
  memset( &contents, 0, sizeof( contents ) );
  memset( plots, 0, sizeof( JevpPlot* ) *np );
}

etofBuilder::~etofBuilder() {
  // Delete any existing histograms...
  int n = sizeof( contents ) / sizeof( TH1* );
  for( int i=0; i<n; i++ ) {
    if( plots[ i ] ) delete plots[ i ];
  }
  delete plots;
}

void etofBuilder::initialize( int argc, char *argv[] ) {

    LOG( NOTE, "etofBuilder::initialize" );

    nrOfGdpbInSys       = 12;
    nrOfGbtxPerGdpb     =  6;
    nrOfFeePerGbtx      =  5;
    nrOfGet4PerFee      =  8;
    nrOfChannelsPerGet4 =  4;

    nrOfChannelsPerFee  = nrOfChannelsPerGet4 * nrOfGet4PerFee;
    nrOfGet4PerGbtx     = nrOfGet4PerFee      * nrOfFeePerGbtx;
    nrOfChannelsPerGdpb = nrOfChannelsPerGet4 * nrOfGet4PerGbtx * nrOfGbtxPerGdpb;

    // map form Id (hex number) of the GDPB to the running number ( 0 to nrOfGdpb-1 )
    gdpbMap2018[ 0x18f6 ] = 0;
    gdpbMap2018[ 0x0b59 ] = 1;
    gdpbMap2018[ 0x1898 ] = 2;
    gdpbMap2018[ 0x5f64 ] = 3;
    gdpbMap2018[ 0x18e6 ] = 4;

    gdpbMap2019[ 0x1970 ] = 0;
    gdpbMap2019[ 0x18f6 ] = 1;
    gdpbMap2019[ 0x0b59 ] = 2;
    gdpbMap2019[ 0x1898 ] = 3;
    gdpbMap2019[ 0x5bb0 ] = 4; //gdpbMap2019[ 0x5f64 ] = 4;
    gdpbMap2019[ 0x18e6 ] = 5;
    gdpbMap2019[ 0x6141 ] = 6;
    gdpbMap2019[ 0x0b5c ] = 7;
    gdpbMap2019[ 0x1861 ] = 8;
    gdpbMap2019[ 0x0b39 ] = 9;
    gdpbMap2019[ 0x5f71 ] = 10;
    gdpbMap2019[ 0x18b6 ] = 11;

    for ( auto kv : gdpbMap2018 ) {
        gdpbRevMap2018[  kv.second ] = kv.first;
    }
    for ( auto kv : gdpbMap2019 ) {
        gdpbRevMap2019[  kv.second ] = kv.first;
    }

    gdpbMap    = gdpbMap2019;
    gdpbRevMap = gdpbRevMap2019;


    vector<int> sector;
    sector.push_back( 13 );
    sector.push_back( 14 );
    sector.push_back( 15 );
    sector.push_back( 16 );
    sector.push_back( 17 );
    sector.push_back( 18 );
    sector.push_back( 19 );
    sector.push_back( 20 );
    sector.push_back( 21 );
    sector.push_back( 22 );
    sector.push_back( 23 );
    sector.push_back( 24 );

    // initialize the eLink to Get4 conversion
    int eLinkToGet4Array[ 40 ] = {
        27,  2,  7,  3, 31, 26, 30,  1,
        33, 37, 32, 13,  9, 14, 10, 15,
        17, 21, 16, 35, 34, 38, 25, 24,
         0,  6, 20, 23, 18, 22, 28,  4,
        29,  5, 19, 36, 39,  8, 12, 11 };


    for( size_t i = 0; i < nrOfGet4PerGbtx; i++ ) {
        eLinkToGet4.push_back( eLinkToGet4Array[ i ] );
        LOG( DBG, "elinkToGet4 (%d) = %d\n", i, eLinkToGet4[ i ] );
    }

    // initialize Get4 to PADI channel conversion
    int get4ToPadiArray[ 32 ] = {
         3,  2,  1,  0, 
        23, 22, 21, 20,
         7,  6,  5,  4,
        27, 26, 25, 24,
        11, 10,  9,  8,
        31, 30, 29, 28,
        15, 14, 13, 12,
        19, 18, 17, 16 };


    for( size_t i = 0; i < nrOfChannelsPerFee; i++ ) {
        get4ToPadi.push_back( get4ToPadiArray[ i ] );
        LOG( DBG, "get4ToPadi (%d) = %d\n", i, get4ToPadi[ i ] );
    }


    // Build Root Histograms...
    contents.nDigis             = new TH1D( "nDigis", "nHitMsg;# digis;# events", 300, 0, 300 );

    contents.nDigisVsTofTrgMult = new TH2D( "nDigisVsTofTrgMult", "# digis vs bTof multiplicity;# digis;bTof mult in trigger data", 300, 0, 300, 200, 0, 800 ); 

    contents.digiTot            = new TH1D( "digiTot",           "digi tot;tot (bins);#digis", 256,  0,  256 );
    contents.digiTimeToTrigger  = new TH1D( "digiTimeToTrigger", "digi time to trigger;time to trigger (ms);# digis", 600, -5, 5 );

    contents.digiCoarseTs       = new TH1D( "digiCoarseTs", "digi coarse Ts;coarse Ts (bins);# digis", 4096,   0, 4096 );
    contents.digiFineTs         = new TH1D( "digiFineTs",   "digi fine Ts;fine Ts (bins);# digis",     112,   0,  112 );


    for( size_t i=0; i<nrOfGdpbInSys; i++ ) {
        contents.nDigisPerGdpb[ i ]                  = new TH1D( TString::Format( "nDigisPerGdpb_%d", i ),  TString::Format( "# digis on Gdpb %#06x (sector %d);# digis;# events",  gdpbRevMap[ i ], sector[ i ] ),  50, 0,  50 );
        contents.digiTotPerGdpb[ i ]                 = new TH1D( TString::Format( "digiTotPerGdpb_%d", i ), TString::Format( "digi tot on Gdpb %#06x (sector %d);# digis;# events", gdpbRevMap[ i ], sector[ i ] ), 256, 0, 256 );
        contents.digiMappedChannelNumberPerGdpb[ i ] = new TH1D( TString::Format( "digiMappedChannelNumberPerGdpb_%d", i), TString::Format( "mapped channel number on Gdpb %#06x (sector %d);mapped channel;# digis", gdpbRevMap[ i ], sector[ i ] ), 961, -1, 960 );
    }


    int np = sizeof( contents ) / sizeof( TH1* );

    for( int i = 0; i < np; i++ ) {
        contents.array[ i ]->SetLineColor( kBlue );
        //contents.array[ i ]->SetFillColor( kRed );

        JevpPlot* jp = new JevpPlot( contents.array[ i ] );
        jp->logy = 1;


        if( contents.array[ i ] == contents.nDigisVsTofTrgMult ||
            contents.array[ i ] == contents.digiCoarseTs       ||
            contents.array[ i ] == contents.digiCoarseTs       ) {
            jp->logy = 0;
        }

        if( contents.array[ i ] == contents.nDigisVsTofTrgMult ) {
            jp->optlogz = 1;
        }
        /*
        if (contents.array[i] == contents.hitMapCounter[0] ||
            contents.array[i] == contents.hitMapCounter[1] ||
            contents.array[i] == contents.hitMapCounter[2] ||
            contents.array[i] == contents.hitMapCounter[3] ||
            contents.array[i] == contents.hitMapCounter[4] ||
            contents.array[i] == contents.hitMapCounter[5] ||
            contents.array[i] == contents.hitMapCounter[6] ||
            contents.array[i] == contents.hitMapCounter[7] ||
            contents.array[i] == contents.hitMapCounter[8] ) {
            jp->gridx = 0;
        }
        */

        addPlot( jp );
    }

    resetAllPlots();
}

void etofBuilder::event( daqReader *rdr ) {
    LOG( DBG, "-------------START EVENT----------" );

    StTriggerData* trgd = getStTriggerData( rdr );
    if( !trgd ) return;

    float TofTrgMult = (float) trgd->tofMultiplicity( 0 );


    daq_dta  *dd;
    daq_etof *etof = ( daq_etof* ) rdr->det( "etof" );
    dd = etof->get( "raw" );

    if( !dd ) {
        if( trgd ) delete trgd;
        return;
    }

    map< unsigned int, unsigned short > nDigisInGdpb;
    for( const auto& kv: gdpbMap ) {
        nDigisInGdpb[ kv.first ] = 0;
    }

    while ( dd->iterate() ) {

        unsigned int inputSizeBytes = dd->ncontent;
        unsigned int inputLength = inputSizeBytes / sizeof( uint64_t );

        LOG( DBG, "inputSizeBytes=%d", inputSizeBytes );
        LOG( DBG, "inputLength=%d = inputSizeBytes / %d", inputLength, sizeof( uint64_t ) );

        uint64_t* messageBuffer = static_cast< uint64_t* > ( dd->Void );

        uint64_t     trgGdpbFullTs   =   messageBuffer[ 0 ];
        uint64_t     trgStarFullTs   =   messageBuffer[ 1 ];
        unsigned int starToken       = ( messageBuffer[ 2 ] >> 32 ) & 0xFFF;
        unsigned int starDaqCmdIn    = ( messageBuffer[ 2 ] >> 16 ) & 0x00F;
        unsigned int starTrigCmdIn   = ( messageBuffer[ 2 ]       ) & 0x00F;
        uint64_t     eventStatusFlag =   messageBuffer[ 3 ];

        LOG( DBG, "trgGdpbFullTs=%lu",    trgGdpbFullTs   );
        LOG( DBG, "trgStarFullTs=%lu",    trgStarFullTs   );
        LOG( DBG, "starToken=%lu",        starToken       );
        LOG( DBG, "starDaqCmdIn=%lu",     starDaqCmdIn    );
        LOG( DBG, "starTrigCmdIn=%lu",    starTrigCmdIn   );
        LOG( DBG, "eventStatusFlag=%lu",  eventStatusFlag );

        // ------------------------------------------------------------------------------------------------------
        // determine number of full messages to read:
        // the packed version sent to the STAR DAQ systems is made of a 256 bit header (4 long unsigned integers)
        // followed by a buffer of gdpb::FullMessage (128 bit each).
        //
        // gdbp::Message     = 64 bit message as received from the eTOF gDPB boards
        // gdpb::FullMessage = 128 bit message, obtained by combining a
        //                     64 bit extended epoch (bits 127-64) with a gdpb::Message (bits 63-0)
        // ------------------------------------------------------------------------------------------------------

         // check if there are messages & process them
        if( inputLength > 4 ) {
            size_t nFullMessagesToRead = ( ( inputSizeBytes / sizeof( uint64_t ) ) - 4 ) / 2;

            if( year == 2018 ) {
                processMessages2018( messageBuffer, nFullMessagesToRead, nDigisInGdpb );
            }
            else{
                processMessages( messageBuffer, nFullMessagesToRead, nDigisInGdpb );
            }
        }

    } // dd->iterate

    if( trgd ) delete trgd;

    size_t nDigis = 0;
    for( size_t i=0; i<nrOfGdpbInSys; i++ ) { 
        if( gdpbRevMap.count( i ) > 0 ) {
            contents.nDigisPerGdpb[ i ]->Fill( nDigisInGdpb.at( gdpbRevMap.at( i ) ) );

            nDigis += nDigisInGdpb.at( gdpbRevMap.at( i ) );
        }
    }

    contents.nDigis->Fill( nDigis );
    contents.nDigisVsTofTrgMult->Fill( nDigis, TofTrgMult );
}


void etofBuilder::startrun( daqReader *rdr ) {

  //  There can be events with no trigger data!
  //  in special runs!
  //
  //
  //year = (int) getStTriggerData( rdr )->year();
  year = (rdr->run / 1000000) + 1999;
  LOG(DBG, "rdr->run=%d year=%d", rdr->run, year);
    
    if( year == 2018 ) {
        gdpbMap    = gdpbMap2018;
        gdpbRevMap = gdpbRevMap2018;

        for( size_t i = 0; i<nrOfGdpbInSys; i++ ) {
            if( i < gdpbRevMap.size() ) {
                contents.nDigisPerGdpb[ i ] ->SetTitle( TString::Format( "# digis on Gdpb %#06x (sector %d);# digis;# events",  gdpbRevMap[ i ], 18 ) );
                contents.digiTotPerGdpb[ i ]->SetTitle( TString::Format( "digi tot on Gdpb %#06x (sector %d);# digis;# events", gdpbRevMap[ i ], 18 ) );
                contents.digiMappedChannelNumberPerGdpb[ i ]->SetTitle( TString::Format( "mapped channel number on Gdpb %#06x (sector %d);mapped channel;# digis", gdpbRevMap[ i ], 18 ) );
            }
            else {
                contents.nDigisPerGdpb[ i ]                 ->SetTitle( TString::Format( "not existing ...;# digis;# events" ) );
                contents.digiTotPerGdpb[ i ]                ->SetTitle( TString::Format( "not existing ...;# digis;# events" ) );
                contents.digiMappedChannelNumberPerGdpb[ i ]->SetTitle( TString::Format( "not existing ...;# digis;# events" ) );
            }
        }
    }
    else {
        gdpbMap    = gdpbMap2019;
        gdpbRevMap = gdpbRevMap2019;
    }

    resetAllPlots();
}


void etofBuilder::stoprun( daqReader *rdr ) {
  
}


void etofBuilder::main( int argc, char *argv[] ) {
  etofBuilder me;

  me.Main( argc, argv );
}


void etofBuilder::processMessages( uint64_t* messageBuffer, size_t nFullMessagesToRead, map<unsigned int, unsigned short>& nDigisPerGdpb ) {
    LOG( DBG, " # of full messages to read: %d", nFullMessagesToRead );

    for( size_t msgIndex = 0; msgIndex < nFullMessagesToRead; msgIndex++ ) {
        gdpbv100::FullMessage mess( messageBuffer[ 4 + 2 * msgIndex ], messageBuffer[ 4 + 2 * msgIndex + 1 ] );
        //mess.PrintMessage( gdpbv100::msg_print_Prefix | gdpbv100::msg_print_Data );

        if( mess.isStarTrigger() ) {
            unsigned int gdpb = mess.getGdpbGenGdpbId();                    
            if( mess.getStarTrigMsgIndex() == 0 ) {
                LOG( DBG, "--> STAR trigger message from AFCK %#06x", gdpb );
            }
        }
        else if( mess.isHitMsg() ) {
            unsigned int gdpb = mess.getGdpbGenGdpbId();
            unsigned int chip = mess.getGdpbGenChipId();
            unsigned int chan = mess.getGdpbHitChanId();

            // remove completely empty messages (128 bits of 0)
            if( gdpb == 0 && chip == 0 && chan == 0 ) {
                LOG( DBG, "empty message (?): %lu %lu", messageBuffer[ 4 + 2 * msgIndex ], messageBuffer[ 4 + 2 * msgIndex + 1 ] );
                continue;
            }

            // check if the reported chip && channel ids make sense
            if( gdpbMap.count( gdpb ) > 0 && chip < 256 && chan < 4 ) {

                if( mess.getGdpbHit32DllLck() != 1 ) {
                    LOG( DBG, " *** warning: DLL Lock = 0 " );
                }

                unsigned int mappedChannelNumber = hardwareMapChannelNumber( chip, chan );

                LOG( DBG, "--> hit message from AFCK %#06x with mappedChannelNr %d", gdpb, mappedChannelNumber );

                if( gdpbMap.count( gdpb ) ) {
                    contents.digiMappedChannelNumberPerGdpb[ gdpbMap.at( gdpb ) ]->Fill( mappedChannelNumber );
                }

                nDigisPerGdpb.at( gdpb ) += 1;

                // calculate time difference to trigger 
                double triggerTime   = messageBuffer[ 0 ] * gdpbv100::kdClockCycleSizeNs;
                double digiFullTime  = mess.GetFullTimeNs();
                double timeToTrigger = ( digiFullTime - triggerTime ) / 1000.;

                contents.digiTimeToTrigger->Fill( timeToTrigger );


                unsigned int tot = mess.getGdpbHit32Tot();

                contents.digiTot->Fill( tot );

                if( gdpbMap.count( gdpb ) ) {
                    contents.digiTotPerGdpb[ gdpbMap.at( gdpb ) ]->Fill( tot);
                }


                // fine & coarse time
                int fineTs   = mess.getGdpbHitFullTs() % 112;
                int coarseTs = mess.getGdpbHitFullTs() / 112;

                contents.digiFineTs  ->Fill( fineTs   );
                contents.digiCoarseTs->Fill( coarseTs );
            }
            else {
                LOG( DBG, "some id is out of range: gdpbId %#06x    chip %d    channel %d", gdpb, chip, chan );
                //mess.PrintMessage( gdpbv100::msg_print_Prefix | gdpbv100::msg_print_Data );
            }
        }
        else if( mess.isEpochMsg() ) {
            LOG( DBG, "EPOCH message" );
            //mess.PrintMessage( gdpbv100::msg_print_Prefix | gdpbv100::msg_print_Data );
        }
        else if( mess.isGet4SlCtrMsg() ) {
            LOG( DBG, "GET4 slow control message" );
            //mess.PrintMessage( gdpbv100::msg_print_Prefix | gdpbv100::msg_print_Data );
        }
        else if( mess.isSysMsg() ) {
            LOG( DBG, "SYSTEM message" );
            //mess.PrintMessage( gdpbv100::msg_print_Prefix | gdpbv100::msg_print_Data );
        }
    } // message loop
}


unsigned int etofBuilder::hardwareMapElinkGet4Id( const unsigned int& chip ) {
    return eLinkToGet4.at( chip % nrOfGet4PerGbtx ) + nrOfGet4PerGbtx * ( chip / nrOfGet4PerGbtx );
}

unsigned int etofBuilder::hardwareMapChannelNumberInFee( const unsigned int& chip, const unsigned int& chan ) {
    return ( hardwareMapElinkGet4Id( chip ) % nrOfGet4PerFee ) * nrOfChannelsPerGet4 + chan;
}

unsigned int etofBuilder::hardwareMapFeeNumber( const unsigned int& chip ) {
    return hardwareMapElinkGet4Id( chip ) / nrOfGet4PerFee;
}

unsigned int etofBuilder::hardwareMapChannelNumber( const unsigned int& chip, const unsigned int& chan ) {
    return hardwareMapFeeNumber( chip ) * nrOfChannelsPerFee + get4ToPadi.at( hardwareMapChannelNumberInFee( chip, chan ) );
}





void etofBuilder::processMessages2018( uint64_t* messageBuffer, size_t nFullMessagesToRead, map<unsigned int, unsigned short>& nDigisPerGdpb ) {
    LOG( DBG, " # of full messages to read: %d", nFullMessagesToRead );

    for( size_t msgIndex = 0; msgIndex < nFullMessagesToRead; msgIndex++ ) {
        gdpb::FullMessage mess( messageBuffer[4 + 2 * msgIndex], messageBuffer[4 + 2 * msgIndex + 1] );
        //mess.PrintMessage( gdpb::msg_print_Prefix | gdpb::msg_print_Data);

        if ( mess.isGet4Hit32Msg() ){
            unsigned int gdpb = mess.getRocNumber();
            unsigned int chip = mess.getGdpbGenChipId();
            unsigned int chan = mess.getGdpbHitChanId();

            // check if the reported gdpb, chip and channel ids make sense
            if( gdpbMap.count( gdpb ) > 0 && chip < 32 && chan < 4 ) {
                nDigisPerGdpb.at( gdpb ) += 1;

                contents.digiTot->Fill( mess.getGdpbHit32Tot() );

                if( gdpbMap.count( gdpb ) ) {
                    contents.digiTotPerGdpb[ gdpbMap.at( gdpb ) ]->Fill( mess.getGdpbHit32Tot() );
                }

                // calculate time difference to trigger 
                double triggerTime   = static_cast< double > ( messageBuffer[ 0 ] );
                double digiFullTime  = mess.GetFullTimeNs();
                double timeToTrigger = ( digiFullTime - triggerTime ) / 1000.;

                contents.digiTimeToTrigger->Fill( timeToTrigger );


                // do some mapping from gdpb, chip, chan to module, counter, side ...
                unsigned int channelNrInCard   = ( chip % nrOfGet4PerFee ) * nrOfChannelsPerGet4 + chan;
                unsigned int feeNr             = ( chip / nrOfGet4PerFee );

                unsigned int mappedChannelNumber = feeNr * nrOfChannelsPerFee + get4ToPadi[ channelNrInCard ];

                if( gdpbMap.count( gdpb ) ) {
                    contents.digiMappedChannelNumberPerGdpb[ gdpbMap.at( gdpb ) ]->Fill( mappedChannelNumber );
                }

                /*
                unsigned int strip = get4ToPadi.at( channelNrInCard );
                unsigned int side  = 0;

                if( feeNr % 2 == 1) {
                    strip = 31 - strip;
                    side  = 1;
                }

                unsigned int counterIndex;                      
                if( feeNr / 2 == 0 ) {
                    if( gdpbMap.at( gdpb ) == 0 )      counterIndex = 2;
                    else if( gdpbMap.at( gdpb ) == 1 ) counterIndex = 0;
                    else if( gdpbMap.at( gdpb ) == 2 ) counterIndex = 5;
                    else if( gdpbMap.at( gdpb ) == 3 ) counterIndex = 8;
                    else if( gdpbMap.at( gdpb ) == 4 ) counterIndex = 6;
                }
                else {
                    if( gdpbMap.at( gdpb ) == 0 )      counterIndex = 1;
                    else if( gdpbMap.at( gdpb ) == 1 ) counterIndex = 3;
                    else if( gdpbMap.at( gdpb ) == 2 ) counterIndex = 4;
                    else if( gdpbMap.at( gdpb ) == 3 ) counterIndex = 7;
                }
                */

                // fine & coarse time
                int fineTs   = mess.getGdpbHitFullTs() % 112;
                int coarseTs = mess.getGdpbHitFullTs() / 112;

                contents.digiFineTs  ->Fill( fineTs   );
                contents.digiCoarseTs->Fill( coarseTs );
            }
            else {
                LOG( DBG, "some id is out of range: gdpbId %#06x    chip %d    channel %d", gdpb, chip, chan );
                //mess.PrintMessage( gdpb::msg_print_Prefix | gdpb::msg_print_Data);
            }
            
        } // end of things to do with hit message

        
        if ( mess.isEpoch2Msg() ){
            if ( mess.getEpoch2EpochMissmatch() ) {
                //contents.nEpochMismatch->Fill( gdpbMap.at( mess.getRocNumber() ) );
            }
            if ( mess.getEpoch2EpochLost() ) {
                //contents.nEpochLoss->Fill( gdpbMap.at( mess.getRocNumber() ) );
            }
            if ( mess.getEpoch2DataLost() ) {
                //contents.nEpochDataLoss->Fill( gdpbMap.at( mess.getRocNumber() ) );
            }
            if ( mess.getEpoch2Sync() ) {
                //contents.nEpochSync->Fill( gdpbMap.at( mess.getRocNumber() ) );
            }

        } // end of things to do with epoch2 message

    } // end message loop
}
