/***************************************************************************
 *
 * $Id: StETofDigiMaker.cxx,v 1.4 2019/03/08 18:45:40 fseck Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofDigiMaker - class to fill the StEvent from DAQ reader:
 * unpack raw data & save StETofHeader & StETofDigis in StETofCollection 
 *
 ***************************************************************************
 *
 * $Log: StETofDigiMaker.cxx,v $
 * Revision 1.4  2019/03/08 18:45:40  fseck
 * save middle value of tot bin as raw tot of the digi
 *
 * Revision 1.3  2019/02/19 20:32:09  fseck
 * update for unpacking year 2019+ daq files
 *
 * Revision 1.2  2018/07/27 13:58:12  fseck
 * small change to compile also in 64bit mode
 *
 * Revision 1.1  2018/07/25 14:39:40  jeromel
 * Peer reviewed Raghav+Jerome - code from Florian Seck
 *
 *
 ***************************************************************************/
#include <vector>
#include <map>
#include <array>
#include <algorithm>    // std::is_sorted

#include "StDAQMaker/StDAQReader.h"
#include "StRtsTable.h"

#include "StEvent.h"
#include "StETofCollection.h"
#include "StETofHeader.h"
#include "StETofDigi.h"
#include "StETofHit.h"

#include "StETofDigiMaker.h"
#include "StETofUtil/StETofConstants.h"
#include "StETofUtil/StETofMessageFormat.h"


//_____________________________________________________________
StETofDigiMaker::StETofDigiMaker( const char* name )
: StRTSBaseMaker( "etof", name ),
  mEvent( 0 ),          /// pointer to StEvent
  mETofCollection( 0 ), /// pointer to StETofCollection
  mRunYear( 0 ),        /// year in which the data was taken (switch at 1st Oct)
  mDebug( false )       /// print out of all full messages for debugging
{
    LOG_DEBUG << "StETofDigiMaker::ctor"  << endm;
}

//_____________________________________________________________
StETofDigiMaker::~StETofDigiMaker()
{  /* no op */

}

//_____________________________________________________________
Int_t
StETofDigiMaker::Init()
{
    LOG_INFO << "StETofDigiMaker::Init" << endm;

    return kStOk;
}

//_____________________________________________________________
Int_t
StETofDigiMaker::InitRun( Int_t runnumber )
{ 
    mRunYear = ( runnumber + 727000 ) / 1000000 + 1999;

    LOG_INFO << "runnumber: " << runnumber << "  --> year: " << mRunYear << endm;

    return kStOk;
}

//_____________________________________________________________
Int_t
StETofDigiMaker::FinishRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StETofDigiMaker::Finish()
{ 
    return kStOk;
}

//_____________________________________________________________
/*!
 * This method is to obtain the StETofCollection from StEvent.
 * If StEvent is in the chain, retrieve it;
 * if no StEvent in the chain, a new StEvent is created.
 */
StETofCollection*
StETofDigiMaker::getETofCollection()
{
    /// get StEvent if any at once
    StETofCollection* etofCollection = 0;
    mEvent = dynamic_cast< StEvent* >( GetInputDS( "StEvent" ) );

    if ( mEvent ) {
        etofCollection = mEvent->etofCollection();

        /// need to create the eTof collection
        if ( !etofCollection )  {
            ///  save the eTof collection to StEvent
            LOG_INFO << "StETofDigiMaker::getETofCollection - making new StETofCollection and giving it to StEvent" << endm;
            etofCollection = new StETofCollection();
            mEvent->setETofCollection( etofCollection );
        }
        else {
            LOG_INFO << "StETofDigiMaker::getETofCollection - StEvent already has a StETofCollection - not making a new one" << endm;
        }
    }
    else {
        LOG_WARN << "No StEvent found by StETofDigiMaker::getETofCollection" << endm;
    }

    return etofCollection;
}

//_____________________________________________________________
Int_t
StETofDigiMaker::Make()
{ 
    LOG_INFO << "StETofDigiMaker::Make()" << endm;

    //---------------------------------
    mETofCollection = getETofCollection();
    if( mDebug ) {
        LOG_DEBUG << "StETofDigiMaker::Make() - getting the etof collection " << mETofCollection << endm;
    }

    if ( mETofCollection == nullptr )  {
        LOG_WARN << "No StEvent --> no ETofCollection --> nothing to do" << endm;
        return kStWarn;
    }

    if( mETofCollection->digisPresent() ) {
        LOG_WARN << "StETofDigiMaker::Make() - there are already ETOF DIGIS ... nothing to do." << endm;
        return kStWarn;
    }
    //---------------------------------



    StRtsTable* daqdta = GetNextRaw();

    if ( daqdta == nullptr ) {
        LOG_WARN << "StETofDigiMaker::Make() - NO ETOF DATA found in event" << endm;
        return kStOk;
    }

    // do unpacking of the raw data
    int inputSizeBytes = daqdta->GetSize();

    if( mDebug ) {
        LOG_DEBUG << "StETofDigiMaker::Make() - InputSize (bytes): " << inputSizeBytes << endm;
    }

    uint64_t* messageBuffer = ( uint64_t* ) ( *daqdta->begin() );

    // ------------------------------------------------------------------------------------------------------
    // determine number of full messages to read:
    // the packed version sent to the STAR DAQ systems is made of a 256 bit header (4 long unsigned integers)
    // followed by a buffer of gdpb::FullMessage (128 bit each).
    //
    // gdbp(v100)::Message     = 64 bit message as received from the eTOF gDPB boards
    // gdpb(v100)::FullMessage = 128 bit message, obtained by combining a
    //                           64 bit extended epoch (bits 127-64) with a gdpb::Message (bits 63-0)
    // -------------------------------------------------------------------------------------------------------
    size_t nFullMessagesToRead = ( ( inputSizeBytes / eTofConst::daqMessageSize ) - 4 ) / 2;
    LOG_INFO << "StETofDigiMaker::Make() - # full messages to read: " << nFullMessagesToRead << endm;

    if( mRunYear == 2018 ) {
        if( mDebug ) {
            LOG_DEBUG << "processing event from 2018" << endm;
        }
        processEvent2018( messageBuffer, nFullMessagesToRead );
    }
    else{
        if( mDebug ) {
            LOG_DEBUG << "processing event from 2019+" << endm;
        }
        processEvent( messageBuffer, nFullMessagesToRead );
    }

    return kStOk;
}


//_____________________________________________________________
/*!
 * process events of data taken in 2019+
 */
void
StETofDigiMaker::processEvent( uint64_t* messageBuffer, size_t nFullMessagesToRead )
{
    // create vector to store the trigger messages
    vector< gdpbv100::FullMessage > triggerMessages;

    // loop over gdpb messages
    for( size_t msgIndex = 0; msgIndex < nFullMessagesToRead; msgIndex++ ) {
        // convert to 'full messages'
        gdpbv100::FullMessage mess( messageBuffer[ 4 + 2 * msgIndex ], messageBuffer[ 4 + 2 * msgIndex + 1 ] );

        // print message for debugging
        if( mDebug ) {
            LOG_DEBUG << std::hex << messageBuffer[ 4 + 2 * msgIndex ] << "  " << messageBuffer[ 4 + 2 * msgIndex + 1 ] << std::dec << endm;
            mess.PrintMessage( gdpb::msg_print_Prefix | gdpb::msg_print_Data );
        }

        // fill structural pointer vector with eTOF digis if the message is a 'hit' message 
        if( mess.isHitMsg() ) {
            fillETofDigi( mess );
        }

        // deal with STAR trigger messages A,B,C,D
        if( mess.isStarTrigger() ) {
            triggerMessages.push_back( mess );
        }

    } // end message loop

    // fill eTOF collection with eTOF header
    fillETofHeader( messageBuffer, triggerMessages );

    // check eTOF information in StEvent
    checkEvent();
}

//_____________________________________________________________
/*!
 * This method is to covert the trigger messages A,B,C,D into
 * trigger time stamp and reset time stamp of the bTOF clock
 */
void
StETofDigiMaker::convertTriggerMessages( vector< gdpbv100::FullMessage >& triggerMessages,
                                         map< unsigned int, uint64_t >&    gdpbTsMap,
                                         map< unsigned int, uint64_t >&    starTsMap )
{
    if( mDebug ) {
        LOG_DEBUG << "StETofDigiMaker::convertTriggerMessages() - do the conversion... " << endm; 
    }
    size_t nTriggerMessages = triggerMessages.size();    

    LOG_INFO << "StETofDigiMaker::convertTriggerMessages() - size of triggerMessage vector: " << nTriggerMessages << endm;

    // there should be 4 StarTriggerMessages (A, B, C, D) for each Roc 
    if( nTriggerMessages % 4 != 0 ) {
        LOG_WARN << "number of trigger messages does not match expectations --> skip conversion " << endm;
        return;
    }

    array< int, 4 > messageTypesTemplate = { 0, 1, 2, 3 };

    for( unsigned int i=0; i<nTriggerMessages; i+=4 ) {
        
        array< int, 4 > messageTypes;
        messageTypes.at( 0 ) = triggerMessages.at( i   ).getStarTrigMsgIndex();
        messageTypes.at( 1 ) = triggerMessages.at( i+1 ).getStarTrigMsgIndex();
        messageTypes.at( 2 ) = triggerMessages.at( i+2 ).getStarTrigMsgIndex();
        messageTypes.at( 3 ) = triggerMessages.at( i+3 ).getStarTrigMsgIndex();

        array< int, 4 > rocIds;
        rocIds.at( 0 ) = triggerMessages.at( i   ).getGdpbGenGdpbId();
        rocIds.at( 1 ) = triggerMessages.at( i+1 ).getGdpbGenGdpbId();
        rocIds.at( 2 ) = triggerMessages.at( i+2 ).getGdpbGenGdpbId();
        rocIds.at( 3 ) = triggerMessages.at( i+3 ).getGdpbGenGdpbId();

        if( !std::all_of( rocIds.begin(), rocIds.end(), [ rocIds ]( int i ) { return i == rocIds[ 0 ]; } ) ||
            messageTypes != messageTypesTemplate )
        {
            LOG_WARN << "Roc Id miss match --> skip conversion " << endm;
            gdpbTsMap.clear();
            starTsMap.clear();
            return;
        }

        uint64_t gdpbTsMsb = triggerMessages.at( i   ).getGdpbTsMsbStarA();
        uint64_t gdpbTsLsb = triggerMessages.at( i+1 ).getGdpbTsLsbStarB();
        uint64_t starTsMsb = triggerMessages.at( i+1 ).getStarTsMsbStarB();
        uint64_t starTsMid = triggerMessages.at( i+2 ).getStarTsMidStarC();

        uint64_t rocGdpbTs = ( gdpbTsMsb << 24 )
                           + ( gdpbTsLsb       );

        uint64_t rocStarTs = ( starTsMsb << 48 )
                           + ( starTsMid <<  8 )
                           + triggerMessages.at( i+3 ).getStarTsLsbStarD();

        //unsigned int rocToken   = triggerMessages.at( i+3 ).getStarTokenStarD();
        //unsigned int rocDaqCmd  = triggerMessages.at( i+3 ).getStarDaqCmdStarD();
        //unsigned int rocTrigCmd = triggerMessages.at( i+3 ).getStarTrigCmdStarD();

        gdpbTsMap[ rocIds.at( 0 ) ] = rocGdpbTs;
        starTsMap[ rocIds.at( 0 ) ] = rocStarTs;
    }
}

//_____________________________________________________________
void
StETofDigiMaker::fillETofHeader( uint64_t* messageBuffer, vector< gdpbv100::FullMessage >& triggerMessages )
{
    uint64_t     trgGdpbFullTs   =   messageBuffer[ 0 ]; // time since last eTOF clock reset in ns
    uint64_t     trgStarFullTs   =   messageBuffer[ 1 ]; // time since last bTOF clock reset in ns
    unsigned int starToken       = ( messageBuffer[ 2 ] >> 32 ) & 0xFFF;
    unsigned int starDaqCmdIn    = ( messageBuffer[ 2 ] >> 16 ) & 0x00F;
    unsigned int starTrigCmdIn   = ( messageBuffer[ 2 ]       ) & 0x00F;
    uint64_t     eventStatusFlag =   messageBuffer[ 3 ];

    if( mDebug ) {
        LOG_INFO  << "StETofDigiMaker::fillETofHeader(): " << "  ";
        LOG_INFO  << "trgGdpbFullTs="   << trgGdpbFullTs   << "  ";
        LOG_INFO  << "trgStarFullTs="   << trgStarFullTs   << endm;
        LOG_DEBUG << "starToken="       << starToken       << "  ";
        LOG_DEBUG << "starDaqCmdIn="    << starDaqCmdIn    << "  ";
        LOG_DEBUG << "starTrigCmdIn="   << starTrigCmdIn   << endm;
        LOG_DEBUG << "eventStatusFlag=" << eventStatusFlag << endm;
    }
    
    map< unsigned int, uint64_t > gdpbTsMap;
    map< unsigned int, uint64_t > starTsMap;

    convertTriggerMessages( triggerMessages, gdpbTsMap, starTsMap );

    if( mDebug ) {
        for( const auto& kv : gdpbTsMap ) {
            LOG_DEBUG << "GdpbTs( 0x" << std::hex << kv.first << std::dec << " ): " << kv.second << endm;
        }
        for( const auto& kv : starTsMap ) {
            LOG_DEBUG << "StarTs( 0x" << std::hex << kv.first << std::dec << " ): " << kv.second << endm;
        }
    }

    // fill eTOF header with trigger & star time as doubles
    StETofHeader *etofHeader = new StETofHeader( trgGdpbFullTs * gdpbv100::kdClockCycleSizeNs,
                                                 trgStarFullTs * gdpbv100::kdClockCycleSizeNs,
                                                 gdpbTsMap,
                                                 starTsMap,
                                                 starToken,
                                                 starDaqCmdIn,
                                                 starTrigCmdIn,
                                                 eventStatusFlag
                                                );

    mETofCollection->setHeader( etofHeader );
}

//_____________________________________________________________
void
StETofDigiMaker::fillETofDigi( gdpbv100::FullMessage& mess )
{
    // fill eTOF digis
    mETofCollection->addDigi( new StETofDigi( mess.getGdpbGenGdpbId(),
                                              mess.getGdpbGenChipId(),
                                              mess.getGdpbHitChanId(),
                                              mess.GetFullTimeNs(),
                                              ( double ) mess.getGdpbHit32Tot() + 0.5 ) );
}



//_____________________________________________________________
/*!
 * check eTOF collection in StEvent (create ETofCollection if necessary)
 */
void
StETofDigiMaker::checkEvent() {
    LOG_DEBUG << "StETofDigiMaker::fillEvent() starting..." << endm;

    /// make sure we have an eTOF collection
    if( !mETofCollection ) {
        LOG_WARN << "StETofDigiMaker::fillEvent() - no ETofCollection ... creating one in StEvent" << endm;
        mETofCollection = new StETofCollection();
        mEvent->setETofCollection( mETofCollection );
    }

    ///
    StETofCollection* etofCollection = mEvent->etofCollection();
    if( etofCollection ) {
        if( etofCollection->digisPresent() ) {
            StSPtrVecETofDigi& eTofDigiVec = etofCollection->etofDigis();
            LOG_INFO << "StETofDigiMaker::fillEvent() - StETofCollection has " << eTofDigiVec.size() << " digis" << endm;
            if( mDebug ) {
                for( size_t i=0; i<eTofDigiVec.size(); i++ ) {
                    LOG_INFO << ( *eTofDigiVec[ i ] ) << endm;
                }
            }
        }
        else {
            LOG_INFO << "StETofDigiMaker::fillEvent() - no digis in the StETofCollection" << endm;
        }
    }
    else {
        LOG_WARN << "StETofDigiMaker::fillEvent() - no StETofCollection" << endm;
    }
}
//_____________________________________________________________



//**** methods for year 2018 data format ***

//_____________________________________________________________
/*!
 * process events of data taken in 2018
 */
void
StETofDigiMaker::processEvent2018( uint64_t* messageBuffer, size_t nFullMessagesToRead )
{
    // create vector to store the trigger messages
    vector< gdpb::FullMessage > triggerMessages;

    // loop over gdpb messages
    for( size_t msgIndex = 0; msgIndex < nFullMessagesToRead; msgIndex++ ) {
        // convert to 'full messages'
        gdpb::FullMessage mess( messageBuffer[ 4 + 2 * msgIndex ], messageBuffer[ 4 + 2 * msgIndex + 1 ] );

        // print message for debugging
        if( mDebug ) {
            LOG_DEBUG << std::hex << messageBuffer[ 4 + 2 * msgIndex ] << "  " << messageBuffer[ 4 + 2 * msgIndex + 1 ] << std::dec << endm;
            mess.PrintMessage( gdpb::msg_print_Prefix | gdpb::msg_print_Data );
        }

        // fill structural pointer vector with eTOF digis if the message is a 'hit' message 
        if( mess.isGet4Hit32Msg() ) {
            fillETofDigi( mess );
        }

        // deal with STAR trigger messages A,B,C,D
        if( mess.isStarTrigger() ) {
            triggerMessages.push_back( mess );
        }

    } // end message loop

    // fill eTOF collection with eTOF header
    fillETofHeader( messageBuffer, triggerMessages );

    // check eTOF information in StEvent
    checkEvent();
}

//_____________________________________________________________
/*!
 * This method is to covert the trigger messages A,B,C,D into
 * trigger time stamp and reset time stamp of the bTOF clock
 */
void
StETofDigiMaker::convertTriggerMessages( vector< gdpb::FullMessage >& triggerMessages,
                                         map< unsigned int, uint64_t >&    gdpbTsMap,
                                         map< unsigned int, uint64_t >&    starTsMap )
{
    if( mDebug ) {
        LOG_DEBUG << "StETofDigiMaker::convertTriggerMessages() - do the conversion... " << endm; 
    }
    size_t nTriggerMessages = triggerMessages.size();    

    LOG_INFO << "StETofDigiMaker::convertTriggerMessages() - size of triggerMessage vector: " << nTriggerMessages << endm;

    // there should be 4 StarTriggerMessages (A, B, C, D) for each Roc 
    if( nTriggerMessages % 4 != 0 ) {
        LOG_WARN << "number of trigger messages does not match expectations --> skip conversion " << endm;
        return;
    }

    for( unsigned int i=0; i<nTriggerMessages; i+=4 ) {
        array< int, 4 > rocIds;
        rocIds.at( 0 ) = triggerMessages.at( i   ).getRocNumber();
        rocIds.at( 1 ) = triggerMessages.at( i+1 ).getRocNumber();
        rocIds.at( 2 ) = triggerMessages.at( i+2 ).getRocNumber();
        rocIds.at( 3 ) = triggerMessages.at( i+3 ).getRocNumber();

        // check that all trigger messages come from the same AFCK
        if( !std::all_of( rocIds.begin(), rocIds.end(), [ rocIds ]( int i ) { return i == rocIds[ 0 ]; } ) ) {
            LOG_WARN << "Roc Id miss match --> skip conversion " << endm;
            gdpbTsMap.clear();
            starTsMap.clear();
            return;
        }

        uint64_t gdpbTsMsb = triggerMessages.at( i   ).getGdpbTsMsbStarA();
        uint64_t gdpbTsLsb = triggerMessages.at( i+1 ).getGdpbTsLsbStarB();
        uint64_t starTsMsb = triggerMessages.at( i+1 ).getStarTsMsbStarB();
        uint64_t starTsMid = triggerMessages.at( i+2 ).getStarTsMidStarC();

        uint64_t rocGdpbTs = ( gdpbTsMsb << 24 )
                           + ( gdpbTsLsb       );

        uint64_t rocStarTs = ( starTsMsb << 48 )
                           + ( starTsMid <<  8 )
                           + triggerMessages.at( i+3 ).getStarTsLsbStarD();

        //unsigned int rocToken   = triggerMessages.at( i+3 ).getStarTokenStarD();
        //unsigned int rocDaqCmd  = triggerMessages.at( i+3 ).getStarDaqCmdStarD();
        //unsigned int rocTrigCmd = triggerMessages.at( i+3 ).getStarTrigCmdStarD();

        gdpbTsMap[ rocIds.at( 0 ) ] = rocGdpbTs;
        starTsMap[ rocIds.at( 0 ) ] = rocStarTs;
    }
}

//_____________________________________________________________
void
StETofDigiMaker::fillETofHeader( uint64_t* messageBuffer, vector< gdpb::FullMessage >& triggerMessages )
{
    uint64_t     trgGdpbFullTs   =   messageBuffer[ 0 ]; // time since last eTOF clock reset in ns
    uint64_t     trgStarFullTs   =   messageBuffer[ 1 ]; // time since last bTOF clock reset in ns
    unsigned int starToken       = ( messageBuffer[ 2 ] >> 32 ) & 0xFFF;
    unsigned int starDaqCmdIn    = ( messageBuffer[ 2 ] >> 16 ) & 0x00F;
    unsigned int starTrigCmdIn   = ( messageBuffer[ 2 ]       ) & 0x00F;
    uint64_t     eventStatusFlag =   messageBuffer[ 3 ]; // filled from Apr 24th 2018 on

    if( mDebug ) {
        LOG_DEBUG << "StETofDigiMaker::fillETofHeader()"   << endm;
        LOG_DEBUG << "trgGdpbFullTs="   << trgGdpbFullTs   << endm;
        LOG_DEBUG << "trgStarFullTs="   << trgStarFullTs   << endm;
        LOG_DEBUG << "starToken="       << starToken       << endm;
        LOG_DEBUG << "starDaqCmdIn="    << starDaqCmdIn    << endm;
        LOG_DEBUG << "starTrigCmdIn="   << starTrigCmdIn   << endm;
        LOG_DEBUG << "eventStatusFlag=" << eventStatusFlag << endm;
    }
    map< unsigned int, uint64_t > gdpbTsMap;
    map< unsigned int, uint64_t > starTsMap;

    convertTriggerMessages( triggerMessages, gdpbTsMap, starTsMap );

    if( mDebug ) {
        for( const auto& kv : gdpbTsMap ) {
            LOG_DEBUG << "GdpbTs( 0x" << std::hex << kv.first << std::dec << " ): " << kv.second << endm;
        }
        for( const auto& kv : starTsMap ) {
            LOG_DEBUG << "StarTs( 0x" << std::hex << kv.first << std::dec << " ): " << kv.second << endm;
        }
    }

    // fill eTOF header with trigger & star time as doubles
    StETofHeader *etofHeader = new StETofHeader( static_cast< double > ( trgGdpbFullTs ),
                                                 static_cast< double > ( trgStarFullTs ),
                                                 gdpbTsMap,
                                                 starTsMap,
                                                 starToken,
                                                 starDaqCmdIn,
                                                 starTrigCmdIn,
                                                 eventStatusFlag
                                                );

    mETofCollection->setHeader( etofHeader );
}

//_____________________________________________________________
void
StETofDigiMaker::fillETofDigi( gdpb::FullMessage& mess )
{
    // fill eTOF digis
    mETofCollection->addDigi( new StETofDigi( mess.getRocNumber(),
                                              mess.getGdpbGenChipId(),
                                              mess.getGdpbHitChanId(),
                                              mess.GetFullTimeNs(),
                                              ( double ) mess.getGdpbHit32Tot() + 0.5 ) );
}