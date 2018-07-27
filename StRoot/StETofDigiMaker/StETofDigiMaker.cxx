/***************************************************************************
 *
 * $Id: StETofDigiMaker.cxx,v 1.2 2018/07/27 13:58:12 fseck Exp $
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
 * Revision 1.2  2018/07/27 13:58:12  fseck
 * small change to compile also in 64bit mode
 *
 * Revision 1.1  2018/07/25 14:39:40  jeromel
 * Peer reviewed Raghav+Jerome - code from Florian Seck
 *
 *
 ***************************************************************************/
#include "StETofDigiMaker.h"

#include "StEventTypes.h"

#include "StEvent/StETofHeader.h"
#include "StEvent/StETofDigi.h"
#include "StEvent/StETofCollection.h"

#include "StEvent/StEvent.h"
#include "StDAQMaker/StDAQReader.h"

#include "StRtsTable.h"


const double kdGet4TotBinWidthNs = 0.4; // width of get4 tot bins in ns

//_____________________________________________________________
StETofDigiMaker::StETofDigiMaker( const char* name )
: StRTSBaseMaker( "etof", name ),
  mEvent( 0 ),           /// pointer to StEvent
  mETofCollection( 0 )   /// pointer to StETofCollection
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
 * This method is to obtain the ETofCollection from StEvent.
 * If StEvent is in the chain, retrieve it;
 * if no StEvent in the chain, a new StEvent is created.
 */
StETofCollection*
StETofDigiMaker::GetETofCollection()
{
    /// Get StEvent if any at once
    StETofCollection* etofCollection = 0;
    mEvent = dynamic_cast< StEvent* >( GetInputDS( "StEvent" ) );

    if ( mEvent ) {
        etofCollection = mEvent->etofCollection();

        /// Need to create the eTof collection
        if ( !etofCollection )  {
            ///  Save the eTof collection to StEvent
            LOG_INFO << "StETofDigiMaker::GetETofCollection - making new StETofCollection and giving it to StEvent" << endm;
            etofCollection = new StETofCollection();
            mEvent->setETofCollection( etofCollection );
        }
        else {
            LOG_INFO << "StETofDigiMaker::GetETofCollection - StEvent already has a StETofCollection - not making a new one" << endm;
        }
    }
    else {
        LOG_WARN << "No StEvent found by StETofDigiMaker::GetETofCollection" << endm;
    }

    return etofCollection;
}

//_____________________________________________________________
Int_t
StETofDigiMaker::Make()
{ 
    LOG_INFO << "StETofDigiMaker::Make()" << endm;

    //---------------------------------
    mETofCollection = GetETofCollection();
    LOG_INFO << "StETofDigiMaker::Make() - getting the etof collection " << mETofCollection << endm;
    //---------------------------------

    StRtsTable* daqdta = GetNextRaw();

    if ( daqdta == nullptr ) {
        LOG_WARN << "StETofDigiMaker::Make() - NO ETOF DATA found in event" << endm;
        return kStOk;
    }

    // do unpacking of the raw data
    int iInputSizeBytes = daqdta->GetSize();
    LOG_DEBUG << "StETofDigiMaker::Make() - InputSize (bytes): " << iInputSizeBytes << endm;

    int iInputSzLg = iInputSizeBytes / sizeof( uint64_t );

    uint64_t* pulLongBuff = ( uint64_t* ) ( *daqdta->begin() );


    // determine number of full messages to read
    unsigned int uMsgsToRead = ( iInputSzLg - 4 ) / 2;	
    LOG_INFO << "StETofDigiMaker::Make() - # full messages to read: " << uMsgsToRead << endm;

    // create vector to store the trigger messages
    vector< gdpb::FullMessage > triggerMessages;

    // loop over gdpb messages
    for( unsigned int uMsgIdx = 0; uMsgIdx < uMsgsToRead; uMsgIdx++ ) {
        LOG_DEBUG << std::hex << pulLongBuff[4 + 2 * uMsgIdx] << "  " << pulLongBuff[4 + 2 * uMsgIdx + 1] << std::dec << endm;

        // convert to 'full messages'
        gdpb::FullMessage mess( pulLongBuff[4 + 2 * uMsgIdx], pulLongBuff[4 + 2 * uMsgIdx + 1] );

        // print message
        if( Debug() ) mess.PrintMessage( gdpb::msg_print_Prefix | gdpb::msg_print_Data );

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
    fillETofHeader( pulLongBuff, triggerMessages );

    // fill StEvent with eTOF information
    fillEvent();
    

    return kStOk;
}

//_____________________________________________________________
/*!
 * This method is to covert the trigger messages A,B,C,D into
 * trigger time stamp and reset time stamp of the bTOF clock
 */
void
StETofDigiMaker::convertTriggerMessages( vector< gdpb::FullMessage >& triggerMessages,
                                         map< unsigned int, uint64_t >&    gdpbTs,
                                         map< unsigned int, uint64_t >&    starTs )
{
    LOG_INFO << "StETofDigiMaker::convertTriggerMessages() - Do the conversion... " << endm; 

    size_t nTriggerMessages = triggerMessages.size();    

    LOG_INFO << "StETofDigiMaker::convertTriggerMessages() - size of triggerMessage vector: " << nTriggerMessages << endm;

    // there should be 4 StarTriggerMessages (A, B, C, D) for each Roc 
    if( nTriggerMessages % 4 != 0 ) {
        LOG_WARN << "number of trigger messages does not match expectations --> skip conversion " << endm;
        return;
    }

    for( unsigned int i=0; i< nTriggerMessages; i+=4 ) {
        int rocNumA = triggerMessages.at( i   ).getRocNumber();
        int rocNumB = triggerMessages.at( i+1 ).getRocNumber();
        int rocNumC = triggerMessages.at( i+2 ).getRocNumber();
        int rocNumD = triggerMessages.at( i+3 ).getRocNumber();

        if( rocNumA != rocNumB || rocNumA != rocNumC || rocNumA != rocNumD ) {
            LOG_WARN << "Roc number miss match --> skip conversion " << endm;
            gdpbTs.clear();
            starTs.clear();
            return;
        }

        uint64_t ulGdpbTsMsb = triggerMessages.at( i   ).getGdpbTsMsbStarA();
        uint64_t ulGdpbTsLsb = triggerMessages.at( i+1 ).getGdpbTsLsbStarB();
        uint64_t ulStarTsMsb = triggerMessages.at( i+1 ).getStarTsMsbStarB();
        uint64_t ulStarTsMid = triggerMessages.at( i+2 ).getStarTsMidStarC();

        uint64_t rocGdpbTs = ( ulGdpbTsMsb << 24 )
                           + ( ulGdpbTsLsb       );

        uint64_t rocStarTs = ( ulStarTsMsb << 48 )
                           + ( ulStarTsMid <<  8 )
                           + triggerMessages.at( i+3 ).getStarTsLsbStarD();

        //unsigned int uRocToken   = triggerMessages.at( i+3 ).getStarTokenStarD();
        //unsigned int uRocDaqCmd  = triggerMessages.at( i+3 ).getStarDaqCmdStarD();
        //unsigned int uRocTrigCmd = triggerMessages.at( i+3 ).getStarTrigCmdStarD();

        gdpbTs[ rocNumA ] = rocGdpbTs;
        starTs[ rocNumA ] = rocStarTs;
    }
   
    return; 
}

//_____________________________________________________________
void
StETofDigiMaker::fillETofHeader( uint64_t* pulLongBuff, vector< gdpb::FullMessage >& triggerMessages )
{
    uint64_t trgGdpbFullTs          = pulLongBuff[0]; //time since last ETof clock reset in ns
    uint64_t trgStarFullTs          = pulLongBuff[1]; //time since last BTof clock reset in ns
    unsigned int    starToken       = (pulLongBuff[2] >> 32) & 0xFFF;
    unsigned int    starDaqCmdIn    = (pulLongBuff[2] >> 16) & 0x00F;
    unsigned int    starTrigCmdIn   = (pulLongBuff[2]      ) & 0x00F;
    uint64_t eventStatusFlag        = pulLongBuff[3]; // filled from Apr 24th 2018 on

    LOG_DEBUG << "StETofDigiMaker::fillETofHeader()"   << endm;
    LOG_DEBUG << "trgGdpbFullTs="   << trgGdpbFullTs   << endm;
    LOG_DEBUG << "trgStarFullTs="   << trgStarFullTs   << endm; 
    LOG_DEBUG << "starToken="       << starToken       << endm;
    LOG_DEBUG << "starDaqCmdIn="    << starDaqCmdIn    << endm;
    LOG_DEBUG << "starTrigCmdIn="   << starTrigCmdIn   << endm;
    LOG_DEBUG << "eventStatusFlag=" << eventStatusFlag << endm;

    map< unsigned int, uint64_t > gdpbTs;
    map< unsigned int, uint64_t > starTs;

    convertTriggerMessages( triggerMessages, gdpbTs, starTs );

    for( auto kv : gdpbTs ) {
        LOG_DEBUG << "GdpbTs( 0x" << std::hex << kv.first << std::dec << " ): " << kv.second << endm;
    }
    for( auto kv : starTs ) {
        LOG_DEBUG << "StarTs( 0x" << std::hex << kv.first << std::dec << " ): " << kv.second << endm;
    }

    // fill eTof header with trigger & star time as doubles
    StETofHeader *etofHeader = new StETofHeader( static_cast< double > ( trgGdpbFullTs ),
                                                 static_cast< double > ( trgStarFullTs ),
                                                 gdpbTs,
                                                 starTs,
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
    // fill eTof digis
    mETofCollection->addDigi( new StETofDigi( mess.getRocNumber(),
                                              mess.getGdpbGenChipId(),
                                              mess.getGdpbHitChanId(),
                                              mess.GetFullTimeNs(),
                                              mess.getGdpbHit32Tot() * kdGet4TotBinWidthNs ) );
}

//_____________________________________________________________
/*!
 * Fill and store eTof collection in StEvent, create ETofCollection if necessary
 */
void
StETofDigiMaker::fillEvent() {
    LOG_DEBUG << "StETofDigiMaker::fillEvent() starting..." << endm;

    /// make sure we have a eTof collection
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
            LOG_INFO << "StETofDigiMaker::fillEvent() - ETofDigiCollection: " << eTofDigiVec.size() << " entries" << endm;
            if( Debug() ) {
                for( size_t i=0;i<eTofDigiVec.size();i++ ) {
                    LOG_INFO << ( *eTofDigiVec[i] ) << endm;
                }
            }
        }
        else {
        LOG_INFO << "StETofDigiMaker::fillEvent() - no ETofDigiCollection" << endm;
        }
    }
    else {
        LOG_WARN << "StETofDigiMaker::fillEvent() - no ETofCollection"     << endm;
        LOG_INFO << "StETofDigiMaker::fillEvent() - no ETofDigiCollection" << endm;
    }
}

//_____________________________________________________________
