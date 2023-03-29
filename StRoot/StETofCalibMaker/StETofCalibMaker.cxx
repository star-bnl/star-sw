 /***************************************************************************
 *
 * $Id: StETofCalibMaker.cxx,v 1.8 2020/01/16 03:10:33 fseck Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofCalibMaker - class to read the eTofCollection from
 * StEvent, do calibration, fill the collection with the updated information
 * and write back to StEvent
 *
 ***************************************************************************
 *
 * $Log: StETofCalibMaker.cxx,v $
 * Revision 1.8  2020/01/16 03:10:33  fseck
 * update handling of reset time for new cases in Run20
 *
 * Revision 1.7  2019/12/19 02:19:23  fseck
 * use known pulser time differences inside one Gbtx to recover missing pulser signals
 *
 * Revision 1.6  2019/12/12 02:26:37  fseck
 * ignore duplicate digis from stuck firmware
 *
 * Revision 1.5  2019/12/10 15:55:01  fseck
 * added new database tables for pulsers, updated pulser handling and trigger time calculation
 *
 * Revision 1.4  2019/05/08 23:56:44  fseck
 * change of default value for reference pulser
 *
 * Revision 1.3  2019/03/25 01:09:46  fseck
 * added first version of pulser correction procedure + fix in reading parameters from db
 *
 * Revision 1.2  2019/03/08 19:01:07  fseck
 * pick up the right trigger and reset time on event-by-event basis  +  fix to clearing of calibrated tot in afterburner mode  +  flag pulser digis
 *
 * Revision 1.1  2019/02/19 19:52:28  jeromel
 * Reviewed code provided by F.Seck
 *
 *
 ***************************************************************************/
#include <iostream>
#include <sstream>
#include <fstream>
#include <cmath>
#include <iterator>
#include <algorithm>

#include "TString.h"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TClass.h"
#include "TObject.h"
#include "TProfile.h"

#include "StChain/StChainOpt.h" // for renaming the histogram file

#include "StEvent.h"

#include "StETofCollection.h"
#include "StETofHeader.h"
#include "StETofDigi.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuETofDigi.h"
#include "StMuDSTMaker/COMMON/StMuETofHeader.h"

#include "StETofCalibMaker.h"
#include "StETofUtil/StETofHardwareMap.h"
#include "StETofUtil/StETofConstants.h"

#include "tables/St_etofCalibParam_Table.h"
#include "tables/St_etofElectronicsMap_Table.h"
#include "tables/St_etofStatusMap_Table.h"
#include "tables/St_etofTimingWindow_Table.h"
#include "tables/St_etofSignalVelocity_Table.h"
#include "tables/St_etofDigiTotCorr_Table.h"
#include "tables/St_etofDigiTimeCorr_Table.h"
#include "tables/St_etofDigiSlewCorr_Table.h"
#include "tables/St_etofResetTimeCorr_Table.h"
#include "tables/St_etofPulserTotPeak_Table.h"
#include "tables/St_etofPulserTimeDiffGbtx_Table.h"

namespace etofSlewing {
    const unsigned int nTotBins = 30;

    // for space-efficient value in the database to nanoseconds
    const float conversionFactor = 1. / 100.;
}


//_____________________________________________________________
StETofCalibMaker::StETofCalibMaker( const char* name )
: StMaker( "etofCalib", name ),
  mEvent( nullptr ),
  mMuDst( nullptr ),
  mHwMap( nullptr ),
  mFileNameCalibParam( "" ),
  mFileNameElectronicsMap( "" ),
  mFileNameStatusMap( "" ),
  mFileNameTimingWindow( "" ),
  mFileNameSignalVelocity( "" ),
  mFileNameCalibHistograms( "" ),
  mFileNameResetTimeCorr( "" ),
  mFileNamePulserTotPeak( "" ),
  mFileNamePulserTimeDiffGbtx( "" ),
  mRunYear( 0 ),
  mGet4TotBinWidthNs( 1. ),
  mMinDigisPerSlewBin( 25 ),
  mResetTimeCorr( 0. ),
  mTriggerTime( 0. ),
  mResetTime( 0. ),
  mResetTs( 0 ),
  mPulserPeakTime( 0. ),
  mReferencePulserIndex( 0 ),
  mStrictPulserHandling( 0 ),
  mUsePulserGbtxDiff( true ),
  mDoQA( false ),
  mDebug( false ),
  mHistFileName( "" )
{
    /// default constructor
    LOG_DEBUG << "StETofCalibMaker::ctor"  << endm;

    mStatus.clear();
    mTimingWindow.clear();
    mPulserWindow.clear();
    mSignalVelocity.clear();
    mDigiTotCorr.clear();
    mDigiSlewCorr.clear();

    mPulserPeakTot.clear();
    mPulserTimeDiff.clear();
    mPulserTimeDiffGbtx.clear();
	 mNPulsersCounter.clear();
	 mNStatusBitsCounter.clear();
	 mPulserPresent.clear();

    mJumpingPulsers.clear();

    mUnlockPulserState.clear();

    mStuckFwDigi.clear();

    mHistograms.clear();
}


//_____________________________________________________________
StETofCalibMaker::~StETofCalibMaker()
{ /* no op */

}


//_____________________________________________________________
Int_t
StETofCalibMaker::Init()
{
    LOG_INFO << "StETofCalibMaker::Init()" << endm;

    bookHistograms();

    return kStOk;
}


//_____________________________________________________________
Int_t
StETofCalibMaker::InitRun( Int_t runnumber )
{
    mRunYear = ( runnumber + 727000 ) / 1000000 + 1999;
    LOG_INFO << "runnumber: " << runnumber << "  --> year: " << mRunYear << endm;

    TDataSet* dbDataSet = nullptr;
    std::ifstream paramFile;

    // --------------------------------------------------------------------------------------------
    // initialize calibration parameters from parameter file (if filename is provided) or database:
    // -- electronics-to-hardware map
    // -- status map
    // -- timing window
    // -- calib param
    // -- signal velocity
    // -- calib TOT factor per channel
    // -- calib time offset per channel: position + T0
    // -- slewing corrections
    // -- reset time correction
    // --------------------------------------------------------------------------------------------

    // electronics-to-hardware map
    if( mFileNameElectronicsMap.empty() ) {
        LOG_INFO << "etofElectronicsMap: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Geometry/etof/etofElectronicsMap" );

        St_etofElectronicsMap* etofElectronicsMap = static_cast< St_etofElectronicsMap* > ( dbDataSet->Find( "etofElectronicsMap" ) );
        if( !etofElectronicsMap ) {
            LOG_ERROR << "unable to get the electronics map from the database" << endm;
            return kStFatal;
        }

        mHwMap = new StETofHardwareMap( etofElectronicsMap, mRunYear );
    }
    else {
        LOG_INFO << "etofElectronicsMap: filename provided --> use parameter file: " << mFileNameElectronicsMap.c_str() << endm;

        mHwMap = new StETofHardwareMap( mFileNameElectronicsMap, mRunYear );
    }

    // --------------------------------------------------------------------------------------------

    // status map
    mStatus.clear();

    if( mFileNameStatusMap.empty() ) {
        LOG_INFO << "etofStatusMap: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofStatusMap" );

        St_etofStatusMap* etofStatusMap = static_cast< St_etofStatusMap* > ( dbDataSet->Find( "etofStatusMap" ) );
        if( !etofStatusMap ) {
            LOG_ERROR << "unable to get the status map from the database" << endm;
            return kStFatal;
        }

        etofStatusMap_st* statusMapTable = etofStatusMap->GetTable();
        
        for( size_t i=0; i<eTofConst::nChannelsInSystem; i++ ) {
            mStatus[ channelToKey( i ) ] = (int) statusMapTable->status[ i ];
        }
    }
    else {
        LOG_INFO << "etofStatusMap: filename provided --> use parameter file: " << mFileNameStatusMap.c_str() << endm;
        
        paramFile.open( mFileNameStatusMap.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etofStatusMap' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        std::vector< int > status;
        int temp;
        while( paramFile >> temp ) {
            status.push_back( temp );
        }

        paramFile.close();

        if( status.size() != eTofConst::nChannelsInSystem ) {
            LOG_ERROR << "parameter file for 'etofStatusMap' has not the right amount of entries: ";
            LOG_ERROR << status.size() << " instead of " << eTofConst::nChannelsInSystem << " !!!!" << endm;
            return kStFatal;  
        }

        for( size_t i=0; i<eTofConst::nChannelsInSystem; i++ ) {
            mStatus[ channelToKey( i ) ] = status[ i ];
        }
    }

    for( const auto& kv : mStatus ) {
        LOG_DEBUG << "channel key: " << kv.first << " --> status = " << kv.second << endm;
    }

    // --------------------------------------------------------------------------------------------

    // timing window
    mTimingWindow.clear();
    mPulserWindow.clear();

    if( mFileNameTimingWindow.empty() ) {
        LOG_INFO << "etofTimingWindow: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofTimingWindow" );

        St_etofTimingWindow* etofTimingWindow = static_cast< St_etofTimingWindow* > ( dbDataSet->Find( "etofTimingWindow" ) );
        if( !etofTimingWindow ) {
            LOG_ERROR << "unable to get the timing window from the database" << endm;
            return kStFatal;
        }

        etofTimingWindow_st* timingWindowTable = etofTimingWindow->GetTable();
        
        for( size_t i=0; i<12 ; i++ ) {
            int key = timingWindowTable->afckAddress[ i ];
            if( key > 0 ) {
                mTimingWindow[ key ] = std::make_pair( timingWindowTable->timingMin[ i ], timingWindowTable->timingMax[ i ] );
                mPulserWindow[ key ] = std::make_pair( timingWindowTable->pulserMin[ i ], timingWindowTable->pulserMax[ i ] );

                mPulserPeakTime = timingWindowTable->pulserPeak[ i ];
            }
        }
    }
    else {
        LOG_INFO << "etofTimingWindow: filename provided --> use parameter file: " << mFileNameTimingWindow.c_str() << endm;
        
        paramFile.open( mFileNameTimingWindow.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etofTimingWindow' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        std::string temp;
        std::vector< float > times;
        int address;
        float time;

        while( std::getline( paramFile, temp ) ) {
            std::istringstream istring( temp );
            times.clear();

            istring >>std::hex >> address >> std::dec;

            while( istring >> time ) {
                times.push_back( time );
            }

            if( times.size() != 6 ) {
                LOG_ERROR << "parameter file for 'etofTimingWindow' has not the right amount of entries: ";
                LOG_ERROR << "times for address (0x" << std::hex << address << std::dec << ") has " << times.size() << " instead of " << 6 << " !!!!" << endm;
                return kStFatal;  
            }
        
            if( address > 0 ) {
                mTimingWindow[ address ] = std::make_pair( times.at( 0 ), times.at( 1 ) );
                mPulserWindow[ address ] = std::make_pair( times.at( 3 ), times.at( 4 ) );

                mPulserPeakTime = times.at( 5 );
            }
        }
        paramFile.close();

        if( mTimingWindow.size() > 12 ) {
            LOG_ERROR << " too many entries in mTimingWindow map ...." << endm;
            return kStFatal;
        }
        if( mPulserWindow.size() > 12 ) {
            LOG_ERROR << " too many entries in mPulserWindow map ...." << endm;
            return kStFatal;
        }
    }

    for( const auto& kv : mTimingWindow ) {
        LOG_DEBUG << "AFCK address: 0x" << std::hex << kv.first << std::dec << " --> timing window from " << kv.second.first << " to " << kv.second.second << " ns" << endm;
    }
    for( const auto& kv : mPulserWindow ) {
        LOG_DEBUG << "AFCK address: 0x" << std::hex << kv.first << std::dec << " --> pulser window from " << kv.second.first << " to " << kv.second.second << " ns" << endm;
    }
    LOG_DEBUG << "pulser time peak at " << mPulserPeakTime << " ns" << endm;
    // --------------------------------------------------------------------------------------------

    // calib param
    if( mFileNameCalibParam.empty() ) {
        LOG_INFO << "etofCalibParam: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofCalibParam" );

        St_etofCalibParam* etofCalibParam = static_cast< St_etofCalibParam* > ( dbDataSet->Find( "etofCalibParam" ) );
        if( !etofCalibParam ) {
            LOG_ERROR << "unable to get the calibration params from the database" << endm;
            return kStFatal;
        }

        etofCalibParam_st* calibParamTable = etofCalibParam->GetTable();
        
        mGet4TotBinWidthNs    = calibParamTable->get4TotBinWidthNs;
        mMinDigisPerSlewBin   = calibParamTable->minDigisInSlewBin;

        // only set the reference pulser index if it is not alredy set from outside by a steering macro
        if( mReferencePulserIndex == 0 ) {
            mReferencePulserIndex = calibParamTable->referencePulserIndex;
        }
        else {
            LOG_INFO << "--- reference pulser index is set manually ---" << endm;
        }
    }
    else {
        LOG_INFO << "etofCalibParam: filename provided --> use parameter file: " << mFileNameCalibParam.c_str() << endm;
        
        paramFile.open( mFileNameCalibParam.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etofCalibParam' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        std::vector< float > param;
        float temp;
        while( paramFile >> temp ) {
            param.push_back( temp );
        }
        
        paramFile.close();

        if( param.size() != 3 ) {
            LOG_ERROR << "parameter file for 'etofCalibParam' has not the right amount of entries: ";
            LOG_ERROR << param.size() << " instead of 3 !!!!" << endm;
            return kStFatal;
        }

        if( param.at( 0 ) > 0. ) {
            mGet4TotBinWidthNs = param.at( 0 );
        }
        if( param.at( 1 ) > 0 ) {
            mMinDigisPerSlewBin = param.at( 1 );
        }

        // only set the reference pulser index if it is not alredy set from outside by a steering macro
        if( param.at( 2 ) > 0 && mReferencePulserIndex == 0 ) {
            mReferencePulserIndex = param.at( 2 );
        }
        else {
            LOG_INFO << "--- reference pulser index is set manually ---" << endm;
        }
    }

    LOG_INFO << " Get4 TOT bin width to ns conversion factor: "      << mGet4TotBinWidthNs    << endm;
    LOG_INFO << " minimal number of digis required in slewing bin: " << mMinDigisPerSlewBin   << endm;
    LOG_INFO << " reference pulser index: "                          << mReferencePulserIndex << endm;

   
    // --------------------------------------------------------------------------------------------

    // signal velocities
    mSignalVelocity.clear();

    if( mFileNameSignalVelocity.empty() ) {
        LOG_INFO << "etofSignalVelocity: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofSignalVelocity" );

        St_etofSignalVelocity* etofSignalVelocity = static_cast< St_etofSignalVelocity* > ( dbDataSet->Find( "etofSignalVelocity" ) );
        if( !etofSignalVelocity ) {
            LOG_ERROR << "unable to get the signal velocity from the database" << endm;
            return kStFatal;
        }

        etofSignalVelocity_st* velocityTable = etofSignalVelocity->GetTable();
        
        for( size_t i=0; i<eTofConst::nCountersInSystem; i++ ) {
            if( velocityTable->signalVelocity[ i ] > 0 ) {
                mSignalVelocity[ detectorToKey( i ) ] = velocityTable->signalVelocity[ i ];
            }
        }
    }
    else {
        LOG_INFO << "etofSignalVelocity: filename provided --> use parameter file: " << mFileNameSignalVelocity.c_str() << endm;
        
        paramFile.open( mFileNameSignalVelocity.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etofSignalVelocity' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        std::vector< float > velocity;
        float temp;
        while( paramFile >> temp ) {
            velocity.push_back( temp );
        }

        paramFile.close();

        if( velocity.size() != eTofConst::nCountersInSystem ) {
            LOG_ERROR << "parameter file for 'etofSignalVelocity' has not the right amount of entries: ";
            LOG_ERROR << velocity.size() << " instead of " << eTofConst::nCountersInSystem << " !!!!" << endm;
            return kStFatal;  
        }

        for( size_t i=0; i<eTofConst::nCountersInSystem; i++ ) {
            if( velocity.at( i ) > 0 ) {
                mSignalVelocity[ detectorToKey( i ) ] = velocity.at( i );
            }
        }
    }

    for( const auto& kv : mSignalVelocity ) {
        LOG_DEBUG << "counter key: " << kv.first << " --> signal velocity = " << kv.second << " cm / ns" << endm;
    }

    // --------------------------------------------------------------------------------------------

    // digi tot correction factor, time correction offset and slewing corrections
    mDigiTotCorr.clear();
    mDigiTimeCorr.clear();
    mDigiSlewCorr.clear();

    if( mFileNameCalibHistograms.empty() ) {
        //-------------------
        // digi tot corr
        //-------------------
        LOG_INFO << "etofDigiTotCorr: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofDigiTotCorr" );

        St_etofDigiTotCorr* etofDigiTotCorr = static_cast< St_etofDigiTotCorr* > ( dbDataSet->Find( "etofDigiTotCorr" ) );
        if( !etofDigiTotCorr ) {
            LOG_ERROR << "unable to get the digi tot correction from the database" << endm;
            return kStFatal;
        }

        etofDigiTotCorr_st* digiTotCorrTable = etofDigiTotCorr->GetTable();
        
        for( size_t i=0; i<eTofConst::nChannelsInSystem; i++ ) {
            unsigned int key = channelToKey( i );
            unsigned int detector = key / 1000;

            if( mDigiTotCorr.count( detector ) == 0 ) {
                TString name  = Form( "digiTotCorr_%d", detector );
                mDigiTotCorr[ detector ] = new TH1F( name, name, 2 * eTofConst::nStrips, 0, 2 * eTofConst::nStrips );
            }

            unsigned int strip = ( key % 1000 ) / 10;
            unsigned int side  = key % 10;

            if( mDebug ) {
                LOG_DEBUG << i << "  " << detector << "  " << strip << " " << side << "  " << digiTotCorrTable->totCorr[ i ] << endm;
            }

            mDigiTotCorr.at( detector )->SetBinContent( strip + eTofConst::nStrips * ( side - 1 ) , digiTotCorrTable->totCorr[ i ] );
        }

        for( auto& kv : mDigiTotCorr ) {
            kv.second->SetDirectory( 0 );
        }


        //-------------------
        // digi time corr
        //-------------------
        LOG_INFO << "etofDigiTimeCorr: no filename provided --> load database table" << endm;

        // (1) position offset +  (2) T0 offset
        dbDataSet = GetDataBase( "Calibrations/etof/etofDigiTimeCorr" );

        St_etofDigiTimeCorr* etofDigiTimeCorr = static_cast< St_etofDigiTimeCorr* > ( dbDataSet->Find( "etofDigiTimeCorr" ) );
        if( !etofDigiTimeCorr ) {
            LOG_ERROR << "unable to get the digi time correction from the database" << endm;
            return kStFatal;
        }

        etofDigiTimeCorr_st* digiTimeCorrTable = etofDigiTimeCorr->GetTable();
        
        for( size_t i=0; i<eTofConst::nChannelsInSystem; i++ ) {
            unsigned int key = channelToKey( i );
            unsigned int detector = key / 1000;

            if( mDigiTimeCorr.count( detector ) == 0 ) {
                TString name  = Form( "digiTimeCorr_%d", detector );
                mDigiTimeCorr[ detector ] = new TH1F( name, name, 2 * eTofConst::nStrips, 0, 2 * eTofConst::nStrips );
            }

            unsigned int strip = ( key % 1000 ) / 10;
            unsigned int side  = key % 10;

            if( mDebug ) {
                LOG_DEBUG << i << "  " << detector << "  " << strip << " " << side << "  " << digiTimeCorrTable->timeCorr[ i ] << endm;
            }

            mDigiTimeCorr.at( detector )->SetBinContent( strip + eTofConst::nStrips * ( side - 1 ) , digiTimeCorrTable->timeCorr[ i ] );
        }

        for( auto& kv : mDigiTimeCorr ) {
            kv.second->SetDirectory( 0 );
        }


        //-------------------
        // digi slewing corr
        //-------------------
        LOG_INFO << "etofDigiSlewCorr: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofDigiSlewCorr" );

        St_etofDigiSlewCorr* etofDigiSlewCorr = static_cast< St_etofDigiSlewCorr* > ( dbDataSet->Find( "etofDigiSlewCorr" ) );
        if( !etofDigiSlewCorr ) {
            LOG_ERROR << "unable to get the digi slewing correction from the database" << endm;
            return kStFatal;
        }

        etofDigiSlewCorr_st* digiSlewCorrTable = etofDigiSlewCorr->GetTable();
        
        for( size_t i=0; i<eTofConst::nChannelsInSystem; i++ ) {
            unsigned int key = channelToKey( i );

            // fill for each channel the tot-bin edges and correction values into a vector
            std::vector< float > binEdges;
            std::vector< float > corr;

            binEdges.push_back( 0. );
            for( size_t j=0; j<etofSlewing::nTotBins; j++ ) {
                int tableIndex = i * etofSlewing::nTotBins + j;

                if( i != digiSlewCorrTable->channelId[ tableIndex ] ) {
                    LOG_ERROR << "something went wrong in reading the database table for eTOF slewing corrections" << endm;
                    return kStFatal;
                }

                binEdges.push_back( digiSlewCorrTable->upperTotEdge[ tableIndex ] * etofSlewing::conversionFactor );
                corr.push_back(     digiSlewCorrTable->corr[ tableIndex ]         * etofSlewing::conversionFactor );
            }

            // create slewing correction TProfile histograms
            if( mDigiSlewCorr.count( key ) == 0 ) {
                TString name  = Form( "digiSlewCorr_%d", key );
                mDigiSlewCorr[ key ] = new TProfile( name, name, etofSlewing::nTotBins, binEdges.data() );
            }

            // fill the histograms with weight, so that GetBinEnties( bin ) is larger that mMinDigisPerSlewBin
            for( size_t j=0; j<etofSlewing::nTotBins; j++ ) {
                float totCenter = mDigiSlewCorr.at( key )->GetBinCenter( j+1 ); 
                mDigiSlewCorr.at( key )->Fill( totCenter, corr.at( j ), mMinDigisPerSlewBin + 1 );
            }
        }

        for( auto& kv : mDigiSlewCorr ) {
            kv.second->SetDirectory( 0 );
        }
    }
    else {//input from file
        LOG_INFO << "etof calibration histograms: filename provided --> use parameter file: " << mFileNameCalibHistograms.c_str() << endm;

        if( !isFileExisting( mFileNameCalibHistograms ) ) {
            LOG_ERROR << "unable to get the 'etofDigiTotCorr', 'etofDigiTimeCorr', 'etofDigiSlewCorr' parameters from file --> file does not exist" << endm;
        }

        TFile* histFile = new TFile( mFileNameCalibHistograms.c_str(), "READ" );

        if( !histFile ) {
            LOG_ERROR << "No calibration file found: " << mFileNameCalibHistograms.c_str() << endm;
            LOG_INFO  << "setting all parameters to default" << endm;
        }else if( histFile->IsZombie() ){
            LOG_ERROR << "Zombie calibration file: " << mFileNameCalibHistograms.c_str() << endm;
            LOG_INFO  << "stopping execution" << endm;
            return kStFatal;        
        }

        TFile* histOffsetFile = new TFile( mFileNameOffsetHistograms.c_str(), "READ" ); //create setter!

        if( !histOffsetFile ) {
            LOG_INFO << "No offset file found: " << mFileNameOffsetHistograms.c_str() << endm;
            LOG_INFO  << "setting all parameters to default" << endm;
        }else if( histOffsetFile->IsZombie() ) {
            LOG_ERROR << "Zombie offset file: " << mFileNameOffsetHistograms.c_str() << endm;
            LOG_INFO  << "stopping execution" << endm;
            return kStFatal;        
        }else{
            LOG_INFO  << "Successfully opened RunOffset file  "<< mFileNameOffsetHistograms.c_str() << endm;
			}



        TString hPosOffsetName = Form( "calib_Run%d_PosOffsets_pfx", runnumber );
        TString hT0OffsetName = Form( "calib_Run%d_T0Offsets_pfx", runnumber );
		  TProfile* hPosOffsetProfile = nullptr;
		  TProfile* hT0OffsetProfile = nullptr;
		  		  
		  if( histOffsetFile && !(histOffsetFile->IsZombie()) ){
           LOG_INFO  << "Getting run offset histograms Pos: "<< hPosOffsetName << " T0: "<< hT0OffsetName << endm;
		  	  hPosOffsetProfile = ( TProfile* ) histOffsetFile->Get( hPosOffsetName );
		  	  hT0OffsetProfile = ( TProfile* ) histOffsetFile->Get( hT0OffsetName );
		  }

        for( unsigned int sector = eTofConst::sectorStart; sector <= eTofConst::sectorStop; sector++ ) {

            if( mRunYear == 2018 && sector != 18 ) continue;

            for( unsigned int zPlane = eTofConst::zPlaneStart; zPlane <= eTofConst::zPlaneStop; zPlane++ ) {
                for( unsigned int counter = eTofConst::counterStart; counter <= eTofConst::counterStop; counter++ ) {

                    unsigned int key = sector * 100 + zPlane * 10  + counter;

                    LOG_DEBUG << "detectorId key: " << sector << " " << zPlane << " " << counter << endm;

                    TString hname;
                    TProfile* hProfile;

					
                    //-------------------
                    // digi tot corr
                    //-------------------
                    if( mDigiTotCorr.count( key ) == 0 ) {
                        TString name  = Form( "digiTotCorr_%d", key );
                        mDigiTotCorr[ key ] = new TH1F( name, name, 2 * eTofConst::nStrips, 0, 2 * eTofConst::nStrips );
                    }

                    hname = Form( "calib_Sector%02d_ZPlane%d_Det%d_TotDigi_pfx", sector, zPlane, counter );
                    hProfile = ( TProfile* ) histFile->Get( hname );

                    if( hProfile ) {
                        for( size_t i=1; i<=2 * eTofConst::nStrips; i++ ) {
                            if( hProfile->GetBinContent( i ) != 0 ) {
                                mDigiTotCorr.at( key )->SetBinContent( i , hProfile->GetBinContent( i ) );
                            }
                            else {
                                mDigiTotCorr.at( key )->SetBinContent( i , 1. );
                            }


                            if( mDigiTotCorr.at( key )->GetBinContent( i ) < 0.05 || mDigiTotCorr.at( key )->GetBinContent( i ) > 10 ) {
                                mDigiTotCorr.at( key )->SetBinContent( i , 1. );
                            }
                        }
                    }
                    else{
                        if( isFileExisting( mFileNameCalibHistograms ) ) {
                            LOG_WARN << "unable to find histogram: " << hname << endm;
                        }

                        for( size_t i=1; i<=2 * eTofConst::nStrips; i++ ) {
                            mDigiTotCorr.at( key )->SetBinContent( i , 1. );
                        }
                    }

                    LOG_DEBUG << "histogram " << mDigiTotCorr.at( key )->GetName() << " filled" << endm;


                    //-------------------
                    // digi time corr
                    //-------------------
                    if( mDigiTimeCorr.count( key ) == 0 ) {
                        TString name  = Form( "digiTimeCorr_%d", key );
                        mDigiTimeCorr[ key ] = new TH1F( name, name, 2 * eTofConst::nStrips, 0, 2 * eTofConst::nStrips );
                    }

                    // (1) position offset
                    hname = Form( "calib_Sector%02d_ZPlane%d_Det%d_Pos_pfx", sector, zPlane, counter );
                    hProfile = ( TProfile* ) histFile->Get( hname );

						  double dRunOffset = 0;
						  if (hPosOffsetProfile) {						  
						  		int mCounterBin = hPosOffsetProfile->FindBin(  9*(sector -13) + 3 * (zPlane - 1) + counter );
			 					dRunOffset = hPosOffsetProfile->GetBinContent( mCounterBin );
            			   LOG_DEBUG  << "setting run position offset to "<< dRunOffset<< " for counter "<< ( 9*(sector -13) + 3 * (zPlane - 1) + counter ) << endm;
						  }else{
            			   LOG_INFO  << "position offset histogram "<<hPosOffsetName <<" not found" << endm;
						  }

                    if( hProfile ) {
                        for( size_t i=1; i<= eTofConst::nStrips; i++ ) {
                            mDigiTimeCorr.at( key )->AddBinContent( i , -1. * ( hProfile->GetBinContent( i ) + dRunOffset ) / mSignalVelocity.at( key ) );
                            mDigiTimeCorr.at( key )->AddBinContent( eTofConst::nStrips + i , ( hProfile->GetBinContent( i ) + dRunOffset ) / mSignalVelocity.at( key ) );
                        }
                    }
                    else{
                        if( isFileExisting( mFileNameCalibHistograms ) ) {
                            LOG_WARN << "unable to find histogram: " << hname << endm;
                        }
                    }

                    // (2) T0 offset
                    hname = Form( "calib_Sector%02d_ZPlane%d_Det%d_T0corr", sector, zPlane, counter );
                    hProfile = ( TProfile* ) histFile->Get( hname );

						  dRunOffset = 0;
						  if (hT0OffsetProfile) {						  
								int mCounterBin = hT0OffsetProfile->FindBin(  9*(sector -13) + 3 * (zPlane - 1) + counter );
			 					dRunOffset = hT0OffsetProfile->GetBinContent( mCounterBin );
						  }
            			LOG_DEBUG  << "setting run time offset to "<< dRunOffset<< " for counter "<< ( 9*(sector -13) + 3 * (zPlane - 1) + counter ) << endm;

                    if( hProfile && hProfile->GetNbinsX() == 1 ) {
                        LOG_DEBUG << "T0 histogram with 1 bin: " << key << endm;
                        for( size_t i=1; i<=2 * eTofConst::nStrips; i++ ) {
                            mDigiTimeCorr.at( key )->AddBinContent( i , hProfile->GetBinContent( 1 ) + dRunOffset );
                        }
                    }
                    else if( hProfile && hProfile->GetNbinsX() == eTofConst::nStrips ) {
                        LOG_DEBUG << "T0 histogram with 32 bins: " << key << endm;
                        for( size_t i=1; i<= eTofConst::nStrips; i++ ) {
                            mDigiTimeCorr.at( key )->AddBinContent( i ,                      hProfile->GetBinContent( i ) + dRunOffset );
                            mDigiTimeCorr.at( key )->AddBinContent( i + eTofConst::nStrips , hProfile->GetBinContent( i ) + dRunOffset );
                        }
                    }
                    else{
                        if( isFileExisting( mFileNameCalibHistograms ) ) {
                            LOG_WARN << "unable to find histogram: " << hname << endm;
                        }                        
                    }

                    LOG_DEBUG << "histogram " << mDigiTimeCorr.at( key )->GetName() << " filled" << endm;


                    //-------------------
                    // digi slewing corr
                    //-------------------
                    for( unsigned int chan = 0; chan < eTofConst::nChannelsPerCounter; chan++ ) {
                        unsigned int strip = ( chan % 32 ) + 1;
                        unsigned int side  = ( chan / 32 ) + 1;
                        unsigned int key = sector * 100000 + zPlane * 10000  + counter * 1000 + strip * 10 + side;

                        LOG_DEBUG << "channelId key: " << sector << " " << zPlane << " " << counter << " " << strip << " " << side << endm;

                        hname    = Form( "calib_Sector%02d_ZPlane%d_Det%d_Chan%d_Walk_pfx", sector, zPlane, counter, chan );
                        hProfile = ( TProfile* ) histFile->Get( hname );

                        // check if channel-wise slewing parameters are available otherwise (due to low statistics) use detector-wise
                        if( !hProfile ) {
                            LOG_DEBUG << "unable to find histogram: " << hname << "--> check detector-wise" << endm;
                            hname    = Form( "calib_Sector%02d_ZPlane%d_Det%d_AvWalk_pfx", sector, zPlane, counter );
                            hProfile = ( TProfile* ) histFile->Get( hname );
                        }

                        if( hProfile ) {
                            unsigned int nbins = hProfile->GetNbinsX();

                            if( mDigiSlewCorr.count( key ) == 0 ) {
                                // histogram could have variable bin size --> get vector of bin edges
                                std::vector< float > bins( nbins + 1 );

                                for( size_t i=0; i<nbins; i++ ) {
                                    bins.at( i ) = hProfile->GetXaxis()->GetBinLowEdge( i+1 );
                                }
                                bins.at( nbins ) = hProfile->GetXaxis()->GetBinUpEdge( nbins );

                                TString name  = Form( "digiSlewCorr_%d", key );
                                mDigiSlewCorr[ key ] = new TProfile( name, name, nbins, bins.data() );
                            }

                            for( size_t iTotBin=1; iTotBin<=nbins; iTotBin++ ) {
                                mDigiSlewCorr.at( key )->Fill( hProfile->GetBinCenter( iTotBin ), hProfile->GetBinContent( iTotBin ), hProfile->GetBinEntries( iTotBin ) );
                            }
                        }
                        else{
                            LOG_DEBUG << "unable to find histogram: " << hname << endm;
                        }

                    }
                }
            }
        }

        for( auto& kv : mDigiTotCorr ) {
            kv.second->SetDirectory( 0 );
        }
        for( auto& kv : mDigiTimeCorr ) {
            kv.second->SetDirectory( 0 );
        }
        for( auto& kv : mDigiSlewCorr ) {
            kv.second->SetDirectory( 0 );
        }

        histFile->Close();
        delete histFile;
        histFile = nullptr;
    }

    // --------------------------------------------------------------------------------------------

    // reset time correction
    mResetTimeCorr = 0.;

    if( mFileNameResetTimeCorr.empty() ) {
        LOG_INFO << "etofResetTimeCorr: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofResetTimeCorr" );

        St_etofResetTimeCorr* etofResetTimeCorr = static_cast< St_etofResetTimeCorr* > ( dbDataSet->Find( "etofResetTimeCorr" ) );
        if( !etofResetTimeCorr ) {
            LOG_ERROR << "unable to get the reset time correction from the database" << endm;
            return kStFatal;
        }

        etofResetTimeCorr_st* resetTimeCorrTable = etofResetTimeCorr->GetTable();
        
        mResetTimeCorr = resetTimeCorrTable->resetTimeOffset;
    }
    else {
        LOG_INFO << "etofResetTimeCorr: filename provided --> use parameter file: " << mFileNameResetTimeCorr.c_str() << endm;
        
        paramFile.open( mFileNameResetTimeCorr.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etofResetTimeCorr' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        std::map< unsigned int, float > param;
        double temp;
        double temp2 = 0;
        while( paramFile >> temp ) {
            paramFile >> temp2;
            param[ ( unsigned int ) temp ] = temp2;
        }
        
        paramFile.close();

        for( const auto& kv : param ) {
            LOG_DEBUG << "run: " << kv.first << " --> reset time corr = " << kv.second << " ns" << endm;
        }

        if( param.count( runnumber ) ) {
            mResetTimeCorr = param.at( runnumber );
        }
    }

    LOG_INFO << "additional reset time offset correction: " << mResetTimeCorr << " ns" << endm;

    // --------------------------------------------------------------------------------------------

    // pulser peak tot
    mPulserPeakTot.clear();

    if( mFileNamePulserTotPeak.empty() ) {
        LOG_INFO << "etofPulserPeakTot: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofPulserTotPeak" );

        St_etofPulserTotPeak* etofPulserTotPeak = static_cast< St_etofPulserTotPeak* > ( dbDataSet->Find( "etofPulserTotPeak" ) );
        if( !etofPulserTotPeak ) {
            LOG_ERROR << "unable to get the pulser tot peak parameters from the database" << endm;
            return kStFatal;
        }

        etofPulserTotPeak_st* pulserTotTable = etofPulserTotPeak->GetTable();

        for( size_t i=0; i<eTofConst::nCountersInSystem * 2; i++ ) {
            if( pulserTotTable->pulserTot[ i ] > 0 ) {
                mPulserPeakTot[ sideToKey( i ) ] = ( int ) pulserTotTable->pulserTot[ i ];
            }
        }
    }
    else {
        LOG_INFO << "etofPulserPeakTot: filename provided --> use parameter file: " << mFileNamePulserTotPeak.c_str() << endm;

        paramFile.open( mFileNamePulserTotPeak.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etofPulserTotPeak' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        std::vector< float > param;
        float temp;
        while( paramFile >> temp ) {
            param.push_back( temp );
        }

        paramFile.close();

        if( param.size() != eTofConst::nCountersInSystem * 2 ) {
            LOG_ERROR << "parameter file for 'etofPulserTotPeak' has not the right amount of entries: ";
            LOG_ERROR << param.size() << " instead of " << eTofConst::nCountersInSystem * 2 << " !!!!" << endm;
            return kStFatal;
        }

        for( size_t i=0; i<eTofConst::nCountersInSystem * 2; i++ ) {
            if( param.at( i ) > 0 ) {
                mPulserPeakTot[ sideToKey( i ) ] = param.at( i );
            }
        }
    }

    for( const auto& kv : mPulserPeakTot ) {
        LOG_DEBUG << "side key: " << kv.first << " --> pulser peak tot = " << kv.second << " (bin)" << endm;
    }

    // --------------------------------------------------------------------------------------------

    // pulser time difference (initialized to some useful value if pulser is not there for a whole run)
    mPulserTimeDiffGbtx.clear();

    if( mFileNamePulserTimeDiffGbtx.empty() ) {
        LOG_INFO << "etofPulserTimeDiffGbtx: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofPulserTimeDiffGbtx" );

        St_etofPulserTimeDiffGbtx* etofPulserTimeDiffGbtx = static_cast< St_etofPulserTimeDiffGbtx* > ( dbDataSet->Find( "etofPulserTimeDiffGbtx" ) );
        if( !etofPulserTimeDiffGbtx ) {
            LOG_ERROR << "unable to get the pulser time difference parameters from the database" << endm;
            return kStFatal;
        }

        etofPulserTimeDiffGbtx_st* pulserTimeTable = etofPulserTimeDiffGbtx->GetTable();

        for( size_t i=0; i<eTofConst::nModules * eTofConst::nSides; i++ ) {
            unsigned int sector   = eTofConst::sectorStart  +   i / ( eTofConst::nPlanes * eTofConst::nSides );
            unsigned int zPlane   = eTofConst::zPlaneStart  + ( i % ( eTofConst::nPlanes * eTofConst::nSides ) ) / eTofConst::nSides;
            unsigned int side     = eTofConst::counterStart +   i %   eTofConst::nSides;

            mPulserTimeDiffGbtx[ sector * 1000 + zPlane * 100 + 1 * 10 + side ] = 0.;
            mPulserTimeDiffGbtx[ sector * 1000 + zPlane * 100 + 2 * 10 + side ] = pulserTimeTable->pulserTimeDiffGbtx[ i * 2 + 0 ];
            mPulserTimeDiffGbtx[ sector * 1000 + zPlane * 100 + 3 * 10 + side ] = pulserTimeTable->pulserTimeDiffGbtx[ i * 2 + 1 ];
        }
    }
    else {
        LOG_INFO << "etofPulserTimeDiff: filename provided --> use parameter file: " << mFileNamePulserTimeDiffGbtx.c_str() << endm;

        paramFile.open( mFileNamePulserTimeDiffGbtx.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etotPulserTimeDiffGbtc' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        std::vector< float > param;
        float temp;
        while( paramFile >> temp ) {
            param.push_back( temp );
        }

        paramFile.close();

        if( param.size() != eTofConst::nModules * eTofConst::nSides * 2 ) {
            LOG_ERROR << "parameter file for 'etofPulserTimeDiffGbtx' has not the right amount of entries: ";
            LOG_ERROR << param.size() << " instead of " << eTofConst::nModules * eTofConst::nSides << " !!!!" << endm;
            return kStFatal;
        }

        for( size_t i=0; i<eTofConst::nModules * eTofConst::nSides; i++ ) {
            unsigned int sector   = eTofConst::sectorStart  +   i / ( eTofConst::nPlanes * eTofConst::nSides );
            unsigned int zPlane   = eTofConst::zPlaneStart  + ( i % ( eTofConst::nPlanes * eTofConst::nSides ) ) / eTofConst::nSides;
            unsigned int side     = eTofConst::counterStart +   i %   eTofConst::nSides;

            mPulserTimeDiffGbtx[ sector * 1000 + zPlane * 100 + 1 * 10 + side ] = 0.;
            mPulserTimeDiffGbtx[ sector * 1000 + zPlane * 100 + 2 * 10 + side ] = param.at( i * 2 + 0 );
            mPulserTimeDiffGbtx[ sector * 1000 + zPlane * 100 + 3 * 10 + side ] = param.at( i * 2 + 1 );
        }
    }


    // check if all values in the map are zero --> if yes, disable pulser averaging inside a Gbtx
    bool allZero = true;

    for( const auto& kv : mPulserTimeDiffGbtx ) {
        if( fabs( kv.second ) > 1e-4 ) allZero = false;

        if( mDebug ) {
            LOG_INFO << "side key: " << kv.first << " --> pulser time diff inside Gbtx ( to counter 1 ) = " << kv.second << endm;
        }
    }

    if( allZero ) {
        mUsePulserGbtxDiff = false;
        LOG_INFO << "the use of pulser relations inside a Gbtx is turned off" << endm;
    }

    // --------------------------------------------------------------------------------------------


    return kStOk;
}


//_____________________________________________________________
Int_t
StETofCalibMaker::FinishRun( Int_t runnumber )
{
    if( mHwMap ) {
        delete mHwMap;
        mHwMap = nullptr;
    }

    // delete histograms from the map
    for( auto kv = mDigiTotCorr.begin(); kv != mDigiTotCorr.end(); kv++ ) {
        delete kv->second;
        kv->second = nullptr;
    }
    mDigiTotCorr.clear();

    for( auto kv = mDigiTimeCorr.begin(); kv != mDigiTimeCorr.end(); kv++ ) {
        delete kv->second;
        kv->second = nullptr;
    }
    mDigiTimeCorr.clear();

    for( auto kv = mDigiSlewCorr.begin(); kv != mDigiSlewCorr.end(); kv++ ) {
        delete kv->second;
        kv->second = nullptr;
    }
    mDigiSlewCorr.clear();

    mPulserPeakTot.clear();
    mPulserTimeDiff.clear();

    mJumpingPulsers.clear();

    return kStOk;
}


//_____________________________________________________________
Int_t
StETofCalibMaker::Finish()
{
    if( mDoQA ) {
        LOG_INFO << "Finish() - writing *.etofCalib.root ..." << endm;
        setHistFileName();
        writeHistograms();
    }

    return kStOk;
}

//_____________________________________________________________
Int_t
StETofCalibMaker::Make()
{
    LOG_DEBUG << "StETofCalibMaker::Make(): starting ..." << endm;

    mEvent = ( StEvent* ) GetInputDS( "StEvent" );
    //mEvent = NULL; //don't check for StEvent for genDst.C testing. PW

    if ( mEvent ) {
        LOG_DEBUG << "Make(): running on StEvent" << endm;

        StETofCollection* etofCollection = mEvent->etofCollection();
      
        if( !etofCollection ) { //additional check for empty StEvents structures produced by other Makers. Needed for genDst.C
           LOG_WARN << "Make() - Found StEvent data structure, but no eTOF collection. Try MuDst processing instead" << endm;
           mMuDst = ( StMuDst* ) GetInputDS( "MuDst" );
      
           if( mMuDst ) {
               LOG_DEBUG << "Make() - running on MuDsts" << endm;
      
               processMuDst();
      
               return kStOk;
           }
        }

        processStEvent();

        return kStOk;
    }
    else {
        LOG_DEBUG << "Make(): no StEvent found" << endm;

        mMuDst = ( StMuDst* ) GetInputDS( "MuDst" );

        if( mMuDst ) {
            LOG_DEBUG << "Make(): running on MuDsts" << endm;

            processMuDst();
        
            return kStOk;
        }
        else {
            LOG_WARN << "Make() - no StMuDst or StEvent" << endm;
            return kStOk;
        }
    }

}


//_____________________________________________________________
void
StETofCalibMaker::processStEvent()
{
    StETofCollection* etofCollection = mEvent->etofCollection();

    if( !etofCollection ) {
        LOG_WARN << "processStEvent() - no etof collection" << endm;
        return;
    }

    if( !etofCollection->digisPresent() ) {
        LOG_WARN << "processStEvent() - no digis present" << endm;
        return;
    }

    if( !etofCollection->etofHeader() ) {
        LOG_WARN << "processStEvent() - no header" << endm;
        return;
    }

    //---------------------------------

    StETofHeader*      etofHeader = etofCollection->etofHeader();
    StSPtrVecETofDigi& etofDigis  = etofCollection->etofDigis();

		/*if( mDoQA ){
			LOG_INFO << "filling missmatch histograms now" << endm;
			TClass* headerClass = etofHeader->IsA();
			if( headerClass->GetClassVersion() > 1 ){
			  LOG_INFO << "getting missmatch vector" << endm;
			  std::vector< Bool_t >  vMissmatchVec = etofHeader->missMatchFlagVec(); //lookup error?
				int iGet4Id = 0;
				for( auto iMissMatchFlag : vMissmatchVec ){
					int iCounter = iGet4Id % 16; //probalby wrong!				
			  		mHistograms[ "ETOF_QA_daqMissmatches_get4" ]->Fill(iGet4Id, iMissMatchFlag); 
			  		mHistograms[ "ETOF_QA_daqMissmatches_counter" ]->Fill(iCounter, iMissMatchFlag);
				}
			} 
		}*/

    size_t nDigis = etofDigis.size();
    if( mDebug ) {
        LOG_INFO << "processStEvent() - # fired eTOF digis : " << nDigis << endm;
    }

    mTriggerTime = triggerTime( etofHeader );
    mResetTime   = fmod( resetTime( etofHeader ), eTofConst::bTofClockCycle );

    std::map< unsigned int, std::vector< unsigned int > > pulserCandMap;

    /// first loop over digis to apply hardware mappping and find the pulsers
    for( size_t i=0; i<nDigis; i++ ) {
          //  LOG_INFO << "Digi array" << endm;
        StETofDigi* aDigi =  etofDigis[ i ];

        if( !aDigi ) {
            LOG_WARN << "No digi found" << endm;
            continue;
        }
        //    LOG_INFO << "Digi reset" << endm;
        /// reset digi to carry only raw information (needed for afterburner mode)
        resetToRaw( aDigi );

        //    LOG_INFO << "Mapping reset" << endm;
        /// apply hardware mapping from rocId, chipId, channelId to
        /// sector, zplane, counter, strip, side 
        applyMapping( aDigi );

        /// flag pulser digis
        if( mRunYear != 2018 ) {
            flagPulserDigis( aDigi, i, pulserCandMap );
        }
    }

           // LOG_INFO << "pulsers" << endm;
    if( mDebug ) {
        LOG_INFO << "size of pulserCandMap: " << pulserCandMap.size() << endm;
    }
    calculatePulserOffsets( pulserCandMap );
   
    // collect status bit information and fill good event flag for 2020+ data
	 TClass* headerClass = etofHeader->IsA();
	 if( headerClass->GetClassVersion() > 2 ){
	 	mNStatusBitsCounter.clear();			
	  std::vector< Bool_t >  vMissmatchVec = etofHeader->missMatchFlagVec();
		int iGet4Id = 0;
		for( auto iMissMatchFlag : vMissmatchVec ){
			// From DigiMaker:
		   // mMissMatchFlagVec.at( 144 * ( sector - 13 ) + 48 * ( zplane -1 ) + 16 * ( counter - 1 ) + 8 * ( side - 1 ) + ( ( strip - 1 ) / 4 ) ) = true;
		   if (iMissMatchFlag == false) continue;			
			int iCounter = iGet4Id / 16;
			if( mNStatusBitsCounter.count(iCounter) ){
				mNStatusBitsCounter[iCounter]++;
			}else{
				mNStatusBitsCounter[iCounter] = 1;
			}	
		 }
		
		std::vector<bool> goodEventFlagVec; 	
	  	for( int iCounter = 0; iCounter < 108; iCounter++){
	  		if ( !(mNPulsersCounter.count(iCounter) ) ){
	  			goodEventFlagVec.push_back(false);
	  		}else{
		  		if ( !(mNStatusBitsCounter.count(iCounter)) && mNPulsersCounter[iCounter] == 2){
		  			goodEventFlagVec.push_back(true); //true when 2 pulser digis and zero status bits are available on this counter
		  		}else{
		  			goodEventFlagVec.push_back(false);
		  		}
		  	}
	  	}
	  	if (goodEventFlagVec.size() == 108){
	  		etofHeader->setGoodEventFlagVec(goodEventFlagVec);
	  	}   
	  }

    /// second loop to apply calibrations to (non-pulser) digis inside the timing window
    StructStuckFwDigi current = { -1, -1., -1. };
    StructStuckFwDigi prev    = { -1, -1., -1. };
    int nDuplicates = 0;

    for( size_t i=0; i<nDigis; i++ ) {
        StETofDigi* aDigi =  etofDigis[ i ];

        if( !aDigi ) {
            LOG_WARN << "No digi found" << endm;
            continue;
        }

        current.geomId = aDigi->sector() * 100000 + aDigi->zPlane() * 10000 + aDigi->counter() * 1000 + aDigi->strip() * 10 + aDigi->side();
        current.tot    = aDigi->rawTot();
        current.time   = aDigi->rawTime();

        // ignore digis that were sent in bulk from the same channel with exactly the same tot and time due to stuck firmware
        auto it = std::find( mStuckFwDigi.begin(), mStuckFwDigi.end(), current );
        if( it != mStuckFwDigi.end() ) {
            if( mDebug ) {
                LOG_INFO << "digi from stuck firmware (s" << aDigi->sector() << " m" << aDigi->zPlane() << " c" << aDigi->counter() << ") --> ignore" << endm;
            }

            nDuplicates++;
            continue;
        }
        else if( current == prev ) {
            mStuckFwDigi.push_back( current );
            resetToRaw( mMuDst->etofDigi( i-1 ) );

            nDuplicates++;
            continue;
        }
        else {
            prev = current;
        }


        /// calculate calibrated time and tot for the digi
        /// only for digis inside the timing window
        applyCalibration( aDigi, etofHeader );
    }

    if( mDoQA && nDuplicates > 0 ) {
        LOG_INFO << "*** # duplicate digis from stuck firmware: " << nDuplicates << endm;
        for( const auto& v : mStuckFwDigi ) {
            LOG_INFO << "stuck channel with geomId: " << v.geomId << "  " << v.tot << "  " << v.time << endm;
        }
    }
    mStuckFwDigi.clear();
}


//_____________________________________________________________
void
StETofCalibMaker::processMuDst()
{
    LOG_DEBUG << "processMuDst(): starting ..." << endm;


    if( !mMuDst->etofArray( muETofDigi ) ) {
        LOG_WARN << "processMuDst() - no digi array" << endm;
        return;
    }

    if( !mMuDst->numberOfETofDigi() ) {
        LOG_WARN << "processMuDst() - no digis present" << endm;
        return;
    }

    if( !mMuDst->etofArray( muETofHeader ) ) {
        LOG_WARN << "processMuDst() - no digi array" << endm;
        return;
    }

    StMuETofHeader* etofHeader = mMuDst->etofHeader();
    mNPulsersCounter.clear();

    //---------------------------------

/*	if( mDoQA ){
		LOG_INFO << "filling missmatch histograms now" << endm;
		TClass* headerClass = etofHeader->IsA();
		if( headerClass->GetClassVersion() > 1 ){
		  LOG_INFO << "getting missmatch vector" << endm;
		  std::vector< Bool_t >  vMissmatchVec = etofHeader->missMatchFlagVec(); //lookup error?
			int iGet4Id = 0;
			for( auto iMissMatchFlag : vMissmatchVec ){
				int iCounter = iGet4Id % 16; //probalby wrong!				
		  		mHistograms[ "ETOF_QA_daqMissmatches_get4" ]->Fill(iGet4Id, iMissMatchFlag); 
		  		mHistograms[ "ETOF_QA_daqMissmatches_counter" ]->Fill(iCounter, iMissMatchFlag);
			}
		} 
	}*/

    size_t nDigis = mMuDst->numberOfETofDigi();
    //LOG_INFO << "processMuDst() - # fired eTOF digis : " << nDigis << endm;

    mTriggerTime = triggerTime( ( StETofHeader* ) etofHeader );
    mResetTime   = fmod( resetTime( ( StETofHeader* ) etofHeader ), eTofConst::bTofClockCycle );
    std::map< unsigned int, std::vector< unsigned int >> pulserCandMap;

    /// first loop over digis to apply hardware mappping and find the pulsers
    for( size_t i=0; i<nDigis; i++ ) {
        //LOG_INFO << "accessing etof digis: "<< i <<"/"<< nDigis << endm;
        StMuETofDigi* aDigi = mMuDst->etofDigi( i );

        if( !aDigi ) {
            LOG_WARN << "No digi found" << endm;
            continue;
        }
        /// reset digi to carry only raw information (needed for afterburner mode)
        //LOG_INFO << "resetting digi "<< i <<"/"<< nDigis << endm;
        resetToRaw( aDigi );

        /// apply hardware mapping from rocId, chipId, channelId to
        /// sector, zplane, counter, strip, side
        //LOG_INFO << "mapping digi: "<< i <<"/"<< nDigis << endm; 
        applyMapping( aDigi );

        /// flag pulser digis
    //LOG_INFO << "pulser digi flagging: "<< i <<"/"<< nDigis << endm;
        if( mRunYear != 2018 ) {
            flagPulserDigis( aDigi, i, pulserCandMap );
        }
    }

    //LOG_INFO << "size of pulserCandMap: " << pulserCandMap.size() << endm;

    calculatePulserOffsets( pulserCandMap );
    
    // collect status bit information and fill good event flag for 2020+ data
	 TClass* headerClass = etofHeader->IsA();
	 if( headerClass->GetClassVersion() > 2 ){
	 	mNStatusBitsCounter.clear();			
	  std::vector< Bool_t >  vMissmatchVec = etofHeader->missMatchFlagVec();
		int iGet4Id = 0;
		for( auto iMissMatchFlag : vMissmatchVec ){
			// From DigiMaker:
		   // mMissMatchFlagVec.at( 144 * ( sector - 13 ) + 48 * ( zplane -1 ) + 16 * ( counter - 1 ) + 8 * ( side - 1 ) + ( ( strip - 1 ) / 4 ) ) = true;
		   if (iMissMatchFlag == false) continue;			
			int iCounter = iGet4Id / 16;
			if( mNStatusBitsCounter.count(iCounter) ){
				mNStatusBitsCounter[iCounter]++;
			}else{
				mNStatusBitsCounter[iCounter] = 1;
			}	
		 }
		
		std::vector<bool> goodEventFlagVec; 	
	  	for( int iCounter = 0; iCounter < 108; iCounter++){
	  		if ( !(mNPulsersCounter.count(iCounter) ) ){
	  			goodEventFlagVec.push_back(false);
	  		}else{
		  		if ( !(mNStatusBitsCounter.count(iCounter)) && mNPulsersCounter[iCounter] == 2){
		  			goodEventFlagVec.push_back(true); //true when 2 pulser digis and zero status bits are available on this counter
		  		}else{
		  			goodEventFlagVec.push_back(false);
		  		}
		  	}
	  	}
	  	if (goodEventFlagVec.size() == 108){
	  		etofHeader->setGoodEventFlagVec(goodEventFlagVec);
	  	}   
	  }

    /// second loop to apply calibrations to (non-pulser) digis inside the timing window
    StructStuckFwDigi current = { -1, -1., -1. };
    StructStuckFwDigi prev    = { -1, -1., -1. };
    int nDuplicates = 0;

    for( size_t i=0; i<nDigis; i++ ) {
        StMuETofDigi* aDigi = mMuDst->etofDigi( i );

        if( !aDigi ) {
            LOG_WARN << "No digi found" << endm;
            continue;
        }

        current.geomId = aDigi->sector() * 100000 + aDigi->zPlane() * 10000 + aDigi->counter() * 1000 + aDigi->strip() * 10 + aDigi->side();
        current.tot    = aDigi->rawTot();
        current.time   = aDigi->rawTime();

        // ignore digis that were sent in bulk from the same channel with exactly the same tot and time due to stuck firmware
        auto it = std::find( mStuckFwDigi.begin(), mStuckFwDigi.end(), current );
        if( it != mStuckFwDigi.end() ) {
            if( mDebug ) {
                LOG_INFO << "digi from stuck firmware (s" << aDigi->sector() << " m" << aDigi->zPlane() << " c" << aDigi->counter() << ") --> ignore" << endm;
            }

            nDuplicates++;
            continue;
        }
        else if( current == prev ) {
            mStuckFwDigi.push_back( current );
            resetToRaw( mMuDst->etofDigi( i-1 ) );

            nDuplicates++;
            continue;
        }
        else {
            prev = current;
        }


        /// calculate calibrated time and tot for the digi
        /// only for digis inside the timing window
        applyCalibration( aDigi, etofHeader );
    }

    if( mDoQA && nDuplicates > 0 ) {
        LOG_INFO << "*** # duplicate digis from stuck firmware: " << nDuplicates << endm;
        for( const auto& v : mStuckFwDigi ) {
            LOG_INFO << "stuck channel with geomId: " << v.geomId << "  " << v.tot << "  " << v.time << endm;
        }
    }
    mStuckFwDigi.clear();
}
//_____________________________________________________________


//_____________________________________________________________
bool
StETofCalibMaker::isFileExisting( const std::string fileName )
{
    std::ifstream infile( fileName );
    return infile.good();
}


//_____________________________________________________________
/*!
 * reset all calibrated information (needed for running in afterburner mode)
 */
void
StETofCalibMaker::resetToRaw( StETofDigi* aDigi )
{
    if( !aDigi ) return;

    aDigi->setGeoAddress( 0, 0, 0, 0, 0 );
    aDigi->setCalibTime( 0. );
    aDigi->setCalibTot( -1. );

    aDigi->setAssociatedHit( nullptr );
}


//_____________________________________________________________
/*!
 * apply electronic address mapping to geometry
 */
void
StETofCalibMaker::applyMapping( StETofDigi* aDigi )
{
    std::vector< unsigned int > geomVec;

    unsigned int rocId  = aDigi->rocId();
    unsigned int get4Id = aDigi->get4Id();
    unsigned int elChan = aDigi->elChan();

    mHwMap->mapToGeom( rocId, get4Id, elChan, geomVec );

    if( geomVec.size() < 5 ) {
        if( mDebug ) {
            LOG_ERROR << "geometry vector has wrong size !!! --> skip digi" << endm;
        }
        return;
    }

    unsigned int sector  = geomVec.at( 0 );
    unsigned int zplane  = geomVec.at( 1 );
    unsigned int counter = geomVec.at( 2 );
    unsigned int strip   = geomVec.at( 3 );
    unsigned int side    = geomVec.at( 4 );

    if( mDebug && ( sector == 0 || zplane == 0 || counter == 0 || strip == 0 || side == 0 ) ) {
        LOG_ERROR << "geometry vector has entries equal to zero !!! --> skip digi" << endm;
    }

    aDigi->setGeoAddress( sector, zplane, counter, strip, side );

    if( mDebug ) {
        // print out the new information
        LOG_DEBUG << "sector, zplane, counter, strip, side: " << aDigi->sector() << ", ";
        LOG_DEBUG << aDigi->zPlane()    << ", " << aDigi->counter()  << ", ";
        LOG_DEBUG << aDigi->strip()     << ", " << aDigi->side()     << endm;

        LOG_DEBUG << "continuous module number: " << mHwMap->module( aDigi->sector(), aDigi->zPlane() ) << endm;
    }
}


//_____________________________________________________________
/*!
 * flag pulser digis
 */
void
StETofCalibMaker::flagPulserDigis( StETofDigi* aDigi, unsigned int index, std::map< unsigned int, std::vector< unsigned int > >& pulserDigiMap )
{
    bool isPulserCand = false;

    unsigned int key = aDigi->sector() * 1000 + aDigi->zPlane() * 100 + aDigi->counter() * 10 + aDigi->side();

    // pulser channel
    if( ( aDigi->strip() == 1 && aDigi->side() == 1 ) || ( aDigi->strip() == 32 && aDigi->side() == 2 ) ) {
        float timeToTrigger = aDigi->rawTime() - mTriggerTime;
        float totToPeak     = aDigi->rawTot()  - mPulserPeakTot.at( key );
        float totToHalfPeak = aDigi->rawTot()  - mPulserPeakTot.at( key ) * 0.5;

        if( timeToTrigger > mPulserWindow.at( aDigi->rocId() ).first  && timeToTrigger < mPulserWindow.at( aDigi->rocId() ).second  ) {
            if( fabs( totToPeak ) < 25 || fabs( totToHalfPeak ) < 10 ) {
                isPulserCand = true;
            }
        }
    }

    if( isPulserCand ) {
        pulserDigiMap[ key ].push_back( index );
    }
}


//_____________________________________________________________
/*!
 * calculate pulser timing offsets & set calib tot of used pulsers to -999;
 * the offsets are stored as member variable of the CalibMaker and keep set for the next event
 * in case some of the pulsers are missing
 */
void
StETofCalibMaker::calculatePulserOffsets( std::map< unsigned int, std::vector< unsigned int > >& pulserDigiMap )
{
    if( mDebug ) {
        for( auto it=pulserDigiMap.begin(); it!=pulserDigiMap.end(); it++ ) {
            LOG_DEBUG << "channel: " << it->first << "   nCandidates: " << it->second.size() << endm;
        }
    }

    if( mReferencePulserIndex == 0 ) {
        if( mDebug ) {
            LOG_INFO << "reference pulser index is 0 --> pulser correction is turned off" << endm;
        }
        return;
    }

    if( mDebug ) {
        LOG_INFO << "reference pulser index: " << mReferencePulserIndex << endm;
    }

    std::map< int, double > pulserTimes;
    mNPulsersCounter.clear();
	 mPulserPresent.clear(); //clear map of present pulsers in each event

    // loop over all candidates to find real pulser, save time in pulserTimes map
    for( auto it=pulserDigiMap.begin(); it!=pulserDigiMap.end(); it++ ) {
        if( it->second.size() == 0 ) {
            continue;
        }
        int sideIndex = it->first;

        double bestDiff  = 100000;
        int candIndex = -1;

        for( size_t j=0; j<it->second.size(); j++ ) {
            double pulserTime = 0.;
            double pulserTot  = 0.;
            if( mMuDst ) {
                pulserTime = mMuDst->etofDigi( it->second.at( j ) )->rawTime();
                pulserTot  = mMuDst->etofDigi( it->second.at( j ) )->rawTot();
            } else if( mEvent ) {
                pulserTime = ( mEvent->etofCollection()->etofDigis() )[ it->second.at( j ) ]->rawTime();
                pulserTot  = ( mEvent->etofCollection()->etofDigis() )[ it->second.at( j ) ]->rawTot();
            }
            double timeToTrigger = pulserTime - mTriggerTime;
            double totToPeak     = pulserTot  - mPulserPeakTot.at( sideIndex );

            if( mDebug && it->second.size() > 1 ) {
                LOG_INFO << it->second.size() <<  " pulsers @ " << sideIndex << " : timeToTrigger: " << timeToTrigger << "  tot: " << pulserTot << endm;
            }
            
            // find "best fitting digi", remove other digis (likely misidentified noise)
            double currentDiff = fabs( timeToTrigger - mPulserPeakTime ) * 0.1 + fabs( totToPeak );   // might need better criterion? Normalisation to widths? PW
            if( currentDiff < bestDiff ) {
                bestDiff = currentDiff;
                candIndex = j;
            }
        }

        if( mDebug && it->second.size() > 1 ) {
            LOG_INFO << " --> selected CAND-INDEX: " << candIndex << endm;
        }

        double pulserTime = 0.;

        if( mMuDst ) {
            pulserTime = mMuDst->etofDigi( it->second.at( candIndex ) )->rawTime();

            // set calibTot to -999. to exclude it from being calibrated in the next step --> pulser will not be used to build hits
            mMuDst->etofDigi( it->second.at( candIndex ) )->setCalibTot( -999. );
        } else if( mEvent ) {
            pulserTime = ( mEvent->etofCollection()->etofDigis() )[ it->second.at( candIndex ) ]->rawTime();

            // set calibTot to -999. to exclude it from being calibrated in the next step --> pulser will not be used to build hits
            mEvent->etofCollection()->etofDigis() [ it->second.at( candIndex ) ]->setCalibTot( -999. );
        }

        pulserTimes[ sideIndex ] = pulserTime;
        
        int sector = sideIndex / 1000;
        int plane  = ( sideIndex % 1000) / 100;
        int counter  = ( sideIndex % 100) / 10;
        int key = 9 * ( sector - 13 ) + 3 * ( plane - 1 ) + ( counter - 1 ); 
        if( mNPulsersCounter.count( key ) ){
        		mNPulsersCounter[key]++;        
        }else{
            mNPulsersCounter[key] = 1;        
        }

		  mPulserPresent[ sideIndex ] = true;
    }

    double referenceTime = 0.;

    // update reference time (for QA)
  if( pulserTimes.count( mReferencePulserIndex ) ) {
    referenceTime = pulserTimes.at( mReferencePulserIndex ); //only updated for QA?? needed to remove smeared pulsers
    if( mDoQA ) {
            if( mDebug ) {
                LOG_INFO << "preliminary reference time:" << referenceTime << endm;
            }
        }
    }


    // deal with the pulser times --> tweek pulser times based on time differences inside/outside a Gbtx
    if( mUsePulserGbtxDiff ) {
        for( int gbtxId = 0; gbtxId < eTofConst::nModules * eTofConst::nSides; gbtxId++ ) {
            int sector  = eTofConst::sectorStart +   gbtxId / ( eTofConst::nPlanes * eTofConst::nSides );
            int zPlane  = eTofConst::zPlaneStart + ( gbtxId % ( eTofConst::nPlanes * eTofConst::nSides ) ) / eTofConst::nSides;
            int side    = eTofConst::sideStart   +   gbtxId % eTofConst::nSides;

            int partialKey = sector * 1000 + zPlane * 100 + side;

            vector< double > times( eTofConst::nCounters );

            double average = 0.;
            int    nAv     = 0;

            for( int counter = 1; counter <= eTofConst::nCounters; counter++ ) {
                int key = partialKey + 10 * counter;

                if( pulserTimes.count( key ) ) {
                    if( mDoQA ) {// fill if all relevant pulsers are available.
                        if( pulserTimes.count( partialKey + 10 ) ) {
                            mHistograms.at( "pulserDigiTimeToCounter1" )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, pulserTimes.at( partialKey + 10 ) - pulserTimes.at( key ) );
                        }
                        if( pulserTimes.count( partialKey + 20 ) ) {
                            mHistograms.at( "pulserDigiTimeToCounter2" )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, pulserTimes.at( partialKey + 20 ) - pulserTimes.at( key ) );
                        }
                        if( pulserTimes.count( partialKey + 30 ) ) {
                            mHistograms.at( "pulserDigiTimeToCounter3" )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, pulserTimes.at( partialKey + 30 ) - pulserTimes.at( key ) );
                        }
                    }

                    times.at( counter - 1 ) = pulserTimes.at( key ) + mPulserTimeDiffGbtx.at( key ); //substract pulser time difference from database table

                    bool isNonSmearedPulser = false;
                    if( referenceTime != 0 ) { 
                        double dist    = times.at( counter - 1 ) - referenceTime; //distance to reference pulser
                        double redDist = mHistograms.at( "pulserDigiTimeDiff_GbtxCorrProfMod" )->GetBinContent( gbtxId * eTofConst::nCounters + counter ); // average distance to next clock edge for this pulser
                        
                        double modDist = fmod( dist - redDist, eTofConst::coarseClockCycle ); //Distance to "normal" offset. full clock cycle distances are thrown out? Why? 
                        modDist = modDist - eTofConst::coarseClockCycle * round( modDist / eTofConst::coarseClockCycle ); //substract a clock cycle if modDist > 0.5 coarseClockCycle 
                        //=> -0.5*coarseClockCycle < modDist < 0.5*coarseClockCycle => Distance to closest clock cycle
                        
                        if( redDist == 0 || fabs( modDist ) > 0.5 ) { //> 0.5ns + n*ClockCycle away from reference pulser. Hard cut? If the first pulser is off, all following will be neglected!
                            if( redDist != 0 ) LOG_INFO << "too far away --> smeared pulser: " << key << "(" << gbtxId << "-" << counter << ")" << endm;
                            redDist = dist; //empty in the beginning, Example distance to reference pulser
                        }
                        else {
                            redDist += modDist; //adds always up?
                            isNonSmearedPulser = true;
                        }
          
                        mHistograms.at( "pulserDigiTimeDiff_GbtxCorrProf"    )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, dist    );
                        mHistograms.at( "pulserDigiTimeDiff_GbtxCorrProfMod" )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, redDist ); // TProfile! => Average!

                        if( mDoQA ) {
                            mHistograms.at( "pulserDigiTimeDiff_GbtxCorr" )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, dist ); //Pulser offset on GBTX from database substracted
                            mHistograms.at( "pulserDigiTimeDiff"          )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, pulserTimes.at( key ) - referenceTime ); //Pulser offset on GBTX not substracted
                        }
                    }

                    // only use non-smeared pulsers for the 
                    if( isNonSmearedPulser ) {
                        average += times.at( counter - 1 );
                        nAv++;
                    }
                    else {
                        times.at( counter - 1 ) = 0.;
                    }

                }
            }


            if( nAv == eTofConst::nCounters ) { //all pulser present, check for single pulser jumps by comparing to average of the other two.
                double diff12 = fabs( times.at( 0 ) - times.at( 1 ) );
                double diff13 = fabs( times.at( 0 ) - times.at( 2 ) );
                double diff23 = fabs( times.at( 1 ) - times.at( 2 ) );

                if( diff12 > 0.2 && diff13 > 0.2 && diff23 < 0.2 ) {
                    average -= times.at( 0 );
                    nAv--;

                    if( fabs( times.at( 0 ) - ( average / nAv ) + eTofConst::coarseClockCycle ) < 0.2 ) {
                        mJumpingPulsers[ partialKey + 10 ] = 1;

                        times.at( 0 ) += eTofConst::coarseClockCycle;
                        average       += times.at( 0 );
                        nAv++;
                    }
                    else if( fabs( times.at( 0 ) - ( average / nAv ) - eTofConst::coarseClockCycle ) < 0.2 ) {
                        mJumpingPulsers[ partialKey + 10 ] = -1;
                    
                        times.at( 0 ) -= eTofConst::coarseClockCycle;
                        average       += times.at( 0 );
                        nAv++;
                    }

                    if( mDoQA ) {
                        mHistograms.at( "1_off" )->Fill( gbtxId * eTofConst::nCounters + 1.5, times.at( 0 ) - ( average / nAv ) );
                    }
                }
                else if( diff12 > 0.2 && diff13 < 0.2 && diff23 > 0.2 ) {
                    average -= times.at( 1 );
                    nAv--;

                    if( fabs( times.at( 1 ) - ( average / nAv ) + eTofConst::coarseClockCycle ) < 0.2 ) {
                        mJumpingPulsers[ partialKey + 20 ] = 1;

                        times.at( 1 ) += eTofConst::coarseClockCycle;
                        average       += times.at( 1 );
                        nAv++;
                    }
                    else if( fabs( times.at( 1 ) - ( average / nAv ) - eTofConst::coarseClockCycle ) < 0.2 ) {
                        mJumpingPulsers[ partialKey + 20 ] = -1;

                        times.at( 1 ) -= eTofConst::coarseClockCycle;
                        average       += times.at( 1 );
                        nAv++;
                    }

                    if( mDoQA ) {
                        mHistograms.at( "2_off" )->Fill( gbtxId * eTofConst::nCounters + 1.5, times.at( 1 ) - ( average / nAv ) );
                    }
                }
                else if( diff12 < 0.2 && diff13 > 0.2 && diff23 > 0.2 ) {
                    average -= times.at( 2 );
                    nAv--;

                    if( fabs( times.at( 2 ) - ( average / nAv ) + eTofConst::coarseClockCycle ) < 0.2 ) {
                        mJumpingPulsers[ partialKey + 30 ] = 1;

                        times.at( 2 ) += eTofConst::coarseClockCycle;
                        average       += times.at( 2 );
                        nAv++;
                    }
                    else if( fabs( times.at( 2 ) - ( average / nAv ) - eTofConst::coarseClockCycle ) < 0.2 ) {
                        mJumpingPulsers[ partialKey + 30 ] = -1;

                        times.at( 2 ) -= eTofConst::coarseClockCycle;
                        average       += times.at( 2 );
                        nAv++;
                    }

                    if( mDoQA ) {
                        mHistograms.at( "3_off" )->Fill( gbtxId * eTofConst::nCounters + 1.5, times.at( 2 ) - ( average / nAv ) );
                    }
                }
            }

            if( nAv == eTofConst::nCounters - 1 ) {
                // if there are two pulsers, restore missing pulser from average of the other two 
                if( times.at( 0 ) > 0 && times.at( 1 ) > 0 && fabs( fabs( times.at( 0 ) - times.at( 1 ) ) - eTofConst::coarseClockCycle ) < 0.2 ) {
                    if( mJumpingPulsers.count( partialKey + 10 ) ) {
                        //LOG_INFO << gbtxId << " ### case 1 (1) ### " << endm;
                        times.at( 0 ) += mJumpingPulsers.at( partialKey + 10 ) * eTofConst::coarseClockCycle;
                        average       += mJumpingPulsers.at( partialKey + 10 ) * eTofConst::coarseClockCycle;
                    } 
                    else if( mJumpingPulsers.count( partialKey + 20 ) ) {
                        //LOG_INFO << gbtxId << " ### case 1 (2) ### " << endm;
                        times.at( 1 ) += mJumpingPulsers.at( partialKey + 20 ) * eTofConst::coarseClockCycle;
                        average       += mJumpingPulsers.at( partialKey + 20 ) * eTofConst::coarseClockCycle;
                    }
                    else {
                        //LOG_INFO << gbtxId << " ### case 1 (3) ### " << endm;
                        if( times.at( 0 ) < times.at( 1 ) ) {
                            times.at( 0 ) += eTofConst::coarseClockCycle;
                            average       += eTofConst::coarseClockCycle;
                        }
                        else {
                            times.at( 1 ) += eTofConst::coarseClockCycle;
                            average       += eTofConst::coarseClockCycle;
                        }
                    }
                }
                else if( times.at( 0 ) && times.at( 2 ) > 0 && fabs( fabs( times.at( 0 ) - times.at( 2 ) ) - eTofConst::coarseClockCycle ) < 0.2 ) {
                    if( mJumpingPulsers.count( partialKey + 10 ) ) {
                        //LOG_INFO << gbtxId << " ### case 2 (1) ### " << endm;
                        times.at( 0 ) += mJumpingPulsers.at( partialKey + 10 ) * eTofConst::coarseClockCycle;
                        average       += mJumpingPulsers.at( partialKey + 10 ) * eTofConst::coarseClockCycle;
                    } 
                    else if( mJumpingPulsers.count( partialKey + 30 ) ) {
                        //LOG_INFO << gbtxId << " ### case 2 (2) ### " << endm;
                        times.at( 2 ) += mJumpingPulsers.at( partialKey + 30 ) * eTofConst::coarseClockCycle;
                        average       += mJumpingPulsers.at( partialKey + 30 ) * eTofConst::coarseClockCycle;
                    }
                    else {
                        //LOG_INFO << gbtxId << " ### case 2 (3) ### " << endm;
                        if( times.at( 0 ) < times.at( 2 ) ) {
                            times.at( 0 ) += eTofConst::coarseClockCycle;
                            average       += eTofConst::coarseClockCycle;
                        }
                        else {
                            times.at( 2 ) += eTofConst::coarseClockCycle;
                            average       += eTofConst::coarseClockCycle;
                        }
                    }
                }
                else if( times.at( 1 ) > 0 && times.at( 2 ) > 0 && fabs( fabs( times.at( 1 ) - times.at( 2 ) ) - eTofConst::coarseClockCycle ) < 0.2 ) {
                    if( mJumpingPulsers.count( partialKey + 20 ) ) {
                        //LOG_INFO << gbtxId << " ### case 3 (1) ### " << endm;
                        times.at( 1 ) += mJumpingPulsers.at( partialKey + 20 ) * eTofConst::coarseClockCycle;
                        average       += mJumpingPulsers.at( partialKey + 20 ) * eTofConst::coarseClockCycle;
                    } 
                    else if( mJumpingPulsers.count( partialKey + 30 ) ) {
                        //LOG_INFO << gbtxId << " ### case 3 (2) ### " << endm;
                        times.at( 2 ) += mJumpingPulsers.at( partialKey + 30 ) * eTofConst::coarseClockCycle;
                        average       += mJumpingPulsers.at( partialKey + 30 ) * eTofConst::coarseClockCycle;
                    }
                    else {
                        //LOG_INFO << gbtxId << " ### case 3 (3) ### " << endm;
                        if( times.at( 1 ) < times.at( 2 ) ) {
                            times.at( 1 ) += eTofConst::coarseClockCycle;
                            average       += eTofConst::coarseClockCycle;
                        }
                        else {
                            times.at( 2 ) += eTofConst::coarseClockCycle;
                            average       += eTofConst::coarseClockCycle;
                        }
                    }
                }
            }


            if( nAv >= 2 ) {
                average /= nAv;
            }

            if( mDoQA && referenceTime != 0 ) {
                mHistograms.at( "pulserDigiTimeDiff_perGbtx" )->Fill( gbtxId * eTofConst::nCounters + 1.5, average - referenceTime );
            }

            for( int counter = eTofConst::counterStart; counter <= eTofConst::counterStop; counter++ ) {
                double diffToAv = 0.;

                if( times.at( counter - eTofConst::counterStart ) != 0. ) {
                    diffToAv = times.at( counter - eTofConst::counterStart ) - average;

                    if( fabs( diffToAv ) > 0.2 ) diffToAv = 0.; //removing didn't work

                    if( mDoQA ) {
                        mHistograms.at( "pulserDigiTimeDiff_toAverage" )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, diffToAv );
                    }
                }

                if( average != 0. ) {//only allow counter pulsers that are now close to average
                     //times.at( counter - 1 ) = pulserTimes.at( key ) + mPulserTimeDiffGbtx.at( key )
                    pulserTimes[ partialKey + 10 * counter ] = average + diffToAv - mPulserTimeDiffGbtx.at( partialKey + 10 * counter ); //restores original pulser times INCLUDING GBTX offset ?!?!
							 //pulserTimes[ partialKey + 10 * counter ] = average;
                }
                else {
                    if( pulserTimes.count( partialKey + 10 * counter ) ) {
                        pulserTimes.erase( partialKey + 10 * counter );
                    }
                }
            }
        }
    }


    // calculate difference to the reference
    referenceTime = 0.;
    if( pulserTimes.count( mReferencePulserIndex ) ) {
		  if( mPulserTimeDiff.count( mReferencePulserIndex ) ){
           referenceTime = pulserTimes.at( mReferencePulserIndex ) - mPulserTimeDiff[ mReferencePulserIndex ];
           //LOG_INFO << "time of reference pulser updated: " << referenceTime << " with reference correction "<< mPulserTimeDiff[ mReferencePulserIndex ] << endm;
		  }else{
			  referenceTime = pulserTimes.at( mReferencePulserIndex );
			  //LOG_INFO << "time of reference pulser updated: " << referenceTime << endm;
		  }
    }  



    if( referenceTime != 0 ) {
        int iLockThreshold = 0;
	     mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->Reset("ICESM");

        for( auto& kv : pulserTimes ) {
            // check if new pulser time difference seems reasonable ( only allow jumps in multiple of the coarse clock tick ) to avoid smeared pulsers
            if( mPulserTimeDiff.count( kv.first ) ) {//pulser time difference default available previous events
                //double modDist = fmod( mPulserTimeDiff.at( kv.first ) - ( kv.second - referenceTime ), eTofConst::coarseClockCycle );
                //modDist = modDist - eTofConst::coarseClockCycle * round( modDist / eTofConst::coarseClockCycle );

					 double modDist = mPulserTimeDiff.at( kv.first ) - ( kv.second - referenceTime ); //test PW
                //modDist = modDist - eTofConst::coarseClockCycle * round( modDist / eTofConst::coarseClockCycle );


                if( fabs( modDist ) > 0.2 ) {
                    mUnlockPulserState[ kv.first ]++;

                   // LOG_INFO << " pulser time " << kv.first << " seems unreasonable (" << kv.second - referenceTime << ")";
                   // LOG_INFO << " compared to previous stored value (" << mPulserTimeDiff.at( kv.first ) << ")" << endm;
                        
                    // only unlock pulser state if 10 consecutive events have a modDist larger then the threshold
                    if( mUnlockPulserState.at( kv.first ) < iLockThreshold ) {
                       // LOG_INFO << " --> ignore for now and move on" << endm;
                        continue; //move on, don't update pulser times!
                    }
                    else{ 
                       // LOG_INFO << " --> pulser state has been unlocked" << endm;
                    }

							//fill 2d Hist here with GBTX and counter number
							mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->Fill( kv.second - referenceTime );

                }
                else{
                    if( mUnlockPulserState.count( kv.first ) ) {
                        LOG_INFO << " --> new event doesn't have offset for pulser " << kv.first << " --> remove the entry" << endm;
                        mUnlockPulserState.erase( kv.first );
                    }
                }
            }
				//pulser time differece was set here!


            if( mDoQA ) {
                int sector  = kv.first / 1000;
                int zPlane  = ( kv.first % 1000 ) / 100;
                int counter = ( kv.first % 100 )  / 10;
                int side    = kv.first % 10;

                int gbtxId = ( sector - eTofConst::sectorStart ) * ( eTofConst::nPlanes * eTofConst::nSides )
                           + ( zPlane - eTofConst::zPlaneStart ) * eTofConst::nSides
                           + ( side   - eTofConst::sideStart   );

                if( mPulserTimeDiff.count( kv.first ) ) {
                    mHistograms.at( "pulserDigiTimeDiff_fullCorr" )->Fill( gbtxId * eTofConst::nCounters + counter - 0.5, mPulserTimeDiff.at( kv.first ) );
                } 

					 mHistograms[ "pulserDigiPresence" ]->Fill(gbtxId * eTofConst::nCounters + counter - 0.5);
            }
        }

			//LOG_INFO << "Check  " << referenceTime << endm;
         if( mDoQA ) {					 
				mHistograms[ "pulserDigiPresence" ]->Fill( -1 ); //use as event counter
		   	mHistograms[ "pulserDigiTimeDiff_CorrAgreement"  ]->Fill( mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->GetMaximum() );
				mHistograms[ "pulserDigiTimeDiff_CorrCommonJump" ]->Fill( mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->GetEntries() );
			}
			if( ( mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->GetEntries() > 150 ) && ( mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->GetMaximum() > 15 ) ){

				//LOG_INFO << "Check: found  " << mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->GetMaximum() <<" agreeing time shifts. Shifting reference times " << endm;
				int iMaximumBin        = mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->GetMaximumBin();
				double dRefCorrAverage = 0;
				int iNRefCorrAgree     = 0;

	   		for( auto& kv : pulserTimes ) { //build average of all agreeing pulsers
					if ( mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->FindBin(kv.second - referenceTime) == iMaximumBin ){
						dRefCorrAverage += kv.second - referenceTime;
						iNRefCorrAgree++;
					}
				}
				dRefCorrAverage                          /= iNRefCorrAgree;
				referenceTime                            -= dRefCorrAverage;
				//pulserTimes.at( mReferencePulserIndex )  -= dRefCorrAverage;
				mPulserTimeDiff[ mReferencePulserIndex ] -= dRefCorrAverage;

			}else{
				//LOG_INFO << "Check: found only " << mHistograms[ "pulserDigiTimeDiff_RefCorr"  ]->GetMaximum() <<" agreeing time shifts. Shifting pulser times " << endm;
	   		for( auto& kv : pulserTimes ) {
					if ( ! mPulserTimeDiff[ kv.first ] ) {
				   		mPulserTimeDiff[ kv.first ] = kv.second - referenceTime;
							continue;
					}
					if( mUnlockPulserState.count( kv.first ) ){
						if( mUnlockPulserState.at( kv.first ) > iLockThreshold ){// check if pulser is locked
				   		mPulserTimeDiff[ kv.first ] = kv.second - referenceTime;
						}
					}
				}
			}
    }
}


//_____________________________________________________________
/*!
 * apply calibrations
 */
void
StETofCalibMaker::applyCalibration( StETofDigi* aDigi, StETofHeader* etofHeader )
{
    int key = aDigi->sector() * 100000 + aDigi->zPlane() * 10000  + aDigi->counter() * 1000 + aDigi->strip() * 10 + aDigi->side();
    if( !mStatus.count( key) || mStatus.at( key ) != 1 ) {
        if( mDebug ) {
            LOG_DEBUG << "status of channel with key " << key << " was not ok ---> skip calibrating this digi" << endm;
        }
        return;
    }

    // ignore digis flaged as pulsers ( calibTot = -999. )
    if( fabs( aDigi->calibTot() + 999. ) < 1.e-5 ) {
        if( mDebug ) {
            LOG_DEBUG << "digi flaged as pulser --> skip" << endm;
        }
        return;
    }

    float timeToTrigger = aDigi->rawTime() - mTriggerTime;

    // check if digi is inside the timing window and only calibrate those, do nothing digis outside the window ( calibTime = 0, calibTot = -1 )
    if( timeToTrigger > mTimingWindow.at( aDigi->rocId() ).first  &&
        timeToTrigger < mTimingWindow.at( aDigi->rocId() ).second  )
    {

			if( mStrictPulserHandling ){
         int PulserKey = aDigi->sector() * 1000 + aDigi->zPlane() * 100 + aDigi->side() + 10 * aDigi->counter();
				if( !mPulserPresent.count( PulserKey ) ) {
				  if( mDebug ) {
						LOG_DEBUG << "no pulser in the same event for this counter --> digi skipped due to strict pulser handling" << endm;
				  }
				  return;
				}
			}

        double calibTot = aDigi->rawTot() * mGet4TotBinWidthNs * calibTotFactor( aDigi );

        aDigi->setCalibTot( calibTot );

        double calibTime = aDigi->rawTime() - mResetTime
                                            - resetTimeCorr()
                                            - calibTimeOffset(   aDigi )
                                            - slewingTimeOffset( aDigi )
                                            - applyPulserOffset( aDigi );

        aDigi->setCalibTime( calibTime );

        if( mDebug ) {
            // print out the new information
            LOG_DEBUG << "raw Time, ToT: "        << aDigi->rawTime()   << ", " << aDigi->rawTot()   << endm;
            LOG_DEBUG << "calibrated Time, ToT: " << aDigi->calibTime() << ", " << aDigi->calibTot() << endm;
        }

    }
    else{
        if( mDebug ) {
            LOG_DEBUG << "digi is outside the timing window (time to trigger = " << timeToTrigger << ")  --> skip" << endm;
        }
    }
}
//_____________________________________________________________


//_____________________________________________________________
void
StETofCalibMaker::resetToRaw( StMuETofDigi* aDigi )
{
    if( !aDigi ) return;

    aDigi->setGeoAddress( 0, 0, 0, 0, 0 );
    aDigi->setCalibTime( 0. );
    aDigi->setCalibTot( -1. );

    aDigi->setAssociatedHitId( -1 );
}


//_____________________________________________________________
void
StETofCalibMaker::applyMapping( StMuETofDigi* aDigi )
{
    applyMapping( ( StETofDigi* ) aDigi );
}


//_____________________________________________________________
void
StETofCalibMaker::flagPulserDigis( StMuETofDigi* aDigi, unsigned int index, std::map< unsigned int, std::vector< unsigned int > >& pulserDigiMap )
{
    flagPulserDigis( ( StETofDigi* ) aDigi, index, pulserDigiMap );
}


//_____________________________________________________________
void
StETofCalibMaker::applyCalibration( StMuETofDigi* aDigi, StMuETofHeader* etofHeader )
{
    applyCalibration( ( StETofDigi* ) aDigi, ( StETofHeader* ) etofHeader );
}
//_____________________________________________________________


//_____________________________________________________________
/*!
 * get tot rescaling from calibration histograms to digis
 */
double
StETofCalibMaker::calibTotFactor( StETofDigi* aDigi )
{
    unsigned int key = aDigi->sector() * 100 + aDigi->zPlane() * 10 + aDigi->counter();
    unsigned int bin = aDigi->strip() + 32 * ( aDigi->side() - 1 );

    if( mDigiTotCorr.count( key ) ) {
        float binContent = mDigiTotCorr.at( key )->GetBinContent( bin );

        if( fabs( binContent ) > 1e-5 ) {
            if( mDebug ) {
                LOG_DEBUG << "calibTotFactor: histogram with key " << key << " at bin " << bin << " -> return bin content: " << binContent << endm;
            }
            return (1.0/binContent); //invert here to get to fixed mean value!
        }
        else {
            if( mDebug ) {
                LOG_WARN << "calibTotFactor: histogram with key " << key << " at bin " << bin << " has content of 0 -> return 1" << endm;
            }
            return 1.;
        }
    }
    else {
        if( mDebug ) {
            LOG_WARN << "calibTotFactor: required histogram with key " << key << " doesn't exist -> return 1" << endm;
        }
        return 1.;
    }     
}


//_____________________________________________________________
/*!
 * get time offsets from calibration histograms to digis
 */
double
StETofCalibMaker::calibTimeOffset( StETofDigi* aDigi )
{
    unsigned int key = aDigi->sector() * 100 + aDigi->zPlane() * 10 + aDigi->counter();
    unsigned int bin = aDigi->strip() + 32 * ( aDigi->side() - 1 );

    if( mDigiTimeCorr.count( key ) ) {
        float binContent = mDigiTimeCorr.at( key )->GetBinContent( bin );
        if( mDebug ) {
            LOG_DEBUG << "calibTimeOffset: histogram with key " << key << " at bin " << bin << " -> return bin content: " << binContent << endm;
        }
        return binContent;
    }
    else {
        if( mDebug ) {
            LOG_WARN << "calibTimeOffset: required histogram with key " << key << " doesn't exist -> return 0" << endm;
        }
        return 0.;
    }
}


//_____________________________________________________________
/*!
 * get time offsets from calibration histograms to digis
 */
double
StETofCalibMaker::slewingTimeOffset( StETofDigi* aDigi )
{
    unsigned int key = aDigi->sector() * 100000 + aDigi->zPlane() * 10000  + aDigi->counter() * 1000 + aDigi->strip() * 10 + aDigi->side();

    if( mDigiSlewCorr.count( key ) ) {

        unsigned int totBin = mDigiSlewCorr.at( key )->FindBin( aDigi->rawTot() ); //adjusted. PW
mDebug = true;
        if( mDigiSlewCorr.at( key )->GetBinEntries( totBin ) <= mMinDigisPerSlewBin && totBin < etofSlewing::nTotBins ) {
            if( mDebug ) {
                LOG_DEBUG << "slewingTimeOffset: insufficient statistics for slewing calibration in channel " << key << " at tot bin " << totBin << "  --> return 0" << endm;
            }
            return 0.;
        }

        float val = mDigiSlewCorr.at( key )->Interpolate( aDigi->rawTot() ); //adjusted. PW
        if( mDebug ) {
            LOG_DEBUG << "slewingTimeOffset: histogram with key " << key << "  with calib TOT of " << aDigi->calibTot() << " --> interpolated correction: " << val << endm;
        }
        return val;
    }
    else {
        if( mDebug ) {
            LOG_DEBUG << "slewingTimeOffset: required histogram with key " << key << " doesn't exist -> return 0" << endm;
        }
        return 0.;
    }
}


//_____________________________________________________________
/*!
 * apply pulser time difference offsets to the digis
 */
double
StETofCalibMaker::applyPulserOffset( StETofDigi* aDigi )
{
    int key = aDigi->sector() * 1000 + aDigi->zPlane() * 100 + aDigi->counter() * 10 + aDigi->side();

    if( !mPulserTimeDiff.count( key ) ) {
        return 0.;
    }

    return mPulserTimeDiff.at( key );
}


//_____________________________________________________________
/*!
 * get trigger time from StETofHeader
 */
double
StETofCalibMaker::triggerTime( StETofHeader* header )
{
    // initialize trigger time with the one from the header in case the map of trigger time stamps per AFCK is empty
    double triggerTime = header->trgGdpbFullTime();

    // count the occurance of a given trigger time stamp in the GdbpTs map of the eTOF header
    std::map< uint64_t, short > countsGdpbTs;
    for( const auto& kv : header->rocGdpbTs() ) {
        if( mDebug ) {
            LOG_DEBUG << "triggerTime (" << std::hex << "Ox" << kv.first << std::dec << ")  " << kv.second * eTofConst::coarseClockCycle * 1.e-9 << endm;
        }
        ++countsGdpbTs[ kv.second ];
    }

    // combine adjacent trigger times to get the number of right trigger time stamps without outliers
    // take the trigger Ts that occured most often in the combined counting map
    short    maxCount = 0;
    short    accCount = 0;
    uint64_t mostProbableTriggerTs = 0;

    for( auto it = countsGdpbTs.begin(); it != countsGdpbTs.end(); it++ ) {
        auto next = std::next( it, 1 );
        auto prev = std::prev( it, 1 );

        short countTs = it->second;

        if( next != countsGdpbTs.end() && ( next->first - it->first ) == 1 ) {
            countTs += next->second;
        }
        if( accCount > 0 && ( it->first - prev->first ) == 1 ) {
            countTs += prev->second;
        }

        if( countTs >= accCount ) {
            accCount = countTs;

            if( it->second > maxCount ) {
                maxCount = it->second;
                mostProbableTriggerTs = it->first;
            }
        }
    }

    if( mostProbableTriggerTs > 0 ) {
        triggerTime = mostProbableTriggerTs * eTofConst::coarseClockCycle;
    }

    if( mDebug ) {
        LOG_DEBUG << "trigger TS: " << mostProbableTriggerTs << " -->  trigger time (ns): " << triggerTime << endm;
    }

    return triggerTime;
}


//_____________________________________________________________
/*!
 * get reset time from StETofHeader
 */
double
StETofCalibMaker::resetTime( StETofHeader* header )
{
    // count the occurance of a given reset time stamp in the StarTs map of the eTOF header
    std::map< uint64_t, short > countsStarTsRaw;
    for( const auto& kv : header->rocStarTs() ) {
        if( mDebug ) {
            LOG_DEBUG << "resetTime (" << std::hex << "Ox" << kv.first << std::dec << ")  " << kv.second * eTofConst::coarseClockCycle * 1.e-9 << endm;
        }

        // in Run18 only one of the AFCKs was giving the correct reset time: 0x18e6
        if( mRunYear == 2018 && kv.first != 0x18e6 ) continue;

        if( kv.second != 0 ) {
            ++countsStarTsRaw[ kv.second ];
        }
    }


    // combine adjacent reset time values with the earlier one
    std::map< uint64_t, short > countsStarTs;
    for( auto it = countsStarTsRaw.begin(); it != countsStarTsRaw.end(); it++ ) {
        auto next = std::next( it, 1 );

        short countTs = it->second;

        if( next != countsStarTsRaw.end() && ( next->first - it->first ) == 1 ) {
            countTs += next->second;
        }

        countsStarTs[ it->first ] = countTs;
    }




    if( mDoQA ) {
        if( countsStarTs.size() == 0 ) {
            LOG_INFO << "all AFCKs report a reset time value 0" << endm;
        }

        for( const auto& kv : countsStarTs ) {
            LOG_DEBUG << "resetTime cand:" << kv.first  << " (" << kv.second << " times)" << endm;
            mHistograms.at( "resetTimeCand_times" )->Fill( kv.second );
        }

        for( const auto& kv : header->rocStarTs() ) {
            unsigned int sector;
            mHwMap->mapToSector( kv.first, sector );

            LOG_DEBUG << "resetTime(" << sector << "): " << kv.second << endm;

            std::string histName = "resetTimeDifferenceToSector" + to_string( sector );
            for( const auto& jv : header->rocStarTs() ) {
                unsigned int sec;
                mHwMap->mapToSector( jv.first, sec );

                mHistograms.at( histName )->Fill( sec, ( int64_t ) jv.second - ( int64_t ) kv.second );
            }
        }
    }


    while( countsStarTs.size() > 0 ) {
        auto it = std::max_element( countsStarTs.begin(), countsStarTs.end(),
                                    []( const pair< uint64_t, short >& p1, const pair< uint64_t, short >& p2 ) {
                                    return p1.second < p2.second; } );

        double resetTime = it->first * eTofConst::coarseClockCycle;


        // only update reset time if it is at least two clock ticks away from the old reset time to avoid jitter 
        if( abs( mResetTs - ( int64_t ) it->first ) < 2 ) {
            resetTime = mResetTs * eTofConst::coarseClockCycle;
        }
        else {
            LOG_INFO << "change in reset TS: " << mResetTs << " --> " << it->first << endm;
            mResetTs = it->first;
        }


        // Run19: trigger - reset time should be on the order of a few second up to 120 minutes (7.2*10^12 ns), i.e. max. run length
        // Run20: difference can be negative due to eTOF DAQ restarts at the beginning of runs while eTOF is put to "BUSY" in run control
        if( mTriggerTime - resetTime < 7.2e12 ) {
            if( mDebug ) {
                LOG_DEBUG << "reset time (ns): " << resetTime << " --> difference to trigger time in seconds: " << ( mTriggerTime - resetTime ) * 1.e-9 << endm;
            }
            LOG_DEBUG << "--> picked reset TS:" << mResetTs << endm;

            if( mDoQA ) {
                mHistograms.at( "resetTimeCand_picked" )->Fill( it->second );

                auto rawIt = std::max_element( countsStarTsRaw.begin(), countsStarTsRaw.end(),
                                               []( const pair< uint64_t, short >& p1, const pair< uint64_t, short >& p2 ) {
                                               return p1.second < p2.second; } );

                mHistograms.at( "resetTimeCand_compare" )->Fill( ( int64_t ) mResetTs - ( int64_t ) rawIt->first );
                mHistograms.at( "resetTimeCand_value"   )->Fill( mResetTs % ( int ) eTofConst::bTofClockCycle    );
            }

            return resetTime;
        }
        else {
            countsStarTs.erase( it );
        }
    }

    return 0.;
}


//_____________________________________________________________
unsigned int
StETofCalibMaker::channelToKey( const unsigned int channelId ) {
    unsigned int sector   = (   channelId                                    / eTofConst::nChannelsPerSector  ) + eTofConst::sectorStart;
    unsigned int zPlane   = ( ( channelId % eTofConst::nChannelsPerSector  ) / eTofConst::nChannelsPerModule  ) + eTofConst::zPlaneStart;
    unsigned int counter  = ( ( channelId % eTofConst::nChannelsPerModule  ) / eTofConst::nChannelsPerCounter ) + eTofConst::counterStart;
    unsigned int strip    = ( ( channelId % eTofConst::nChannelsPerCounter ) / eTofConst::nSides              ) + eTofConst::stripStart;
    unsigned int side     =   ( channelId % eTofConst::nSides )                                                 + eTofConst::sideStart;

    return sector * 100000 + zPlane * 10000 + counter * 1000 + strip * 10 + side;
}


//_____________________________________________________________
unsigned int
StETofCalibMaker::detectorToKey( const unsigned int detectorId ) {
    unsigned int sector   = (   detectorId / eTofConst::nCountersPerSector  )                           + eTofConst::sectorStart;
    unsigned int zPlane   = ( ( detectorId % eTofConst::nCountersPerSector  ) / eTofConst::nCounters  ) + eTofConst::zPlaneStart;
    unsigned int counter  = (   detectorId % eTofConst::nCounters                                     ) + eTofConst::counterStart;

    return sector * 100 + zPlane * 10 + counter;
}


//_____________________________________________________________
unsigned int
StETofCalibMaker::sideToKey( const unsigned int sideId ) {
    unsigned int sector   = (   sideId / ( eTofConst::nCountersPerSector * eTofConst::nSides ) )    + eTofConst::sectorStart;
    unsigned int zPlane   = ( ( sideId % ( eTofConst::nCountersPerSector * eTofConst::nSides ) ) / (  eTofConst::nCounters * eTofConst::nSides ) ) + eTofConst::zPlaneStart;
    unsigned int counter  = ( ( sideId % ( eTofConst::nCounters          * eTofConst::nSides ) ) /    eTofConst::nSides  ) + eTofConst::counterStart;
    unsigned int side     = (   sideId % eTofConst::nSides                                     )    + eTofConst::sideStart;

    return sector * 1000 + zPlane * 100 + counter * 10 + side;
}



//_____________________________________________________________
// setHistFileName uses the string argument from the chain being run to set
// the name of the output histogram file.
void
StETofCalibMaker::setHistFileName()
{
    string extension = ".etofCalib.root";

    if( GetChainOpt()->GetFileOut() != nullptr ) {
        TString outFile = GetChainOpt()->GetFileOut();

        mHistFileName = ( string ) outFile;

        // get rid of .root
        size_t lastindex = mHistFileName.find_last_of( "." );
        mHistFileName = mHistFileName.substr( 0, lastindex );

        // get rid of .MuDst or .event if necessary
        lastindex = mHistFileName.find_last_of( "." );
        mHistFileName = mHistFileName.substr( 0, lastindex );

        // get rid of directories
        lastindex = mHistFileName.find_last_of( "/" );
        mHistFileName = mHistFileName.substr( lastindex + 1, mHistFileName.length() );

        mHistFileName = mHistFileName + extension;
    } else {
        LOG_ERROR << "Cannot set the output filename for histograms" << endm;
    }
}

//_____________________________________________________________
void
StETofCalibMaker::bookHistograms()
{
    LOG_INFO << "bookHistograms() ... " << endm;

    mHistograms[ "pulserDigiTimeDiff_GbtxCorrProf"    ] = new TProfile( "pulserDigiTimeDiff_GbtxCorrProf",    "time difference of pulsers to reference;pulser channel;#Delta T (ns)", 216, 0, 216, "s" );
    mHistograms[ "pulserDigiTimeDiff_GbtxCorrProfMod" ] = new TProfile( "pulserDigiTimeDiff_GbtxCorrProfMod", "time difference of pulsers to reference;pulser channel;#Delta T (ns)", 216, 0, 216, "s" );
    mHistograms[ "pulserDigiTimeDiff_fullCorr"  ] = new TH2F( "pulserDigiTimeDiff_fullCorr",  "time difference of pulsers to reference;pulser channel;#Delta T (ns)", 216, 0, 216, 360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );
    mHistograms[ "pulserDigiTimeDiff_RefCorr"  ] = new TH1F("pulserDigiTimeDiff_RefCorr",  "time difference of pulsers to reference; #Delta T (ns)", 45, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ));

    if( mDoQA ) {
        mHistograms[ "pulserDigiTimeDiff"           ] = new TH2F( "pulserDigiTimeDiff",           "time difference of pulsers to reference;pulser channel;#Delta T (ns)", 216, 0, 216, 360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );
        mHistograms[ "pulserDigiTimeToCounter1"     ] = new TH2F( "pulserDigiTimeToCounter1",     "time difference of pulsers to counter 1;pulser channel;#Delta T (ns)", 216, 0, 216, 2*360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );
        mHistograms[ "pulserDigiTimeToCounter2"     ] = new TH2F( "pulserDigiTimeToCounter2",     "time difference of pulsers to counter 2;pulser channel;#Delta T (ns)", 216, 0, 216, 2*360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );
        mHistograms[ "pulserDigiTimeToCounter3"     ] = new TH2F( "pulserDigiTimeToCounter3",     "time difference of pulsers to counter 3;pulser channel;#Delta T (ns)", 216, 0, 216, 2*360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );

        mHistograms[ "pulserDigiTimeDiff_GbtxCorr"  ] = new TH2F( "pulserDigiTimeDiff_GbtxCorr",  "time difference of pulsers to reference;pulser channel;#Delta T (ns)", 216, 0, 216, 360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );
        mHistograms[ "pulserDigiTimeDiff_perGbtx"   ] = new TH2F( "pulserDigiTimeDiff_perGbtx",   "average time difference of pulsers in one Gbtx to reference;pulser channel;#Delta T (ns)", 216/3, 0, 216, 360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );
        mHistograms[ "pulserDigiTimeDiff_toAverage" ] = new TH2F( "pulserDigiTimeDiff_toAverage", "time difference of pulsers to reference;pulser channel;#Delta T (ns)", 216, 0, 216, 4*360, -359.5 * ( 6.25 / 112 ), 360.5 * ( 6.25 / 112 ) );

        mHistograms[ "1_off" ] = new TH2F( "1_off", "average time difference of pulsers in one Gbtx to reference;pulser channel;#Delta T (ns)", 216/3, 0, 216, 360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );
        mHistograms[ "2_off" ] = new TH2F( "2_off", "average time difference of pulsers in one Gbtx to reference;pulser channel;#Delta T (ns)", 216/3, 0, 216, 360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );
        mHistograms[ "3_off" ] = new TH2F( "3_off", "average time difference of pulsers in one Gbtx to reference;pulser channel;#Delta T (ns)", 216/3, 0, 216, 360, -179.5 * ( 6.25 / 112 ), 180.5 * ( 6.25 / 112 ) );

		  mHistograms[ "pulserDigiTimeDiff_CorrAgreement"  ] = new TH1F("pulserDigiTimeDiff_CorrAgreement",  "Number of pulsers agreeing on a common shift between events; #pulsers", 218, -0.5, 217.5);
		  mHistograms[ "pulserDigiTimeDiff_CorrCommonJump"  ] = new TH1F("pulserDigiTimeDiff_CorrCommonJump",  "Number of pulsers jumping at the same time between events; #pulsers", 218, -0.5, 217.5);
        mHistograms[ "pulserDigiPresence" ] = new TH1F( "pulserDigiPresence",           "pulser presence (number of events at ( -1 );pulser channel", 218, -1.5, 216.5);

        for( int i=0; i<12; i++ ) {
            std::string histName = "resetTimeDifferenceToSector" + to_string( i + 13 );
            mHistograms[ histName ] = new TH2F( Form( "resetTimeDifferenceToSector%d", i + 13 ), Form("reset time difference to sector %d;sector;#DeltaT (clock ticks)", i + 13 ), 12, 12.5, 24.4, 5, -2.5, 2.5 );
        }
		  mHistograms[ "ETOF_QA_daqMissmatches_get4" ]    = new TProfile( "ETOF_QA_daqMissmatches_get4", "missmatch percentage for each get4; get4 []; missmatch percentage", 1728, 0.5, 1728.5 );
		  mHistograms[ "ETOF_QA_daqMissmatches_counter" ] = new TProfile( "ETOF_QA_daqMissmatches_counter", "missmatch percentage for each counter; counter []; missmatch percentage", 108, 0.5, 108.5 );   
        mHistograms[ "resetTimeCand_times"   ] = new TH1F( "resetTimeCand_times",   "sectors agreeing on reset time candidates;# sectors with common candidate;# events", 12, 0.5, 12.5 );
        mHistograms[ "resetTimeCand_picked"  ] = new TH1F( "resetTimeCand_picked",  "sectors agreeing on picked reset time;# sectors with picked reset time;# events",    12, 0.5, 12.5 );
        mHistograms[ "resetTimeCand_compare" ] = new TH1F( "resetTimeCand_compare", "difference between old and new way;#DeltaT (clock ticks);# events",                   5, -2.5, 2.5 );
        mHistograms[ "resetTimeCand_value"   ] = new TH1F( "resetTimeCand_value",   "picked reset time value;clock ticks;# events", ( int ) eTofConst::bTofClockCycle, 0.5, 0.5 + eTofConst::bTofClockCycle );
    }

    for ( auto& kv : mHistograms ) {
        kv.second->SetDirectory( 0 );
    }
}

//_____________________________________________________________
void
StETofCalibMaker::writeHistograms()
{
    if( mHistFileName != "" ) {
        LOG_INFO << "writing histograms to: " << mHistFileName.c_str() << endm;

        TFile histFile( mHistFileName.c_str(), "RECREATE", "etofCalib" );
        histFile.cd();

        for( int i=0; i<12; i++ ) {
            std::string histName = "resetTimeDifferenceToSector" + to_string( i + 13 );
            mHistograms.at( histName )->Scale( 12. / mHistograms.at( histName )->GetEntries() );
        }
        
        for ( const auto& kv : mHistograms ) {
            if( kv.second->GetEntries() > 0 ) kv.second->Write();
        }

        histFile.Close();
    }
    else {
        LOG_INFO << "histogram file name is empty string --> cannot write histograms" << endm;
    }
}
