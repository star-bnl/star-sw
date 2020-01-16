/***************************************************************************
 *
 * $Id: StETofHitMaker.cxx,v 1.7 2020/01/16 03:40:23 fseck Exp $
 *
 * Author: Philipp Weidenkaff & Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofHitMaker - class to read the eTofCollection from
 * StEvent and combine digis on both sides of each read-out strip into a hit.
 * The hits on each strip are further merger into clusters.
 * The eTOF collection is filled with hits and written to StEvent.
 *
 ***************************************************************************
 *
 * $Log: StETofHitMaker.cxx,v $
 * Revision 1.7  2020/01/16 03:40:23  fseck
 * add possibility to calculate VPD start time + updated deadtime handling for negative hit times
 *
 * Revision 1.6  2019/12/17 03:27:51  fseck
 * update to histograms for .hist.root files
 *
 * Revision 1.5  2019/12/12 02:27:09  fseck
 * enable clock jump correction by default
 *
 * Revision 1.4  2019/12/10 15:58:33  fseck
 * ignore digis in dead time software-wise + possibility to correct clock jumps based on hit position via setting a flag
 *
 * Revision 1.3  2019/03/25 01:07:40  fseck
 * added more correlation & average hit time histograms for offline QA
 *
 * Revision 1.2  2019/03/08 19:07:20  fseck
 * introduced dead time handling + fixed clustering to only pick up hits on adjacent strips + moved QA histograms for clustered hits into separate function + added correlation plots to bTOF hits
 *
 * Revision 1.1  2019/02/19 19:52:28  jeromel
 * Reviewed code provided by F.Seck
 *
 *
 ***************************************************************************/
#include <algorithm>
#include <vector>
#include <cmath>

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"

#include "StEvent.h"
#include "StETofCollection.h"
#include "StETofDigi.h"
#include "StETofHit.h"

#include "StBTofCollection.h"
#include "StBTofHeader.h"
#include "StBTofHit.h"

#include "StEpdCollection.h"
#include "StEpdHit.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuETofDigi.h"
#include "StMuDSTMaker/COMMON/StMuETofHit.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuEpdHit.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StChain/StChainOpt.h" // for renaming the histogram file

#include "StETofHitMaker.h"
#include "StETofUtil/StETofConstants.h"

#include "tables/St_etofHitParam_Table.h"
#include "tables/St_etofSignalVelocity_Table.h"


//_____________________________________________________________
StETofHitMaker::StETofHitMaker( const char* name )
: StMaker( "etofHit", name ),
  mEvent( nullptr ),          /// pointer to StEvent
  mMuDst( nullptr ),          /// pointer to MuDst
  mFileNameHitParam( "" ),
  mFileNameSignalVelocity( "" ),
  mMaxYPos( 15. ),
  mMergingRadius( 1. ),
  mSoftwareDeadTime( 50. ),
  mDoClockJumpShift( true ),
  mDoQA( false ),
  mDebug( false ),
  mHistFileName( "" )
{
    LOG_DEBUG << "StETofHitMaker::ctor"  << endm;

    mStoreDigi.clear();
    mStoreHit.clear();
    mMapDigiIndex.clear();
    mMapHitDigiIndices.clear();
    mMapHitIndexDigiIndices.clear();

    mSigVel.clear();

    mHistograms.clear();
    mCounterActive.clear();
}

//_____________________________________________________________
StETofHitMaker::~StETofHitMaker()
{
    /* no op */ 
}

//_____________________________________________________________
Int_t
StETofHitMaker::Init()
{
    LOG_INFO << "StETofHitMaker::Init()" << endm;

    bookHistograms();

    return kStOk;
}

//_____________________________________________________________
Int_t
StETofHitMaker::InitRun( Int_t runnumber )
{
    LOG_INFO << "StETofHitMaker::InitRun()" << endm;

    TDataSet* dbDataSet = nullptr;
    std::ifstream paramFile;

    // --------------------------------------------------------------------------------------------
    // initialize hit building parameters from parameter file (if filename is provided) or database:
    // -- hit param
    // -- signal velocity
    // --------------------------------------------------------------------------------------------

    // hit param
    if( mFileNameHitParam.empty() ) {
        LOG_INFO << "etofHitParam: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofHitParam" );

        St_etofHitParam* etofHitParam = static_cast< St_etofHitParam* > ( dbDataSet->Find( "etofHitParam" ) );
        if( !etofHitParam ) {
            LOG_ERROR << "unable to get the hit params from the database" << endm;
            return kStFatal;
        }

        etofHitParam_st* hitParamTable = etofHitParam->GetTable();
        
        mMaxYPos       = hitParamTable->maxLocalY;
        mMergingRadius = hitParamTable->clusterMergeRadius;
    }
    else {
        LOG_INFO << "etofHitParam: filename provided --> use parameter file: " << mFileNameHitParam.c_str() << endm;
        
        paramFile.open( mFileNameHitParam.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etofHitParam' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        float temp1 = 0;
        float temp2 = 0;
        if( paramFile.good() ) {
            paramFile >> temp1 >> temp2;
        }

        paramFile.close();

        if( temp1 > 0. ) {
            mMaxYPos = temp1;
        }
        if( temp2 > 0. ) {
            mMergingRadius = temp2;
        }
    }

    LOG_INFO << " maximum local Y: " << mMaxYPos << " , cluster merging radius: " << mMergingRadius << endm;

    // --------------------------------------------------------------------------------------------

    // signal velocities
    mSigVel.clear();

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
                mSigVel[ detectorToKey( i ) ] = velocityTable->signalVelocity[ i ];
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
                mSigVel[ detectorToKey( i ) ] = velocity.at( i );
            }
        }
    }

    for( const auto& kv : mSigVel ) {
        LOG_DEBUG << "counter key: " << kv.first << " --> signal velocity = " << kv.second << " cm / ns" << endm;
    }

    // --------------------------------------------------------------------------------------------
    for( int i=0; i<108; i++ ) {
        mCounterActive.push_back( false );
    }

    return kStOk;
}

//_____________________________________________________________
Int_t
StETofHitMaker::FinishRun( Int_t runnumber )
{
    if( mDoQA ) {
        for( int iCounter = 0; iCounter < eTofConst::nCountersInSystem; iCounter++ ) {
            if( mCounterActive.at( iCounter ) ) {
                int day = ( runnumber % 1000000 ) / 1000;
                int run = runnumber % 1000;

                mHistograms.at( "counter_active" )->Fill( 100 * day + run, iCounter );
            }
        }
    }

    LOG_INFO << "StETofHitMaker::FinishRun()" << endm;

    return kStOk;
}

//_____________________________________________________________
Int_t
StETofHitMaker::Finish()
{
    LOG_INFO << "StETofHitMaker::Finish()" << endm;
    
    if( mDoQA ) {
        LOG_INFO << "Finish() - writing *.etofHit.root ..." << endm;
        setHistFileName();
        writeHistograms();
    }

    return kStOk;
}

//_____________________________________________________________
Int_t
StETofHitMaker::Make()
{
    LOG_DEBUG << "StETofHitMaker::Make(): starting ..." << endm;

    mEvent = ( StEvent* ) GetInputDS( "StEvent" );

    if ( mEvent ) {
        LOG_DEBUG << "Make() - running on StEvent" << endm;

        processStEvent();

        return kStOk;
    }
    else {
        LOG_DEBUG << "Make(): no StEvent found" << endm;

        mMuDst = ( StMuDst* ) GetInputDS( "MuDst" );

        if( mMuDst ) {
            LOG_DEBUG << "Make() - running on MuDsts" << endm;

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
StETofHitMaker::processStEvent()
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

    const StSPtrVecETofDigi& etofDigis  = etofCollection->etofDigis();

    //---------------------------------

    size_t nDigis = etofDigis.size();
    if( mDebug ) {
        LOG_DEBUG << "processStEvent() - # fired eTOF digis : " << nDigis << endm;
    }

    bool isMuDst = false;

    // clear existing hits from eTofCollection (important for afterburner mode )
    clearHits( isMuDst );

    // clear all storage containers
    //(to be sure that there are no digis left from the previous event)
    clearStorage();

    //sort digis into the storage (one vector of digis for each strip)
    size_t nDigisInStore = 0;

    for( size_t i = 0; i<nDigis; i++ ) {
        StETofDigi* aDigi = etofDigis[ i ];

        if( fillStorage( aDigi, i ) ) {
            nDigisInStore++;
        }
    }
    LOG_INFO << "processStEvent() - storage is filled with " << nDigisInStore << " digis" << endm;

    matchSides();

    double tstart = startTime();  
    
    if( mDoQA ) {
        fillUnclusteredHitQA( tstart, isMuDst );
    }

    mergeClusters( isMuDst );

    assignAssociatedHits( isMuDst );


    if( mDoQA ) {
        mHistograms.at( "multiplicity_etofDigis_etofHits" )->Fill( nDigisInStore, etofCollection->etofHits().size() );
    }

    if( etofCollection->hitsPresent() ) {
        StSPtrVecETofHit& etofHits = etofCollection->etofHits();
        LOG_INFO << "processStEvent() - etof hit collection: " << etofHits.size() << " entries" << endm;

        fillHitQA( isMuDst, tstart );
    }
    else {
        LOG_INFO << "processStEvent() - no hits" << endm;
    }
}

//_____________________________________________________________
void
StETofHitMaker::processMuDst()
{
    if( !mMuDst->etofArray( muETofDigi ) ) {
        LOG_WARN << "processMuDst() - no digi array" << endm;
        return;
    }

    if( !mMuDst->numberOfETofDigi() ) {
        LOG_WARN << "processMuDst() - no digis present" << endm;
        return;
    }

    //---------------------------------

    size_t nDigis = mMuDst->numberOfETofDigi();
    if( mDebug ) {
        LOG_DEBUG << "processMuDst() - # fired eTOF digis : " << nDigis << endm;
    }
    bool isMuDst = true;

    // clear existing hits from eTofCollection (important for afterburner mode )
    clearHits( isMuDst );

    // clear all storage containers
    //(to be sure that there are no digis left from the previous event)
    clearStorage();

    //sort digis into the storage (one vector of digis for each strip)
    size_t nDigisInStore = 0;

    for( size_t i = 0; i<nDigis; i++ ) {
        StMuETofDigi* aDigi = mMuDst->etofDigi( i );

        if( fillStorage( aDigi, i ) ) {
            nDigisInStore++;
        }
    }
    LOG_INFO << "processMuDst() - storage is filled with " << nDigisInStore << " digis" << endm;

    matchSides();

    double tstart = startTime();

    if( mDoQA ) {
        fillUnclusteredHitQA( tstart, isMuDst );
    }

    mergeClusters( isMuDst );

    assignAssociatedHits( isMuDst );


    if( mDoQA ) {
        mHistograms.at( "multiplicity_etofDigis_etofHits" )->Fill( nDigisInStore, mMuDst->numberOfETofHit() );
    }


    if( mMuDst->numberOfETofHit() ) {
        size_t nHits = mMuDst->numberOfETofHit();
        LOG_INFO << "processMuDst() - etof hits: " << nHits << " entries" << endm;

        fillHitQA( isMuDst, tstart );
    }
    else {
        LOG_INFO << "processMuDst() - no hits" << endm;
    }
}
//_____________________________________________________________


//_____________________________________________________________
// get the start time -- from bTOF header from bTOF header for now
double
StETofHitMaker::startTime()
{
    if( mDebug ) {
        LOG_INFO << "startTime(): -- loading start time from bTOF header" << endm;
    }

    StBTofHeader* btofHeader = nullptr; 

    if( mEvent ) {
        StBTofCollection* btofCollection = ( StBTofCollection* ) mEvent->btofCollection();

        if ( btofCollection ) {
            btofHeader = btofCollection->tofHeader();
        }
        else {
            LOG_DEBUG << "no StBTofCollection found by getTstart" << endm;
            return -9999.;
        }
    }
    else if( mMuDst ) {
        btofHeader = mMuDst->btofHeader();
    }

    if( !btofHeader ) {
        LOG_DEBUG << "startTime(): -- no bTOF header --> no start time avaiable" << endm;
        return -9999.;
    }

    double tstart = btofHeader->tStart();

    if( !isfinite( tstart ) ) {
        LOG_DEBUG << "startTime(): -- from bTOF header is NaN" << endm;
        return -9999.;
    }

    if( tstart != -9999. ) {
        tstart = fmod( tstart, eTofConst::bTofClockCycle );
        if( tstart < 0. ) tstart += eTofConst::bTofClockCycle;
    }

    if( mDebug ) {
        LOG_INFO << "startTime():  --  start time: " << tstart << endm;
    }

    return tstart;
}

//_____________________________________________________________
// get the start time (and vertex) -- from VPD
void
StETofHitMaker::startTimeVpd( double& tstart, double& vertexVz )
{
    tstart   = -9999.;
    vertexVz = -999.;

    if( mDebug ) {
        LOG_INFO << "startTimeVpd(): -- calculating VPD start time from bTOF header" << endm;
    }

    StBTofHeader* btofHeader = nullptr;

    if( mEvent ) {
        StBTofCollection* btofCollection = ( StBTofCollection* ) mEvent->btofCollection();

        if ( btofCollection ) {
            btofHeader = btofCollection->tofHeader();
        }
        else {
            LOG_DEBUG << "no StBTofCollection found by getTstart" << endm;
            return;
        }
    }
    else if( mMuDst ) {
        btofHeader = mMuDst->btofHeader();
    }

    if( !btofHeader ) {
        LOG_DEBUG << "startTimeVpd(): -- no bTOF header --> no start time avaiable" << endm;
        return;
    }

    const int nVpd = 19; // number of VPD tubes on each side of STAR

    int nWest = btofHeader->numberOfVpdHits( west );
    int nEast = btofHeader->numberOfVpdHits( east );

    double vpdLeTime[ 2 * nVpd ];


    // check if bTof header is filled with useful information
    if( fabs( btofHeader->vpdVz() ) > 200. ) {
        if( mDoQA ) {
            LOG_INFO << "startTimeVpd(): no valid Vpd data in the bTOF header " << endm;
        }
        return;
    }
    else {
        vertexVz = btofHeader->vpdVz();
        LOG_DEBUG << "startTimeVpd(): Vpd vertex is at: " << vertexVz << endm;
    }

    double tMean   = 0.;
    int nTubes     = 0;
    int nTubesWest = 0;
    int nTubesEast = 0;

    // west side
    for( int i=0; i< nVpd; i++ ) {
        vpdLeTime[ i ] = btofHeader->vpdTime( west, i+1 );
        if( vpdLeTime[ i ] > 0. ) {
            updateCyclicRunningMean( vpdLeTime[ i ], tMean, nTubes, eTofConst::bTofClockCycle );
            nTubesWest++;
            LOG_DEBUG << "startTimeVpd(): loading VPD west tubeId = " << i+1 << " time " << vpdLeTime[ i ] << endm;
        }
    }

    // east side
    for( int i=0; i< nVpd; i++ ) {
        vpdLeTime[ i + nVpd ] = btofHeader->vpdTime( east, i+1 );
        if( vpdLeTime[ i + nVpd ] > 0. ) {
            updateCyclicRunningMean( vpdLeTime[ i + nVpd ], tMean, nTubes, eTofConst::bTofClockCycle );
            nTubesEast++;
            LOG_DEBUG << "startTimeVpd(): loading VPD east tubeId = " << i+1 << " time " << vpdLeTime[ i + nVpd ] << endm;
        }
    }

    if( nTubesEast >= 2 && nTubesWest >= 2 ) {
        tstart = tMean;
    }

    if( tstart != -9999 ) {
        tstart = fmod( tstart, eTofConst::bTofClockCycle );
        if( tstart < 0. ) tstart += eTofConst::bTofClockCycle;
    }

    if( mDoQA ) {
        LOG_INFO << "startTimeVpd(): -- sum: " << tMean << " nWest: " << nWest << " nEast: " << nEast;
        LOG_INFO << " --> compare (if bTofHeader is filled): " << btofHeader->tStart() << endm;
        LOG_INFO << "startTimeVpd():  --  start time: " << tstart << endm;
    }
}


//_____________________________________________________________
bool
StETofHitMaker::fillStorage( StETofDigi* aDigi, unsigned int index )
{
    if( !aDigi ) {
        LOG_WARN << "No digi found" << endm;
        return false;
    }

    if( fabs( aDigi->calibTime() ) < 1e-5 && aDigi->calibTot() < 0 ) {
        if( mDebug ) {
            LOG_DEBUG << "fillStorage() - digi not calibrated, most likely since it is outside the trigger window or pulser. Ignore." << endm;
        }
        return false;
    }

    if( aDigi->sector() == 0 || aDigi->zPlane() == 0 || aDigi->counter() == 0 || aDigi->strip() == 0 ) {
        LOG_WARN << "fillStorage() - sector / zPlane / counter / strip  was not assigned to the digi" << endm;
        return false;
    }

    StETofDigi* pDigi = new StETofDigi( *aDigi );

    if( mDebug ) {
        LOG_DEBUG << "fillStorage() -- sector plane counter strip: " << pDigi->sector() << " ";
        LOG_DEBUG << pDigi->zPlane() << " " << pDigi->counter() << " " << pDigi->strip() << endm;
        LOG_DEBUG << "fillStorage() -- calibTime: " << pDigi->calibTime();
        LOG_DEBUG << "  calibToT: " << pDigi->calibTot() << endm;
    }

    unsigned int key = pDigi->sector()  * 10000 +
                       pDigi->zPlane()  *  1000 +
                       pDigi->counter() *   100 +
                       pDigi->strip();

    mStoreDigi[ key ].push_back( pDigi );

    mMapDigiIndex[ pDigi ] = index;


    if( mDoQA ) {
        std::string histName_digi_tot = "digi_tot_s" + std::to_string( pDigi->sector() ) + "m" + std::to_string( pDigi->zPlane() ) + "c" + std::to_string( pDigi->counter() );

        mHistograms[ histName_digi_tot ]->Fill( pDigi->strip() - 1 + pDigi->side() * 0.3, pDigi->calibTot() );
    }

    return true;
}//::fillStorage

//_____________________________________________________________
bool
StETofHitMaker::fillStorage( StMuETofDigi* aDigi, unsigned int index )
{
    return fillStorage( ( StETofDigi* ) aDigi, index );
}//::fillStorage


//_____________________________________________________________
void
StETofHitMaker::clearHits( const bool isMuDst )
{
    if( !isMuDst ) {
        if( !( mEvent->etofCollection()->hitsPresent() ) ) {
            return;
        }

        LOG_DEBUG << "clearHits() - number of hits present (before clear): " << mEvent->etofCollection()->etofHits().size() << endm;

        // clear hits (if there are any when running in afterburner mode)
        StSPtrVecETofHit&  etofHits  = mEvent->etofCollection()->etofHits();
        etofHits.clear();

        LOG_DEBUG << "clearHits() - number of hits present (after clear): "  << mEvent->etofCollection()->etofHits().size() << endm;

        // and remove pointers to associated hit of the digis
        StSPtrVecETofDigi& etofDigis = mEvent->etofCollection()->etofDigis();
        size_t nDigis = etofDigis.size();

        for( size_t i=0; i<nDigis; i++ ) {
            StETofDigi* aDigi = etofDigis[ i ];

            if( !aDigi ) continue;

            aDigi->setAssociatedHit( nullptr );
        }
        LOG_DEBUG << "clearHits() - associated hits of digis set to nullptr" << endm;
    }
    else {
        if( mMuDst->numberOfETofHit() == 0 ) {
            return;
        }

        LOG_DEBUG << "clearHits() - number of hits present (before clear): " << mMuDst->numberOfETofHit() << endm;

        // clear hits (if there are any when running in afterburner mode)
        mMuDst->etofArray( muETofHit )->Clear( "C" );

        LOG_DEBUG << "clearHits() - number of hits present (after clear): "  << mMuDst->numberOfETofHit() << endm;

        // and remove pointers to associated hit of the digis
        size_t nDigis = mMuDst->numberOfETofDigi();

        for( size_t i=0; i<nDigis; i++ ) {
            StMuETofDigi* aDigi = mMuDst->etofDigi( i );

            if( !aDigi ) continue;

            aDigi->setAssociatedHitId( -1 );
        }
        LOG_DEBUG << "clearHits() - associated hit id of digis set to -1" << endm;
    }
}


//_____________________________________________________________
void
StETofHitMaker::clearStorage()
{
    // clear mStoreDigi  -- if any digis are left
    for( auto kv = mStoreDigi.begin(); kv != mStoreDigi.end(); kv++ ) {
        size_t remainingDigis = kv->second.size();

        if( mDebug ) {
            LOG_DEBUG << "strip key " << kv->first << " has " << remainingDigis << " left" << endm;
        }

        for( unsigned int i=0; i<remainingDigis; i++ ) {
            delete kv->second.at( i );
        }
        kv->second.clear();
    }
    mStoreDigi.clear();


    // clear mStoreHit  -- if any hits are left
    for( auto kv = mStoreHit.begin(); kv != mStoreHit.end(); kv++ ) {
        size_t remainingHits = kv->second.size();

        if( mDebug ) {
            LOG_DEBUG << "detector key " << kv->first << " has " << remainingHits << " left" << endm;
        }

        for( size_t i=0; i<remainingHits; i++ ) {
            delete kv->second.at( i );
        }
        kv->second.clear();
    }
    mStoreHit.clear();


    // clear mMapDigiIndex
    mMapDigiIndex.clear();    

    // clear mMapHitDigiIndices
    mMapHitDigiIndices.clear();

    // clear mMapHitIndexDigiIndices
    mMapHitIndexDigiIndices.clear();
}//::clearStorage

//_____________________________________________________________
void
StETofHitMaker::matchSides()
{
    std::vector< StETofDigi* >::iterator iterDigi;

    std::string histNameDigisErased;

    for( auto kv = mStoreDigi.begin(); kv != mStoreDigi.end(); kv++ ) {
        unsigned int stripIndex             = kv->first;
        std::vector< StETofDigi* > *digiVec = &( kv->second );

        // timeorder digis from both sides via lambda functions of C++11
        std::sort( digiVec->begin(), digiVec->end(), [] ( StETofDigi* lhs, StETofDigi* rhs ) {
                                                        return lhs->calibTime() < rhs->calibTime();
                                                    }
                 );

        int nDigisOnStrip = digiVec->size();
        //--------------------------------------------------------------------------------
        // print out for testing
        if( mDebug ) {
            LOG_INFO << stripIndex << "  size: " << nDigisOnStrip << endm;

            for( size_t i=0; i<digiVec->size(); i++ ) {
                LOG_INFO << "matchSides() - DIGI: " << digiVec->at( i ) << "  ";
                LOG_INFO << "calibTime=" << setprecision( 16 ) << digiVec->at( i )->calibTime() << "  " << endm;
                LOG_INFO << "calibTot="  << setprecision( 4 )  << digiVec->at( i )->calibTot()  << "  ";
                LOG_INFO << "side="      << setprecision( 4 )  << digiVec->at( i )->side()      << endm;
            }
        }
        //--------------------------------------------------------------------------------

        int detIndex = stripIndex / 100;
        int sector   =   detIndex / 100;
        int plane    = ( detIndex % 100 ) / 10;
        int counter  =   detIndex % 10;
        int strip    = stripIndex % 100;

        if( mDoQA ) {
            std::string histNameDigisPerStrip = "digisPerStrip_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
            mHistograms.at( histNameDigisPerStrip )->Fill( strip, nDigisOnStrip );

            histNameDigisErased = "digisErased_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
        }


        //--------------------------------------------------------------------------------
        // remove digis from the vector that fall within the dead time caused by another digi on the same side of a strip
        std::vector< double > deadTime( 2, -60. );

        for( auto it = digiVec->begin(); it != digiVec->end(); it++ ) {
            if( fabs( (*it)->calibTime() - deadTime.at( (*it)->side() - 1 ) ) < mSoftwareDeadTime ) {

                if( mDebug ) {
                    LOG_INFO << "digi within dead time --> ignore ... ( geomId : " << stripIndex * 10 + (*it)->side();
                    LOG_INFO << " dead time: "  << setprecision( 16 ) << deadTime.at( (*it)->side() - 1 );
                    LOG_INFO << " calib time: " << setprecision( 16 ) << (*it)->calibTime();
                    LOG_INFO << " difference: " << (*it)->calibTime() - deadTime.at( (*it)->side() - 1 ) << endm;
                }

                delete *it;
                digiVec->erase( it );
                it--;
                if( mDoQA ) {
                    mHistograms.at( histNameDigisErased )->Fill( 1 );
                }
            }
            else {
                deadTime.at( (*it)->side() - 1 ) = (*it)->calibTime();
            }
        }
        //--------------------------------------------------------------------------------


        double posX     = 0.0;
        double posY     = 0.0;
        double time     = 0.0;
        double timeDiff = 0.0;
        double totSum   = 0.0;

        if( mDoQA && digiVec->size() == 1 ) {
            mHistograms.at( histNameDigisErased )->Fill( 2 );
        }

        // loop over digis on the same strip
        while( digiVec->size() > 1 ) {
            if( mDebug ) { 
                LOG_DEBUG << stripIndex << " -- digiVec->size() -- " << digiVec->size() << endm;
            }
            // treat consecutive digis on the same side:
            // we want to have the first and second digi to be on different sides
            // of the strip in order to build hits out of them
            while( digiVec->at( 0 )->side() == digiVec->at( 1 )->side() ) {

                if( digiVec->size() > 2 ) { //more than 2 digis left on strip

                    // test for three (or more) consecutive digis on the same side
                    if( digiVec->at( 2 )->side() == digiVec->at( 0 )->side() ) {

                        // delete first digi
                        iterDigi = digiVec->begin();
                        delete *iterDigi;
                        digiVec->erase( iterDigi );
                        if( mDoQA ) {
                            mHistograms.at( histNameDigisErased )->Fill( 3 );
                        }
                    }
                    else { // --> third digi is on the other side compared to first and second digi
                        if( digiVec->at( 2 )->calibTime() - digiVec->at( 0 )->calibTime() >
                            digiVec->at( 2 )->calibTime() - digiVec->at( 1 )->calibTime() ) {

                            // third digi is not same side and fits better with second digi
                            // --> delete first digi
                            iterDigi = digiVec->begin();
                            delete *iterDigi;
                            digiVec->erase( iterDigi );
                            if( mDoQA ) {
                                mHistograms.at( histNameDigisErased )->Fill( 4 );
                            }
                        }
                        else {
                            // third digi is not same side and fits better with first digi
                            // --> delete second digi
                            iterDigi = digiVec->begin() + 1;
                            delete *iterDigi;
                            digiVec->erase( iterDigi );
                            if( mDoQA ) {
                                mHistograms.at( histNameDigisErased )->Fill( 4 );
                            }
                        }
                    }
                }
                else{ // --> 2 or less digis left on the strip (on the same side of the strip)
                      // delete the remaining digi
                    iterDigi = digiVec->begin();
                    delete *iterDigi;
                    digiVec->erase( iterDigi );
                    if( mDoQA && digiVec->size() == 1 ){
                        mHistograms.at( histNameDigisErased )->Fill( 5 );
                    }
                }

                if( digiVec->size() < 2 ) { //only one digi left on strip. break loop.
                    if(mDoQA &&  digiVec->size() == 1) {
                        mHistograms.at( histNameDigisErased )->Fill( 5 );
                    }
                    break;
                }
            } // first and second digi in the vector are on different sides

            if( mDebug ) {
                LOG_DEBUG << "matchSides() - digi processing for sector " << stripIndex / 10000;
                LOG_DEBUG << " plane " << ( stripIndex % 10000 ) / 1000  << " counter " << ( stripIndex % 1000 ) / 100;
                LOG_DEBUG << " strip " << stripIndex % 100;
                LOG_DEBUG << " size: " << digiVec->size() << endm;
            }

            if( digiVec->size() < 2 ) {
                // only one digi left on strip. break loop.
                if( mDoQA ) {
                    mHistograms.at( histNameDigisErased )->Fill( 5 );
                }
                break;
            }

            // two digis --> both sides present    
            StETofDigi* xDigiA = digiVec->at( 0 );
            StETofDigi* xDigiB = digiVec->at( 1 );

            timeDiff = xDigiA->calibTime() - xDigiB->calibTime();

            if( mDebug ) {
                LOG_DEBUG << "matchSides() - time difference in ns: " << timeDiff << endm;
            }

            // side 1 is the top, side 2 is bottom
            if( xDigiA->side() == 2 ) {
                posY = mSigVel.at( detIndex ) * timeDiff * 0.5;
            }
            else {                            
                posY = -1 * mSigVel.at( detIndex ) * timeDiff * 0.5;
            }


            // check for a better match if the local y position is outside the detector bounds
            if( fabs( posY ) > mMaxYPos && digiVec->size() > 2 ) {
                if( mDebug ) {
                    LOG_DEBUG << "matchSides() - hit candidate outside correlation window, check for better possible digis" << endm;
                    LOG_DEBUG << "size of digi vector: " << digiVec->size() << endm;
                }

                StETofDigi* xDigiC = digiVec->at( 2 );
                
                double posYNew     = 0.;
                double timeDiffNew = 0.;

                if( xDigiC->side() == xDigiA->side() ) {
                    timeDiffNew = xDigiC->calibTime() - xDigiB->calibTime();
                }
                else {
                    timeDiffNew = xDigiA->calibTime() - xDigiC->calibTime();
                }

                if( xDigiA->side() == 2 ) {
                    posYNew = mSigVel.at( detIndex ) * timeDiff * 0.5;
                }
                else {                              
                    posYNew = -1 * mSigVel.at( detIndex ) * timeDiff * 0.5;
                }

                if( fabs( posYNew ) < fabs( posY ) ) {
                    if( mDebug ) {
                        LOG_DEBUG << "matchSides() - found better match for hit candidate -> changing out digis" << endm;
                    }

                    timeDiff = timeDiffNew;
                    posY     = posYNew;

                    if( xDigiC->side() == xDigiA->side() ) {
                        xDigiA = xDigiC;

                        iterDigi = digiVec->begin();
                        delete *iterDigi;
                        digiVec->erase( iterDigi );
                        if( mDoQA ){
                            mHistograms.at( histNameDigisErased )->Fill( 4 );
                        }
                    }
                    else {
                        xDigiB = xDigiC;

                        iterDigi = digiVec->begin() + 1;
                        delete *iterDigi;
                        digiVec->erase( iterDigi );
                        if( mDoQA ){
                            mHistograms.at( histNameDigisErased )->Fill( 4 );
                        }
                    }
                }
                else { // --> keeps candidate even if it is outside correlation window
                    if( mDebug ) {
                        LOG_DEBUG << "matchSides() - no better match -> keep this hit candidate" << endm;
                    }
                }
            } // check for better match with third digi done


            if( xDigiA->side() == xDigiB->side() ) {
                LOG_ERROR << "matchSides() - wrong combinations of digis:" << endm;
                LOG_ERROR << *xDigiA << endm;
                LOG_ERROR << *xDigiB << endm; 
            }



            // create the hit candidate:
            // the "strip" time is the mean time between each end
            time = 0.5 * ( xDigiA->calibTime() + xDigiB->calibTime() );

            // weight of merging of hits (later) is the total charge => sum of both ends ToT
            totSum = xDigiA->calibTot() + xDigiB->calibTot();

            // use local coordinates... (0,0,0) is in the center of counter
            posX = ( -1 * eTofConst::nStrips / 2. + strip - 0.5 ) * eTofConst::stripPitch;



            // correct for single side clock jumps. Sync signal recovers time jumps, so no double jumps *should* occur
            // it seems more likely that one Get4 misses one clock pulse and is 6.25ns behind --> need to add half a clock cycle to hit time
            if( mDoClockJumpShift && fabs( posY ) > 0.5 * ( eTofConst::coarseClockCycle * mSigVel.at( detIndex ) - eTofConst::stripLength ) * 0.9 ) {
                if( mDoQA ) {
                    LOG_INFO << "shifting time on: " << sector << "-" << plane << "-" << counter << endm;
                }
                time     += eTofConst::coarseClockCycle * 0.5;
                timeDiff -= eTofConst::coarseClockCycle * ( ( timeDiff < 0 ) ? -1 : ( timeDiff > 0 ) );

                if( xDigiA->side() == 2 ) { // recalculate Y-Position based on new time.
                    posY = mSigVel.at( detIndex ) * timeDiff * 0.5;
                }
                else {
                    posY = -1 * mSigVel.at( detIndex ) * timeDiff * 0.5;
                }
            }



            if( mDebug ) {
                LOG_DEBUG << "detIndex=" << detIndex << "posX=" << posX << "  posY=" << posY << "  time= " << time << "  totSum=" << totSum << endm;
            }

            // build a hit (clustersize is always one strip at this point)
            StETofHit* constructedHit = new StETofHit( sector, plane, counter, time, totSum, 1., posX, posY );

            // push hit into intermediate collection
            mStoreHit[ detIndex ].push_back( constructedHit ); 

            // fill pointer vector
            std::vector< unsigned int > containedDigiIndices;

            containedDigiIndices.push_back( mMapDigiIndex.at( xDigiA ) );
            containedDigiIndices.push_back( mMapDigiIndex.at( xDigiB ) );

            mMapHitDigiIndices[ constructedHit ] = containedDigiIndices;


            LOG_DEBUG << *( mStoreHit.at( detIndex ).back() ) << endm;

            // erase the two used digis!
            iterDigi = digiVec->begin();
            delete *iterDigi;
            delete *(iterDigi+1);
            digiVec->erase( iterDigi + 1 );
            digiVec->erase( iterDigi );
            if( mDoQA ){
                mHistograms.at( histNameDigisErased )->Fill( 6 );
                mHistograms.at( histNameDigisErased )->Fill( 6 );
            }
        } // end of loop over digis on the same strip

    } // end of loop over strips

    if( mDebug ) {
        LOG_DEBUG << "matchSides() - matching of sides done" << endm;
    }
}//::matchSides


//_____________________________________________________________
void
StETofHitMaker::fillUnclusteredHitQA( const double& tstart, const bool isMuDst )
{
    if( !mDoQA ) {
        return;
    }

    // ---------------------------------------
    if( fabs( tstart + 9999. ) < 1.e-5 ) {
        LOG_WARN << "-- no valid start time available ... skip filling histograms with time of flight information" << endm;
    }
    // ---------------------------------------

    int nHitsPrinted = 0;

    //int eventTime = ( this->GetTime() / 10000 ) * 3600 + ( ( this->GetTime() % 10000 ) / 100 ) * 60 + ( this->GetTime() % 100 );

    for( const auto& kv : mStoreHit ) {
        unsigned int detIndex  = kv.first;

        unsigned int sector  =   detIndex / 100;
        unsigned int plane   = ( detIndex % 100 ) / 10;
        unsigned int counter =   detIndex % 10;

        if( mDebug ) {
            LOG_DEBUG << sector  << "  " << plane << "  " << counter << "  size hitVec: " << kv.second.size() << endm;
        }

        for( const auto& hit : kv.second ) {
            if( mDebug ) {
                LOG_DEBUG << "matchSides() - HIT: ";
                LOG_DEBUG << "time="     << setprecision( 16 ) << hit->time()     << "  ";
                LOG_DEBUG << "totalTot=" << setprecision(  4 ) << hit->totalTot() << "  ";
                LOG_DEBUG << "localX="   << setprecision(  4 ) << hit->localX()   << "  ";
                LOG_DEBUG << "localY="   << setprecision(  4 ) << hit->localY()   << endm;
            }

            mCounterActive.at( ( sector - 13 ) * 9 + ( plane - 1 ) * 3 + ( counter - 1 ) ) = true;

            mHistograms.at( "unclusteredHit_tot" )->Fill( hit->totalTot() );

            std::string histNameTot = "unclusteredHit_tot_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
            mHistograms.at( histNameTot )->Fill( hit->totalTot(), hit->localX() );



            // ---------------------------------------

            if( mMapHitDigiIndices.at( hit ).size() >= 2 ) {
                int digiIndexA = mMapHitDigiIndices.at( hit ).at( 0 );
                int digiIndexB = mMapHitDigiIndices.at( hit ).at( 1 );

                float totA = 0.;
                float totB = 0.;

                bool sideSwitch = false;

                if( !isMuDst ) {
                    StSPtrVecETofDigi& etofDigis = mEvent->etofCollection()->etofDigis();
                    totA = etofDigis[ digiIndexA ]->calibTot();
                    totB = etofDigis[ digiIndexB ]->calibTot();

                    if( etofDigis[ digiIndexA ]->side() == 2 ) {
                        sideSwitch = true;
                    }
                }
                else {
                    totA = mMuDst->etofDigi( digiIndexA )->calibTot();
                    totB = mMuDst->etofDigi( digiIndexB )->calibTot();

                    if( mMuDst->etofDigi( digiIndexA )->side() == 2 ) {
                        sideSwitch = true;
                    }
                }

                if( mDebug ) {
                    LOG_DEBUG << "tot of digis in the hit: " << totA << " and " << totB << " sideSwitch: " << sideSwitch << endm;
                }

                float totDiff = totA - totB;
                if( sideSwitch ) totDiff *= -1;

                mHistograms.at( "unclusteredHit_tot_difference" )->Fill( totDiff );

                if( fabs( hit->localY() ) > mMaxYPos ) {
                    mHistograms.at( "unclusteredHit_tail_totAsym" )->Fill( totDiff / ( totA + totB) );
                }
            }

            // ---------------------------------------



            std::string histNamePos = "unclusteredHit_pos_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
            mHistograms.at( histNamePos )->Fill( hit->localX(), hit->localY() );


            std::string histNamePosTime = "unclusteredHit_pos_time_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
            //mHistograms.at( histNamePosTime )->Fill( eventTime, hit->localY() );

            // ---------------------------------------
            if( fabs( tstart + 9999. ) < 1.e-5 ) continue;

            double tof = fmod( hit->time(), eTofConst::bTofClockCycle ) - tstart;

            if( mDebug ) {
                LOG_DEBUG << "hit time, hit time mod bTOF clock cycle, start time, time difference: ";
                LOG_DEBUG << hit->time() << " , " << tof + tstart << " , " << tstart << " , " <<  tof << endm;
                LOG_DEBUG << "sector, plane, counter: " << hit->sector() << " , " << hit->zPlane() << " , " <<  hit->counter() << endm;
            }
            mHistograms.at( "unclusteredHit_tof_fullrange_all" )->Fill( tof );


            while( tof < 0. ) {
                tof += eTofConst::bTofClockCycle;
            }
            tof = fmod( tof, eTofConst::bTofClockCycle );


            mHistograms.at( "unclusteredHit_tof_fullrange" )->Fill( tof );

            mHistograms.at( "unclusteredHit_tof" )->Fill( tof );


            std::string histNameTof = "unclusteredHit_tof_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
            mHistograms[ histNameTof ]->Fill( hit->localX(), tof );


            if( isMuDst ) {
                StMuPrimaryVertex* pVtx = mMuDst->primaryVertex( 0 );
                if( pVtx ) {
                    StThreeVectorD vtxPos = pVtx->position();

                    if( fabs( vtxPos.z() ) <= 10. && fabs( vtxPos.perp() < 2.5 ) ) {
                        histNameTof = "unclusteredHit_pVtx_tof_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                        mHistograms[ histNameTof ]->Fill( hit->localX(), tof );
                    }
                }
            }



            if( fabs( hit->localY() ) > 25. ) {
                histNameTof = "unclusteredHit_tail_tof_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                mHistograms[ histNameTof ]->Fill( hit->localX(), tof );
            }
            else {
                histNameTof = "unclusteredHit_good_tof_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                mHistograms[ histNameTof ]->Fill( hit->localX(), tof );
            }


            if( mDebug ) {
                if( fabs( tof ) > 1000.  && nHitsPrinted < 5 ) {
                    LOG_INFO << "TOF UNNORMALLY LARGE: " << tof << " !!! " << endm;
                    nHitsPrinted++;
                }
            }

            // ---------------------------------------
        } // end of loop over hits in hitVec    
    } // end of loop over mStoreHit
}//::fillUnclusteredHitQA

//_____________________________________________________________
void
StETofHitMaker::mergeClusters( const bool isMuDst )
{
    std::vector< StETofHit* >::iterator iterHit;
    std::vector< std::vector< StETofDigi* > >::iterator iterDigi;

    for( auto kv = mStoreHit.begin(); kv != mStoreHit.end(); kv++ ) {
        unsigned int detIndex  = kv->first;

        unsigned int sector    =   detIndex / 100;
        unsigned int plane     = ( detIndex % 100 ) / 10;
        unsigned int counter   =   detIndex % 10;

        std::vector< StETofHit* > *hitVec = &( kv->second );

        size_t nHitsOnDet = 0;

        while( hitVec->size() > 0 ) {
            if( mDebug ) {
                LOG_DEBUG << "mergeClusters() - hit vector size: " << hitVec->size();
                LOG_DEBUG << " for sector " << sector << " plane " << plane << " counter " << counter << endm;
                LOG_DEBUG << "mergeClusters() - checking hit vector for possible hits to merge with..." << endm;
            }

            StETofHit* pHit = hitVec->at( 0 );

            // scale with tot for weigthed average
            double weight = pHit->totalTot();

            if( weight==0 ) {
                weight = 0.001;
            }

            double weightedTime  = pHit->time()   * weight;
            double weightedPosX  = pHit->localX() * weight;
            double weightedPosY  = pHit->localY() * weight;
            double weightsTotSum = 1.             * weight;
            
            // currently only one-strip clusters --> lowest and highest strip identical
            unsigned int clusterSize = pHit->clusterSize();
            int lowestStrip  = ceil( pHit->localX() / eTofConst::stripPitch );
            int highestStrip = lowestStrip;


            unsigned int index = 1;
            while( hitVec->size() > 1 ) {
                if( mDebug ) {
                    LOG_DEBUG << "mergeClusters() - index in hit vector = " << index << endm;
                }
                if( index >= hitVec->size() ) {
                    if( mDebug ) {
                        LOG_DEBUG << "mergeClusters() - loop index is exceeds size of hit vector -> stop looping" << endm;
                    }
                    break;
                }

                StETofHit* pMergeHit = hitVec->at( index );

                //calculate distance measures
                double timeDiff = pHit->time() - pMergeHit->time();
                double posYDiff = ( pHit->localY() - pMergeHit->localY() ) / mSigVel.at( detIndex ); // divide by signal velocity

                bool isHigherAdjacentStip = false;                
                bool isLowerAdjacentStip  = false;

                if( ceil( pMergeHit->localX() / eTofConst::stripPitch ) - highestStrip == 1 ) {
                    isHigherAdjacentStip = true;
                }
                else if( ceil( pMergeHit->localX() / eTofConst::stripPitch ) - lowestStrip == -1 ) {
                    isLowerAdjacentStip = true;
                }

                // check merging condition: X is not convoluted into the clusterbuilding radius 
                // since it is not supposed to be zero --> check if X position is on a adjacent strip
                if( ( isHigherAdjacentStip || isLowerAdjacentStip ) && 
                    ( sqrt( timeDiff * timeDiff + posYDiff * posYDiff ) ) < mMergingRadius )
                {
                    if( mDebug ) {
                        LOG_DEBUG << "mergeClusters() - merging is going on" << endm; 
                    }
                    //merge hit into cluster
                    double hitWeight = pMergeHit->totalTot();

                    if( hitWeight==0 ) {
                        hitWeight = 0.001;
                    }

                    weightedTime   += ( pMergeHit->time()   * hitWeight );
                    weightedPosX   += ( pMergeHit->localX() * hitWeight );
                    weightedPosY   += ( pMergeHit->localY() * hitWeight );
                    weightsTotSum  += hitWeight;

                    clusterSize++;

                    if( isHigherAdjacentStip ) {
                        highestStrip++;
                    }
                    else if( isLowerAdjacentStip  ) {
                        lowestStrip--;
                    }

                    if( mDebug ) {
                        LOG_DEBUG << "mergeClusters() - detector: " << detIndex << " seed hit localX: " << pHit->localX();
                        LOG_DEBUG << " <>  hit to merge localX: " << pMergeHit->localX();
                        LOG_DEBUG << " <> weighted localX: " << weightedPosX / weightsTotSum << endm; 
                    }

                    // merge contained digi index vectors
                    for( unsigned int i : mMapHitDigiIndices.at( pMergeHit ) ) {
                        mMapHitDigiIndices.at( pHit ).push_back( i );
                    }


                    // erase the hit that was merged
                    iterHit = hitVec->begin() + index;
                    delete *iterHit;
                    hitVec->erase( iterHit );

                    if( mDebug ) {
                        LOG_DEBUG << "mergeClusters() - deleting merged hit from Map" << endm;
                    }

                    mMapHitDigiIndices.erase( pMergeHit );

                } 
                else { // merging condition not fulfilled
                    if( mDebug ) {
                        LOG_DEBUG << "mergeClusters() - merging condition not fulfilled -- check the next hit" << endm;
                    }
                    index++;
                } // check next hit 

            } // end of loop over hits for merging


            // renormalize with the total ToT
            weightedTime /= weightsTotSum;
            weightedPosX /= weightsTotSum;
            weightedPosY /= weightsTotSum;

            // use only the floating point remainder of the time with respect the the bTof clock range
            weightedTime = fmod( weightedTime, eTofConst::bTofClockCycle );
            if( weightedTime < 0 ) weightedTime += eTofConst::bTofClockCycle;

            if( mDebug ) {
                LOG_DEBUG << "mergeClusters() - MERGED HIT: ";
                LOG_DEBUG << "sector: " << sector << "  plane: " << plane << "  counter: " << counter << "\n";
                LOG_DEBUG << "time = "        << setprecision( 16 ) << weightedTime  << "  ";
                LOG_DEBUG << "totalTot = "    << setprecision(  4 ) << weightsTotSum << "  ";
                LOG_DEBUG << "clusterSize = " << setprecision(  1 ) << clusterSize   << "  ";
                LOG_DEBUG << "localX = "      << setprecision(  4 ) << weightedPosX  << "  ";
                LOG_DEBUG << "localY = "      << setprecision(  4 ) << weightedPosY  << endm;
            }

            // create combined hit
            StETofHit* combinedHit = new StETofHit( sector, plane, counter, weightedTime, weightsTotSum, clusterSize, weightedPosX, weightedPosY );

            // fill hit into the eTOF collection or the eTOf hit array depending on StEvent or MuDst input
            if( !isMuDst ) {
                mEvent->etofCollection()->addHit( combinedHit );

                // copy contained digis vector map over to the ETofCollection address
                mMapHitDigiIndices[ combinedHit ] = mMapHitDigiIndices.at( pHit );

                if( mDebug ) {
                    LOG_DEBUG << "mergeClusters(): size of digi vector for combined hit " << nHitsOnDet << " on the counter: ";
                    LOG_DEBUG << mMapHitDigiIndices.at( combinedHit ).size() << " copied over from merged hit vector of size "<< mMapHitDigiIndices.at( pHit ).size() << endm;
                }
            }
            else{
                mMuDst->addETofHit( ( StMuETofHit* ) combinedHit );

                int lastHitIndex = mMuDst->numberOfETofHit() - 1;

                // copy contained digis vector map over to the ETofCollection address
                mMapHitIndexDigiIndices[ lastHitIndex ] = mMapHitDigiIndices.at( pHit );
                if( mDebug ) {
                    LOG_DEBUG << "mergeClusters(): size of digi vector for combined hit " << nHitsOnDet << " on the counter: ";
                    LOG_DEBUG << mMapHitIndexDigiIndices.at( lastHitIndex ).size() << " copied over from merged hit vector of size "<< mMapHitDigiIndices.at( pHit ).size() << endm;
                }
            }


            // delete hit from the internal storage
            iterHit = hitVec->begin();
            delete *iterHit;
            hitVec->erase( iterHit );

            mMapHitDigiIndices.erase( pHit );

            if( mDebug ) {
                if( !isMuDst && mMapHitDigiIndices.count( combinedHit ) ) {
                    LOG_DEBUG << "mergeClusters(): size of digi vector for combined hit " << nHitsOnDet << " on the counter: " << mMapHitDigiIndices.at( combinedHit ).size() << " after deletion of merged hit" << endm;
                }
                if( isMuDst && mMapHitIndexDigiIndices.count( nHitsOnDet ) ) {
                    LOG_DEBUG << "mergeClusters(): size of digi vector for combined hit " << nHitsOnDet << " on the counter: " << mMapHitIndexDigiIndices.at( mMuDst->numberOfETofHit() - 1 ).size() << endm;
                }
            }

            nHitsOnDet++;
        } // end of loop over hits
    } // end of loop over detectors

    if( mDebug ) {
        LOG_DEBUG << "mergeClusters() - merging into clusters done" << endm;
    }
}//::mergeClusters

//_____________________________________________________________
void
StETofHitMaker::assignAssociatedHits( const bool isMuDst )
{
    if( !isMuDst ) {
        StSPtrVecETofHit&  etofHits  = mEvent->etofCollection()->etofHits();
        StSPtrVecETofDigi& etofDigis = mEvent->etofCollection()->etofDigis();

        for( size_t ihit = 0; ihit < etofHits.size(); ihit++ ) {   
            StETofHit* aHit = etofHits[ ihit ];

            if( mDebug ) {
                LOG_DEBUG << "assignAssociatedHits(): size of digi vector for hit " << ihit << ": "<< mMapHitDigiIndices.at( aHit ).size() << endm;
            }

            for ( size_t idigi = 0; idigi < mMapHitDigiIndices.at( aHit ).size(); idigi++ ) {
                if( mDebug ) {
                    LOG_DEBUG << "assignAssociatedHits(): link points to digi " << etofDigis[ mMapHitDigiIndices.at( aHit ).at( idigi ) ] << endm;
                }
                etofDigis[ mMapHitDigiIndices.at( aHit ).at( idigi ) ]->setAssociatedHit( aHit );
            }
        }

    }
    else {
        for( const auto& kv : mMapHitIndexDigiIndices ) {
            if( mDebug ) {
                LOG_DEBUG << "assignAssociatedHits(): size of digi vector for hit index " << kv.first << ": "<< kv.second.size() << endm;
            }

            for( const auto& v: kv.second ) {
                if( mDebug ) {
                    LOG_DEBUG << "assignAssociatedHits(): link to digi index " << v << endm;
                }
                mMuDst->etofDigi( v )->setAssociatedHitId( kv.first );
            }
        }
    }

    LOG_DEBUG << "assignAssociatedHits() - association between digis and hits done" << endm;   
}//::assignAssociatedHits



//_____________________________________________________________
void
StETofHitMaker::fillHitQA( const bool isMuDst, const double& tstart )
{
    int bTofCentral = 700;

    double vpdStart = -9999.;
    double vertexVz = -999.;
    
    if( mDoQA ) {
        startTimeVpd( vpdStart, vertexVz );
    }

    if( !isMuDst ) {
        // --------------------------------------------------------
        // analyze hits in eTOF
        // --------------------------------------------------------
        const StSPtrVecETofHit& etofHits = mEvent->etofCollection()->etofHits();

        int    nHitsETof = 0;
        double averageETofHitTime = 0.;

        for( size_t i=0; i<etofHits.size(); i++ ) {
            StETofHit* aHit = etofHits[ i ];

            if( !aHit ) {
                continue;
            }

            if( mDebug ) {
                LOG_DEBUG << *aHit << endm;
            }

            updateCyclicRunningMean( aHit->time(), averageETofHitTime, nHitsETof, eTofConst::bTofClockCycle );

            // fill histogram to be saved in .hist.root file
            string histNamePos = "etofHit_pos_s" + std::to_string( aHit->sector() ) + "m" + std::to_string( aHit->zPlane() ) + "c" + std::to_string( aHit->counter() );
            mHistograms.at( histNamePos )->Fill( aHit->localX(), aHit->localY() );

            if( mDoQA ) {
                std::string histNameClustersize = "clustersize_s" + std::to_string( aHit->sector() ) + "m" + std::to_string( aHit->zPlane() );
                mHistograms.at( histNameClustersize )->Fill( aHit->clusterSize() );
            }

            // if tstart exists
            if( fabs( tstart ) > 0.001 && fabs( tstart - ( eTofConst::bTofClockCycle - 9999. ) ) > 0.001 ) {
                double tof = aHit->time() - tstart;
                if( tof < -800 ) {
                    tof += eTofConst::bTofClockCycle;
                }

                mHistograms.at( "etofHit_tof"           )->Fill( tof );
                mHistograms.at( "etofHit_tof_fullrange" )->Fill( tof );
            }

            if( mDoQA ) {
                if( fabs( vpdStart - ( eTofConst::bTofClockCycle - 9999. ) ) > 0.001 ) {
                    double tofVpd = aHit->time() - vpdStart;
                    if( tofVpd < -800 ) {
                        tofVpd += eTofConst::bTofClockCycle;
                    }
                    mHistograms.at( "etofHit_vpdVz_tof" )->Fill( vertexVz, tofVpd );
                }
            }
        }

        // --------------------------------------------------------
        // analyze hits in bTOF to get the eTOF-bTOF correlation
        // --------------------------------------------------------
        StBTofCollection* btofCollection = mEvent->btofCollection();

        if( !btofCollection || !btofCollection->hitsPresent() ) {
            LOG_WARN << "fillHitQA - no btof collection or no bTof hits present" << endm;
            return;
        }

        const StSPtrVecBTofHit& btofHits = btofCollection->tofHits();

        int    nHitsBTof = 0;
        double averageBTofHitTime = 0.;

        for( size_t i=0; i<btofHits.size(); i++ ) {
            StBTofHit* aHit = btofHits[ i ];

            if( !aHit ) {
                continue;
            }

            updateCyclicRunningMean( aHit->leadingEdgeTime(), averageBTofHitTime, nHitsBTof, eTofConst::bTofClockCycle );

            // if doQA && tstart exists
            if( mDoQA && fabs( tstart ) > 0.001 && fabs( tstart - ( eTofConst::bTofClockCycle - 9999. ) ) > 0.001 ) {
                double tof = aHit->leadingEdgeTime() - tstart;
                if( tof < 0 ) {
                    tof += eTofConst::bTofClockCycle;
                }

                mHistograms.at( "btofHit_tof_fullrange" )->Fill( tof );
            }
        }

        float diff = averageETofHitTime - averageBTofHitTime;
        if( diff < -800 ) diff += eTofConst::bTofClockCycle;

        mHistograms.at( "averageTimeDiff_etofHits_btofHits" )->Fill( diff );
        mHistograms.at( "multiplicity_etofHits_btofHits"    )->Fill( nHitsETof, nHitsBTof );

        if( mDoQA && nHitsBTof > bTofCentral ) {
            std::vector< int > etofHitsPerModule( eTofConst::nModules );
            for( size_t i=0; i<etofHits.size(); i++ ) {
                StETofHit* aHit = etofHits[ i ];

                if( !aHit ) {
                    continue;
                }
                etofHitsPerModule.at( ( aHit->sector() - 13 ) * 3 + aHit->zPlane() - 1 ) += 1;
            }

            for( size_t i=0; i<eTofConst::nModules; i++ ) {
                mHistograms.at( "hitMultiplicityPerModuleCentral" )->Fill( i, etofHitsPerModule.at( i ) );
            }
        }

        // --------------------------------------------------------
        // analyze correlation with EPD East
        // --------------------------------------------------------
        StEpdCollection* epdCollection = mEvent->epdCollection();
        if( !epdCollection || !epdCollection->hitsPresent() ) {
            LOG_WARN << "fillHitQA - no epd collection or no epd hits present" << endm;
            return;
        }

        const StSPtrVecEpdHit& epdHits = epdCollection->epdHits();

        float nHitsEpdEast = 0.;

        LOG_INFO << epdHits.size() << " ### " << endm;

        for( size_t i=0; i<epdHits.size(); i++ ) {
            StEpdHit* epdHit = epdHits[ i ];
            if( !epdHit ) {
                continue;
            }

            if( epdHit->nMIP() < 0.3 ) continue;
            if( epdHit->id() > 0 )     continue; //positive id is the west side
            
            if( epdHit->nMIP() < 5 ) {
                nHitsEpdEast += epdHit->nMIP();
            }
            else {
                nHitsEpdEast += 5;  //high hits are dominated by landau fluctuations
            }
        }

        mHistograms.at( "multiplicity_etofHits_epdEast" )->Fill( nHitsETof, nHitsEpdEast );
        if( mDoQA ) {
            mHistograms.at( "multiplicity_btofHits_epdEast" )->Fill( nHitsBTof, nHitsEpdEast );
        }

    }
    else {
        // --------------------------------------------------------
        // analyze hits in eTOF
        // --------------------------------------------------------
        int    nHitsETof = 0;
        double averageETofHitTime = 0.;

        for( size_t i=0; i<mMuDst->numberOfETofHit(); i++ ) {
            StMuETofHit* aHit = mMuDst->etofHit( i );

            if( !aHit ) {
                continue;
            }

            if( mDebug ) {
                LOG_DEBUG << "hit (" << i << "): sector,plane,counter=" << aHit->sector() << ",";
                LOG_DEBUG << aHit->zPlane() << "," << aHit->counter() << " time=" << aHit->time();
                LOG_DEBUG << " localX=" << aHit->localX() << " localY=" << aHit->localY();
                LOG_DEBUG << " clustersize=" << aHit->clusterSize() << endm;
            }

            updateCyclicRunningMean( aHit->time(), averageETofHitTime, nHitsETof, eTofConst::bTofClockCycle );

            // fill histogram to be saved in .hist.root file
            string histNamePos = "etofHit_pos_s" + std::to_string( aHit->sector() ) + "m" + std::to_string( aHit->zPlane() ) + "c" + std::to_string( aHit->counter() );
            mHistograms.at( histNamePos )->Fill( aHit->localX(), aHit->localY() );

            if( mDoQA ) {
                std::string histNameClustersize = "clustersize_s" + std::to_string( aHit->sector() ) + "m" + std::to_string( aHit->zPlane() );
                mHistograms.at( histNameClustersize )->Fill( aHit->clusterSize() );
            }

            // if tstart exists
            if( fabs( tstart ) > 0.001 && fabs( tstart - ( eTofConst::bTofClockCycle - 9999. ) ) > 0.001 ) {
                double tof = aHit->time() - tstart;
                if( tof < -800 ) {
                    tof += eTofConst::bTofClockCycle;
                }

                mHistograms.at( "etofHit_tof"           )->Fill( tof );
                mHistograms.at( "etofHit_tof_fullrange" )->Fill( tof );
            }

            if( mDoQA ) {
                if( fabs( vpdStart - ( eTofConst::bTofClockCycle - 9999. ) ) > 0.001 ) {
                    double tofVpd = aHit->time() - vpdStart;
                    if( tofVpd < -800 ) {
                        tofVpd += eTofConst::bTofClockCycle;
                    }
                    mHistograms.at( "etofHit_vpdVz_tof" )->Fill( vertexVz, tofVpd );
                }
            }
        }

        // --------------------------------------------------------
        // analyze hits in bTOF to get the eTOF-bTOF correlation
        // --------------------------------------------------------
        if( !mMuDst->btofArray( muBTofHit ) || !mMuDst->numberOfBTofHit() ) {
            LOG_WARN << "fillHitQA - no btof hit array or no btof hits present" << endm;
            return;
        }

        int    nHitsBTof = 0;
        double averageBTofHitTime = 0.;

        for( size_t i=0; i<mMuDst->numberOfBTofHit(); i++ ) {
            StMuBTofHit* aHit = mMuDst->btofHit( i );
            
            if( !aHit ) {
                continue;
            }

            updateCyclicRunningMean( aHit->leadingEdgeTime(), averageBTofHitTime, nHitsBTof, eTofConst::bTofClockCycle );

            // if doQA && tstart exists
            if( mDoQA && fabs( tstart ) > 0.001 && fabs( tstart - ( eTofConst::bTofClockCycle - 9999. ) ) > 0.001 ) {
                double tof = aHit->leadingEdgeTime() - tstart;
                if( tof < -800 ) {
                    tof += eTofConst::bTofClockCycle;
                }

                mHistograms.at( "btofHit_tof_fullrange" )->Fill( tof );
            }
        }

        double diff = averageETofHitTime - averageBTofHitTime;
        if( diff < -800 ) diff += eTofConst::bTofClockCycle;

        mHistograms.at( "averageTimeDiff_etofHits_btofHits" )->Fill( diff );
        mHistograms.at( "multiplicity_etofHits_btofHits"    )->Fill( nHitsETof, nHitsBTof );

        if( mDoQA && nHitsBTof > bTofCentral ) {
            std::vector< int > etofHitsPerModule( eTofConst::nModules );
            for( size_t i=0; i<mMuDst->numberOfETofHit(); i++ ) {
                StMuETofHit* aHit = mMuDst->etofHit( i );

                if( !aHit ) {
                    continue;
                }
                etofHitsPerModule.at( ( aHit->sector() - 13 ) * 3 + aHit->zPlane() - 1 ) += 1;
            }

            for( size_t i=0; i<eTofConst::nModules; i++ ) {
                mHistograms.at( "hitMultiplicityPerModuleCentral" )->Fill( i, etofHitsPerModule.at( i ) );
            }
        }

        // --------------------------------------------------------
        // analyze correlation with EPD East
        // --------------------------------------------------------
        if( !mMuDst->epdHits() || !mMuDst->numberOfEpdHit() ) {
            LOG_WARN << "fillHitQA - no epd hit array or no epd hits present" << endm;
            return;
        }

        size_t nHitsEpd = mMuDst->numberOfEpdHit();
        float nHitsEpdEast = 0.;

        for( size_t i=0; i<nHitsEpd; i++ ) {
            StMuEpdHit* epdHit = mMuDst->epdHit( i );
            if( !epdHit ) {
                continue;
            }

            if( epdHit->nMIP() < 0.3 ) continue;
            if( epdHit->id() > 0 )     continue; //positive id is the west side
            
            if( epdHit->nMIP() < 5 ) {
                nHitsEpdEast += epdHit->nMIP();
            }
            else {
                nHitsEpdEast += 5;  //high hits are dominated by landau fluctuations
            }
        }

        mHistograms.at( "multiplicity_etofHits_epdEast" )->Fill( nHitsETof, nHitsEpdEast );
        if( mDoQA ) {
            mHistograms.at( "multiplicity_btofHits_epdEast" )->Fill( nHitsBTof, nHitsEpdEast );
        }

    }

    LOG_DEBUG << "fillHitQA() - histograms filled" << endm;   
}//::fillHitQA


//_____________________________________________________________
// setHistFileName uses the string argument from the chain being run to set
// the name of the output histogram file.
void
StETofHitMaker::setHistFileName()
{
    string extension = ".etofHit.root";

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
StETofHitMaker::bookHistograms()
{
    LOG_INFO << "bookHistograms() ... " << endm;

    mHistograms[ "etofHit_tof"                       ] = new TH1F( "etofHit_tof",           "eTOF hit time of flight;time of flight (ns);# hits", 4000, -100., 150 );
    mHistograms[ "etofHit_tof_fullrange"             ] = new TH1F( "etofHit_tof_fullrange", "eTOF hit time of flight;time of flight (ns);# hits", 5000, -800., eTofConst::bTofClockCycle );
    mHistograms[ "averageTimeDiff_etofHits_btofHits" ] = new TH1F( "averageTimeDiff_etofHits_btofHits", "difference between average times in bTOF and eTOF hits;#DeltaT (ns);# events", 4000, -500, 500 );
    mHistograms[ "multiplicity_etofHits_btofHits"    ] = new TH2F( "multiplicity_etofHits_btofHits", "multiplicity correlation between bTOF and eTOF;# eTOF hits;# bTOF hits",         300, 0, 300, 500, 0, 1000 );
    mHistograms[ "multiplicity_etofHits_epdEast"     ] = new TH2F( "multiplicity_etofHits_epdEast",  "multiplicity correlation between eTOF and east EPD;# eTOF hits;# hits east EPD", 300, 0, 300, 200, 0, 1000 );

    AddHist( mHistograms.at( "etofHit_tof"                       ) );
    AddHist( mHistograms.at( "etofHit_tof_fullrange"             ) );
    AddHist( mHistograms.at( "averageTimeDiff_etofHits_btofHits" ) );
    AddHist( mHistograms.at( "multiplicity_etofHits_btofHits"    ) );
    AddHist( mHistograms.at( "multiplicity_etofHits_epdEast"     ) );

    if( mDoQA ) {
        mHistograms[ "etofHit_vpdVz_tof"             ] = new TH2F( "etofHit_vpdVz_tof",     "eTOF hit time of flight;VPD Vz (cm);time of flight (ns)", 100, -200, 200, 1000, -50., 50 );
        mHistograms[ "btofHit_tof_fullrange"         ] = new TH1F( "btofHit_tof_fullrange", "bTOF hit time of flight;time of flight (ns);# hits", 5000, -800., eTofConst::bTofClockCycle );
        mHistograms[ "multiplicity_btofHits_epdEast" ] = new TH2F( "multiplicity_btofHits_epdEast", "multiplicity correlation between bTOF and east EPD;# bTOF hits;# hits east EPD", 200, 0, 1000, 200, 0, 1000 );
        mHistograms[ "hitMultiplicityPerModuleCentral" ] = new TH2F( "hitMultiplicityPerModuleCentral", "hit multiplicity per module in central bTOF events", 36, 0, 36, 50, 0, 50 );
        mHistograms[ "multiplicity_etofDigis_etofHits" ] = new TH2F( "multiplicity_etofDigis_etofHits",  "multiplicity correlation between eTOF digis and hits;# eTOF digis;# eTOF hits", 500, 0, 1000, 500, 0, 1000 );
    }

    for( int sector = eTofConst::sectorStart; sector <= eTofConst::sectorStop; sector++ ) {
        for( int plane = eTofConst::zPlaneStart; plane <= eTofConst::zPlaneStop; plane++ ) {
            for( int counter = eTofConst::counterStart; counter <= eTofConst::counterStop; counter++ ) {
                std::string histName_etofHit_pos = "etofHit_pos_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );

                mHistograms[ histName_etofHit_pos ] = new TH2F( Form( "etofHit_pos_s%d_m%d_c%d", sector, plane, counter ), "eTOF hit localXY;local X (cm);local Y (cm)", 64, -16., 16., 400, -500., 500. );
                AddHist( mHistograms.at( histName_etofHit_pos ) );

                if( mDoQA ) {
                    std::string histNameDigisPerStrip = "digisPerStrip_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    mHistograms[ histNameDigisPerStrip ] = new TH2F( Form( "digisPerStrip_s%d_m%d_c%d", sector, plane, counter ), "digis per strip;strip;# digis per event", 32, 0.5, 32.5, 20, 0., 20. );

                    std::string histNameDigisErased = "digisErased_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    mHistograms[ histNameDigisErased ] = new TH1F( Form( "digisErased_s%d_m%d_c%d", sector, plane, counter ), "digis erased;;# digis", 6, 0.5, 6.5 );
                    mHistograms[ histNameDigisErased ]->GetXaxis()->SetBinLabel( 1, "digi inside dead time"         );
                    mHistograms[ histNameDigisErased ]->GetXaxis()->SetBinLabel( 2, "single digi on strip"          );
                    mHistograms[ histNameDigisErased ]->GetXaxis()->SetBinLabel( 3, "3 consecutive same side digis" );
                    mHistograms[ histNameDigisErased ]->GetXaxis()->SetBinLabel( 4, "better match for partner"      );
                    mHistograms[ histNameDigisErased ]->GetXaxis()->SetBinLabel( 5, "single remaining digi"         );
                    mHistograms[ histNameDigisErased ]->GetXaxis()->SetBinLabel( 6, "matched into pair"             );
                }
            }

            if( mDoQA ) {
                std::string histName_clustersize = "clustersize_s" + std::to_string( sector ) + "m" + std::to_string( plane );
                mHistograms[ histName_clustersize ] = new TH1F( Form( "clustersize_s%d_m%d", sector, plane ), "clustersize;clustersize;# events", 32, 0., 32. );
            }
        }
    }

    if( mDoQA ) {
        mHistograms[ "unclusteredHit_tot" ] = new TH1F( "unclusteredHit_tot", "unclustered hit tot; tot (ns);# unclustered hits", 1000, 0., 80. );
        mHistograms[ "unclusteredHit_tof" ] = new TH1F( "unclusteredHit_tof", "unclustered hit time of flight; time of flight (ns);# unclustered hits", 5000, 0., 1000. );

        mHistograms[ "unclusteredHit_tot_difference" ] = new TH1F( "unclusteredHit_tot_difference", "unclustered hit tot difference of digis; #Delta tot (ns);# unclustered hits", 1000, -100., 100. );
        mHistograms[ "unclusteredHit_tail_totAsym"   ] = new TH1F( "unclusteredHit_tail_totAsym",   "unclustered hit tail tot asymmetry of digis; #Delta tot/ tot sum;# unclustered hits", 200, -1., 1. );

        mHistograms[ "unclusteredHit_tof_fullrange"     ] = new TH1F( "unclusteredHit_tof_fullrange",     "unclustered hit time of flight; time of flight (ns);# unclustered hits", 5000, -1. * eTofConst::bTofClockCycle, eTofConst::bTofClockCycle );
        mHistograms[ "unclusteredHit_tof_fullrange_all" ] = new TH1F( "unclusteredHit_tof_fullrange_all", "unclustered hit time of flight; time of flight (ns);# unclustered hits", 5000, -1. * eTofConst::bTofClockCycle, eTofConst::bTofClockCycle );

        for( int sector = eTofConst::sectorStart; sector <= eTofConst::sectorStop; sector++ ) {
            for( int plane = eTofConst::zPlaneStart; plane <= eTofConst::zPlaneStop; plane++ ) {
                for( int counter = eTofConst::counterStart; counter <= eTofConst::counterStop; counter++ ) {
                    std::string histName_unclusteredHit_tot = "unclusteredHit_tot_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    std::string histName_unclusteredHit_pos = "unclusteredHit_pos_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    std::string histName_unclusteredHit_tof = "unclusteredHit_tof_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );

                    std::string histName_unclusteredHit_tail_tof = "unclusteredHit_tail_tof_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    std::string histName_unclusteredHit_good_tof = "unclusteredHit_good_tof_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    std::string histName_unclusteredHit_pVtx_tof = "unclusteredHit_pVtx_tof_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );

                    mHistograms[ histName_unclusteredHit_tot ] = new TH2F( Form( "unclusteredHit_tot_s%d_m%d_c%d", sector, plane, counter ), "unclustered hit tot; tot (ns);local X (cm)", 1000, 0.,   80., 32, -16., 16. );
                    mHistograms[ histName_unclusteredHit_pos ] = new TH2F( Form( "unclusteredHit_pos_s%d_m%d_c%d", sector, plane, counter ), "unclustered hit localXY; local X (cm);local Y (cm)", 32, -16., 16., 400, -50., 50. );

                    mHistograms[ histName_unclusteredHit_tof      ] = new TH2F( Form( "unclusteredHit_tof_s%d_m%d_c%d",      sector, plane, counter ), "unclustered hit time of flight;local X (cm);time of flight (ns)",         32, -16., 16., 5000, 0., 1000. );

                    mHistograms[ histName_unclusteredHit_tail_tof ] = new TH2F( Form( "unclusteredHit_tof_tail_s%d_m%d_c%d", sector, plane, counter ), "unclustered hit (tail) time of flight;local X (cm);time of flight (ns)",  32, -16., 16., 5000, 0., 1000. );
                    mHistograms[ histName_unclusteredHit_good_tof ] = new TH2F( Form( "unclusteredHit_tof_good_s%d_m%d_c%d", sector, plane, counter ), "unclustered hit (good) time of flight;local X (cm);time of flight (ns)",  32, -16., 16., 2500, 0., 50.   );
                    mHistograms[ histName_unclusteredHit_pVtx_tof ] = new TH2F( Form( "unclusteredHit_pVtx_tof_s%d_m%d_c%d", sector, plane, counter ), "unclustered hit time of flight;local X (cm);time of flight (ns)",         32, -16., 16., 2500, 0., 50.   );

                    std::string histName_digi_tot = "digi_tot_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );

                    mHistograms[ histName_digi_tot ] = new TH2F( Form( "digi_tot_s%d_m%d_c%d", sector, plane, counter ), "digi tot;(strip-1)+0.5*(side-1);tot (arb. units)", 64, 0., 32., 200, 0., 40. );

                    std::string histName_unclusteredHit_pos_time = "unclusteredHit_pos_time_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    mHistograms[ histName_unclusteredHit_pos_time ] = new TH2F( Form( "unclusteredHit_pos_time_s%d_m%d_c%d", sector, plane, counter ), "hit position over time;time (s);hit position (cm)", 3600, 19800., 23400., 200, -100., 100. );
                }
            }
        }

        mHistograms[ "counter_active" ] = new TH2F( "counter_active", "active counters by run;100*day + run number;counter", 15000, 5000., 20000., 108 , 0.5, 108.5 );
    }

    for ( auto& kv : mHistograms ) {
        kv.second->SetDirectory( 0 );
    }
}

//_____________________________________________________________
void
StETofHitMaker::writeHistograms()
{
    if( mHistFileName != "" ) {
        LOG_INFO << "writing histograms to: " << mHistFileName.c_str() << endm;

        TFile histFile( mHistFileName.c_str(), "RECREATE", "etofHit" );
        histFile.cd();
        
        for ( const auto& kv : mHistograms ) {
            if( kv.second->GetEntries() > 0 ) kv.second->Write();
        }

        histFile.Close();
    }
    else {
        LOG_INFO << "histogram file name is empty string --> cannot write histograms" << endm;
    }
}


//_____________________________________________________________
unsigned int
StETofHitMaker::detectorToKey( const unsigned int detectorId )
{
    unsigned int sector   = (   detectorId / eTofConst::nCountersPerSector  )                           + eTofConst::sectorStart;
    unsigned int zPlane   = ( ( detectorId % eTofConst::nCountersPerSector  ) / eTofConst::nCounters  ) + eTofConst::zPlaneStart;
    unsigned int counter  = (   detectorId % eTofConst::nCounters                                     ) + eTofConst::counterStart;

    return sector * 100 + zPlane * 10  + counter;
}

//_____________________________________________________________
void
StETofHitMaker::updateCyclicRunningMean( const double& value, double& mean, int& count, const double& range )
{
    double valIn = value;
    if( mean - value < -0.9 * range ) {
        valIn -= range;  
    } 
    else if( mean - value > 0.9 * range ) {
        valIn += range;
    }

    count++;

    double scaling = 1. / count;

    if( mDebug ) {
        LOG_INFO << "old mean: " << mean << "  scaling: " << scaling << " value in: " << valIn << endm;
    }

    mean = valIn * scaling + mean * ( 1. - scaling );

    if( mDebug ) {
        LOG_INFO << "new mean: " << mean << endm;
    }
}