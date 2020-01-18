/***************************************************************************
 *
 * $Id: StETofMatchMaker.cxx,v 1.10 2020/01/18 02:37:10 fseck Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofMatchMaker - class to match StETofHits to tracks.
 *              The matching is done in several steps:
 *              - get a list of eTOF hits
 *              - check for each track if its helix has an intersection with
 *                the eTOF active volumes (MRPCs gas gaps)
 *              - resolve matching ambiguities 
 *
 ***************************************************************************
 *
 * $Log: StETofMatchMaker.cxx,v $
 * Revision 1.10  2020/01/18 02:37:10  fseck
 * fixing StEvent part of eTOF-only T0 calculation
 *
 * Revision 1.9  2020/01/16 03:53:41  fseck
 * added etof-only and hybrid btof-etof start time calculations for on-the-fly corrections
 *
 * Revision 1.8  2019/12/17 03:28:01  fseck
 * update to histograms for .hist.root files
 *
 * Revision 1.7  2019/12/10 16:00:34  fseck
 * possibility to use step-wise track extrapolation in changing magnetic field via setting a flag
 *
 * Revision 1.6  2019/05/09 00:02:46  fseck
 * match distances as member variables and updated handling for many-tracks-to-one-hit matches
 *
 * Revision 1.5  2019/04/24 02:33:48  fseck
 * start time fix to previous commit
 *
 * Revision 1.4  2019/04/24 01:02:11  fseck
 * fix to start time for simulation and more histograms added to doQA mode
 *
 * Revision 1.3  2019/03/25 01:05:48  fseck
 * added more histograms for offline QA
 *
 * Revision 1.2  2019/03/08 19:09:31  fseck
 * added a few eTOF histograms to the .hist.root files for offline QA
 *
 * Revision 1.1  2019/02/19 19:52:28  jeromel
 * Reviewed code provided by F.Seck
 *
 *
 ***************************************************************************/
#include <sstream>
#include <cmath>

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"

#include "StParticleTypes.hh"
#include "StParticleDefinition.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

#include "StChainOpt.h" // for renaming the histogram file

#include "StEvent.h"
#include "StTrackNode.h"
#include "StTrackGeometry.h"
#include "StGlobalTrack.h"
#include "StPrimaryTrack.h"
#include "StPrimaryVertex.h"
#include "StETofCollection.h"
#include "StETofHit.h"
#include "StTrackPidTraits.h"
#include "StETofPidTraits.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StDedxPidTraits.h"

#include "StDetectorDbMaker/St_MagFactorC.h"

#include "StBTofCollection.h"
#include "StBTofHeader.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuArrays.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuETofHit.h"
#include "StMuDSTMaker/COMMON/StMuETofPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuETofDigi.h"

#include "StETofMatchMaker.h"
#include "StETofUtil/StETofGeometry.h"
#include "StETofUtil/StETofConstants.h"

#include "tables/St_etofMatchParam_Table.h"


// *******************************************************************************************************

namespace etofProjection {
    // safety margins in cm in local x and y direction for track extrapolation to etof modules
    const double safetyMargins[ 2 ] = { 2., 2. };

    const float minEtaCut     = 0.0;

    const float minEtaProjCut = -1.0;
    const float maxEtaProjCut = -1.7;


    // --> TODO: move to database once alignment procedure is in place
    const double deltaXoffset[ 108 ] = {
        2.01,  2.46,  2.91,  2.09,  2.68,  3.34,  2.08,  2.82,  3.46,
        1.68,  2.21,  2.97,  2.03,  2.53,  3.52,  1.99,  2.64,  3.59,
        1.67,  2.17,  2.68,  1.99,  2.69,  3.33,  2.11,  2.80,  3.43,
        1.53,  2.10,  2.59,  1.89,  2.47,  3.19,  2.17,  2.86,  3.60,
        1.35,  1.89,  2.40,  1.75,  2.39,  3.06,  1.97,  2.63,  3.43,
        1.04,  1.78,  2.18,  1.85,  2.34,  3.09,  1.93,  2.56,  3.27,
        1.14,  1.76,  2.27,  1.75,  2.39,  3.05,  1.91,  2.68,  3.28,
        1.34,  1.96,  2.28,  2.02,  2.46,  3.02,  1.88,  2.54,  3.14,
        1.70,  2.22,  2.51,  2.08,  2.65,  3.24,  2.05,  2.78,  3.32,
        1.91,  2.41,  2.86,  2.20,  2.79,  3.35,  2.20,  2.91,  3.50,
        1.97,  2.40,  2.87,  2.08,  2.64,  3.28,  2.06,  2.77,  3.35,
        2.08,  2.52,  3.07,  2.03,  2.67,  3.31,  2.04,  2.74,  3.39 };

    const double deltaYoffset[ 108 ] = {
        0.79,  0.83,  0.65,  0.18,  0.62,  1.03, -0.22, -0.10, -0.15,
        0.50,  0.63,  0.77,  0.11,  0.53,  1.11, -0.10, -0.24,  0.08,
        0.35,  0.49,  0.36, -0.03,  0.26,  0.84, -0.24, -0.28, -0.15,
        0.20,  0.05,  0.08, -0.24,  0.04,  0.37, -0.41, -0.62, -0.75,
        0.50,  0.63,  0.48, -0.08,  0.28,  0.47, -0.36, -0.39, -0.39,
        0.62,  0.67,  1.03,  0.13,  0.49,  0.75, -0.36, -0.29, -0.30,
        1.05,  1.12,  1.06,  0.15,  0.50,  0.69, -0.29, -0.05, -0.23,
        1.39,  1.17,  1.22,  0.39,  0.63,  0.93, -0.17, -0.50, -0.36,
        1.27,  1.22,  0.89,  0.29,  0.22,  1.06, -0.35, -0.57, -0.54,
        1.67,  1.51,  1.16,  0.33,  0.70,  1.04,  0.09, -0.11, -0.28,
        1.38,  1.31,  0.94,  0.22,  0.49,  0.54, -0.21, -0.26, -0.28,
        1.06,  1.12,  1.29,  0.30,  0.70,  1.34, -0.01,  0.16, -0.03 };

    const double deltaRcut = 4.;
}

//---------------------------------
static const double kaon_minus_mass_c2 = 493.68 * MeV;
//---------------------------------

namespace etofHybridT0 {
    const unsigned int minCand     = 2;
    const unsigned int nSwitch     = 5;
    const unsigned int nMin        = 10;
    const unsigned int nUpdate     = 5;
    const float        lowCut      = 0.10;  // 10%
    const float        highCut     = 0.85;  // 85%
    const float        diffToClear = 2.;    // ns
    const float        biasOffset  = 0.075; // ns
}
// *******************************************************************************************************


//---------------------------------------------------------------------------
StETofMatchMaker::StETofMatchMaker( const char* name )
: StMaker( "etofMatch", name ),
  mEvent( nullptr ),
  mMuDst( nullptr ),
  mETofGeom( nullptr ),
  mFileNameMatchParam( "" ),
  mIsStEventIn( false ),
  mIsMuDstIn( false ),
  mOuterTrackGeometry( true ),
  mUseHelixSwimmer( false ),
  mUseOnlyBTofHeaderStartTime( false ),
  mIsSim( false ),
  mDoQA( false ),
  mDebug( false ),
  mMatchDistX(  5. ),
  mMatchDistY( 10. ),
  mMatchDistT( 99999. ),
  mT0corr( 0. ),
  mT0switch( 0 ),
  mNupdatesT0( 0 ),
  mMatchRadius( 0. ),
  mHistFileName( "" )
{
    mT0corrVec.clear();
    mT0corrVec.reserve( 500 );


    mIndex2Primary.clear();

    mTrackCuts.push_back( 0. ); // nHitsFit
    mTrackCuts.push_back( 0. ); // nHitsRatio
    mTrackCuts.push_back( 0. ); // low pt

    mHistograms.clear();
    mHistograms2d.clear();
}


//---------------------------------------------------------------------------
StETofMatchMaker::~StETofMatchMaker()
{
    /* nope */
}


//---------------------------------------------------------------------------
Int_t
StETofMatchMaker::Init()
{
    LOG_INFO << "StETofMatchMaker::Init()" << endm;

    LOG_INFO << "isSimulation flag was set to: " << mIsSim << endm;

    bookHistograms();

    return kStOk;
}


//---------------------------------------------------------------------------
Int_t
StETofMatchMaker::InitRun( Int_t runnumber )
{
    LOG_INFO << "StETofMatchMaker::InitRun()" << endm;

    TDataSet* dbDataSet = nullptr;
    std::ifstream paramFile;

    // --------------------------------------------------------------------------------------------
    // initialize hit building parameters from parameter file (if filename is provided) or database:
    // -- match param
    // --------------------------------------------------------------------------------------------

    // match param
    if( mFileNameMatchParam.empty() ) {
        LOG_INFO << "etofMatchParam: no filename provided --> load database table" << endm;

        dbDataSet = GetDataBase( "Calibrations/etof/etofMatchParam" );

        St_etofMatchParam* etofMatchParam = static_cast< St_etofMatchParam* > ( dbDataSet->Find( "etofMatchParam" ) );
        if( !etofMatchParam ) {
            LOG_ERROR << "unable to get the match params from the database" << endm;
            return kStFatal;
        }

        etofMatchParam_st* matchParamTable = etofMatchParam->GetTable();

        mMatchRadius       = matchParamTable->matchRadius; 

        mTrackCuts.at( 0 ) = matchParamTable->trackCutNHitsFit;
        mTrackCuts.at( 1 ) = matchParamTable->trackCutNHitsRatio;
        mTrackCuts.at( 2 ) = matchParamTable->trackCutLowPt;

    }
    else {
        LOG_INFO << "etofMatchParam: filename provided --> use parameter file: " << mFileNameMatchParam.c_str() << endm;
        
        paramFile.open( mFileNameMatchParam.c_str() );

        if( !paramFile.is_open() ) {
            LOG_ERROR << "unable to get the 'etofMatchParam' parameters from file --> file does not exist" << endm;
            return kStFatal;
        }

        std::vector< float > param;
        float temp;
        while( paramFile >> temp ) {
            param.push_back( temp );
        }
        
        paramFile.close();

        if( param.size() != 4 ) {
            LOG_ERROR << "parameter file for 'etofMatchParam' has not the right amount of entries: ";
            LOG_ERROR << param.size() << " instead of 4 !!!!" << endm;
            return kStFatal;
        }

        mMatchRadius       = param.at( 0 ); 

        mTrackCuts.at( 0 ) = param.at( 1 );
        mTrackCuts.at( 1 ) = param.at( 2 );
        mTrackCuts.at( 2 ) = param.at( 3 );

    }

    LOG_INFO << " match radius: "           << mMatchRadius       << endm;
    LOG_INFO << " track cut (nHitsFit): "   << mTrackCuts.at( 0 ) << endm;
    LOG_INFO << " track cut (nHitsRatio): " << mTrackCuts.at( 1 ) << endm;
    LOG_INFO << " track cut (low pt): "     << mTrackCuts.at( 2 ) << endm;

    // --------------------------------------------------------------------------------------------



    // --------------------------------------------------------------------------------------------
    // initializie etof geometry
    // --------------------------------------------------------------------------------------------
    if( !mETofGeom ) {
        LOG_INFO << " creating a new eTOF geometry . . . " << endm;
        mETofGeom = new StETofGeometry( "etofGeometry", "etofGeometry in MatchMaker" );
    }

    if( mETofGeom && !mETofGeom->isInitDone() ) {
        LOG_INFO << " eTOF geometry initialization ... " << endm;

        if( !gGeoManager ) GetDataBase( "VmcGeometry" );

        if( !gGeoManager ) {
            LOG_ERROR << "Cannot get GeoManager" << endm;
            return kStErr;
        }

        LOG_DEBUG << " gGeoManager: " << gGeoManager << endm;

        mETofGeom->init( gGeoManager, etofProjection::safetyMargins, mUseHelixSwimmer );
    }

    if( mDoQA ) {
        // for geometry debugging
        for( unsigned int i=0; i<mETofGeom->nValidModules(); i++ ) {

            StThreeVectorF pos = mETofGeom->module( i )->centerPos();

            int sector = mETofGeom->module( i )->sector();
            int plane  = mETofGeom->module( i )->plane();

            for( int j=0; j<sector; j++ ) {
                mHistograms.at( "eTofSectors" )->Fill( pos.x(), pos.y() );
            }
            for( int j=0; j<plane; j++ ) {
                mHistograms.at( "eTofModules" )->Fill( pos.x(), pos.y() );
            }
        }


        // for magnetic field debugging
        int nBinsX = mHistograms2d.at( "bfield_z" )->GetNbinsX();
        int nBinsY = mHistograms2d.at( "bfield_z" )->GetNbinsY();
        for( int i=0; i<nBinsX; i++ ) {
            for( int j=0; j<nBinsY; j++ ) {
                double z = mHistograms2d.at( "bfield_z" )->GetXaxis()->GetBinCenter( i );
                double y = mHistograms2d.at( "bfield_z" )->GetYaxis()->GetBinCenter( j );

                mHistograms2d.at( "bfield_z" )->Fill( z, y, mETofGeom->getFieldZ( 0., y, z ) );
            }
        }
    }

    return kStOk;
}


//---------------------------------------------------------------------------
Int_t 
StETofMatchMaker::FinishRun( Int_t runnumber )
{
    LOG_DEBUG << "StETofMatchMaker::FinishRun() -- cleaning up the geometry" << endm;

    if( mETofGeom ) {
        mETofGeom->reset();
    }

    return kStOk;
}


//---------------------------------------------------------------------------
Int_t
StETofMatchMaker::Finish()
{
    LOG_DEBUG << "StETofMatchMaker::Finish()" << endm;

    if( mDoQA ) {
        LOG_INFO << "Finish() - writing *.etofMatch.root ..." << endm;
        setHistFileName();
        writeHistograms();
    }

    if( mETofGeom ) {
        delete mETofGeom;
        mETofGeom = nullptr;
    }

    return kStOk;
}


//---------------------------------------------------------------------------
Int_t
StETofMatchMaker::Make()
{
    LOG_DEBUG << "StETofMatchMaker::Make(): starting ..." << endm;

    mIsStEventIn = false;
    mIsMuDstIn   = false;

    mEvent = ( StEvent* ) GetInputDS( "StEvent" );

    if ( mEvent ) {
        LOG_DEBUG << "Make() - running on StEvent" << endm;

        mIsStEventIn = true;

        cleanUpTraits();
    }
    else {
        LOG_DEBUG << "Make(): no StEvent found" << endm;

        mMuDst = ( StMuDst* ) GetInputDS( "MuDst" );

        if( mMuDst ) {
            LOG_DEBUG << "Make() - running on MuDsts" << endm;

            mIsMuDstIn = true;

            fillIndexToPrimaryMap();

            cleanUpTraits();
        }
        else {
            LOG_DEBUG << "Make() - no StMuDst found" << endm;
            return kStOk;
        }
    }

    if( !mIsStEventIn && !mIsMuDstIn ) {
        LOG_WARN << "Make() - neither StEvent nor MuDst available ... bye-bye" << endm;
        return kStOk;
    }

    if( mDoQA ) {
        mHistograms.at( "eventCounter" )->Fill( 1 );
    }

    //.........................................................................
    // A. read data from StETofHit & build vector of candidate hits
    //    
    eTofHitVec detectorHitVec; 

    readETofDetectorHits( detectorHitVec );

    if( detectorHitVec.size() == 0 ) {
        LOG_INFO << "Make() -- event done ... bye-bye" << endm;

        return kStOk;
    }

    //.........................................................................
    // B. loop over global tracks & determine all track intersections with active eTof volumes
    //
    eTofHitVec intersectionVec;
    int nPrimaryWithIntersection = 0;

    float bFieldFromGeom = mETofGeom->getFieldZ( 100., 100., 0. );
    float bField = 0;
    if( mIsMuDstIn ) {
        bField = mMuDst->event()->runInfo().magneticField();
    }
    else {
        bField = mEvent->runInfo()->magneticField();
    }

    if( mUseHelixSwimmer && fabs( bFieldFromGeom - bField ) > 0.2 ) {
        LOG_WARN << "Wrong magnetc field bField = " << bField << " bFieldFromGeom = " << bFieldFromGeom << " --> check the magF input!" << endm;
    }

    findTrackIntersections( intersectionVec, nPrimaryWithIntersection );

    if( intersectionVec.size() == 0 ) {
        LOG_INFO << "Make() -- event done ... bye-bye" << endm;

        return kStOk;
    }

    mHistograms.at( "intersectionMult_etofMult" )->Fill( detectorHitVec.size(), intersectionVec.size() );

    //.........................................................................
    // C. match detector hits to track intersections
    //
    eTofHitVec matchCandVec;

    matchETofHits( detectorHitVec, intersectionVec, matchCandVec );

    if( matchCandVec.size() == 0 ) {
        LOG_INFO << "Make() -- event done ... bye-bye" << endm;

        return kStOk;
    }
    
    //.........................................................................
    // D. sort matchCand vector and deal with (discard) hits matched by multiple tracks
    //
    // define the vectors that store matchCandidates with a single / multiple tracks pointing to an eTof hit
    eTofHitVec           singleTrackMatchVec;
    vector< eTofHitVec > multiTrackMatchVec;

    sortSingleMultipleHits( matchCandVec, singleTrackMatchVec, multiTrackMatchVec );

    if( singleTrackMatchVec.size() == 0 ) {
        LOG_INFO << "Make() -- event done ... bye-bye" << endm;

        return kStOk;
    }

    //.........................................................................
    // E. sort singleTrackMatchVector for multiple hits associated to single tracks and determine the best match
    //
    eTofHitVec finalMatchVec;

    finalizeMatching( singleTrackMatchVec, finalMatchVec );

    if( finalMatchVec.size() == 0 ) {
        LOG_INFO << "Make() -- event done ... bye-bye" << endm;

        return kStOk;
    }
    else{
        LOG_INFO << "Make() -- number of found matches of eTOF hits with tracks: " << finalMatchVec.size() << endm;        
    }

    //.........................................................................
    // F. fill ETofPidTraits for global and primary tracks and assign associated track to hits
    //
    fillPidTraits( finalMatchVec );

    //.........................................................................
    // G. calculate pid variables for primary tracks and update PidTraits
    //
    int nPrimaryWithPid = 0;

    calculatePidVariables( finalMatchVec, nPrimaryWithPid );

    mHistograms.at( "primaryIntersect_validMatch" )->Fill( nPrimaryWithIntersection, nPrimaryWithPid );

    //.........................................................................
    // H. fill QA histograms
    //
    fillQaHistograms( finalMatchVec );

    fillSlewHistograms( finalMatchVec );


    LOG_INFO << "Make() -- event done ... bye-bye" << endm;

    return kStOk;
}


void StETofMatchMaker::fillIndexToPrimaryMap()
{
    // clear and fill index2Primary map
    mIndex2Primary.clear();

    for( int i = 0; i < mMuDst->array( muPrimary )->GetEntries(); i++ ) {
        StMuTrack* pTrack = ( StMuTrack* ) mMuDst->array( muPrimary )->UncheckedAt( i );
        if( !pTrack ) {
            continue;
        }
        int index2Global = pTrack->index2Global();
        if( index2Global < 0 ) {
            continue;
        }
        mIndex2Primary[ index2Global ] = i;
    }
}


void StETofMatchMaker::cleanUpTraits()
{
    // StEvent processing
    if( mIsStEventIn ) {
        StSPtrVecTrackNode& nodes = mEvent->trackNodes();
        size_t nNodes = nodes.size();
        for( size_t iNode = 0; iNode < nNodes; iNode++ ) {
            StGlobalTrack* gTrack = dynamic_cast< StGlobalTrack* > ( nodes[ iNode ]->track( global ) );
            if( !gTrack ) continue;

            //clean up any association done before
            StSPtrVecTrackPidTraits& traits = gTrack->pidTraits();
            for( auto it = traits.begin(); it != traits.end(); it++ ) {
                if( ( *it )->detector() == kETofId ) {

                    if( mDebug ) {
                        StETofPidTraits* etofTraits = ( StETofPidTraits* ) *it;

                        LOG_INFO << "cleanUpTraits() - etof traits on global track " << iNode << " already exist" << endm;
                        LOG_INFO << "matchFlag: " << etofTraits->matchFlag() << "  localX: " << etofTraits->localX() << "  localY: " << etofTraits->localY();
                        LOG_INFO << "  tof: " << etofTraits->timeOfFlight() << "  pathlength: " << etofTraits->pathLength() << "  beta: " << etofTraits->beta() << endm;

                        if( etofTraits->etofHit() ) {
                            LOG_INFO << "time: " << etofTraits->etofHit()->time() << endm;
                        }
                    }

                    traits.erase( it );

                    if( mDebug ) {
                        LOG_INFO << " ... erased" << endm;
                    }

                    break;
                }
            }

            StPrimaryTrack* pTrack = dynamic_cast< StPrimaryTrack* > ( gTrack->node()->track( primary ) );
            if( pTrack ) {
                //clean up any association done before
                StSPtrVecTrackPidTraits& ptraits = pTrack->pidTraits();
                for( auto it = ptraits.begin(); it != ptraits.end(); it++ ) {
                    if( ( *it )->detector() == kETofId ) {

                        if( mDebug ) {
                            StETofPidTraits* etofTraits = ( StETofPidTraits* ) *it;
                            
                            LOG_INFO << "cleanUpTraits() - etof traits on primary track corresponding to node " << iNode << " already exist" << endm;
                            LOG_INFO << "matchFlag: " << etofTraits->matchFlag() << "  localX: " << etofTraits->localX() << "  localY: " << etofTraits->localY();
                            LOG_INFO << "  tof: " << etofTraits->timeOfFlight() << "  pathlength: " << etofTraits->pathLength() << "  beta: " << etofTraits->beta() << endm;

                            if( etofTraits->etofHit() ) {
                                LOG_INFO << "time: " << etofTraits->etofHit()->time() << endm;
                            }
                        }

                        ptraits.erase( it );

                        if( mDebug ) {
                            LOG_INFO << " ... erased" << endm;
                        }

                        break;
                    }
                }
            }

        }

        size_t nHits = mEvent->etofCollection()->etofHits().size();
        for( size_t i=0; i<nHits; i++ ) {
            StETofHit* aHit = mEvent->etofCollection()->etofHits().at( i );
            if( !aHit ) continue;
            aHit->setAssociatedTrack( nullptr );
        }

    }
    else { // MuDst processing
        size_t nNodes = mMuDst->numberOfGlobalTracks();
        for( size_t iNode=0; iNode<nNodes; iNode++ ) {
            StMuTrack* track = mMuDst->globalTracks( iNode );
            if( !track ) continue;
            if( track->index2ETofHit() < 0 ) continue;

            if( mDebug ) {
                StMuETofPidTraits etofTraits = track->etofPidTraits();

                LOG_INFO << "cleanUpTraits() - etof traits on global track " << iNode << " already exist" << endm;
                LOG_INFO << "matchFlag: " << etofTraits.matchFlag() << "  localX: " << etofTraits.localX() << "  localY: " << etofTraits.localY();
                LOG_INFO << "  tof: " << etofTraits.timeOfFlight() << "  pathlength: " << etofTraits.pathLength() << "  beta: " << etofTraits.beta() << endm;

                if( mMuDst->etofHit( track->index2ETofHit() ) ) {
                    LOG_INFO << "time: " << mMuDst->etofHit( track->index2ETofHit() )->time() << endm;
                }
            }

            //clean up any association done before
            StMuETofPidTraits pidETof;
            track->setETofPidTraits( pidETof );
            track->setIndex2ETofHit( -1 );

            if( mDebug ) {
                LOG_INFO << " ... erased" << endm;
            }

            int pIndex = -1;
            auto it = mIndex2Primary.find( iNode );
            if( it != mIndex2Primary.end() ) {
                pIndex = it->second;
            }
            if( pIndex >= 0 ) {
                StMuTrack* pTrack = ( StMuTrack* ) mMuDst->array( muPrimary )->UncheckedAt( pIndex );
                if( pTrack ) {

                    if( mDebug ) {
                        StMuETofPidTraits etofTraits = pTrack->etofPidTraits();

                        LOG_INFO << "cleanUpTraits() - etof traits on primary track " << pIndex << " already exist" << endm;
                        LOG_INFO << "matchFlag: " << etofTraits.matchFlag() << "  localX: " << etofTraits.localX() << "  localY: " << etofTraits.localY();
                        LOG_INFO << "  tof: " << etofTraits.timeOfFlight() << "  pathlength: " << etofTraits.pathLength() << "  beta: " << etofTraits.beta() << endm;

                        if( mMuDst->etofHit( pTrack->index2ETofHit() ) ) {
                            LOG_INFO << "time: " << mMuDst->etofHit( pTrack->index2ETofHit() )->time() << endm;
                        }
                    }

                    //clean up any association done before
                    pTrack->setETofPidTraits( pidETof );
                    pTrack->setIndex2ETofHit( -1 );

                    if( mDebug ) {
                        LOG_INFO << " ... erased" << endm;
                    }
                }
            }
        }

        size_t nHits = mMuDst->numberOfETofHit();
        for( size_t i=0; i<nHits; i++ ) {
            StMuETofHit* aHit = mMuDst->etofHit( i );
            if( !aHit ) continue;
            aHit->setIndex2Primary( -1 );
            aHit->setIndex2Global( -1 );
            aHit->setAssociatedTrackId( -1 );
        }
    }
}



//.........................................................................
// A. read data from StETofHit & build vector of candidate hits
//  
//---------------------------------------------------------------------------
void
StETofMatchMaker::readETofDetectorHits( eTofHitVec& detectorHitVec )
{
    size_t nHits = 0;
    
    // event selection ... only continue with events that have eTOF hits
    if( mIsStEventIn ) {
        if( !mEvent->etofCollection() ) {
            LOG_INFO << "readETofDetectorHits() - no etof collection --> nothing to do ... bye-bye" << endm;
            return;
        }
        if( !mEvent->etofCollection()->hitsPresent() ) {
            LOG_INFO << "readETofDetectorHits() - no etof hits present --> nothing to do ... bye-bye" << endm;
            return;
        }
        
        nHits = mEvent->etofCollection()->etofHits().size();
        LOG_INFO << " number of ETOF hits: " << nHits << endm;
    }
    else if( mIsMuDstIn ) {
        if( !mMuDst->etofArray( muETofHit ) ) {
            LOG_WARN << "readETofDetectorHits() - no digi array --> nothing to do ... bye-bye" << endm;
            return;
        }

        if( !mMuDst->numberOfETofHit() ) {
            LOG_WARN << "readETofDetectorHits() - no hits present --> nothing to do ... bye-bye" << endm;
            return;
        }

        nHits = mMuDst->numberOfETofHit();
        LOG_INFO << " number of ETOF hits: " << nHits << endm;
    }

    if( mDoQA ) {
        mHistograms.at( "eventCounter" )->Fill( 2 );
    }

    // fill hits into the detectorHitVec structure
    // (depending on if StEvent or MuDst is used as input)
    if( mIsStEventIn ) {
        for( size_t i=0; i<nHits; i++ ) {
            StETofHit* aHit = mEvent->etofCollection()->etofHits().at( i );

            if( !aHit ) {
                continue;
            }

            StructETofHit detectorHit;

            detectorHit.sector         = aHit->sector();
            /*
            // ---------------------------------------------------
            // rotate hits by 60 degree to avaluate random matches
            int sec = rotateHit( aHit->sector(), 2 );
            detectorHit.sector = sec;
            // ---------------------------------------------------
            */
            detectorHit.plane          = aHit->zPlane();
            detectorHit.counter        = aHit->counter();
            detectorHit.hitTime        = aHit->time();
            detectorHit.localX         = aHit->localX();
            detectorHit.localY         = aHit->localY();
            detectorHit.tot            = aHit->totalTot();
            detectorHit.clusterSize    = aHit->clusterSize();
            detectorHit.index2ETofHit  = i;

            detectorHitVec.push_back( detectorHit );
        }        
    }
    else {
        for( size_t i=0; i<nHits; i++ ) {
            StMuETofHit* aHit = mMuDst->etofHit( i );

            if( !aHit ) {
                continue;
            }

            StructETofHit detectorHit;

            detectorHit.sector         = aHit->sector();
            /*
            // ---------------------------------------------------
            // rotate hits by 60 degree to avaluate random matches
            int sec = rotateHit( aHit->sector(), 2 );
            detectorHit.sector = sec;
            // ---------------------------------------------------
            */
            detectorHit.plane          = aHit->zPlane();
            detectorHit.counter        = aHit->counter();
            detectorHit.hitTime        = aHit->time();
            detectorHit.localX         = aHit->localX();
            detectorHit.localY         = aHit->localY();
            detectorHit.tot            = aHit->totalTot();
            detectorHit.clusterSize    = aHit->clusterSize();
            detectorHit.index2ETofHit  = i;

            detectorHitVec.push_back( detectorHit );
        }
    }


    // fill the hits in the structre with more information e.g. global position
    // (independent from used input file format )
    for( auto& hit : detectorHitVec ) {
        double xl[ 3 ] = { hit.localX, hit.localY, 0 };

        int moduleId  = mETofGeom->calcModuleIndex( hit.sector, hit.plane );
        int counterId = hit.counter - 1;
        double xg[ 3 ];

        mETofGeom->hitLocal2Master( moduleId, counterId, xl, xg );

        StThreeVectorF globalPos( xg[ 0 ], xg[ 1 ], xg[ 2 ] );

        hit.globalPos = globalPos;

        hit.strip = ( ( StETofGeomCounter* ) mETofGeom->findETofNode( moduleId, counterId ) )->findStrip( xl );

        if( mDebug ) {
            LOG_DEBUG << "hit( " << hit.index2ETofHit << " ) at sector: " << hit.sector;
            LOG_DEBUG << " zPlane: " << hit.plane << "  counter: " << hit.counter;
            LOG_DEBUG << " with local (X, Y): (" << xl[ 0 ] << ", " << xl[ 1 ] << ")" << endm; 
            LOG_DEBUG << "global (X, Y, Z): " << xg[ 0 ] << ", " << xg[ 1 ] << ", " << xg[ 2 ] << endm;
            LOG_DEBUG << " strip: " << hit.strip << endm;
        }

        // some more histogramming for QA 
        float hit_eta = globalPos.pseudoRapidity();
        float hit_phi = globalPos.phi();

        if ( hit_phi < 0. ) hit_phi += 2. * M_PI;

        LOG_DEBUG << "global (eta, phi): " << hit_eta << ", " << hit_phi << endm;

        if( fabs( hit.localY ) < eTofConst::stripLength / 2. * 1.5 ) {
            mHistograms.at( "eTofHits_globalXY" )->Fill( globalPos.x(), globalPos.y() );
        }

        if( mDoQA ) {
            if( fabs( hit.localY ) < eTofConst::stripLength / 2. * 1.5 ) {
                mHistograms.at( "eTofHits_phi_eta"  )->Fill( hit_phi, hit_eta );
            }

            if( hit.sector == 18 || hit.sector == 24 ) {
                mHistograms.at( "eTofHits_globalYZ" )->Fill( globalPos.y(), globalPos.z() );
            }

            std::string histName_hit_localXY  = "eTofHits_localXY_s"  + std::to_string( hit.sector ) + "m" + std::to_string( hit.plane ) + "c" + std::to_string( hit.counter );
            std::string histName_hit_globalXY = "eTofHits_globalXY_s" + std::to_string( hit.sector ) + "m" + std::to_string( hit.plane ) + "c" + std::to_string( hit.counter );
            std::string histName_hit_eta_phi  = "eTofHits_phi_eta_s"  + std::to_string( hit.sector ) + "m" + std::to_string( hit.plane ) + "c" + std::to_string( hit.counter );

            mHistograms.at( histName_hit_localXY  )->Fill( hit.localX, hit.localY );
            mHistograms.at( histName_hit_globalXY )->Fill( hit.globalPos.x(), hit.globalPos.y() );
            mHistograms.at( histName_hit_eta_phi  )->Fill( hit_phi, hit_eta );
        }
    }

    if( mDoQA ) {
        mHistograms.at( "detectorHitMult" )->Fill( detectorHitVec.size() );
        if( detectorHitVec.size() > 0 ) {
            mHistograms.at( "eventCounter" )->Fill( 3 );
        }
    }
}


//.........................................................................
// B. loop over global tracks & determine all track intersections with active eTof volumes
//
//---------------------------------------------------------------------------
void
StETofMatchMaker::findTrackIntersections( eTofHitVec& intersectionVec, int& nPrimaryWithIntersection )
{
    size_t nNodes = 0;
    size_t nPrimaryCrossings = 0;

    // StEvent processing
    if( mIsStEventIn ) {
        StSPtrVecTrackNode& nodes = mEvent->trackNodes();

        nNodes = nodes.size();

        if( mDebug ) {
            LOG_INFO << "# tracks in the event: " << nNodes << endm;
        }

        for( size_t iNode = 0; iNode < nNodes; iNode++ ) {
            if( mDebug ) {
                LOG_DEBUG << "track : " << iNode << endm;
            }
            StGlobalTrack* theTrack = dynamic_cast< StGlobalTrack* > ( nodes[ iNode ]->track( global ) );

            if( !validTrack( theTrack ) ) continue;

            bool isPrimary = false;
            StPrimaryTrack* pTrack = dynamic_cast< StPrimaryTrack* > ( theTrack->node()->track( primary ) );
            if( pTrack ) {
                isPrimary = true;
                if( mDebug ) {
                    LOG_DEBUG << "track : " << iNode << " is a primary track" << endm;
                }
            }

            StPhysicalHelixD theHelix = mOuterTrackGeometry ? theTrack->outerGeometry()->helix() : theTrack->geometry()->helix();

            int nCrossings = 0;

            extrapolateTrackToETof( intersectionVec, theHelix, iNode, nCrossings, isPrimary );


            if( isPrimary ) {
                nPrimaryCrossings += nCrossings;
                if( nCrossings > 0 ) {
                    nPrimaryWithIntersection++;
                 
                    if( mDoQA ) {
                        int charge = pTrack->geometry()->charge();
                        float pMom = pTrack->geometry()->momentum().mag();

                        mHistograms.at( "intersection_primaryTrack_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        if( charge > 0 ) mHistograms.at( "intersection_primaryTrackpos_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        else             mHistograms.at( "intersection_primaryTrackneg_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );

                        if( pMom > 1 ) {
                            mHistograms.at( "intersection_primaryTrackMom0_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            if( charge > 0 ) mHistograms.at( "intersection_primaryTrackMom0pos_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            else             mHistograms.at( "intersection_primaryTrackMom0neg_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        }
                        else if( pMom > 0.5 ) {
                            mHistograms.at( "intersection_primaryTrackMom1_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            if( charge > 0 ) mHistograms.at( "intersection_primaryTrackMom1pos_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            else             mHistograms.at( "intersection_primaryTrackMom1neg_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        }
                        else {
                            mHistograms.at( "intersection_primaryTrackMom2_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            if( charge > 0 ) mHistograms.at( "intersection_primaryTrackMom2pos_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            else             mHistograms.at( "intersection_primaryTrackMom2neg_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        }
                    }
                }
            }

            if( mDoQA && nCrossings > 0 ) {
                ETofTrack eTrack( theTrack );

                mHistograms.at( "intersection_track_pt_eta"  )->Fill( eTrack.pt,  eTrack.eta );
                mHistograms.at( "intersection_track_pt_phi"  )->Fill( eTrack.pt,  eTrack.phi );
                mHistograms.at( "intersection_track_phi_eta" )->Fill( eTrack.phi, eTrack.eta );

                mHistograms.at( "intersection_track_nHitsTpc" )->Fill( eTrack.nFtPts );

                if( eTrack.dEdx       > -999. ) mHistograms.at( "intersection_track_mom_dEdx"     )->Fill( eTrack.pt * cosh( eTrack.eta ), eTrack.dEdx );
                if( eTrack.nSigmaPion > -999. ) mHistograms.at( "intersection_track_mom_nsigmaPi" )->Fill( eTrack.pt * cosh( eTrack.eta ), eTrack.nSigmaPion );
            }
        }
    } // end of StEvent processing
    else { // MuDst processing
        nNodes = mMuDst->numberOfGlobalTracks();

        if( mDebug ) {
            LOG_INFO << "# tracks in the event: " << nNodes << endm;
        }

        for( size_t iNode = 0; iNode < nNodes; iNode++ ) {
            if( mDebug ) {
                LOG_DEBUG << "track : " << iNode << endm;
            }

            StMuTrack* theTrack = mMuDst->globalTracks( iNode );

            if( !validTrack( theTrack ) ) continue;

            bool isPrimary= false;

            int pIndex = -1;
            auto it = mIndex2Primary.find( iNode );
            if( it != mIndex2Primary.end() ) {
                pIndex = it->second;
            }
            if( pIndex >= 0 ) {
                isPrimary = true;
                if( mDebug ) {
                    LOG_DEBUG << "track : " << iNode << " is a primary track" << endm;
                }
            }

            StPhysicalHelixD theHelix = mOuterTrackGeometry ? theTrack->outerHelix() : theTrack->helix();

            int nCrossings = 0;

            extrapolateTrackToETof( intersectionVec, theHelix, iNode, nCrossings, isPrimary );

            if( isPrimary ) {
                nPrimaryCrossings += nCrossings;
                if( nCrossings > 0 ) {
                    nPrimaryWithIntersection++;

                    if( mDoQA ) {
                        int charge = theTrack->primaryTrack()->charge();
                        float pMom = theTrack->primaryTrack()->momentum().mag();

                        mHistograms.at( "intersection_primaryTrack_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        if( charge > 0 ) mHistograms.at( "intersection_primaryTrackpos_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        else             mHistograms.at( "intersection_primaryTrackneg_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );

                        if( pMom > 1 ) {
                            mHistograms.at( "intersection_primaryTrackMom0_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            if( charge > 0 ) mHistograms.at( "intersection_primaryTrackMom0pos_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            else             mHistograms.at( "intersection_primaryTrackMom0neg_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        }
                        else if( pMom > 0.5 ) {
                            mHistograms.at( "intersection_primaryTrackMom1_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            if( charge > 0 ) mHistograms.at( "intersection_primaryTrackMom1pos_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            else             mHistograms.at( "intersection_primaryTrackMom1neg_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        }
                        else {
                            mHistograms.at( "intersection_primaryTrackMom2_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            if( charge > 0 ) mHistograms.at( "intersection_primaryTrackMom2pos_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                            else             mHistograms.at( "intersection_primaryTrackMom2neg_globalXY" )->Fill( intersectionVec.back().globalPos.x() , intersectionVec.back().globalPos.y() );
                        }
                    }
                }
            }

            if( mDoQA && nCrossings > 0 ) {
                ETofTrack eTrack( theTrack );

                mHistograms.at( "intersection_track_pt_eta"  )->Fill( eTrack.pt,  eTrack.eta );
                mHistograms.at( "intersection_track_pt_phi"  )->Fill( eTrack.pt,  eTrack.phi );
                mHistograms.at( "intersection_track_phi_eta" )->Fill( eTrack.phi, eTrack.eta );

                mHistograms.at( "intersection_track_nHitsTpc" )->Fill( eTrack.nFtPts );

                if( eTrack.dEdx       >  0.   ) mHistograms.at( "intersection_track_mom_dEdx"     )->Fill( eTrack.pt * cosh( eTrack.eta ), eTrack.dEdx );
                if( eTrack.nSigmaPion > -999. ) mHistograms.at( "intersection_track_mom_nsigmaPi" )->Fill( eTrack.pt * cosh( eTrack.eta ), eTrack.nSigmaPion );
            }
        }
    }   // end of MuDst processing

    LOG_INFO << "# tracks in the event: " << nNodes << "  ... out of which " << intersectionVec.size() << " intersect with eTOF" << endm;

    if( mDoQA ) {
        mHistograms.at( "intersectionMult" )->Fill( intersectionVec.size() );
        mHistograms.at( "intersectionMult_primary" )->Fill( nPrimaryCrossings );

        if( intersectionVec.size() > 0 ) {
            mHistograms.at( "eventCounter" )->Fill( 4 );
        }
    }
}


//---
/// StTrack-based Track selection for initial extrapolation to ETOF
bool
StETofMatchMaker::validTrack( const StTrack* track )
{
    if( track ) {
        return validTrack( ETofTrack( track ) );
    }
    else {
        return false;
    }
}

//---
/// StMuTrack-based Track selection for initial extrapolation to ETOF
bool
StETofMatchMaker::validTrack( const StMuTrack* track )
{
    if( track ) {
        return validTrack( ETofTrack( track ) );
    }
    else {
        return false;
    }
}

//---
/// ETofTrack-based Track selection for initial extrapolation to ETOF
bool
StETofMatchMaker::validTrack( const ETofTrack& track )
{
    double ratioFitToPoss = 1. * track.nFtPts / track.nHitsPoss;

    if( mDoQA ) {
        mHistograms.at( "track_phi_eta" ) ->Fill( track.phi, track.eta );
        mHistograms.at( "track_phi_pt"  ) ->Fill( track.phi, track.pt  );
        mHistograms.at( "nHits" )         ->Fill( track.nFtPts );
        mHistograms.at( "track_pt_nHits" )->Fill( track.pt, track.nFtPts );
    }

    // kick out tracks that will not point to the eTOF
    // TODO: more carefull eta acceptance cut in the future (performance vs. efficientcy)
    if( track.eta > etofProjection::minEtaCut ) return false;

    if( mDoQA && track.eta > -1.65 && track.eta < -1.05 ) {
        mHistograms.at( "nHits_etofregion" )->Fill( track.nFtPts );
    }

    // kick out low quality tracks
    //if( track.flag <= flagMinCut )                  return false;
    //if( track.flag >= flagMaxCut )                  return false;
    if( track.nFtPts   < mTrackCuts.at( 0 ) )       return false;
    if( ratioFitToPoss < mTrackCuts.at( 1 ) )       return false;
    if( track.pt       < mTrackCuts.at( 2 ) )       return false;
    //if( track.mom      < mTrackCuts.at( 2 ) )       return false;

    if( mDebug ) {
        LOG_DEBUG << "valid track for extrapolation to eTOF with nHitsFit: " << track.nFtPts;
        LOG_DEBUG << " mom: " << track.mom << " pt: " << track.pt << "  phi: " << track.phi << "  eta: " << track.eta  << endm;
    }

    return true;
}

//---
void
StETofMatchMaker::extrapolateTrackToETof( eTofHitVec& intersectionVec, const StPhysicalHelixD& theHelix, const int& iNode, int& nCrossings, bool isPrimary )
{
    // first project helix to the middle eTof plane to get the sector(s) of possible intersections
    StThreeVectorD projection = mETofGeom->helixCrossPlane( theHelix, eTofConst::zplanes[ 1 ] );

    // get rid of tracks that do not project to the rough eta region of the eTof
    float projEta = projection.pseudoRapidity();

    if( projEta < etofProjection::maxEtaProjCut ) return;
    if( projEta > etofProjection::minEtaProjCut ) return;

    float projPhi = projection.phi();
    if ( projPhi < 0. ) projPhi += 2. * M_PI;

    if( mDoQA ) {
        // histogramming for QA
        mHistograms.at( "trackProj_globalXY" )->Fill( projection.x(), projection.y() );
        mHistograms.at( "trackProj_phi_eta"  )->Fill( projPhi, projEta ); 
    }

    vector< int > idVec;
    vector< StThreeVectorD > globalVec;
    vector< StThreeVectorD > localVec;
    vector< double > thetaVec;
    vector< double > pathLenVec;

    // look for intersections of the extrapolated helix with the active eTof volumes
    mETofGeom->helixCrossCounter( theHelix, idVec, globalVec, localVec, thetaVec, pathLenVec );

    nCrossings = idVec.size();

    // loop backwards through the vectors, so that one can remove the
    // current entry if it doesn't pass e.g. a local Y cut in the future
    for( int i = nCrossings-1; i >= 0; i-- ) {
        if( mDebug ) {
            LOG_INFO << " ------> crossing with volume index: " << idVec.at( i ) << endm;
            LOG_INFO << "track intersection: " << globalVec.at( i ).x() << ", " << globalVec.at( i ).y() << ", " << globalVec.at( i ).z() << endm;
            LOG_INFO << "local coordinates: "  << localVec.at( i ).x() << ", " << localVec.at( i ).y() << ", " << localVec.at( i ).z() << endm;
        }

        StructETofHit intersect;

        mETofGeom->decodeVolumeIndex( idVec.at( i ), intersect.sector, intersect.plane, intersect.counter, intersect.strip );

        intersect.localX     = localVec.at( i ).x();
        intersect.localY     = localVec.at( i ).y();
        intersect.globalPos  = globalVec.at( i );
        intersect.trackId    = iNode;
        intersect.theta      = thetaVec.at( i );
        intersect.pathLength = pathLenVec.at( i );
        intersect.isPrimary  = isPrimary;

        // fill intersection into storage vector
        intersectionVec.push_back( intersect );


        if( mDoQA ) {
            float intersectPhi = intersect.globalPos.phi();
            if( intersectPhi < 0. ) intersectPhi += 2. * M_PI;

            mHistograms.at( "intersection_globalXY" )->Fill( intersect.globalPos.x(), intersect.globalPos.y() );
            mHistograms.at( "intersection_phi_eta"  )->Fill( intersectPhi, intersect.globalPos.pseudoRapidity() );
        }
    }

    if( mDoQA ) {
        mHistograms.at( "intersection_perTrack" )->Fill( idVec.size() );
    }
}


//.........................................................................
// C. match detector hits to track intersections
//
//---------------------------------------------------------------------------
void
StETofMatchMaker::matchETofHits( eTofHitVec& detectorHitVec, eTofHitVec& intersectionVec, eTofHitVec& matchCandVec )
{
    for( auto detHitIter = detectorHitVec.begin(); detHitIter != detectorHitVec.end(); detHitIter++ ) {
        for( auto interIter = intersectionVec.begin(); interIter != intersectionVec.end(); interIter++ ) {
            
            //fill correlation histograms
            int sector = detHitIter->sector;
            int plane  = detHitIter->plane;

            int moduleId = ( sector - eTofConst::sectorStart ) * eTofConst::nPlanes + plane - eTofConst::zPlaneStart; 

            if( mDoQA ) {
                int detHitIndex   = ( detHitIter->counter - eTofConst::counterStart ) * eTofConst::nStrips + detHitIter->strip - eTofConst::stripStart;
                int intersecIndex = ( interIter->counter  - eTofConst::counterStart ) * eTofConst::nStrips + interIter->strip  - eTofConst::stripStart;

                mHistograms.at( "detHitvsInter_strip_s"   + std::to_string( sector ) + "m" + std::to_string( plane ) )->Fill( detHitIndex, intersecIndex );

                mHistograms.at( "detHitvsInter_X" )->Fill( detHitIter->globalPos.x(), interIter->globalPos.x() );
                mHistograms.at( "detHitvsInter_Y" )->Fill( detHitIter->globalPos.y(), interIter->globalPos.y() );

                mHistograms.at( "moduleIndex_deltaX" )->Fill( moduleId, detHitIter->localX - interIter->localX );
                mHistograms.at( "moduleIndex_deltaY" )->Fill( moduleId, detHitIter->localY - interIter->localY );

                mHistograms.at( "detHitvsInter_localX" )->Fill(detHitIter->localX, interIter->localX ); 
                mHistograms.at( "detHitvsInter_localY" )->Fill(detHitIter->localY, interIter->localY ); 
            }


            // store match candidates
            bool isMatch = false;

            // deltaX, deltaY (subtract offset until alignment is done properly)
            float deltaX = detHitIter->localX  - interIter->localX;
            float deltaY = detHitIter->localY  - interIter->localY;

            int counterIndex = ( detHitIter->sector  - eTofConst::sectorStart  ) * eTofConst::nPlanes * eTofConst::nCounters
                             + ( detHitIter->plane   - eTofConst::zPlaneStart  ) * eTofConst::nCounters
                             + ( detHitIter->counter - eTofConst::counterStart );

            deltaX -= etofProjection::deltaXoffset[ counterIndex ];
            deltaY -= etofProjection::deltaYoffset[ counterIndex ];

            if( detHitIter->sector == interIter->sector ) {
                if( detHitIter->plane == interIter->plane ) {
                    if( detHitIter->counter == interIter->counter ) {
                        if( fabs( deltaX ) < mMatchDistX ) {
                            if( fabs( deltaY ) < mMatchDistY ) {
                                isMatch = true;
                            }
                        }
                    }
                }
            }

            if( isMatch ) {
                StructETofHit matchCand;

                matchCand.sector        = detHitIter->sector;
                matchCand.plane         = detHitIter->plane;
                matchCand.counter       = detHitIter->counter;
                matchCand.strip         = detHitIter->strip;
                matchCand.localX        = detHitIter->localX;
                matchCand.localY        = detHitIter->localY;
                matchCand.hitTime       = detHitIter->hitTime;
                matchCand.tot           = detHitIter->tot;
                matchCand.clusterSize   = detHitIter->clusterSize;
                matchCand.index2ETofHit = detHitIter->index2ETofHit;

                matchCand.globalPos     = interIter->globalPos;
                matchCand.trackId       = interIter->trackId;
                matchCand.theta         = interIter->theta;
                matchCand.pathLength    = interIter->pathLength;
                matchCand.isPrimary     = interIter->isPrimary;

                matchCand.matchFlag = 0;
                matchCand.deltaX    = deltaX;
                matchCand.deltaY    = deltaY;

                matchCand.tof        = -999.;
                matchCand.beta       = -999.;


                matchCandVec.push_back( matchCand );

                if( mDebug ) {
                    LOG_INFO << " * * FOUND MATCH CAND : " << matchCand.sector << "  " << matchCand.plane << "  " << matchCand.counter;
                    LOG_INFO << " with (deltaX, deltaY) = (" << deltaX << ", " << deltaY << ")" << endm;
                }

                mHistograms.at( "matchCand_globalXY" )->Fill( matchCand.globalPos.x(), matchCand.globalPos.y() );

                if( mDoQA ) {
                    float matchCandPhi = matchCand.globalPos.phi();
                    if ( matchCandPhi < 0. ) matchCandPhi += 2. * M_PI;

                    mHistograms.at( "matchCand_phi_eta"  )->Fill( matchCandPhi, matchCand.globalPos.pseudoRapidity() );

                    mHistograms.at( "matchCand_deltaX" )->Fill( deltaX ); 
                    mHistograms.at( "matchCand_deltaY" )->Fill( deltaY );

                    std::string histName_deltaX = "matchCand_deltaX_s" + std::to_string( matchCand.sector ) + "m" + std::to_string( matchCand.plane ) + "c" + std::to_string( matchCand.counter );
                    std::string histName_deltaY = "matchCand_deltaY_s" + std::to_string( matchCand.sector ) + "m" + std::to_string( matchCand.plane ) + "c" + std::to_string( matchCand.counter );

                    mHistograms.at( histName_deltaX )->Fill( deltaX );
                    mHistograms.at( histName_deltaY )->Fill( deltaY );


                    // get track corresponding to the trackId stored for matchCand
                    if( mIsStEventIn ) {
                        StSPtrVecTrackNode& nodes = mEvent->trackNodes();
                        StGlobalTrack *matchCandTrack = dynamic_cast< StGlobalTrack* > ( nodes[ matchCand.trackId ]->track( global ) );
                        if( matchCandTrack ) {
                            int nFitPts = matchCandTrack->fitTraits().numberOfFitPoints( kTpcId );

                            mHistograms.at( "matchCand_deltaX_nHitsTpc" )->Fill( nFitPts, deltaX );
                            mHistograms.at( "matchCand_deltaY_nHitsTpc" )->Fill( nFitPts, deltaY );
                        }
                    }
                    else {
                        StMuTrack* matchCandTrack = mMuDst->globalTracks( matchCand.trackId );
                        if( matchCandTrack ) {
                            int nFitPts = matchCandTrack->nHitsFit( kTpcId );

                            mHistograms.at( "matchCand_deltaX_nHitsTpc" )->Fill( nFitPts, deltaX );
                            mHistograms.at( "matchCand_deltaY_nHitsTpc" )->Fill( nFitPts, deltaY );
                        }
                    }
                }
            }

        }
    }

    if( mDoQA ) {
        mHistograms.at( "matchCandMult" )->Fill( matchCandVec.size() );

        if( matchCandVec.size() > 0 ) {
            mHistograms.at( "eventCounter" )->Fill( 5 );
        }
    }
}


//.........................................................................
// D. sort matchCand vector and deal with (discard) hits matched by multiple tracks
//
//---------------------------------------------------------------------------
void
StETofMatchMaker::sortSingleMultipleHits( eTofHitVec& matchCandVec, eTofHitVec& singleTrackMatchVec, std::vector< eTofHitVec >& multiTrackMatchVec )
{
    int nSingleTrackMatch = 0;
    int nMultiTrackMatch  = 0;


    // define temporary vectors for iterating through matchCandVec
    eTofHitVec tempVec   = matchCandVec;
    eTofHitVec erasedVec = tempVec;

    while( tempVec.size() != 0 ) {
        int nTracks = 0;

        // define temporary storage vectors
        eTofHitVec candVec;

        eTofHitVecIter tempIter   = tempVec.begin();
        eTofHitVecIter erasedIter = erasedVec.begin();

        while( erasedIter != erasedVec.end() ) {

            if( tempIter->index2ETofHit == erasedIter->index2ETofHit ) {
                nTracks++;
                candVec.push_back( *erasedIter );

                erasedVec.erase( erasedIter );
                erasedIter--;
            }
            erasedIter++;
        }
        // reset tempVec for next iteration in the while-loop
        tempVec = erasedVec;

        StructETofHit cand = candVec.front();

        if( mDebug ) {
            LOG_INFO << "matchCand at sector " << cand.sector << " plane " << cand.plane << " counter " << cand.counter << " with local (X,Y) = (" << cand.localX << "," << cand.localY << ")";
            LOG_INFO << " has " << nTracks << " track(s) pointing to it:" << endm;
        }

        if( nTracks == 1 ) {
            nSingleTrackMatch++;
            singleTrackMatchVec.push_back( cand );

            if( mDebug ) {
                LOG_INFO << "track id: " << cand.trackId << " and matching distance " << cand.deltaX << "  " << cand.deltaY << endm;
            }
        }   
        else if( nTracks > 1 ) {
            // for multiple tracks pointing to the same detector hit: either discard or take "most likely" / "best" match candidate
            // for now: take match with smallest deltaR
            nMultiTrackMatch++;

            multiTrackMatchVec.push_back( candVec );

            float bestResidual = 999.;
            int   bestIndex    = -1;

            for( size_t i=0; i<candVec.size(); i++ ) {
                if( mDebug ) {
                    LOG_INFO << "track id: " << candVec.at( i ).trackId << " and matching distance " << candVec.at( i ).deltaX << "  " << candVec.at( i ).deltaY << endm;
                }
                float residual = pow( candVec.at( i ).deltaX, 2 ) + pow( candVec.at( i ).deltaY, 2 );

                if( residual < bestResidual ) {
                    bestResidual = residual;
                    bestIndex    = i;
                }
            }

            if( bestIndex > -1 ) {
                singleTrackMatchVec.push_back( candVec.at( bestIndex ) );
                if( mDebug ) {
                    LOG_INFO << "best candidate has track id: " << candVec.at( bestIndex ).trackId << endm;
                }
            }

            if( mDebug ) {
                for( const auto& c: candVec ) {
                    LOG_INFO << "track id: " << c.trackId << " and matching distance " << c.deltaX << "  " << c.deltaY << endm;
                }
            }
        }

        if( mDoQA ) {
            mHistograms.at( "trackMatchMultPerDetectorHit" )->Fill( nTracks );
        }
    }

    LOG_INFO << "nSingleTrackMatch: " << nSingleTrackMatch << " ... nMultiTrackMatch: " << nMultiTrackMatch << endm;

    if( mDoQA ) {
        mHistograms.at( "singleTrackMatchMult" )->Fill( singleTrackMatchVec.size() );

        if( singleTrackMatchVec.size() > 0 ) {
            mHistograms.at( "eventCounter" )->Fill( 6 );
        }
    }
}


//.........................................................................
// E. sort singleTrackMatchVector for multiple hits associated to single tracks and determine the best match
//    also set the match flag ( TODO: set it more sophisticated when dealing with multi-hits )
//
//---------------------------------------------------------------------------
void
StETofMatchMaker::finalizeMatching( eTofHitVec& singleTrackMatchVec, eTofHitVec& finalMatchVec )
{
    // setup temporary vectors for iterating trough singleTrackMatchVec
    eTofHitVec tempVec   = singleTrackMatchVec;
    eTofHitVec erasedVec = tempVec;

    while( tempVec.size() != 0 ) {
        int nHits = 0;

        // define temporary storage vectors
        eTofHitVec candVec;

        eTofHitVecIter tempIter   = tempVec.begin();
        eTofHitVecIter erasedIter = erasedVec.begin();

        while( erasedIter != erasedVec.end() ) {

            if( tempIter->trackId == erasedIter->trackId ) {
                nHits++;
                candVec.push_back( *erasedIter );

                erasedVec.erase( erasedIter );
                erasedIter--;
            }
            erasedIter++;
        }
        // reset tempVec for next iteration in the while-loop
        tempVec = erasedVec;

        // for a track matched to only one eTof hit -> copy match candidate to finalMatchVec
        if( nHits == 1 ) {
            candVec.front().matchFlag = 1;
            finalMatchVec.push_back( candVec.front() );

            if( mDebug ) {
                LOG_INFO << "one-to-one match (track id: " << candVec.front().trackId << ") -> pushed into final match vector" << endm;
            }
        }
        // for a track matched to many eTof hits -> only take the most likely / best match
        else if ( nHits > 1 ) {
            if( mDebug ) {
                LOG_INFO << "one-to-many match -> needs further treatment" << endm;
            }

            // for the moment sort on distance between track intersection and eTof detector hit:
            double bestMatchDist = pow( candVec.front().deltaX, 2 ) + pow( candVec.front().deltaY, 2 );
            StructETofHit bestCand = candVec.front();

            bool isOverlapHit = false;
            
            for( const auto& c: candVec ) {
                double candMatchDist = pow( c.deltaX, 2 ) + pow( c.deltaY, 2 );
                
                if( mDebug ) {
                    LOG_INFO << "candidate match distance: " << sqrt( candMatchDist ) << endm;
                }

                if( candMatchDist < bestMatchDist ) {
                    bestMatchDist = candMatchDist;
                    bestCand = c;

                    if( mDebug ) {
                        LOG_INFO << " --> new best match candidate" << endm;
                    }
                }

                if( ( bestCand.sector * 100 + bestCand.plane * 10 + bestCand.counter ) != ( c.sector * 100 + c.plane * 10 + c.counter ) ) {
                    isOverlapHit = true;
                }
            }
            
            if( isOverlapHit ) {
                bestCand.matchFlag = 3;

                if( mDoQA ) {
                    mHistograms.at( "overlapHit_globalXY" )->Fill( bestCand.globalPos.x(), bestCand.globalPos.y() );
                }
            }
            else {
                bestCand.matchFlag = 2;
            }

            finalMatchVec.push_back( bestCand );

            if( mDebug ) {
                LOG_INFO << "one-to-many match resolved (track id: " << bestCand.trackId << ", cand deltaX: " << bestCand.deltaX << ") -> pushed into final match vector" << endm;
            }
        }

        if( mDoQA ) {
            mHistograms.at( "hitMultPerTrack" )->Fill( nHits );
        }
    }

    if( mDoQA ) {
        if( mIsStEventIn ) {  
            StSPtrVecTrackNode& nodes = mEvent->trackNodes();
            for( const auto& v: finalMatchVec ) {
                StGlobalTrack* track = dynamic_cast< StGlobalTrack* > ( nodes[ v.trackId ]->track( global ) );

                mHistograms.at( "finalMatch_pt" )->Fill( track->geometry()->momentum().perp() );
            }
        }
        else {
            for( const auto& v: finalMatchVec ) {
                StMuTrack* track = mMuDst->globalTracks( v.trackId );

                mHistograms.at( "finalMatch_pt" )->Fill( track->momentum().perp() );
            }
        }

        mHistograms.at( "finalMatchMult" )->Fill( finalMatchVec.size() );

        if( finalMatchVec.size() > 0 ) {
            mHistograms.at( "eventCounter" )->Fill( 7 );
        }
    }
}


//.........................................................................
// F. fill ETofPidTraits for global and primary tracks and assign associated track to hits
//
//---------------------------------------------------------------------------
void
StETofMatchMaker::fillPidTraits( eTofHitVec& finalMatchVec )
{
    size_t nFinalMatchesGlobal  = 0;
    size_t nFinalMatchesPrimary = 0;

    if( mIsStEventIn ) {
        //get track & hit collection
        StSPtrVecETofHit& etofHits = mEvent->etofCollection()->etofHits();
        StSPtrVecTrackNode& nodes = mEvent->trackNodes();

        for( const auto& match : finalMatchVec ) {
            // get global track
            StGlobalTrack* globalTrack = dynamic_cast< StGlobalTrack* > ( nodes[ match.trackId ]->track( global ) );
            if( !globalTrack ) {
                LOG_WARN << "fillPidTraits() - global track does not exist" << endm;
                continue;
            }

            // fill association in ETofCollection
            StETofHit* hit = etofHits[ match.index2ETofHit ];
            if( !hit ) {
                LOG_WARN << "fillPidTraits() - eTof hit does not exist" << endm;
                continue;   
            }
            hit->setAssociatedTrack( globalTrack );

            nFinalMatchesGlobal++;

            // fill the matching data into StETofPidTraits
            StETofPidTraits* pidTraits = new StETofPidTraits();

            pidTraits->setETofHit( hit );
            pidTraits->setMatchFlag(  match.matchFlag  );
            pidTraits->setLocalX(     match.localX     );
            pidTraits->setLocalY(     match.localY     );
            pidTraits->setThetaLocal( match.theta      );
            pidTraits->setDeltaX(     match.deltaX     );
            pidTraits->setDeltaY(     match.deltaY     );
            pidTraits->setPosition(   match.globalPos  );

            pidTraits->setPathLength( match.pathLength );

            pidTraits->setTimeOfFlight( -999. );
            pidTraits->setBeta(         -999. );

            globalTrack->addPidTraits( pidTraits );

            // get primary track if exists and fill matching data into StETofPidTraits
            StPrimaryTrack *pTrack = dynamic_cast< StPrimaryTrack* > ( nodes[ match.trackId ]->track( primary ) );
            if( pTrack ) {
                nFinalMatchesPrimary++;

                if( mDoQA ) {
                    mHistograms.at( "finalMatchPrimary_globalXY" )->Fill( match.globalPos.x(), match.globalPos.y() );

                    float mom = pTrack->geometry()->momentum().mag();
                    if( mom > 1 ) {
                        mHistograms.at( "finalMatchPrimaryMom0_globalXY" )->Fill( match.globalPos.x() , match.globalPos.y() );
                    }
                    else if( mom > 0.5 ) {
                        mHistograms.at( "finalMatchPrimaryMom1_globalXY" )->Fill( match.globalPos.x() , match.globalPos.y() );
                    }
                    else {
                        mHistograms.at( "finalMatchPrimaryMom2_globalXY" )->Fill( match.globalPos.x() , match.globalPos.y() );
                    }
                }

                StETofPidTraits* ppidTraits = new StETofPidTraits();

                ppidTraits->setETofHit( hit );
                ppidTraits->setMatchFlag(  match.matchFlag  );
                ppidTraits->setLocalX(     match.localX     );
                ppidTraits->setLocalY(     match.localY     );
                ppidTraits->setThetaLocal( match.theta      );
                ppidTraits->setDeltaX(     match.deltaX     );
                ppidTraits->setDeltaY(     match.deltaY     );
                ppidTraits->setPosition(   match.globalPos  );

                ppidTraits->setPathLength( match.pathLength );

                ppidTraits->setTimeOfFlight( -999. );
                ppidTraits->setBeta(         -999. );

                pTrack->addPidTraits( ppidTraits );
            }
        }
    }
    else {
        for( const auto& match : finalMatchVec ) {
            // get global track
            StMuTrack* gTrack = mMuDst->globalTracks( match.trackId );
            if( !gTrack ) {
                LOG_WARN << "fillPidTraits() - global track does not exist" << endm;
                continue;
            }

            // fill association to hit
            StMuETofHit* hit = mMuDst->etofHit( match.index2ETofHit );
            if( !hit ) {
                LOG_WARN << "fillPidTraits() - eTof hit does not exist" << endm;
                continue;
            }
            hit->setAssociatedTrackId( gTrack->id() );
            hit->setIndex2Global( match.trackId );
            gTrack->setIndex2ETofHit( match.index2ETofHit );

            nFinalMatchesGlobal++;

            // fill the matching data into StETofPidTraits
            StMuETofPidTraits pidTraits = gTrack->etofPidTraits();
            pidTraits.setMatchFlag(  match.matchFlag  );
            pidTraits.setLocalX(     match.localX     );
            pidTraits.setLocalY(     match.localY     );
            pidTraits.setThetaLocal( match.theta      );
            pidTraits.setDeltaX(     match.deltaX     );
            pidTraits.setDeltaY(     match.deltaY     );
            pidTraits.setPosition(   match.globalPos  );

            pidTraits.setPathLength( match.pathLength );

            pidTraits.setTimeOfFlight( -999. );
            pidTraits.setBeta(         -999. );

            gTrack->setETofPidTraits ( pidTraits );

            int pIndex = -1;
            auto it = mIndex2Primary.find( match.trackId );
            if( it != mIndex2Primary.end() ) {
                pIndex = it->second;
            }
            if( pIndex < 0 ) continue;

            StMuTrack* pTrack = ( StMuTrack* ) mMuDst->array( muPrimary )->UncheckedAt( pIndex );
            if( pTrack ) {
                nFinalMatchesPrimary++;

                if( mDoQA ) {
                    mHistograms.at( "finalMatchPrimary_globalXY" )->Fill( match.globalPos.x(), match.globalPos.y() );

                    float mom = pTrack->momentum().mag();
                    if( mom > 1 ) {
                        mHistograms.at( "finalMatchPrimaryMom0_globalXY" )->Fill( match.globalPos.x() , match.globalPos.y() );
                    }
                    else if( mom > 0.5 ) {
                        mHistograms.at( "finalMatchPrimaryMom1_globalXY" )->Fill( match.globalPos.x() , match.globalPos.y() );
                    }
                    else {
                        mHistograms.at( "finalMatchPrimaryMom2_globalXY" )->Fill( match.globalPos.x() , match.globalPos.y() );
                    }
                }


                hit->setIndex2Primary( pIndex );
                pTrack->setIndex2ETofHit( match.index2ETofHit );

                StMuETofPidTraits ppidTraits = pTrack->etofPidTraits();
                ppidTraits.setMatchFlag(  match.matchFlag  );
                ppidTraits.setLocalX(     match.localX     );
                ppidTraits.setLocalY(     match.localY     );
                ppidTraits.setThetaLocal( match.theta      );
                ppidTraits.setDeltaX(     match.deltaX     );
                ppidTraits.setDeltaY(     match.deltaY     );
                ppidTraits.setPosition(   match.globalPos  );

                ppidTraits.setPathLength( match.pathLength );

                ppidTraits.setTimeOfFlight( -999. );
                ppidTraits.setBeta(         -999. );

                pTrack->setETofPidTraits ( ppidTraits );
            }
        }
    }

    if( mDoQA ) {
        mHistograms.at( "finalMatchMultGlobal"  )->Fill( nFinalMatchesGlobal  );
        mHistograms.at( "finalMatchMultPrimary" )->Fill( nFinalMatchesPrimary );
    }
}


//.........................................................................
// G. calculate pid variables for primary tracks and update PidTraits
//
//---------------------------------------------------------------------------
void
StETofMatchMaker::calculatePidVariables( eTofHitVec& finalMatchVec, int& nPrimaryWithPid )
{
    // get start time
    double tstart = startTime( finalMatchVec );
        
    if( fabs( tstart + 9999. ) < 1.e-5 ) {
        LOG_WARN << "calculatePidVariables() -- no valid start time available ... skip filling pidTraits with more information" << endm;
        nPrimaryWithPid = -1;
        return;
    }

    if( mIsStEventIn ) { // StEvent processing ...
        // assign pathlength, time-of-flight, beta ... to the match candidates
        for( auto& matchCand : finalMatchVec ) {

            StETofHit* aHit = dynamic_cast< StETofHit* > ( mEvent->etofCollection()->etofHits().at( matchCand.index2ETofHit ) );
            if( !aHit ) {
                LOG_ERROR << "calculatePidVariables() - no hit" << endm;
                continue;
            }

            // global track
            StTrack* gTrack = aHit->associatedTrack();
            if( !gTrack ) {
                LOG_ERROR << "calculatePidVariables() - no associated track" << endm;
                continue;
            }


            StSPtrVecTrackPidTraits& traits = gTrack->pidTraits();
            StETofPidTraits* pidTraits = 0;
            for( size_t i=0; i<traits.size(); i++ ) {
                if( traits[ i ]->detector() == kETofId ) {
                  pidTraits = dynamic_cast< StETofPidTraits* > ( traits[ i ] );
                  break;
                }
            }
            if( !pidTraits ) continue;


            double tof = timeOfFlight( tstart, aHit->time() );

            // set time-of-flight
            matchCand.tof = tof;

            pidTraits->setTimeOfFlight( tof );


            // primary track
            StTrack* pTrack = gTrack->node()->track( primary );
            StETofPidTraits* ppidTraits = 0;
            if( pTrack ) {
                StSPtrVecTrackPidTraits& ptraits = pTrack->pidTraits();
                for( size_t i=0; i<ptraits.size(); i++ ) {
                    if( ptraits[ i ]->detector() == kETofId ) {
                      ppidTraits = dynamic_cast< StETofPidTraits* > ( ptraits[ i ] );
                      break;
                    }
                }
                if( ppidTraits ) {
                    ppidTraits->setTimeOfFlight( tof );
                }
            }

            if( mDebug ) {
                LOG_INFO << "calculatePidVariables() - time-of-flight assigned to pid traits: " << tof << " ..." << endm;
            }

            // pid calculation if the track is a primary track
            // switch indicating to calculate PID or not
            bool doPID = false;

            double pathLength = matchCand.pathLength;


            if( !pTrack ) {
                if( mDebug ) {
                    LOG_INFO << "calculatePidVariables() - the associated track is not a primary one. Skip PID calculation! " << endm;
                }
            }
            else {
                const StVertex* pVtx = pTrack->vertex();
                if( !pVtx ) {
                    if( mDebug ) {
                        LOG_INFO << "calculatePidVariables() - the associated track is not coming from any vertex. Skip PID calculation! " << endm;
                    }
                }
                else {
                    StThreeVectorD   vtxPos   = pVtx->position();
                    StPhysicalHelixD theHelix = mOuterTrackGeometry ? gTrack->outerGeometry()->helix() : gTrack->geometry()->helix();

                    pathLength -= theHelix.pathLength( vtxPos );

                    doPID = true;
                }
            }
            
            if( !doPID ) continue;

            // set pathlength
            matchCand.pathLength = pathLength;


            double beta = pathLength / ( tof * nanosecond * c_light );
            
            // set beta
            matchCand.beta = beta;

            if( mDebug ) {
                LOG_INFO << "calculatePidVariables() - pathlength: " << pathLength << "  time-of-flight: " << tof << " and beta: " << beta << " are set" << endm;
            }

            if( mDoQA ) {
                mHistograms.at( "matchCand_timeOfFlight"            )->Fill( tof );
                mHistograms.at( "matchCand_timeOfFlight_pathLength" )->Fill( tof, pathLength );
            }
            mHistograms.at( "matchCand_timeOfFlight_pathLength_zoom" )->Fill( tof, pathLength );

            pidTraits->setPathLength( pathLength );
            pidTraits->setBeta( beta );

            if( ppidTraits ) {
                ppidTraits->setPathLength( pathLength );
                ppidTraits->setBeta( beta );

                nPrimaryWithPid++;
            }

            if( mDebug ) {
                LOG_INFO << "calculatePidVariables() - pathlength and beta assigned to pid traits ..." << endm;
            }
        }
    }
    else { // MuDst processing ...
        // assign pathlength, time-of-flight, beta ... to the match candidates
        for( auto& matchCand : finalMatchVec ) {

            StMuETofHit* aHit = mMuDst->etofHit( matchCand.index2ETofHit );
            if( !aHit ) {
                LOG_ERROR << "calculatePidVariables() - no hit" << endm;
                continue;
            }

            // global track
            StMuTrack* gTrack = aHit->globalTrack();
            if( !gTrack ) {
                LOG_ERROR << "calculatePidVariables() - no associated track" << endm;
                continue;
            }

            StMuETofPidTraits pidTraits = gTrack->etofPidTraits();


            double tof = timeOfFlight( tstart, aHit->time() );

            // set time-of-flight
            matchCand.tof = tof;


            pidTraits.setTimeOfFlight( tof );
            gTrack->setETofPidTraits( pidTraits );

            // primary track
            StMuTrack* pTrack = aHit->primaryTrack();
            StMuETofPidTraits ppidTraits;
            if( pTrack ) {
                ppidTraits = pTrack->etofPidTraits();

                ppidTraits.setTimeOfFlight( tof );
                pTrack->setETofPidTraits( ppidTraits );
            }

            if( mDebug ) {
                LOG_INFO << "calculatePidVariables() - time-of-flight assigned to pid traits: " << tof << " ..." << endm;
            }

            // pid calculation if the track is a primary track
            // switch indicating to calculate PID or not
            bool doPID = false;

            double pathLength = matchCand.pathLength;

            if( !pTrack ) {
                if( mDebug ) {
                    LOG_INFO << "calculatePidVariables() - the associated track is not a primary one. Skip PID calculation!" << endm;
                }
            }
            else {
                StMuPrimaryVertex* pVtx = mMuDst->primaryVertex( pTrack->vertexIndex() );
                if( !pVtx ) {
                    if( mDebug ) {
                        LOG_INFO << "calculatePidVariables() - the associated track is not coming from any vertex. Skip PID calculation!" << endm;
                    }
                }
                else {
                    StThreeVectorD   vtxPos   = pVtx->position();
                    StPhysicalHelixD theHelix = mOuterTrackGeometry ? gTrack->outerHelix() : gTrack->helix();

                    pathLength -= theHelix.pathLength( vtxPos );

                    doPID = true;
                }
            }

            if( !doPID ) continue;

            // set pathlength
            matchCand.pathLength = pathLength;


            double beta =  pathLength / ( tof * nanosecond * c_light );
            
            // set beta
            matchCand.beta = beta;

            if( mDebug ) {
                LOG_INFO << "calculatePidVariables() - pathlength: " << pathLength << "  time-of-flight: " << tof << " and beta: " << beta << " are set" << endm;
            }

            if( mDoQA ) {
                mHistograms.at( "matchCand_timeOfFlight"            )->Fill( tof );
                mHistograms.at( "matchCand_timeOfFlight_pathLength" )->Fill( tof, pathLength );
            }
            mHistograms.at( "matchCand_timeOfFlight_pathLength_zoom" )->Fill( tof, pathLength );

            pidTraits.setPathLength( pathLength );
            pidTraits.setBeta( beta );
            gTrack->setETofPidTraits( pidTraits );

            if( pTrack ) {
                ppidTraits.setPathLength( pathLength );
                ppidTraits.setBeta( beta ); 
                pTrack->setETofPidTraits( ppidTraits );

                nPrimaryWithPid++;
            }

            if( mDebug ) {
                LOG_INFO << "calculatePidVariables() - pathlength and beta assigned to pid traits ..." << endm;
            }
        }
    }
}


//---------------------------------------------------------------------------
// get the start time -- from bTOF header (default)
double
StETofMatchMaker::startTimeBTof()
{
    if( mDebug ) {
        LOG_INFO << "startTimeBTof(): -- loading start time from bTOF header" << endm;
    }

    StBTofHeader* btofHeader = nullptr; 

    if( mIsStEventIn ) {
        StBTofCollection* btofCollection = ( StBTofCollection* ) mEvent->btofCollection();

        if ( btofCollection ) {
            btofHeader = btofCollection->tofHeader();
        }
        else {
            LOG_DEBUG << "startTimeBTof(): -- no StBTofCollection found by getTstart" << endm;
            return -9999.;
        }
    }
    else if( mIsMuDstIn ) {
        btofHeader = mMuDst->btofHeader();
    }

    if( !btofHeader ) {
        LOG_DEBUG << "startTimeBTof(): -- no bTOF header --> no start time avaiable" << endm;
        return -9999.;
    }

    double tstart = btofHeader->tStart();

    if( !isfinite( tstart ) ) {
        LOG_DEBUG << "startTimeBTof(): -- from bTOF header is NaN" << endm;
        return -9999.;
    }

    if( tstart != -9999. ) {
        tstart = fmod( tstart, eTofConst::bTofClockCycle );
        if( tstart < 0 ) tstart += eTofConst::bTofClockCycle;
    }

    if( mDebug ) {
        LOG_DEBUG << "startTimeBTof():  --  start time: " << tstart << endm;
    }

    return tstart;
}

//---------------------------------------------------------------------------
// get the start time -- only from eTOF matches
double
StETofMatchMaker::startTimeETof( const eTofHitVec& finalMatchVec, unsigned int& nCand_etofT0 )
{
    if( mDebug ) {
        LOG_INFO << "startTimeETof(): -- calculating start time from eTOF matches" << endm;
    }
    std::vector< double > t0_cand;
    double t0_sum = 0.;

    nCand_etofT0 = 0;

    for( auto& match : finalMatchVec ) {
        if( !match.isPrimary ) continue;

        if( sqrt( pow( match.deltaX, 2 ) + pow( match.deltaY, 2 ) ) > etofProjection::deltaRcut ) continue;

        double pathLength = match.pathLength;

        double momentum;
        int    charge;

        double nsigmaPi = -999.;
        double nsigmaK  = -999.;
        double nsigmaP  = -999.;

        if( mIsStEventIn ) { // StEvent processing ...
            StSPtrVecTrackNode& nodes = mEvent->trackNodes();

            // global track
            StGlobalTrack* gTrack = dynamic_cast< StGlobalTrack* > ( nodes[ match.trackId ]->track( global ) );
            if( !gTrack ) {
                continue;
            }

            // primary track
            StPrimaryTrack* pTrack = dynamic_cast< StPrimaryTrack* > ( gTrack->node()->track( primary ) );
            if( !pTrack ) {
                continue;
            }


            momentum = pTrack->geometry()->momentum().mag();
            charge   = pTrack->geometry()->charge();

            static StTpcDedxPidAlgorithm PidAlgorithm;
            static StPionPlus* Pion   = StPionPlus::instance();
            static StKaonPlus* Kaon   = StKaonPlus::instance();
            static StProton*   Proton = StProton::instance();
            const  StParticleDefinition* pd = pTrack->pidTraits( PidAlgorithm );

            if( pd && PidAlgorithm.traits() ) {
                nsigmaPi = PidAlgorithm.numberOfSigma( Pion   );
                nsigmaK  = PidAlgorithm.numberOfSigma( Kaon   );
                nsigmaP  = PidAlgorithm.numberOfSigma( Proton );
            }

            StPhysicalHelixD theHelix = mOuterTrackGeometry ? gTrack->outerGeometry()->helix() : gTrack->geometry()->helix();
            pathLength -= theHelix.pathLength( pTrack->vertex()->position() );
        }
        else { // MuDst processing ...
            StMuETofHit* aHit = mMuDst->etofHit( match.index2ETofHit );
            if( !aHit ) continue;

            // global track
            StMuTrack* gTrack = aHit->globalTrack();
            if( !gTrack ) continue;

            // primary track
            StMuTrack* pTrack = aHit->primaryTrack();
            if( !pTrack ) continue;


            momentum = pTrack->momentum().mag();
            charge   = pTrack->charge();

            nsigmaPi = pTrack->nSigmaPion();
            nsigmaK  = pTrack->nSigmaKaon();
            nsigmaP  = pTrack->nSigmaProton();

            StPhysicalHelixD theHelix = mOuterTrackGeometry ? gTrack->outerHelix() : gTrack->helix();
            pathLength -= theHelix.pathLength( mMuDst->primaryVertex( pTrack->vertexIndex() )->position() );
        }

        if( momentum < 0.2 ) continue;

        double tofExpect;

        // identyfy paricles via dE/dx
        if( momentum < 0.6 && fabs( nsigmaPi ) < 2. ) {
            tofExpect = expectedTimeOfFlight( pathLength, momentum, pion_minus_mass_c2 );
            if( mDoQA ) {
                LOG_INFO << "startTimeETof(): -- using a pion as start time candidate" << endm;
            }
        }
        else if( momentum < 0.5 && fabs( nsigmaK ) < 1. ) {
            tofExpect = expectedTimeOfFlight( pathLength, momentum, kaon_minus_mass_c2 );
            if( mDoQA ) {
                LOG_INFO << "startTimeETof(): -- using a kaon as start time candidate" << endm;
            }
        }
        else if( momentum < 0.9 && charge == 1 && fabs( nsigmaP ) < 2. ) {
            tofExpect = expectedTimeOfFlight( pathLength, momentum, proton_mass_c2 );
            if( mDoQA ) {
                LOG_INFO << "startTimeETof(): -- using a proton as start time candidate" << endm;
            }
        }
        else{
            continue;
        }

        t0_cand.push_back( match.hitTime - tofExpect );
        t0_sum += t0_cand.back();

        if( mDebug ) {
            LOG_INFO << match.hitTime  << "  " << tofExpect << "  " << t0_cand.back() << endm;
        }
    }

    nCand_etofT0 = t0_cand.size();

    if( nCand_etofT0 == 0 ) {
        return -9999.;
    }
    else if( nCand_etofT0 > 1 ) { // remove hits too far from others
        for( unsigned int i=0; i < nCand_etofT0; i++ ) {
            
            double t0_diff = t0_cand.at( i ) - ( t0_sum - t0_cand.at( i ) ) / ( nCand_etofT0 - 1 );
            
            if( fabs( t0_diff ) > 5.0 ) {
                t0_sum -= t0_cand.at( i );
                nCand_etofT0--;
            }
        }
    }
    
    if( nCand_etofT0 < 2 ) {
        return -9999.;
    }


    double tStart = fmod( t0_sum / nCand_etofT0, eTofConst::bTofClockCycle );
    if( tStart < 0 ) tStart += eTofConst::bTofClockCycle;

    return tStart;
}


//---------------------------------------------------------------------------
// distance on a circle
double
StETofMatchMaker::moduloDist( const double& dist, const double& mod ) {
    return dist - mod * round( dist / mod );
}

//---------------------------------------------------------------------------
// calculate hybrid start time with eTOF and bTOF information
double
StETofMatchMaker::startTime( const eTofHitVec& finalMatchVec ) {
    // for simulation tstart == 0
    if( mIsSim ) {
        return 0.;
    }

    double tstartBTof = startTimeBTof();

    if( mUseOnlyBTofHeaderStartTime ) {
        return tstartBTof;
    }


    unsigned int nCand_etofT0 = 0;
    double tstartETof = startTimeETof( finalMatchVec, nCand_etofT0 );

    double t0Diff = moduloDist( tstartETof - tstartBTof, eTofConst::bTofClockCycle );

    LOG_INFO << "startTime(): -- start time comparison: bTOF " << tstartBTof << "  eTOF " << tstartETof;
    LOG_INFO << " nCand_etofT0: " << nCand_etofT0 << "  difference: " << t0Diff << " mT0corr: " << mT0corr << endm;

    if( mDoQA && tstartBTof != -9999. && tstartETof != -9999. ) {
        mHistograms.at( "startTimeDiff"       )->Fill( t0Diff );
        mHistograms.at( "startTimeDiff_nCand" )->Fill( t0Diff, nCand_etofT0 );
    }

    //---------------------------------
    // calculate T0 corr for hybrid start time
    if( tstartBTof != -9999. && tstartETof != -9999. && nCand_etofT0 > etofHybridT0::minCand ) {
        if( mT0corr != 0 && fabs( moduloDist( t0Diff - mT0corr, eTofConst::bTofClockCycle ) ) > etofHybridT0::diffToClear ) {
            mT0switch++;
            if( mDebug ) {
                LOG_INFO << "mT0switch: " << mT0switch << endm;
            }

            if( mT0switch > etofHybridT0::nSwitch ) {
                mT0corrVec.clear();
                mT0corrVec.push_back( t0Diff );
                mT0switch = 0;
                mT0corr   = 0.;
                if( mDebug ) {
                    LOG_INFO << "startTime(): -- cleared T0 correction vector since the start time (eTof <-> bTOF) seems to have changed" << endm;
                }
            }
        }
        else {
            mT0corrVec.push_back( t0Diff );
            mT0switch = 0;
        }

        if( ( mT0corr != 0. || mT0corrVec.size() >= etofHybridT0::nMin ) && mT0corrVec.size() % etofHybridT0::nUpdate == 0 ) {
            std::sort( mT0corrVec.begin(), mT0corrVec.end() );

            if( mDebug ) {
                for( const auto& v : mT0corrVec ) {
                    LOG_DEBUG << "startTime(): --  T0corrVec: " << v << endm;
                }
            }


            // preselection if mT0corr is not yet filled with information:
            // reject outliers far away from the median
            if( mT0corr == 0. ) {
                mT0corr = mT0corrVec.at( mT0corrVec.size() / 2 );
                for( auto it = mT0corrVec.begin(); it != mT0corrVec.end(); it++ ) {
                    if( fabs( *it - mT0corr ) > 0.5 ) {
                        mT0corrVec.erase( it-- );
                    }
                }
                mT0corr = mT0corrVec.at( mT0corrVec.size() / 2 );

                //reject outliers
                for( auto it = mT0corrVec.begin(); it != mT0corrVec.end(); it++ ) {
                    if( fabs( *it - mT0corr ) > 0.2 ) {
                        mT0corrVec.erase( it-- );
                    }
                }
                mT0corr = mT0corrVec.at( mT0corrVec.size() / 2 );
            }


            //reject outliers
            for( auto it = mT0corrVec.begin(); it != mT0corrVec.end(); it++ ) {
                if( fabs( *it - mT0corr ) > 0.5 ) {
                    mT0corrVec.erase( it-- );
                }
            }

            if( mDebug ) {
                for( const auto& v : mT0corrVec ) {
                    LOG_DEBUG << "startTime(): -- cleaned T0corrVec: " << v << endm;
                }
            }


            // reject some fraction of the early and late matches
            int low  = floor( mT0corrVec.size() * etofHybridT0::lowCut  );
            int high = floor( mT0corrVec.size() * etofHybridT0::highCut );

            double temp = 0.;
            int nAccept = 0;
            for( int i = low; i < high; i++ ) {
                if( mDebug ) {
                    LOG_DEBUG << "startTime(): --  T0corrVec: " << mT0corrVec.at( i ) << endm;
                }

                temp += mT0corrVec.at( i );
                nAccept++;
            }

            if( nAccept >= 5 ) {
                mT0corr = ( temp / nAccept ) - etofHybridT0::biasOffset;
                mNupdatesT0++;

                if( mDoQA ) {
                    LOG_INFO << "startTime(): -- hybrid T0 correction between eTOF & bTOF (update " << mNupdatesT0 << "): " << mT0corr << "  (" << nAccept << ")" << endm;

                    mHistograms.at( "startTime_T0corr" )->SetBinContent( mNupdatesT0 , mT0corr );
                }
            }
        }
    }
    //---------------------------------

    double tstart;

    if( mT0corr != 0. && tstartBTof != -9999. ) {
        tstart = fmod( moduloDist( tstartBTof + mT0corr, eTofConst::bTofClockCycle ), eTofConst::bTofClockCycle );
        if( tstart < 0. ) tstart += eTofConst::bTofClockCycle;

        if( mDoQA ) {
            LOG_INFO << "startTime(): -- returning hybrid start time (BTof/VPD T0: " << tstartBTof << " T0 corr: " << mT0corr << "): " << tstart << endm;
        }
    }
    else if( tstartETof != -9999. && nCand_etofT0 > etofHybridT0::minCand ) {
        tstart = tstartETof;
        if( mDoQA ) {
            LOG_INFO << "startTime(): -- returning eTOF-only start time: " << tstart << endm;
        }
    }
    else {
        tstart = tstartBTof;

        if( mDoQA ) {
            LOG_INFO << "startTime(): -- returning bTOF/VPD start time: " << tstart << endm;
        }
    }

    return tstart;
}


//---------------------------------------------------------------------------
// calculate measured time of flight
double
StETofMatchMaker::timeOfFlight( const double& startTime, const double& stopTime )
{
    return moduloDist( stopTime - startTime, eTofConst::bTofClockCycle );
}


//---------------------------------------------------------------------------
// calculate expected time of flight of particle hypothesis
double
StETofMatchMaker::expectedTimeOfFlight( const double& pathLength, const double& momentum, const double& mass )
{
    double inverseBeta = sqrt( 1 + mass * mass / ( momentum * momentum ) );

    return pathLength * centimeter * ( 1. / c_light ) * inverseBeta / nanosecond;
}


//.........................................................................
// H. fill QA histograms
//
//---------------------------------------------------------------------------
void
StETofMatchMaker::fillQaHistograms( eTofHitVec& finalMatchVec )
{
    vector< int > nPidMatches( 36 );

    for( auto& matchCand : finalMatchVec ) {

        int charge;
        float mom;

        float dEdx = -999.;
        float nSigmaPion = -999;

        if( mIsStEventIn ) {

            StSPtrVecTrackNode& nodes = mEvent->trackNodes();

            StGlobalTrack* gTrack = dynamic_cast< StGlobalTrack* > ( nodes[ matchCand.trackId ]->track( global ) );
            if( !gTrack ) continue;

            StPrimaryTrack* pTrack = dynamic_cast< StPrimaryTrack* > ( gTrack->node()->track( primary ) );
            if( !pTrack ) continue;

            charge = pTrack->geometry()->charge();
            mom    = pTrack->geometry()->momentum().mag();

            static StTpcDedxPidAlgorithm PidAlgorithm;
            static StPionPlus* Pion         = StPionPlus::instance();
            const  StParticleDefinition* pd = pTrack->pidTraits( PidAlgorithm );

            if( pd && PidAlgorithm.traits() ) {
                dEdx       = PidAlgorithm.traits()->mean() * 1.e6;
                nSigmaPion = PidAlgorithm.numberOfSigma( Pion );
            }
        }
        else {

            StMuETofHit* aHit = mMuDst->etofHit( matchCand.index2ETofHit );
            if( !aHit ) continue;

            StMuTrack* pTrack = aHit->primaryTrack();
            if( !pTrack ) continue;

            charge = pTrack->charge();
            mom    = pTrack->momentum().mag();

            dEdx       = pTrack->dEdx() * 1.e6;
            nSigmaPion = pTrack->nSigmaPion();
        }

        int sign   = ( charge  < 0 ) ? -1 : ( charge > 0 );
        float beta = matchCand.beta;

        // skip events with no valid start time: beta of matchCand structure != -999.
        if( fabs( beta + 999. ) < 0.001  ) {
            if( mDoQA ) {
                LOG_WARN << "beta not set --> no start time available???" << endm;
            }
            continue;
        }

        // expected time of flight for mass hypothesis pion
        float tofpi = expectedTimeOfFlight( matchCand.pathLength, mom, pion_plus_mass_c2 );

        mHistograms.at( "matchCand_beta_signmom" )->Fill( sign * mom, 1. / beta );
        if( mom > 0.6 && mom < 1.5 ) {
            mHistograms.at( "matchCand_t0corr_1d" )->Fill( matchCand.tof - tofpi );
        }

        if( mDoQA ) {
            float tof        = matchCand.tof;
            float pathlength = matchCand.pathLength;
            float m2         = mom * mom * ( -1 + 1 / ( beta * beta ) );

            LOG_INFO << "momentum: " << mom << " ... beta: " << beta << " ... m^2: " << m2 << " ... dEdx: " << dEdx << endm;
            //LOG_DEBUG << "pathlength: " << pathlength << " ... time-of-flight: " << matchCand.tof << " ... mom: " << mom << " ... beta: " << beta << " ...  charge: " << charge << " ... m^2: " << m2 << " ... dEdx: " << dEdx << endm;

            if( fabs( tof+999. ) < 1.e-5 || fabs( pathlength+999. ) < 1.e-5 ) {
              LOG_INFO << "tof==0 or pathlength==0 ..." << endl;
              continue;
            }

            mHistograms.at( "matchCand_beta_mom"   )->Fill(        mom, 1. / beta );
            mHistograms.at( "matchCand_m2_mom"     )->Fill(        mom, m2 );
            mHistograms.at( "matchCand_m2_signmom" )->Fill( sign * mom, m2 );


            // plots per counter
            std::string histName_beta_mom = "matchCand_beta_mom_s" + std::to_string( matchCand.sector ) + "m" + std::to_string( matchCand.plane ) + "c" + std::to_string( matchCand.counter );
            mHistograms.at( histName_beta_mom )->Fill( mom, 1. / beta );

            std::string histName_t0corr_mom  = "matchCand_t0corr_mom_s" + std::to_string( matchCand.sector ) + "m" + std::to_string( matchCand.plane ) + "c" + std::to_string( matchCand.counter );
            mHistograms.at( histName_t0corr_mom )->Fill( mom, tof - tofpi );

            std::string histName_t0corr_mom_zoom = "matchCand_t0corr_mom_zoom_s" + std::to_string( matchCand.sector ) + "m" + std::to_string( matchCand.plane ) + "c" + std::to_string( matchCand.counter );
            mHistograms.at( histName_t0corr_mom_zoom )->Fill( mom, tof - tofpi );

            if( nSigmaPion < 1.5 ) {
                std::string histName_t0corr_strip = "matchCand_t0corr_strip_s" + std::to_string( matchCand.sector ) + "m" + std::to_string( matchCand.plane ) + "c" + std::to_string( matchCand.counter );
                mHistograms.at( histName_t0corr_strip )->Fill( matchCand.localX, tof - tofpi );
            }
            
            if( sqrt( pow( matchCand.deltaX, 2 ) + pow( matchCand.deltaY, 2 ) ) < etofProjection::deltaRcut ) {

                mHistograms.at( "matchCand_beta_mom_matchDistCut" )->Fill( mom, 1. / beta );
                mHistograms.at( "matchCand_m2_mom_matchDistCut"   )->Fill( mom, m2        );

                std::string histName_t0corr_mom_zoom_cut  = "matchCand_t0corr_mom_zoom_cut_s" + std::to_string( matchCand.sector ) + "m" + std::to_string( matchCand.plane ) + "c" + std::to_string( matchCand.counter );

                mHistograms.at( histName_t0corr_mom_zoom_cut )->Fill( mom, tof - tofpi );

                nPidMatches.at( ( matchCand.sector - 13 ) * 3 + matchCand.plane - 1 ) += 1;
            }

            if( fabs( mom - 1 ) < 0.1 && dEdx > 0 ) mHistograms.at( "matchCand_dEdx_beta_mom1gev" )->Fill( 1. / beta, dEdx );
            if( fabs( mom - 2 ) < 0.1 && dEdx > 0 ) mHistograms.at( "matchCand_dEdx_beta_mom2gev" )->Fill( 1. / beta, dEdx );
        }
    }

    if( mDoQA ) {
        for( size_t i=0; i<36; i++ ) {
            mHistograms.at( "matchCandMultPerSector_matchDistCut" )->Fill( i, nPidMatches.at( i ) );
        }
    }
}

void
StETofMatchMaker::fillSlewHistograms( eTofHitVec& finalMatchVec )
{
    if( !mDoQA || !mIsMuDstIn ) return;

    map< int, float > hitIndexToDeltaTmap;

    for( auto& matchCand : finalMatchVec ) {
        StMuETofHit* aHit = mMuDst->etofHit( matchCand.index2ETofHit );
        if( !aHit ) continue;

        StMuTrack* pTrack = aHit->primaryTrack();
        if( !pTrack ) continue;

        float mom        = pTrack->momentum().mag();
        float nSigmaPion = pTrack->nSigmaPion();

        if( fabs( nSigmaPion > 1.5 ) ) continue;


        // skip events with no valid start time: beta of matchCand structure != -999.
        if( fabs(  matchCand.beta + 999. ) < 0.001  ) {
            LOG_INFO << "beta not set --> no start time available???" << endm;
            continue;
        }

        float tof        = matchCand.tof;
        float pathlength = matchCand.pathLength;

        if( fabs( tof+999. ) < 1.e-5 || fabs( pathlength+999. ) < 1.e-5 ) {
            LOG_INFO << "tof==0 or pathlength==0 ..." << endl;
            continue;
        }

        LOG_DEBUG << "for slewing: momentum: " << mom << " ... nsigmaPi: " << nSigmaPion << " tof:" << tof << endm;

        // expected time of flight for mass hypothesis pion
        float tofpi = expectedTimeOfFlight( pathlength , mom, pion_plus_mass_c2 );
        hitIndexToDeltaTmap[ matchCand.index2ETofHit ] = tof - tofpi;
    }

    //find digis that belong to the matched hits
    size_t nDigis = mMuDst->numberOfETofDigi();
    for( size_t i=0; i<nDigis; i++ ) {
        StMuETofDigi* aDigi = mMuDst->etofDigi( i );
        if( !aDigi ) continue;

        if( hitIndexToDeltaTmap.count( aDigi->associatedHitId() ) ) {
            std::string histName_slewing  = "matchCand_slewing_digi_s" + std::to_string( aDigi->sector() ) + "m" + std::to_string( aDigi->zPlane() ) + "c" + std::to_string( aDigi->counter() );
            mHistograms.at( histName_slewing )->Fill( aDigi->calibTot(), hitIndexToDeltaTmap.at( aDigi->associatedHitId() ) );
        }
    }
}


//---------------------------------------------------------------------------
// returns the proper track geometry, based on a global user setting
StTrackGeometry*
StETofMatchMaker::trackGeometry( StTrack* track ) const
{
    // returns apropriate StTrackGeometry (standard or outerGeometry)
    if ( !track ) return nullptr;

    StTrackGeometry* thisTrackGeometry;

    if ( mOuterTrackGeometry ) {
        thisTrackGeometry = track->outerGeometry();
    }
    else {
        thisTrackGeometry = track->geometry();
    }

    return thisTrackGeometry;
}


//---------------------------------------------------------------------------
// setHistFileName uses the string argument from the chain being run to set
// the name of the output histogram file.
void
StETofMatchMaker::setHistFileName()
{
    string extension = ".etofMatch.root";

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


//---------------------------------------------------------------------------
void
StETofMatchMaker::bookHistograms()
{
    LOG_INFO << "bookHistograms" << endm;

    mHistograms[ "eTofHits_globalXY" ]         = new TH2F( "A_eTofHits_globalXY",         "global XY;x (cm);y (cm)",                                            400, -300., 300.,  400, -300., 300. );
    mHistograms[ "intersectionMult_etofMult" ] = new TH2F( "B_intersectionMult_etofMult", "multiplicity correlation;# eTOF hits;#track intersections;# events", 300,    0., 300.,  200,    0., 200. );
    mHistograms[ "matchCand_globalXY" ]        = new TH2F( "C_matchCand_globalXY",        "global XY;x (cm);y (cm)",                                            400, -300., 300.,  400, -300., 300. );
    mHistograms[ "matchCand_beta_signmom" ]    = new TH2F( "G_matchCand_beta_signmom" ,   "match candidate 1/beta vs. momentum;q/|q| * p (GeV/c);1/#beta",      400,   -4.,   6., 1000,   0.8,   2. );
    mHistograms[ "matchCand_timeOfFlight_pathLength_zoom" ] = new TH2F( "G_matchCand_timeOfFlight_pathLength", "match candidate pathlength vs. time of flight;ToF (ns);pathlength (cm)",  800, -25., 75., 800, 200., 600. );
    mHistograms[ "primaryIntersect_validMatch" ]            = new TH2F( "G_primary_Intersection_validMatch", "primary tracks at eTOF;# tracks with intersection;# tracks with valid PID", 200,  0., 200., 100,   0., 100. );
    mHistograms[ "matchCand_t0corr_1d"     ] = new TH1F( "H_matchCand_t0corr_1d", "measured tof - tof_{#pi};#Delta time (ns);counts", 3000, -15., 15. );
                    

    AddHist( mHistograms.at( "eTofHits_globalXY"         ) );
    AddHist( mHistograms.at( "intersectionMult_etofMult" ) );
    AddHist( mHistograms.at( "matchCand_globalXY"        ) );
    AddHist( mHistograms.at( "matchCand_beta_signmom"    ) );
    AddHist( mHistograms.at( "matchCand_timeOfFlight_pathLength_zoom" ) );
    AddHist( mHistograms.at( "primaryIntersect_validMatch" ) );
    AddHist( mHistograms.at( "matchCand_t0corr_1d" ) );

    if( mDoQA ) {
        // histograms to test sector & module numbering
        mHistograms[ "eTofSectors" ] = new TH2F( "QA_eTofSectors", "center of modules in global XY;x (cm);y (cm)", 100, -300, 300, 100, -300, 300 );
        mHistograms[ "eTofModules" ] = new TH2F( "QA_eTofModules", "center of modules in global XY;x (cm);y (cm)", 100, -300, 300, 100, -300, 300 );

        // histograms testing the magnetic field
        mHistograms2d[ "bfield_z" ] = new TH2F( "QA_bField_z", "magnetic field (z);z (cm);y (cm)", 350, -350., 0., 300, 0., 300. );


        // event-level QA
        mHistograms[ "eventCounter" ] = new TH1F( "QA_eventCounter","eventCounter;;# events", 10, 0.5, 10.5 );


        // ----------
        // step - A -
        // ----------
        mHistograms[ "eTofHits_phi_eta"  ] = new TH2F( "A_eTofHits_phi_eta",  "eta vs. phi; #phi; #eta", 400, 0., 2 * M_PI, 400,  -1.7, -0.9 );

        mHistograms[ "eTofHits_globalYZ" ] = new TH2F( "A_eTofHits_globalYZ", "global YZ for sector 18 & 24;y (cm);z (cm)", 400, -300., 300., 100, -310., -270. );

        for( int sector = eTofConst::sectorStart; sector <= eTofConst::sectorStop; sector++ ) {
            for( int plane = eTofConst::zPlaneStart; plane <= eTofConst::zPlaneStop; plane++ ) {
                for( int counter = eTofConst::counterStart; counter <= eTofConst::counterStop; counter++ ) {
                    std::string histName_hit_localXY  = "eTofHits_localXY_s"  + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    std::string histName_hit_globalXY = "eTofHits_globalXY_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    std::string histName_hit_eta_phi  = "eTofHits_phi_eta_s"  + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );

                    // local XY per counter
                    mHistograms[ histName_hit_localXY ]  = new TH2F( Form( "A_eTofHits_localXY_s%dm%dc%d",  sector, plane, counter ), Form( "local XY  sector %d module %d counter %d;x (cm);y (cm)", sector, plane, counter ),   40,  -20.,  20.,  50, -25., 25. );

                    // global XY per counter
                    mHistograms[ histName_hit_globalXY ] = new TH2F( Form( "A_eTofHits_globalXY_s%dm%dc%d", sector, plane, counter ), Form( "global XY  sector %d module %d counter %d;x (cm);y (cm)", sector, plane, counter ), 200, -300., 300., 200, -300., 300. );

                    // eta vs. phi per counter
                    mHistograms[ histName_hit_eta_phi ]  = new TH2F( Form( "A_eTofHits_phi_eta_s%dm%dc%d",  sector, plane, counter ), Form( "eta vs. phi  sector %d module %d counter %d; #phi; #eta", sector, plane, counter ), 200, 0., 2 * M_PI, 200, -1.7, -0.9 );
                }
            }
        }

        mHistograms[ "detectorHitMult" ]  = new TH1F( "A_detectorHitMult",  "detectorHitMult;multiplicity;# events",  200, 0., 200. );


        // --------------
        // track-level QA
        // --------------
        mHistograms[ "track_phi_eta" ] = new TH2F( "QA_track_phi_eta", "eta vs. phi; #phi; #eta", 400, 0., 2 * M_PI, 800, -1.9, 1.9 );
        mHistograms[ "track_phi_pt"  ] = new TH2F( "QA_track_phi_pt",  "pt vs. phi; #phi; p_{T}", 400, 0., 2 * M_PI, 400,  0.,  5.  );

        mHistograms[ "nHits" ]            = new TH1F( "QA_nHits",            "nHitsTpc;nHitsFit;# tracks",                75, 0., 75. );
        mHistograms[ "nHits_etofregion" ] = new TH1F( "QA_nHits_etofregion", "nHitsTpc in etof region;nHitsFit;# tracks", 75, 0., 75. );

        mHistograms[ "track_pt_nHits" ] = new TH2F( "QA_track_pt_nHits", "track nHitsTpc vs. p_{T};p_{T} (GeV/c);nHitsFit", 400, 0., 2., 75, 0., 75. );

        mHistograms[ "trackProj_globalXY" ] = new TH2F( "QA_trackProj_globalXY", "global XY;x (cm);y (cm)", 400, -300.,     300., 400, -300., 300. );
        mHistograms[ "trackProj_phi_eta"  ] = new TH2F( "QA_trackProj_phi_eta",  "eta vs. phi; #phi; #eta", 400,    0., 2 * M_PI, 400,  -1.7, -1.0 );


        // ----------
        // step - B -
        // ----------

        mHistograms[ "intersection_globalXY" ] = new TH2F( "B_intersection_globalXY", "global XY;x (cm);y (cm)", 400, -300.,     300., 400, -300., 300. );
        mHistograms[ "intersection_phi_eta"  ] = new TH2F( "B_intersection_phi_eta",  "eta vs. phi; #phi; #eta", 400,    0., 2 * M_PI, 400,  -1.7, -1.0 );

        mHistograms[ "intersectionMult" ]         = new TH1F( "B_intersectionMult",         "intersectionMult;multiplicity;# events",                    200, 0., 200. );
        mHistograms[ "intersectionMult_primary" ] = new TH1F( "B_intersectionMult_primary", "intersectionMult for primary tracks;multiplicity;# events", 200, 0., 200. );

        mHistograms[ "intersection_perTrack" ] = new TH1F( "B_intersection_perTrack", "intersections per track;# intersections;# tracks", 50, 0., 50. );  


        // track-level QA plots for tracks that have an intersection with eTof volume(s)
        mHistograms[ "intersection_primaryTrack_globalXY" ] = new TH2F( "B_intersection_primaryTrack_globalXY",         "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom0_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom0_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom1_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom1_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom2_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom2_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );

        mHistograms[ "intersection_primaryTrackpos_globalXY" ] = new TH2F( "B_intersection_primaryTrackpos_globalXY",         "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom0pos_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom0pos_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom1pos_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom1pos_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom2pos_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom2pos_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );

        mHistograms[ "intersection_primaryTrackneg_globalXY" ] = new TH2F( "B_intersection_primaryTrackneg_globalXY",         "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom0neg_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom0neg_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom1neg_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom1neg_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "intersection_primaryTrackMom2neg_globalXY" ] = new TH2F( "B_intersection_primaryTrackMom2neg_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );



        mHistograms[ "intersection_track_pt_eta"  ] = new TH2F( "B_intersection_track_pt_eta",  "eta vs. pt;p_{T} (GeV/c);#eta", 400, 0.,       5., 400, -2.,     -0.7 );
        mHistograms[ "intersection_track_pt_phi"  ] = new TH2F( "B_intersection_track_pt_phi",  "phi vs. pt;p_{T} (GeV/c);#phi", 400, 0.,       5., 400,  0., 2 * M_PI );
        mHistograms[ "intersection_track_phi_eta" ] = new TH2F( "B_intersection_track_phi_eta", "eta vs. phi;#phi;#eta",         400, 0., 2 * M_PI, 400, -2.,     -0.9 );

        mHistograms[ "intersection_track_nHitsTpc" ] = new TH1F( "B_intersection_track_nHitsTpc", "nHitsTpc;nHitsFit;# tracks", 75, 0., 75. );

        mHistograms[ "intersection_track_mom_dEdx"     ] = new TH2F( "B_intersection_track_mom_dEdx",     "dE/dx vs. mom;mom (GeV/c);dE/dx (keV/cm)",     100, 0., 5., 100,   0., 10. );
        mHistograms[ "intersection_track_mom_nsigmaPi" ] = new TH2F( "B_intersection_track_mom_nsigmaPi", "n#sigma_{#pi} vs. mom; mom (GeV/c);n#sigma_{#pi}", 100, 0., 5., 100, -10., 10. );  


        // ----------
        // step - C -
        // ----------
        mHistograms[ "detHitvsInter_X" ] = new TH2F( "C_detHitvsInter_X" , "detectorHit vs. intersection X;detectorHit X (cm);intersection X (cm)", 400, -300., 300., 400, -300., 300.);
        mHistograms[ "detHitvsInter_Y" ] = new TH2F( "C_detHitvsInter_Y" , "detectorHit vs. intersection Y;detectorHit Y (cm);intersection Y (cm)", 400, -300., 300., 400, -300., 300.);

        mHistograms[ "detHitvsInter_localX" ] = new TH2F( "C_detHitvsInter_localX" , "detectorHit vs. intersection X;detectorHit local X (cm);intersection local X (cm)",  40, -20., 20.,  80, -20., 20.);
        mHistograms[ "detHitvsInter_localY" ] = new TH2F( "C_detHitvsInter_localY" , "detectorHit vs. intersection Y;detectorHit local Y (cm);intersection local Y (cm)", 200, -50., 50.,  80, -20., 20.);


        for( int sector = eTofConst::sectorStart; sector <= eTofConst::sectorStop; sector++ ) {
            for( int plane = eTofConst::zPlaneStart; plane <= eTofConst::zPlaneStop; plane++ ) {
                std::string histName_detHitvsInter_strip = "detHitvsInter_strip_s" + std::to_string( sector ) + "m" + std::to_string( plane );

                mHistograms[ histName_detHitvsInter_strip ] = new TH2F( Form( "C_detHitvsInter_strip_s%dm%d", sector, plane ), Form( "detectorHit vs. intersection on sector %d module %d;detectorHit strip;intersection strip", sector, plane ), 96, -0.5, 95.5, 96, -0.5, 95.5 );
            }
        }

        mHistograms[ "moduleIndex_deltaX" ] = new TH2F( "C_moduleIndex_deltaX", "module index vs. local #Delta X;module index;#DeltaX (cm)", 36, -0.5, 35.5, 100, -50., 50. );
        mHistograms[ "moduleIndex_deltaY" ] = new TH2F( "C_moduleIndex_deltaY", "module index vs. local #Delta Y;module index;#DeltaY (cm)", 36, -0.5, 35.5, 100, -50., 50. );

        mHistograms[ "matchCand_phi_eta"  ] = new TH2F( "C_matchCand_phi_eta",  "eta vs. phi; #phi; #eta", 400,    0., 2 * M_PI, 400,  -1.7, -0.9 );

        mHistograms[ "matchCand_deltaX" ] = new TH1F( "C_matchCand_deltaX" , "match candidate delta X;match candidate #DeltaX (cm); #match cand", 400, -15., 15. );
        mHistograms[ "matchCand_deltaY" ] = new TH1F( "C_matchCand_deltaY" , "match candidate delta Y;match candidate #DeltaY (cm); #match cand", 400, -15., 15. );


        for( int sector = eTofConst::sectorStart; sector <= eTofConst::sectorStop; sector++ ) {
            for( int plane = eTofConst::zPlaneStart; plane <= eTofConst::zPlaneStop; plane++ ) {
                for( int counter = eTofConst::counterStart; counter <= eTofConst::counterStop; counter++ ) {
                    std::string histName_deltaX = "matchCand_deltaX_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    std::string histName_deltaY = "matchCand_deltaY_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );

                    mHistograms[ histName_deltaX ] = new TH1F( Form( "C_matchCand_deltaX_s%dm%dc%d", sector, plane, counter ) , Form( "match candidate delta X in sector %d module %d counter %d;match candidate #DeltaX (cm); #match cand", sector, plane, counter ), 400, -15., 15. );
                    mHistograms[ histName_deltaY ] = new TH1F( Form( "C_matchCand_deltaY_s%dm%dc%d", sector, plane, counter ) , Form( "match candidate delta Y in sector %d module %d counter %d;match candidate #DeltaY (cm); #match cand", sector, plane, counter ), 400, -15., 15. );
                }
            }
        }

        mHistograms[ "matchCand_deltaX_nHitsTpc" ] = new TH2F( "C_matchCand_deltaX_nHitsTpc" , "match candidate delta X vs. nHitsFit in TPC;nHitsFit in TPC;match candidate #DeltaX (cm)", 75, 0., 75., 400, -15., 15. );
        mHistograms[ "matchCand_deltaY_nHitsTpc" ] = new TH2F( "C_matchCand_deltaY_nHitsTpc" , "match candidate delta Y vs. nHitsFit in TPC;nHitsFit in TPC;match candidate #DeltaY (cm)", 75, 0., 75., 400, -15., 15. );

        mHistograms[ "matchCandMult" ] = new TH1F( "C_matchCandMult", "matchCandMult;multiplicity;# events", 200, 0., 200. );


        // ----------
        // step - D -
        // ----------
        mHistograms[ "trackMatchMultPerDetectorHit" ] = new TH1F( "D_trackMatchMultPerDetectorHit", "multiplicity of tracks pointing to the same detector hit;#tracks;#detector hits", 15, 0., 15. );

        mHistograms[ "singleTrackMatchMult" ] = new TH1F( "D_singleTrackMatchMult", "singleTrackMatchMult;multiplicity;# events", 200, 0., 200. );


        // ----------
        // step - E -
        // ----------
        mHistograms[ "hitMultPerTrack" ] = new TH1F( "E_hitMultPerTrack", "multiplicity of hit matched to the same track;#hits;#tracks", 15, 0., 15. );

        mHistograms[ "finalMatch_pt" ] = new TH1F( "E_finalMatch_pt", "p_{T} distribution of matched tracks", 200, 0., 2. );

        mHistograms[ "finalMatchMult" ] = new TH1F( "E_finalMatchMult", "finalMatchMult;multiplicity;# events", 200, 0., 200. );

        mHistograms[ "overlapHit_globalXY" ] = new TH2F( "E_overlapHit_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        

        // ----------
        // step - F -
        // ----------
        mHistograms[ "finalMatchMultGlobal"  ] = new TH1F( "F_finalMatchMultGlobal",  "finalMatchMultGlobal;multiplicity;# events",  200, 0., 200. );
        mHistograms[ "finalMatchMultPrimary" ] = new TH1F( "F_finalMatchMultPrimary", "finalMatchMultPrimary;multiplicity;# events", 200, 0., 200. );

        mHistograms[ "finalMatchPrimary_globalXY" ] = new TH2F( "F_finalMatchPrimary_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "finalMatchPrimaryMom0_globalXY" ] = new TH2F( "F_finalMatchPrimaryMom0_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "finalMatchPrimaryMom1_globalXY" ] = new TH2F( "F_finalMatchPrimaryMom1_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );
        mHistograms[ "finalMatchPrimaryMom2_globalXY" ] = new TH2F( "F_finalMatchPrimaryMom2_globalXY", "global XY;x (cm);y (cm)", 400, -300., 300., 400, -300., 300. );


        // ----------
        // step - G -
        // ----------
        mHistograms[ "startTimeDiff"       ] = new TH1F( "G_startTimeDiff",       "bTOF - eTOF start time;#DeltaT;#events",       1000,    -20.,     20. );
        mHistograms[ "startTimeDiff_nCand" ] = new TH2F( "G_startTimeDiff_nCand", "bTOF - eTOF start time;#DeltaT;#nCand tracks", 1000,    -20.,     20., 50, 0, 50 );
        mHistograms[ "startTime_T0corr"    ] = new TH1F( "G_startTime_T0corr",    "T0corr evolution;#updates;T0 corr. (ns)",      1000,      0.,   1000. );

        mHistograms[ "matchCand_timeOfFlight"            ] = new TH1F( "G_matchCand_timeOfFlight",            "match candidate time of flight;ToF (ns);# match candidates",             2000, -400., 600. );
        mHistograms[ "matchCand_timeOfFlight_pathLength" ] = new TH2F( "G_matchCand_timeOfFlight_pathLength", "match candidate pathlength vs. time of flight;ToF (ns);pathlength (cm)", 1000, -400., 600., 800, 200., 600. );


        // ----------
        // step - H -
        // ----------
        mHistograms[ "matchCand_beta_mom"     ] = new TH2F( "H_matchCand_beta_mom"     , "match candidate 1/beta vs. momentum;p (GeV/c);1/#beta",         400,   0., 10., 1000, 0.8, 2. );

        mHistograms[ "matchCand_beta_mom_matchDistCut" ] = new TH2F( "H_matchCand_beta_mom_matchDistCut" , "match candidate 1/beta vs. momentum;p (GeV/c);1/#beta", 400, 0., 10., 1000, 0.8, 2. );

        mHistograms[ "matchCandMultPerSector_matchDistCut" ] = new TH2F( "H_matchCandMultPerSector_matchDistCut" , "matchCandMultPerSector_matchDistCut;module;# matches per event", 36, 0, 36, 25, 0, 25 );


        mHistograms[ "matchCand_m2_signmom" ] = new TH2F( "H_matchCand_m2_signmom" , "match candidate m^{2} vs. momentum;q/|q| * p (GeV/c);m^{2} (GeV^{2}/c^{4})", 400, -10., 10., 1000, -0.2, 1.3 );
        mHistograms[ "matchCand_m2_mom"     ] = new TH2F( "H_matchCand_m2_mom"     , "match candidate m^{2} vs. momentum;p (GeV/c);m^{2} (GeV^{2}/c^{4})",         400,   0., 10., 1000, -0.2, 1.3 );

        mHistograms[ "matchCand_m2_mom_matchDistCut" ] = new TH2F( "H_matchCand_m2_mom_matchDistCut" , "match candidate m^{2} vs. momentum;p (GeV/c);m^{2} (GeV^{2}/c^{4})", 400, 0., 10., 1000, -0.2, 1.3 );


        for( int sector = eTofConst::sectorStart; sector <= eTofConst::sectorStop; sector++ ) {
            for( int plane = eTofConst::zPlaneStart; plane <= eTofConst::zPlaneStop; plane++ ) {
                for( int counter = eTofConst::counterStart; counter <= eTofConst::counterStop; counter++ ) {

                    std::string histName_beta_mom = "matchCand_beta_mom_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    mHistograms[ histName_beta_mom ] = new TH2F( Form( "H_matchCand_beta_mom_s%dm%dc%d", sector, plane, counter ), Form( "match candidate 1/beta vs. momentum in sector %d module %d counter %d;p (GeV/c);1/#beta", sector, plane, counter), 200, 0., 10., 500, 0.8, 2. );

                    std::string histName_t0corr_mom  = "matchCand_t0corr_mom_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    mHistograms[ histName_t0corr_mom ] = new TH2F( Form( "H_matchCand_t0corr_mom_s%dm%dc%d", sector, plane, counter ),  Form( "measured tof - tof_{#pi} vs. momentum in sector %d module %d counter %d;mom (GeV/c);#Delta time (ns)",  sector, plane, counter ), 400,     0.,  10., 1000, -500., 500. );

                    std::string histName_t0corr_mom_zoom      = "matchCand_t0corr_mom_zoom_s"     + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    std::string histName_t0corr_mom_zoom_cut  = "matchCand_t0corr_mom_zoom_cut_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );

                    mHistograms[ histName_t0corr_mom_zoom     ] = new TH2F( Form( "H_matchCand_t0corr_mom_zoom_s%dm%dc%d",     sector, plane, counter ),  Form( "measured tof - tof_{#pi} vs. momentum in sector %d module %d counter %d;mom (GeV/c);#Delta time (ns)", sector, plane, counter ), 200, 0., 3., 2000, -10., 10. );
                    mHistograms[ histName_t0corr_mom_zoom_cut ] = new TH2F( Form( "H_matchCand_t0corr_mom_zoom_cut_s%dm%dc%d", sector, plane, counter ),  Form( "measured tof - tof_{#pi} vs. momentum in sector %d module %d counter %d;mom (GeV/c);#Delta time (ns)", sector, plane, counter ), 200, 0., 3., 2000, -10., 10. );

                    std::string histName_t0corr_strip = "matchCand_t0corr_strip_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    mHistograms[ histName_t0corr_strip ] = new TH2F( Form( "H_matchCand_t0corr_strip_s%dm%dc%d", sector, plane, counter ),  Form( "measured tof - tof_{#pi} vs. momentum in sector %d module %d counter %d;local X (cm);#Delta time (ns)", sector, plane, counter ), 32, -16., 16., 2000, -10., 10. );

                    std::string histName_slewing_digi    = "matchCand_slewing_digi_s" + std::to_string( sector ) + "m" + std::to_string( plane ) + "c" + std::to_string( counter );
                    mHistograms[ histName_slewing_digi ] = new TH2F( Form( "H_matchCand_slewing_digi_s%dm%dc%d", sector, plane, counter ),  Form( "measured tof - tof_{#pi} vs. digi ToT in sector %d module %d counter %d;digi Tot (arb. units);#Delta time (ns)",  sector, plane, counter ), 400, 0., 20., 1000, -3., 3. );
                }
            }
        }

        mHistograms[ "matchCand_dEdx_beta_mom1gev" ] = new TH2F( "H_matchCand_dEdx_beta_mom1gev", "dE/dx vs. 1/#beta at p=1 GeV;1/#beta;dE/dx (keV/cm)", 800, 0.8, 1.8, 800, 0., 15. );
        mHistograms[ "matchCand_dEdx_beta_mom2gev" ] = new TH2F( "H_matchCand_dEdx_beta_mom2gev", "dE/dx vs. 1/#beta at p=2 GeV;1/#beta;dE/dx (keV/cm)", 800, 0.8, 1.8, 800, 0., 15. );
    }


    for( auto& kv : mHistograms ) {
        kv.second->SetDirectory( 0 );
    }
    for( auto& kv : mHistograms2d ) {
        kv.second->SetDirectory( 0 );
    }
}


//---------------------------------------------------------------------------
void
StETofMatchMaker::writeHistograms()
{
    if( mHistFileName != "" ) {
        LOG_INFO << "writing histograms to: " << mHistFileName.c_str() << endm;

        TFile histFile( mHistFileName.c_str(), "RECREATE", "etofMatch" );
        histFile.cd();
        
        for ( const auto& kv : mHistograms ) {
            if( kv.second->GetEntries() > 0 ) kv.second->Write();
        }
        for ( const auto& kv : mHistograms2d ) {
            if( kv.second->GetEntries() > 0 ) kv.second->Write();
        }
        histFile.Close();
    }
    else {
        LOG_INFO << "histogram file name is empty string --> cannot write histograms" << endm;
    }
}


//---------------------------------------------------------------------------
void 
StETofMatchMaker::setMatchDistXYT( double x, double y, double t = 99999. )
{
    mMatchDistX = fabs( x );
    mMatchDistY = fabs( y );
    mMatchDistT = fabs( t );
}

//---------------------------------------------------------------------------
int
StETofMatchMaker::rotateHit( const int& sector, const int& rot )
{
    int sec = 13 + ( sector - 13 + rot ) % 12;
    LOG_DEBUG << "hit rotated from: " << sector << " to " << sec << endm;
    return sec;
}


//---------------------------------------------------------------------------
ETofTrack::ETofTrack( const StTrack* sttrack )
{
    mom        = -999.;
    pt         = -999.;
    eta        = -999.;
    phi        = -999.;
    nFtPts     =    0;
    nDedxPts   =    0;
    flag       =    0;
    nHitsPoss  =  999;
    dEdx       = -999.;
    nSigmaPion = -999.;

    if( sttrack ) {
        mom     = sttrack->geometry()->momentum().mag();
        pt      = sttrack->geometry()->momentum().perp();
        eta     = sttrack->geometry()->momentum().pseudoRapidity();
        phi     = sttrack->geometry()->momentum().phi();
        nFtPts  = sttrack->fitTraits().numberOfFitPoints( kTpcId );
        
        static StTpcDedxPidAlgorithm PidAlgorithm;
        static StPionPlus* Pion = StPionPlus::instance();
        const  StParticleDefinition* pd = sttrack->pidTraits( PidAlgorithm );
        if( pd ){
            nSigmaPion   = PidAlgorithm.numberOfSigma( Pion );
            if(PidAlgorithm.traits()){
                dEdx     = PidAlgorithm.traits()->mean() * 1.e6;
                nDedxPts = PidAlgorithm.traits()->numberOfPoints();
            }
        }
        flag        = sttrack->flag();
        nHitsPoss   = sttrack->numberOfPossiblePoints( kTpcId );

        if ( phi < 0. ) phi += 2. * M_PI;
    }
}

//---------------------------------------------------------------------------
ETofTrack::ETofTrack( const StMuTrack* mutrack )
{
    mom        = -999.;
    pt         = -999.;
    eta        = -999.;
    phi        = -999.;
    nFtPts     =    0;
    nDedxPts   =    0;
    flag       =    0;
    nHitsPoss  =  999;
    dEdx       = -999.;
    nSigmaPion = -999.;

    if( mutrack ) {
        mom         = mutrack->momentum().mag();
        pt          = mutrack->momentum().perp();
        eta         = mutrack->momentum().pseudoRapidity();
        phi         = mutrack->momentum().phi(); 
        nFtPts      = mutrack->nHitsFit( kTpcId );
        nDedxPts    = mutrack->nHitsDedx();
        flag        = mutrack->flag();
        nHitsPoss   = mutrack->nHitsPoss( kTpcId );
        dEdx        = mutrack->dEdx() * 1.e6;
        nSigmaPion  = mutrack->nSigmaPion();

        if ( phi < 0. ) phi += 2. * M_PI;
    }
}