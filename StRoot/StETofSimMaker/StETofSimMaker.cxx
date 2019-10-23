/***************************************************************************
 *
 * $Id$
 *
 * Author: Florian Seck, July 2018
 ***************************************************************************
 *
 * Description: StETofSimMaker - class to create StETofHits out of GEANT
 *              hits in several steps:
 *              - get hits from GEANT
 *              - combine different "hits" in several gas gaps into one hit
 *              - create a corresponding StETofHit with
 *                      -- smeared local x,y position according to detector
 *                         resolution
 *                      -- smeared time of flight
 *
 ***************************************************************************
 *
 * $Log$
 *
 ***************************************************************************/
#include <cstddef>         // std::size_t

#include "TFile.h"
#include "TH2.h"
#include "TRandom3.h"

//SCL
#include "SystemOfUnits.h"

#include "StChainOpt.h" // for renaming the histogram file
#include "StEvent.h"
#include "StETofCollection.h"
#include "StETofHit.h"

// Tables
#include "tables/St_g2t_ctf_hit_Table.h"
//#include "tables/St_g2t_track_Table.h"

#include "StETofSimMaker.h"
#include "CombinedGeantHit.h"


//TODO: move parameters to database

const double signalVec = 17.0; //[cm/ns]

const double mergingRadius = 1.0; //[ns]


const double timeResolution = 60.; // 60 ps
const double xResolution    = 1.8; // 1.8 mm
const double yResolution    = 3.0; // 3.0 mm

//_____________________________________________________________________________
StETofSimMaker::StETofSimMaker( const char *name )
: StMaker( "etofSim", name ),
  mGeantData( 0 ),
  mEvent( 0 ),
  mETofCollection( 0 ),
  mWriteHistos( true ),
  mHistoFileName( "" ),
  mTotFileName( "" )
{
    mRawHitMap.clear();
    mCombinedHitVec.clear();
    mMergedHitVec.clear();    

    mIdMultiplier.push_back( 10000000 );
    mIdMultiplier.push_back(   100000 );
    mIdMultiplier.push_back(    10000 );
    mIdMultiplier.push_back(      100 );

    mHistograms.clear();
}


//_____________________________________________________________________________
StETofSimMaker::~StETofSimMaker()
{
 /* no op */
}


//_____________________________________________________________________________
Int_t
StETofSimMaker::Init()
{
    LOG_INFO << "StETofSimMaker::Init()" << endm;

    bookHistograms();

    TFile* totFile = nullptr;

    mTotFileName = "/star/data06/ETOF/StETofSim.hst.root";

    if( isFileExisting( mTotFileName ) ) {
        totFile = new TFile( mTotFileName.c_str(), "READ" );
    }

    if( totFile && !totFile->IsZombie() ) {
        LOG_INFO << "StETofSimMaker::Init() - tot input paramerter file loaded" << endm;

        if( totFile->GetListOfKeys()->Contains( "unclusteredHit_tot" ) ) {
            mHistograms[ "dataTotDistribution" ] = ( TH1F* ) totFile->Get( "unclusteredHit_tot" )->Clone( "dataTotDistribution" );

            mHistograms.at( "dataTotDistribution" )->SetDirectory( 0 );
        }
        else {
            LOG_INFO << "StETofCalibMaker::Init() - tot input histogram not existing in loaded file ..." << endm;    
        }
        totFile->Close();
        delete totFile;
    }
    else{
        LOG_INFO << "StETofCalibMaker::Init() - tot input paramerter file not existing ..." << endm;
    }

    return kStOK;
}


//_____________________________________________________________________________
Int_t
StETofSimMaker::InitRun( Int_t runnumber )
{
    LOG_INFO << "StETofSimMaker::InitRun()" << endm;

    return kStOK;
}


//_____________________________________________________________________________
Int_t
StETofSimMaker::FinishRun( Int_t runnumber )
{

    return kStOk;
}


//_____________________________________________________________________________
Int_t
StETofSimMaker::Finish()
{
    LOG_INFO << "StETofSimMaker::Finish()" << endm;
    
    if ( mWriteHistos ) {        
        setHistoFileName();
        
        LOG_INFO << "writing histograms to: " << mHistoFileName.c_str() << endm;
        
        TFile aFile( mHistoFileName.c_str(), "RECREATE" , "etofsim" );
        
        aFile.cd();
        writeHistograms();
        aFile.Close();
    }
    return kStOK;
}


//_____________________________________________________________________________
Int_t
StETofSimMaker::Make()
{
    LOG_INFO << "StETofSimMaker::Make() starts" << endm;

    mRawHitMap.clear();
    mCombinedHitVec.clear();
    mMergedHitVec.clear();

    // initialize StEvent & ETofCollection
    mEvent = ( StEvent* ) GetInputDS( "StEvent" );
    if( !mEvent ) {
        LOG_ERROR << " No StEvent! Bailing out ... " << endm;
    }
    
    mETofCollection = mEvent->etofCollection();
    if( !mETofCollection ) {
        LOG_INFO << " No ETofCollection -- making a new one " << endm;
        mETofCollection = new StETofCollection();

        mEvent->setETofCollection( mETofCollection );
    }


    // check to see that there are GEANT hits
    mGeantData = GetInputDS( "geant" );  // in bfc chain
    if( !mGeantData ) { // when reading the geant.root file
        mGeantData = GetInputDS( "geantBranch" );
    }
    if( !mGeantData ) {
        LOG_WARN << " No GEANT data loaded. Exit! " << endm;
        return kStWarn;
    }
    LOG_DEBUG << " Found GEANT data -- loading GEANT ETOF hits... " << endm;


    // look for ETOF hits
    St_g2t_ctf_hit* g2t_eto_hits = 0;
    g2t_eto_hits = dynamic_cast< St_g2t_ctf_hit* > ( mGeantData->Find( "g2t_eto_hit" ) );
    if( !g2t_eto_hits ) {
        LOG_WARN << " No ETOF hits in GEANT " << endm;
        return kStOK;
    }

    size_t nhits = g2t_eto_hits->GetNRows();
    LOG_INFO << " Found GEANT ETOF hits: " << nhits << endm;

    g2t_ctf_hit_st* etof_hit = g2t_eto_hits->begin();

    // check for first etof hit
    if( !etof_hit ) {
        LOG_WARN << " No ETOF hit!" << endm;
        return kStWarn;
    }

    // fill raw hit map with track-id as key and geant hits as value 
    for( size_t i = 0; i < nhits; i++, etof_hit++ ) {

        float p = sqrt( pow( etof_hit->p[ 0 ], 2 ) + pow( etof_hit->p[ 1 ], 2 ) + pow( etof_hit->p[ 2 ], 2 ) );

        LOG_DEBUG << "id: " << etof_hit->id << "  p = " << p << "  track_p = " << etof_hit->track_p << "   ---   ";
        LOG_DEBUG << "pathLength: " << etof_hit->s_track << "  volume indices: " << etof_hit->volume_id << endm;
        
        // reject hits from low momentum ( < 100 MeV/c ) tracks: likely secondaries
        if ( p > 0.1 ) { 
            //volume index has no information on gas gap: sector, plane, counter, strip
            int volumeIndex = calcVolumeIndex( convertVolumeId( etof_hit->volume_id ) );

            mRawHitMap[ volumeIndex ].push_back( etof_hit );
        }
    }

    if( mWriteHistos ) {
        fillRawHitsToHistograms( g2t_eto_hits );
    }


    combineRawHits();

    if( mWriteHistos ) {
        fillCombinedHitsToHistograms();
    }


    fastDetectorResponse();


    if( mWriteHistos ) {
        fillMergedHitsToHistograms();
    }


    fillEvent();


    return kStOK;
}


//_____________________________________________________________________________
void
StETofSimMaker::combineRawHits()
{
    // count the number of strips with raw hits above the momentum threshold
    size_t nStrips = 0;
    // for QA: count the number of detectors with raw hits above momentum threshold
    vector< int > seenDetectors;
    size_t nDetectors = 0;

    for( const auto& kv : mRawHitMap ) {
        //LOG_INFO << "volumeIndex: " << kv.first << "  number of raw hits: " << kv.second.size() << endm;
        nStrips++;

        // ---------------
        int detectorIndex = kv.first / 100;

        if( std::find( seenDetectors.begin(), seenDetectors.end(), detectorIndex ) == seenDetectors.end() ) {
            seenDetectors.push_back( detectorIndex );
            nDetectors++;
        }
        // ---------------

        vector< int > seenTrackIds;

        // count the number of tracks that each strip has seen
        size_t nTracks   = 0;
        
        for( const auto& v : kv.second ) {

            if( std::find( seenTrackIds.begin(), seenTrackIds.end(), v->track_p ) == seenTrackIds.end() ) {
                seenTrackIds.push_back( v->track_p );
                nTracks++;
            }
        }
        LOG_DEBUG << "strip " << kv.first << " has seen " << nTracks << " track(s)" << endm;

        mHistograms.at( "nTracks" )->Fill( nTracks );


        // now combine hits on a given strip and add to list
        for( const int& track_p : seenTrackIds ) {

            LOG_DEBUG << "seenTrackIds: " << track_p << endm;

            CombinedGeantHit cHit;

            for( const g2t_ctf_hit_st* rawHit : kv.second ) {  
                    
                    //only combine raw hits from the same track
                    if( rawHit->track_p == track_p ) {
                        vector<int> volumeIds = convertVolumeId( rawHit->volume_id );
                        
                        cHit.addRawHit( rawHit, volumeIds );
                    }
            }
            cHit.averageRawHits();

            mCombinedHitVec.push_back( cHit );
        }

    }

    mHistograms.at( "nStrips"    )->Fill( nStrips    );
    mHistograms.at( "nDetectors" )->Fill( nDetectors );

    // uncomment to get print out of every combined hit
    //for( const auto& hit : mCombinedHitVec ) {
    //  hit.log();
    //}

    LOG_INFO << "size of combined hit vector: " << mCombinedHitVec.size() << endm;

    return;   
}


//_____________________________________________________________________________
void
StETofSimMaker::fastDetectorResponse()
{
    //      ------------------------------------------------
    //      -- (1) reject hits on the same strip from different particles
    //      -- (2) proxy for ToT
    //      -- (3) clustering of hits on nearby strip
    //      -- (4) smear time of flight with detector resolution
    //      -- (5) smear local X,Y with detector resolution  
    //      ------------------------------------------------



    //      ------------------------------------------------    
    //      (1) reject hits on the same strip from different particles
    //      ------------------------------------------------
    map< int, CombinedGeantHit > combinedHitMap; // key: volume index, value: a combined hit
    
    for( const auto& v : mCombinedHitVec ) {

        int volIndex = calcVolumeIndex( v.volumeVec() );

        LOG_DEBUG << " * * * * * * " << volIndex << endm;

        if ( combinedHitMap.count( volIndex ) ) {  // key exists
            // compare the leading edge times of the hits and only keep the earliest hit
            LOG_DEBUG << " more than one combined hit on the same strip ... " << endm;

            double storedTime = combinedHitMap.at( volIndex ).time();
            double hitTime    = v.time();

            if( hitTime < storedTime ) {
                combinedHitMap.at( volIndex ) = v;
            }

            LOG_DEBUG << "  ... store hit with time " << combinedHitMap.at( volIndex ).time() << endm;
        }
        else {
            // store hit in the map
            LOG_DEBUG << " first combined hit on the strip ... storing the hit in the map " << endm;
            combinedHitMap[ volIndex ] = v;           
        }
    }

    //      ------------------------------------------------
    //      (2) proxy for ToT
    //      ------------------------------------------------
    map< int, vector< CombinedGeantHit > > combinedHitDetMap;  // key: detector index, value: vector of combined hits

    // fill map with vector of hits on the same detector for hit clustering
    // need to sample the time over threshold before
    for( auto& kv : combinedHitMap ) {
        int detIndex = kv.first / 100;

        LOG_DEBUG << "kv.first = " << kv.first << "  detIndex = " << detIndex << endm;

        double sampledTot = 1;
        if( mHistograms.count( "dataTotDistribution" ) ) {
            sampledTot = mHistograms.at( "dataTotDistribution" )->GetRandom();
        }
        mHistograms.at( "fastsim_unclusteredTot" )->Fill( sampledTot );

        kv.second.setTot( sampledTot );

        combinedHitDetMap[ detIndex ].push_back( kv.second );
    }


    //      ------------------------------------------------
    //      (3) clustering of hits on nearby strip
    //      ------------------------------------------------
    // loop over detectors and merge hits on adjecent strips into clusters
    for( auto& kv : combinedHitDetMap ) {
        LOG_INFO << kv.first << "   with size ( unclustered ) " << kv.second.size()  << " ... merging hits " << endm;

        mergeClusters( kv.second );
    }



    // apply detector resolution in X,Y position and time
    // need to loop over the vector this way, otherwise one cannot change the hit properties
    TRandom3 randEngine( 0 );

    for( size_t i=0; i<mMergedHitVec.size(); i++ ) {
        // TODO: get detector resolution from a database table

        //      ------------------------------------------------
        //      (4) smear time of flight with detector resolution
        //      ------------------------------------------------
        double timeBlur = randEngine.Gaus( 0., timeResolution ) * 1e-12 / nanosecond; // at the moment the same for each detector

        LOG_DEBUG << " hit time w/o blur: " << mMergedHitVec.at( i ).time() << " add a blur of: " << timeBlur << endm;

        mHistograms.at( "mergedHit_tof_w/o_blur" )->Fill( mMergedHitVec.at( i ).time()  );
        mHistograms.at( "mergedHit_timeBlur"     )->Fill( timeBlur );

        mMergedHitVec.at(i).setTime( mMergedHitVec.at( i ).time() + timeBlur );

        LOG_DEBUG << " hit time w/ blur: " << mMergedHitVec.at( i ).time() << endm;


        //      ------------------------------------------------
        //      (5) smear local X,Y with detector resolution 
        //      ------------------------------------------------
        double xBlur = randEngine.Gaus( 0., xResolution ) * millimeter / centimeter; // at the moment the same for each detector
        double yBlur = randEngine.Gaus( 0., yResolution ) * millimeter / centimeter; // at the moment the same for each detector

        LOG_DEBUG << " hit local x w/o blur: " << mMergedHitVec.at( i ).pos().x() << " add a blur of: " << xBlur << endm;
        LOG_DEBUG << " hit local y w/o blur: " << mMergedHitVec.at( i ).pos().y() << " add a blur of: " << yBlur << endm;

        mHistograms.at( "mergedHit_localX_w/o_blur" )->Fill( mMergedHitVec.at( i ).pos().x()  );
        mHistograms.at( "mergedHit_localY_w/o_blur" )->Fill( mMergedHitVec.at( i ).pos().y()  );
        mHistograms.at( "mergedHit_xBlur"           )->Fill( xBlur );
        mHistograms.at( "mergedHit_yBlur"           )->Fill( yBlur );

        mMergedHitVec.at( i ).setPos( mMergedHitVec.at( i ).pos().x() + xBlur, mMergedHitVec.at( i ).pos().y() + yBlur, mMergedHitVec.at( i ).pos().z() );

        LOG_DEBUG << " hit local x w/ blur: " << mMergedHitVec.at( i ).pos().x() << endm;
        LOG_DEBUG << " hit local y w/ blur: " << mMergedHitVec.at( i ).pos().y() << endm;
    }
}


//_____________________________________________________________________________
// taken from StETofHitMaker and adjusted for CombinedGeantHits
void
StETofSimMaker::mergeClusters( vector< CombinedGeantHit >& v )
{
    int hitsOnDet = 0;

    while( v.size() > 0 ) {
        CombinedGeantHit cHit = v.at( 0 );

        LOG_DEBUG << "mergeClusters() - checking hit vector for possible hits to merge with..." << endm;

        // scale with tot for weigthed average
        double weight        = cHit.tot();
        double weightedTime  = cHit.time()    * weight;
        double weightedPosX  = cHit.pos().x() * weight;
        double weightedPosY  = cHit.pos().y() * weight;

        unsigned  int clusterSize   = 1;
        int           lowestStrip   = ( int ) cHit.pos().x(); // currently only one-strip clusters: lowest and highest strip identical
        int           highestStrip  = ( int ) cHit.pos().x();

        double weightedMomX  = cHit.mom().x() * weight;
        double weightedMomY  = cHit.mom().y() * weight;
        double weightedMomZ  = cHit.mom().z() * weight;

        double weightedPathLength = cHit.pathLength() * weight;
        int    track_p       = cHit.track_p();     
        int    addedNRawHits = cHit.nRawHits();

        int nTracks_contributing = 1;

        map< int, int > trackMap;
        trackMap[ track_p ] = 1;


        unsigned int mergeHitIndex = 1;
        while( v.size() > 1 ) {
            LOG_DEBUG << "mergeClusters() - merge hit index = " << mergeHitIndex << endm;

            if( mergeHitIndex >= v.size() ) {
                LOG_INFO << "mergeClusters() - merge hit index > size of hit vector -> stop looping" << endm;
                break;
            }

            CombinedGeantHit cMergeHit = v.at( mergeHitIndex );

            // calculate distance measures
            double timeDiff = cHit.time() - cMergeHit.time();  
            double posYDiff = ( cHit.pos().y() - cMergeHit.pos().y() ) / signalVec; //divide by signal velocity
            
            bool lowerAdjecentStip  = ( ( int ) cMergeHit.pos().x() > -1.01 + lowestStrip  ); 
            bool higherAdjecentStip = ( ( int ) cMergeHit.pos().x() <  1.01 + highestStrip ); 

            // merging condition
            // x is not convoluted into the clusterbuilding radius since it is not supposed to be zero
            // -> check if x position is on a adjecent strip            
            if( ( sqrt( timeDiff * timeDiff + posYDiff * posYDiff ) ) < mergingRadius &&
                ( lowerAdjecentStip || higherAdjecentStip ) ) {

                LOG_DEBUG << "mergeClusters() - merging is going on" << endm;
                //merge hit into cluster
                weightedTime  += ( cMergeHit.time()    * cMergeHit.tot() );
                weightedPosX  += ( cMergeHit.pos().x() * cMergeHit.tot() );
                weightedPosY  += ( cMergeHit.pos().y() * cMergeHit.tot() );
                weight        +=   cMergeHit.tot();
                clusterSize++;

                if( lowerAdjecentStip  ) {
                    lowestStrip = ( int ) cMergeHit.pos().x();
                }
                if( higherAdjecentStip ) {
                    highestStrip = ( int ) cMergeHit.pos().x();
                }

                weightedMomX  += ( cMergeHit.mom().x() * cMergeHit.tot() );
                weightedMomY  += ( cMergeHit.mom().y() * cMergeHit.tot() );
                weightedMomZ  += ( cMergeHit.mom().z() * cMergeHit.tot() );
                
                weightedPathLength += ( cMergeHit.pathLength() * cMergeHit.tot() );
                
                addedNRawHits += cMergeHit.nRawHits();

                if( trackMap.count( cMergeHit.track_p() ) ) {
                    trackMap.at( cMergeHit.track_p() )++;
                }
                else {
                    trackMap[ cMergeHit.track_p() ] = 1;
                    LOG_DEBUG << " *** merging hits created by different tracks ... *** " << endm;
                    nTracks_contributing++;
                }

                // erase hit after merging
                v.erase( v.begin() + mergeHitIndex );
            }
            else {
                LOG_DEBUG << "mergeClusters() - merging condition not fulfilled -- check the next hit" << endm;
                mergeHitIndex++;
            } // check next hit 

        } // merge loop

        // renormalize with the total ToT
        weightedTime /= weight;
        weightedPosX /= weight;
        weightedPosY /= weight;

        weightedMomX /= weight;
        weightedMomY /= weight;
        weightedMomZ /= weight;

        weightedPathLength /= weight;


        LOG_DEBUG << "mergeClusters() - MERGED HIT: ";
        LOG_DEBUG << "time="            << setprecision( 16 ) << weightedTime  << "  ";
        LOG_DEBUG << "totalTot="        << setprecision(  4 ) << weight        << "  ";
        LOG_DEBUG << "clusterSize="     << setprecision(  1 ) << clusterSize   << "  ";
        LOG_DEBUG << "localX="          << setprecision(  4 ) << weightedPosX  << "  ";
        LOG_DEBUG << "localY="          << setprecision(  4 ) << weightedPosY  << endm;

        if( nTracks_contributing > 1 ) {
            mHistograms.at( "mergedHits_track_p_fail"             )->Fill( trackMap.size() );
            mHistograms.at( "mergedHits_track_p_fail_clusterSize" )->Fill( trackMap.size(), clusterSize );
        }

        //add merged hit to a storage vector
        CombinedGeantHit mergedHit;

        mergedHit.setSector(  cHit.sector()  );
        mergedHit.setPlane(   cHit.plane()   );
        mergedHit.setCounter( cHit.counter() );

        mergedHit.setTime( weightedTime );
        mergedHit.setTot(  weight );

        mergedHit.setPos( weightedPosX, weightedPosY, 0. );
        mergedHit.setMom( weightedMomX, weightedMomY, weightedMomZ );

        mergedHit.setPathLength( weightedPathLength );

        mergedHit.setTrack_p( track_p );

        mergedHit.setNRawHits(    addedNRawHits );
        mergedHit.setClusterSize( clusterSize   );


        mMergedHitVec.push_back( mergedHit );

        hitsOnDet++;

        // erase hit from vector
        v.erase( v.begin() );
    }

    LOG_INFO << "mergeClusters() - merging done with " << hitsOnDet << " cluster(s) " << endm;
}


//_____________________________________________________________________________
void
StETofSimMaker::fillEvent()
{

    LOG_INFO << "ETofCollection has " << mETofCollection->etofHits().size() << " real hits ..." << endm;

    size_t nMcHits = 0;
    
    // at the moment just loop over combined hits and convert them as they are to StETofHits
    // or loop over merged hits and convert them to StETofHits
    // for( const auto& v: mCombinedHitVec ) {
    for( const auto& v: mMergedHitVec ) {

        StETofHit* aHit = new StETofHit();

        aHit->setHwAddress(  v.sector(), v.plane(), v.counter() );

        aHit->setLocalX( v.pos().x() );
        aHit->setLocalY( v.pos().y() );
        aHit->setClusterSize( v.clusterSize() );

        aHit->setTime(     v.time() );
        aHit->setTotalTot( v.tot()  );

        mETofCollection->addHit( aHit );

        nMcHits++;
    }

    LOG_INFO << " ... and " << nMcHits << " MC hits stored" << endm;
}


//_____________________________________________________________________________
vector < int >
StETofSimMaker::convertVolumeId( const int& id )
{
    vector< int > vol_num;

    vol_num.push_back( sector(  id ) );
    vol_num.push_back( plane(   id ) );
    vol_num.push_back( counter( id ) );
    vol_num.push_back( strip(   id ) );
    vol_num.push_back( gap(     id ) );

    return vol_num;
}


//_____________________________________________________________________________
int
StETofSimMaker::calcVolumeIndex( const vector< int >& volId )
{
    int volumeIndex = volId[ 3 ]        +
                      volId[ 2 ] * 100  +
                      volId[ 1 ] * 1000 + 
                      volId[ 0 ] * 10000;
    return volumeIndex;
}


//_____________________________________________________________________________
int
StETofSimMaker::calcDetectorIndex( const vector< int >& volId )
{
    int detectorIndex = volId[ 2 ]      +
                        volId[ 1 ] * 10 + 
                        volId[ 0 ] * 100;
    return detectorIndex;
}


//_____________________________________________________________________________
void
StETofSimMaker::fillRawHitsToHistograms( const St_g2t_ctf_hit* g2t_eto_hits )
{
    size_t nRawHits = g2t_eto_hits->GetNRows();

    size_t nRawHitsHighP = 0;

    g2t_ctf_hit_st* etof_hit = g2t_eto_hits->begin();

    // check for etof_hit (only applies to the first one)
    if( !etof_hit ) {
        return;
    }

    for( size_t i = 0; i < nRawHits; i++, etof_hit++ ) {
        LOG_DEBUG << "etof_hit " << etof_hit << endm;
        LOG_DEBUG << "etof_hit->s_track = " << etof_hit->s_track << endm;

        // histogram all geant output variables
        mHistograms.at( "id"        )->Fill( etof_hit->id        );
        mHistograms.at( "track_p"   )->Fill( etof_hit->track_p   );
        mHistograms.at( "volume_id" )->Fill( etof_hit->volume_id );

        vector< int > volume_num = convertVolumeId( etof_hit->volume_id );

        float p = sqrt( pow( etof_hit->p[ 0 ], 2 ) + pow( etof_hit->p[ 1 ], 2 ) + pow( etof_hit->p[ 2 ], 2 ) );


        mHistograms.at( "etof_sector"  )->Fill( volume_num[ 0 ] );
        mHistograms.at( "etof_plane"   )->Fill( volume_num[ 1 ] );
        mHistograms.at( "etof_counter" )->Fill( volume_num[ 2 ] );
        mHistograms.at( "etof_strip"   )->Fill( volume_num[ 3 ] );
        mHistograms.at( "etof_gap"     )->Fill( volume_num[ 4 ] );

        mHistograms.at( "de"   )->Fill( etof_hit->de / keV );
        mHistograms.at( "de_p" )->Fill( etof_hit->de / keV, p );

        mHistograms.at( "ds" )->Fill( etof_hit->ds );

        mHistograms.at( "s_track" )->Fill( etof_hit->s_track );

        mHistograms.at( "tof" )->Fill( etof_hit->tof / nanosecond );

        mHistograms.at( "x_0" )->Fill( etof_hit->x[ 0 ] );
        mHistograms.at( "x_1" )->Fill( etof_hit->x[ 1 ] );
        mHistograms.at( "x_2" )->Fill( etof_hit->x[ 2 ] );

        mHistograms.at( "p_0"   )->Fill( etof_hit->p[ 0 ] );
        mHistograms.at( "p_1"   )->Fill( etof_hit->p[ 1 ] );
        mHistograms.at( "p_2"   )->Fill( etof_hit->p[ 2 ] );
        mHistograms.at( "p_mag" )->Fill( p );

        mHistograms.at( "track_p_p_mag" )->Fill( etof_hit->track_p, p );        

        if ( p > 1.0 ) {
            nRawHitsHighP++;
        }

    } // loop on hits

    mHistograms.at( "nRawHits"      )->Fill( nRawHits );
    mHistograms.at( "nRawHitsHighP" )->Fill( nRawHitsHighP );
}


//_____________________________________________________________________________
void
StETofSimMaker::fillCombinedHitsToHistograms()
{
    mHistograms.at( "nCombinedHits" )->Fill( mCombinedHitVec.size() );

    for( const auto& hit : mCombinedHitVec ) {   
        mHistograms.at( "combinedHits_pt"  )->Fill( hit.mom().perp() );
        mHistograms.at( "combinedHits_pz"  )->Fill( hit.mom().z()    );

        mHistograms.at( "combinedHits_tof" )->Fill( hit.time() );

        mHistograms.at( "combinedHits_tof_counter" )->Fill( ( hit.plane() - 1 ) * 3 + hit.counter(), hit.time() );

        mHistograms.at( "combinedHits_pathlength" )->Fill( hit.pathLength() );

        mHistograms.at( "combinedHits_localX"  )->Fill( hit.pos().x() );
        mHistograms.at( "combinedHits_localY"  )->Fill( hit.pos().y() );

        mHistograms.at( "combinedHits_sector"  )->Fill( hit.sector()  );
        mHistograms.at( "combinedHits_plane"   )->Fill( hit.plane()   );
        mHistograms.at( "combinedHits_counter" )->Fill( hit.counter() );
        mHistograms.at( "combinedHits_strip"   )->Fill( hit.strip()   );
    } // loop on hits
}

//_____________________________________________________________________________
void
StETofSimMaker::fillMergedHitsToHistograms()
{
    mHistograms.at( "nMergedHits" )->Fill( mMergedHitVec.size() );

    for( const auto& hit : mMergedHitVec ) {   
        mHistograms.at( "mergedHits_pt"  )->Fill( hit.mom().perp() );
        mHistograms.at( "mergedHits_pz"  )->Fill( hit.mom().z()    );

        mHistograms.at( "mergedHits_tof" )->Fill( hit.time() );

        mHistograms.at( "mergedHits_tof_counter" )->Fill( ( hit.plane() - 1 ) * 3 + hit.counter(), hit.time() );

        mHistograms.at( "mergedHits_pathlength" )->Fill( hit.pathLength() );

        mHistograms.at( "mergedHits_localX"  )->Fill( hit.pos().x() );
        mHistograms.at( "mergedHits_localY"  )->Fill( hit.pos().y() );

        mHistograms.at( "mergedHits_sector"  )->Fill( hit.sector()  );
        mHistograms.at( "mergedHits_plane"   )->Fill( hit.plane()   );
        mHistograms.at( "mergedHits_counter" )->Fill( hit.counter() ); 

        mHistograms.at( "mergedHits_track_p" )->Fill( hit.track_p() );  

        mHistograms.at( "mergedHits_addedNRawHits" )->Fill( hit.nRawHits() );
        mHistograms.at( "mergedHits_clusterSize"   )->Fill( hit.pos().x(), hit.clusterSize() );

    } // loop on hits
}


//_____________________________________________________________________________
void
StETofSimMaker::bookHistograms()
{
    LOG_INFO << "StETofSimMaker::bookHistograms" << endm;

    // histograms for raw hits from geant
    mHistograms[ "nRawHits" ]      = new TH1F( "nRawHits",      "# of raw eTof hits from GEANT;# hits;# events",               2000, 0., 2000. );
    mHistograms[ "nRawHitsHighP" ] = new TH1F( "nRawHitsHighP", "# of raw eTof hits from GEANT with p > 1GeV;# hits;# events",  200, 0.,  200. );


    mHistograms[ "etof_sector" ]  = new TH1F( "etof_sector",  "etof sector;sector;# hits",   20,  10.5, 30.5 );
    mHistograms[ "etof_plane" ]   = new TH1F( "etof_plane",   "etof plane;plane;# hits",      6,  -0.5,  5.5 );
    mHistograms[ "etof_counter" ] = new TH1F( "etof_counter", "etof counter;counter;# hits",  6,  -0.5,  5.5 );
    mHistograms[ "etof_strip" ]   = new TH1F( "etof_strip",   "etof strip;strip;# hits",     40,  -0.5, 39.5 );
    mHistograms[ "etof_gap" ]     = new TH1F( "etof_gap",     "etof gap;gap;# hits",         15,  -0.5, 14.5 );

    mHistograms[ "id" ]           = new TH1F( "id",        "geant hit id;hit id;# hits",       500,      0.5,    500.5 );
    mHistograms[ "track_p" ]      = new TH1F( "track_p",   "geant track_p;track id;# hits",    200,      0.5,    200.5 );
    mHistograms[ "volume_id" ]    = new TH1F( "volume id", "geant volume_id;volume id;#hits", 5000, 10000000, 35000000 );

    mHistograms[ "ds" ] = new TH1F( "ds", "geant ds;step length (cm);# hits",  400, -0.01, 1.01 );
    mHistograms[ "de" ] = new TH1F( "de", "geant de;energy loss (keV);# hits", 200, -0.01, 1.01 );
   
    mHistograms[ "de_p" ] = new TH2F( "de_p", "geant de vs geant p_4;energy loss (keV);p (GeV/c)", 200, -0.01, 1.01, 200, 0., 10. );


    mHistograms[ "p_0" ]   = new TH1F( "p_0",   "geant p_0;p_{x} (GeV/c);# hits",  200, -10., 10. );
    mHistograms[ "p_1" ]   = new TH1F( "p_1",   "geant p_1;p_{y} (GeV/c);# hits",  200, -10., 10. );
    mHistograms[ "p_2" ]   = new TH1F( "p_2",   "geant p_2;p_{z} (GeV/c);# hits",  200, -10., 10. );
    mHistograms[ "p_mag" ] = new TH1F( "p_mag", "geant p_4;p (GeV/c);# hits",      500,   0., 50. );

    mHistograms[ "track_p_p_mag" ] = new TH2F( "track_p_p_mag", "geant p_4 vs. track_p;track id;p (GeV/c)", 200, 0.5, 200.5, 500, 0., 5. );

    mHistograms[ "s_track" ] = new TH1F( "s_track", "geant s_track;track length (cm);# hits", 1000, 0., 500. );
    mHistograms[ "tof" ]     = new TH1F( "tof",     "geant tof;ToF (ns);# hits",              1000, 0.,  50. );

    mHistograms[ "x_0" ] = new TH1F( "x_0", "geant x_0;gas gap local x (cm);# hits", 200, -0.55, 0.55 );
    mHistograms[ "x_1" ] = new TH1F( "x_1", "geant x_1;gas gap local y (cm);# hits", 200,  -15.,  15. );
    mHistograms[ "x_2" ] = new TH1F( "x_2", "geant x_2;gas gap local z (cm);# hits", 200, -0.01, 0.01 );


    // histograms for combined hits
    mHistograms[ "nStrips"    ] = new TH1F( "nStrips",    "# of strips with raw hits;# strips;# event",          100, 0., 100. );
    mHistograms[ "nDetectors" ] = new TH1F( "nDetectors", "# of detectors with raw hits;# detectors;# event",     20, 0.,  20. );
    mHistograms[ "nTracks"    ] = new TH1F( "nTracks",    "# of tracks hitting the same strip;# tracks; #strip",  10, 0.,  10. );


    mHistograms[ "nCombinedHits" ] = new TH1F( "nCombinedHits", "# of combined eTof hits from GEANT;# hits;# events", 200, 0., 200. );

    mHistograms[ "combinedHits_pt"  ] = new TH1F( "combinedHits_pt",  "combined hit p_{T};p_{T} (GeV/c);# hits",  200, -10., 10. );
    mHistograms[ "combinedHits_pz"  ] = new TH1F( "combinedHits_pz",  "combined hit p_{Z};p_{Z} (GeV/c);# hits",  200, -10., 10. );

    mHistograms[ "combinedHits_tof" ] = new TH1F( "combinedHits_tof", "combined hit ToF;ToF (ns);# hits",        1000,   0., 50. );

    mHistograms[ "combinedHits_tof_counter" ] = new TH2F( "combinedHits_tof_counter", "combined hit Tof vs. counter;plane * 3 + counter; ToF (ns)", 9, 0.5, 9.5, 400, 5., 25. );

    mHistograms[ "combinedHits_pathlength"  ] = new TH1F( "combinedHits_pathlength",  "combined hit path length; path length (cm);# hits", 1000, 0., 500. );

    mHistograms[ "combinedHits_localX" ] = new TH1F( "combinedHits_localX", "combined hit local x;local x (cm);# hits", 500, -20., 20. );
    mHistograms[ "combinedHits_localY" ] = new TH1F( "combinedHits_localY", "combined hit local y;local y (cm);# hits", 500, -20., 20. );

    mHistograms[ "combinedHits_sector"  ] = new TH1F( "combinedHits_sector",  "combined hit sector;sector;# hits",   20,  10.5, 30.5 );
    mHistograms[ "combinedHits_plane"   ] = new TH1F( "combinedHits_plane",   "combined hit plane;plane;# hits",      6,  -0.5,  5.5 );
    mHistograms[ "combinedHits_counter" ] = new TH1F( "combinedHits_counter", "combined hit counter;counter;# hits",  6,  -0.5,  5.5 );
    mHistograms[ "combinedHits_strip"   ] = new TH1F( "combinedHits_strip",   "combined hit strip;strip;# hits",     40,  -0.5, 39.5 );


    // histograms for detector fast response
    mHistograms[ "fastsim_unclusteredTot" ] = new TH1F( "fastsim_unclusteredTot", "tot of unclustered hits; ToT (ns);# hits", 200, 0., 50. );

    // histograms for merged hits
    mHistograms[ "nMergedHits" ] = new TH1F( "nMergedHits", "# of merged eTof hits;# hits;# events", 200, 0., 200. );

    mHistograms[ "mergedHits_pt"  ] = new TH1F( "mergedHits_pt",  "merged hit p_{T};p_{T} (GeV/c);# hits",  200, -10., 10. );
    mHistograms[ "mergedHits_pz"  ] = new TH1F( "mergedHits_pz",  "merged hit p_{Z};p_{Z} (GeV/c);# hits",  200, -10., 10. );

    mHistograms[ "mergedHits_tof" ] = new TH1F( "mergedHits_tof", "merged hit ToF;ToF (ns);# hits",        1000,   0., 50. );

    mHistograms[ "mergedHits_tof_counter" ] = new TH2F( "mergedHits_tof_counter", "merged hit Tof vs. counter;plane * 3 + counter; ToF (ns)", 9, 0.5, 9.5, 400, 5., 25. );

    mHistograms[ "mergedHits_pathlength"  ] = new TH1F( "mergedHits_pathlength",  "merged hit path length; path length (cm);# hits", 1000, 0., 500. );

    mHistograms[ "mergedHits_localX" ] = new TH1F( "mergedHits_localX", "merged hit local x;local x (cm);# hits", 500, -20., 20. );
    mHistograms[ "mergedHits_localY" ] = new TH1F( "mergedHits_localY", "merged hit local y;local y (cm);# hits", 500, -20., 20. );

    mHistograms[ "mergedHits_sector"  ] = new TH1F( "mergedHits_sector",  "merged hit sector;sector;# hits",   20,  10.5, 30.5 );
    mHistograms[ "mergedHits_plane"   ] = new TH1F( "mergedHits_plane",   "merged hit plane;plane;# hits",      6,  -0.5,  5.5 );
    mHistograms[ "mergedHits_counter" ] = new TH1F( "mergedHits_counter", "merged hit counter;counter;# hits",  6,  -0.5,  5.5 );

    mHistograms[ "mergedHits_track_p" ] = new TH1F( "mergedHits_track_p", "merged hit track id;track id;# hits", 200, 0., 200. );
    mHistograms[ "mergedHits_track_p_fail" ] = new TH1F( "mergedHits_track_p_fail", "merged hits form than one track;# of wrongly merged;# merged hits", 10, 0., 10. );
    mHistograms[ "mergedHits_track_p_fail_clusterSize" ] = new TH2F( "mergedHits_track_p_fail_clusterSize", "merged hits form than one track;# wrongly merged;cluster size", 10, 0., 10., 10, 0., 10. );

    mHistograms[ "mergedHits_addedNRawHits" ] = new TH1F( "mergedHits_addedNRawHits", "added number of raw hits per raw hit", 200, 0., 200. );
    mHistograms[ "mergedHits_clusterSize"   ] = new TH2F( "mergedHits_clusterSize", "merged hits cluster size vs local X;local x (cm);cluster size", 50, -25, 25, 10, 0., 10. );

    mHistograms[ "mergedHit_tof_w/o_blur"    ] = new TH1F( "mergedHits_tof_w/o_blur",    "merged hit ToF w/o blur;ToF (ns);# hits",         1000,   0., 50. );
    mHistograms[ "mergedHit_localX_w/o_blur" ] = new TH1F( "mergedHits_localX_w/o_blur", "merged hit local x w/o blur;local x (cm);# hits",  500, -20., 20. );
    mHistograms[ "mergedHit_localY_w/o_blur" ] = new TH1F( "mergedHits_localY_w/o_blur", "merged hit local y w/o blur;local y (cm);# hits",  500, -20., 20. );

    mHistograms[ "mergedHit_timeBlur" ] = new TH1F( "mergedHits_timeBlur", "merged hit time blur; blur (ns);# hits",      1000, -10., 10. );  
    mHistograms[ "mergedHit_xBlur"    ] = new TH1F( "mergedHits_xBlur",    "merged hit local x blur; blur (cm); # hits",  1000, -10., 10. );
    mHistograms[ "mergedHit_yBlur"    ] = new TH1F( "mergedHits_yBlur",    "merged hit local y blur; blur (cm); # hits",  1000, -10., 10. );


    for ( auto& kv : mHistograms ) {
        kv.second->SetDirectory( 0 );
    }
}


//_____________________________________________________________________________
// setHistFileName uses the string argument from the chain being run to set
// the name of the output histogram file.
//
void
StETofSimMaker::setHistoFileName()
{
    string extension = ".etofSim.root";

        if( GetChainOpt()->GetFileOut() != nullptr ) {
        TString outFile = GetChainOpt()->GetFileOut();

        mHistoFileName = ( string ) outFile;

        // get rid of .root
        size_t lastindex = mHistoFileName.find_last_of( "." );
        mHistoFileName = mHistoFileName.substr( 0, lastindex );

        // get rid of .MuDst or .event if necessary
        lastindex = mHistoFileName.find_last_of( "." );
        mHistoFileName = mHistoFileName.substr( 0, lastindex );

        // get rid of directories
        lastindex = mHistoFileName.find_last_of( "/" );
        mHistoFileName = mHistoFileName.substr( lastindex + 1, mHistoFileName.length() );

        mHistoFileName = mHistoFileName + extension;
    }
    else {
        LOG_ERROR << "cannot set the output filename for histograms" << endm;
        mHistoFileName = "etofSim.root";
    }
}


//_____________________________________________________________________________
void
StETofSimMaker::writeHistograms()
{
    LOG_DEBUG << "StETofSimMaker::writeHistograms()" << endm;
    for ( const auto& kv : mHistograms ) {
        if( kv.second->GetEntries() > 0 ) kv.second->Write();
    }
}

//_____________________________________________________________
bool
StETofSimMaker::isFileExisting( const std::string& fileName )
{
    std::ifstream infile( fileName );
    return infile.good();
}
