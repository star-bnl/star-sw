/***************************************************************************
 *
 * $Id: StFttClusterMaker.cxx,v 1.4 2019/03/08 18:45:40 fseck Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StFttClusterMaker - class to fill the StEvent from DAQ reader:
 * unpack raw data & save StETofHeader & StETofDigis in StETofCollection 
 *
 ***************************************************************************/
#include <vector>
#include <set>
#include <map>
#include <array>
#include <algorithm>    // std::is_sorted


#include "StEvent.h"
#include "StEnumerations.h"

#include "StFttClusterMaker.h"


#include "StEvent/StFttRawHit.h"
#include "StEvent/StFttCluster.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCollection.h"

#include "StFttDbMaker/StFttDb.h"


//_____________________________________________________________
StFttClusterMaker::StFttClusterMaker( const char* name )
: StMaker( name ),
  mEvent( 0 ),          /// pointer to StEvent
  mRunYear( 0 ),        /// year in which the data was taken (switch at 1st Oct)
  mDebug( false ),       /// print out of all full messages for debugging
  mFttDb( nullptr )
{
    LOG_INFO << "StFttClusterMaker::ctor"  << endm;
}

//_____________________________________________________________
StFttClusterMaker::~StFttClusterMaker()
{  /* no op */

}

//_____________________________________________________________
Int_t
StFttClusterMaker::Init()
{
    LOG_INFO << "StFttClusterMaker::Init" << endm;

    return kStOk;
}

//_____________________________________________________________
Int_t
StFttClusterMaker::InitRun( Int_t runnumber )
{ 
    mRunYear = ( runnumber + 727000 ) / 1000000 + 1999;

    LOG_INFO << "runnumber: " << runnumber << "  --> year: " << mRunYear << endm;

    return kStOk;
}

//_____________________________________________________________
Int_t
StFttClusterMaker::FinishRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttClusterMaker::Finish()
{ 
    return kStOk;
}


//_____________________________________________________________
Int_t
StFttClusterMaker::Make()
{ 
    mEvent = (StEvent*)GetInputDS("StEvent");
    if(mEvent) {
    } else {
        LOG_WARN<<"No StEvent!"<<endm;
        return kStWarn;
    }
    mFttCollection=mEvent->fttCollection();
    if(!mFttCollection) {
        LOG_WARN << "No StFttCollection" << endm;
        return kStOk;
    }

    mFttDb = static_cast<StFttDb*>(GetDataSet("fttDb"));
    assert( mFttDb );

    LOG_INFO << "Found " << mFttCollection->rawHits().size() << " Ftt Hits" << endm;
    ApplyHardwareMap();

    // InjectTestData();

    // next we need to sort the hits into 1D projections
    // process 1 quadrant (ROB) at a time,
    // process horizontal, vertical or diagonal strips one at a time

    // key == ROB
    std::map< UChar_t, std::vector<StFttRawHit *> > hStripsPerRob; // Horizontal
    std::map< UChar_t, std::vector<StFttRawHit *> > vStripsPerRob; // Vertical
    std::map< UChar_t, std::vector<StFttRawHit *> > dhStripsPerRob; // Diagonal on Horizontal
    std::map< UChar_t, std::vector<StFttRawHit *> > dvStripsPerRob; // Diagonal on Vertical

    size_t nStripsHit = 0;
    for ( StFttRawHit* hit : mFttCollection->rawHits() ) {
        UChar_t rob = mFttDb->rob( hit );
        UChar_t so = mFttDb->orientation( hit );

        // Apply the time cut
        if ( !PassTimeCut( hit ) ) continue;

        if ( kFttHorizontal == so ){
            hStripsPerRob[ rob ].push_back(hit);
            nStripsHit++;
        }
        if ( kFttVertical   == so ){
            vStripsPerRob[ rob ].push_back(hit);
            nStripsHit++;
        }
        if ( kFttDiagonalH   == so ){
            dhStripsPerRob[ rob ].push_back(hit);
            nStripsHit++;
        }
        if ( kFttDiagonalV   == so ){
            dvStripsPerRob[ rob ].push_back(hit);
            nStripsHit++;
        }
    } // loop on hit

    size_t nClusters = 0;
    LOG_INFO << "StFttClusterMaker::Make{ nStripsHit = " << nStripsHit << " }" << endm;
    if ( nStripsHit > 0 ){ // could make more strict?
        for ( UChar_t iRob = 1; iRob < StFttDb::nRob+1; iRob++ ){

            auto hClusters = FindClusters( hStripsPerRob[iRob] );
            // Add them to StEvent  
            for ( StFttCluster * clu : hClusters ){
                mFttCollection->addCluster( clu );
                nClusters++;
            }
            auto vClusters = FindClusters( vStripsPerRob[iRob] );
            // Add them to StEvent  
            for ( StFttCluster * clu : vClusters ){
                mFttCollection->addCluster( clu );
                nClusters++;
            }
            auto hdClusters = FindClusters( dhStripsPerRob[iRob] );
            // Add them to StEvent  
            for ( StFttCluster * clu : hdClusters ){
                mFttCollection->addCluster( clu );
                nClusters++;
            }
            auto vdClusters = FindClusters( dvStripsPerRob[iRob] );
            // Add them to StEvent  
            for ( StFttCluster * clu : vdClusters ){
                mFttCollection->addCluster( clu );
                nClusters++;
            }
        } // loop on iRob
    } // nStripsHit
    LOG_INFO << "Found " << nClusters << " clusters this event" << endm;

    return kStOk;
} // Make

void StFttClusterMaker::InjectTestData(){
    mFttCollection->rawHits().clear();

    StFttRawHit *hit = new StFttRawHit( 1, 1, 1, 1, 1, 55, 1, 1, 0 );
    hit->setMapping( 1, 1, 1, 23, kFttHorizontal ); // LEFT 2
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 90, 1, 1, 0 );
    hit->setMapping( 1, 1, 1, 24, kFttHorizontal ); // LEFT 1
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 60, 1, 1, 0 );
    hit->setMapping( 1, 1, 1, 27, kFttHorizontal );
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 95, 1, 1, 0 );
    hit->setMapping( 1, 1, 1, 25, kFttHorizontal ); // CENTER
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 93, 1, 1, 0 );
    hit->setMapping( 1, 1, 1, 26, kFttHorizontal );
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 19, 1, 1, 0 );
    hit->setMapping( 1, 1, 1, 28, kFttHorizontal );
    mFttCollection->addRawHit( hit );
} // InjectTestData


bool StFttClusterMaker::PassTimeCut( StFttRawHit * hit ){
    int time_cut0 = -50;
    int time_cut1 =  50;
    int time_cutm = 1;
    //  in principle it could vary VMM to VMM;
    // mFttDb->getTimeCut(hit, time_cutm, time_cut0, time_cut1);
    // LOG_INFO << "Time Cut: " << time_cutm << " , " << time_cut0 << ", " << time_cut1 << endm;
    if ( time_cutm == 0 ) // default, cut on bunch crossing
        return (hit->time() <= time_cut1 && hit->time() >= time_cut0); 

    // cut on timebin
    return (hit->tb() <= time_cut1 && hit->tb() >= time_cut0);
}


StFttRawHit * StFttClusterMaker::FindMaxAdc( std::vector<StFttRawHit *> hits, size_t &pos ){
    auto itMax = std::max_element(hits.begin(),
                             hits.end(),
                             [](const StFttRawHit* a,const StFttRawHit* b) { return a->adc() < b->adc(); });

    pos = (itMax - hits.begin());
    if ( itMax == hits.end() || pos >= hits.size() ) return nullptr;
    return *itMax;
}

void StFttClusterMaker::SearchClusterEdges( std::vector< StFttRawHit * > hits, 
                                            size_t start, // start index at MaxADC
                                            size_t &left, size_t &right ){
    // set initial values
    left     = start;
    right    = start;

    auto lastHitLeft  = hits[start];
    auto lastHitRight = hits[start];

    bool searchRight = true;
    bool searchLeft = true;

    StFttRawHit *hitLeft = nullptr, *hitRight = nullptr;

    while ( searchRight || searchLeft ){
            LOG_DEBUG << "LEFT: " << left << ", RIGHT: " << right <<  ", start = " << start << ", size=" << hits.size() << endm;
        if ( searchRight ){
            if ( right == hits.size() || right == hits.size() - 1 ){ 
                searchRight = false;
            }
            else {
                
                hitRight = hits[right+1];
                if ( hitRight->adc() > lastHitRight->adc() || hitRight->adc() < GetThresholdFor( hitRight ) ){
                    searchRight = false;
                }
                if ( hitRight->row() != lastHitRight->row() || abs( hitRight->strip() - lastHitRight->strip() ) > 1 ){
                    searchRight = false;
                }

                if ( searchRight ){
                    right ++;
                    lastHitRight = hitRight;
                }
            } // right < size - 1
        } // searchRight

        if ( searchLeft ){
            if ( left == 0 ){
                searchLeft = false;
            } else {
                hitLeft = hits[left-1];
                if ( hitLeft->adc() > lastHitLeft->adc() || hitLeft->adc() < GetThresholdFor( hitLeft ) ){
                    searchLeft = false;
                }
                if ( hitLeft->row() != lastHitLeft->row() || abs( hitLeft->strip() - lastHitLeft->strip() ) > 1 ){
                    searchLeft = false;
                }

                if (searchLeft){
                    left--;
                    lastHitLeft = hitLeft;
                }
            } // left != 0
        } // searchLeft
    } // while searching
} // SearchClusterEdges


void StFttClusterMaker::CalculateClusterInfo( StFttCluster * clu ){

    clu->setNStrips( clu->rawHits().size() );

    // Compute the sumAdc, strip gravity center, and variance
    float m0Sum = 0;
    float m1Sum = 0;
    float m2Sum = 0;

    std::for_each (clu->rawHits().begin(), clu->rawHits().end(), [&](const StFttRawHit *h) {
        float x = h->stripCenter(); // ideal position is ( h->strip() * 3.2 - 1.6 );
        m0Sum += h->adc();
        m1Sum += (h->adc() * x); 
        m2Sum += ( h->adc() * x * x );
    });

    if ( mDebug ) {
        LOG_DEBUG << "m0Sum = " << m0Sum << endm; 
        LOG_DEBUG << "m1Sum = " << m1Sum << endm;
        LOG_DEBUG << "m2Sum = " << m2Sum << endm;
    }

    // m0Sum = sumAdc
    // m1Sum / m0Sum = gravity center (1st moment)
    // m2Sum = accumulated variance (2nd moment)

    clu->setSumAdc( m0Sum );
    clu->setX( m1Sum / m0Sum );
    float var = (m2Sum - m1Sum*m1Sum / m0Sum) / m0Sum;
    clu->setSigma( sqrt( var ) );
}

std::vector<StFttCluster*> StFttClusterMaker::FindClusters( std::vector< StFttRawHit * > hits ){
    std::vector<StFttCluster*> clusters;

    /* early data (i.e. cosmic data pre dec 10 2021)
     * had duplicates where the hits are identical except 
     * a different tb. Tonko fixed at some point
     * So this could be wrapped in a run range block, but
     * does no harm.
     */
    const bool dedup = false;
    if ( dedup ){
        auto cmp = [](StFttRawHit* a, StFttRawHit* b) { 
            
            return  a->plane() < b->plane() ||
                    a->quadrant() < b->quadrant() ||
                    a->row() < b->row() ||
                    a->strip() < b->strip() ||
                    a->orientation() < b->orientation(); 
        };
    
        // NOTE according to SO this is faster than using ctor
        set<StFttRawHit*, decltype(cmp)> s(cmp);
        unsigned size = hits.size();
        for( auto h : hits ) s.insert( h );
        hits.assign( s.begin(), s.end() );
    }

    // Sort the hits by row and strip
    sort(hits.begin(), hits.end(), [](const StFttRawHit * a, const StFttRawHit * b) -> bool { 
            size_t indexA = a->orientation() + StFttDb::nStripOrientations * ( a->strip() + a->row() * StFttDb::maxStripPerRow);
            size_t indexB = b->orientation() + StFttDb::nStripOrientations * ( b->strip() + b->row() * StFttDb::maxStripPerRow);
            return indexA < indexB; 
        });

    if ( mDebug ) {
        LOG_DEBUG << "We have " << hits.size() << " hits after removing duplicates" << endm;
    }


    // Get the max ADC hit in this projection
    size_t anchor  = hits.size()+1;
    auto maxAdcHit = FindMaxAdc( hits, anchor );

    // Loop as long as there is at least 1 hit left
    while ( maxAdcHit ){
        StFttCluster * clu = new StFttCluster();

        if ( Debug() ){
            LOG_DEBUG << "CLUSTER FIND START WITH HITS:" << endm;
            size_t i = 0;
            for ( auto *h : hits ){
                LOG_DEBUG << "[" << i << "]" << *h;
                i++;
            }
        }

        // Set cluster "location" from max ADC hit
        clu->setPlane       ( maxAdcHit->plane       ( ) );
        clu->setQuadrant    ( maxAdcHit->quadrant    ( ) );
        clu->setRow         ( maxAdcHit->row         ( ) );
        clu->setOrientation ( maxAdcHit->orientation ( ) );
        clu->setIndexMaxStrip 
                            ( maxAdcHit->strip()         );
        clu->setMaxStripCenter
                            ( maxAdcHit->stripCenter()   );
        clu->setMaxStripLeftEdge
                            ( maxAdcHit->stripLeftEdge() );
        clu->setMaxStripRightEdge
                            ( maxAdcHit->stripRightEdge() );

        // Now find the cluster edges
        size_t left = anchor, right = anchor;
        SearchClusterEdges( hits, anchor, left, right);
        
        LOG_DEBUG << "Cluster points ( " << left << ", " << anchor << ", " << right << " )" << endm;
        
        
        // OK now add these hits to the cluster
        for ( size_t i = left; i < right + 1; i++ ){
            clu->addRawHit( hits[i] );
        }

        // Compute cluster information from the added hits
        CalculateClusterInfo( clu );

        if (mDebug){
            LOG_INFO << *clu << endm;;
        }
        clusters.push_back( clu );

        // Now erase all hits from this cluster so that we can move on to find the next one
        hits.erase( hits.begin() + left, hits.begin() + right + 1 );
        maxAdcHit = FindMaxAdc( hits, anchor );
    } // while maxAdcHit
    return clusters;
}



void StFttClusterMaker::ApplyHardwareMap(){
    for ( StFttRawHit* rawHit : mFttCollection->rawHits() ) {
        mFttDb->hardwareMap( rawHit );
    }

}

