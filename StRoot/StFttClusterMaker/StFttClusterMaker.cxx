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
    LOG_DEBUG << "StFttClusterMaker::ctor"  << endm;
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
    LOG_INFO << "StFttClusterMaker::Make()" << endm;


    mEvent = (StEvent*)GetInputDS("StEvent");
    if(mEvent) {
        LOG_DEBUG<<"Found StEvent"<<endm;
    } else {
        return kStOk;
    }
    mFttCollection=mEvent->fttCollection();
    if(!mFttCollection) {
        LOG_WARN << "No StFttCollection" << endm;
        return kStOk;
    }

    mFttDb = static_cast<StFttDb*>(GetDataSet("fttDb"));

    LOG_INFO << "Found " << mFttCollection->rawHits().size() << " Ftt Hits" << endm;
    assert( mFttDb );
    ApplyHardwareMap();

    

    // InjectTestData();

    // net we need to sort the hits into 1D projections
    // process 1 quadrant at a time,
    // process horizontal, vertical or diagonal strips one at a time

    // key == ROB
    std::map< UChar_t, std::vector<StFttRawHit *> > hStripsPerRob;
    std::map< UChar_t, std::vector<StFttRawHit *> > vStripsPerRob;
    std::map< UChar_t, std::vector<StFttRawHit *> > hdStripsPerRob;
    std::map< UChar_t, std::vector<StFttRawHit *> > vdStripsPerRob;

    size_t nStripsHit = 0;
    for ( StFttRawHit* hit : mFttCollection->rawHits() ) {
        UChar_t rob = mFttDb->rob( hit );
        UChar_t so = mFttDb->orientation( hit );

        LOG_INFO << "StFttRawHit with tb= " << hit->tb() << StFttDb::orientationLabels[ so ] << endm;
        // Apply the time cut
        if ( !PassTimeCut( hit ) ) continue;

        if ( kFttHorizontal == so ){
            hStripsPerRob[ rob ].push_back(hit);
            // LOG_INFO << "HORIZONTAL @ ROB = " << (int) rob << endm;
            nStripsHit++;
        }
        if ( kFttVertical   == so ){
            vStripsPerRob[ rob ].push_back(hit);
            // LOG_INFO << "VERTICAL @ ROB = " << (int) rob << endm;
            nStripsHit++;
        }
        if ( kFttDiagonalH   == so ){
            hdStripsPerRob[ rob ].push_back(hit);
            // LOG_INFO << "DIAGONAL @ ROB = " << (int) rob << endm;
            nStripsHit++;
        }
        if ( kFttDiagonalV   == so ){
            vdStripsPerRob[ rob ].push_back(hit);
            // LOG_INFO << "DIAGONAL @ ROB = " << (int) rob << endm;
            nStripsHit++;
        }
    } // loop on hit

    size_t nClusters = 0;
    LOG_INFO << "nStripsHit = " << nStripsHit << endm;
    if ( nStripsHit > 0 ){ // could make more strict?
        for ( UChar_t iRob = 1; iRob < StFttDb::nRob+1; iRob++ ){
            LOG_INFO << "ROB=" << (int)iRob << " has " << hStripsPerRob[iRob].size() << " horizontal, "
                << vStripsPerRob[iRob].size() << " vertical, "
                << hdStripsPerRob[iRob].size() << " diagonalH, "
                << vdStripsPerRob[iRob].size() << " diagonalV, "
                << " strips hit" << endm;

            auto hClusters = FindClusters( hStripsPerRob[iRob], (UChar_t)kFttHorizontal );
            // Add them to StEvent  
            for ( StFttCluster * clu : hClusters ){
                mFttCollection->addCluster( clu );
                nClusters++;
            }
            auto vClusters = FindClusters( vStripsPerRob[iRob], (UChar_t)kFttVertical );
            // Add them to StEvent  
            for ( StFttCluster * clu : vClusters ){
                mFttCollection->addCluster( clu );
                nClusters++;
            }
            auto hdClusters = FindClusters( hdStripsPerRob[iRob], (UChar_t)kFttDiagonalH );
            // Add them to StEvent  
            for ( StFttCluster * clu : hdClusters ){
                mFttCollection->addCluster( clu );
                nClusters++;
            }
            auto vdClusters = FindClusters( vdStripsPerRob[iRob], (UChar_t)kFttDiagonalV );
            // Add them to StEvent  
            for ( StFttCluster * clu : vdClusters ){
                mFttCollection->addCluster( clu );
                nClusters++;
            }
        } // loop on iRob
    } // nStripsHit
    LOG_INFO << "Found " << nClusters << " clusters this event" << endm;

    return kStOk;
}

void StFttClusterMaker::InjectTestData(){
    mFttCollection->rawHits().clear();

    // TODO: inject clean strip hits to test cluster finder

    StFttRawHit *hit = new StFttRawHit( 1, 1, 1, 1, 1, 55, 1, 1 );
    hit->setMapping( 1, 1, 1, 23, kFttHorizontal ); // LEFT 2
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 90, 1, 1 );
    hit->setMapping( 1, 1, 1, 24, kFttHorizontal ); // LEFT 1
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 60, 1, 1 );
    hit->setMapping( 1, 1, 1, 27, kFttHorizontal );
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 95, 1, 1 );
    hit->setMapping( 1, 1, 1, 25, kFttHorizontal ); // CENTER
    mFttCollection->addRawHit( hit );

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 93, 1, 1 );
    hit->setMapping( 1, 1, 1, 26, kFttHorizontal );
    mFttCollection->addRawHit( hit );

    

    hit = new StFttRawHit( 1, 1, 1, 1, 1, 19, 1, 1 );
    hit->setMapping( 1, 1, 1, 28, kFttHorizontal );
    mFttCollection->addRawHit( hit );


}


bool StFttClusterMaker::PassTimeCut( StFttRawHit * hit ){
    return ( hit->tb() > -70 && hit->tb() < 20 ); // about +/- 5 sigma based on RUN 22338015
}


StFttRawHit * StFttClusterMaker::FindMaxAdc( std::vector<StFttRawHit *> hits, size_t &pos ){
    auto itMax = std::max_element(hits.begin(),
                             hits.end(),
                             [](const StFttRawHit* a,const StFttRawHit* b) { return a->adc() < b->adc(); });

    // LOG_INFO << "FindMaxAdc, pos = " << (pos) << endm; 
    pos = (itMax - hits.begin());
    if ( pos >= hits.size() )
        return nullptr;
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
        LOG_INFO << "LEFT: " << left << ", RIGHT: " << right <<  ", start = " << start << ", size=" << hits.size() << endm;
        if ( searchRight ){
            if ( right == hits.size() || right == hits.size() - 1 ){ 
                LOG_INFO << "END SEARCH RIGHT EDGE" << endm;
                searchRight = false;
            }
            else {
                
                hitRight = hits[right+1];
                
                if ( hitRight->adc() > lastHitRight->adc() || hitRight->adc() < GetThresholdFor( hitRight ) ){
                    LOG_INFO << "END SEARCH RIGHT ADC" << endm;
                    searchRight = false;
                }
                if ( hitRight->row() != lastHitRight->row() || abs( hitRight->strip() - lastHitRight->strip() ) > 1 ){
                    LOG_INFO << "END SEARCH RIGHT STRIP" << endm;
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
                LOG_INFO << "END SEARCH LEFT EDGE" << endm;
                searchLeft = false;
            } else {
                hitLeft = hits[left-1];
                if ( hitLeft->adc() > lastHitLeft->adc() || hitLeft->adc() < GetThresholdFor( hitLeft ) ){
                    LOG_INFO << "END SEARCH LEFT ADC" << endm;
                    searchLeft = false;
                }
                if ( hitLeft->row() != lastHitLeft->row() || abs( hitLeft->strip() - lastHitLeft->strip() ) > 1 ){
                    LOG_INFO << "END SEARCH LEFT STRIP" << endm;
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

    if ( mDebug ){
        LOG_INFO << "sumAdc = ";
    }
    std::for_each (clu->rawHits().begin(), clu->rawHits().end(), [&](const StFttRawHit *h) {
        float x = ( h->strip() * 3.2 - 1.6 ); // replace with CONST
        
        if ( mDebug ){
            LOG_INFO << " + " << h->adc();
        }
        m0Sum += h->adc();
        m1Sum += (h->adc() * x); 
        m2Sum += ( h->adc() * x * x );
    });

    if ( mDebug ) {
        LOG_INFO << " = " << m0Sum << endm; 
        LOG_INFO << "m1Sum = " << m1Sum << ", m2Sum = " << m2Sum << endm;
    }

    // m0Sum = sumAdc
    // m1Sum / m0Sum = gravity center (1st moment)
    // m2Sum = accumulated variance (2nd moment)

    clu->setSumAdc( m0Sum );
    clu->setX( m1Sum / m0Sum );
    float var = (m2Sum - m1Sum*m1Sum / m0Sum) / m0Sum;
    clu->setSigma( sqrt( var ) );
}



std::vector<StFttCluster*> StFttClusterMaker::FindClusters( std::vector< StFttRawHit * > hits, UChar_t stripOrientattion ){
    std::vector<StFttCluster*> clusters;
    LOG_INFO << "FindClusters( std::vector< StFttRawHit * > hits, UChar_t stripOrientattion )" << endm;

    

    if ( mDebug ){
        LOG_INFO << "We have " << hits.size() << " hits with dups" << endm;
    }

    bool dedup = true;

    if ( dedup ){
        auto cmp = [](StFttRawHit* a, StFttRawHit* b) { 
            
            return  a->plane() < b->plane() ||
                    a->quadrant() < b->quadrant() ||
                    a->row() < b->row() ||
                    a->strip() < b->strip(); 
        };
    
        // NOTE according to SO this is faster than using ctor
        set<StFttRawHit*, decltype(cmp)> s(cmp);
        unsigned size = hits.size();
        for( auto h : hits ) s.insert( h );
        hits.assign( s.begin(), s.end() );
    }

    // Sort the hits by row and strip
    sort(hits.begin(), hits.end(), [](const StFttRawHit * a, const StFttRawHit * b) -> bool { 
            size_t indexA = a->strip() + a->row() * StFttDb::maxStripPerRow;
            size_t indexB = b->strip() + b->row() * StFttDb::maxStripPerRow;
            return indexA < indexB; 
        });

    if ( mDebug ) {
        LOG_INFO << "We have " << hits.size() << " hits after removing dups" << endm;
    }

    size_t anchor  = hits.size()+1;
    auto maxAdcHit = FindMaxAdc( hits, anchor );

    while ( maxAdcHit ){
        StFttCluster * clu = new StFttCluster();

        LOG_INFO << "CLUSTER FIND START WITH HITS:" << endm;
        if ( mDebug ){
            size_t i = 0;
            for ( auto *h : hits ){
                LOG_INFO << "[" << i << "]" << *h;
                i++;
            }
        }

        // Set "location" from max ADC hit
        clu->setPlane( maxAdcHit->plane() );
        clu->setQuadrant( maxAdcHit->quadrant() );
        clu->setRow( maxAdcHit->row() );
        clu->setOrientation( maxAdcHit->orientation() );

        // Now find the cluster edges
        size_t left = anchor, right = anchor;
        SearchClusterEdges( hits, anchor, left, right);
        LOG_INFO << "Cluster points ( " << left << ", " << anchor << ", " << right << " )" << endm;
        
        // OK now add these hits to the cluster
        for ( size_t i = left; i < right + 1; i++ ){
            clu->addRawHit( hits[i] );
        }

        // Compute cluster information from the added hits
        CalculateClusterInfo( clu );

        LOG_INFO << *clu << endm;;
        clusters.push_back( clu );

        // Now erase all hits from this cluster so that we can move on to find the next one
        hits.erase( hits.begin() + left, hits.begin() + right + 1 );
        maxAdcHit = FindMaxAdc( hits, anchor );
    }


    return clusters;
}



void StFttClusterMaker::ApplyHardwareMap(){
    for ( StFttRawHit* rawHit : mFttCollection->rawHits() ) {
        mFttDb->hardwareMap( rawHit );
    }

    // HARDWARE MAP CHECK
    // mSector = 4
    // mRDO = 4
    // mFEB = 3
    // mVMM = 1
    // mChannel = 46
    // mADC = 1023
    // mBCID = 360
    // mTB = -29
    StFttRawHit * testHit = new StFttRawHit( 4, 4, 3, 1, 46, 1023, 360, -29 );
    LOG_INFO << "---------HARDWARE MAP TEST: ---------" << endm;
    mFttDb->hardwareMap( testHit );
    LOG_INFO << *testHit << endm;

    
    LOG_INFO << "---------HARDWARE MAP TEST: ---------" << endm;
    StFttRawHit * testHit2 = new StFttRawHit( 4, 4, 2, 2, 11, 1023, 360, -29 );
    mFttDb->hardwareMap( testHit2 );
    LOG_INFO << *testHit2 << endm;
}