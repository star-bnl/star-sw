/***************************************************************************
 *
 * StFttPointMaker.cxx
 * Author: jdb 2021
 ***************************************************************************
 *
 * Description: StFttPointMaker - class to fill the StFttPoint in StEvent
 *
 ***************************************************************************/
#include <vector>
#include <map>
#include <array>
#include <algorithm>


#include "StEvent.h"
#include "StEnumerations.h"

#include "StFttPointMaker.h"


#include "StEvent/StFttRawHit.h"
#include "StEvent/StFttCluster.h"
#include "StEvent/StFttPoint.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCollection.h"

#include "StFttDbMaker/StFttDb.h"


//_____________________________________________________________
StFttPointMaker::StFttPointMaker( const char* name )
: StMaker( name ),
  mEvent( 0 ),          /// pointer to StEvent
  mDebug( false ),       /// print out of all full messages for debugging
  mUseTestData( false ),
  mFttDb( nullptr )
{
    LOG_DEBUG << "StFttPointMaker::ctor"  << endm;
}

//_____________________________________________________________
StFttPointMaker::~StFttPointMaker()
{  /* no op */ }

//_____________________________________________________________
Int_t
StFttPointMaker::Init()
{
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttPointMaker::InitRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttPointMaker::FinishRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttPointMaker::Finish()
{ 
    return kStOk;
}


//_____________________________________________________________
Int_t
StFttPointMaker::Make()
{ 
    mEvent = (StEvent*)GetInputDS("StEvent");
    if(mEvent) {
        LOG_DEBUG<<"Found StEvent"<<endm;
    } else {
        return kStOk;
    }
    mFttCollection=mEvent->fttCollection();
    if(!mFttCollection) {
        return kStOk;
    }

    mFttDb = static_cast<StFttDb*>(GetDataSet("fttDb"));

    assert( mFttDb );

    if ( mUseTestData )
        InjectTestData();

    MakeLocalPoints();
    MakeGlobalPoints();

    LOG_INFO << "StFttPointMaker made " << mFttCollection->numberOfPoints() << " points this event" << endm;

    return kStOk;
}

void StFttPointMaker::InjectTestData(){
    mFttCollection->rawHits().clear();
    // TODO: inject clean strip hits to test cluster finder
    // should be empty for production code
}

void StFttPointMaker::MakeLocalPoints(){
    // next we will need them in even more detail
    // per strip group, but start here
    // key, dir, value is cluster
    // std::map< UChar_t, std::vector<StFttCluster *> > clustersPerRob[4][StFttDb::nRowsPerQuad];
    std::vector< StFttCluster *> clusters[StFttDb::nRob][StFttDb::nRowsPerQuad][StFttDb::nStripOrientations];

    for ( StFttCluster* clu : mFttCollection->clusters() ) {
        UChar_t rob = mFttDb->rob( clu );
        if ( clu->nStrips() < 2 ) continue;
        clusters[ rob ][ clu->row() ][ clu->orientation() ].push_back( clu );
    } // loop on hit

    
    for ( size_t iRob = 1; iRob < StFttDb::nRob; iRob ++ ){
        for ( size_t iRowH = 0; iRowH < 3; iRowH++ ){
            size_t nH = clusters[ iRob ][ iRowH ][ kFttHorizontal ].size();
            for ( size_t iRowV = 0; iRowV < 3; iRowV++ ){
                size_t nV = clusters[ iRob ][ iRowV ][ kFttVertical ].size();
                for ( size_t iH = 0; iH < nH; iH++ ){
                    auto cluH = clusters[ iRob ][ iRowH ][ kFttHorizontal ][ iH ];
                    for ( size_t iV = 0; iV < nV; iV++ ){
                        auto cluV = clusters[ iRob ][ iRowV ][ kFttVertical ][ iV ];

                        StFttPoint * p = makePoint( cluH, cluV );
                    } // iV
                } // iH
            } // iRowV
        } // iRowH
    } // iRob
} // MakeLocalPoints

void StFttPointMaker::MakeGlobalPoints() {
    for ( StFttPoint * p : mFttCollection->points() ){

        float x = p->x();
        float y = p->y();
        float z = 0.0;
        StThreeVectorD global;

        // dx is a local shift
        float dx = 0, dy = 0, dz = 0;
        // sx is only {1,-1} -> reflected or normal
        float sx = 0, sy = 0, sz = 0;
        mFttDb->getGloablOffset( p->plane(), p->quadrant(), dx, sx, dy, sy, dz, sz );
        global.set( (x + dx) * sx, (y + dy) * sy, (z + dz) * sz );
        p->setXYZ( global );
    }
}


StFttPoint * StFttPointMaker::makePoint( StFttCluster * cluH, StFttCluster * cluV, int mode ){

    float x = cluV->x();
    float y = cluH->x();

    // No pad sepearation
    if ( mode == 0 ) {
        StFttPoint * p = new StFttPoint();
        p->setPlane( cluV->plane() );
        p->setQuadrant( cluV->quadrant() );
        p->setX( x );
        p->setY( y );
        p->addCluster( cluH, kFttHorizontal );
        p->addCluster( cluV, kFttVertical );
        mFttCollection->addPoint(p);
        return p;
    } else if ( mode == 1 ){ // simple pad separation
        float hx1, hx2, hy1, hy2;
        clusterBounds( cluH, hx1, hy1, hx2, hy2 );
        float vx1, vx2, vy1, vy2;
        clusterBounds( cluV, vx1, vy1, vx2, vy2 );
        

        if ( y < vy1 || y > vy2 || x < hx1 || x > hx2 ){
            return nullptr;
        }
        
        StFttPoint * p = new StFttPoint();
        p->setPlane( cluV->plane() );
        p->setQuadrant( cluV->quadrant() );
        p->setX( x );
        p->setY( y );
        p->addCluster( cluH, kFttHorizontal );
        p->addCluster( cluV, kFttVertical );
        mFttCollection->addPoint(p);
        return p;
    }

    return nullptr;
} // makePoint

void StFttPointMaker::clusterBounds( StFttCluster* clu, float &x1, float &y1, float &x2, float &y2 ){
    // printf( "clusterBounds:" );
    const float rowWidth = 176;
    if ( clu->orientation() == kFttHorizontal ){
        y1 = y2 = clu->x();
        x1 = clu->row() * rowWidth; // rowWidth is a rough estimate of strip width
        x2 = (clu->row() + 1) * rowWidth; // rowWidth is a rough estimate of strip width
    }

    if ( clu->orientation() == kFttVertical ){
        x1 = x2 = clu->x();
        y1 = clu->row() * rowWidth; // rowWidth is a rough estimate of strip width
        y2 = (clu->row() + 1) * rowWidth; // rowWidth is a rough estimate of strip width
    }
} // cluster bounds

