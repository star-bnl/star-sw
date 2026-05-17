#include <utility>
#include <vector>
#include <map>
#include <array>
#include <algorithm>

#include "StEvent.h"
#include "StEnumerations.h"

#include "StFttClusterPointMaker.h"

#include "StEvent/StFttRawHit.h"
#include "StEvent/StFttCluster.h"
#include "StEvent/StFttPoint.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCollection.h"

#include "StFttDbMaker/StFttDb.h"

#include "tables/St_g2t_fts_hit_Table.h"

#include "TRandom.h"

StFttClusterPointMaker::StFttClusterPointMaker( const char* name )
: StMaker( name ),
  mEvent( 0 ),          /// pointer to StEvent
  mDebug( false ),       /// print out of all full messages for debugging
  mUseTestData( false ),
  mUseGeantData( false ),
  mFttDb( nullptr )
{
    LOG_DEBUG << "StFttClusterPointMaker::ctor"  << endm;
}

//_____________________________________________________________
StFttClusterPointMaker::~StFttClusterPointMaker()
{  /* no op */ }

//_____________________________________________________________
Int_t
StFttClusterPointMaker::Init()
{
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttClusterPointMaker::InitRun( Int_t runnumber )
{
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttClusterPointMaker::FinishRun( Int_t runnumber )
{
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttClusterPointMaker::Finish()
{
    return kStOk;
}

Int_t StFttClusterPointMaker::Make() {
    if ( mDebug ){
        LOG_INFO << "StFttClusterPointMaker::Make()" << endm;
    }

    mEvent = (StEvent*)GetInputDS("StEvent"); //get the event from stevent and make sure it exists
    if(mEvent) {
        if(mDebug){ LOG_DEBUG<<"Found StEvent"<<endm; }
    } else {
        LOG_WARN << "StFttClusterPointMaker::Make() - No StEvent found, cannot continue" << endm;
        return kStOk;
    }

    // if set to use as a fast sim, bypass real data mode
    if (mUseGeantData) {

        if ( mEvent->fttCollection() == nullptr ){
            LOG_INFO << "Creating FttCollection" << endm;
            StFttCollection *fttcollection = new StFttCollection();
            mEvent->setFttCollection(fttcollection);
        }

        mFttCollection=mEvent->fttCollection(); //get the ftt collection from the event and make sure it exists

        MakeGeantPoints(); // act as a slow-sim
        return kStOk;
    }

    

    mFttCollection=mEvent->fttCollection(); //get the ftt collection from the event and make sure it exists
    if(!mFttCollection) { return kStOk; }

    mFttDb = static_cast<StFttDb*>(GetDataSet("fttDbMkr")); //get the ftt database
    if ( !mFttDb ) {
        LOG_ERROR << "StFttClusterPointMaker::Make() - fttDbMkr dataset not found, cannot continue" << endm;
        return kStErr;
    }

    //clear cluster vector
    for (size_t i = 0; i < StFttDb::nRob; i++) {
        for (size_t j = 0; j < StFttDb::nStripOrientations; j++) {
            clustersPerRob[i][j].clear();
        }
    }

    //organize clusters by rob and orientation
    for ( StFttCluster* clu : mFttCollection->clusters() ) {
        // group clusters by rob (1-16, 4 quadrants of 4 sTGC planes) and strip orientation
        UChar_t rob = mFttDb->rob( clu );
        UChar_t orient = clu->orientation();
        if (mDebug){
            LOG_INFO << "rob = " << (int)rob << endm;
            LOG_INFO << "direction = " << (int)orient << endm;
            LOG_INFO << "cluster x = " << clu->x() << endm;
        }
        if ( rob >= StFttDb::nRob || orient >= StFttDb::nStripOrientations ) {
            LOG_WARN << "StFttClusterPointMaker: out-of-range rob=" << (int)rob
                     << " orientation=" << (int)orient << ", skipping cluster" << endm;
            continue;
        }
        clustersPerRob[ rob ][ orient ].push_back( clu );
    } // loop on cluster

    //loop over rob and make local points
    for (size_t i = 0; i < StFttDb::nRob; i++) {
        if (mDebug){
            LOG_INFO << "Now at ROB " << i << endm;
            LOG_INFO << "nCluster kFttVertical = " << clustersPerRob[ i ][ kFttVertical ].size() << endm; //vertical is vertical strips, clusters_x
            LOG_INFO << "nCluster kFttHorizontal = " << clustersPerRob[ i ][ kFttHorizontal ].size() << endm;
            LOG_INFO << "nCluster kFttDiagonalV = " << clustersPerRob[ i ][ kFttDiagonalV ].size() << endm;
            LOG_INFO << "nCluster kFttDiagonalH = " << clustersPerRob[ i ][ kFttDiagonalH ].size() << endm;
        }
        MakeLocalPoints((UChar_t)i);
    }

    //make the global points
    MakeGlobalPoints();
    LOG_INFO << "StFttClusterPointMaker made " << mFttCollection->numberOfPoints() << " points this event" << endm;

    return kStOk;
}

void StFttClusterPointMaker::InjectTestData(){
    mFttCollection->rawHits().clear();

    // TODO: inject clean strip hits to test cluster finder
    // StFttRawHit *hit = new StFttRawHit( sec, rdo, feb, vm, vmm[0].ch, vmm[0].adc, vmm[0].bcid, vmm[0].tb );
    // hit->setMapping( plane, quadrant, row, strip )
}

void StFttClusterPointMaker::MakeLocalPoints(UChar_t Rob) {
    if (mDebug){
         LOG_INFO << "Making local points for ROB " << (int)Rob << endm;
    }

    //initialize
    StFttPoint* point;
    size_t nClusters_X = 0;size_t nClusters_Y = 0;size_t nClusters_DX = 0;size_t nClusters_DY = 0;

    //set nClusters
    nClusters_X = clustersPerRob[(UChar_t)Rob][kFttVertical].size();
    nClusters_Y = clustersPerRob[(UChar_t)Rob][kFttHorizontal].size();
    nClusters_DX = clustersPerRob[(UChar_t)Rob][kFttDiagonalV].size();
    nClusters_DY = clustersPerRob[(UChar_t)Rob][kFttDiagonalH].size();

    if(mDebug)
    {
        LOG_INFO << "rob = " << (int)Rob << endm;
        LOG_INFO << "nClusterX = " << nClusters_X << " nClusterY = " << nClusters_Y << endm;
    }

    //loop over x clusters; clustersPerRob[Rob][kFttVertical][iClu_X]
    for (int iClu_X=0; iClu_X < nClusters_X; iClu_X++) {
        StFttCluster* clu_x = clustersPerRob[(UChar_t)Rob][kFttVertical][iClu_X];

        point = new StFttPoint();

        std::vector<std::vector<float>> covMatrix(2,std::vector<float>(2,0));
        covMatrix[0][0] = clu_x->sigma()*clu_x->sigma();
        covMatrix[1][1] = clu_x->maxStripLength()*clu_x->maxStripLength()/12.;
        covMatrix[0][1] = 0;
        covMatrix[1][0] = 0;

        point->setX( clu_x->x() );
        point->setY( mFttDb->YX_StripGroupEdge[clu_x->row()]+clu_x->maxStripLength()/2. );
        point->setSigmaX(clu_x->sigma());
        point->setSigmaY(clu_x->maxStripLength()/sqrt(12));
        point->setSigmaXY(0);
        point->setCov(covMatrix);

        point->setQuadrant( clu_x->quadrant() );
        point->setPlane( clu_x->plane() );
        point->addCluster(clu_x,kFttVertical);

        mFttCollection->addPoint(point);
    }

    //loop over y clusters; clustersPerRob[Rob][kFttHorizontal][iClu_Y]
    for (int iClu_Y=0; iClu_Y < nClusters_Y; iClu_Y++) {
        StFttCluster* clu_y = clustersPerRob[(UChar_t)Rob][kFttHorizontal][iClu_Y];

        point = new StFttPoint();

        std::vector<std::vector<float>> covMatrix(2,std::vector<float>(2,0));
        covMatrix[0][0] = clu_y->maxStripLength()*clu_y->maxStripLength()/12.;
        covMatrix[1][1] = clu_y->sigma()*clu_y->sigma();
        covMatrix[0][1] = 0;
        covMatrix[1][0] = 0;

        point->setX(  mFttDb->YX_StripGroupEdge[clu_y->row()]+clu_y->maxStripLength()/2. );
        point->setY( clu_y->x() );
        point->setSigmaX(clu_y->maxStripLength()/sqrt(12.));
        point->setSigmaY(clu_y->sigma());
        point->setSigmaXY(0);
        point->setCov(covMatrix);

        point->setQuadrant( clu_y->quadrant() );
        point->setPlane( clu_y->plane() );
        point->addCluster(clu_y,kFttHorizontal);

        mFttCollection->addPoint(point);
    }

    //loop over dx clusters; clustersPerRob[Rob][kFttDiagonalV][iClu_DX]
    for (int iClu_DX=0; iClu_DX<nClusters_DX; iClu_DX++){
        StFttCluster* clu_dx = clustersPerRob[(UChar_t)Rob][kFttDiagonalV][iClu_DX];

        point = new StFttPoint();

        double x_prime = clu_dx->x();
        double y_prime = clu_dx->maxStripLength()/2.; //needs to be + or - depending on row 3 or row 4
        if (clu_dx->row() == 3) {y_prime = -y_prime;}
        // row 4 keeps positive y_prime

        double xvar_prime = (clu_dx->sigma())*(clu_dx->sigma());
        double yvar_prime = clu_dx->maxStripLength()*clu_dx->maxStripLength()/12.;

        double x = mFttDb->D_StripGroupEdge[0]+(sqrt(2)/2)*(x_prime-y_prime);
        double y = mFttDb->D_StripGroupEdge[0]+(sqrt(2)/2)*(x_prime+y_prime);

        double C_diag = (xvar_prime + yvar_prime)/2;
        double C_off = (xvar_prime - yvar_prime)/2;

        std::vector<std::vector<float>> covMatrix(2,std::vector<float>(2,0));
        covMatrix[0][0] = C_diag;
        covMatrix[1][1] = C_diag;
        covMatrix[0][1] = C_off;
        covMatrix[1][0] = C_off;

        point->setX( x );
        point->setY( y );
        point->setSigmaX(sqrt(C_diag));
        point->setSigmaY(sqrt(C_diag));
        point->setSigmaXY(C_off);
        point->setCov(covMatrix);

        point->setQuadrant( clu_dx->quadrant() );
        point->setPlane( clu_dx->plane() );
        point->addCluster(clu_dx,kFttDiagonalV);

        mFttCollection->addPoint(point);
    }

    //loop over dy clusters; clustersPerRob[Rob][kFttDiagonalH][iClu_DY]
    for (int iClu_DY=0; iClu_DY<nClusters_DY; iClu_DY++){
        StFttCluster* clu_dy = clustersPerRob[(UChar_t)Rob][kFttDiagonalH][iClu_DY];

        point = new StFttPoint();

        double x_prime = clu_dy->x();
        double y_prime = clu_dy->maxStripLength()/2.;
        // row 3 keeps positive y_prime
        if (clu_dy->row() == 4) {y_prime = -y_prime;}

        double xvar_prime = (clu_dy->sigma())*(clu_dy->sigma());
        double yvar_prime = clu_dy->maxStripLength()*clu_dy->maxStripLength()/12.;

        double x = mFttDb->D_StripGroupEdge[0]+(sqrt(2)/2)*(x_prime-y_prime);
        double y = mFttDb->D_StripGroupEdge[0]+(sqrt(2)/2)*(x_prime+y_prime);

        double C_diag = (xvar_prime + yvar_prime)/2;
        double C_off = (xvar_prime - yvar_prime)/2;

        std::vector<std::vector<float>> covMatrix(2,std::vector<float>(2,0));
        covMatrix[0][0] = C_diag;
        covMatrix[1][1] = C_diag;
        covMatrix[0][1] = C_off;
        covMatrix[1][0] = C_off;

        point->setX( x );
        point->setY( y );
        point->setSigmaX(sqrt(C_diag));
        point->setSigmaY(sqrt(C_diag));
        point->setSigmaXY(C_off);
        point->setCov(covMatrix);

        point->setQuadrant( clu_dy->quadrant() );
        point->setPlane( clu_dy->plane() );
        point->addCluster(clu_dy,kFttDiagonalH);

        mFttCollection->addPoint(point);
    }
}

void StFttClusterPointMaker::MakeGlobalPoints() {
    LOG_INFO << "Making global points" << endm;
    for ( StFttPoint * p : mFttCollection->points() ){
        if (!p) {
            if (mDebug) {LOG_WARN << "StFttClusterPointMaker::MakeGlobalPoints - null point, skipping" << endm;}
            continue;
        }
        float x=p->x(); float y=p->y(); float z=0; //local coordinates
        float dx = 0, dy = 0, dz = 0; //offset to global coordiantes
        float sx = 1, sy = 1, sz = 1; //reflection to global coordinates

        mFttDb->getGloablOffset_ClusterPoint( p->plane(), p->quadrant(), dx, sx, dy, sy, dz, sz );

        StThreeVectorD global;
        // Scale by 10 to convert from mm to cm, since the geometry is in mm but the points are in cm
        // Z is already in cm since it is just the offset
        global.set( ((x*sx)+dx)/10.0, ((y*sy)+dy)/10.0, z+dz );

        if (p->quadrant() == 1 || p->quadrant() == 3) {
            p->setSigmaXY(-p->sigmaXY());
            std::vector<std::vector<float>> new_cov(2,std::vector<float>(2,0));
            new_cov[0][0] = p->cov()[0][0];
            new_cov[1][1] = p->cov()[1][1];
            new_cov[0][1] = -p->cov()[0][1];
            new_cov[1][0] = -p->cov()[1][0];
            p->setCov(new_cov);
        }

        if (mDebug) {
            LOG_INFO << "Global x: " << global.x() << " y: " << global.y() << " z: " << global.z() << endm;
        }

        p->setXYZ( global );

        if (mDebug) {
            LOG_INFO << "Point x: " << p->xyz().x() << " y: " << p->xyz().y() << " z: " << p->xyz().z() << endm;
            }
    }
}



void StFttClusterPointMaker::MakeGeantPoints() {
    LOG_INFO << "Making Geant points" << endm;

    // Aug 1, 2025
    // Note: this function does not do exactly the same thing as in the data
    // instead of looking up the complex detector layout, it just randomizes the hit location within
    // the strip length and assigns uncertainty. This simulates the effect of long strips.
    
    // Get the Geant hits
    St_g2t_fts_hit *geantFtt = (St_g2t_fts_hit *)GetDataSet("geant/g2t_stg_hit");
    if (!geantFtt) {
        LOG_WARN << "geant/g2t_stg_hit is empty" << endm;
        return;
    }

    std::map<std::pair<int, int>, int> track_vol_count;
    std::vector<std::vector<float>> covMatrix(2,std::vector<float>(2,0));
    for (int i = 0; i < geantFtt->GetNRows(); i++) {
        g2t_fts_hit_st *git = (g2t_fts_hit_st *)geantFtt->At(i);
        if (!git)
            continue; // invalid geant hit


        int track_id = git->track_p;
        int volume_id = git->volume_id;
        track_vol_count[ std::make_pair(track_id, volume_id) ] += 1;

        if ( track_vol_count[ std::make_pair(track_id, volume_id) ] > 1 ) {
            // skip hits from the same track on the same volume
            continue;
        }
        int plane_id = (volume_id - 1) / 100;           // from 1 - 16. four chambers per station
        // extract tens (10, 20, 30, 40) and units (0-9) from volume_id
        // volume_id = 10*plane_id + quadrant_id + 1
        // where plane_id is 0-3 and quadrant_id is 0-9
        int quadrant_id = ((volume_id - (100 * plane_id)) / 10) - 1; // 0 - 3
        int orientation = volume_id % 2;
        // only use the hits on the front modules
        // we will use front as vertical strips and back as horizontal strips, but need to check if this is correct
        
        // printf("Track ID: %d, Volume ID: %d, Plane ID: %d, Quad ID: %d, Orientation: %d\n", track_id, volume_id, plane_id, quadrant_id, (orientation));

        float x = git->x[0];// + gRandom->Gaus(0, sigXY); // 100 micron blur according to approx sTGC reso
        float y = git->x[1];// + gRandom->Gaus(0, sigXY); // 100 micron blur according to approx sTGC reso
        float z = git->x[2];

        auto point = new StFttPoint();
        point->setPlane(plane_id);
        point->setQuadrant(quadrant_id); // 0-3 for 4 quadrants
        point->setIdTruth(track_id);
        // point->setXYZ(StThreeVectorD(x, y, z));
        const double sigXY = 0.01; // 100 microns in cm, this is sigma in cluster measurement direction
        const double stripLength = 15.0; // cm, approximate length of the strip
        if ( orientation == 0 ) { // vertical strips
            // 100 microns in cms = 0.01 cm
            covMatrix[0][0] = sigXY * sigXY; // sigma^2 for x
            covMatrix[1][1] = (stripLength*stripLength)/12.; // along strips
            covMatrix[0][1] = 0;
            covMatrix[1][0] = 0;
            point->setX( x + gRandom->Gaus(0, sigXY) ); // add some noise
            point->setY( gRandom->Uniform(y - stripLength/2.0, y + stripLength/2.0) ); // randomize y within strip length
            // printf("Vertical Strip: x = %f, y = %f (geant x=%f, y=%f)\n", point->x(), point->y(), x, y);
        } else if ( orientation == 1 ) { // horizontal strips
            covMatrix[0][0] = (stripLength*stripLength)/12.; // along strips
            covMatrix[1][1] = sigXY * sigXY; // sigma^2 for y
            covMatrix[0][1] = 0;
            covMatrix[1][0] = 0;
            point->setX( gRandom->Uniform(x - stripLength/2.0, x + stripLength/2.0) ); // randomize x within strip length
            point->setY( y + gRandom->Gaus(0, sigXY) ); // add some noise
            // printf("Horizontal Strip: x = %f, y = %f (geant x=%f, y=%f)\n", point->x(), point->y(), x, y);
        } else {
            LOG_WARN << "Unknown orientation: " << orientation << endm;
            continue;
        }

        point->setCov(covMatrix);
        point->setXYZ(StThreeVectorD(point->x(), point->y(), z));

        mFttCollection->addPoint(point);

    } // loop on hits
    // for ( auto kv : track_vol_count ){
    //     printf( "track=%d, vol=%d => count = %d\n", kv.first.first, kv.first.second, kv.second);
    // }

}