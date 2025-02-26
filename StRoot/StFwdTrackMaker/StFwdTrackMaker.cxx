#include "StFwdTrackMaker/StFwdTrackMaker.h"
#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StFwdTrackMaker/include/Tracker/FwdTracker.h"
#include "StFwdTrackMaker/include/Tracker/TrackFitter.h"
#include "StFwdTrackMaker/include/Tracker/FwdGeomUtils.h"
#include "StFwdTrackMaker/include/Tracker/ObjExporter.h"

#include "KiTrack/IHit.h"
#include "GenFit/Track.h"

#include "TMath.h"

#include <climits>
#include <map>
#include <string>
#include <string>
#include <vector>

#include "StBFChain/StBFChain.h"

#include "StEvent/StEvent.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StHelixModel.h"
#include "StEvent/StPrimaryTrack.h"
#include "StEvent/StRnDHit.h"
#include "StEvent/StRnDHitCollection.h"
#include "StEvent/StTrack.h"
#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofHeader.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StTrackNode.h"
#include "StEvent/StPrimaryVertex.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StFttPoint.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFttCollection.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StFstHitCollection.h"
#include "StEvent/StFstHit.h"
#include "StEvent/StFwdTrackCollection.h"
#include "StChain/StChainOpt.h"

#include "StEventUtilities/StEventHelper.h"

#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"

#include "tables/St_g2t_fts_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_event_Table.h"

#include "StarMagField/StarMagField.h"

#include "St_base/StMessMgr.h"
#include "StarClassLibrary/StPhysicalHelix.hh"
#include "StarClassLibrary/SystemOfUnits.h"

#include <SystemOfUnits.h>
#include <exception>

#include "TROOT.h"
#include "TLorentzVector.h"

#include "StRoot/StEpdUtil/StEpdGeom.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "StFstUtil/StFstCollection.h"

#include "StEvent/StFwdTrack.h"
#include "GenFit/AbsMeasurement.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuFstCollection.h"
#include "StMuDSTMaker/COMMON/StMuFstHit.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "sys/types.h"
#include "sys/sysinfo.h"

FwdSystem* FwdSystem::sInstance = nullptr;

//_______________________________________________________________________________________
class GenfitUtils{
    public:

    // For now, accept anything we are passed, no matter what it is or how bad it is
    template<typename T> static bool accept( T ) { return true; }
}; // GenfitUtils

// Basic sanity cuts on genfit tracks
template<> bool GenfitUtils::accept( genfit::Track *track )
{
    // This also gets rid of failed fits (but may need to explicitly
    // for fit failure...)
    if (track->getNumPoints() <= 0 ) return false; // fit may have failed

    auto cardinal = track->getCardinalRep();

    // Check that the track fit converged
    auto status = track->getFitStatus( cardinal );
    if ( !status->isFitConverged() ) {
    return false;
    }


    // Next, check that all points on the track have fitter info
    // (may be another indication of a failed fit?)
    for ( auto point : track->getPoints() ) {
    if ( !point->hasFitterInfo(cardinal) ) {
    return false;
    }
    }

    // Following line fails with an exception, because some tracks lack
    //   forward update, or prediction in fitter info at the first point
    //
    // genfit::KalmanFitterInfo::getFittedState(bool) const of
    //                         GenFit/fitters/src/KalmanFitterInfo.cc:250

    // Fitted state at the first point
    // const auto &atFirstPoint = track->getFittedState();

    // Getting the fitted state from a track occasionally fails, because
    // the first point on the fit doesn't have forward/backward fit
    // information.  So we want the first point with fit info...

    genfit::TrackPoint* first = nullptr;
    unsigned int ipoint = 0;
    for ( ipoint = 0; ipoint < track->getNumPoints(); ipoint++ ) {
    first = track->getPointWithFitterInfo( ipoint );
    if ( first ) break;
    }

    // No points on the track have fit information
    if ( !first ) {
        LOG_WARN << "No fit information on fwd genfit track" << endm;
        return false;
    }

    auto& fittedState= track->getFittedState(ipoint);

    TVector3 momentum = fittedState.getMom();
    double   pt       = momentum.Perp();

    if (pt < 0.10 ) return false; // below this

    return true;

};

//______________________________________________________________________________________
class SiRasterizer {
  public:
    SiRasterizer() {}
    SiRasterizer(FwdTrackerConfig &_cfg) { setup(_cfg); }
    ~SiRasterizer() {}
    void setup(FwdTrackerConfig &_cfg) {
        cfg = _cfg;
        mRasterR = cfg.get<double>("SiRasterizer:r", 3.0);
        mRasterPhi = cfg.get<double>("SiRasterizer:phi", 0.004);
    }

    bool active() {
        return cfg.get<bool>("SiRasterizer:active", true);
    }

    TVector3 raster(TVector3 p0) {
        TVector3 p = p0;
        double r = p.Perp();
        double phi = p.Phi();
        const double minR = 5.0;
        // 5.0 is the r minimum of the Si
        p.SetPerp(minR + (std::floor((r - minR) / mRasterR) * mRasterR + mRasterR / 2.0));
        p.SetPhi(-TMath::Pi() + (std::floor((phi + TMath::Pi()) / mRasterPhi) * mRasterPhi + mRasterPhi / 2.0));
        return p;
    }

    FwdTrackerConfig cfg;
    double mRasterR, mRasterPhi;
};

//  Wrapper class around the forward tracker
class ForwardTracker : public ForwardTrackMaker {
  public:
    // Replaces original initialization.  Config file and hitloader
    // will be provided by the maker.
    void initialize( TString geoCache, bool genHistograms ) {
        nEvents = 1; // only process single event

        // Create the forward system...
        FwdSystem::sInstance = new FwdSystem();

        // initialize the track fitter
        mTrackFitter = new TrackFitter(mConfig, geoCache);
        mTrackFitter->setup();

        ForwardTrackMaker::initialize( geoCache, genHistograms );
    }

    void finish() {

        if (FwdSystem::sInstance){
            delete FwdSystem::sInstance;
            FwdSystem::sInstance = 0;
        }
        if (mTrackFitter){
            delete mTrackFitter;
            mTrackFitter= 0;
        }
    }
};

//________________________________________________________________________
StFwdTrackMaker::StFwdTrackMaker() : StMaker("fwdTrack"), mEventVertex(0,0,0), mForwardTracker(nullptr), mForwardData(nullptr), mGeoCache(""){
    
    
    mEventVertexCov.ResizeTo(3, 3);
    mEventVertexCov.Zero();
    
    SetAttr("useFtt",1);                 // Default Ftt on
    SetAttr("useFst",1);                 // Default Fst on
    SetAttr("useFcs",1);                 // Default Fcs on
    SetAttr("config", "config.xml");     // Default configuration file (user may override before Init())
    SetAttr("fillEvent",1); // fill StEvent

    // Load the default configuration
    configLoaded = false;
    LoadConfiguration();

    // set additional default configuration values
    setOutputFilename( "stfwdtrackmaker_data.root" );

    
};

int StFwdTrackMaker::Finish() {
    mForwardTracker->finish();
    return kStOk;
}

void StFwdTrackMaker::LoadConfiguration() {
    if (mConfigFile.length() < 5){
        // no config file specified, use default
        // 5 characters is the minimum length for a valid filename since we must have at least .xml
        mFwdConfig.load( defaultConfig, true );
    } else {
        LOG_INFO << "Forward Tracker is using config from file : " <<  mConfigFile << endm;
        mFwdConfig.load( mConfigFile );
    }
    configLoaded = true;
}

//________________________________________________________________________
int StFwdTrackMaker::Init() {
    
    if ( mGeoCache == "" ){
        /// Instantiate and cache the geometry
        GetDataBase("VmcGeometry");

        mGeoCache = GetChainOpt()->GetFileOut();
        if ( mGeoCache=="" )
            mGeoCache = GetChainOpt()->GetFileIn();

        // Strip out @ symbol
        mGeoCache = mGeoCache.ReplaceAll("@","");
        // Strip off the last extention in the mGeoCache
        mGeoCache = mGeoCache( 0, mGeoCache.Last('.') );
        // Append geom.root to the extentionless mGeoCache
        mGeoCache+=".geom.root";
    }
    // create an SiRasterizer in case we need it
    mSiRasterizer = std::shared_ptr<SiRasterizer>( new SiRasterizer(mFwdConfig));
    mForwardTracker = std::shared_ptr<ForwardTracker>(new ForwardTracker( ));
    mForwardTracker->setConfig(mFwdConfig);

    // in production we disable crit saving.
    mForwardTracker->setSaveCriteriaValues(false);

    mForwardData = std::shared_ptr<FwdDataSource>(new FwdDataSource());
    mForwardTracker->setData(mForwardData);
    mForwardTracker->initialize( mGeoCache, false );

    // geometry should be available from here (mForwardTracker will initialize cache if needed)
    if (gGeoManager) {
        FwdGeomUtils fwdGeoUtils( gGeoManager );
        // get the z-locations from geometry model and fallback to the defaults
        auto fstZ = fwdGeoUtils.fstZ( {151.750000, 165.248001, 178.781006} );
        mFstZFromGeom.assign( fstZ.begin(), fstZ.end() );
        auto fttZ = fwdGeoUtils.fttZ( {280.904999, 303.704987, 326.605011, 349.404999} );
        mFttZFromGeom.assign( fttZ.begin(), fttZ.end() );
    }
    return kStOK;
};

TMatrixDSym makeSiCovMat(TVector3 hit, FwdTrackerConfig &xfg) {
    // we can calculate the CovMat since we know the det info, but in future we should probably keep this info in the hit itself

    float rSize = xfg.get<float>("SiRasterizer:r", 3.0);
    float phiSize = xfg.get<float>("SiRasterizer:phi", 0.004);

    // measurements on a plane only need 2x2
    // for Si geom we need to convert from cylindrical to cartesian coords
    TMatrixDSym cm(2);
    TMatrixD T(2, 2);
    TMatrixD J(2, 2);
    const float x = hit.X();
    const float y = hit.Y();
    const float R = sqrt(x * x + y * y);
    const float cosphi = x / R;
    const float sinphi = y / R;
    const float sqrt12 = sqrt(12.);

    const float dr = rSize / sqrt12;
    const float dphi = (phiSize) / sqrt12;

    // Setup the Transposed and normal Jacobian transform matrix;
    // note, the si fast sim did this wrong
    // row col
    T(0, 0) = cosphi;
    T(0, 1) = -R * sinphi;
    T(1, 0) = sinphi;
    T(1, 1) = R * cosphi;

    J(0, 0) = cosphi;
    J(0, 1) = sinphi;
    J(1, 0) = -R * sinphi;
    J(1, 1) = R * cosphi;

    TMatrixD cmcyl(2, 2);
    cmcyl(0, 0) = dr * dr;
    cmcyl(1, 1) = dphi * dphi;

    TMatrixD r = T * cmcyl * J;

    // note: float sigmaX = sqrt(r(0, 0));
    // note: float sigmaY = sqrt(r(1, 1));

    cm(0, 0) = r(0, 0);
    cm(1, 1) = r(1, 1);
    cm(0, 1) = r(0, 1);
    cm(1, 0) = r(1, 0);

    TMatrixDSym tamvoc(3);
    tamvoc( 0, 0 ) = cm(0, 0); tamvoc( 0, 1 ) = cm(0, 1); tamvoc( 0, 2 ) = 0.0;
    tamvoc( 1, 0 ) = cm(1, 0); tamvoc( 1, 1 ) = cm(1, 1); tamvoc( 1, 2 ) = 0.0;
    tamvoc( 2, 0 ) = 0.0;      tamvoc( 2, 1 ) = 0.0; tamvoc( 2, 2 )      = 0.01*0.01;


    return tamvoc;
}

void StFwdTrackMaker::loadFttHits( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap, int count ){
    LOG_DEBUG << "Loading FTT Hits" << endm;
    // Get the StEvent handle to see if the rndCollection is available
    StEvent *event = (StEvent *)GetDataSet("StEvent");
    string fttFromSource = mFwdConfig.get<string>( "Source:ftt", "DATA" );

    if (!event){
        LOG_ERROR << "No StEvent, cannot load Ftt Data" << endm;
        return;
    }

    StFttCollection *col = event->fttCollection();
    // From Data
    if ( col || "DATA" == fttFromSource ) {
        loadFttHitsFromStEvent( mcTrackMap, hitMap, count );
        return;
    }

    // Load GEANT hits directly if requested
    if ( true ) {
        LOG_DEBUG << "Try loading sTGC hits directly from GEANT hits" << endm;
        loadFttHitsFromGEANT( mcTrackMap, hitMap, count );
        return;
    }
} // loadFttHits

void StFwdTrackMaker::loadFttHitsFromStEvent( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap, int count ){
    LOG_DEBUG << "Loading FTT Hits from Data" << endm;
    StEvent *event = (StEvent *)GetDataSet("StEvent");
    StFttCollection *col = event->fttCollection();
    size_t numFwdHitsPrior = mFwdHitsFtt.size();

    if ( col && col->numberOfPoints() > 0 ){
        LOG_DEBUG << "The Ftt Collection has " << col->numberOfPoints() << " points" << endm;
        TMatrixDSym hitCov3(3);
        const double sigXY = 0.2; //
        hitCov3(0, 0) = sigXY * sigXY;
        hitCov3(1, 1) = sigXY * sigXY;
        hitCov3(2, 2) = 4; // unused since they are loaded as points on plane
        static const double mm_to_cm = 0.1;
        for ( auto point : col->points() ){

            float xcm = point->xyz().x()*mm_to_cm;
            float ycm = point->xyz().y()*mm_to_cm;
            float zcm = point->xyz().z();

            // get the track id
            int track_id = point->idTruth();
            shared_ptr<McTrack> mcTrack = nullptr;
            if ( mcTrackMap.count(track_id) ) {
                mcTrack = mcTrackMap[track_id];
                LOG_DEBUG << "Adding McTrack to FTT hit: " << track_id << endm;
            }

            mFwdHitsFtt.push_back(FwdHit(count++, // id
                xcm, ycm, zcm,
                -point->plane(), // volume id
                kFttId, // detid
                track_id, // track id
                hitCov3, // covariance matrix
                mcTrack) // mcTrack
                );
            mFttHits.push_back( TVector3( xcm, ycm, zcm)  );
        } // end of loop over points
    } else {
        LOG_DEBUG << "The Ftt Collection is EMPTY points" << endm;
    }

    // this has to be done AFTER because the vector reallocates mem when expanding, changing addresses
    size_t numFwdHitsPost = mFwdHitsFtt.size();
    for ( size_t iFwdHit = numFwdHitsPrior; iFwdHit < numFwdHitsPost; iFwdHit++){
        FwdHit *hit = &(mFwdHitsFtt[ iFwdHit ]);
        // Add the hit to the hit map
        if ( hit->getLayer() >= 0 )
            hitMap[hit->getSector()].push_back(hit);
        // add to MC track map
        if ( hit->getMcTrack() ){
            hit->getMcTrack()->addFttHit(hit);
        }
    }

    if ( numFwdHitsPost != numFwdHitsPrior ){
        LOG_INFO << "Loaded " << numFwdHitsPost - numFwdHitsPrior << " FTT hits from StEvent" << endm;
    }
}

void StFwdTrackMaker::loadFttHitsFromGEANT( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap, int count ){
    /************************************************************/
    // STGC Hits
    St_g2t_fts_hit *g2t_stg_hits = (St_g2t_fts_hit *)GetDataSet("geant/g2t_stg_hit");

    size_t numFwdHitsPrior = mFwdHitsFtt.size();
    if (!g2t_stg_hits){
        LOG_WARN << "geant/g2t_stg_hit is empty" << endm;
        return;
    }

    // make the Covariance Matrix once and then reuse
    TMatrixDSym hitCov3(3);
    const double sigXY = 0.02;
    hitCov3(0, 0) = sigXY * sigXY;
    hitCov3(1, 1) = sigXY * sigXY;
    hitCov3(2, 2) = 0.1; // unused since they are loaded as points on plane

    int nstg = g2t_stg_hits->GetNRows();

    LOG_DEBUG << "This event has " << nstg << " stg hits in geant/g2t_stg_hit " << endm;

    bool filterGEANT = mFwdConfig.get<bool>( "Source:fttFilter", false );
    for (int i = 0; i < nstg; i++) {

        g2t_fts_hit_st *git = (g2t_fts_hit_st *)g2t_stg_hits->At(i);
        if (0 == git)
            continue; // geant hit
        int track_id = git->track_p;
        int volume_id = git->volume_id;
        int plane_id = (volume_id - 1) / 100;           // from 1 - 16. four chambers per station

        // only use the hits on the front modules
        if ( volume_id % 2 ==0 )
            continue;

        float x = git->x[0];// + gRandom->Gaus(0, sigXY); // 100 micron blur according to approx sTGC reso
        float y = git->x[1];// + gRandom->Gaus(0, sigXY); // 100 micron blur according to approx sTGC reso
        float z = git->x[2];

        if (plane_id < 0 || plane_id >= 4) {
            continue;
        }
        mFwdHitsFtt.push_back(
            FwdHit(
                count++, // id
                x, y, z, // position
                -plane_id, // volume id
                kFttId, // detid
                track_id, // track id
                hitCov3, // covariance matrix
                mcTrackMap[track_id] // mcTrack
                )
            );
        mFttHits.push_back( TVector3( x, y, z )  );
    } // loop on hits

    // this has to be done AFTER because the vector reallocates mem when expanding, changing addresses
    size_t numFwdHitsPost = mFwdHitsFtt.size();
    for ( size_t iFwdHit = numFwdHitsPrior; iFwdHit < numFwdHitsPost; iFwdHit++){
        FwdHit *hit = &(mFwdHitsFtt[ iFwdHit ]);
        // Add the hit to the hit map
        if ( hit->getLayer() >= 0 )
            hitMap[hit->getSector()].push_back(hit);

        if ( dynamic_cast<FwdHit*>(hit)->_mcTrack ){
            dynamic_cast<FwdHit*>(hit)->_mcTrack->addFttHit(hit);
        }
    }

    if ( numFwdHitsPost != numFwdHitsPrior ){
        LOG_INFO << "Loaded " << numFwdHitsPost - numFwdHitsPrior << " FST hits from MuDst" << endm;
    }

} // loadFttHits

/**
 * @brief Loads FST hits from various sources into the hitmap and McTrackMap (if availabale)
 *
 * Order of precedence:
 * MuDst StMuFstCollection (Data)
 * StEvent StFstHitCollection (Data or slowsim)
 * StEvent StRndHitCollection (fast sim)
 * GEANT St_g2t_fts_hit (starsim only) - note if rasterizer is active this takes priority over FastSim
 *
 * @param mcTrackMap : MC track map if running sim
 * @param hitMap : FST hitmap to populate
 * @param count  : number of hits loaded
 */
int StFwdTrackMaker::loadFstHits( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap ){

    int count = loadFstHitsFromMuDst(mcTrackMap, hitMap);
    if ( count > 0 ) return count; // only load from one source at a time

    count += loadFstHitsFromStEvent(mcTrackMap, hitMap);
    if ( count > 0 ) return count; // only load from one source at a time

    bool siRasterizer = mFwdConfig.get<bool>( "SiRasterizer:active", false );

    if ( !siRasterizer ) count += loadFstHitsFromStRnDHits( mcTrackMap, hitMap );
    if ( count > 0 ) return count; // only load from one source at a time

    return loadFstHitsFromGEANT( mcTrackMap, hitMap );
} // loadFstHits

int StFwdTrackMaker::loadFstHitsFromMuDst( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap){
    int count = 0;
    StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
    if(!mMuDstMaker) {
        LOG_WARN << " No MuDstMaker ... bye-bye" << endm;
        return 0;
    }
    StMuDst *mMuDst = mMuDstMaker->muDst();
    if(!mMuDst) {
        LOG_WARN << " No MuDst ... bye-bye" << endm;
        return 0;
    }

    StMuFstCollection * fst = mMuDst->muFstCollection();
    if (!fst) {
        LOG_WARN << "No StMuFstCollection ... bye-bye" << endm;
        return 0;
    }

    size_t numFwdHitsPrior = mFwdHitsFst.size();
    LOG_INFO << "Loading " << fst->numberOfHits() << " StMuFstHits" << endm;
    TMatrixDSym hitCov3(3);
    for ( unsigned int index = 0; index < fst->numberOfHits(); index++){
        StMuFstHit * muFstHit = fst->getHit( index );

        float vR = muFstHit->localPosition(0);
        float vPhi = muFstHit->localPosition(1);
        float vZ = muFstHit->localPosition(2);

        const float dz0 = fabs( vZ - mFstZFromGeom[0] );
        const float dz1 = fabs( vZ - mFstZFromGeom[1] );
        const float dz2 = fabs( vZ - mFstZFromGeom[2] );
        static const float fstThickness = 2.0; // thickness in cm between inner and outer on sigle plane

        // assign disk according to which z value the hit has, within the z-plane thickness
        int d = 0 * ( dz0 < fstThickness ) + 1 * ( dz1 < fstThickness ) + 2 * ( dz2 < fstThickness );

        float x0 = vR * cos( vPhi );
        float y0 = vR * sin( vPhi );
        hitCov3 = makeSiCovMat( TVector3( x0, y0, vZ ), mFwdConfig );

        LOG_DEBUG << "FST HIT: d = " << d << ", x=" << x0 << ", y=" << y0 << ", z=" << vZ << endm;
        mFstHits.push_back( TVector3( x0, y0, vZ)  );

        // we use d+4 so that both FTT and FST start at 4
        mFwdHitsFst.push_back(
            FwdHit(
                count++, // id
                x0, y0, vZ, // position
                d+4, // volume id
                kFstId, // detid
                0, // track id
                hitCov3, // covariance matrix
                nullptr // mcTrack
            )
        );
    } // index

    // this has to be done AFTER because the vector reallocates mem when expanding, changing addresses
    size_t numFwdHitsPost = mFwdHitsFst.size();
    for ( size_t iFwdHit = numFwdHitsPrior; iFwdHit < numFwdHitsPost; iFwdHit++){
        FwdHit *hit = &(mFwdHitsFst[ iFwdHit ]);
        // Add the hit to the hit map
        if ( hit->getLayer() >= 0 )
            hitMap[hit->getSector()].push_back(hit);
    }

    if ( numFwdHitsPost != numFwdHitsPrior ){
        LOG_INFO << "Loaded " << numFwdHitsPost - numFwdHitsPrior << " FST hits from MuDst" << endm;
    }

    // TODO add to hitmap
    return count;
} // loadFstHitsFromMuDst

int StFwdTrackMaker::loadFstHitsFromStEvent( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap){
    int count = 0;
    StEvent *event = (StEvent *)GetDataSet("StEvent");
    if (!event) {
        LOG_WARN << "No StEvent, cannot load FST hits from StEvent StFstHitCollection" << endm;
        return 0;
    }
    LOG_DEBUG << "Got StEvent, loading Fst Hits" << endm;
    StFstHitCollection *fstHitCollection = event->fstHitCollection();
    size_t numFwdHitsPrior = mFwdHitsFst.size();

    if ( fstHitCollection && fstHitCollection->numberOfHits() > 0){
        // reuse this to store cov mat
        TMatrixDSym hitCov3(3);
        LOG_DEBUG << "StFstHitCollection is NOT NULL, loading hits" << endm;
        for ( unsigned int iw = 0; iw < kFstNumWedges; iw++ ){
            StFstWedgeHitCollection * wc = fstHitCollection->wedge( iw );
            if ( !wc ) continue;
            for ( unsigned int is = 0; is < kFstNumSensorsPerWedge; is++ ){
                StFstSensorHitCollection * sc = wc->sensor( is );
                if ( !sc ) continue;
                StSPtrVecFstHit fsthits = sc->hits();
                for ( unsigned int ih = 0; ih < fsthits.size(); ih++ ){
                    float vR = fsthits[ih]->localPosition(0);
                    float vPhi = fsthits[ih]->localPosition(1);
                    float vZ = fsthits[ih]->localPosition(2);

                    const float dz0 = fabs( vZ - mFstZFromGeom[0] );
                    const float dz1 = fabs( vZ - mFstZFromGeom[1] );
                    const float dz2 = fabs( vZ - mFstZFromGeom[2] );
                    static const float fstThickness = 3.0; // thickness in cm between inner and outer on sigle plane

                    // assign disk according to which z value the hit has, within the z-plane thickness
                    int d = 0 * ( dz0 < fstThickness ) + 1 * ( dz1 < fstThickness ) + 2 * ( dz2 < fstThickness );

                    float x0 = vR * cos( vPhi );
                    float y0 = vR * sin( vPhi );
                    hitCov3 = makeSiCovMat( TVector3( x0, y0, vZ ), mFwdConfig );

                    LOG_DEBUG << "FST HIT: d = " << d << ", x=" << x0 << ", y=" << y0 << ", z=" << vZ << endm;
                    mFstHits.push_back( TVector3( x0, y0, vZ)  );
                    int track_id = fsthits[ih]->idTruth();
                    LOG_DEBUG << "FST Hit: idTruth = " << track_id << endm;
                    shared_ptr<McTrack> mcTrack = nullptr;
                    if ( mcTrackMap.count(track_id) ) {
                        mcTrack = mcTrackMap[track_id];
                        LOG_DEBUG << "Adding McTrack to FST hit" << endm;
                    }

                    // we use d+4 so that both FTT and FST start at 4
                    mFwdHitsFst.push_back(
                        FwdHit(
                            count++, // id
                            x0, y0, vZ, // position
                            d+4, // volume id
                            kFstId, // detid
                            track_id, // mc track id
                            hitCov3, // covariance matrix
                            mcTrack // mcTrack
                        )
                    );
                    // store a pointer to the original StFstHit
                    mFwdHitsFst.back()._hit = fsthits[ih];
                }
            } // loop is
        } // loop iw
        LOG_DEBUG << " FOUND " << mFstHits.size() << " FST HITS in StFstHitCollection" << endm;
    } // fstHitCollection
    // this has to be done AFTER because the vector reallocates mem when expanding, changing addresses
    size_t numFwdHitsPost = mFwdHitsFst.size();
    for ( size_t iFwdHit = numFwdHitsPrior; iFwdHit < numFwdHitsPost; iFwdHit++){
        FwdHit *hit = &(mFwdHitsFst[ iFwdHit ]);
        // Add the hit to the hit map
        if ( hit->getLayer() >= 0 )
            hitMap[hit->getSector()].push_back(hit);
        // add to MC track map
        if ( hit->getMcTrack() ){
            hit->getMcTrack()->addFstHit(hit);
        }
    }
    if ( numFwdHitsPost != numFwdHitsPrior ){
        LOG_INFO << "Loaded " << numFwdHitsPost - numFwdHitsPrior << " FST hits from StEvent" << endm;
    }
    return count;
} //loadFstHitsFromStEvent

int StFwdTrackMaker::loadFstHitsFromStRnDHits( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap){
    LOG_DEBUG << "Looking for FST hits in StEvent StRnDHit Collection" << endm;
    int count = 0;
    // Get the StEvent handle
    StEvent *event = (StEvent *)GetDataSet("StEvent");
    if (!event) {
        LOG_DEBUG << "No StEvent, cannot load FST FastSim hits from StEvent StRndHitCollection" << endm;
        return 0;
    }

    size_t numFwdHitsPrior = mFwdHitsFst.size();
    StRnDHitCollection *rndCollection = event->rndHitCollection();
    if (!rndCollection) return 0;

    const StSPtrVecRnDHit &hits = rndCollection->hits();

    // we will reuse this to hold the cov mat
    TMatrixDSym hitCov3(3);

    for (unsigned int fsthit_index = 0; fsthit_index < hits.size(); fsthit_index++) {
        StRnDHit *hit = hits[fsthit_index];

        if ( hit->layer() > 6 ){
            // skip sTGC hits here
            continue;
        }

        const StThreeVectorF pos = hit->position();

        StMatrixF covmat = hit->covariantMatrix();

        // copy covariance matrix element by element from StMatrixF
        hitCov3(0,0) = covmat[0][0]; hitCov3(0,1) = covmat[0][1]; hitCov3(0,2) = covmat[0][2];
        hitCov3(1,0) = covmat[1][0]; hitCov3(1,1) = covmat[1][1]; hitCov3(1,2) = covmat[1][2];
        hitCov3(2,0) = covmat[2][0]; hitCov3(2,1) = covmat[2][1]; hitCov3(2,2) = covmat[2][2];

        mFwdHitsFst.push_back(
            FwdHit(
                count++, // id
                hit->position().x(), hit->position().y(), hit->position().z(), // position
                hit->layer(), // volume id
                kFstId, // detid
                hit->idTruth(), // mc track id
                hitCov3, // covariance matrix
                mcTrackMap[hit->idTruth()] // mcTrack
            )
        );
        mFstHits.push_back( TVector3( hit->position().x(), hit->position().y(), hit->position().z())  );
    }

    // this has to be done AFTER because the vector reallocates mem when expanding, changing addresses
    size_t numFwdHitsPost = mFwdHitsFst.size();
    for ( size_t iFwdHit = numFwdHitsPrior; iFwdHit < numFwdHitsPost; iFwdHit++){
        FwdHit *hit = &(mFwdHitsFst[ iFwdHit ]);
        // Add the hit to the hit map
        if ( hit->getLayer() >= 0 )
            hitMap[hit->getSector()].push_back(hit);
    }
    if ( numFwdHitsPost != numFwdHitsPrior ){
        LOG_INFO << "Loaded " << numFwdHitsPost - numFwdHitsPrior << " FST hits from StEvent FastSim" << endm;
    }

    return count;
} //loadFstHitsFromStEvent

int StFwdTrackMaker::loadFstHitsFromGEANT( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap ){
    int count = 0;
    LOG_DEBUG << "Looking for FST hits in geant struct" << endm;
    /************************************************************/
    // Load FSI Hits from GEANT
    St_g2t_fts_hit *g2t_fsi_hits = (St_g2t_fts_hit *)GetDataSet("geant/g2t_fsi_hit");

    if ( !g2t_fsi_hits ){
        LOG_DEBUG << "No g2t_fts_hits, cannot load FST hits from GEANT" << endm;
        return 0;
    }

    int nfsi = g2t_fsi_hits->GetNRows();
    size_t numFwdHitsPrior = mFwdHitsFst.size();

    // reuse this to store cov mat
    TMatrixDSym hitCov3(3);

    for (int i = 0; i < nfsi; i++) {

        g2t_fts_hit_st *git = (g2t_fts_hit_st *)g2t_fsi_hits->At(i);

        if (0 == git)
            continue; // geant hit

        int track_id = git->track_p;
        int volume_id = git->volume_id;  // 4, 5, 6
        int d = volume_id / 1000;        // disk id

        int plane_id = d - 4;
        float x = git->x[0];
        float y = git->x[1];
        float z = git->x[2];

        if (mSiRasterizer->active()) {
            TVector3 rastered = mSiRasterizer->raster(TVector3(git->x[0], git->x[1], git->x[2]));
            LOG_INFO << TString::Format("Rastered: %f %f %f -> %f %f %f", git->x[0], git->x[1], git->x[2], rastered.X(), rastered.Y(), rastered.Z()) << endm;
            x = rastered.X();
            y = rastered.Y();
        } else {
            LOG_INFO << "Using GEANT FST hit positions without rasterization" << endm;
        }

        if (plane_id > 3 || plane_id < 0) {
            continue;
        }

        hitCov3 = makeSiCovMat( TVector3( x, y, z ), mFwdConfig );
        mFwdHitsFst.push_back(
            FwdHit(
                count++, // id
                x, y, z, // position
                d, // volume id
                kFstId, // detid
                track_id, // mc track id
                hitCov3, // covariance matrix
                mcTrackMap[track_id] // mcTrack
            )
        );
        mFstHits.push_back( TVector3( x, y, z )  );
    }

    // this has to be done AFTER because the vector reallocates mem when expanding, changing addresses
    size_t numFwdHitsPost = mFwdHitsFst.size();
    for ( size_t iFwdHit = numFwdHitsPrior; iFwdHit < numFwdHitsPost; iFwdHit++){
        FwdHit *hit = &(mFwdHitsFst[ iFwdHit ]);
        // Add the hit to the hit map
        if ( hit->getLayer() >= 0 )
            hitMap[hit->getSector()].push_back(hit);

        // add to MC track map
        if ( hit->getMcTrack() )
            hit->getMcTrack()->addFstHit(hit);
    }
    if ( numFwdHitsPost != numFwdHitsPrior ){
        LOG_INFO << "Loaded " << numFwdHitsPost - numFwdHitsPrior << " FST hits from GEANT" << endm;
    }

    return count;
} // loadFstHitsFromGEANT

/**
 * Loads the Monte Carlo (MC) tracks from the GEANT simulation data.
 *
 * @param mcTrackMap A reference to the MC track map.
 *
 * @return The number of forward tracks.
 *
 * @throws None.
 */
size_t StFwdTrackMaker::loadMcTracks( FwdDataSource::McTrackMap_t &mcTrackMap ){

    LOG_DEBUG << "Looking for GEANT sim vertex info" << endm;
    St_g2t_vertex *g2t_vertex = (St_g2t_vertex *)GetDataSet("geant/g2t_vertex");

    if ( g2t_vertex != nullptr ) {
        // Set the MC Vertex for track fitting
        g2t_vertex_st *vert = (g2t_vertex_st*)g2t_vertex->At(0);
        TMatrixDSym cov;
        cov.ResizeTo(3, 3);
        cov(0, 0) = 0.001;
        cov(1, 1) = 0.001;
        cov(2, 2) = 0.001;
        mForwardTracker->setEventVertex( TVector3( vert->ge_x[0], vert->ge_x[1], vert->ge_x[2] ), cov );
    }


    // Get geant tracks
    St_g2t_track *g2t_track = (St_g2t_track *)GetDataSet("geant/g2t_track");

    if (!g2t_track)
        return 0;

    size_t nShowers = 0;
    LOG_DEBUG << g2t_track->GetNRows() << " mc tracks in geant/g2t_track " << endm;

    for (int irow = 0; irow < g2t_track->GetNRows(); irow++) {
        g2t_track_st *track = (g2t_track_st *)g2t_track->At(irow);

        if (0 == track)
            continue;

        int track_id = track->id;
        float pt2 = track->p[0] * track->p[0] + track->p[1] * track->p[1];
        float pt = std::sqrt(pt2);
        float eta = track->eta;
        TVector3 pp( track->p[0], track->p[1], track->p[2] );
        float phi = std::atan2(track->p[1], track->p[0]); //track->phi;
        int q = track->charge;
        // sometimes the track->eta is wrong, pt, phi
        if (!mcTrackMap[track_id] )
            mcTrackMap[track_id] = shared_ptr<McTrack>(new McTrack(pp.Pt(), pp.Eta(), pp.Phi(), q, track->start_vertex_p));

    } // loop on track (irow)

    // now check the Mc tracks against the McEvent filter
    size_t nForwardTracks = 0;
    size_t nForwardTracksNoThreshold = 0;
    for (auto mctm : mcTrackMap ){
        if ( mctm.second == nullptr ) continue;
        if ( mctm.second->mEta > 2.5 && mctm.second->mEta < 4.0 ){
            nForwardTracksNoThreshold++;
            if ( mctm.second->mPt > 0.05  )
                nForwardTracks++;
        }
    } // loop on mcTrackMap
    return nForwardTracks;
} // loadMcTracks

/**
 * Load FCS data from StEvent for ECAL/HCAL clusters and Preshower hits (EPD).
 *
 * @param None
 *
 * @return None
 *
 * @throws None
 */
void StFwdTrackMaker::loadFcs( ) {
    StEvent *stEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
    if ( !stEvent || !mFcsDb ){
        return;
    }
    StFcsCollection* fcsCol = stEvent->fcsCollection();
    if ( !fcsCol ){
        return;
    }

    StEpdGeom epdgeo;

    // LOAD ECAL / HCAL CLUSTERS
    for ( int idet = 0; idet  < 4; idet++ ){
        StSPtrVecFcsCluster& clusters = fcsCol->clusters(idet);
        int nc=fcsCol->numberOfClusters(idet);
        for ( int i = 0; i < nc; i++ ){
            StFcsCluster* clu = clusters[i];
            StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(clu->detectorId(),clu->x(),clu->y());
            mFcsClusters.push_back( TVector3( xyz.x(), xyz.y(), xyz.z() - 2 ) );
            mFcsClusterEnergy.push_back( clu->energy() );
        } // i
    } // idet

    // LOAD PRESHOWER HITS (EPD)
    for ( int det = 4; det < 6; det ++ ) {

        StSPtrVecFcsHit& hits = stEvent->fcsCollection()->hits(det);
        int nh=fcsCol->numberOfHits(det);
        for ( int i = 0; i < nh; i++ ){
            StFcsHit* hit=hits[i];

            if(det==kFcsPresNorthDetId || det==kFcsPresSouthDetId){ //EPD
                double zepd=375.0;
                int pp,tt,n;
                double x[5],y[5];

                if ( hit->energy() < 0.2 ) continue;
                mFcsDb->getEPDfromId(det,hit->id(),pp,tt);
                epdgeo.GetCorners(100*pp+tt,&n,x,y);
                double x0 = (x[0] + x[1] + x[2] + x[3]) / 4.0;
                double y0 = (y[0] + y[1] + y[2] + y[3]) / 4.0;
                mFcsPreHits.push_back( TVector3( x0, y0, zepd ) );
            } // if det
        } // for i
    } // for det
} // loadFcs

TVector3 StFwdTrackMaker::GetEventPrimaryVertex(){
    if ( mFwdVertexSource != kFwdVertexSourceUnknown ){
        // This includes the case where we have already searched and found nothing
        return mEventVertex;
    }

    mEventVertexCov.ResizeTo(3, 3);
    mEventVertexCov.Zero();
    double sig2 = 1;// default resolution, overwritten if valid vtx found
    mEventVertexCov(0, 0) = sig2; 
    mEventVertexCov(1, 1) = sig2;
    mEventVertexCov(2, 2) = sig2;
    // if something is found it will overwrite this, if not
    // it will indicate that we have searched and found nothing
    mFwdVertexSource = kFwdVertexSourceNone;

    /*****************************************************
     * Add Primary Vertex to the track
     */
    St_g2t_vertex *g2t_vertex = (St_g2t_vertex *)GetDataSet("geant/g2t_vertex");
    LOG_DEBUG << "Searching for Event Vertex from geant/g2t_vertex: " << g2t_vertex << endm;
    if ( g2t_vertex != nullptr ) {
        // Set the MC Vertex for track fitting
        g2t_vertex_st *vert = (g2t_vertex_st*)g2t_vertex->At(0);
        LOG_INFO << "Setting Event Vertex from geant/g2t_vertex[0]: " << vert << endm;
        if ( vert ){
            mEventVertexCov.ResizeTo(3, 3);
            const double sigXY = 0.1; // TODO: read from MC vertex info?
            const double sigZ = 0.1;
            mEventVertexCov(0, 0) = pow(sigXY,2);
            mEventVertexCov(1, 1) = pow(sigXY,2);
            mEventVertexCov(2, 2) = pow(sigZ, 2);
            auto rhc = TVectorD( 3 );
            rhc[0] = vert->ge_x[0];
            rhc[1] = vert->ge_x[1];
            rhc[2] = vert->ge_x[2];
            mEventVertex.SetXYZ( vert->ge_x[0], vert->ge_x[1], vert->ge_x[2] );
            mFwdVertexSource = kFwdVertexSourceMc;
            return mEventVertex;
        }
    }

    // or try the McEvent
    StMcEvent *stMcEvent = static_cast<StMcEvent *>(GetInputDS("StMcEvent"));
    LOG_DEBUG << "Searching for Event Vertex from StMcEvent: " << stMcEvent << endm;
    if (stMcEvent && stMcEvent->primaryVertex() ) {
        StThreeVectorF vertex = stMcEvent->primaryVertex()->position();
        mEventVertex.SetXYZ( vertex.x(), vertex.y(), vertex.z() );
        mFwdVertexSource = kFwdVertexSourceMc;
        LOG_INFO << "FWD Tracking on event with MC Primary Vertex: " << mEventVertex.X() << ", " << mEventVertex.Y() << ", " << mEventVertex.Z() << endm;

        const double sigXY = 0.1; // TODO: read from MC vertex info?
        const double sigZ = 0.1;
        mEventVertexCov(0, 0) = pow(sigXY,2);
        mEventVertexCov(1, 1) = pow(sigXY,2);
        mEventVertexCov(2, 2) = pow(sigZ, 2);
        return mEventVertex;
    }

    StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
    if(mMuDstMaker && mMuDstMaker->muDst() && mMuDstMaker->muDst()->numberOfPrimaryVertices() > 0 && mMuDstMaker->muDst()->primaryVertex() ) {
        mEventVertex.SetX(mMuDstMaker->muDst()->primaryVertex()->position().x());
        mEventVertex.SetY(mMuDstMaker->muDst()->primaryVertex()->position().y());
        mEventVertex.SetZ(mMuDstMaker->muDst()->primaryVertex()->position().z());
        mFwdVertexSource = kFwdVertexSourceTpc;
        return mEventVertex;
    } 
    
    
    LOG_DEBUG << "FWD Tracking on event without available Mu Primary Vertex" << endm;
    StEvent *stEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    if (!stEvent) return mEventVertex; // if we get here and there is no StEvent, we are done
    
    StBTofCollection *btofC = stEvent->btofCollection();
    if (!btofC) {
        LOG_WARN << "Cannot get BTOF collections, Cannot use VPD vertex" << endm;
        return mEventVertex;
    }

    StBTofHeader * btofHeader = btofC->tofHeader();
    if (!btofHeader){
        LOG_WARN << "Cannot get BTOF Header, Cannot use VPD vertex" << endm;
        return mEventVertex;
    }

    int nEast = btofHeader->numberOfVpdHits( east );
    int nWest = btofHeader->numberOfVpdHits( west );
    int nTof = btofC->tofHits().size();

    if ( btofHeader->vpdVz() && fabs(btofHeader->vpdVz()) < 100 ){
        // default event vertex
        LOG_DEBUG << "FWD Tracking on event using VPD z vertex: (, 0, 0, " << btofHeader->vpdVz() << " )" << endm;
        mFwdVertexSource = kFwdVertexSourceVpd;
        const double sigXY = 1;
        const double sigZ = 6; // approximate resolution of VPD in p+p collisions
        mEventVertexCov(0, 0) = pow(sigXY,2);
        mEventVertexCov(1, 1) = pow(sigXY,2);
        mEventVertexCov(2, 2) = pow(sigZ, 2);
        mEventVertex.SetXYZ( 0, 0, btofHeader->vpdVz() );
        return mEventVertex;
    }
    
    // if we get here we failed to find a valid vtx
    return mEventVertex;
}

//________________________________________________________________________
int StFwdTrackMaker::Make() {
    // START time for measuring tracking
    long long itStart = FwdTrackerUtils::nowNanoSecond();

    StEvent *stEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    if (!stEvent) return kStOk;

    /**********************************************************************/
    // Access forward track and hit maps
    FwdDataSource::McTrackMap_t &mcTrackMap = mForwardData->getMcTracks();
    FwdDataSource::HitMap_t &hitMap = mForwardData->getFttHits();
    FwdDataSource::HitMap_t &fsiHitMap = mForwardData->getFstHits();

    /**********************************************************************/
    // get the primary vertex for use with FWD tracking
    mFwdVertexSource = StFwdTrackMaker::kFwdVertexSourceUnknown;
    GetEventPrimaryVertex();
    LOG_DEBUG << "FWD Vertex Source: " << mFwdVertexSource << endm;
    LOG_DEBUG << "Setting FWD event vertex to: " << TString::Format("mEventVertex=(%0.3f+/-%0.3f, %0.3f+/-%0.3f, %0.3f+/-%0.3f)", mEventVertex.X(), sqrt(mEventVertexCov(0, 0)), mEventVertex.Y(), sqrt(mEventVertexCov(1, 1)), mEventVertex.Z(), sqrt(mEventVertexCov(2, 2)) ) << endm;
    mForwardTracker->setEventVertex( mEventVertex, mEventVertexCov );
    

    /**********************************************************************/
    // Load MC tracks
    size_t nForwardTracks = loadMcTracks( mcTrackMap );
    size_t maxForwardTracks = mFwdConfig.get<size_t>( "McEvent.Mult:max", 10000 );
    if ( nForwardTracks > maxForwardTracks ){
        LOG_WARN << "Skipping event with more than " << maxForwardTracks << " forward tracks" << endm;
        return kStOk;
    }
    LOG_DEBUG << "We have " << nForwardTracks << " forward MC tracks" << endm;

    /**********************************************************************/
    // Load sTGC
    LOG_DEBUG << ">>StFwdTrackMaker::loadFttHits" << endm;
    if ( IAttr("useFtt") ) {
        loadFttHits( mcTrackMap, hitMap );
    }

    /**********************************************************************/
    // Load FST
    if ( IAttr("useFst") ) {
        LOG_DEBUG << ">>StFwdTrackMaker::loadFstHits" << endm;
        int fstCount = loadFstHits( mcTrackMap, fsiHitMap );
        LOG_DEBUG << "Loaded " << fstCount << " FST hits" << endm;
    }

    /**********************************************************************/
    // Load FCS
    LOG_DEBUG << ">>StFwdTrackMaker::loadFcsHits" << endm;
    if ( IAttr("useFcs") ) {
        loadFcs();
    }

    /**********************************************************************/
    // Print out the MC tracks and their hit counts
    for ( auto kv : mcTrackMap ){
        if ( kv.second == nullptr ) continue;
        LOG_DEBUG << "MC Track: id=" << kv.first << ", nFTT=" << kv.second->mFttHits.size() << ", nFST=" << kv.second->mFstHits.size() << endm;
    }

    /**********************************************************************/
    // Run Track finding + fitting
    LOG_DEBUG << ">>START Event Forward Tracking" << endm;
    LOG_INFO << "\tFinding FWD Track Seeds" << endm;
    mForwardTracker->findTrackSeeds();
    LOG_INFO << "\tFitting FWD Track Seeds" << endm;
    // in principle we could filter the track seeds further if we wanted
    mForwardTracker->doTrackFitting( mForwardTracker->getTrackSeeds() );
    LOG_DEBUG << "<<FINISH Event Forward Tracking" << endm;
    LOG_INFO << "<<Fwd Tracking Found : " << mForwardTracker -> getTrackSeeds().size() << " Track Seeds" << endm;
    LOG_INFO << "<<Fwd Tracking Fit :" << mForwardTracker -> getTrackResults().size() << " GenFit Tracks" << endm;
    /**********************************************************************/


    /**********************************************************************/
    // Output track visualization if configured to do so
    if ( mVisualize ){
        std::vector<genfit::Track *> genfitTracks;
        for ( auto gtr : mForwardTracker->getTrackResults() ) {
            if ( gtr.mIsFitConvergedFully == false ) continue;
            genfitTracks.push_back( gtr.mTrack.get() );
        }

        if ( mVisualize && genfitTracks.size() > 0 && genfitTracks.size() < 400 && eventIndex < 50 ) {
            const auto &seed_tracks = mForwardTracker -> getTrackSeeds();

            ObjExporter woe;
            woe.output(
                TString::Format( "ev%lu", eventIndex ).Data(),
                stEvent,
                seed_tracks, genfitTracks, mRaveVertices,
                mFttHits, mFstHits, mFcsPreHits, mFcsClusters, mFcsClusterEnergy );
            eventIndex++;
            LOG_DEBUG << "Done Writing OBJ " << endm;
        } else if (mVisualize && genfitTracks.size() == 0) {
            LOG_DEBUG << "Skipping visualization, no FWD tracks" << endm;
        } else if (mVisualize && genfitTracks.size() >= 400) {
            LOG_DEBUG << "Skipping visualization, too many FWD tracks" << endm;
        }
    }

    LOG_DEBUG << "Forward tracking on this event took " << (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6 << " ms" << endm;
    if ( IAttr("fillEvent") ) {
        if (!stEvent) {
            LOG_WARN << "No StEvent found. Forward tracks will not be saved" << endm;
            return kStWarn;
        }
        FillEvent();
    } // IAttr FillEvent

    return kStOK;
} // Make

/**
 * Creates a StFwdTrack object from a GenfitTrackResult.
 *
 * @param gtr The GenfitTrackResult object containing the track information.
 * @param indexTrack The index of the track.
 *
 * @return A pointer to the created StFwdTrack object, or nullptr if the GenfitTrackResult is nullptr.
 *
 * @throws None.
 */
StFwdTrack * StFwdTrackMaker::makeStFwdTrack( GenfitTrackResult &gtr, size_t indexTrack ){
    LOG_DEBUG << "StFwdTrackMaker::makeStFwdTrack()" << endm;
    StFwdTrack *fwdTrack = new StFwdTrack( );

    /*******************************************************************************/
    // store the seed points for the track
    int nSeedPoints = 0;
    for ( auto s : gtr.mSeed ){
        FwdHit * fh = static_cast<FwdHit*>( s );
        if (!fh) continue;
        float cov[9];
        cov[0] = fh->_covmat(0,0); cov[3] = fh->_covmat(1,0); cov[6] = fh->_covmat(2,0);
        cov[1] = fh->_covmat(0,1); cov[4] = fh->_covmat(1,1); cov[7] = fh->_covmat(2,1);
        cov[2] = fh->_covmat(0,2); cov[5] = fh->_covmat(1,2); cov[8] = fh->_covmat(2,2);

        StFwdTrackSeedPoint p(
            StThreeVectorD( fh->getX(), fh->getY(), fh->getZ() ),
            fh->_detid * 10 + fh->getSector(), // 10 * detid + sector
            fh->getTrackId(),
            cov
        );
        if ( fh->isFst() )
            fwdTrack->mFSTPoints.push_back( p );
        else if ( fh->isFtt() )
            fwdTrack->mFTTPoints.push_back( p );

        nSeedPoints++;
    }

    // set total number of seed points
    fwdTrack->setNumberOfSeedPoints( nSeedPoints );
    int idt = 0;
    double qual = 0;
    idt = MCTruthUtils::dominantContribution(gtr.mSeed, qual);
    fwdTrack->setMc( idt, qual*100 ); // QAtruth stored as UChar_t
    LOG_DEBUG << "Dominant contribution: " << idt << " with quality " << qual << endm;

    // Fit failed beyond use
    if ( gtr.mTrack == nullptr ){
        gtr.Clear();
        LOG_DEBUG << "GenfitTrack is nullptr, making StFwdTrack with seed info only" << endm;
        return fwdTrack;
    }
    // Fill charge and quality info
    fwdTrack->setDidFitConverge( gtr.mStatus->isFitConverged() );
    fwdTrack->setDidFitConvergeFully( gtr.mStatus->isFitConvergedFully() );
    fwdTrack->setNumberOfFailedPoints( gtr.mStatus->getNFailedPoints() );

    fwdTrack->setNumberOfFitPoints( gtr.mTrack->getNumPoints() );
    fwdTrack->setChi2( gtr.mStatus->getChi2() );
    fwdTrack->setNDF( gtr.mStatus->getNdf() );
    fwdTrack->setPval( gtr.mStatus->getPVal() );

    // charge at first point
    fwdTrack->setCharge( gtr.mCharge );
    fwdTrack->setDCA( gtr.mDCA.X(), gtr.mDCA.Y(), gtr.mDCA.Z() );

    TVector3 p = gtr.mMomentum;//cr->getMom( gtr.mTrack->getFittedState( 0, cr ));
    fwdTrack->setPrimaryMomentum( StThreeVectorD( gtr.mMomentum.X(), gtr.mMomentum.Y(), gtr.mMomentum.Z() ) );

    if ( gtr.isPrimary ){
        fwdTrack->setVtxIndex( 0 );
    } else {
        fwdTrack->setVtxIndex( UCHAR_MAX );
    }

    /*******************************************************************************/
    // if the track is not (at least partially) converged, do not try to project it
    if ( !gtr.mStatus->isFitConvergedPartially() ){
        gtr.Clear();
        return fwdTrack;
    }

    /*******************************************************************************/
    // compute projections to z-planes of various detectors
    // TODO: update FCS to use correct z + angle
    map<int, float> mapDetectorToZPlane = {
        { kTpcId, 0.0 },
        { kFstId, mFstZFromGeom[0] },
        { kFstId, mFstZFromGeom[1] },
        { kFstId, mFstZFromGeom[2] },
        { kFttId, mFttZFromGeom[0] },
        { kFttId, mFttZFromGeom[1] },
        { kFttId, mFttZFromGeom[2] },
        { kFttId, mFttZFromGeom[3] },
        { kFcsPresId, 375.0 },
        { kFcsWcalId, 715.0 },
        { kFcsHcalId, 807.0 }
    };

    if ( gtr.mStatus->isFitConverged() ){ // dont project if the fit did not converge
        size_t zIndex = 0;
        TVector3 mom(0, 0, 0);
        float cov[9];
        TVector3 tv3(0, 0, 0);
        for ( auto zp : mapDetectorToZPlane ){
            int detIndex = zp.first;
            float z = zp.second;
            tv3.SetXYZ(0, 0, 0);
            if ( detIndex != kFcsHcalId && detIndex != kFcsWcalId ){
                float detpos[3] = {0,0,z};
                float detnorm[3] = {0,0,1};
                if( detIndex==kFcsPresId ){
                    StThreeVectorD xyzoff = mFcsDb->getDetectorOffset(kFcsPresId);
                    detpos[0] = (float)xyzoff.x();
                    detpos[1] = (float)xyzoff.y();
                    detpos[2] = (float)xyzoff.z();
                }
                tv3 = ObjExporter::trackPosition( gtr.mTrack.get(), detpos, detnorm, cov, mom );
            } else {
                // use a straight line projection to HCAL since GenFit cannot handle long projections
                int det=0;
                if( detIndex==kFcsWcalId ){
                    det = 0;   // North side for negative px
                    // South side for positive px, since px==0 does not hit detector choose south side for that case
                    if( p[2]>=0 && p[0]>=0 ){ det=1; }
                    if( p[2]<0  && p[0]<0  ){ det=1; }
                }
                //Since detIndex cannot be both don't need "else if"
                if( detIndex==kFcsHcalId ){
                    det = 2;  // North side for negative px
                    // South side for positive px, since px==0 does not hit detector choose south side for that case
                    if( p[2]>=0 && p[0]>=0 ){ det=3; }
                    if( p[2]<0  && p[0]<0  ){ det=3; }
                }
                StThreeVectorD xyzoff = mFcsDb->getDetectorOffset(det);
                StThreeVectorD planenormal = mFcsDb->getNormal(det);
                float xyz0[3] = { 0, 0, 575.0 };
                float xyz1[3] = { 0, 0, 625.0 };
                float xyzdet[3] = { (float)xyzoff.x(), (float)xyzoff.y(), (float)xyzoff.z() };
                float detnorm[3] = { (float)planenormal.x(), (float)planenormal.y(), (float)planenormal.z() };
                LOG_DEBUG << "Projecting to: " << detIndex << endm;
                tv3 = ObjExporter::projectAsStraightLine( gtr.mTrack.get(), xyz0, xyz1, xyzdet, detnorm, cov, mom );
            }
            fwdTrack->mProjections.push_back( StFwdTrackProjection( detIndex, StThreeVectorF( tv3.X(), tv3.Y(), tv3.Z() ), StThreeVectorF( mom.X(), mom.Y(), mom.Z() ), cov) );
            zIndex++;
        }
    }
    /*******************************************************************************/

    /*******************************************************************************/
    // clear the GenfitTrackResult
    gtr.Clear();

    // return the StFwdTrack we made
    return fwdTrack;
}

void StFwdTrackMaker::FillEvent() {
    StEvent *stEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    if (!stEvent)
        return;
    StFwdTrackCollection * ftc = stEvent->fwdTrackCollection();
    if ( !ftc ){
        LOG_INFO << "Creating the StFwdTrackCollection" << endm;
        ftc = new StFwdTrackCollection();
        stEvent->setFwdTrackCollection( ftc );
    }

    size_t indexTrack = 0;
    for ( auto gtr : mForwardTracker->getTrackResults() ) {
            StFwdTrack* fwdTrack = makeStFwdTrack( gtr, indexTrack );
            indexTrack++;
            if (nullptr == fwdTrack)
                continue;
            ftc->addTrack( fwdTrack );
    }

    LOG_INFO << "StFwdTrackCollection has " << ftc->numberOfTracks() << " tracks now" << endm;

    // Pico Dst requires a primary vertex,
    // if we have a PicoDst maker in the chain, we need to add a primary vertex
    // when one does not exist to get a "FWD" picoDst
    auto mk = GetMaker("PicoDst");
    if ( mk && stEvent->numberOfPrimaryVertices() == 0 ){
        LOG_INFO << "Adding a primary vertex to StEvent since PicoDst maker was found in chain, but no vertices found" << endm;
        stEvent->addPrimaryVertex( new StPrimaryVertex() );
        LOG_INFO << "StPrimaryVertex::numberOfPrimaryVertices = " << stEvent->numberOfPrimaryVertices() << endm;
    }

    // ProcessFwdTracks();
}

void StFwdTrackMaker::FitVertex(){
    vector<genfit::Track *> genfitTracks;

    const auto &trackResults = mForwardTracker -> getTrackResults();
    if ( genfitTracks.size() >= 2 ){
        genfit::GFRaveVertexFactory gfrvf;

        TMatrixDSym bscm(3);
        const double bssXY = 2.0;
        bscm(0, 0) = bssXY*bssXY;
        bscm(1, 1) = bssXY*bssXY;
        bscm(2, 2) = 50.5 * 50.5;
        gfrvf.setBeamspot( TVector3( 0, 0, 0 ), bscm );

        mRaveVertices.clear();
        gfrvf.findVertices( &mRaveVertices, genfitTracks, false );

        LOG_DEBUG << "mRaveVertices.size() = " << mRaveVertices.size() << endm;
        for ( auto vert : mRaveVertices ){
            LOG_DEBUG << TString::Format( "RAVE vertex @(%f, %f, %f)\n\n", vert->getPos().X(), vert->getPos().Y(), vert->getPos().Z() ) << endm;
        }
    }
} // FitVertex

//________________________________________________________________________
void StFwdTrackMaker::Clear(const Option_t *opts) {
    LOG_DEBUG << "StFwdTrackMaker::CLEAR" << endm;
    mForwardData->clear();
    mForwardTracker->Clear();

    // clear fwd hits from fst and ftt
    mFwdHitsFst.clear();
    mFwdHitsFtt.clear();

    // clear vectors for visualization OBJ hits
    mFttHits.clear();
    mFstHits.clear();
    mFcsPreHits.clear();
    mFcsClusters.clear();
    mFwdTracks.clear();

}
//________________________________________________________________________
void StFwdTrackMaker::ProcessFwdTracks(  ){
    // This is an example of how to process fwd track collection
    LOG_DEBUG << "StFwdTrackMaker::ProcessFwdTracks" << endm;
    StEvent *stEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    StFwdTrackCollection * ftc = stEvent->fwdTrackCollection();
    for ( auto fwdTrack : ftc->tracks() ){
        LOG_DEBUG << TString::Format("StFwdTrack[ nProjections=%lu, nFTTSeeds=%lu, nFSTSeeds=%lu, mPt=%f ]", fwdTrack->mProjections.size(), fwdTrack->mFTTPoints.size(), fwdTrack->mFSTPoints.size(), fwdTrack->momentum().perp()) << endm;
        for ( auto proj : fwdTrack->mProjections ) {
            LOG_DEBUG << TString::Format("Proj[ %d, %f, %f, %f ]", proj.mDetId, proj.mXYZ.x(), proj.mXYZ.y(), proj.mXYZ.z() ) << endm;
        }
    }
}


std::string StFwdTrackMaker::defaultConfig = R"(
<?xml version="1.0" encoding="UTF-8"?>
<config>
    <TrackFinder nIterations="1">
        <Iteration nPhiSlices="1" > <!-- Options for first iteration -->
            <SegmentBuilder>
                <!-- <Criteria name="Crit2_RZRatio" min="0" max="1.20" /> -->
                <!-- <Criteria name="Crit2_DeltaRho" min="-50" max="50.9"/> -->
                <Criteria name="Crit2_DeltaPhi" min="0" max="10.0" />
                <!-- <Criteria name="Crit2_StraightTrackRatio" min="0.01" max="5.85"/> -->
            </SegmentBuilder>

            <ThreeHitSegments>
				<!-- <Criteria name="Crit3_3DAngle" min="0" max="60" />
                <Criteria name="Crit3_PT" min="0" max="100" />
				<Criteria name="Crit3_ChangeRZRatio" min="0.8" max="1.21" />
				<Criteria name="Crit3_2DAngle" min="0" max="30" /> -->
            </ThreeHitSegments>

        </Iteration>

        <Connector distance="2"/>

        <SubsetNN active="true" min-hits-on-track="2" >
            <!-- <InitialTemp>2.1</InitialTemp> -->
            <!-- <InfTemp>0.1</InfTemp> -->
            <Omega>0.99</Omega>
            <StableThreshold>0.001</StableThreshold>
        </SubsetNN>

        <HitRemover active="false" />
    </TrackFinder>

	<TrackFitter refit="true" zeroB="false" active="true">
        <Vertex sigmaXY="1" sigmaZ="10" includeInFit="true" smearMcVertex="false" />
    </TrackFitter>
</config>
)";


const std::vector<Seed_t> &StFwdTrackMaker::getTrackSeeds() const{
    return mForwardTracker->getTrackSeeds();
}

const std::vector<GenfitTrackResult> &StFwdTrackMaker::getFitResults()const{
    return mForwardTracker->getTrackResults();
}
