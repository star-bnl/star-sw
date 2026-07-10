
#include "KiTrack/IHit.h"
#include "GenFit/Track.h"

#include "StFwdTrackMaker.h"
#include "TMath.h"
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

#include <cstdlib>

bool StFwdTrackMaker::mDebug = false; // initialize static member variable
// Per-file gating of STAR logging macros via mDebug. Bypasses a leak in the
// log4cxx pipeline (~312 B per LOG_INFO call). When mDebug==false the entire
// LOG expression is skipped at the AST level — no allocation, no leak.
// Affects only this translation unit. The `if (!mDebug) {} else` form guards
// against dangling-else attaching to a caller's `if`.
#undef  LOG_INFO
#undef  LOG_DEBUG
#undef  LOG_WARN
#define LOG_INFO  if (!StFwdTrackMaker::mDebug) {} else LOGGERMESSAGE(Info)
#define LOG_DEBUG if (!StFwdTrackMaker::mDebug) {} else LOGGERMESSAGE(Debug)
#define LOG_WARN  if (!StFwdTrackMaker::mDebug) {} else LOGGERMESSAGE(Warning)

#include "StFwdTrackMaker/StFwdTrackMaker.h"
#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StFwdTrackMaker/include/Tracker/FwdTracker.h"
#include "StFwdTrackMaker/include/Tracker/TrackFitter.h"
#include "StFwdTrackMaker/include/Tracker/FwdGeomUtils.h"
#include "StFwdTrackMaker/include/Tracker/ObjExporter.h"

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
    // const bool mDebug = false; // set to true to enable debug logging in this function
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

//  Wrapper class around the forward tracker
class ForwardTracker : public ForwardTrackMaker {
  public:
    // Replaces original initialization.  Config file and hitloader
    // will be provided by the maker.
    void initialize( TString geoCache, bool genHistograms ) {
        nEvents = 1; // only process single event

        // Create the forward system, cleaning up any previous instance
        if (FwdSystem::sInstance) {
            delete FwdSystem::sInstance;
        }
        FwdSystem::sInstance = new FwdSystem();

        // initialize the track fitter, cleaning up any previous instance
        if (mTrackFitter) {
            delete mTrackFitter;
        }
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
    LOG_DEBUG << "StFwdTrackMaker::StFwdTrackMaker()" << endm;
    mEventVertexCov.ResizeTo(3, 3);
    mEventVertexCov.Zero();
    
    SetAttr("useFtt",1);                 // Default Ftt on
    SetAttr("useFst",1);                 // Default Fst on
    SetAttr("useFcs",1);                 // Default Fcs on
    SetAttr("useEpd",1);                 // Default Epd on
    SetAttr("config", "config.xml");     // Default configuration file (user may override before Init())
    SetAttr("fillEvent",1); // fill StEvent

    // Load the default configuration
    configLoaded = false;
    LoadConfiguration();

    // set additional default configuration values
    setOutputFilename( "stfwdtrackmaker_data.root" );
    LOG_DEBUG << "Done with StFwdTrackMaker::StFwdTrackMaker()" << endm;  
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
        LOG_DEBUG << "Forward Tracker is using the default config" <<  mConfigFile << endm;
    } else {
        LOG_DEBUG << "Forward Tracker is using config from file : " <<  mConfigFile << endm;
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
    } else {
        LOG_INFO << "Using cached geometry file: " << mGeoCache << endm;
    }
    
    mForwardTracker = std::shared_ptr<ForwardTracker>(new ForwardTracker( ));
    mForwardTracker->setConfig(mFwdConfig);

    // in production we disable crit saving.
    mForwardTracker->setSaveCriteriaValues(false);

    mForwardData = std::shared_ptr<FwdDataSource>(new FwdDataSource());
    mForwardTracker->setData(mForwardData);
    mForwardTracker->initialize( mGeoCache, false );
    // Setup the mFwdHitLoader


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

EventStats StFwdTrackMaker::GetEventStats() { 
    return mForwardTracker->getEventStats(); 
}

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

    // PV is established once per event by GetEventPrimaryVertex() in Make();
    // do not overwrite it here.

    // Get geant tracks
    St_g2t_track *g2t_track = (St_g2t_track *)GetDataSet("geant/g2t_track");

    if (!g2t_track)
        return 0;

    LOG_DEBUG << g2t_track->GetNRows() << " mc tracks in geant/g2t_track " << endm;

    for (int irow = 0; irow < g2t_track->GetNRows(); irow++) {
        g2t_track_st *track = (g2t_track_st *)g2t_track->At(irow);

        if (0 == track)
            continue;

        int track_id = track->id;
        TVector3 pp( track->p[0], track->p[1], track->p[2] );
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
        StMuPrimaryVertex *muPv = mMuDstMaker->muDst()->primaryVertex();
        mEventVertex.SetX( muPv->position().x() );
        mEventVertex.SetY( muPv->position().y() );
        mEventVertex.SetZ( muPv->position().z() );
        // Use the TPC PV's own resolution rather than the 1 cm² default
        const StThreeVectorF posErr = muPv->posError();
        if ( posErr.x() > 0 && posErr.y() > 0 && posErr.z() > 0 ) {
            mEventVertexCov(0, 0) = posErr.x() * posErr.x();
            mEventVertexCov(1, 1) = posErr.y() * posErr.y();
            mEventVertexCov(2, 2) = posErr.z() * posErr.z();
        } else {
            LOG_WARN << "TPC PV posError is not positive (" << posErr.x() << ", " << posErr.y() << ", " << posErr.z() << "), keeping default cov" << endm;
        }
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
    // return kStOk;
    // START time for measuring tracking
    long long itStart = FwdTrackerUtils::nowNanoSecond();

    StEvent *stEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    if (!stEvent) return kStOk;

    

    /**********************************************************************/
    // Access forward track and hit maps
    FwdDataSource::McTrackMap_t &mcTrackMap = mForwardData->getMcTracks();
    FwdDataSource::HitMap_t &hitMap         = mForwardData->getFttHits();
    FwdDataSource::HitMap_t &fsiHitMap      = mForwardData->getFstHits();
    FwdDataSource::HitMap_t &epdHitMap      = mForwardData->getEpdHits();

    mFwdHitLoader.setStEvent( stEvent );
    mFwdHitLoader.setMuDstMaker( (StMuDstMaker *)GetMaker("MuDst") );
    mFwdHitLoader.setTables(
        (St_g2t_fts_hit *)GetDataSet("geant/g2t_stg_hit"),
        (St_g2t_fts_hit *)GetDataSet("geant/g2t_fsi_hit"),
        nullptr
    );
    mFcsDb = dynamic_cast<StFcsDb*>(GetDataSet("fcsDb"));
    if ( !mFcsDb ) {
        LOG_WARN << "No FCS DB found, cannot load FCS hits" << endm;
    }

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
        mFwdHitLoader.loadFttHits( mcTrackMap, hitMap );
    }

    /**********************************************************************/
    // Load FST
    if ( IAttr("useFst") ) {
        LOG_DEBUG << ">>StFwdTrackMaker::loadFstHits" << endm;
        int fstCount = mFwdHitLoader.loadFstHits( mcTrackMap, fsiHitMap );
        LOG_DEBUG << "Loaded " << fstCount << " FST hits" << endm;
    }

    /**********************************************************************/
    // Load FCS
    LOG_DEBUG << ">>StFwdTrackMaker::loadFcsHits" << endm;
    if ( IAttr("useEpd") ) {
        LOG_DEBUG << ">>StFwdTrackMaker::loadEpdHits" << endm;
        int epdCount = mFwdHitLoader.loadEpdHits( mcTrackMap, epdHitMap, mFcsDb );
        LOG_DEBUG << "Loaded " << epdCount << " Epd hits" << endm;
    }

    /**********************************************************************/
    // Print out the MC tracks and their hit counts
    map<int, int> nFstMcTracks;
    map<int, int> nFttMcTracks;
    for ( auto kv : mcTrackMap ){
        if ( kv.second == nullptr ) continue;
        LOG_DEBUG << "MC Track: id=" << kv.first << ", nFTT=" << kv.second->mFttHits.size() << ", nFST=" << kv.second->mFstHits.size() << endm;
        nFstMcTracks[ kv.second->mFstHits.size() ]++;
        nFttMcTracks[ kv.second->mFttHits.size() ]++;
    }
    int idealNumberOfSeeds = (nFstMcTracks[3]);
    if (!mcTrackMap.empty()) {
        // Only print this if we have MC tracks
        LOG_INFO << "There are: " << Form( "%d with 0 FST, %d with 1 FST, %d with 2 FST, %d with 3 FST", nFstMcTracks[0], nFstMcTracks[1], nFstMcTracks[2], nFstMcTracks[3] ) << endm;
        LOG_INFO << "There are: " << Form( "%d with 0 FTT, %d with 1 FTT, %d with 2 FTT, %d with 3 FTT, %d with 4 FTT, %d with 5 FTT, %d with 6 FTT, %d with 7 FTT, %d with 8 FTT", nFttMcTracks[0], nFttMcTracks[1], nFttMcTracks[2], nFttMcTracks[3], nFttMcTracks[4], nFttMcTracks[5], nFttMcTracks[6], nFttMcTracks[7], nFttMcTracks[8] ) << endm;
        LOG_INFO << "There are " << Form( "%d McTracks with > 2 FST hits (#of possible seeds)", idealNumberOfSeeds  ) << endm;
    }

    /**********************************************************************/
    // Run Track finding + fitting
    LOG_DEBUG << ">>START Event Forward Tracking" << endm;
    LOG_INFO << "\tFinding FWD Track Seeds" << endm;
    mForwardTracker->findTrackSeeds();

    
    // Report the results of the seed finding, in the future we could provide more info about #hits etc.
    LOG_INFO << "<<Fwd Tracking Found : " << mForwardTracker -> getTrackSeeds().size() << " Track Seeds from " << fsiHitMap.size() << " FST hits and " << hitMap.size() << " sTGC hits"  << endm;
    if ( idealNumberOfSeeds > 0 ){
        float seedFindingEff = ( mForwardTracker -> getTrackSeeds().size() + 1e-5 ) / ( idealNumberOfSeeds + 1e-5 );
        LOG_INFO << "    (vs. " << idealNumberOfSeeds << " McTracks with FST>2, eff = " << seedFindingEff << ")" << endm;
    } 
    /**********************************************************************/
    
    /**********************************************************************/
    // Run Track fitting on the seeds we found
    LOG_INFO << "\tFitting FWD Track Seeds" << endm;
    mForwardTracker->doTrackFitting( mForwardTracker->getTrackSeeds() );
    LOG_INFO << "<<Fwd Tracking Fit :" << mForwardTracker -> getTrackResults().size() << " GenFit Tracks" << endm;
    LOG_DEBUG << "<<FINISH Event Forward Tracking" << endm;
    /**********************************************************************/


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
    TVector3 p = gtr.mMomentum;

    /*******************************************************************************/
    // store the seed points for the track
    int nSeedPoints = 0;
    float cov[9]; // reused covariance matrix for seed points
    for ( auto s : gtr.mSeed ){
        FwdHit * fh = static_cast<FwdHit*>( s );
        if (!fh) continue;
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


    // for seed only, we save the seed charge and momentum computed from the seed points
    fwdTrack->setCharge( gtr.mCharge );
    fwdTrack->setPrimaryMomentum( StThreeVectorD( gtr.mMomentum.X(), gtr.mMomentum.Y(), gtr.mMomentum.Z() ) );
    fwdTrack->setVtxIndexAndTrackType( gtr.mVertexIndex, gtr.mTrackType );
    fwdTrack->setGlobalTrackIndex( gtr.mGlobalTrackIndex);

    // Fit failed beyond use
    if ( !gtr.mIsFitConvergedPartially|| gtr.mNumFitPoints == 0 ){
        // if num points == 0 then calling PVal seg faults :/
        fwdTrack->setDidFitConverge( false );
        fwdTrack->setDidFitConvergeFully( false );
        fwdTrack->setNumberOfFailedPoints( 99 );
        fwdTrack->setNumberOfFitPoints( 0 );
        fwdTrack->setChi2( 0 );
        fwdTrack->setNDF( 0 );
        fwdTrack->setPval( 0 );

        fwdTrack->setNumberOfFitPoints( 1 ); // setting this to 1 so that the charge is still saved as charge * n
        
        gtr.Clear();
        return fwdTrack;
    }
    // Fill fit quality info
    fwdTrack->setDidFitConverge( gtr.mIsFitConverged );
    fwdTrack->setDidFitConvergeFully( gtr.mIsFitConvergedFully );
    fwdTrack->setNumberOfFailedPoints( gtr.mNFailedPoints );

    fwdTrack->setNumberOfFitPoints( gtr.mNumFitPoints );
    fwdTrack->setChi2( gtr.mChi2 );
    fwdTrack->setNDF( gtr.mNdf );
    fwdTrack->setPval( gtr.mPval );

    // DCA and fitted momentum
    fwdTrack->setDCA( gtr.mDCA.X(), gtr.mDCA.Y(), gtr.mDCA.Z() );
    fwdTrack->setPrimaryMomentum( StThreeVectorD( gtr.mMomentum.X(), gtr.mMomentum.Y(), gtr.mMomentum.Z() ) );

    /*******************************************************************************/
    // if the track did not converged, do not try to project it
    if ( !gtr.mIsFitConvergedFully ){
        gtr.Clear();
        LOG_WARN << "Genfit track did not converge fully, skipping projections" << endm;

        return fwdTrack;
    }

    /*******************************************************************************/
    // compute projections to z-planes of various detectors
    // TODO: update FCS to use correct z + angle
    // Use vector<pair> instead of map to allow multiple entries per detector
    std::vector<std::pair<int, float>> detectorZPlanes;

    // Add TPC projection
    detectorZPlanes.push_back({ kTpcId, 0.0 });

    // Add FST projections (check vector size first)
    for (size_t i = 0; i < mFstZFromGeom.size() && i < 3; i++) {
        detectorZPlanes.push_back({ kFstId, mFstZFromGeom[i] });
    }

    // Add FTT projections (check vector size first)
    for (size_t i = 0; i < mFttZFromGeom.size() && i < 4; i++) {
        detectorZPlanes.push_back({ kFttId, mFttZFromGeom[i] });
    }

    // Add FCS projections
    detectorZPlanes.push_back({ kFcsPresId, 375.0 });
    detectorZPlanes.push_back({ kFcsWcalId, 715.0 });
    detectorZPlanes.push_back({ kFcsHcalId, 807.0 });

    size_t zIndex = 0;
    TVector3 mom(0, 0, 0);
    TVector3 tv3(0, 0, 0);
    for ( auto zp : detectorZPlanes ){
        int detIndex = zp.first;
        float z = zp.second;
        tv3.SetXYZ(0, 0, 0);
        std::fill(std::begin(cov), std::end(cov), 0.0f);
        LOG_DEBUG << "Projecting to: " << detIndex << " at z=" << z << endm;
        if ( detIndex != kFcsHcalId && detIndex != kFcsWcalId ){
            float detpos[3] = {0,0,z};
            float detnorm[3] = {0,0,1};
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
            if (!mFcsDb) {
                continue;
            }
            StThreeVectorD xyzoff = mFcsDb->getDetectorOffset(det,mFcsDb->getShowerMaxZ(det));
            StThreeVectorD planenormal = mFcsDb->getNormal(det);
            float xyz0[3] = { 0, 0, 575.0 };
            float xyz1[3] = { 0, 0, 625.0 };
            float xyzdet[3] = { (float)xyzoff.x(), (float)xyzoff.y(), (float)xyzoff.z() };
            float detnorm[3] = { (float)planenormal.x(), (float)planenormal.y(), (float)planenormal.z() };
            LOG_DEBUG << "Projecting to: " << detIndex << endm;
            tv3 = ObjExporter::projectAsStraightLine( gtr.mTrack.get(), xyz0, xyz1, xyzdet, detnorm, cov, mom );
        }
        fwdTrack->mProjections.push_back( StFwdTrackProjection( detIndex, StThreeVectorF( tv3.X(), tv3.Y(), tv3.Z() ), StThreeVectorF( mom.X(), mom.Y(), mom.Z() ), cov) );
        // LOG_INFO << "Projection added for " << detIndex << " at z=" << z << endm;
        zIndex++;
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
    } else {
        ftc->Clear();
    }

    if (!mFcsDb) {
        LOG_ERROR << "FCS database not initialized, cannot project FwdTracks to FCS" << endm;
    }

    // Pack a 3x3 TMatrixDSym covariance into the 6-element lower-triangle
    // representation expected by StVertex::setCovariantMatrix:
    //   [xx, xy, yy, xz, yz, zz]
    auto packCov6 = []( const TMatrixDSym &cov, float out[6] ) {
        out[0] = cov(0,0);
        out[1] = cov(0,1);
        out[2] = cov(1,1);
        out[3] = cov(0,2);
        out[4] = cov(1,2);
        out[5] = cov(2,2);
    };

    // -------------------------------------------------------------------
    // Establish the StEvent PV index for the event vertex used by the fits
    // so that StFwdTrack::vertexIndex() points into stEvent->primaryVertices().
    // For TPC-sourced PVs we try to reuse an existing entry by position match;
    // otherwise we add a copy of our PV with a source-tagging finder id.
    int eventPvIndex = -1;
    if ( mFwdVertexSource == kFwdVertexSourceTpc ) {
        const double tol = 1e-3; // 10 microns
        for ( UInt_t i = 0; i < stEvent->numberOfPrimaryVertices(); i++ ) {
            StPrimaryVertex *pv = stEvent->primaryVertex(i);
            if (!pv) continue;
            if (pv->isFwdVtx()) continue; // skip ones we previously added
            const StThreeVectorF &pos = pv->position();
            double dx = pos.x() - mEventVertex.X();
            double dy = pos.y() - mEventVertex.Y();
            double dz = pos.z() - mEventVertex.Z();
            if ( dx*dx + dy*dy + dz*dz < tol*tol ) {
                eventPvIndex = static_cast<int>(i);
                break;
            }
        }
    }
    if ( eventPvIndex < 0 &&
         mFwdVertexSource != kFwdVertexSourceUnknown &&
         mFwdVertexSource != kFwdVertexSourceNone ) {
        StPrimaryVertex *pv = new StPrimaryVertex();
        pv->setPosition( StThreeVectorF( mEventVertex.X(), mEventVertex.Y(), mEventVertex.Z() ) );
        float cov6[6] = {0,0,0,0,0,0};
        packCov6( mEventVertexCov, cov6 );
        pv->setCovariantMatrix( cov6 );
        if ( mFwdVertexSource == kFwdVertexSourceMc ) {
            pv->setVertexFinderId( mcEventVertexFFinder );
        } else if ( mFwdVertexSource == kFwdVertexSourceVpd ) {
            pv->setBeamConstrained(); // VPD provides z only — effectively beam-constrained in (x,y)
        }
        stEvent->addPrimaryVertex( pv );
        eventPvIndex = static_cast<int>(stEvent->numberOfPrimaryVertices()) - 1;
    }

    // -------------------------------------------------------------------
    // Add the FWD (RAVE) vertices BEFORE the tracks so that we know each
    // secondary-fit track's vertex index. mVertexIndex stored in the
    // GenfitTrackResult is the 0-based position in mForwardTracker->getVertices(),
    // so adding fwdRaveOffset recovers the StEvent index.
    const int fwdRaveOffset = static_cast<int>(stEvent->numberOfPrimaryVertices());
    auto fwdVertices = mForwardTracker->getVertices();
    for ( auto vert : fwdVertices ) {
        StPrimaryVertex *pv = new StPrimaryVertex();
        pv->setPosition( StThreeVectorF( vert->getPos().X(), vert->getPos().Y(), vert->getPos().Z() ) );
        float cov6[6] = {0,0,0,0,0,0};
        packCov6( vert->getCov(), cov6 );
        pv->setCovariantMatrix( cov6 );
        pv->setChiSquared( vert->getChi2() );
        pv->setNumTracksUsedInFinder( vert->getNTracks() );
        pv->setFwdVertex();
        stEvent->addPrimaryVertex( pv );
    }

    // -------------------------------------------------------------------
    // Add the tracks, remapping mVertexIndex to the StEvent PV collection.
    size_t indexTrack = 0;
    for ( auto &gtr : mForwardTracker->getTrackResults() ) {
            LOG_INFO << "Processing GenfitTrackResult(type=" << gtr.mTrackType << "): " << indexTrack << " mIsFitConverged=" << gtr.mIsFitConverged << ", mIsFitConvergedPartially=" << gtr.mIsFitConvergedPartially << ", mNumFitPoints=" << gtr.mNumFitPoints << endm;

            // Translate the working index that the tracker stored into the
            // index that points into stEvent->primaryVertices(). For Global,
            // Beamline, and PrimaryVertex-constrained tracks the relevant PV
            // is the event PV (and DCA is measured against it). For
            // ForwardVertex-constrained tracks the relevant PV is the RAVE
            // forward vertex at offset + iVtx.
            if ( gtr.mTrackType == StFwdTrack::kForwardVertexConstrained ) {
                gtr.mVertexIndex = fwdRaveOffset + gtr.mVertexIndex;
            } else if ( eventPvIndex >= 0 ) {
                gtr.mVertexIndex = eventPvIndex;
            } else {
                gtr.mVertexIndex = 0; // no event PV in StEvent — fall back
            }

            StFwdTrack* fwdTrack = makeStFwdTrack( gtr, indexTrack );
            indexTrack++;
            if (nullptr == fwdTrack)
                continue;
            ftc->addTrack( fwdTrack );
    }

    LOG_INFO << "StFwdTrackCollection has " << ftc->numberOfTracks() << " tracks now" << endm;
}




//________________________________________________________________________
void StFwdTrackMaker::Clear(const Option_t *opts) {
    LOG_DEBUG << "StFwdTrackMaker::Clear" << endm;
    mForwardData->clear();
    mFwdHitLoader.clear();
    mForwardTracker->Clear();
}


std::string StFwdTrackMaker::defaultConfig = R"(
<?xml version="1.0" encoding="UTF-8"?>
<config>
    <TrackFinder nIterations="1">
        <Iteration nPhiSlices="1" > <!-- Options for first iteration -->
            <SegmentBuilder>
                <!-- <Criteria name="Crit2_RZRatio" min="0" max="1.20" /> -->
                <!-- <Criteria name="Crit2_DeltaRho" min="-50" max="50.9"/> -->
                <!-- Fix (Issue #22): require adjacent-disk r-strip change to be physical.
                     deltaRho = rhoParent - rhoChild (parent=outer disk, child=inner).
                     Same strip (s,s): deltaRho~0; adjacent strip (s,s+1): deltaRho~2.875 cm.
                     Rejects looping/backward hits (deltaRho < -0.3) and multi-strip jumps
                     (deltaRho > 3.1). 0.3 cm buffer for floating-point + disk misalignment.
                     Recommended by Barak Schmookler 2026-06-22. -->
                <Criteria name="Crit2_DeltaRho" min="-0.3" max="3.1" />
                <Criteria name="Crit2_DeltaPhi" min="0" max="2.0" />
                <!-- <Criteria name="Crit2_StraightTrackRatio" min="0.01" max="5.85"/> -->
            </SegmentBuilder>

            <ThreeHitSegments>
                <!-- Fix (Issue #22): Crit3_3DAngle/Crit3_2DAngle parameters are in
                     DEGREES, not radians. max="1" means 1 degree, not 1 radian. For a
                     seed with r-segments (s,s,s+1) -- one r-strip change between
                     adjacent disks, geometrically expected at eta~2.5-3 -- the 3D kink
                     angle is ~21 deg and the 2D angle is ~90 deg, both far above the 1
                     deg cut, so every valid seed with any r-strip change was rejected.
                     Removed both; replaced by Crit2_DeltaRho above.
                     [Barak Schmookler 2026-06-22] -->
                <!-- <Criteria name="Crit3_3DAngle" min="0" max="1" /> -->
                <!-- <Criteria name="Crit3_PT" min="0" max="100" /> -->
				<!-- <Criteria name="Crit3_ChangeRZRatio" min="0.8" max="1.21" /> -->
                <!-- <Criteria name="Crit3_2DAngle" min="0" max="1" /> -->
            </ThreeHitSegments>

        </Iteration>

        <Connector distance="1"/>

        <SubsetNN active="true" min-hits-on-track="3" >
            <!-- <InitialTemp>2.1</InitialTemp> -->
            <!-- <InfTemp>0.1</InfTemp> -->
            <Omega>0.99</Omega>
            <StableThreshold>0.001</StableThreshold>
        </SubsetNN>

        <HitRemover active="false" />
    </TrackFinder>
</config>
)";


const std::vector<Seed_t> &StFwdTrackMaker::getTrackSeeds() const{
    return mForwardTracker->getTrackSeeds();
}

const std::vector<GenfitTrackResult> &StFwdTrackMaker::getFitResults()const{
    return mForwardTracker->getTrackResults();
}
