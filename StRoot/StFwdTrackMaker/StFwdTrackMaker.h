#ifndef ST_FWD_TRACK_MAKER_H
#define ST_FWD_TRACK_MAKER_H

#include "StChain/StMaker.h"

#ifndef __CINT__
#include "GenFit/Track.h"
#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#endif

#include "FwdTrackerConfig.h"
#include "TVector3.h"
#include "TMatrix.h"

namespace KiTrack {
class IHit;
};

namespace genfit {
  class Track;
  class GFRaveVertex;
}

class ForwardTracker;
class ForwardTrackMaker;
class FwdDataSource;
class FwdHit;
class StarFieldAdaptor;

class StGlobalTrack;
class StRnDHitCollection;
class StTrack;
class StTrackDetectorInfo;
class SiRasterizer;
class McTrack;

class StFcsDb;

// ROOT includes
#include "TNtuple.h"
#include "TTree.h"
// STL includes
#include <vector>
#include <memory>
class StFwdTrack;
class GenfitTrackResult;


class StFwdTrackMaker : public StMaker {

    ClassDef(StFwdTrackMaker, 0);

  public:
    StFwdTrackMaker();
    ~StFwdTrackMaker(){/* nada */};

    int Init();
    int Finish();
    int Make();
    void Clear(const Option_t *opts = "");

    enum { kInnerGeometry,
           kOuterGeometry };

    void SetConfigFile(std::string n) {
        mConfigFile = n;
        LoadConfiguration();
    }
    void LoadConfiguration();
    void SetVisualize( bool _viz ) { mVisualize = _viz; }

    vector<StFwdTrack*> mFwdTracks;

    vector<FwdHit> &GetFttHits() { return mFwdHitsFtt; }
    vector<FwdHit> &GetFstHits() { return mFwdHitsFst; }

  #ifndef __CINT__
    // Get the FwdTracker object
    std::shared_ptr<ForwardTracker> GetForwardTracker() { return mForwardTracker; }
    const std::vector<Seed_t> &getTrackSeeds() const;
    const std::vector<GenfitTrackResult> &getFitResults() const;
  #endif
    TVector3 GetEventPrimaryVertex();

  private:
  protected:

  StFcsDb* mFcsDb = 0; // Pointer to fcs db object
  
    // Event Filters
    float mEventFilterMinTofMult = 2;
    bool  mEventFilterRequireEventVertex = false;
    bool  mEventFilterRequireVpdVertex = true;
    float mEventFilterMinVpdZ = -99;
    float mEventFilterMaxVpdZ = 99;

    // for Wavefront OBJ export
    size_t eventIndex = 0; // counts up for processed events
    size_t mEventNum = 0; // global event num (index)
    TVector3 mEventVertex; // primary vertex used in fwd tracking this event

    std::string mConfigFile;

    std::map<std::string, TH1 *> mHistograms;

    bool mVisualize = false; // if true,write out a Wavefront OBJ to visualize the event in 3D
    vector<TVector3> mFttHits;
    vector<TVector3> mFstHits;
    vector<TVector3> mFcsClusters;
    vector<float> mFcsClusterEnergy;
    vector<TVector3> mFcsPreHits;

    std::vector< genfit::GFRaveVertex * > mRaveVertices;
    vector<float> mFttZFromGeom, mFstZFromGeom;

    void ProcessFwdTracks();
    void FillEvent();
    void FillTrackDeltas();
    bool SkipEvent();

    StFwdTrack * makeStFwdTrack( GenfitTrackResult &gtr, size_t indexTrack );

    // I could not get the library generation to succeed with these.
    // so I have removed them
    #ifndef __CINT__
        TMatrixDSym mEventVertexCov; // covariance matrix for the primary vertex
        enum FwdVertexSource { kFwdVertexSourceUnknown, kFwdVertexSourceNone, kFwdVertexSourceTpc, kFwdVertexSourceMc, kFwdVertexSourceVpd }; // unknown means we havent looked yet
        FwdVertexSource mFwdVertexSource = StFwdTrackMaker::kFwdVertexSourceUnknown;
        vector<FwdHit> mFwdHitsFtt;
        vector<FwdHit> mFwdHitsFst;
        std::shared_ptr<SiRasterizer> mSiRasterizer;
        FwdTrackerConfig mFwdConfig;
        std::shared_ptr<ForwardTracker> mForwardTracker;
        std::shared_ptr<FwdDataSource> mForwardData;
        size_t loadMcTracks( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap );
        void loadFcs();
        void loadFttHits( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFttHitsFromStEvent( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFttHitsFromGEANT( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );

        int loadFstHits( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap );
        int loadFstHitsFromMuDst( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap );
        int loadFstHitsFromGEANT( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap );
        int loadFstHitsFromStEvent( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap );
        int loadFstHitsFromStRnDHits( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap );
    #endif


    /** @brief Fit the primary vertex using FWD tracks */
    void FitVertex();

    static std::string defaultConfig;
    bool configLoaded = false;
    TString mGeoCache;

    // Helper functions for modifying configuration
    public:
    /** @brief Set the filename for output ROOT file
     * @param fn : filename of output ROOT file
    */
    void setOutputFilename( std::string fn ) { mFwdConfig.set( "Output:url", fn ); }
    /** @brief Set the data source for FTT hits
     *
     * @param source : {DATA, GEANT}, DATA means read from StEvent, GEANT means read directly from the GEANT hits
    */
    void setFttHitSource( std::string source ) { mFwdConfig.set( "Source:ftt", source ); }

    /** @brief Enable or disable the Fst Rasterizer
     * @param use : if true, load FST hits from GEANT and raster them according to r, phi resolutions.
    */
    void setUseFstRasteredGeantHits( bool use = true ){ mFwdConfig.set<bool>( "SiRasterizer:active", use ); }
    /** @brief Set the resolution in R for rasterizing FST hits (from fast sim)
     * Only used when the Rasterizer is enabled, which results from reading FST hits from GEANT
     * @param r : resolution in r (cm)
    */
    void setFstRasterR( double r = 3.0 /*cm*/ ){ mFwdConfig.set<double>( "SiRasterizer:r", r ); }
    /** @brief Set the resolution in phi for rasterizing FST hits (from fast sim)
     * Only used when the Rasterizer is enabled, which results from reading FST hits from GEANT
     * @param phi : resolution in phi (rad)
    */
    void setFstRasterPhi( double phi = 0.00409 /*2*pi/(12*128)*/ ){ mFwdConfig.set<double>( "SiRasterizer:phi", phi ); }

    //Track Finding
    /** @brief Use FST and Ftt hits (sequentially) in the Seed Finding - then merge tracks
     *
    */
    void setSeedFindingWithFstFttSequential() { mFwdConfig.set( "TrackFinder:source", "seq" ); }
    /** @brief Use FST and Ftt hits (simultaneously) in the Seed Finding
     *
    */
    void setSeedFindingWithFstFttSimultaneous() { mFwdConfig.set( "TrackFinder:source", "sim" ); }
    /** @brief Use Ftt hits in the Seed Finding
     *
    */
    void setSeedFindingWithFtt() { mFwdConfig.set( "TrackFinder:source", "ftt" ); }
    /** @brief Use Fst hits in the Seed Finding
     *
    */
    void setSeedFindingWithFst() { mFwdConfig.set( "TrackFinder:source", "fst" ); }
    /** @brief Set the number of track finding iterations
     * @param n : number of iterations to run
    */
    void setSeedFindingNumInterations( int n = 1 ) { mFwdConfig.set<int>("TrackFinder:nIterations", n); }
    /** @brief Set the number of phi slices to split the track iterations into
     * @param n : number of slices of equal size (2pi)/n
    */
    void setSeedFindingNumPhiSlices( int n = 8 ) { mFwdConfig.set<int>("TrackFinder.Iteration:nPhiSlices", n); }
    /** @brief Set the connector distance for track finding
     * @param d : distance between planes (1 = adjacent)
    */
    void setSeedFindingConnectorDistance( int d = 1 ) { mFwdConfig.set<int>( "TrackFinder.Connector:distance", d ); }
    /** @brief Enable or disable the SubsetNN
     * @param use : if true, enables the subsetNN which find the most compatible set of tracks without shared hits
     *            if false, all tracks are reported regardless of shared hits
    */
    void setSeedFindingUseSubsetNN( bool use = true ) { mFwdConfig.set<bool>( "TrackFinder.SubsetNN:active", use ); }
    /** @brief Enable or disable the SubsetNN
     * @param n : minimum number of hits on a track seed. Seeds with fewer hits are discarded
    */
    void setSeedFindingMinHitsOnTrack( int n = 3 ) { mFwdConfig.set<int>( "TrackFinder.SubsetNN:min-hits-on-track", n ); }
    /** @brief Enable or disable the HitRemover
     * @param use : if true, enables the hit remover which removes any hits from the hitmap that were used in a track
     *            if false, hits are not removed after each iteration
    */
    void setSeedFindingUseHitRemover( bool use = true ) { mFwdConfig.set<bool>( "TrackFinder.HitRemover:active", use ); }
    /** @brief Enable or disable the Truth Seed finding
     * @param use : if true, use Mc info to group hits into track seeds
     *            if false, seed finding uses options as in the case for data
    */
    void setUseTruthSeedFinding( bool use = true ) { mFwdConfig.set<bool>( "TrackFinder:active", !use ); }

    // Track Fitting
    /** @brief Turn off track fitting
     * Useful if you want to speed up the run but dont need fitting (testing seed finding)
    */
    void setTrackFittingOff() { mFwdConfig.set( "TrackFitter:active", "false" ); }
    /** @brief Enable / disable material effects
     * Material effects in kalman filter
    */
    void setFittingMaterialEffects( bool mat = true) { mFwdConfig.set<bool>( "TrackFitter:materialEffects", mat ); }
    /** @brief Set the resolution for the Primary Vertex in XY
     * @params sXY : sigma in XY (cm)
    */
    void setPrimaryVertexSigmaXY( double sXY ) { mFwdConfig.set<double>( "TrackFitter.Vertex:sigmaXY", sXY ); }
    /** @brief Set the resolution for the Primary Vertex in Z
     * @params sZ : sigma in Z (cm)
    */
    void setPrimaryVertexSigmaZ(  double sZ ) { mFwdConfig.set<double>( "TrackFitter.Vertex:sigmaZ", sZ ); }
    // TODO: add options for beamline constraint
    /** @brief Set B-field to zero (for zero field running)
     * @param zeroB : if true, use Zero B field
    */
    void setZeroB( bool zeroB = true ) { mFwdConfig.set<bool>( "TrackFitter:zeroB", zeroB ); }
    /** @brief Set B-field to constant (even outside of TPC)
     * @param constB : if true, use const 0.5T B field
    */
    void setConstB( bool constB = true ) { mFwdConfig.set<bool>( "TrackFitter:constB", constB ); }
    /** @brief Force the use of McSeed for fit
     * @param mcSeed : if true, use mc momentum as the seed for the track fitter
    */
    void setUseMcSeedForFit( bool mcSeed = true ) { mFwdConfig.set<bool>( "TrackFitter:mcSeed", mcSeed ); }

    /** @brief Sets the tracking to refit
     * This adds compatible hits from whichever detector was NOT used in seed finding
     * if FTT seeding -> project to and add FST hits
     * if FST seeding -> project to and add FTT hits
     * @param refit : true, perform refit, false do not
    */
    void setTrackRefit( bool refit = true) { mFwdConfig.set<bool>( "TrackFitter:refit", refit ); }

    /** @brief Sets the maximum number of hits that can be considered failed before the entire track fit fails
     * @param n : number of failed hits allowed, -1 = no limit
    */
    void setMaxFailedHitsInFit( int n = -1 /*no lim*/ ) {mFwdConfig.set<int>("TrackFitter.KalmanFitterRefTrack:MaxFailedHits", n);}
    /** @brief Sets Fitter debug level
     * @param level : 0 = no output, higher numbers are more verbose
    */
    void setFitDebugLvl( int level = 0 /*0=no output*/ ) {mFwdConfig.set<int>("TrackFitter.KalmanFitterRefTrack:DebugLvl", level); }
    /** @brief Sets Max fit iterations before failing
     * @param n : num iterations
    */
    void setFitMaxIterations( int n=4 ) {mFwdConfig.set<int>("TrackFitter.KalmanFitterRefTrack:MaxIterations", n); }
    /** @brief Sets Min fit iterations before converging
     * @param n : num iterations
    */
    void setFitMinIterations( int n = 1) {mFwdConfig.set<int>("TrackFitter.KalmanFitterRefTrack:MinIterations", n); }

    /** @brief Enables smearing of the MC Primary Vertex according to sigmaXY,Z
     * @param pvs : if true, smear vertex
    */
    void setSmearMcPrimaryVertex( bool pvs = true ) { mFwdConfig.set<bool>( "TrackFitter.Vertex:smearMcVertex", pvs ); }

    /**
     * @brief Sets geometry cache filename
     *
     */
    void setGeoCache( TString gc ) { mGeoCache = gc; }

    /**
     * @brief Set a generic Key Value in the Config object
     *
     * @param k key: any string representing absolute path e.g. `the.path.to.node:attribute`
     * @param v value: value encoded as a string
     */
    void setConfigKeyValue( std::string k, std::string v ){
      mFwdConfig.set( k, v );
    }

    /** @brief Sets a criteria value in the config for 2-hit criteria
     *  @param string name: name of the crit2, e.g. Crit2_RZRatio
     *  @param double min: minimum for the criteria, meaning depends on specific crit2
     *  @param double max: maximum for the criteria, meaning depends on specific crit2
     */
    void setCrit2( std::string name, double min, double max ){
      for ( auto p : mFwdConfig.childrenOf( "TrackFinder.Iteration.SegmentBuilder" ) ){
        auto nName = mFwdConfig.get<std::string>( p + ":name", "DNE" );
        if (nName == name) {
          LOG_DEBUG << "Setting Crit2=" << nName << " (min=" << min << ", max=" << max << ")" << endm;
          mFwdConfig.set<double>(p + ":min", min );
          mFwdConfig.set<double>(p + ":max", max );
          return;
        }
      } // loop on existing crit2
      // if we got here then the crit did not exist

    }

    /** @brief Sets a criteria value in the config for 3-hit criteria
     *  @param string name: name of the crit3, e.g. Crit2_RZRatio
     *  @param double min: minimum for the criteria, meaning depends on specific crit2
     *  @param double max: maximum for the criteria, meaning depends on specific crit2
     */
    void setCrit3( std::string name, double min, double max ){
      for ( auto p : mFwdConfig.childrenOf( "TrackFinder.Iteration.ThreeHitSegments" ) ){
        auto nName = mFwdConfig.get<std::string>( p + ":name", "DNE" );
        if (nName == name) {
          LOG_DEBUG << "Setting Crit3=" << nName << " (min=" << min << ", max=" << max << ")" << endm;
          mFwdConfig.set<double>(p + ":min", min );
          mFwdConfig.set<double>(p + ":max", max );
          return;
        }
      } // loop on existing crit3
      // if we got here then the crit did not exist
    }

};

#endif
