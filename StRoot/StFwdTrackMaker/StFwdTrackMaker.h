#ifndef ST_FWD_TRACK_MAKER_H
#define ST_FWD_TRACK_MAKER_H

#include "StChain/StMaker.h"

#ifndef __CINT__
#include "GenFit/Track.h"
#endif

#include "FwdTrackerConfig.h"
#include "TVector3.h"

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

// ROOT includes
#include "TNtuple.h"
#include "TTree.h"
// STL includes
#include <vector>
#include <memory>


// 877-369-6347
class StFwdTrack;
class GenfitTrackResult;



const size_t MAX_TREE_ELEMENTS = 4000;
struct FwdTreeData {
  
    // hits;
    int fttN;
    vector<float> fttX, fttY, fttZ;
    vector<int> fttVolumeId;
    // Only avalaible for hits if MC
    vector<float> fttPt;
    vector<int> fttTrackId, fttVertexId;

    // hits;
    int fstN;
    vector<float> fstX, fstY, fstZ;
    vector<int> fstTrackId;

    int fcsN;
    vector<float> fcsX, fcsY, fcsZ;
    vector<int> fcsDet;

    // RC tracks
    int rcN;
    vector<float> rcPt, rcEta, rcPhi, rcQuality;
    vector<int> rcTrackId, rcNumFST, rcCharge, rcNumFTT, rcNumPV;

    // MC Tracks
    int mcN;
    vector<float> mcPt, mcEta, mcPhi;
    vector<int> mcVertexId, mcCharge;

    // MC Level vertex info
    // maybe use also for TPC vertex if available in data
    int vmcN;
    vector<float> vmcX, vmcY, vmcZ;

    int tprojN;
    vector<float> tprojX, tprojY, tprojZ;
    vector<float> tprojPx, tprojPy, tprojPz;
    vector<int> tprojIdD, tprojIdT;

    // RAVE reco vertices
    int vrcN;
    vector<float> vrcX, vrcY, vrcZ;

    int thdN;
    vector<float> thdX, thdY, thaX, thaY, thaZ;

    bool saveCrit = false;
    std::map<string, std::vector<float>> Crits;
    std::map<string, std::vector<int>> CritTrackIds;

};

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
    void SetGenerateHistograms( bool _genHisto ){ mGenHistograms = _genHisto; }
    void SetGenerateTree(bool _genTree) { mGenTree = _genTree; }
    void SetVisualize( bool _viz ) { mVisualize = _viz; }

    vector<StFwdTrack*> mFwdTracks;

  private:
  protected:

    // Track Seed typdef 
    typedef std::vector<KiTrack::IHit *> Seed_t;

    
    // for Wavefront OBJ export
    size_t eventIndex = 0;
    

    bool mGenHistograms = false;
    bool mGenTree = false;
    std::string mConfigFile;


    std::map<std::string, TH1 *> mHistograms;
    TFile *mTreeFile = nullptr;
    TTree *mTree     = nullptr;
    FwdTreeData mTreeData;

    bool mVisualize = false;
    vector<TVector3> mFttHits;
    vector<TVector3> mFstHits;
    vector<TVector3> mFcsClusters;
    vector<float> mFcsClusterEnergy;
    vector<TVector3> mFcsPreHits;

    std::vector< genfit::GFRaveVertex * > mRaveVertices;

    void ProcessFwdTracks();
    void FillEvent();
    void FillTrackDeltas();

    StFwdTrack * makeStFwdTrack( GenfitTrackResult &gtr, size_t indexTrack );

    // I could not get the library generation to succeed with these.
    // so I have removed them
    #ifndef __CINT__
        std::shared_ptr<SiRasterizer> mSiRasterizer;
        FwdTrackerConfig mFwdConfig;
        std::shared_ptr<ForwardTracker> mForwardTracker;
        std::shared_ptr<FwdDataSource> mForwardData;
        
        size_t loadMcTracks( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap );
        void loadFcs();
        void loadFttHits( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFttHitsFromStEvent( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFttHitsFromGEANT( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );

        void loadFstHits( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFstHitsFromGEANT( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFstHitsFromStEvent( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
    #endif

    void FillTTree(); // if debugging ttree is turned on (mGenTree)
    void FitVertex();

    static std::string defaultConfigSim;
    static std::string defaultConfigData;
    std::string defaultConfig;
    bool configLoaded = false;

    // Helper functions for modifying configuration
    // NOTE: to override configuration, call individual functions after setConfigForXXX
    void setConfigForData() { defaultConfig = defaultConfigData; LoadConfiguration(); }
    void setConfigForSim()  { defaultConfig = defaultConfigSim; LoadConfiguration();  }
    
    // General / hit and uncertainty
    void setOutputFilename( std::string fn ) { mFwdConfig.set( "Output:url", fn ); }
    void setFttHitSource( std::string source ) { mFwdConfig.set( "Source:ftt", source ); }
    void setFstRasterR( double r = 3.0 /*cm*/ ){ mFwdConfig.set<double>( "SiRasterizer:r", r ); }
    void setFstRasterPhi( double phi = 0.00409 /*2*pi/(12*128)*/ ){ mFwdConfig.set<double>( "SiRasterizer:phi", phi ); }

    //Track Finding
    void setSeedFindingWithFtt() { mFwdConfig.set( "TrackFinder:source", "ftt" ); }
    void setSeedFindingWithFst() { mFwdConfig.set( "TrackFinder:source", "fst" ); }
    void setSeedFindingNumInterations( int n = 1 ) { mFwdConfig.set<int>("TrackFinder:nIterations", n); }
    void setSeedFindingNumPhiSlices( int n = 8 ) { mFwdConfig.set<int>("TrackFinder.Iteration:nPhiSlices", n); }
    void setSeedFindingConnectorDistance( int d = 1 ) { mFwdConfig.set<int>( "TrackFinder.Connector:distance", d ); }
    void setSeedFindingUseSubsetNN( bool use = true ) { mFwdConfig.set<bool>( "TrackFinder.SubsetNN:active", use ); }
    void setSeedFindingMinHitsOnTrack( int n = 3 ) { mFwdConfig.set<int>( "TrackFinder.SubsetNN:min-hits-on-track", n ); }
    void setSeedFindingUseHitRemover( bool use = true ) { mFwdConfig.set<bool>( "TrackFinder.HitRemover:active", use ); }
    void setUseTruthSeedFinding( bool use = true ) { mFwdConfig.set<bool>( "TrackFinder:active", !use ); }

    // Track Fitting
    void setTrackFittingOff() { mFwdConfig.set( "TrackFitter:active", "false" ); }
    void setFittingMaterialEffects( bool mat = true) { mFwdConfig.set<bool>( "TrackFitter:materialEffects", mat ); }
    void setPrimaryVertexSigmaXY( double sXY ) { mFwdConfig.set<double>( "TrackFitter.Vertex:sigmaXY", sXY ); }
    void setPrimaryVertexSigmaZ(  double sZ ) { mFwdConfig.set<double>( "TrackFitter.Vertex:sigmaZ", sZ ); }
    void setIncludePrimaryVertexInFit( bool pvf = true ) { mFwdConfig.set<bool>( "TrackFitter.Vertex:includeInFit", pvf ); }
    void setZeroB( bool zeroB = true ) { mFwdConfig.set<bool>( "TrackFitter:zeroB", zeroB ); }
    void setConstB( bool constB = true ) { mFwdConfig.set<bool>( "TrackFitter:constB", constB ); }
    void setUseMcSeedForFit( bool mcSeed = true ) { mFwdConfig.set<bool>( "TrackFitter:mcSeed", mcSeed ); }

    void setRefitWithFst() { mFwdConfig.set( "TrackFitter:refitSi", "true" ); mFwdConfig.set( "TrackFitter:refitFtt", "false" ); }
    void setRefitWithFtt() { mFwdConfig.set( "TrackFitter:refitSi", "false" ); mFwdConfig.set( "TrackFitter:refitFtt", "true" ); }

    void setMaxFailedHitsInFit( int n = -1 /*no lim*/ ) {mFwdConfig.set<int>("TrackFitter.KalmanFitterRefTrack:MaxFailedHits", n);}
    void setFitDebugLvl( int level = 0 /*0=no output*/ ) {mFwdConfig.set<int>("TrackFitter.KalmanFitterRefTrack:DebugLvl", level); }
    void setFitMaxIterations( int n=4 ) {mFwdConfig.set<int>("TrackFitter.KalmanFitterRefTrack:MaxIterations", n); }
    void setFitMinIterations( int n = 1) {mFwdConfig.set<int>("TrackFitter.KalmanFitterRefTrack:MinIterations", n); }

    // for MC
    void setSmearMcPrimaryVertex( bool pvs = true ) { mFwdConfig.set<bool>( "TrackFitter.Vertex:smearMcVertex", pvs ); }
  
};

#endif
