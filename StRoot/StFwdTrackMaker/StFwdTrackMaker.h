#ifndef ST_FWD_TRACK_MAKER_H
#define ST_FWD_TRACK_MAKER_H

#include "StChain/StMaker.h"

#ifndef __CINT__
#include "GenFit/Track.h"
#endif

#include "FwdTrackerConfig.h"

namespace KiTrack {
class IHit;
};

namespace genfit {
  class Track;
}

class ForwardTracker;
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

const size_t MAX_TREE_ELEMENTS = 4000;

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
    }
    void SetGenerateHistograms( bool _genHisto ){ mGenHistograms = _genHisto; }
    void SetGenerateTree(bool _genTree) { mGenTree = _genTree; }

  private:
  protected:

    // Track Seed typdef 
    typedef std::vector<KiTrack::IHit *> Seed_t;

    

    bool mGenHistograms = false;
    std::map<std::string, TH1 *> mHistograms;
    TFile *mTreeFile = nullptr;
    TTree *mTree     = nullptr;
    bool mGenTree = false;
    std::string mConfigFile;

    // elements used only if the mGenTree = true
    float mTreeX[MAX_TREE_ELEMENTS], mTreeY[MAX_TREE_ELEMENTS], mTreeZ[MAX_TREE_ELEMENTS], mTreeHPt[MAX_TREE_ELEMENTS];
    int mTreeN, mTreeTID[MAX_TREE_ELEMENTS], mTreeVID[MAX_TREE_ELEMENTS], mTreeHSV[MAX_TREE_ELEMENTS];

    int mTreeNTracks, mTreeRNTracks, mTreeRTID[MAX_TREE_ELEMENTS], mTreeVertID[MAX_TREE_ELEMENTS];
    unsigned short mTreeRNumFst[MAX_TREE_ELEMENTS];
    short mTreeQ[MAX_TREE_ELEMENTS], mTreeRQ[MAX_TREE_ELEMENTS];
    float mTreePt[MAX_TREE_ELEMENTS], mTreeEta[MAX_TREE_ELEMENTS], mTreePhi[MAX_TREE_ELEMENTS];
    float mTreeRPt[MAX_TREE_ELEMENTS], mTreeREta[MAX_TREE_ELEMENTS], mTreeRPhi[MAX_TREE_ELEMENTS], mTreeRQual[MAX_TREE_ELEMENTS];

    int mTreeNVert;
    float mTreeVertX[MAX_TREE_ELEMENTS], mTreeVertY[MAX_TREE_ELEMENTS], mTreeVertZ[MAX_TREE_ELEMENTS];
    std::map<string, std::vector<float>> mTreeCrits;
    std::map<string, std::vector<int>> mTreeCritTrackIds;

    // I could not get the library generation to succeed with these.
    // so I have removed them
    #ifndef __CINT__
        std::shared_ptr<SiRasterizer> mSiRasterizer;
        FwdTrackerConfig mFwdConfig;
        std::shared_ptr<ForwardTracker> mForwardTracker;
        std::shared_ptr<FwdDataSource> mForwardData;
        void loadMcTracks( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap );
        void loadStgcHits( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadStgcHitsFromGEANT( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadStgcHitsFromStEvent( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFstHits( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFstHitsFromGEANT( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
        void loadFstHitsFromStEvent( std::map<int, std::shared_ptr<McTrack>> &mcTrackMap, std::map<int, std::vector<KiTrack::IHit *>> &hitMap, int count = 0 );
    #endif

    void FillTTree(); // if debugging ttree is turned on (mGenTree)
    // Fill StEvent
    void FillEvent();
    void FillDetectorInfo(StTrackDetectorInfo *info, const genfit::Track *track, bool increment);
    void FillTrack(StTrack *otrack, const genfit::Track *itrack, const Seed_t &iseed, StTrackDetectorInfo *info);
    void FillTrackFlags(StTrack *otrack, const genfit::Track *itrack);
    void FillTrackGeometry(StTrack *otrack, const genfit::Track *itrack, double zplane, int io);
    void FillTrackDcaGeometry ( StGlobalTrack    *otrack, const genfit::Track *itrack );
    void FillTrackFitTraits(StTrack *otrack, const genfit::Track *itrack);
    void FillTrackMatches(StTrack *otrack, const genfit::Track *itrack);
};

#endif
