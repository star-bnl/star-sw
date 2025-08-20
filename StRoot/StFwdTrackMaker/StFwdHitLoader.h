#ifndef ST_FWD_HIT_LOADER_H
#define ST_FWD_HIT_LOADER_H

#ifndef __CINT__
#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StFwdTrackMaker/include/Tracker/FwdDataSource.h"
#else
class FwdHit;
#endif

class StEvent;
class StMuDstMaker;
class St_g2t_fts_hit;
class StFcsDb;


class FstRasterizer {
  public:
    FstRasterizer( double r = 3.0, double phi = 0.0040906154 ) { 
        mRasterR = r;
        mRasterPhi = phi;
    }
    void setRPhi( double r, double phi ) {
        mRasterR = r;
        mRasterPhi = phi;
    }
    ~FstRasterizer() {}

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
    double mRasterR, mRasterPhi;
};

class StFwdHitLoader {
 public:
    StFwdHitLoader() : 
    mFttDataSource(StFwdHitLoader::DataSource::STEVENT), 
    mFstDataSource(StFwdHitLoader::DataSource::STEVENT), 
    mEpdDataSource(StFwdHitLoader::DataSource::STEVENT) 
    {}
    ~StFwdHitLoader() {}
    void clear() {
      #if !defined (__CINT__)
        mFwdHitsFtt.clear();
        mFwdHitsFst.clear();
        mFwdHitsEpd.clear();
      #endif

        // clear vectors for visualization OBJ hits
        mSpacepointsFtt.clear();
        mSpacepointsFst.clear();
        mSpacepointsEpd.clear();
    }

  #if !defined(__CINT__) && !defined(__CLING__)
    /********************************/
    // Load hits from the FTT detector (location based on mFttDataSource)
    //  * @param mcTrackMap : map of mc tracks
    //  * @param hitMap : FTT hitmap to populate
    //  * @return number of hits loaded
    //  * @note: This function is called by StFwdTrackMaker::Make()
    int loadFttHits( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap );
    int loadFttPointsFromStEvent( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap );
    int loadFttPointsFromGEANT( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap );

    /********************************/
    // Load hits from the FST detector (location based on mFstDataSource)
    //  * @param mcTrackMap : map of mc tracks
    //  * @param hitMap : FST hitmap to populate
    //  * @return number of hits loaded
    //  * @note: This function is called by StFwdTrackMaker::Make()
    int loadFstHits( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap );
    int loadFstHitsFromStEvent( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap );
    int loadFstHitsFromMuDst( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap );
    int loadFstHitsFromGEANT( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap );

    /********************************/
    // Load hits from the EPD detector (location based on mEpdDataSource)
    //  * @param mcTrackMap : map of mc tracks
    //  * @param hitMap : EPD hitmap to populate
    //  * @return number of hits loaded
    //  * @note: This function is called by StFwdTrackMaker::Make()
    int loadEpdHits( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap, StFcsDb *fcsDb = nullptr );
    int loadEpdHitsFromStEvent( FwdDataSource::McTrackMap_t &mcTrackMap, FwdDataSource::HitMap_t &hitMap, StFcsDb *fcsDb = nullptr );
  #endif

    // all caps to avoid conflict with types themselves
    enum DataSource { GEANT=0, STEVENT, MUDST, IGNORE };
    /********************************/
    // Specify the data source for FTT, FST and EPD hits
    //  * @param ds : data source (GEANT, StEvent, MuDst)
    //  * @return None
    void setFttDataSource( DataSource ds ) { mFttDataSource = ds; }
    void setFstDataSource( DataSource ds ) { mFstDataSource = ds; }
    void setEpdDataSource( DataSource ds ) { mEpdDataSource = ds; }
    void setDataSource( DataSource ds ) { mFttDataSource = ds; mFstDataSource = ds; mEpdDataSource = ds; }

    void setStEvent( StEvent *stEvent ) { mStEvent = stEvent; }
    void setMuDstMaker( StMuDstMaker *muDstMaker ) { mMuDstMaker = muDstMaker; }
    void setTables( St_g2t_fts_hit *stg_hits, St_g2t_fts_hit *fst_hits, St_g2t_fts_hit *epd_hits ) {
        mGeantFtt = stg_hits;
        mGeantFst = fst_hits;
        mGeantEpd = epd_hits;
    }
    FstRasterizer mFstRasterizer;
  protected:
    DataSource mFttDataSource;
    DataSource mFstDataSource;
    DataSource mEpdDataSource;
    float mEpdThreshold = 0.2;

    // Pointers to these are used by StFwdTrackMaker, clear the vectors after each event
    #if !defined (__CINT__)
    vector<FwdHit> mFwdHitsFtt;
    vector<FwdHit> mFwdHitsFst;
    vector<FwdHit> mFwdHitsEpd;

    // this disables logging at compile time
    constexpr static int kLogVerbose = 10;
    constexpr static int kLogInfo = 1;
    constexpr static int kLogSilent = 0;
    constexpr static int kLogLevel = kLogSilent;
    #endif

    vector<TVector3> mSpacepointsFtt;
    vector<TVector3> mSpacepointsFst;
    vector<TVector3> mSpacepointsEpd;

    /// Non-owning pointer: StEvent provided externally via `setStEvent`
    StEvent *mStEvent = nullptr; // pointer to StEvent
    /// Non-owning pointer: StMuDstMaker provided externally via `setMuDstMaker`
    StMuDstMaker *mMuDstMaker = nullptr; // pointer to StMuDstMaker

    /// Non-owning pointers to GEANT hits
    St_g2t_fts_hit *mGeantFtt = nullptr; // pointer to GEANT FTT hits
    St_g2t_fts_hit *mGeantFst = nullptr; // pointer to GEANT FST hits
    St_g2t_fts_hit *mGeantEpd = nullptr; // pointer to GEANT EPD hits

    

    bool verbosity = 0;
};


#endif // ST_FWD_HIT_LOADER_H