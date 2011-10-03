//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-------------------------------------------------
// header file for classes MatchedHitPair
//                         TrackInfo
//                         rcTrackInfo:TrackInfo
//                         mcTrackInfo:TrackInfo
//                         MatchedTrackPair
//-------------------------------------------------
#ifndef StTpcEvalEvent_hh
#define StTpcEvalEvent_hh

// To store these classes directly in a TTree
// we need to inherit from TObject
#include "TObject.h"
#include "TClonesArray.h"

#include "StLorentzVectorF.hh"
#include "StThreeVectorD.hh"

class StTpcDb;
class StMcTpcHit;
class StMcTrack;
class StTpcHit;
class StGlobalTrack;

//-------------------------------------------------
class MatchedHitPair {
public:
    MatchedHitPair(StTpcDb*); //!
    StThreeVectorF&   resolution(); //!
    StThreeVectorF&   resolution(const StMcTpcHit*, const StTpcHit*); //!
private:
    StTpcDb*          mStTpcDb; //!
    StThreeVectorF    mResolution; //!
};
//-------------------------------------------------
class TrackInfo : public TObject {
public:
    TrackInfo();
    virtual ~TrackInfo();
    Int_t       trackId();
    UInt_t      numberOfHits();
    UInt_t      numberOfMatchedHits();
    UInt_t      numberOfMatchedTracks();
    
    void             setId(Int_t);
    void             setHits(UInt_t);
    void             setMatchedHits(UInt_t);
    void             setMatchedTracks(UInt_t);
private:
    Int_t      mId;
    UInt_t     mHits;
    UInt_t     mMatchedHits;
    UInt_t     mMatchedTracks;

    ClassDef(TrackInfo,1)
};
//-------------------------------------------------
class rcTrackInfo : public TrackInfo {
public:
    rcTrackInfo();
    virtual ~rcTrackInfo();
    StThreeVectorD&  momentum();
    UInt_t           fitHits();
    void             setMomentum(StThreeVectorD);
    void             setFitHits(UInt_t);
private:
    StThreeVectorD   mMomentum;
    UInt_t           mFitHits;
    
    ClassDef(rcTrackInfo,1)
};
//-------------------------------------------------
class mcTrackInfo : public TrackInfo {
public:
  mcTrackInfo();
  virtual ~mcTrackInfo();
  StLorentzVectorF&  fourMomentum();
  void               setFourMomentum(const StLorentzVectorF&);
private:
  StLorentzVectorF mFourMomentum;

    ClassDef(mcTrackInfo,1)

};

//-------------------------------------------------
class MatchedTrackPair : public TObject {
public:

    MatchedTrackPair();
    virtual ~MatchedTrackPair();
    //    MatchedTrackPair(const MatchedTrackPair&);
    mcTrackInfo*        mcInfo();
    rcTrackInfo*        rcInfo();
    UInt_t              commonHits();
    StThreeVectorF&     momentumResolution();
    StThreeVectorF&     spatialResolution();
    StThreeVectorF&     spatialResolutionRMS();
    
    void  setMomentumResolution(StThreeVectorF&);
    void  setCommonHits(UInt_t);
    void  addCommonHit();
    void  setSpatialResolution(StThreeVectorF&);
    void  setSpatialResolutionRMS(StThreeVectorF&);
    void  addHitResolution(StThreeVectorF&);
    
private:
    mcTrackInfo     mMcInfo;
    rcTrackInfo     mRcInfo;
    UInt_t          mCommonHits;
    UInt_t          mHitCounter;
    StThreeVectorF  mMomentumResolution;
    StThreeVectorF  mSpatialResolution;
    StThreeVectorF  mSpatialResolutionRMS;
    
    ClassDef(MatchedTrackPair,1)

};
//-------------------------------------------------
class StTpcEvalEventHeader : public TObject {
private:
    UInt_t mNumberOfGeantTracks;
    UInt_t mNumberOfGlobalTracks;
    UInt_t mNumberOfPrimaryGeantTracks;
    UInt_t mNumberOfPrimaryReconTracks;
    UInt_t mNumberOfMcTpcHits;
    UInt_t mNumberOfRecTpcHits;

public:
    StTpcEvalEventHeader() : mNumberOfGeantTracks(0),
			  mNumberOfGlobalTracks(0),
			  mNumberOfPrimaryGeantTracks(0),
			  mNumberOfPrimaryReconTracks(0) {}
    virtual ~StTpcEvalEventHeader() {};
    void setHeader(UInt_t geTrk, UInt_t globTrk, UInt_t gePTrk, UInt_t primTrk, UInt_t mcTpcH, UInt_t rcTpcH) {
	mNumberOfGeantTracks        = geTrk;
	mNumberOfGlobalTracks       = globTrk;
	mNumberOfPrimaryGeantTracks = gePTrk;
	mNumberOfPrimaryReconTracks = primTrk;
	mNumberOfMcTpcHits          = mcTpcH;
	mNumberOfRecTpcHits         = rcTpcH;
    }

    UInt_t geantTracks()        const { return mNumberOfGeantTracks; }
    UInt_t globalTracks()       const { return mNumberOfGlobalTracks; }
    UInt_t geantPrimaryTracks() const { return mNumberOfPrimaryGeantTracks; }
    UInt_t reconPrimaryTracks() const { return mNumberOfPrimaryReconTracks; }
    UInt_t geantTpcHits()       const { return mNumberOfMcTpcHits; }
    UInt_t reconTpcHits()       const { return mNumberOfRecTpcHits; }
    
    ClassDef(StTpcEvalEventHeader,1)
};

class StTpcEvalEvent : public TObject {
private:
    Int_t             mNumMatchedTracks;
    StTpcEvalEventHeader mHeader;
    TClonesArray*     mMatchedTracks;
    
public:
    StTpcEvalEvent();
    virtual ~StTpcEvalEvent();
    void Clear(Option_t* option = "");
    void SetHeader(UInt_t geTrk, UInt_t globTrk, UInt_t gePTrk, UInt_t primTrk,  UInt_t geTHit, UInt_t reTHit);
    void addTrackPair(MatchedTrackPair&);
    
    Int_t numberOfMatchedTracks() { return mNumMatchedTracks;}
    StTpcEvalEventHeader*    header() { return &mHeader;}
    TClonesArray*  matchedTracks() const { return mMatchedTracks; }

    ClassDef(StTpcEvalEvent,1)
};
#endif
