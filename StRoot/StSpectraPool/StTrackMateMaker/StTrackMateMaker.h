//
// $Id: StTrackMateMaker.h,v 1.2 2009/11/10 20:57:28 fisyak Exp $
//
#ifndef STAR_St_TrackMate_Maker
#define STAR_St_TrackMate_Maker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <map>
#include <vector>
#include "StThreeVectorF.hh"
class TH2D;
class TTree;
class TBranch;
class StTrack;
class StGlobalTrack;
class StPrimaryTrack;
class StTpcHit;
class StSPtrVecTrackNode;
class StTpcHitCollection;
typedef multimap<const StTpcHit*,StGlobalTrack*> Hit2TrackMap;
typedef multimap<StGlobalTrack*,StGlobalTrack*>  Track2TrackMap;
typedef map<const StTpcHit*,const StTpcHit*>     Hit2HitMap;
typedef pair<const StTpcHit*,StGlobalTrack*>     HitTrackPair;
typedef pair<StGlobalTrack*,StGlobalTrack*>      TrackTrackPair;
typedef Hit2TrackMap::iterator                   Hit2TrackIter;
typedef pair<Hit2TrackIter, Hit2TrackIter>       Hit2Track2Iter;
typedef Track2TrackMap::iterator                 Track2TrackIter;
typedef pair<Track2TrackIter, Track2TrackIter>   Track2Track2Iter;
class StTrack;
#include "TrackMatch.h"
struct StTrackPing {
StTrackPing(StTrack *t=0, UInt_t ping=0) : mTrack(t), mNPings(ping) {};
  StTrack*  mTrack;
  unsigned int    mNPings;
};

bool compStTrackPing(const StTrackPing& rhs,const StTrackPing& lhs);
class StTrackMateMaker : public StMaker {
public: 
  StTrackMateMaker(const char *name="TrackMate") : StMaker(name) {}
      ~StTrackMateMaker() {}
    Int_t  Init();
    virtual void Clear(const char* opt="");
    Int_t  Make();
    size_t buildRecHitTrackMap(const StSPtrVecTrackNode& nodes,Hit2TrackMap& htMap);
    void   buildHit2HitMaps(const StTpcHitCollection *tpchitcoll1, const StTpcHitCollection *tpchitcoll2,
			    Hit2HitMap        &Hit1ToHit2,Hit2HitMap        &Hit2ToHit1);
    void   buildTrack2TrackMap(const StSPtrVecTrackNode &trackNodes1, 
			       const StSPtrVecTrackNode &trackNodes2, 
			       Hit2HitMap &Hit1ToHit2, 
			       Hit2TrackMap &hitTrackMap2,
			       Track2TrackMap &Track1ToTrack2);
    void    checkConsistency(Track2TrackMap &Track1ToTrack2, Track2TrackMap &Track2ToTrack1);
    Bool_t  GoodTrack(StTrack* trk);
    Bool_t  GoodMatch(StTrack* trk1, StTrack* trk2, UInt_t NPings);
    TrackParameters TrackParametersFill(StGlobalTrack *gTrack = 0);
    virtual const char *GetCVS() const {
      static const char cvs[]= "Tag $Name:  $ $Id: StTrackMateMaker.h,v 1.2 2009/11/10 20:57:28 fisyak Exp $ built __DATE__ __TIME__" ; 
      return cvs;
    }
 private:
    Float_t evOutput[1]; // 1 element
    TTree* trackTree;
    TBranch* trackBr;
    TrackMatch *fTrackMatch;
    //    TBranch* eventBr;
    void FillMatch(StGlobalTrack* trk1, StGlobalTrack* trk2 = 0);
    ClassDef(StTrackMateMaker, 1)   //StAF chain virtual base class for Makers
    
};

#endif
	

