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
class TH2D;
class TTree;
class TBranch;
class StTrack;
class StGlobalTrack;
class StPrimaryTrack;
class StTpcHit;
class StSPtrVecTrackNode;
class StTpcHitCollection;
class StTrackMateMaker : public StMaker {

public: 
  StTrackMateMaker(const char *name="TrackMate") : StMaker(name) {}
      ~StTrackMateMaker() {}
    Int_t  Init();
    virtual void Clear(const char* opt="");
    Int_t  Make();
    size_t buildRecHitTrackMap(const StSPtrVecTrackNode& nodes,map<const StTpcHit*,StGlobalTrack*>& htMap);
    void   buildHit2HitMaps(const StTpcHitCollection *tpchitcoll1, const StTpcHitCollection *tpchitcoll2,
			    map<const StTpcHit*,const StTpcHit*>        &Hit1ToHit2,map<const StTpcHit*,const StTpcHit*>        &Hit2ToHit1);
    Float_t getTpcDedx(StTrack* trk);
    Bool_t  GoodTrack(StTrack* trk);
    Bool_t  GoodMatch(StTrack* trk1, StTrack* trk2, UInt_t NPings);
    virtual const char *GetCVS() const {
      static const char cvs[]= "Tag $Name:  $ $Id: StTrackMateMaker.h,v 1.2 2009/11/10 20:57:28 fisyak Exp $ built __DATE__ __TIME__" ; 
      return cvs;
    }
 private:
    Float_t evOutput[1]; // 1 element
    TTree* trackTree;
    TBranch* trackBr;
    TBranch* eventBr;
    void Fill(StGlobalTrack* trk1, StPrimaryTrack* ptrk1,StGlobalTrack* trk2, StPrimaryTrack* ptrk2,Int_t maxPing);
    ClassDef(StTrackMateMaker, 1)   //StAF chain virtual base class for Makers
    
};

#endif
	

