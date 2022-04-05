//
// $Id: StTrackMateMaker.h,v 1.2 2009/11/10 20:57:28 fisyak Exp $
//
#ifndef STAR_St_TrackMate_Maker
#define STAR_St_TrackMate_Maker

#ifndef StMaker_H
#include "StMaker.h"
#endif
class TH2D;
class TTree;
class TBranch;
class StTrack;
class StTrackMateMaker : public StMaker {

public: 
  StTrackMateMaker(const char *name="TrackMate") : StMaker(name) {}
      ~StTrackMateMaker() {}
    Int_t  Init();
    virtual void Clear(const char* opt="");
    Int_t  Make();
    virtual const char *GetCVS() const {
      static const char cvs[]= "Tag $Name:  $ $Id: StTrackMateMaker.h,v 1.2 2009/11/10 20:57:28 fisyak Exp $ built __DATE__ __TIME__" ; 
      return cvs;
    }
 private:
    Float_t evOutput[1]; // 1 element
    TTree* trackTree;
    TBranch* trackBr;
    TBranch* eventBr;
    void Fill(StTrack* trk1, StTrack* ptrk1,StTrack* trk2, StTrack* ptrk2,Int_t maxPing);
    ClassDef(StTrackMateMaker, 1)   //StAF chain virtual base class for Makers
    
};

#endif
	

