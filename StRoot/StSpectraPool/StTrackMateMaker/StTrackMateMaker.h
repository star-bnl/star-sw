//
// $Id: StTrackMateMaker.h,v 1.1 2004/09/13 22:04:53 calderon Exp $
//
#ifndef STAR_St_TrackMate_Maker
#define STAR_St_TrackMate_Maker

#ifndef StMaker_H
#include "StMaker.h"
#endif
class TH2D;
class TTree;

class StTrackMateMaker : public StMaker {

public: 
    StTrackMateMaker(const char *name="TrackMate");
    ~StTrackMateMaker();
    Int_t  Init();
    virtual void Clear(const char* opt="");
    Int_t  Make();
    Int_t  Finish();
    void  SetFileIndex(const char* val) { mFileIndex = val; }
    void  SetOutDir(const char* val) { mOutDir = val; }
    virtual const char *GetCVS() const
	{
	    static const char cvs[]= "Tag $Name:  $ $Id: StTrackMateMaker.h,v 1.1 2004/09/13 22:04:53 calderon Exp $ built __DATE__ __TIME__" ; return cvs;}
private:
    TH2D* mPtDiff;
    Float_t output[9]; //9 elements
    TTree* trackTree;
    const char* mFileIndex;
    const char* mOutDir;
    ClassDef(StTrackMateMaker, 1)   //StAF chain virtual base class for Makers
    
};

#endif
	

