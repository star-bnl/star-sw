//StSvtSelfMaker.h

#ifndef StSvtSelfMaker_HH
#define StSvtSelfMaker_HH


#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "StThreeVectorF.hh"
#include "TObjArray.h"
#include "TList.h"

class TFile;
class TTree;
class StEvent;
class StiHit;
class StiToolkit;
class TExMap;
class StSelfEvent;
class StSelfHit;
class StSvtGeometry; 
class StSvtSelfMaker : public StMaker 
{
 public:
    
    StSvtSelfMaker(const char* name = "SvtSelfMaker");
    virtual ~StSvtSelfMaker();
    virtual void  Clear(const char* opt="");
    virtual int Init();
    virtual int InitRun(int);
    virtual int Make();
    virtual int Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSvtSelfMaker.h,v 1.2 2008/10/29 18:55:18 perev Exp $ built "__DATE__" "__TIME__; return cvs;}	
private:
    int SelectTracks();
    int MapHits();  
    int MakeSelfTracks();  
    int MakeVertex();  
    int UpdateSelfTracks();  
    int FillEvent();  
    int TestVtx();


private:
StiToolkit  	*mToolkit;
StEvent		*mEvent;    
TExMap        	*mStStiMap;
TObjArray       *mStTrackList;
TObjArray       *mSelfTrackList;
StSelfEvent     *mSelfEvent;
StSvtGeometry   *mSvtGeometry;
double	         mVtx[3];
double	         mEtx[6];
double	         mChi2;
double	         mVtxOld[3];
double	         mEtxOld[6];

TString         mTreeFile;
TFile          *mTFile;
TTree          *mTTree;

ClassDef(StSvtSelfMaker,0)
};
#endif
