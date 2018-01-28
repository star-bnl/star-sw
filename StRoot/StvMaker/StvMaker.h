//StvMaker.h

#ifndef StvMaker_HH
#define StvMaker_HH

#include <string>

#include "StMaker.h"
#include "StEvent/StEnumerations.h"

class TFile;
class TTree;
class StvPullEvent;
class StEvent;
class StvHit;
class StvHitLoader;
class StvTrack;
class StvTrackContainer;
class StvTrackFinder;
class StvTrackFitter;
class StvTrackNode;
class StvTrack;
class StvToolkit;
class StvVertexFinder;
class StvEventFiller;
class StvSeedFinders;

class StvMaker : public StMaker 
{
 public:
    
    StvMaker(const char* name = "Stv");
    virtual ~StvMaker();
    virtual void  Clear(const char* opt="");
    virtual int Init();
            int InitDetectors();
            int InitPulls();
            int FillPulls();
    virtual int InitRun(int);
    virtual int Make();
    virtual int Finish();
            int CleanGlobalTracks();
            int TestGlobalTracks() const;

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StvMaker.h,v 1.9 2018/01/28 00:49:01 perev Exp $ built " __DATE__ " " __TIME__; return cvs;}	



 protected:
    int GeoTest();
private:
  char mBeg[1]; 
    int                  mEtaRegion;	//bit0:TpcLike medium eta,bit1:Forward eta
    StvHitLoader        *mHitLoader[2];
    StvSeedFinders      *mSeedFinders[2];
    StvEventFiller      *mEventFiller[2];
    StvTrackFinder      *mTrackFinder[2];
    StvTrackFinder      *mCurTrackFinder;
    StvTrackFitter      *mCurTrackFitter;
    StvTrackFitter      *mTrackFitter[2];
    StvVertexFinder     *mVertexFinder[2];
    TFile               *mPullFile;
    StvPullEvent        *mPullEvent;
    TTree               *mPullTTree;
    int                  mMaxTimes;	//max times hit reused
    int                  mFETracks;	//max number of track requested if fit hit errs
    int                  mToTracks;	//total tracks created, if > mFETracks, stop
  char mEnd[1];
    ClassDef(StvMaker,0)
};

#endif
