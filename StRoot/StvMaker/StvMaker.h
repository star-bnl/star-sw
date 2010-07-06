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
class StvTrack;
class StvTrackContainer;
class StvTrackFinder;
class StvTrackFitter;
class StvTrackNode;
class StvTrack;
class StvToolkit;
class StvVertexFinder;
class StvEventFiller;

class StvMaker : public StMaker 
{
 public:
    
    StvMaker(const char* name = "Stv");
    virtual ~StvMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
            Int_t InitDetectors();
            Int_t InitPulls();
            Int_t FillPulls();
    virtual Int_t InitRun(int);
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StvMaker.h,v 1.1 2010/07/06 20:27:53 perev Exp $ built "__DATE__" "__TIME__; return cvs;}	



 protected:
    
private:
  char mBeg[1];
    StvEventFiller      *mEventFiller;
    StvVertexFinder     *mVertexFinder;
    TFile               *mPullFile;
    StvPullEvent        *mPullEvent;
    TTree               *mPullTTree;
  char mEnd[1];
    ClassDef(StvMaker,0)
};

#endif
