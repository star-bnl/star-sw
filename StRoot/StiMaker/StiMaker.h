//StiMaker.h

#ifndef StiMaker_HH
#define StiMaker_HH

#include <string>
using std::string;

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "Sti/StiKalmanTrackFinder.h"

class TH1D;
class StEvent;

class StiTrackContainer;
class StiEvaluableTrack;
class StiTrackSeedFinder;
class StiTrackFinder;
class StiKalmanTrackFinder;
class StiKalmanTrackNode;
class StiKalmanTrack;
class StMcEventMaker;
class StAssociationMaker;
class StMcEvent;
class StiTrackMerger;
class StiToolkit;
class StiTrackingPlots;

class StiMaker : public StMaker 
{
 public:
    
    virtual ~StiMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t InitDetectors();
    virtual Int_t InitRun(int);
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 2.4 2003/03/13 16:32:33 andrewar Exp $ built "__DATE__" "__TIME__; return cvs;}	

    static StiMaker* instance();
    static void kill();
    void setMcEventMaker(StMcEventMaker*);
    void setAssociationMaker(StAssociationMaker*);

    TH1D * dyHist;
    TH1D * dzHist;
    TH1D * phiHist;
    TH1D * pseudoRapHist;

protected:
    StiMaker(const char* name = "StiMaker");

private:

    static StiMaker* sinstance; //!
    bool eventIsFinished;
    bool initialized;
    StiToolkit  *    _toolkit;
    StiTrackFinder * tracker;
    StMcEventMaker* mMcEventMaker; //!
    StAssociationMaker* mAssociationMaker; //!
    StiTrackingPlots* plotter;
    ClassDef(StiMaker, 1)
};

//inlines

inline void StiMaker::setMcEventMaker(StMcEventMaker* val)
{
    mMcEventMaker = val;
}

inline void StiMaker::setAssociationMaker(StAssociationMaker* val)
{
	
    mAssociationMaker = val;
}


#endif
