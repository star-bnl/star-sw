//StiMaker.h

#ifndef StiMaker_HH
#define StiMaker_HH

#include <string>
using std::string;

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "Sti/StiKalmanTrackFinder.h"

class StEvent;

class StiTrackContainer;
class StiEvaluableTrack;
class StiSeedFinder;
class StiTrackFinder;
class StiKalmanTrackFinder;
class StiKalmanTrackNode;
class StiKalmanTrack;
class StMcEventMaker;
class StAssociationMaker;
class StMcEvent;
class StiTrackMerger;
class StiIOBroker;
class StiToolkit;

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
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 2.0 2002/12/04 16:50:56 pruneau Exp $ built "__DATE__" "__TIME__; return cvs;}	

    static StiMaker* instance();
    static void kill();
    void setMcEventMaker(StMcEventMaker*);
    void setAssociationMaker(StAssociationMaker*);
    StiIOBroker* getIOBroker();

protected:
    StiMaker(const char* name = "StiMaker");

private:

    static StiMaker* sinstance; //!
    bool eventIsFinished;
    bool initialized;
    StiIOBroker * ioBroker;
    StiToolkit  * toolkit;
    StiTrackFinder * tracker;
    StMcEventMaker* mMcEventMaker; //!
    StAssociationMaker* mAssociationMaker; //!
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
