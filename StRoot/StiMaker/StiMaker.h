//StiMaker.h

#ifndef StiMaker_HH
#define StiMaker_HH

#include <string>
using std::string;

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "Sti/StiObjectFactoryInterface.h"
#include "Sti/StiFactoryTypes.h"
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
class StiDynamicTrackFilter;
class StiIOBroker;
class StiToolkit;
class StiStEventFiller;


class StiMaker : public StMaker {
 public:
    
    virtual ~StiMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t InitRun(int);
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 1.44 2002/03/15 22:57:39 pruneau Exp $ built "__DATE__" "__TIME__; return cvs;}	

public:

    //Singleton access
    static StiMaker* instance();
    static void kill();

    //Gets/sets
    void setMcEventMaker(StMcEventMaker*);
    void setAssociationMaker(StAssociationMaker*);
    void setEvaluationFileName(const char*);

    StiHitContainer* hitContainer() const;
    void printStatistics() const;
    
    //Used for stepping to next action (via MainFrame class)
    void doNextTrackStep();
    void finishTrack();
    void finishEvent();
    void defineNextTrackStep(StiFindStep);

    //Temporary definition to defaut cvs/DEV mismatch
    void doNextAction() {}; //

    StiIOBroker* getIOBroker();

protected:
    StiMaker(const char* name = "StiMaker");


private:

    static StiMaker* sinstance; //!
    
    bool initialized;
    string mEvalFileName; //!
    
    StiIOBroker * ioBroker;
    StiToolkit  * toolkit;
    StiKalmanTrackFinder * tracker;
    
    //Tracker
    //StiKalmanTrackFinder* mtracker; //!
    
    //TrackFilter
    StiDynamicTrackFilter* mFilter; //!
    
    //EventFiller
    StiStEventFiller* mStEventFiller; //!

    StEvent* mevent; //!
    StMcEvent* mMcEvent; //!
    StMcEventMaker* mMcEventMaker; //!
    StAssociationMaker* mAssociationMaker; //!
    ClassDef(StiMaker, 1)

};

//inlines

inline void StiMaker::setMcEventMaker(StMcEventMaker* val)
{
    mMcEventMaker = val;
}

inline void StiMaker::setEvaluationFileName(const char* val)
{
    mEvalFileName=val;
}

inline void StiMaker::setAssociationMaker(StAssociationMaker* val)
{
	
    mAssociationMaker = val;
}


#endif
