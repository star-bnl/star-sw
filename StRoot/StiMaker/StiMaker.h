//StiMaker.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiMaker_HH
#define StiMaker_HH

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "Sti/StiHit.h" //For StiHitFactory
#include "Sti/StiCompositeTreeNode.h" //For data_node_factory
#include "Sti/StiTrackNode.h"
#include "Sti/StiKalmanTrackNode.h"

#include "StiGui/StiRootDrawableStiEvaluableTrack.h" //For EvaluableTrackFactory
#include "StiGui/StiRootDrawableDetector.h"

class StEvent;
class StiHitContainer;
class StiHitFiller;
class StiDisplayManager;
class StiDetectorContainer;
class StiTrackContainer;
class StiEvaluableTrackSeedFinder;
class StiTrackSeedFinder;
class StiTrackFinder;
class StiKalmanTrackFinder;
class StiCompositeSeedFinder;
class StMcEventMaker;
class StAssociationMaker;

class StiMaker : public StMaker {
 public:
    typedef StiObjectFactory<StiKalmanTrack> StiKalmanTrackFactory;
    
    virtual ~StiMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 1.21 2001/09/06 15:15:36 mmiller Exp $ built "__DATE__" "__TIME__; return cvs;}	

public:

    //Singleton access
    static StiMaker* instance();
    static void kill();

    //Gets/sets
    void setMaterialBuildPath(char* val);
    void setDetectorBuildPath(char* val);
    void setMcEventMaker(StMcEventMaker*);
    void setAssociationMaker(StAssociationMaker*);

    void printStatistics() const;
    
    //Used for stepping to next action (via StiControlPad)
    void doNextAction();
    
protected:
    StiMaker(const char* name = "StiMaker");

private:

    //Containers
    StiHitContainer* mhitstore; //!
    StiDetectorContainer* mdetector; //!
    StiTrackContainer* mtrackstore; //!

    //Factories
    StiHitFactory* mhitfactory; //!
    StiEvaluableTrackFactory* mtrackfactory; //!
    StiKalmanTrackNodeFactory* mktracknodefactory; //!
    detector_factory* mdetectorfactory; //!
    data_node_factory* mdatanodefactory; //!
    StiKalmanTrackFactory* mkalmantrackfactory; //!

    //Display
    StiDisplayManager* mdisplay; //!
    
    //Utilites
    StiHitFiller* mhitfiller; //!

    //SeedFinders
    StiEvaluableTrackSeedFinder* mEvaluableSeedFinder; //!
    StiTrackSeedFinder* mKalmanSeedFinder; //!
    StiCompositeSeedFinder* mcompseedfinder; //!

    //Tracker
    StiKalmanTrackFinder* mtracker; //!
    
    static StiMaker* sinstance; //!

private:
    StEvent* mevent; //!
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
