//StiMaker.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiMaker_HH
#define StiMaker_HH

#include <string>
using std::string;

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "Sti/StiCompositeTreeNode.h" //For typedefs
#include "Sti/StiObjectFactoryInterface.h"
#include "Sti/StiFactoryTypes.h"

class StEvent;
class StiHitContainer;
class StiHitFiller;
class StiDisplayManager;
class StiDetectorContainer;
class StiTrackContainer;
class StiEvaluableTrack;
class StiSeedFinder;
class StiTrackFinder;
class StiKalmanTrackFinder;
class StiKalmanTrackNode;
class StiKalmanTrack;
class StMcEventMaker;
class StAssociationMaker;

class StiMaker : public StMaker {
 public:
    enum SeedFinderType {kUndefined=0, kComposite=1, kEvaluable=3};
    
    virtual ~StiMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 1.29 2001/10/11 13:18:04 mmiller Exp $ built "__DATE__" "__TIME__; return cvs;}	

public:

    //Singleton access
    static StiMaker* instance();
    static void kill();

    //Gets/sets
    void setMcEventMaker(StMcEventMaker*);
    void setAssociationMaker(StAssociationMaker*);
    void setSimulation(bool);
    void setGui(bool); // true->gui version.  Defaults to true
    void setDoFit(bool);// true->doFit, false->doFind
    void setSeedFinderType(SeedFinderType);
    void setEvaluationFileName(const char*);

    void printStatistics() const;
    
    //Used for stepping to next action (via StiControlPad)
    void doNextAction();
    void finishEvent();
    
protected:
    StiMaker(const char* name = "StiMaker");

private:

    //Names
    string mEvalFileName; //!
    
    //flags
    bool mSimulation;//! true->m.c.
    bool mUseGui; //!
    bool mDoTrackFit; //! ture->doFit, false->doFind
    SeedFinderType mSeedFinderType;
    
    //Containers
    StiHitContainer* mhitstore; //!
    StiDetectorContainer* mdetector; //!
    StiTrackContainer* mtrackstore; //!

    //Factories
    StiObjectFactoryInterface<StiHit>* mhitfactory; //!
    StiObjectFactoryInterface<StiKalmanTrack>* mtrackfactory; //!
    StiObjectFactoryInterface<StiKalmanTrackNode>* mktracknodefactory; //!
    StiObjectFactoryInterface<StiDetector>* mdetectorfactory; //!
    StiObjectFactoryInterface<StiDetectorNode>* mdatanodefactory; //!

    //Display
    StiDisplayManager* mdisplay; //!
    
    //Utilites
    StiHitFiller* mhitfiller; //!

    //SeedFinder(s)
    StiSeedFinder* mSeedFinder; //!

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
inline void StiMaker::setEvaluationFileName(const char* val)
{
    mEvalFileName=val;
}

inline void StiMaker::setAssociationMaker(StAssociationMaker* val)
{
    mAssociationMaker = val;
}

inline void StiMaker::setSimulation(bool val)
{
    mSimulation=val;
}

inline void StiMaker::setGui(bool val)
{
    mUseGui=val;
}

inline void StiMaker::setDoFit(bool val)
{
    mDoTrackFit=val;
}

#endif
