//StiMaker.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiMaker_HH
#define StiMaker_HH

#include "StMaker.h"
#include "Sti/StiFactoryTypedefs.h"
#include "Sti/StiTrackNode.h"
#include "StiGui/StiGuiTypedefs.h"

class StEvent;
class StiHitContainer;
class StiHitFiller;
class StiDisplayManager;
class StiDetectorContainer;
class StiTrackContainer;
class StiDrawableHits;
class StiEvaluableTrackSeedFinder;
class StiTrackSeedFinder;
class Sti2HitComboFilter;

class StiMaker : public StMaker {
 public:
    typedef StiObjectFactory<StiKalmanTrack> StiKalmanTrackFactory;
    typedef StiObjectFactory<StiTrackNode> StiTrackNodeFactory;
    
    virtual ~StiMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 1.12 2001/08/15 17:54:01 mmiller Exp $ built "__DATE__" "__TIME__; return cvs;}	

public:

    //Singleton access
    static StiMaker* instance();
    static void kill();

    //Gets/sets
    void setMaterialBuildPath(char* val);
    void setDetectorBuildPath(char* val);

    void printStatistics() const;
    
    //Used for stepping to next action (via StiControlPad)
    void doNextAction();
    
protected:
    StiMaker(const char* name = "StiMaker");
    void initSeedFinderForStart();

private:

    //Containers
    StiHitContainer* mhitstore; //!
    StiDetectorContainer* mdetector; //!
    StiTrackContainer* mtrackstore; //!

    //Factories
    StiHitFactory* mhitfactory; //!
    StiEvaluableTrackFactory* mtrackfactory; //!
    StiKalmanTrackNodeFactory* mkalmantracknodefactory; //!
    detector_factory* mdetectorfactory; //!
    data_node_factory* mdatanodefactory; //!
    StiTrackNodeFactory* mtracknodefactory; //!
    StiKalmanTrackFactory* mkalmantrackfactory; //!

    //Display
    StiDisplayManager* mdisplay; //!
    StiDrawableHits* mdrawablehits; //!
    
    //Utilites
    StiHitFiller* mhitfiller; //!
    StiEvaluableTrackSeedFinder* mtrackseedfinder; //!
    StiTrackSeedFinder* mkalmanseedfinder; //!
    Sti2HitComboFilter* mhitcombofilter; //!
    
    char* mmaterialbuildpath; //!
    char* mdetectorbuildpath; //!
    
    static StiMaker* sinstance; //!

private:
    StEvent* mevent; //!
    ClassDef(StiMaker, 1)

};
#endif




