//StiMaker.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiMaker_HH
#define StiMaker_HH

#include "StMaker.h"
#include "Sti/StiFactoryTypedefs.h"
#include "StiGui/StiGuiTypedefs.h"

class StEvent;
class StiHitContainer;
class StiHitFiller;
class StiDisplayManager;
class StiDetectorContainer;
class StiTrackContainer;
class StiDrawableHits;
class StiEvaluableTrackSeedFinder;

class StiMaker : public StMaker {
 public:
    
    virtual ~StiMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 1.9 2001/08/01 19:10:33 mmiller Exp $ built "__DATE__" "__TIME__; return cvs;}	

public:

    //Singleton access
    static StiMaker* instance();
    static void kill();

    //Gets/sets
    void setMaterialBuildPath(char* val);
    void setDetectorBuildPath(char* val);

    void printStatistics() const;
    
    //Used for stepping to next action (via StiControlPad)
    static void reset();
    static bool hasMore();
    static void doNextAction();
    
protected:
    StiMaker(const char* name = "StiMaker");

private:

    StiHitContainer* mhitstore; //!
    StiHitFactory* mhitfactory; //!
    StiEvaluableTrackFactory* mtrackfactory; //!
    StiHitFiller* mhitfiller; //!
    StiDisplayManager* mdisplay; //!
    StiDetectorContainer* mdetector; //!
    StiTrackContainer* mtrackstore; //!
    StiDrawableHits* mdrawablehits; //!
    StiEvaluableTrackSeedFinder* mtrackseedfinder; //!
    StiRDEvaluableTrackFactory* md_trackfactory; //!
    detector_factory* mdetectorfactory; //!
    data_node_factory* mdatanodefactory; //!

    char* mmaterialbuildpath; //!
    char* mdetectorbuildpath; //!
    
    static StiMaker* sinstance; //!
    static bool mdone;
    static int mcounter;

private:
    StEvent* mevent; //!
    ClassDef(StiMaker, 1)

};
#endif




