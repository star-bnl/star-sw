//StiMaker.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiMaker_HH
#define StiMaker_HH

#include "StMaker.h"

class StEvent;
class StiHitContainer;
class StiHitFactory;
class StiHitFiller;
class StiDisplayManager;
class StiDetectorLayerContainer;
class StiTrackContainer;

class StiMaker : public StMaker {
 public:
    
    virtual ~StiMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 1.3 2001/05/17 14:22:05 mmiller Exp $ built "__DATE__" "__TIME__; return cvs;}	

public:

    //Singleton access
    static StiMaker* instance();
    static void kill();
    
    //Used for stepping to next action (via StiControlPad)
    static void reset();
    static bool hasMore();
    static void doNextAction();
    
protected:
    StiMaker(const char* name = "StiMaker");

private:

    static StiMaker* sinstance; //!
    static bool mdone;
    static int mcounter;

    StiHitContainer* mhitstore; //!
    StiHitFactory* mhitfactory; //!
    StiHitFiller* mhitfiller; //!
    StiDisplayManager* mdisplay; //!
    StiDetectorLayerContainer* mdetector; //!
    StiTrackContainer* mtrackstore; //!
    
private:
    StEvent* mevent; //!
    ClassDef(StiMaker, 1)

};
#endif




