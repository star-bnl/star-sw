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

class StiMaker : public StMaker {
 public:
    
    StiMaker(const char* name = "StiMaker");
    virtual ~StiMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 1.2 2001/05/02 19:04:38 mmiller Exp $ built "__DATE__" "__TIME__; return cvs;}	

public:

    //Static access
    static StiMaker* instance();
    static void kill();
    
    //Used for stepping to next action (via StiControlPad)
    static void reset();
    static bool hasMore();
    static void doNextAction();
    
private:

    static StiMaker* sinstance; //!
    static bool mdone;
    static int mcounter;

    StiHitContainer* mhitstore; //!
    StiHitFactory* mhitfactory; //!
    StiHitFiller* mhitfiller; //!
    StiDisplayManager* mdisplay; //!
    StiDetectorLayerContainer* mdetector; //!
    
private:
    StEvent* mevent; //!
    ClassDef(StiMaker, 1)

};
#endif




