#ifndef __StvTpcActive_h_
#define __StvTpcActive_h_
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcActive: public StActorFunctor
{
public:
    StvTpcActive(const char *name="");
virtual  ~StvTpcActive(){}
virtual int operator()(const double xyz[3]=0);
int VoluId();
protected:
char mBeg[1];
int mSector;
int mIsDet;
int mGPad;		//Padrow number including all non physical
int mTPad;		//Padrow number including only physical
int mPrompt;		//It is a prompt hit , 
char mEnd[1];

ClassDef(StvTpcActive,0)
};

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcSelector: public StvTpcActive
{
public:
    StvTpcSelector(const char *name);
virtual ~StvTpcSelector(){}
virtual int operator()(const double xyz[3]);
protected:
int mInOut;

ClassDef(StvTpcSelector,0)
};


// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvTpcEdit: public StvTpcActive
{
public:
    StvTpcEdit();
   ~StvTpcEdit(){}
int operator()(const double xyz[3]=0);
protected:

ClassDef(StvTpcEdit,0)
};
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __

class StvTpcPrompt: public StvTpcActive
{
public:
    StvTpcPrompt(){;}
   ~StvTpcPrompt(){;}
int operator()(const double xyz[3]=0);
protected:

ClassDef(StvTpcPrompt,0)
};
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __

class StvTpcHitActor: public StvTpcActive
{
public:
    StvTpcHitActor(){;}
   ~StvTpcHitActor(){;}
int operator()(const double xyz[3]=0);
protected:

ClassDef(StvTpcHitActor,0)
};
#endif
