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

class StiMaker : public StMaker {
 public:
    
    StiMaker(const char* name = "StiMaker");
    virtual ~StiMaker();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 1.1 2001/04/18 22:27:37 mmiller Exp $ built "__DATE__" "__TIME__; return cvs;}	

private:
    StiHitContainer* mhitstore; //!
    StiHitFactory* mhitfactory; //!
    StiHitFiller* mhitfiller; //!
    
private:
    StEvent* mevent; //!
    ClassDef(StiMaker, 1)

};
#endif




