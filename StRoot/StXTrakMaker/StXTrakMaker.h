//StXTrakMaker.h

#ifndef StXTrakMaker_HH
#define StXTrakMaker_HH

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
class StvELossTrak;
class MyMag;

class StXTrakMaker : public StMaker 
{
 public:
    
    StXTrakMaker(const char* name = "StXTrak");
    virtual ~StXTrakMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StXTrakMaker.h,v 1.1 2016/05/20 18:40:41 perev Exp $ built " __DATE__ " " __TIME__; return cvs;}	

 protected:
 StvELossTrak* mELoss;		//ELoss calculator
 MyMag*        mMyMag;		//Mag field calculator

 private:
    ClassDef(StXTrakMaker,0)
};

//inlines

#endif
