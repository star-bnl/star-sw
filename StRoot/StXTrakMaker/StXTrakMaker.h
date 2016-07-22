//StXTrakMaker.h

#ifndef StXTrakMaker_HH
#define StXTrakMaker_HH

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
class StvELossTrak;
class MyMag;
class StXTrak;
class TGeoSwim;

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
    {static const char cvs[]="Tag $Name:  $ $Id: StXTrakMaker.h,v 1.3 2016/07/22 19:00:20 perev Exp $ built " __DATE__ " " __TIME__; return cvs;}	

 protected:
 StXTrak *mSwim;		//Swimmer

 private:
    ClassDef(StXTrakMaker,0)
};

//inlines

#endif
