//StXTrakTestMaker.h

#ifndef StXTrakTestMaker_HH
#define StXTrakTestMaker_HH

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include <map>

class TH1;
class TH2;
class StXTrakTestMaker : public StMaker 
{
 public:
    
    StXTrakTestMaker(const char* name = "StXTrakTest");
    virtual ~StXTrakTestMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    TH2 *GetTH2(const char *name);
    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StXTrakTestMaker.h,v 1.1 2017/05/04 01:22:15 perev Exp $ built " __DATE__ " " __TIME__; return cvs;}	

 protected:
 std::map<TString,TH1*> mMap;

 private:
    ClassDef(StXTrakTestMaker,0)
};

//inlines

#endif
