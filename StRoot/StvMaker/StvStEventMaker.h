//StvStEventMaker.h

#ifndef StvStEventMaker_HH
#define StvStEventMaker_HH


#include "StMaker.h"


class StvStEventMaker : public StMaker 
{
 public:
    
    StvStEventMaker(const char* name = "StvStEvent");
    virtual Int_t Make();
    static StvStEventMaker* Inst();
    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StvStEventMaker.h,v 1.1 2013/03/08 19:18:57 perev Exp $ built "__DATE__" "__TIME__; return cvs;}	
    ClassDef(StvStEventMaker,0)
};

#endif
