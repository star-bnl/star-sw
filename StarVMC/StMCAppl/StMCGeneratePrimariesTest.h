// $Id: StMCGeneratePrimariesTest.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
//
// Class StMCGeneratePrimariesTest
// ------------------


#ifndef STMC_GENERATEPRIMARIESTEST_H
#define STMC_GENERATEPRIMARIESTEST_H

#include "GCall.h"

class StMCGeneratePrimariesTest : public GCall
{
  public:
    StMCGeneratePrimariesTest(int ntrk=10,int pdg=0);
    virtual ~StMCGeneratePrimariesTest(){}    
    // methods
    virtual int  Fun();
    virtual void Print(const Option_t* opt=0) const;
            void SetNTrk(int ntrk){fNTrk=ntrk;}
            void SetPDG (int pdg ){fPDG =pdg ;}

  private:
    // data members
    int fNTrk;
    int fPDG;
    ClassDef(StMCGeneratePrimariesTest,0) // Extended TParticle
};

#endif //STMC_GENERATEPRIMARIESTEST_H   
   

