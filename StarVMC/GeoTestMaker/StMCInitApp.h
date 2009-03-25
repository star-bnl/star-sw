// $Id: StMCInitApp.h,v 1.1 2009/03/25 23:15:10 perev Exp $
//
//
// Class StMCInitApp
// ------------------


#ifndef STMC_INITAPP_H
#define STMC_INITAPP_H

#include "GCall.h"
class StMCInitApp : public GCall
{
  public:
    StMCInitApp();
    virtual ~StMCInitApp(){}    
    // methods
    int Fun();
  protected:
    // data members
  
    ClassDef(StMCInitApp,0) // Extended TParticle
};

#endif //STMC_GENERATEPRIMARIES_H   
   


