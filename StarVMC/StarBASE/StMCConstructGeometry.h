// $Id: StMCConstructGeometry.h,v 1.1 2010/02/11 19:50:51 jwebb Exp $
//
//
// Class StMCConstructGeometry
// ------------------


#ifndef STMC_GENERATEPRIMARIES_H
#define STMC_GENERATEPRIMARIES_H

#include "GCall.h"
class StMCConstructGeometry : public GCall
{
  public:
    StMCConstructGeometry(const char *gy);
    virtual ~StMCConstructGeometry(){}    
    // methods
    int Fun();
  protected:
    // data members
  
    ClassDef(StMCConstructGeometry,0) // Extended TParticle
};

#endif //STMC_GENERATEPRIMARIES_H   
   


