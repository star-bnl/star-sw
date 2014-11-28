// $Id: StMCConstructGeometry.h,v 1.2 2010/01/27 23:02:57 perev Exp $
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
  private:
  void SetTop(const char *topName);

  protected:
    // data members
  
    ClassDef(StMCConstructGeometry,0) // Extended TParticle
};

#endif //STMC_GENERATEPRIMARIES_H   
   


