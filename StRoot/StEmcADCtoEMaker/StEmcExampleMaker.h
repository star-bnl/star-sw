// $Id: StEmcExampleMaker.h,v 1.2 2003/09/10 19:47:11 perev Exp $
// $Log: StEmcExampleMaker.h,v $
// Revision 1.2  2003/09/10 19:47:11  perev
// ansi corrs
//
// Revision 1.1  2003/02/14 16:30:54  suaide
// added example maker
//
// Revision 1.12  1999/09/24 22:03:09  perev
// Add InitRun & FinishRun to template maker
//
#ifndef STAR_StEmcExampleMaker
#define STAR_StEmcExampleMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif


class StMcCalorimeterHit;

class StEmcExampleMaker : public StMaker 
{
  private: 
  protected:
    
  public: 
     
                   StEmcExampleMaker(const char *name="Teste");
   virtual         ~StEmcExampleMaker();
   virtual Int_t   Init();
   virtual Int_t   Make();
   virtual Int_t   Finish();

   ClassDef(StEmcExampleMaker,0)  
};

#endif
