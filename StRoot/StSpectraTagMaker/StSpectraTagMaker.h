#ifndef STAR_StSpectraTagMaker
#define STAR_StSpectraTagMaker

//
// StSpectraTagMaker 
//
#ifndef StMaker_H
#include "StMaker.h"
#endif
class StSpectraTagMaker : public StMaker {
 private:
 
 protected:
 public: 
                  StSpectraTagMaker(const char *name="SpectraTag");
   virtual       ~StSpectraTagMaker();
   virtual Int_t Init();
   virtual Int_t  Make();

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSpectraTagMaker.h,v 1.2 2003/09/10 19:47:32 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StSpectraTagMaker,0)   //StAF chain virtual base class for Makers
};

#endif
