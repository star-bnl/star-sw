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
  {static const char cvs[]="Tag $Name:  $ $Id: StSpectraTagMaker.h,v 1.1 2000/11/22 22:05:11 hardtke Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StSpectraTagMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
