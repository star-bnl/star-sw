#ifndef STAR_StPCollTagMaker
#define STAR_StPCollTagMaker

//
// StPCollTagMaker 
//
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;
class StPCollTagMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StPCollTagMaker.h,v 1.1 2000/06/13 17:26:44 khodinov Exp $";
 
 protected:
 public: 
                  StPCollTagMaker(const char *name="PCollTag");
   virtual       ~StPCollTagMaker();
   virtual Int_t Init();
   virtual Int_t  Make();

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPCollTagMaker.h,v 1.1 2000/06/13 17:26:44 khodinov Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StPCollTagMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
