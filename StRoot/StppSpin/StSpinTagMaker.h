//////////////////////////////////////////////////////////////////////
//
// $Id: StSpinTagMaker.h,v 1.2 2003/09/10 19:47:55 perev Exp $
// $Log: StSpinTagMaker.h,v $
// Revision 1.2  2003/09/10 19:47:55  perev
// ansi corrs
//
// Revision 1.1  2001/06/22 19:41:46  balewski
// *** empty log message ***
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#ifndef StSPINTagMaker
#define StSPINTagMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StSpinTagMaker : public StMaker {

 public: 
                  StSpinTagMaker(const char *name="SpinTag");
   virtual       ~StSpinTagMaker();
   virtual Int_t Init();
   virtual Int_t  Make();

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSpinTagMaker.h,v 1.2 2003/09/10 19:47:55 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 protected:
 private:

   ClassDef(StSpinTagMaker,0)   //StAF chain virtual base class for Makers
};

#endif
