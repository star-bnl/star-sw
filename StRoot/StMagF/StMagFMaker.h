// $Id: StMagFMaker.h,v 1.2 2000/01/07 00:42:33 fisyak Exp $
// $Log: StMagFMaker.h,v $
// Revision 1.2  2000/01/07 00:42:33  fisyak
// merge Make with Init
//
// Revision 1.1  2000/01/04 20:44:41  fisyak
// Add StMagFMaker
//
#ifndef STAR_StMagFMaker
#define STAR_StMagFMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMagFMaker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class StMagF;
class St_MagFactor;
class StMagFMaker : public StMaker {
 private:
  St_MagFactor *fMagFactor; //!
  StMagF       *fMagF; //!
  Float_t       fScale; //!
 protected:
 public: 
                  StMagFMaker(const char *name="MagField");
   virtual       ~StMagFMaker();
   virtual Int_t  Init();
   virtual Int_t  Make(){return Init();}

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StMagFMaker.h,v 1.2 2000/01/07 00:42:33 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StMagFMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
