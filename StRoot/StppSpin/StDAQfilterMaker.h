// $Id: StDAQfilterMaker.h,v 1.1 2001/04/12 15:19:08 balewski Exp $
// $Log: StDAQfilterMaker.h,v $
// Revision 1.1  2001/04/12 15:19:08  balewski
// *** empty log message ***
//
#ifndef STAR_StDAQfilterMaker
#define STAR_StDAQfilterMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StDAQfilterMaker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;

class  ppDbConfColl_st;
class St_ppDbAvrColl;

class StDAQfilterMaker : public StMaker {
 private:

  void readDB();
 protected:
 public: 
  StDAQfilterMaker(const char *name="ppDAQfilter0");
   virtual       ~StDAQfilterMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
// static Char_t  m_VersionCVS = "$Id: StDAQfilterMaker.h,v 1.1 2001/04/12 15:19:08 balewski Exp $";

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StDAQfilterMaker.h,v 1.1 2001/04/12 15:19:08 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StDAQfilterMaker, 0)   //StAF chain virtual base class for Makers
};

#endif



