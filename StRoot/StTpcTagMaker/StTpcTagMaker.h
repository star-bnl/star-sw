// $Id: StTpcTagMaker.h,v 1.3 2000/05/24 00:25:50 sakrejda Exp $
// $Log: StTpcTagMaker.h,v $
// Revision 1.3  2000/05/24 00:25:50  sakrejda
// Body of the Maker and the header cleaned up, comments added.
//
// Revision 1.2  2000/05/24 00:20:39  sakrejda
// $Id added at the top
//
// Revision 1.1  2000/05/24 00:07:02  sakrejda
// Maker to fill TPC reconstruction quality flags created
//

#ifndef STAR_StTpcTagMaker
#define STAR_StTpcTagMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcTagMaker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;
class StTpcTagMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StTpcTagMaker.h,v 1.3 2000/05/24 00:25:50 sakrejda Exp $";
 
 protected:
 public: 
                  StTpcTagMaker(const char *name="TpcTag");
   virtual       ~StTpcTagMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcTagMaker.h,v 1.3 2000/05/24 00:25:50 sakrejda Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StTpcTagMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
