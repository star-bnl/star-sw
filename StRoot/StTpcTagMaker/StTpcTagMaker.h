// $Log: StTpcTagMaker.h,v $
// Revision 1.1  2000/05/24 00:07:02  sakrejda
// Maker to fill TPC reconstruction quality flags created
//
// Revision 1.12  1999/09/24 22:03:09  perev
// Add InitRun & FinishRun to template maker
//
// Revision 1.11  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.10  1999/07/10 22:59:17  fine
// Some comments have been introduced to show html docs
//
// Revision 1.9  1999/03/11 03:33:16  perev
// new schema
//
// Revision 1.8  1999/03/10 15:02:07  fine
// HTML link to STAR problem report form has been introduced
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
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
// static Char_t  m_VersionCVS = "$Id: StTpcTagMaker.h,v 1.1 2000/05/24 00:07:02 sakrejda Exp $";
 
 protected:
 public: 
                  StTpcTagMaker(const char *name="TLA");
   virtual       ~StTpcTagMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcTagMaker.h,v 1.1 2000/05/24 00:07:02 sakrejda Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StTpcTagMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
