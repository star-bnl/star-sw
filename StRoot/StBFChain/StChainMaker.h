// $Id: StChainMaker.h,v 1.1 1999/11/18 23:34:17 fisyak Exp $
// $Log: StChainMaker.h,v $
// Revision 1.1  1999/11/18 23:34:17  fisyak
// Add l3 chain with new clustering, add ChainMaker to remove ugly print out
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
#ifndef STAR_St_TLA_Maker
#define STAR_StChainMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChainMaker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;
class StChainMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StChainMaker.h,v 1.1 1999/11/18 23:34:17 fisyak Exp $";
 
 protected:
 public: 
                  StChainMaker(const char *name="miniChain");
   virtual       ~StChainMaker();

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StChainMaker.h,v 1.1 1999/11/18 23:34:17 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StChainMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
