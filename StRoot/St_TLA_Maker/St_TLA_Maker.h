// $Id: St_TLA_Maker.h,v 1.9 1999/03/11 03:33:16 perev Exp $
// $Log: St_TLA_Maker.h,v $
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
#define STAR_St_TLA_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TLA_Maker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;
class St_TLA_Maker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: St_TLA_Maker.h,v 1.9 1999/03/11 03:33:16 perev Exp $";
 
 protected:
 public: 
                  St_TLA_Maker(const char *name="TLA");
   virtual       ~St_TLA_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_TLA_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
