//*-- Author :    Valery Fine   29/06/99  (E-mail: fine@bnl.gov)
// $Id: St_geom_Maker.h,v 1.1 1999/06/29 20:50:34 fine Exp $
// $Log: St_geom_Maker.h,v $
// Revision 1.1  1999/06/29 20:50:34  fine
// Maker to provide a St_node geom structure for others
//

#ifndef STAR_St_geom_Maker
#define STAR_St_geom_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geom_Maker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_geom_Maker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: St_geom_Maker.h,v 1.1 1999/06/29 20:50:34 fine Exp $";
 
 protected:
 public: 
                  St_geom_Maker(const char *name="geom");
   virtual       ~St_geom_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_geom_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
