// $Id: St_geant_Maker.h,v 1.2 1998/10/31 00:26:17 fisyak Exp $
// $Log: St_geant_Maker.h,v $
// Revision 1.2  1998/10/31 00:26:17  fisyak
// Makers take care about branches
//
// Revision 1.1  1998/10/12 14:34:18  fisyak
// new geant Maker
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
#ifndef STAR_St_geant_Maker
#define STAR_St_geant_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geant_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;
class St_geant_Maker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: St_geant_Maker.h,v 1.2 1998/10/31 00:26:17 fisyak Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters
 
 protected:
 public: 
                  St_geant_Maker(const char *name="geant", const char *title="event/geant/Event");
   virtual       ~St_geant_Maker();
   virtual void   Clear(Option_t *option="");
   virtual Int_t   Finish();
   virtual Int_t   Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_geant_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
