// $Id: St_ems_Maker.h,v 1.1 1998/11/30 21:18:31 fisyak Exp $
// $Log: St_ems_Maker.h,v $
// Revision 1.1  1998/11/30 21:18:31  fisyak
// ems raw data Maker
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
#ifndef STAR_St_ems_Maker
#define STAR_St_ems_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ems_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_ems_control;
class St_calb_calg;
class St_ems_Maker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: St_ems_Maker.h,v 1.1 1998/11/30 21:18:31 fisyak Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
   St_ems_control *m_ems_control;//!
   St_calb_calg   *m_calb_calg;  //!
 protected:
 public: 
                  St_ems_Maker(const char *name="emc_raw", const char *title="event/raw_data/emc");
   virtual       ~St_ems_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_ems_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
