// $Id: St_mwc_Maker.h,v 1.3 1999/02/19 18:22:58 druss Exp $
// $Log: St_mwc_Maker.h,v $
// Revision 1.3  1999/02/19 18:22:58  druss
// init routine now uses parameter files from StRoot/params
// included a few histograms
//
// Revision 1.2  1999/02/08 16:51:58  fisyak
// Fix to parameters
//
// Revision 1.1  1999/01/14 19:11:01  druss
// root Maker definitions/header for mwc
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
#ifndef STAR_St_mwc_Maker
#define STAR_St_mwc_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_mwc_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_mwc_geo;
class St_mwc_cal;
class St_mwc_mpar;
class TH1F;
class TH2F;

class St_mwc_Maker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: St_mwc_Maker.h,v 1.3 1999/02/19 18:22:58 druss Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters
   St_mwc_geo  *m_geom; //!
   St_mwc_cal  *m_cal;  //!
   St_mwc_mpar *m_mpar; //!
 
 protected:
   TH2F   *m_xy;  //!
   TH2F   *m_pxy; //!
   TH1F   *m_pz;  //!
  
 public: 
                  St_mwc_Maker(const char *name="mwc", const char *title="mwc");
   virtual       ~St_mwc_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_mwc_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
