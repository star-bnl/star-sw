#ifndef STAR_St_rl0_Maker
#define STAR_St_rl0_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_rl0_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_rl0_ctrl;
class St_rl0_ctbcal;
class St_rl0_mwccal;

class St_mwc_geo;

class St_ctg_geo;


class St_rl0_Maker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: St_rl0_Maker.h,v 1.3 1999/07/15 13:58:19 perev Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters

   St_rl0_ctrl   *m_rl0_ctrl;    //!
   St_rl0_ctbcal *m_rl0_ctbcal;  //!
   St_rl0_mwccal *m_rl0_mwccal;  //!
   St_mwc_geo    *m_geom;        //!
   St_ctg_geo    *m_ctb;         //!

 protected:
 public: 
                 St_rl0_Maker(const char *name="rl0");
   virtual      ~St_rl0_Maker();
   virtual Int_t Init();
   virtual Int_t Make();
// virtual void  Set_mode     (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_rl0_Maker.h,v 1.3 1999/07/15 13:58:19 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_rl0_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif




