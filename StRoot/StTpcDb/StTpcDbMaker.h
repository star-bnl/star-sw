#ifndef STAR_StTpcDbMaker
#define STAR_StTpcDbMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDbMaker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class StTpcDb;
class St_tpg_pad_plane;
class St_tpg_detector;
class StTpcDbMaker : public StMaker {
 private:
  StTpcDb* m_TpcDb;               //! tpc database class
  St_tpg_pad_plane* m_tpg_pad_plane; //!
  St_tpg_detector* m_tpg_detector; //! 
 protected:
 public: 
                  StTpcDbMaker(const char *name="TLA");
   virtual       ~StTpcDbMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void Update_tpg_pad_plane();
   virtual void Update_tpg_detector();
   virtual StTpcDb* tpcDbInterface() const;    //! return m_TpcDb
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcDbMaker.h,v 1.2 1999/09/21 20:14:35 hardtke Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StTpcDbMaker, 1)   //StAF chain virtual base class for Makers
};

inline StTpcDb* StTpcDbMaker::tpcDbInterface() const {return m_TpcDb;}

#endif
