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
class StTpcDbMaker : public StMaker {
 private:
  StTpcDb* m_TpcDb;               //! tpc database class 
 protected:
 public: 
                  StTpcDbMaker(const char *name="TLA");
   virtual       ~StTpcDbMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual StTpcDb* tpcDbInterface();    //! return m_TpcDb
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcDbMaker.h,v 1.1 1999/09/16 18:47:24 hardtke Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StTpcDbMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
