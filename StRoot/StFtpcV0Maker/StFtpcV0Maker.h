// $Id: StFtpcV0Maker.h,v 1.4 2000/01/03 13:16:17 jcs Exp $
//
// $Log: StFtpcV0Maker.h,v $
// Revision 1.4  2000/01/03 13:16:17  jcs
// Add CVS Id strings
//

#ifndef STAR_St_fv0_Maker
#define STAR_St_fv0_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcV0Maker                                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;
class StFtpcV0Maker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: StFtpcV0Maker.h,v 1.4 2000/01/03 13:16:17 jcs Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters
 
 protected:
 TH1F  *m_b;
 TH1F  *m_dca;
 TH1F  *m_kaonMass;
 TH1F  *m_lambdaMass;
 TH1F  *m_antiLambdaMass;
 
 public: 
                  StFtpcV0Maker(const char *name="fv0", const char *title="fv0_something");
   virtual       ~StFtpcV0Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcV0Maker.h,v 1.4 2000/01/03 13:16:17 jcs Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StFtpcV0Maker, 1)   //StAF chain virtual base class for Makers
};

#endif



