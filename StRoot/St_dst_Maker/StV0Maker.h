#ifndef STAR_StV0Maker
#define STAR_StV0Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0Maker virtual base class for Maker                               //
//                                                                      //
// $Id: StV0Maker.h,v 1.3 1999/07/08 19:09:52 fisyak Exp $
// $Log: StV0Maker.h,v $
// Revision 1.3  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_ev0_ev0par;
class St_ev0_ev0par2;

class StV0Maker : public StMaker {

 private:
  Bool_t m_ev0EvalOn;   //switch for the evaluation
  // static Char_t m_VersionCVS = "$Id: StV0Maker.h,v 1.3 1999/07/08 19:09:52 fisyak Exp $";
  St_ev0_ev0par  *m_ev0par;      //!
  St_ev0_ev0par2 *m_ev0par2;     //!

 protected:

  
 public: 
  StV0Maker(const char *name="v0");
  virtual       ~StV0Maker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual void   ev0Eval(Bool_t flag=kFALSE){m_ev0EvalOn=flag;} // *MENU*
  virtual void   ev0EvalOn() {ev0Eval(kTRUE);} 
  virtual void   ev0EvalOff(){ev0Eval();}      
  ClassDef(StV0Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
