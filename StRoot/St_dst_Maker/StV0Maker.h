#ifndef STAR_StV0Maker
#define STAR_StV0Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0Maker virtual base class for Maker                               //
//                                                                      //
// $Id: StV0Maker.h,v 1.7 2000/10/12 14:52:30 genevb Exp $
// $Log: StV0Maker.h,v $
// Revision 1.7  2000/10/12 14:52:30  genevb
// Remove vertex table entries when trimming V0s
//
// Revision 1.6  2000/08/31 21:47:08  genevb
// Allow V0s to be trimmed after finding Xis
//
// Revision 1.5  1999/07/15 13:57:54  perev
// cleanup
//
// Revision 1.4  1999/07/12 23:04:17  fisyak
// Remove glob2
//
// Revision 1.3  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_ev0_ev0par;
class St_ev0_ev0par2;
class St_dst_vertex;
class St_dst_v0_vertex;

class StV0Maker : public StMaker {

 private:
  Bool_t m_ev0EvalOn;   //switch for the evaluation
  // static Char_t m_VersionCVS = "$Id: StV0Maker.h,v 1.7 2000/10/12 14:52:30 genevb Exp $";
  St_ev0_ev0par  *m_ev0par;        //!
  St_ev0_ev0par2 *m_ev0par2;       //!
  St_ev0_ev0par2 *m_ev0parT;       //!
  St_dst_vertex  *vertex;          //!
  St_dst_v0_vertex *dst_v0_vertex; //!

 protected:
  virtual void   ChopVertex(Long_t idVert);
  
 public: 
  StV0Maker(const char *name="v0");
  virtual       ~StV0Maker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void   ev0Eval(Bool_t flag=kFALSE){m_ev0EvalOn=flag;} // *MENU*
  virtual void   ev0EvalOn() {ev0Eval(kTRUE);}
  virtual void   ev0EvalOff(){ev0Eval();}
  virtual void   Trim();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StV0Maker.h,v 1.7 2000/10/12 14:52:30 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StV0Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
