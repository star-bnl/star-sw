// $Id: St_QA_Maker.h,v 2.0 2000/08/25 16:02:41 genevb Exp $
// $Log: St_QA_Maker.h,v $
// Revision 2.0  2000/08/25 16:02:41  genevb
// New revision: new structure, multiplicity classes
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  St_QA_Maker class for QA Histograms using dst tables                 //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_St_QA_Maker
#define STAR_St_QA_Maker

#include "StQAMakerBase.h"

//////////////////////////////////////////////////////////////////////////

class St_QA_Maker : public StQAMakerBase {
 private:

  St_DataSet *dst;        //! Pointer to current dataset - dst
  
//------------------------------------------------------------------------
  
 public: 

  St_QA_Maker(const char *name="QA", const char *title="event/QA");
  virtual       ~St_QA_Maker() {}
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();

  virtual void   MakeHistEvSum();
  virtual void   MakeHistGlob();
  virtual void   MakeHistDE();
  virtual void   MakeHistPrim();
  virtual void   MakeHistGen();
  virtual void   MakeHistPID();
  virtual void   MakeHistVertex();
  virtual void   MakeHistPoint();
  virtual void   MakeHistRich();
  virtual void   MakeHistEval();

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_QA_Maker.h,v 2.0 2000/08/25 16:02:41 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(St_QA_Maker,0)
};
    
#endif









