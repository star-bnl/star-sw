// $Id: StEventQAMaker.h,v 2.0 2000/08/25 16:02:40 genevb Exp $
// $Log: StEventQAMaker.h,v $
// Revision 2.0  2000/08/25 16:02:40  genevb
// New revision: new structure, multiplicity classes
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StEventQAMaker class for QA Histograms using StEvent                 //
//     adapted from St_QA_Maker                                          //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StEventQAMaker
#define STAR_StEventQAMaker

#include "StQAMakerBase.h"

class StEvent;
class HitHistograms;

//////////////////////////////////////////////////////////////////////////

class StEventQAMaker : public StQAMakerBase {
 private:
 
  StEvent *event;          //! pointer to current event
  HitHistograms *mHitHist; //!
 
//------------------------------------------------------------------------
  
 public: 

  StEventQAMaker(const char *name="EventQA", const char *title="StEvent/QA");
  virtual       ~StEventQAMaker() {}
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
  
//  virtual void   SetPntrToHistUtil(StHistUtil *m1);

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEventQAMaker.h,v 2.0 2000/08/25 16:02:40 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEventQAMaker,0)   //StAF chain virtual base class for Makers
    };
    
#endif
