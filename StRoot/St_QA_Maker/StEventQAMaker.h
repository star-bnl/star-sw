// $Id: StEventQAMaker.h,v 1.3 1999/11/23 19:00:51 lansdell Exp $
// $Log: StEventQAMaker.h,v $
// Revision 1.3  1999/11/23 19:00:51  lansdell
// Reorganized Make() and include files (Gene)
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

#include "StQABookHist.h"

class StEvent;

//////////////////////////////////////////////////////////////////////////

class StEventQAMaker : public StQABookHist {
 private:
  //! static Char_t m_VersionCVS = "$Id: StEventQAMaker.h,v 1.3 1999/11/23 19:00:51 lansdell Exp $";
 
  StEvent *event;       //! pointer to current event
 
//------------------------------------------------------------------------
  
 public: 

  StEventQAMaker(const char *name="EventQA", const char *title="StEvent/QA");
  virtual       ~StEventQAMaker();
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();
  
  virtual void   MakeHistEvSum();
  virtual void   MakeHistGlob();
  virtual void   MakeHistDE();
  virtual void   MakeHistPrim();
  virtual void   MakeHistGen();
  virtual void   MakeHistV0();
  virtual void   MakeHistPID();
  virtual void   MakeHistVertex();
  virtual void   MakeHistXi();
  virtual void   MakeHistPoint();
  virtual void   MakeHistKink();
  virtual void   MakeHistL3();
  virtual void   MakeHistV0Eval();
  virtual void   MakeHistRich();
  
//  virtual void   SetPntrToHistUtil(StHistUtil *m1);

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEventQAMaker.h,v 1.3 1999/11/23 19:00:51 lansdell Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEventQAMaker, 1)   //StAF chain virtual base class for Makers
    };
    
#endif
