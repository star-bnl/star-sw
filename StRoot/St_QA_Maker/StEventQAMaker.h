//  
//  
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventQAMaker virtual base class adapted from St_QA_Maker           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StEventQAMaker
#define STAR_StEventQAMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef ROOT_TH1
#include "TH1.h"
#endif

#ifndef ROOT_TH2
#include "TH2.h"
#endif

#include "TList.h"
#include "TString.h"

#include "St_QA_Maker/StQABookHist.h"

class StEvent;

class StEventQAMaker : public StQABookHist {
 private:
  Bool_t drawinit;

  //! static Char_t m_VersionCVS = "$Id: StEventQAMaker.h,v 1.2 1999/11/22 22:46:41 lansdell Exp $";
 
 public: 
  StEventQAMaker(const char *name="EventQA", const char *title="StEvent/QA");
  virtual       ~StEventQAMaker();
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();
  virtual void   MakeHistEvSum(StEvent *event);
  virtual void   MakeHistGlob(StEvent *event);
  virtual void   MakeHistDE(StEvent *event);
  virtual void   MakeHistPrim(StEvent *event);
  virtual void   MakeHistGen(StEvent *event);
  virtual void   MakeHistV0(StEvent *event);
  virtual void   MakeHistPID(StEvent *event);
  virtual void   MakeHistVertex(StEvent *event);
  virtual void   MakeHistXi(StEvent *event);
  virtual void   MakeHistPoint(StEvent *event);
  virtual void   MakeHistKink(StEvent *event);
  virtual void   MakeHistL3(StEvent *event);
  virtual void   MakeHistV0Eval(StEvent *event);
  virtual void   MakeHistRich(StEvent *event);
  
  virtual void   SetDraw(Bool_t drawFlag=kTRUE);
//  virtual void   SetPntrToHistUtil(StHistUtil *m1);

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEventQAMaker.h,v 1.2 1999/11/22 22:46:41 lansdell Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEventQAMaker, 1)   //StAF chain virtual base class for Makers
    };
    
#endif
    
inline void StEventQAMaker::SetDraw(Bool_t drawFlag) 
                         { drawinit = drawFlag;}
