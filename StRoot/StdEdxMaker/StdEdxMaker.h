// $Id: StdEdxMaker.h,v 1.8 2001/12/11 13:31:43 fisyak Exp $
#ifndef STAR_StdEdxMaker
#define STAR_StdEdxMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StdEdxMaker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "dEdxPoint.h"
class St_fee_vs_pad_row;
class St_TpcTimeGain;
class St_TpcDriftDistCorr;
class St_tpcBadPad;
class StThreeVectorD;
class St_TpcSecRowCor;
class StTrackChair;

class StdEdxMaker : public StMaker {
 private:
  St_TpcSecRowCor     *m_TpcSecRow;     //!
  St_fee_vs_pad_row   *m_fee_vs_pad_row;//!
  St_TpcTimeGain      *m_tpcTime;       //!
  St_TpcDriftDistCorr *m_drift;         //!
  St_tpcBadPad        *m_badpads;       //!
  Bool_t               m_Simulation;    //!
  Bool_t               m_InitDone;      //!
  StThreeVectorD      *mNormal[24];     //!
  StThreeVectorD      *mRowPosition[24][45][3]; //!
 public: 
  StdEdxMaker(const char *name="dEdx");
  virtual       ~StdEdxMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t RunNumber);
  virtual Int_t Finish();
  virtual Int_t FinishRun(Int_t OldRunNumber);
  virtual Int_t Make();
  void    SortdEdx(dEdx_t *dEdxS, Int_t NPoints);
#if 0
  Double_t MyDate(Int_t date,Int_t time);
#endif
  void DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ);
  void Histogramming(StTrackChair     *globtrkC, Int_t iglob);
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: StdEdxMaker.h,v 1.8 2001/12/11 13:31:43 fisyak Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StdEdxMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

