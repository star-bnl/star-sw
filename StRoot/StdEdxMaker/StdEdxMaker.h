// $Id: StdEdxMaker.h,v 1.4 2001/03/21 16:25:22 fisyak Exp $
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
class St_tpcGain;
class St_fee_vs_pad_row;
class St_TpcTimeGain;
class St_TpcDriftDistCorr;
class St_tpcBadPad;
class StThreeVectorD;
class St_TpcSecRowCor;
class StdEdxMaker : public StMaker {
 private:
  St_tpcGain          *m_tpcGain;       //!
  St_TpcSecRowCor     *m_TpcSecRow;     //!
  St_fee_vs_pad_row   *m_fee_vs_pad_row;//!
  St_TpcTimeGain      *m_tpcTime;       //!
  St_TpcDriftDistCorr *m_drift;         //!
  St_tpcBadPad        *m_badpads;       //!
  StThreeVectorD      *mNormal[24];     //!
 public: 
  StdEdxMaker(const char *name="dEdx");
  virtual       ~StdEdxMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  void    SortdEdx(Double_t *dEdxS, Int_t NPoints);
  Double_t MyDate(Int_t date,Int_t time);
  void DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ);
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: StdEdxMaker.h,v 1.4 2001/03/21 16:25:22 fisyak Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StdEdxMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

