// $Id: StdEdxMaker.h,v 1.3 2000/12/29 14:36:29 fisyak Exp $
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
class St_tpcFeeGainCor;
class St_fee_vs_pad_row;
class St_TpcTimeGain;
class St_TpcDriftDistCorr;
class StThreeVectorD;
class StdEdxMaker : public StMaker {
 private:
  St_tpcGain          *m_tpcGain;       //!
  St_tpcFeeGainCor    *m_tpcFeeGain;    //!
  St_fee_vs_pad_row   *m_fee_vs_pad_row;//!
  St_TpcTimeGain      *m_tpcTime;       //!
  St_TpcDriftDistCorr *m_drift;         //!
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
      "Tag $Name:  $ $Id: StdEdxMaker.h,v 1.3 2000/12/29 14:36:29 fisyak Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StdEdxMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

