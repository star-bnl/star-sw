// $Id: StdEdxMaker.h,v 1.2 2000/11/25 23:19:53 fisyak Exp $
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
static  Double_t Sirrf(Double_t poverm);
class St_tpcGain;
class St_TpcTimeGain;
class St_TpcDriftDistCorr;
class StThreeVectorD;
class StdEdxMaker : public StMaker {
 private:
  St_tpcGain          *m_tpcGain;      //!
  St_TpcTimeGain      *m_tpcTime;      //!
  St_TpcDriftDistCorr *m_drift;        //!
  StThreeVectorD      *mNormal[24];    //!
 protected:
 public: 
                  StdEdxMaker(const char *name="dEdx");
   virtual       ~StdEdxMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
   void    SortdEdx(Double_t *dEdxS, Int_t NPoints);
   Double_t MyDate(Int_t date,Int_t time);
   void DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ);
   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StdEdxMaker.h,v 1.2 2000/11/25 23:19:53 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StdEdxMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

