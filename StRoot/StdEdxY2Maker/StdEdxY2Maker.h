// $Id: StdEdxY2Maker.h,v 1.7 2004/04/08 16:49:31 fisyak Exp $
#ifndef STAR_StdEdxY2Maker
#define STAR_StdEdxY2Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StdEdxY2Maker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "dEdxPoint.h"
#include "StThreeVectorF.hh"
class St_fee_vs_pad_row;
class St_tpcCorrection;
class tpcCorrection_st;
class StThreeVectorD;
class St_TpcSecRowCor;
class St_tpcGas;
class St_tpcPressure;
class St_TpcSecRowA;
class Bichsel;
class StGlobalTrack;
class St_trigDetSums;
class trigDetSums_st;
class St_tpcGainMonitor;
class TMinuit; 
class StdEdxY2Maker : public StMaker {
 private:
  Bichsel             *m_Bichsel;       //!
  St_TpcSecRowCor     *m_TpcSecRow;     //!
  St_tpcCorrection    *m_drift;         //!
  St_tpcCorrection    *m_Multiplicity;  //!
  St_tpcCorrection    *m_AdcCorrection; //!
  St_tpcCorrection    *m_zCorrection;   //!
  St_tpcCorrection    *m_dXCorrection;  //!
  St_tpcCorrection    *m_TpcdEdxCor;    //!
  St_tpcCorrection    *m_TpcLengthCorrection;  //!
  St_tpcGas           *m_tpcGas;        //!
  St_tpcCorrection    *m_tpcPressure;   //!
  St_TpcSecRowA       *m_TpcSecRowA;    //!
  St_trigDetSums      *m_trigDetSums;   //!
  trigDetSums_st      *m_trig;          //!
  Bool_t               m_Simulation;    //!
  Bool_t               m_InitDone;      //!
  StThreeVectorD      *mNormal[24];     //!
  StThreeVectorD      *mRowPosition[24][45][3]; //!
  St_tpcGainMonitor   *m_tpcGainMonitor;//!
  Int_t                m_ClusterFinder; //! old == 0; new != 0
  Int_t                m_Calibration;   //!
  Int_t                m_DoNotCorrectdEdx; //!
  TMinuit             *m_Minuit;        //!

 public: 
  StdEdxY2Maker(const char *name="dEdxY2");
  virtual       ~StdEdxY2Maker();
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t RunNumber);
  virtual Int_t Finish();
  virtual Int_t FinishRun(Int_t OldRunNumber);
  virtual Int_t Make();
  void    SortdEdx(Int_t N, dEdx_t *dE, dEdx_t *dES);
  Double_t LikeliHood(Double_t Xlog10bg, Int_t NdEdx, dEdx_t *dEdx);
  Double_t CalcCorrection(const tpcCorrection_st *cor,const Double_t x);
  void    Histogramming(StGlobalTrack* gTrack);
  void    QAPlots(StGlobalTrack* gTrack = 0);
  void    BadHit(const StThreeVectorF &xyz);
  void    DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ);
  void    PrintdEdx(Int_t iop = 0);
  static  void Landau(Double_t x, Double_t *val);
  static  void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: StdEdxY2Maker.h,v 1.7 2004/04/08 16:49:31 fisyak Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StdEdxY2Maker,0)   //StAF chain virtual base class for Makers
};

#endif

