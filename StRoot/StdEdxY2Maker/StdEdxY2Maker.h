// $Id: StdEdxY2Maker.h,v 1.33 2019/11/29 19:00:08 fisyak Exp $
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
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh" 
#include "StPhysicalHelixD.hh"
#include "tables/St_trigDetSums_Table.h"
class StGlobalTrack;
class TMinuit; 
class StEvent;
class StGlobalCoordinate;
class TH2F;
class StTpcPadrowHitCollection;
class StTrack;
class StTpcdEdxCorrection;
class dEdxY2_t;
class dst_dedx_st;
class StdEdxY2Maker : public StMaker {
 public: 
  enum  EMode {kOldClusterFinder     =  0,
	       kCalibration          = 	1,
	       kDoNotCorrectdEdx     = 	2,
	       kPadSelection         = 	3,
	       kMip                  = 	4,
	       kAdcHistos            = 	5,
	       kXYZcheck             = 	6,
	       kSpaceChargeStudy     = 	7,
	       kGASHISTOGRAMS        = 	8,
	       kProbabilityPlot      = 	9,
	       kMakeTree             = 10,
	       kCORRELATION          = 11,
	       kAlignment            = 12, 
	       kZBGX                 = 13,
	       kEmbedding            = 15,
	       kNoUsedHits           = 16,
	       kEmbeddingShortCut    = 17, 
	       kV0CrossCheck         = 18
  };
  StdEdxY2Maker(const char *name="dEdxY2");
  virtual       ~StdEdxY2Maker() {}
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t RunNumber);
  virtual Int_t Finish();
  virtual Int_t Make();
  virtual void  SetMask(Int_t mask) {m_Mask = mask;}
  static  void  SortdEdx();
  Double_t LikeliHood(Double_t Xlog10bg, Int_t NdEdx, dEdxY2_t *dEdx, Double_t chargeSq = 1);
  void    Histogramming(StGlobalTrack* gTrack=0);
  void    V0CrossCheck();
  void    TrigHistos(Int_t iok = 0);
  void    XyzCheck(StGlobalCoordinate *global=0, Int_t iokCheck=0);
  void    QAPlots(StGlobalTrack* gTrack = 0);
  void    BadHit(Int_t iFlag, const StThreeVectorF &xyz);
  void    DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ);
  void    DoFitN(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ);
  static  void PrintdEdx(Int_t iop = 0);
  static  void Landau(Double_t x, Double_t *val);
  static  void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
  static  void fcnN(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
  static  Double_t gaus2(Double_t *x, Double_t *p);
  static  TF1 *Gaus2();
 private:
  void   AddEdxTraits(StTrack *tracks[2], dst_dedx_st &dedx);
  static Int_t Propagate(const StThreeVectorD &middle,const StThreeVectorD &normal,
			 const StPhysicalHelixD &helixI, const StPhysicalHelixD &helixO,
			 StThreeVectorD &xyz, StThreeVectorD &dirG, Double_t s[2], Double_t w[2]);
  static Int_t     NdEdx;
  static dEdxY2_t *CdEdx; // corrected
  static dEdxY2_t *FdEdx; // fit
  static dEdxY2_t *dEdxS; // dEdx sorted
  static void      UsedNdx() {fUsedNdx = kTRUE;}
  Int_t                m_Mask; //!
  Char_t               beg[1];
  TMinuit             *m_Minuit;        //!
  StTpcdEdxCorrection *m_TpcdEdxCorrection; // !
#ifdef __OLD_dX_Calculation__
  StThreeVectorD     **mNormal[24];     //!
  StThreeVectorD     **mRowPosition[24][3]; //!
  StThreeVectorD      *mPromptNormal[2][2]; // West/East, Inner/Outer
  StThreeVectorD      *mPromptPosition[2][2][3]; 
#endif /* __OLD_dX_Calculation__ */
  TH2F                *mHitsUsage;//!
  Char_t               end[1];
  static Double_t      bField;
  static Bool_t        fUsedNdx;
 public:
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: StdEdxY2Maker.h,v 1.33 2019/11/29 19:00:08 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StdEdxY2Maker,0)   //StAF chain virtual base class for Makers
};

#endif

