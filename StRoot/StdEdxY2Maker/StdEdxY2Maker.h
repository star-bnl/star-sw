// $Id: StdEdxY2Maker.h,v 1.29 2014/08/06 11:43:59 jeromel Exp $
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
#include "StTpcdEdxCorrection.h" 
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
  virtual Int_t FinishRun(Int_t OldRunNumber);
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
  void    PrintdEdx(Int_t iop = 0);
  static  void Landau(Double_t x, Double_t *val);
  static  void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
  static  Double_t gaus2(Double_t *x, Double_t *p);
  static  TF1 *Gaus2();
 private:
  static Int_t Propagate(const StThreeVectorD &middle,const StThreeVectorD &normal,
			 const StPhysicalHelixD &helixI, const StPhysicalHelixD &helixO,
			 Double_t bField, 
			 StThreeVectorD &xyz, StThreeVectorD &dirG, Double_t s[2], Double_t w[2]);
  static Int_t     NdEdx;
  static dEdxY2_t *CdEdx; // corrected
  static dEdxY2_t *FdEdx; // fit
  static dEdxY2_t *dEdxS; // dEdx sorted
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
  Char_t              end[1];
 public:
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: StdEdxY2Maker.h,v 1.29 2014/08/06 11:43:59 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StdEdxY2Maker,0)   //StAF chain virtual base class for Makers
};

#endif

