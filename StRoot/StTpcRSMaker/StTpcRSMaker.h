#ifndef STAR_ST_TRS_MAKER_HH
#define STAR_ST_TRS_MAKER_HH

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcRSMaker virtual base class for Maker                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#ifndef HEP_SYSTEM_OF_UNITS_H
#include "SystemOfUnits.h"
#endif
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StTpcRawData.h"
#include "TF1F.h"
#include "TH1.h"
#include "TTree.h"
#include "StTpcDb/StTpcDb.h"
#include "StMagF.h"
#include "TArrayF.h"
#include "TArrayI.h"
class Altro;
class StTpcdEdxCorrection;
class StTpcDigitalSector;
class HitPoint_t;
class g2t_tpc_hit_st;
class g2t_vertex_st;
class StTpcCoordinateTransform;
struct SignalSum_t {
  Float_t      Sum;
  Short_t      Adc;
  Short_t  TrackId;
};
class StTpcRSMaker : public StMaker {
 public:
  enum EMode {kPAI         = 0,// switch to PAI from GEANT (obsolete)
	      kBICHSEL     = 1,// switch to Bichsel from GEANT 
	      kHEED        = 6,// switch to HEED
	      kGAINOAtALL  = 2,// do not use GAIN at all
	      kdEdxCorr    = 3,// do use TpcdEdxCorrection
	      kDistortion  = 4,// include distortions
	      kNoToflight  = 5 // don't account for particle time of flight
  };
  enum {kPadMax = 32, kTimeBacketMax = 64, kRowMax = 72};
  StTpcRSMaker(const char *name="TpcRS");
  virtual              ~StTpcRSMaker();
  virtual Int_t         InitRun(int runnumber);
  virtual Int_t         Make();
  virtual Int_t  	Finish();
  TF1F *GetShaperResponse(Int_t io = 0, Int_t sector = 1) {return (TF1F *) mShaperResponses[io][sector-1];}          
  TF1F *GetChargeFraction(Int_t io = 0, Int_t sector = 20)     {return (TF1F *) mChargeFraction[io][sector-1];}     
  TF1F *GetPadResponseFunction(Int_t io = 0, Int_t sector = 20){return (TF1F *) mPadResponseFunction[io][sector-1];}
  TF1F *GetPolya(Int_t io = 0)       {return (TF1F *) mPolya[io];}
  TF1F *GetTimeShape0(Int_t io = 0)  {return fgTimeShape0[io];}
  TF1F *GetTimeShape3(Int_t io = 0)  {return fgTimeShape3[io];}
  TF1  *GetHeed()                    {return mHeed;}
  Double_t GetNoPrimaryClusters(Double_t betaGamma, Int_t charge);
  virtual void Print(Option_t *option="") const;
  StTpcDigitalSector *DigitizeSector(Int_t sector);
  void SetLaserScale(Double_t m=1) {mLaserScale = m;}
  static Int_t    AsicThresholds(Short_t ADCs[__MaxNumberOfTimeBins__]);
  static Int_t    SearchT(const void *elem1, const void **elem2);
  static Int_t    CompareT(const void **elem1, const void **elem2);
  static Double_t shapeEI(Double_t *x, Double_t *par=0);
  static Double_t shapeEI_I(Double_t *x, Double_t *par=0);
  static Double_t shapeEI3(Double_t *x, Double_t *par=0);
  static Double_t shapeEI3_I(Double_t *x, Double_t *par=0);
  static Double_t fei(Double_t t, Double_t t0, Double_t T);
  static Double_t polya(Double_t *x, Double_t *par);
  SignalSum_t  *GetSignalSum(Int_t sector);
  SignalSum_t  *ResetSignalSum(Int_t sector);
  void SettauIntegrationX(Double_t p =      74.6e-9, Int_t io=0) {mtauIntegrationX[io] = p;}
  void SettauCX(Double_t           p =    1000.0e-9, Int_t io=0) {mtauCX[io] = p;}
  void SetCutEle(Double_t p = 1e-4)                  {mCutEle = p;}
  static Double_t Ec(Double_t *x, Double_t *p); // minimal energy to create an ion pair
  static TF1 *fEc(Double_t w = 26.2);           // HEED function to generate Ec
 private:
  static Double_t ShaperFunc(Double_t *x, Double_t *p);
  static Double_t PadResponseFunc(Double_t *x, Double_t *p);
  static Double_t Gatti(Double_t *x, Double_t *p);
  static Double_t InducedCharge(Double_t s, Double_t h, Double_t ra, Double_t Va, Double_t &t0);
  static Float_t  GetCutEle();
#if defined(__CINT__) 
  Bool_t TrackSegment2Propagate(g2t_tpc_hit_st *tpc_hitC, g2t_vertex_st *gver, HitPoint_t *TrackSegmentHits);
  void   GenerateSignal(HitPoint_t *TrackSegmentHits,Int_t sector, Int_t rowMin, Int_t rowMax, Double_t sigmaJitterT, Double_t sigmaJitterX);
  Double_t dEdxCorrection(HitPoint_t *TrackSegmentHits);
#else
  Bool_t TrackSegment2Propagate(g2t_tpc_hit_st *tpc_hitC, g2t_vertex_st *gver, HitPoint_t &TrackSegmentHits);
  void   GenerateSignal(HitPoint_t &TrackSegmentHits, Int_t sector, Int_t rowMin, Int_t rowMax, Double_t sigmaJitterT, Double_t sigmaJitterX);
  Double_t dEdxCorrection(HitPoint_t &TrackSegmentHits);
#endif
  static TF1F     *fgTimeShape3[2];  //!
  static TF1F     *fgTimeShape0[2];   //!
  Char_t   beg[1];                    //!
  TTree   *fTree;                     //!
  SignalSum_t     *m_SignalSum;       //!
  TH1D*    mdNdx;                     //!
  TH1D*    mdNdxL10;                  //!
  TH1D*    mdNdEL10;                  //!
  TF1F  *mShaperResponses[2][24];     //!
  TF1F  *mShaperResponse;             //!
  TF1F  *mChargeFraction[2][24];      //!
  TF1F  *mPadResponseFunction[2][24]; //!
  TF1F  *mPolya[2];                   //!
  TF1F  *mGG;                         //! Gating Grid Transperency
  TF1   *mHeed;                       //!
  StTpcdEdxCorrection *m_TpcdEdxCorrection; // !
  Double_t InnerAlphaVariation[24];   //!
  Double_t OuterAlphaVariation[24];   //!
  Altro *mAltro;                      //!
  // local variables
  Int_t numberOfSectors;              //!
  Int_t NoPads;                       //!
  Int_t numberOfTimeBins;             //!
  Int_t    numberOfInnerSectorAnodeWires; //! 
  Double_t firstInnerSectorAnodeWire; //!
  Double_t lastInnerSectorAnodeWire;  //!
  Int_t    numberOfOuterSectorAnodeWires; //!
  Double_t firstOuterSectorAnodeWire; //!
  Double_t lastOuterSectorAnodeWire;  //!
  Double_t anodeWirePitch;            //!
  Double_t numberOfElectronsPerADCcount; //!
  Double_t anodeWireRadius;           //!
  Double_t innerSectorAnodeVoltage[24];//!
  Double_t outerSectorAnodeVoltage[24];//!
  Double_t      mtauIntegrationX[2];  //! for TPX inner=0/outer=1
  Double_t      mtauCX[2];            //! -"- 
  Double_t    mLocalYDirectionCoupling[2][24][7]; //!
  Double_t   msMin, msMax;            //!
  TArrayI    mNoTpcHitsAll;           //!
  TArrayI    mNoTpcHitsReal;          //!
  Int_t      mNSplittedHits;          //!
  Double_t xOnWire, yOnWire, zOnWire; //!
  Double_t mGainLocal;                //!
  Double_t QAv;                       //!
  Double_t TotalSignal;               //!
  Int_t pad0;                         //!
  Int_t tbk0;                         //!
  Double_t TotalSignalInCluster;      //!
  Double_t padsdE[kPadMax];           //!
  Double_t tbksdE[kTimeBacketMax];    //!
  Double_t rowsdEH[kRowMax];          //!
  Double_t rowsdE[kRowMax];           //!
  Char_t end[1];                      //!
  Double_t             mLaserScale;   //!
  const Double_t minSignal;           //!
  const Double_t ElectronRange;       //!
  const Double_t ElectronRangeEnergy; //!
  const Double_t ElectronRangePower;  //!
  const Int_t NoOfSectors;            //!
  const Int_t NoOfPads;               //!
  const Int_t NoOfTimeBins;           //!
  Double_t   mCutEle;                 //! cut for delta electrons
 public:    
  virtual const char *GetCVS() const {
    static const char cvs[]= 
      "Tag $Name:  $ $Id: StTpcRSMaker.h,v 1.33 2018/12/09 23:22:59 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
      return cvs;
  }
  ClassDef(StTpcRSMaker,0)   //StAF chain virtual base class for Makers
};
#endif
// $Id: StTpcRSMaker.h,v 1.33 2018/12/09 23:22:59 fisyak Exp $
// $Log: StTpcRSMaker.h,v $
// Revision 1.33  2018/12/09 23:22:59  fisyak
// Reshape
//
// Revision 1.32  2018/11/01 13:27:20  fisyak
// Synchronize mCutEle with GEANT3 tracking media setting for TPCE_SENSITIVE_GAS, bug#3369
//
// Revision 1.31  2018/10/17 20:45:28  fisyak
// Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
//
// Revision 1.28  2016/09/18 22:45:25  fisyak
// Clean up, add Heed model, adjust for new StTpcdEdxCorrections
//
// Revision 1.27  2015/07/19 22:48:14  fisyak
// Fix cvs message
//
// Revision 1.26  2015/07/19 22:14:07  fisyak
// Clean up __PAD_BLOCK__, recalculate no. of real hits in g2t_track n_tpc_hit (excluding pseudo pad row), add current and accumulated charge in dE/dx correction
//
// Revision 1.25  2012/05/07 15:36:22  fisyak
// Remove hardcoded TPC parameters
//
// Revision 1.24  2012/04/03 14:05:18  fisyak
// Speed up using  GetSaveL (__PAD_BLOCK__), sluggish shape histograms, Heed electron generation
//
// Revision 1.23  2011/12/13 17:23:22  fisyak
// remove YXTProd, add WIREHISTOGRAM and WIREMAP, use particle definition from StarClassLibrary
//
// Revision 1.22  2011/10/14 23:27:51  fisyak
// Back to standard version
//
// Revision 1.20  2011/09/18 22:39:48  fisyak
// Extend dN/dx table (H.Bichsel 09/12/2011) to fix bug #2174 and #2181, clean-up
//
// Revision 1.19  2011/03/17 14:29:31  fisyak
// Add extrapolation in region beta*gamma < 0.3
//
// Revision 1.18  2010/06/14 23:34:26  fisyak
// Freeze at Version V
//
// Revision 1.17  2010/03/22 23:45:06  fisyak
// Freeze version with new parameters table
//
// Revision 1.16  2010/03/17 15:53:16  fisyak
// Move StTpcdEdxCorrection to StdEdxY2Maker to avoid dependence of StTpcDb on StDetectorDbMaker
//
// Revision 1.15  2010/03/16 19:41:46  fisyak
// Move diffusion and sec/row correction in DB, clean up
//
// Revision 1.14  2010/02/26 18:53:33  fisyak
// Take longitudinal Diffusion from Laser track fit, add Gating Grid
//
// Revision 1.13  2010/01/26 19:47:26  fisyak
// Include dE/dx calibration and distortions in the simulation
//
// Revision 1.12  2009/10/30 21:12:00  fisyak
// Freeze version rcf9108.F, Clean up
//
// Revision 1.11  2009/10/03 21:29:09  fisyak
// Clean up, move all TpcT related macro into StTpcMcAnalysisMaker
//
// Revision 1.10  2009/09/21 13:20:39  fisyak
// Variant O4, no mSigmaJitter, 100 keV
//
// Revision 1.9  2009/09/01 15:06:44  fisyak
// Version N
//
// Revision 1.8  2009/08/24 20:16:41  fisyak
// Freeze with new Altro parameters
//
// Revision 1.7  2008/12/29 15:24:55  fisyak
// Freeze ~/WWW/star/Tpc/TpcRS/ComparisonMIP31
//
// Revision 1.6  2008/12/18 23:06:38  fisyak
// Take care about references to TGiant
//
// Revision 1.5  2008/08/18 15:54:26  fisyak
// Version 20
//
// Revision 1.4  2008/07/30 23:53:19  fisyak
// Freeze
//
// Revision 1.3  2008/07/18 16:21:17  fisyak
// Remove TF1F
//
// Revision 1.2  2008/06/19 22:45:43  fisyak
// Freeze problem with TPX parameterization
//
// Revision 1.1.1.1  2008/04/28 14:39:47  fisyak
// Start new Tpc Response Simulator
//
// Revision 1.14  2008/04/24 10:42:04  fisyak
// Fix binning issues
//
// Revision 1.13  2008/04/04 15:00:11  fisyak
// Freeze before shaper modifications
//
// Revision 1.12  2005/02/07 21:40:31  fisyak
// rename antique TGeant3 to TGiant3
//
// Revision 1.11  2005/01/26 21:45:31  fisyak
// Freeze correction made in June
//
// Revision 1.9  2004/05/29 21:16:27  fisyak
// Fix pad direction, add sorting for ADC/cluster nonlinearity, replace product by sum of logs
//
// Revision 1.8  2004/05/04 13:39:06  fisyak
// Add TF1
//
// Revision 1.7  2004/04/22 01:05:03  fisyak
// Freeze the version before modification parametrization for K3
//
// Revision 1.6  2004/04/06 01:50:13  fisyak
// Switch from Double_t to Float_t for sum
//
// Revision 1.5  2004/03/30 19:30:04  fisyak
// Add Laser
//
// Revision 1.4  2004/03/21 19:00:43  fisyak
// switch to GEANT step length
//
// Revision 1.3  2004/03/20 17:57:15  fisyak
// Freeze the version of PAI model
//
// Revision 1.2  2004/03/17 20:47:43  fisyak
// Add version with TrsCluster TTree
//
// Revision 1.1.1.1  2004/03/05 20:51:25  fisyak
// replacement for Trs
//
