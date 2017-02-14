// $Id: StTpcdEdxCorrection.h,v 1.8 2016/09/18 22:40:31 fisyak Exp $
#ifndef STAR_StTpcdEdxCorrection
#define STAR_StTpcdEdxCorrection
//
#include "TObject.h"
#include "TF1.h"
#include "Stiostream.h"
#include "StDetectorDbMaker/St_tpcCorrectionC.h"
#include "StDetectorDbMaker/St_tpcGasC.h"
//#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StTrackPidTraits.h"
//class St_trigDetSums;
//class trigDetSums_st;
//________________________________________________________________________________
struct dE_t {
 public:
  Float_t dE;
  Float_t dx;
  Float_t dEdx;
  Float_t dEdxL;
  Float_t dEdxN;
};
//________________________________________________________________________________
struct dEdxCorrection_t {
  dEdxCorrection_t(const Char_t *name = "",const  Char_t *title = "", TChair *chair=0, Int_t n=0) : 
    Name(name), Title(title), Chair(chair), nrows(n), dE(0) {}
  const Char_t *Name;
  const Char_t *Title;
  TChair       *Chair;
  Int_t   nrows;
  Float_t dE;
};
//________________________________________________________________________________
class dEdxY2_t;
class StTpcdEdxCorrection : public TObject {
 public:
  enum ESector : int {kTpcOuter = 0, kTpcInner = 1};
  enum EOptions : int {
    kUncorrected           =  0,//U   				           
    kEdge                  =  1,//E   correction near edge of chamber	     
    kAdcCorrection         =  2,//R  					     
    kTpcdCharge            =  3,//D  					     
    kTpcrCharge            =  4,//D  					     
    kTpcCurrentCorrection  =  5,//      					     
    kTpcRowQ               =  6,//   	 					       	   
    kTpcSecRowB            =  7,//S  					     
    kTpcSecRowC            =  8,//S  					     
    ktpcPressure           =  9,//P  					     
    ktpcTime               = 10,//t  					     
    kDrift                 = 11,//O  					     
    kMultiplicity          = 12,//M  					     
    kzCorrection           = 13,//Z  					     
    ktpcMethaneIn          = 14,//m  					     
    ktpcGasTemperature     = 15,//T  					     
    ktpcWaterOut           = 16,//W   				7       	   
    kSpaceCharge           = 17,//C   space charge near the wire	       	   
    kPhiDirection          = 18,//p   correction wrt local interception angle  
    kTanL                  = 19,//p   correction wrt local tan(lambda)  
    kdXCorrection          = 20,//X  					     
    kTpcEffectivedX        = 21,//X   Effective pad row height
    kTpcPadTBins           = 22,//d  					     
    kTpcZDC                = 23,//   					     
    kTpcPadMDF             = 24, 
    kTpcLast               = 25,//                                             
    kTpcNoAnodeVGainC      = 26,//   					     
    kTpcLengthCorrection   = 27,//                                             
    kTpcLengthCorrectionMDF= 28,//   					   
    kTpcdEdxCor            = 29,//   					   
    kTpcAllCorrections     = 30 //                                             
  };
  StTpcdEdxCorrection(Int_t Option=0, Int_t debug=0);
  ~StTpcdEdxCorrection();
  Int_t dEdxCorrection(dEdxY2_t &dEdx, Bool_t doIT=kTRUE); 
  Int_t dEdxTrackCorrection(Int_t type, dst_dedx_st &dedx);
  Int_t dEdxTrackCorrection(EOptions k, Int_t type, dst_dedx_st &dedx);
  void SettpcGas               (St_tpcGas          *m = 0) {m_tpcGas = m;}
  
  void SetDebug(Int_t m=0) {m_Debug = m;}
  void SetMask (Int_t m=0) {m_Mask = m;}
  void ReSetCorrections();
  St_tpcGas         *tpcGas()              {return m_tpcGas;}
  //  St_trigDetSums    *trigDetSums()         {return m_trigDetSums;}

  St_tpcCorrectionC *Correction(Int_t k = 0) { return dynamic_cast<St_tpcCorrectionC *>(m_Corrections[k].Chair);}
  St_tpcCorrectionC *drift()               {return Correction(kDrift);}
  St_tpcCorrectionC *Multiplicity()        {return Correction(kMultiplicity);}
  St_tpcCorrectionC *AdcCorrection()       {return Correction(kAdcCorrection);}
  St_tpcCorrectionC *zCorrection()         {return Correction(kzCorrection);}
  St_tpcCorrectionC *dXCorrection()        {return Correction(kdXCorrection);}
  St_tpcCorrectionC *TpcdEdxCor()          {return Correction(kTpcdEdxCor);}
  St_tpcCorrectionC *TpcLengthCorrection() {return Correction(kTpcLengthCorrection);}
  St_tpcCorrectionC *tpcPressure()         {return Correction(ktpcPressure);}
  St_tpcCorrectionC *tpcMethaneIn()        {return Correction(ktpcMethaneIn);}
  St_tpcCorrectionC *tpcGasTemperature()   {return Correction(ktpcGasTemperature);}
  St_tpcCorrectionC *tpcWaterOut()         {return Correction(ktpcWaterOut);}
  St_tpcCorrectionC *TpcPadTBins()         {return Correction(kTpcPadTBins);}
  Int_t Debug()                            {return m_Debug;}
  Int_t Mask()                             {return m_Mask;}
  Float_t           Adc2GeV()              {return mAdc2GeV;}
  void Print(Option_t *opt = "") const;
 private:
  Int_t                m_Mask;                  //!
  St_tpcGas           *m_tpcGas;                //!
  dEdxY2_t            *mdEdx;
  Float_t              mAdc2GeV;                //! Outer/Inner conversion factors from ADC -> GeV
  dEdxCorrection_t     m_Corrections[kTpcAllCorrections];//!
  Int_t                mNumberOfRows;
  Int_t                mNumberOfInnerRows;
  Int_t                m_Debug;                 //!
  TF1                 *f1000;
  TF1                 *f1100;
  TF1                 *f1200;
  TF1                 *f1300;
};
//________________________________________________________________________________
class dEdxY2_t {
 public:
  dEdxY2_t() {}
  virtual ~dEdxY2_t() {}
  /* U->R->S->P->O->Z->X
     U->R (TpcAdcCorrection) -> P (tpcPressure) ->
     S (TpcSecRowB/TpcSecRowC) ->  O (TpcDriftDistOxygen) ->  
     Z (TpcZCorrection) -> X(TpcdXCorrection) */
  Char_t   first[1];
  Int_t    sector;
  Int_t    row;
  Int_t    channel;
  Float_t  pad;
  Int_t    Npads;
  Int_t    Ntbins;
  Float_t  ZdriftDistance;     // drift distance
  Float_t  ZdriftDistanceO2;   // ZdriftDistance*ppmOxygenIn
  Float_t  ZdriftDistanceO2W;  // ZdriftDistance*ppmOxygenIn*ppmWaterOut
  Float_t  DeltaZ;             // distance to privious cluster
  Float_t  QRatio;             // Ratio to previous cluster Charge 
  Float_t  QRatioA;            // Ratio to Sum of all previous cluster Charge 
  Float_t  QSumA;              // Sum of all previous cluster Charge 
  Float_t  dxC;                // corrected dx which should be used with FitN
  Float_t  xyz[3];  // local
  Float_t  xyzD[3]; // local direction
  Float_t  edge;    // distance to sector edge
  Float_t  PhiR;    // relative phi
  Float_t  resXYZ[3]; // track SectorLocal residual wrt local track 
  Float_t  Prob; 
  Float_t  zdev; 
  Float_t  zP;      // the most probable value from Bichsel
  Float_t  zG;      // global z oh Hit
  Float_t  sigmaP;  // sigma from Bichsel
  Float_t  dCharge; // d_undershoot_Q/Q = ratio of modified - original charge normalized on original charge
  Float_t  rCharge; // d_rounding_Q/Q   = estimated rounding normalized on original charge
  Int_t    lSimulated;
  Float_t  Qcm;     // accumulated charge uC/cm
  Float_t  Crow;    // Current per row;
  Float_t  Zdc;     // ZDC rate from trigger
  Float_t  Weight;  // 1/.sigma^2 of TpcSecRow gas gain correction
  Float_t  adc;     //  adc count from cluster finder
  Float_t  TanL;
  Float_t  Voltage; // Anode Voltage
  Float_t  xpad;    // relative position in pad [-1.0,1.0]
  Float_t  yrow;    // relative position in row [-0.5,0.0] inner, and [0.0,0.5] outer
  dE_t     C[StTpcdEdxCorrection::kTpcAllCorrections+1];
  dE_t     F;     //! 
  Char_t   last[1];
  void Reset() {memset(first, 0, last - first);}
}; 
#endif
