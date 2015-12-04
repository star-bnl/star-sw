// $Id: StTpcdEdxCorrection.h,v 1.6 2013/04/22 19:05:12 fisyak Exp $
#ifndef STAR_StTpcdEdxCorrection
#define STAR_StTpcdEdxCorrection
//
#include "TObject.h"
#include "Stiostream.h"
#include "StDetectorDbMaker/St_tpcCorrectionC.h"
#include "StDetectorDbMaker/St_MDFCorrectionC.h"
#include "tables/St_tpcGas_Table.h"
#include "tables/St_TpcSecRowCor_Table.h"
#include "tables/St_tpcGas_Table.h"
//#include "tables/St_trigDetSums_Table.h"
#include "StTrackPidTraits.h"
//class St_trigDetSums;
//class trigDetSums_st;
//________________________________________________________________________________
struct dE_t {
 public:
  Double_t dE;
  Double_t dEdx;
  Double_t dEdxL;
  Double_t dEdxN;
};
//________________________________________________________________________________
struct dEdxCorrection_t {
  dEdxCorrection_t(const Char_t *name = 0,const  Char_t *title = 0, St_tpcCorrectionC *chair=0, Int_t n=0) 
  {Name = name, Chair = chair; Title = title; nrows = n; dE = 0;} 
  const Char_t *Name;
  const Char_t *Title;
  TChair       *Chair;
  Int_t   nrows;
  Double_t dE;
};
class dEdxY2_t;
//________________________________________________________________________________
class StTpcdEdxCorrection : public TObject {
 public:
  enum ESector  {kTpcOuter = 0, kTpcInner = 1};
  enum EOptions {
    kUncorrected   = 0, //U
    kEdge             , //E correction near edge of chamber
    kAdcCorrection    , //R
    kTpcdCharge       , //D
    kTpcrCharge       , //D
    kTpcCurrentCorrection,
    kTpcRowQ          ,
    kTpcSecRowB       , //S
    kTpcSecRowC       , //S
    ktpcPressure      , //P
    ktpcTime          , //t
    kDrift            , //O
    kMultiplicity     , //M
    kzCorrection      , //Z
    ktpcMethaneIn     , //m
    ktpcGasTemperature, //T
    ktpcWaterOut      , //W 
    kSpaceCharge      , //C space charge near the wire
    kPhiDirection     , //p correction wrt local interception angle 
    kdXCorrection     , //X
    kTpcPadTBins      , //d
    kTpcZDC           ,
    kTpcNoAnodeVGainC ,
    kTpcLast          ,
    kTpcLengthCorrection,
    kTpcLengthCorrectionMDF,
    kTpcdEdxCor       ,
    kTpcAllCorrections
  };
  StTpcdEdxCorrection(Int_t Option=0, Int_t debug=0);
  ~StTpcdEdxCorrection();
  Int_t dEdxCorrection(dEdxY2_t &dEdx, Bool_t doIT=kTRUE); 
  Int_t dEdxTrackCorrection(Int_t type, dst_dedx_st &dedx);
  Int_t dEdxTrackCorrection(EOptions k, Int_t type, dst_dedx_st &dedx);
  void SettpcGas               (St_tpcGas          *m = 0);
  //  void SettrigDetSums          (St_trigDetSums     *m = 0);
  void SetTpcSecRowB           (St_TpcSecRowCor    *m = 0);
  void SetTpcSecRowC           (St_TpcSecRowCor    *m = 0);
  void SetCorrection           (Int_t k = 0, St_tpcCorrection   *m = 0);
  void SetCorrectionMDF        (Int_t k = 0, St_MDFCorrection   *m = 0);
  void Setdrift                (St_tpcCorrection   *m = 0) {SetCorrection (kDrift               , m);}
  void SetMultiplicity         (St_tpcCorrection   *m = 0) {SetCorrection (kMultiplicity        , m);}
  void SetAdcCorrection        (St_tpcCorrection   *m = 0) {SetCorrection (kAdcCorrection       , m);}
  void SetzCorrection          (St_tpcCorrection   *m = 0) {SetCorrection (kzCorrection         , m);}
  void SetdXCorrection         (St_tpcCorrection   *m = 0) {SetCorrection (kdXCorrection        , m);}
  void SetTpcdEdxCor           (St_tpcCorrection   *m = 0) {SetCorrection (kTpcdEdxCor          , m);}
  void SetTpcLengthCorrection  (St_tpcCorrection   *m = 0) {SetCorrection (kTpcLengthCorrection , m);}
  void SettpcPressure          (St_tpcCorrection   *m = 0) {SetCorrection (ktpcPressure         , m);}
  void SettpcMethaneIn         (St_tpcCorrection   *m = 0) {SetCorrection (ktpcMethaneIn        , m);}
  void SettpcGasTemperature    (St_tpcCorrection   *m = 0) {SetCorrection (ktpcGasTemperature   , m);}
  void SettpcWaterOut          (St_tpcCorrection   *m = 0) {SetCorrection (ktpcWaterOut         , m);}
  void SetTpcPadTBins          (St_tpcCorrection   *m = 0) {SetCorrection (kTpcPadTBins         , m);}
  
  void SetDebug(Int_t m=0) {m_Debug = m;}
  void SetMask (Int_t m=0) {m_Mask = m;}
  void ReSetCorrections();

  St_tpcGas         *tpcGas()              {return m_tpcGas;}
  //  St_trigDetSums    *trigDetSums()         {return m_trigDetSums;}

  St_TpcSecRowCor  *TpcSecRowB()           {return m_TpcSecRowB;}
  St_TpcSecRowCor  *TpcSecRowC()           {return m_TpcSecRowC;}

  St_tpcCorrectionC *Correction(Int_t k = 0) {
    return (St_tpcCorrectionC *)((k > kTpcSecRowC && k < kTpcAllCorrections && m_Corrections[k].Chair) ? (m_Corrections[k].Chair) : 0);
  }
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
  Double_t          Adc2GeV()              {return mAdc2GeV;}
  void Print(Option_t *opt = "") const;
 private:
  Int_t                m_Mask;                 //!
  St_tpcGas           *m_tpcGas;               //!
  dEdxY2_t            *mdEdx;
  //  St_trigDetSums      *m_trigDetSums;          //!
  //  trigDetSums_st      *m_trig;                 //!

  St_TpcSecRowCor     *m_TpcSecRowB;            //!
  St_TpcSecRowCor     *m_TpcSecRowC;            //!
  Double_t             mAdc2GeV;               //! Outer/Inner conversion factors from ADC -> GeV
  dEdxCorrection_t     m_Corrections[kTpcAllCorrections];//!
  Int_t                mNumberOfRows;
  Int_t                mNumberOfInnerRows;
  Int_t                m_Debug;                //!
  ClassDef(StTpcdEdxCorrection,0)   //StAF chain virtual base class for Makers
};
//________________________________________________________________________________
class dEdxY2_t : public TObject {
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
  Float_t  pad;
  Int_t    Npads;
  Int_t    Ntbins;
  Double_t ZdriftDistance;     // drift distance
  Double_t ZdriftDistanceO2;   // ZdriftDistance*ppmOxygenIn
  Double_t ZdriftDistanceO2W;  // ZdriftDistance*ppmOxygenIn*ppmWaterOut
  Double_t DeltaZ;             // distance to privious cluster
  Double_t QRatio;             // Ratio to previous cluster Charge 
  Double_t QRatioA;            // Ratio to Sum of all previous cluster Charge 
  Double_t QSumA;              // Sum of all previous cluster Charge 
  Double_t dx;                 // dx with accounting distortions
  Double_t dxC;                // corrected dx which should be used with FitN
  Double_t dE;
  Double_t dEdx;   // after all corrections
  Double_t dEdxL;  // log of dEdx
  Double_t dEdxN;  // normolized to BB
  Double_t xyz[3];  // local
  Double_t xyzD[3]; // local direction
  Double_t edge;    // distance to sector edge
  Double_t PhiR;    // relative phi
  Double_t resXYZ[3]; // track SectorLocal residual wrt local track 
  Double_t Prob; 
  Double_t zdev; 
  Double_t zP;      // the most probable value from Bichsel
  Double_t zG;      // global z oh Hit
  Double_t sigmaP;  // sigma from Bichsel
  Double_t dCharge; // d_undershoot_Q/Q = ratio of modified - original charge normalized on original charge
  Double_t rCharge; // d_rounding_Q/Q   = estimated rounding normalized on original charge
  Int_t    lSimulated;
  Double_t Qcm;     // accumulated charge uC/cm
  Double_t Crow;    // Current per row;
  Double_t Zdc;     // ZDC rate from trigger
  Double_t Weight;  // 1/.sigma^2 of TpcSecRow gas gain correction
  Double_t adc;     //  adc count from cluster finder
  dE_t     C[StTpcdEdxCorrection::kTpcAllCorrections]; //!
  Char_t   last[1];
  void Reset() {memset(first, 0, last - first);}
  ClassDef(dEdxY2_t,1)
}; 

#endif
