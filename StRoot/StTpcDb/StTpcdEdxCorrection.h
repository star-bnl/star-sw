// $Id: StTpcdEdxCorrection.h,v 1.5 2004/07/29 22:29:25 fisyak Exp $
#ifndef STAR_StTpcdEdxCorrection
#define STAR_StTpcdEdxCorrection
//
#include "TObject.h"
#include "Stiostream.h"
#include "St_tpcCorrectionC.h"
#include "tables/St_tpcGas_Table.h"
#include "tables/St_TpcSecRowCor_Table.h"
#include "tables/St_tpcGas_Table.h"
//#include "tables/St_trigDetSums_Table.h"
//#include "tables/St_tpcGainMonitor_Table.h"
#include "tables/St_dst_dedx_Table.h"
//class St_trigDetSums;
//class trigDetSums_st;
#if 1
struct dE_t {
 public:
  Double_t dE;
  Double_t dEdx;
  Double_t dEdxL;
  Double_t dEdxN;
};
#endif
//________________________________________
class dEdx_t : public TObject {
 public:
  dEdx_t() {}
  virtual ~dEdx_t() {}
  /* U->R->S->P->O->Z->X
     U->R (TpcAdcCorrection) -> P (tpcPressure) ->
     S (TpcSecRowB) ->  O (TpcDriftDistOxygen) ->  
     Z (TpcZCorrection) -> X(TpcdXCorrection) */
  Int_t    sector;
  Int_t    row;
  Int_t    pad;
  Int_t    Npads;
  Int_t    Ntbins;
  Double_t ZdriftDistance;     // drift distance
  Double_t ZdriftDistanceO2;     // ZdriftDistance*ppmOxygenIn
  Double_t ZdriftDistanceO2W;     // ZdriftDistance*ppmOxygenIn*ppmWaterOut
  Double_t DeltaZ;            // distance to privious cluster
  Double_t QRatio;            // Ratio to previous cluster Charge 
  Double_t dx;
#if 0
  Double_t dx0;    // stright line approximation
#endif
  Double_t dE;
  Double_t dEdx;   // after all corrections
  Double_t dEdxL;  // log of dEdx
  Double_t dEdxN;  // normolized to BB
#if 1
  dE_t     C[15]; //!
#else
  Double_t dEU;    // before correction (only Scale2keV scale)
  Double_t dEUdx; 
  Double_t dEUdxL; // log of dEdx
  Double_t dEUdxN;  // normolized to BB
  Double_t dER;    // after row correction
  Double_t dERdx;    // after row correction
  Double_t dERdxL;    // after row correction
  Double_t dERdxN; 
  Double_t dEP;    // after Pressure correction
  Double_t dEPdx; 
  Double_t dEPdxN; 
  Double_t dEPdxL; 
  Double_t dET;    // after TimeScale
  Double_t dETdx; 
  Double_t dEdxLT; 
  Double_t dETdxN;  // normolized to BB
  Double_t dEO;    // after Blair correction
  Double_t dEOdx; 
  Double_t dEdxLO; 
  Double_t dEOdxN;  // normolized to BB
  Double_t dES;    // after TimeScale + SecRow corrections
  Double_t dESdx; 
  Double_t dEdxLS; 
  Double_t dESdxN;  // normolized to BB
  Double_t dEZ;    // after TimeScale + SecRow + Sec Z corrections
  Double_t dEZdx; 
  Double_t dEZdxL; 
  Double_t dEZdxN;  // normolized to BB
  Double_t dED;    // after + Accamulated charge correction
  Double_t dEDdx; 
  Double_t dEDdxL; 
  Double_t dEDdxN;  // normolized to BB
  Double_t dEX;    //  dX corrections
  Double_t dEXdx; 
  Double_t dEXdxL; 
  Double_t dEXdxN;  // normolized to BB
  Double_t dEM;     // after TimeScale + SecRow + Sec Z + Multipicity 
  Double_t dEMdx; 
  Double_t dEMdxL; 
  Double_t dEMdxN;  // normolized to BB
#endif  
  Double_t dETot; 
  Double_t xyz[3];  // local
  Double_t Prob; 
  Double_t SigmaFee;
  Double_t xscale;
  Double_t dEIpad;  // total charge integrated so far in the pad
  Double_t dEI3pad; // total charge integrated so far in the pad +/-
  Double_t dEIrow;  // total charge integrated so far in the row
  Double_t dETrow;  // total charge not integrated (time bucket only) in the row
  Double_t dET3row; // total charge not integrated (+0 + 2 time buckets only) in the row
  Double_t dET5row; // total charge not integrated (+0 + 4 time buckets only) in the row
  Double_t zdev; 
  Double_t zP;      // the most probable value from Bichsel
  Double_t sigmaP;  // sigma from Bichsel
  Double_t dCharge; //
  ClassDef(dEdx_t,1)
}; 
struct dEdxCorrection_t {
  dEdxCorrection_t(Char_t *name = 0, Char_t *title = 0, St_tpcCorrectionC *chair=0, Int_t n=0) 
  {Name = name, Chair = chair; Title = title; nrows = n; dE = 0;} 
  Char_t *Name;
  Char_t *Title;
  St_tpcCorrectionC *Chair;
  Int_t   nrows;
  Double_t dE;
};
class StTpcdEdxCorrection : public TObject {
 public:
  enum ESector  {kTpcOuter = 0, kTpcInner = 1};
  enum EOptions {
    kUncorrected         , //U
    kAdcCorrection       , //R
    kTpcSecRow           , //S
    ktpcPressure         , //P
    ktpcTime             , //T
    kDrift               , //O
    kMultiplicity        , //M
    kzCorrection         , //Z
    ktpcMethaneIn        ,
    ktpcGasTemperature   ,
    ktpcWaterOut         ,
    kTpcdCharge          , //D
    kSpaceCharge         , // space charge near the wire
    kTpcLast             ,
    kdXCorrection        , //X
    kTpcPadTBins         ,
    kTpcLengthCorrection ,
    kTpcdEdxCor          ,
    kTpcAllCorrections
  };
  StTpcdEdxCorrection(Int_t Option=0, Int_t debug=0);
  ~StTpcdEdxCorrection();
  Int_t dEdxCorrection(dEdx_t &dEdx); 
  Int_t dEdxTrackCorrection(EOptions k, Int_t type, dst_dedx_st &dedx);
  void SettpcGas               (St_tpcGas          *m = 0);
  //  void SettrigDetSums          (St_trigDetSums     *m = 0);
  //  void SettpcGainMonitor       (St_tpcGainMonitor  *m = 0);
  void SetTpcSecRow            (St_TpcSecRowCor    *m = 0);
  void SetCorrection           (Int_t k = 0, St_tpcCorrection   *m = 0);
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

  St_tpcGas         *tpcGas()              {return m_tpcGas;}
  //  St_trigDetSums    *trigDetSums()         {return m_trigDetSums;}
  //  St_tpcGainMonitor *tpcGainMonitor()      {return m_tpcGainMonitor;}

  St_TpcSecRowCor   *TpcSecRow()           {return m_TpcSecRow;}

  St_tpcCorrectionC *Correction(Int_t k = 0) {
    return (k > kTpcSecRow && k < kTpcAllCorrections && m_Corrections[k].Chair) ? m_Corrections[k].Chair : 0;
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
 private:
  Int_t                m_Mask;                 //!
  St_tpcGas           *m_tpcGas;               //!
  //  St_trigDetSums      *m_trigDetSums;          //!
  //  trigDetSums_st      *m_trig;                 //!
  //  St_tpcGainMonitor   *m_tpcGainMonitor;       //!

  St_TpcSecRowCor     *m_TpcSecRow;            //!

  dEdxCorrection_t     m_Corrections[kTpcAllCorrections];//!
  Double_t             mAdc2GeV[2];            //! Outer/Inner conversion factors from ADC -> GeV
  Int_t                m_Debug;                //!
  ClassDef(StTpcdEdxCorrection,0)   //StAF chain virtual base class for Makers
};

#endif
