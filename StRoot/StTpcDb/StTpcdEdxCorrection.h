// $Id: StTpcdEdxCorrection.h,v 1.4 2004/07/21 14:13:01 fisyak Exp $
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
class St_trigDetSums;
class trigDetSums_st;
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
  Double_t dE;
  Double_t dx;
  Double_t dx0;    // stright line approximation
  Double_t dEdx;   // after all corrections
  Double_t dEdxL;  // log of dEdx
  Double_t dEdxN;  // normolized to BB
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
  Double_t dEX;    //  dX corrections
  Double_t dEXdx; 
  Double_t dEXdxL; 
  Double_t dEXdxN;  // normolized to BB
  Double_t dEM;     // after TimeScale + SecRow + Sec Z + Multipicity 
  Double_t dEMdx; 
  Double_t dEMdxL; 
  Double_t dEMdxN;  // normolized to BB
  Double_t dETot; 
  Double_t xyz[3];
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
  ClassDef(dEdx_t,1)
}; 

class StTpcdEdxCorrection : public TObject {
 public:
  enum ESector  {kTpcOuter = 0, kTpcInner = 1};
  enum EOptions {
    kTpcSecRow           ,
    kDrift               ,
    kMultiplicity        ,
    kAdcCorrection       ,
    kzCorrection         ,
    kdXCorrection        ,
    kTpcdEdxCor          ,
    kTpcLengthCorrection ,
    ktpcGas              ,
    ktpcPressure         ,
    ktpcMethaneIn        ,
    ktpcGasTemperature   ,
    ktpcWaterOut         ,
    kTpcPadTBins
  };
    //  ktrigDetSums      ,
    //  ktpcGainMonitor   ,
  StTpcdEdxCorrection(Int_t Option=0, Int_t debug=0);
  ~StTpcdEdxCorrection();
  Int_t dEdxCorrection(dEdx_t &dEdx); 

  void SetTpcSecRow            (St_TpcSecRowCor    *m = 0);
  void Setdrift                (St_tpcCorrection   *m = 0);
  void SetMultiplicity         (St_tpcCorrection   *m = 0);
  void SetAdcCorrection        (St_tpcCorrection   *m = 0);
  void SetzCorrection          (St_tpcCorrection   *m = 0);
  void SetdXCorrection         (St_tpcCorrection   *m = 0);
  void SetTpcdEdxCor           (St_tpcCorrection   *m = 0);
  void SetTpcLengthCorrection  (St_tpcCorrection   *m = 0);
  void SettpcGas               (St_tpcGas          *m = 0);
  void SettpcPressure          (St_tpcCorrection   *m = 0);
  void SettpcMethaneIn         (St_tpcCorrection   *m = 0);
  void SettpcGasTemperature         (St_tpcCorrection   *m = 0);
  void SettpcWaterOut         (St_tpcCorrection   *m = 0);
  void SetTpcPadTBins         (St_tpcCorrection   *m = 0);
  //  void SettrigDetSums          (St_trigDetSums      *m = 0);
  //  void SettpcGainMonitor       (St_tpcGainMonitor   *m = 0);
  
  void SetDebug(Int_t m=0) {m_Debug = m;}

  St_TpcSecRowCor   *TpcSecRow()           {return m_TpcSecRow;}
  St_tpcCorrectionC *drift()               {return m_drift;}
  St_tpcCorrectionC *Multiplicity()        {return m_Multiplicity;}
  St_tpcCorrectionC *AdcCorrection()       {return m_AdcCorrection;}
  St_tpcCorrectionC *zCorrection()         {return m_zCorrection;}
  St_tpcCorrectionC *dXCorrection()        {return m_dXCorrection;}
  St_tpcCorrectionC *TpcdEdxCor()          {return m_TpcdEdxCor;}
  St_tpcCorrectionC *TpcLengthCorrection() {return m_TpcLengthCorrection;}
  St_tpcGas         *tpcGas()              {return m_tpcGas;}
  St_tpcCorrectionC *tpcPressure()         {return m_tpcPressure;}
  St_tpcCorrectionC *tpcMethaneIn()        {return m_tpcMethaneIn;}
  St_tpcCorrectionC *tpcGasTemperature()        {return m_tpcGasTemperature;}
  St_tpcCorrectionC *tpcWaterOut()        {return m_tpcWaterOut;}
  St_tpcCorrectionC *TpcPadTBins()        {return m_TpcPadTBins;}
  //  St_trigDetSums    *trigDetSums()     {return m_trigDetSums;}
  //  St_tpcGainMonitor *tpcGainMonitor()  {return m_tpcGainMonitor;}
  Int_t Debug()                            {return m_Debug;}
 private:
  Int_t                m_Mask;                  //!
  St_TpcSecRowCor     *m_TpcSecRow;            //!
  St_tpcCorrectionC   *m_drift;                //!
  St_tpcCorrectionC   *m_Multiplicity;         //!
  St_tpcCorrectionC   *m_AdcCorrection;        //!
  St_tpcCorrectionC   *m_zCorrection;          //!
  St_tpcCorrectionC   *m_dXCorrection;         //!
  St_tpcCorrectionC   *m_TpcdEdxCor;           //!
  St_tpcCorrectionC   *m_TpcLengthCorrection;  //!
  St_tpcGas           *m_tpcGas;               //!
  St_tpcCorrectionC   *m_tpcPressure;          //!
  St_tpcCorrectionC   *m_tpcMethaneIn;         //!
  St_tpcCorrectionC   *m_tpcGasTemperature;    //!
  St_tpcCorrectionC   *m_tpcWaterOut;          //!
  St_tpcCorrectionC   *m_TpcPadTBins;          //!
  St_trigDetSums      *m_trigDetSums;          //!
  trigDetSums_st      *m_trig;                 //!
  //  St_tpcGainMonitor   *m_tpcGainMonitor;       //!
  Double_t             mAdc2GeV[2];            //! Outer/Inner conversion factors from ADC -> GeV
  Int_t                m_Debug;                //!
  ClassDef(StTpcdEdxCorrection,0)   //StAF chain virtual base class for Makers
};

#endif
