// $Id: StTpcdEdxCorrection.h,v 1.1 2004/03/05 17:23:49 fisyak Exp $
#ifndef STAR_StTpcdEdxCorrection
#define STAR_StTpcdEdxCorrection
#include "TObject.h"
#include "Stiostream.h"
#include "tables/St_tpcCorrection_Table.h"
#include "tables/St_tpcGas_Table.h"
#include "tables/St_TpcSecRowCor_Table.h"
#include "tables/St_tpcGas_Table.h"
//#include "tables/St_trigDetSums_Table.h"
//#include "tables/St_tpcGainMonitor_Table.h"
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
  Double_t ZdriftDistance;     // drift distance
  Double_t ZdriftDistanceO2;     // ZdriftDistance*ppmOxygenIn
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
 private:
  St_TpcSecRowCor     *m_TpcSecRow;            //!
  St_tpcCorrection    *m_drift;                //!
  St_tpcCorrection    *m_Multiplicity;         //!
  St_tpcCorrection    *m_AdcCorrection;        //!
  St_tpcCorrection    *m_zCorrection;          //!
  St_tpcCorrection    *m_dXCorrection;         //!
  St_tpcCorrection    *m_TpcdEdxCor;           //!
  St_tpcCorrection    *m_TpcLengthCorrection;  //!
  St_tpcGas           *m_tpcGas;               //!
  St_tpcCorrection    *m_tpcPressure;          //!
  //  St_trigDetSums      *m_trigDetSums;          //!
  //  trigDetSums_st      *m_trig;                 //!
  //  St_tpcGainMonitor   *m_tpcGainMonitor;       //!
 public:
  enum ESector  {kTpcOuter = 0, kTpcInner = 1};

  StTpcdEdxCorrection();
  ~StTpcdEdxCorrection() {}
  Int_t dEdxCorrection(dEdx_t &dEdx, Double_t sign = 1); 
  static Double_t CalcCorrection(const tpcCorrection_st *cor,const Double_t x);
  static Double_t SumSeries(const Double_t &X,const Int_t &N,const Float_t *params);
  static Double_t SumSeries(const Double_t &X,const Int_t &N,const Double_t *params);

  void SetTpcSecRow            (St_TpcSecRowCor     *m = 0);
  void Setdrift                (St_tpcCorrection    *m = 0);
  void SetMultiplicity         (St_tpcCorrection    *m = 0);
  void SetAdcCorrection        (St_tpcCorrection    *m = 0);
  void SetzCorrection          (St_tpcCorrection    *m = 0);
  void SetdXCorrection         (St_tpcCorrection    *m = 0);
  void SetTpcdEdxCor           (St_tpcCorrection    *m = 0);
  void SetTpcLengthCorrection  (St_tpcCorrection    *m = 0);
  void SettpcGas               (St_tpcGas           *m = 0);
  void SettpcPressure          (St_tpcCorrection    *m = 0);
  //  void SettrigDetSums          (St_trigDetSums      *m = 0);
  //  void SettpcGainMonitor       (St_tpcGainMonitor   *m = 0);

  St_TpcSecRowCor   *TpcSecRow()           {return m_TpcSecRow;}
  St_tpcCorrection  *drift()               {return m_drift;}
  St_tpcCorrection  *Multiplicity()        {return m_Multiplicity;}
  St_tpcCorrection  *AdcCorrection()       {return m_AdcCorrection;}
  St_tpcCorrection  *zCorrection()         {return m_zCorrection;}
  St_tpcCorrection  *dXCorrection()        {return m_dXCorrection;}
  St_tpcCorrection  *TpcdEdxCor()          {return m_TpcdEdxCor;}
  St_tpcCorrection  *TpcLengthCorrection() {return m_TpcLengthCorrection;}
  St_tpcGas         *tpcGas()              {return m_tpcGas;}
  St_tpcCorrection  *tpcPressure()         {return m_tpcPressure;}
  //  St_trigDetSums    *trigDetSums()         {return m_trigDetSums;}
  //  St_tpcGainMonitor *tpcGainMonitor()      {return m_tpcGainMonitor;}

  ClassDef(StTpcdEdxCorrection,0)   //StAF chain virtual base class for Makers
};

#endif
