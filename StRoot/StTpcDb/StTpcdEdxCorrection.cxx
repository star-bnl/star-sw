#include "StTpcdEdxCorrection.h"
#include "StTpcDb/StTpcDb.h"
#include "StBichsel/Bichsel.h"
#include "StMessMgr.h" 
ClassImp(dEdx_t);
ClassImp(StTpcdEdxCorrection);
//________________________________________________________________________________
StTpcdEdxCorrection::StTpcdEdxCorrection() : m_TpcSecRow(0),m_drift(0),
					     m_Multiplicity(0),m_AdcCorrection(0),
					     m_zCorrection(0),m_dXCorrection (0),
					     m_TpcdEdxCor(0),m_TpcLengthCorrection(0),
					     m_tpcGas(0),m_tpcPressure(0)
					     //,m_trigDetSums(0), m_trig(0),m_tpcGainMonitor(0) 
{}
//________________________________________________________________________________
Double_t StTpcdEdxCorrection::CalcCorrection(const tpcCorrection_st *cor,const Double_t x) {
  Int_t N = TMath::Abs(cor->npar);
  Double_t X = x;
  if (cor->npar  < 0) X = TMath::Exp(x);
  if (N > 0) {
    if (cor->min < cor->max) {
      if (X < cor->min) X = cor->min;
      if (X > cor->max) X = cor->max;
    }
    return SumSeries(X,N,&cor->a[0]);
  }
  else return 0;
}
//________________________________________________________________________________
Double_t StTpcdEdxCorrection::SumSeries(const Double_t &X,const Int_t &N,const Float_t *params) {
  Double_t Sum = 0;
  if (N > 0) {
    Sum = params[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + params[n];
  }
  return Sum;
}
//________________________________________________________________________________
Double_t StTpcdEdxCorrection::SumSeries(const Double_t &X,const Int_t &N,const Double_t *params) {
  Double_t Sum = 0;
  if (N > 0) {
    Sum = params[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + params[n];
  }
  return Sum;
}//________________________________________________________________________________
Int_t  StTpcdEdxCorrection::dEdxCorrection(dEdx_t &CdEdx, Double_t sign) { 
  // sign of correction +1 do correction
  //                    -1 undo correction         
  Int_t iok = -1;
  Double_t dEU = CdEdx.dE;
  Double_t dE  = dEU;
  Double_t dER = dE;
  Double_t dEP = dE;
  Double_t dET = dE;
  Double_t dEO = dE;
  Double_t dES = dE;
  Double_t dEZ = dE;
  Double_t dEX = dE;
  Double_t dEM = dE;
  ESector kTpcOutIn = kTpcOuter;
  if (CdEdx.row <= 13) kTpcOutIn = kTpcInner;
  Double_t TimeScale = 1;
  Double_t PressureScaleI = 1;
  Double_t PressureScaleO = 1;
  if (m_tpcGas && m_tpcPressure && m_tpcPressure->GetNRows() >= 2) {
    Double_t LogPressure = TMath::Log((*m_tpcGas)[0].barometricPressure);
    tpcCorrection_st *cor = m_tpcPressure->GetTable();
    PressureScaleO = TMath::Exp(-sign*CalcCorrection(cor,LogPressure));
    PressureScaleI = TMath::Exp(-sign*CalcCorrection(cor+1,LogPressure));
  }
  if (m_AdcCorrection) {
    Int_t nc = m_AdcCorrection->GetNRows() - 2;
    tpcCorrection_st *cor = m_AdcCorrection->GetTable();
    if (nc >= 0) {
      // correction using positive tracks with built it drift time correction
      // x:x*pow(10.,mu+7.6e-7*y); x = predicted; y = DriftLength*ppOx
      dE = 1.e-6*CalcCorrection(cor+nc+kTpcOutIn,1.e6*dE);
    }
  }
  dER = dE;
  Double_t gc = 1;
  if (m_TpcSecRow) {
    TpcSecRowCor_st *gain = m_TpcSecRow->GetTable() + CdEdx.sector - 1;
    gc =  gain->GainScale[CdEdx.row-1];
    CdEdx.SigmaFee = gain->GainRms[CdEdx.row-1];
  }
  if (gc < 0.0) return iok;;
  dE *= gc;
  dES = dE;
  dE *= CdEdx.row > 13 ? PressureScaleO : PressureScaleI; 
  dEP = dE;
  dE = dE*TimeScale;
  dET = dE;
  dEM = dE;
  if (m_drift) {// Blair correction 
    tpcCorrection_st *cor = m_drift->GetTable();
    //	    Double_t DriftCorr = cor->a[1]*(CdEdx.ZdriftDistanceO2 - cor->a[0]);
    //	    dE *= TMath::Exp(-DriftCorr);
    dE *= TMath::Exp(-sign*CalcCorrection(cor+kTpcOutIn,CdEdx.ZdriftDistanceO2));
  }
  dEO = dE;
  if (m_zCorrection) {
    tpcCorrection_st *cor = m_zCorrection->GetTable()+kTpcOutIn;
    if (cor->min > 0 && cor->min > CdEdx.ZdriftDistance) return iok;;
    if (cor->max > 0 && CdEdx.ZdriftDistance > cor->max) return iok;;
    dE *= TMath::Exp(-sign*CalcCorrection(cor,CdEdx.ZdriftDistance));
  }
  dEZ = dE;
  if (m_dXCorrection) {
    tpcCorrection_st *cor = m_dXCorrection->GetTable();
    Int_t N = m_dXCorrection->GetNRows();
    Double_t xL2 = TMath::Log2(CdEdx.dx);
    Double_t dXCorr = CalcCorrection(cor+kTpcOutIn,xL2); 
    if (N > 2) dXCorr += CalcCorrection(cor+2,xL2);
    if (N > 6) dXCorr += CalcCorrection(cor+5+kTpcOutIn,xL2);
    dE *= TMath::Exp(-sign*dXCorr);
  }
  dEX = dE;
  if (dE <= 0 || CdEdx.dx <= 0) return iok;;
  CdEdx.dE      = dE; // corrected
  CdEdx.dEU     = dEU; // uncorrected
  CdEdx.dER     = dER; // row correction
  CdEdx.dEM     = dEM; // Multiplicity correction
  CdEdx.dEP     = dEP; // Pressure
  CdEdx.dET     = dET; // Time 
  CdEdx.dEO     = dEO; // Blair O2 
  CdEdx.dES     = dES; // SecRow 
  CdEdx.dEZ     = dEZ; // Drift Distance
  CdEdx.dEX     = dEX; // dE correction
  CdEdx.dEdx    = CdEdx.dE/CdEdx.dx;
  CdEdx.dEdxL   = TMath::Log(CdEdx.dEdx);
  CdEdx.dEUdx   = CdEdx.dEU/CdEdx.dx;
  CdEdx.dEUdxL  = TMath::Log(CdEdx.dEUdx);
  CdEdx.dERdx   = CdEdx.dER/CdEdx.dx;
  CdEdx.dERdxL  = TMath::Log(CdEdx.dERdx);
  CdEdx.dEPdx   = CdEdx.dEP/CdEdx.dx;
  CdEdx.dEPdxL  = TMath::Log(CdEdx.dEPdx);
  CdEdx.dETdx   = CdEdx.dET/CdEdx.dx;
  CdEdx.dEdxLT  = TMath::Log(CdEdx.dETdx);
  CdEdx.dEOdx   = CdEdx.dEO/CdEdx.dx;
  CdEdx.dEdxLO  = TMath::Log(CdEdx.dEOdx);
  CdEdx.dESdx   = CdEdx.dES/CdEdx.dx;
  CdEdx.dEdxLS  = TMath::Log(CdEdx.dESdx);
  CdEdx.dEZdx   = CdEdx.dEZ/CdEdx.dx;
  CdEdx.dEZdxL  = TMath::Log(CdEdx.dEZdx);
  CdEdx.dEZdx   = CdEdx.dEZ/CdEdx.dx;
  CdEdx.dEXdx   = CdEdx.dEX/CdEdx.dx;
  CdEdx.dEXdxL  = TMath::Log(CdEdx.dEXdx);
  CdEdx.dEMdx   = CdEdx.dEM/CdEdx.dx;
  CdEdx.dEMdxL  = TMath::Log(CdEdx.dEMdx);
  iok = 0;
  return iok;
}
#define PrPP(A,B) if (B) gMessMgr->Warning() << "StTpcdEdxCorrection::" << (#A) << " correction has been requested" << endm;
void StTpcdEdxCorrection::SetTpcSecRow            (St_TpcSecRowCor     *m) {PrPP(TpcSecRow          ,m); m_TpcSecRow = m;}            
void StTpcdEdxCorrection::Setdrift                (St_tpcCorrection    *m) {PrPP(drift              ,m); m_drift = m;}                
void StTpcdEdxCorrection::SetMultiplicity         (St_tpcCorrection    *m) {PrPP(Multiplicity       ,m); m_Multiplicity = m;}         
void StTpcdEdxCorrection::SetAdcCorrection        (St_tpcCorrection    *m) {PrPP(AdcCorrection      ,m); m_AdcCorrection = m;}        
void StTpcdEdxCorrection::SetzCorrection          (St_tpcCorrection    *m) {PrPP(zCorrection        ,m); m_zCorrection = m;}          
void StTpcdEdxCorrection::SetdXCorrection         (St_tpcCorrection    *m) {PrPP(dXCorrection       ,m); m_dXCorrection  = m;}        
void StTpcdEdxCorrection::SetTpcdEdxCor           (St_tpcCorrection    *m) {PrPP(TpcdEdxCor         ,m); m_TpcdEdxCor = m;}           
void StTpcdEdxCorrection::SetTpcLengthCorrection  (St_tpcCorrection    *m) {PrPP(TpcLengthCorrection,m); m_TpcLengthCorrection = m;}  
void StTpcdEdxCorrection::SettpcGas               (St_tpcGas           *m) {PrPP(tpcGas             ,m); m_tpcGas = m;}               
void StTpcdEdxCorrection::SettpcPressure          (St_tpcCorrection    *m) {PrPP(tpcPressure        ,m); m_tpcPressure = m;}          
//void StTpcdEdxCorrection::SettrigDetSums          (St_trigDetSums      *m = 0) {PrPP(SettrigDetSums        ,m); m_trigDetSums = m;}
//void StTpcdEdxCorrection::SettpcGainMonitor       (St_tpcGainMonitor   *m = 0) {PrPP(SettpcGainMonitor     ,m); m_tpcGainMonitor = m;}
#undef PrPP(A,B)
