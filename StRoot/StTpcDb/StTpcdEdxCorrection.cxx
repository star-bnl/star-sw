/*! 
  \class dEdx_t
  dEdx_t class contains data for each cluster which is used during different calibartion steps
  \class StTpcdEdxCorrection
  StTpcdEdxCorrection class contains utilities  
*/
#include "StMaker.h"
#include "StTpcdEdxCorrection.h"
#include "StTpcDb/StTpcDb.h"
#include "StBichsel/Bichsel.h"
#include "StMessMgr.h" 
#include "tables/St_tss_tsspar_Table.h"
ClassImp(dEdx_t);
ClassImp(StTpcdEdxCorrection);
//________________________________________________________________________________
StTpcdEdxCorrection::StTpcdEdxCorrection(Int_t option, Int_t debug) : 
  m_Mask(option), m_TpcSecRow(0),m_drift(0),
  m_Multiplicity(0),m_AdcCorrection(0),
  m_zCorrection(0),m_dXCorrection (0),
  m_TpcdEdxCor(0),m_TpcLengthCorrection(0),
  m_tpcGas(0),m_tpcPressure(0), m_tpcMethaneIn(0), 
  m_tpcGasTemperature(0), m_tpcWaterOut(0), m_TpcPadTBins(0), 
  m_trigDetSums(0), m_trig(0),//,m_tpcGainMonitor(0)  
  m_Debug(debug)
{
  assert(gStTpcDb);
  if (!m_Mask) {
    SETBIT(m_Mask,ktpcPressure); 
    SETBIT(m_Mask,ktpcMethaneIn); 
    SETBIT(m_Mask,ktpcGasTemperature); 
    SETBIT(m_Mask,ktpcWaterOut); 
    SETBIT(m_Mask,kTpcPadTBins); 
    SETBIT(m_Mask,kAdcCorrection); 
    SETBIT(m_Mask,kTpcSecRow); 
    SETBIT(m_Mask,kDrift);
    SETBIT(m_Mask,kzCorrection);
    SETBIT(m_Mask,kdXCorrection);
    SETBIT(m_Mask,kTpcdEdxCor);
    SETBIT(m_Mask,kTpcLengthCorrection);
  }
  TDataSet *tpctss  = StMaker::GetChain()->GetDataBase("tpc/tsspars"); assert(tpctss);
  St_tss_tsspar *TssPar = (St_tss_tsspar *) tpctss->Find("tsspar");
  tss_tsspar_st *tsspar = TssPar->GetTable();
  mAdc2GeV[0] = tsspar->ave_ion_pot * tsspar->scale /(tsspar->gain_out*tsspar->wire_coupling_out);
  mAdc2GeV[1] = tsspar->ave_ion_pot * tsspar->scale /(tsspar->gain_in *tsspar->wire_coupling_in );
  TDataSet *tpc_calib  = StMaker::GetChain()->GetDataBase("Calibrations/tpc"); assert(tpc_calib);
  St_tpcGas *tpcGas = (St_tpcGas *) tpc_calib->Find("tpcGas");
  if (!tpcGas) {cout << "=== tpcGas is missing ===" << endl; assert(tpcGas);}
  else {
    assert(tpcGas->GetNRows());
    SettpcGas(tpcGas);
  }
  if (TESTBIT(m_Mask,ktpcPressure)) {
    gMessMgr->Warning() << "StTpcdEdxCorrection: Pressure Scale Factor is ON" << endm;
    St_tpcCorrection *tpcPressure = (St_tpcCorrection *) tpc_calib->Find("tpcPressureB");
    if (!tpcPressure) {
      cout << "=== tpcPressure is missing ===" << endl;
    }
    assert (tpcPressure);
    assert(tpcPressure->GetNRows());
    SettpcPressure(tpcPressure);
    if (TESTBIT(m_Mask,ktpcMethaneIn)) {
      gMessMgr->Warning() << "StTpcdEdxCorrection: tpcMethaneIn is ON" << endm;
      St_tpcCorrection *tpcMethaneIn = (St_tpcCorrection *) tpc_calib->Find("tpcMethaneIn");
      if (!tpcMethaneIn) {
	cout << "=== tpcMethaneIn is missing ===" << endl;
      }
      SettpcMethaneIn(tpcMethaneIn);
    }
    if (TESTBIT(m_Mask,ktpcGasTemperature)) {
      gMessMgr->Warning() << "StTpcdEdxCorrection: tpcGasTemperature is ON" << endm;
      St_tpcCorrection *tpcGasTemperature = (St_tpcCorrection *) tpc_calib->Find("tpcGasTemperature");
      if (!tpcGasTemperature) {
	cout << "=== tpcGasTemperature is missing ===" << endl;
      }
      SettpcGasTemperature(tpcGasTemperature);
    }
    if (TESTBIT(m_Mask,ktpcWaterOut)) {
      gMessMgr->Warning() << "StTpcdEdxCorrection: tpcWaterOut is ON" << endm;
      St_tpcCorrection *tpcWaterOut = (St_tpcCorrection *) tpc_calib->Find("tpcWaterOut");
      if (!tpcWaterOut) {
	cout << "=== tpcWaterOut is missing ===" << endl;
      }
      SettpcWaterOut(tpcWaterOut);
    }
    if (TESTBIT(m_Mask,kTpcPadTBins)) {
      gMessMgr->Warning() << "StTpcdEdxCorrection: TpcPadTBins is ON" << endm;
      St_tpcCorrection *TpcPadTBins = (St_tpcCorrection *) tpc_calib->Find("TpcPadTBins");
      if (!TpcPadTBins) {
	cout << "=== TpcPadTBins is missing ===" << endl;
      }
      SetTpcPadTBins(TpcPadTBins);
    }
  }
  if (TESTBIT(m_Mask,kDrift)) {
    gMessMgr->Warning() << "StTpcdEdxCorrection: Drift Distance Correction is ON" << endm;
    St_tpcCorrection *drift = (St_tpcCorrection *) tpc_calib->Find("TpcDriftDistOxygen"); 
    assert(drift); 
    assert(drift->GetNRows());
    Setdrift(drift);
  } // DriftDistanceCorrection
  if (TESTBIT(m_Mask,kAdcCorrection)) {
    gMessMgr->Warning() << "StTpcdEdxCorrection: Adc Correction is ON" << endm;
    St_tpcCorrection *AdcCorrection = (St_tpcCorrection *) tpc_calib->Find("TpcAdcCorrection"); 
    assert(AdcCorrection); 
    assert(AdcCorrection->GetNRows());
    SetAdcCorrection(AdcCorrection);
  } // ADCorrection
  if (TESTBIT(m_Mask,kzCorrection)) {
    gMessMgr->Warning() << "StTpcdEdxCorrection: z Correction is ON" << endm;
    St_tpcCorrection *zCorrection = (St_tpcCorrection *) tpc_calib->Find("TpcZCorrection"); 
    if (! zCorrection) cout << "=== TpcZCorrection is missing ===" << endl;
    else assert(zCorrection->GetNRows());
    SetzCorrection(zCorrection);
  }
  if (TESTBIT(m_Mask,kdXCorrection)) {
    gMessMgr->Warning() << "StTpcdEdxCorrection: dX Correction is ON" << endm;
    St_tpcCorrection *dXCorrection = (St_tpcCorrection *) tpc_calib->Find("TpcdXCorrection"); 
    if (! dXCorrection) cout << "=== TpcdXCorrection is missing ===" << endl;
    else assert(dXCorrection->GetNRows());
    SetdXCorrection(dXCorrection);
  }
  if (TESTBIT(m_Mask,kTpcdEdxCor)) {
    gMessMgr->Warning() << "StTpcdEdxCorrection: dE/dX Correction is ON" << endm;
    St_tpcCorrection *TpcdEdxCor = (St_tpcCorrection *) tpc_calib->Find("TpcdEdxCor"); 
    if (! TpcdEdxCor) cout << "=== TpcdEdxCor is missing ===" << endl;
    else assert(TpcdEdxCor->GetNRows());
    SetTpcdEdxCor(TpcdEdxCor);
  }
  if (TESTBIT(m_Mask,kTpcLengthCorrection)) {
    gMessMgr->Warning() << "StTpcdEdxCorrection: TPC Track Length Correction is ON" << endm;
    St_tpcCorrection *TpcLengthCorrection = (St_tpcCorrection *) tpc_calib->Find("TpcLengthCorrection"); 
    if (! TpcLengthCorrection) cout << "=== TpcLengthCorrection is missing ===" << endl;
    else assert(TpcLengthCorrection->GetNRows());
    SetTpcLengthCorrection(TpcLengthCorrection);
  }
  if (TESTBIT(m_Mask,kTpcSecRow)) {
    gMessMgr->Warning() << "StTpcdEdxCorrection: Tpc Sector Row Correction is ON" << endm;
    St_TpcSecRowCor *TpcSecRow  = (St_TpcSecRowCor *) tpc_calib->Find("TpcSecRowB"); assert(TpcSecRow); 
    SetTpcSecRow(TpcSecRow);
  }
  
}
//________________________________________________________________________________
StTpcdEdxCorrection::~StTpcdEdxCorrection() {
  SafeDelete(m_drift);
  SafeDelete(m_Multiplicity);
  SafeDelete(m_AdcCorrection);
  SafeDelete(m_zCorrection);
  SafeDelete(m_dXCorrection);
  SafeDelete(m_TpcdEdxCor);
  SafeDelete(m_TpcLengthCorrection);
  SafeDelete(m_tpcPressure);
  SafeDelete(m_tpcMethaneIn);
  SafeDelete(m_tpcGasTemperature);
  SafeDelete(m_tpcWaterOut);
  SafeDelete(m_TpcPadTBins);
}
//________________________________________________________________________________
Int_t  StTpcdEdxCorrection::dEdxCorrection(dEdx_t &CdEdx) { 
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
  Int_t sector            = CdEdx.sector; 
  Int_t row       	  = CdEdx.row;   
  Double_t dx     	  = CdEdx.dx;    
#if 0
  Int_t pad       	  = CdEdx.pad;   
  Double_t dx0    	  = CdEdx.dx0;   
  Double_t x      	  = CdEdx.xyz[0];
  Double_t y      	  = CdEdx.xyz[1];
  Double_t z      	  = CdEdx.xyz[2];
#endif
  Double_t ZdriftDistance = CdEdx.ZdriftDistance;
  ESector kTpcOutIn = kTpcOuter;
  if (row <= 13) kTpcOutIn = kTpcInner;
  Double_t TimeScale = 1;
  Double_t PressureScale = 1;
  
  if (m_tpcGas) {
    tpcGas_st *gas = m_tpcGas->GetTable();
    if (m_tpcPressure && m_tpcPressure->GetNRows() >= 2) {
      Double_t LogPressure = TMath::Log(gas->barometricPressure);
      PressureScale = TMath::Exp(-m_tpcPressure->CalcCorrection(kTpcOutIn,LogPressure));
      if (m_tpcMethaneIn) {
	Double_t percentMethaneIn = gas->percentMethaneIn;
	PressureScale *= 	TMath::Exp(-m_tpcMethaneIn->CalcCorrection(kTpcOuter,percentMethaneIn));
      }
      if (m_tpcGasTemperature) {
	Double_t outputGasTemperature = gas->outputGasTemperature;
	PressureScale *= 	TMath::Exp(-m_tpcGasTemperature->CalcCorrection(kTpcOuter,outputGasTemperature));
      }
      if (m_tpcWaterOut) {
	Double_t ppmWaterOut = gas->ppmWaterOut;
	PressureScale *= 	TMath::Exp(-m_tpcWaterOut->CalcCorrection(kTpcOuter,ppmWaterOut));
      }
    }
  }
  if (m_AdcCorrection) {
    Double_t ADC = dE/mAdc2GeV[kTpcOutIn];
    dE = mAdc2GeV[kTpcOutIn]*m_AdcCorrection->CalcCorrection(kTpcOutIn,ADC);
  }
  if (m_TpcPadTBins) {
    dE *= TMath::Exp(-m_TpcPadTBins->CalcCorrection(kTpcOutIn,CdEdx.Npads*CdEdx.Ntbins));
  }
  dER = dE;
  Double_t gc = 1;
  if (m_TpcSecRow) {
    TpcSecRowCor_st *gain = m_TpcSecRow->GetTable() + sector - 1;
    gc =  gain->GainScale[row-1];
    CdEdx.SigmaFee = gain->GainRms[row-1];
  }
  if (gc <= 0.0) return 1;;
  dE *= gc;
  dES = dE;
  dE *= PressureScale; 
  dEP = dE;
  dE = dE*TimeScale;
  dET = dE;
  dEM = dE;
  Double_t ZdriftDistanceO2 = 0;
  Double_t ZdriftDistanceO2W = 0;
  if (m_tpcGas) {
    ZdriftDistanceO2 = ZdriftDistance*(*m_tpcGas)[0].ppmOxygenIn;
    ZdriftDistanceO2W = ZdriftDistanceO2*(*m_tpcGas)[0].ppmWaterOut;
    if (m_drift) {// Blair correction 
      //	    Double_t DriftCorr = cor->a[1]*(ZdriftDistanceO2 - cor->a[0]);
      //	    dE *= TMath::Exp(-DriftCorr);
      dE *= TMath::Exp(-m_drift->CalcCorrection(kTpcOutIn,ZdriftDistanceO2));
    }
  }
  dEO = dE;
  if (m_zCorrection) {
    Int_t kOffSet = kTpcOutIn;
    tpcCorrection_st *cor = ((St_tpcCorrection *)  m_zCorrection->Table())->GetTable();
    if (cor->nrows == 45) kOffSet = row - 1;
    cor += kOffSet;
    if (cor->min > 0 && cor->min > ZdriftDistance) return 2;;
    if (cor->max > 0 && ZdriftDistance > cor->max) return 2;;
    dE *= TMath::Exp(-m_zCorrection->CalcCorrection(kOffSet,ZdriftDistance));
  }
  dEZ = dE;
  if (m_dXCorrection) {
    Int_t N = m_dXCorrection->GetNRows();
    Double_t xL2 = TMath::Log2(dx);
    Double_t dXCorr = m_dXCorrection->CalcCorrection(kTpcOutIn,xL2); 
    if (N > 2) dXCorr += m_dXCorrection->CalcCorrection(2,xL2);
    if (N > 6) dXCorr += m_dXCorrection->CalcCorrection(5+kTpcOutIn,xL2);
    dE *= TMath::Exp(-dXCorr);
  }
  dEX = dE;
  if (dE <= 0 || dx <= 0) {
    return 3;
  }
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
  CdEdx.ZdriftDistanceO2 = ZdriftDistanceO2;
  CdEdx.ZdriftDistanceO2W = ZdriftDistanceO2W;
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
  return 0;
}
#define PrPP(A,B) {if (B)  gMessMgr->Warning() << "StTpcdEdxCorrection: " << (#A) << " correction has been set" << endm;}
#define PrPD(A,B) {if (B) {gMessMgr->Warning() << "StTpcdEdxCorrection: " << (#A) << " correction has been set" << endm; \
                           if (Debug()) {(B)->Print(0,(B)->GetNRows());}}}
void StTpcdEdxCorrection::SetTpcSecRow            (St_TpcSecRowCor  *m) {PrPP(TpcSecRow   ,m); m_TpcSecRow = m;}            
void StTpcdEdxCorrection::Setdrift                (St_tpcCorrection *m) {PrPD(drift       ,m); if (m) m_drift= new St_tpcCorrectionC(m);}                
void StTpcdEdxCorrection::SetMultiplicity         (St_tpcCorrection *m) {PrPD(Multiplicity,m); if (m) m_Multiplicity= new St_tpcCorrectionC(m);}         
void StTpcdEdxCorrection::SetAdcCorrection        (St_tpcCorrection *m) {PrPD(Adc         ,m); if (m) m_AdcCorrection= new St_tpcCorrectionC(m);}        
void StTpcdEdxCorrection::SetzCorrection          (St_tpcCorrection *m) {PrPD(z           ,m); if (m) m_zCorrection= new St_tpcCorrectionC(m);}          
void StTpcdEdxCorrection::SetdXCorrection         (St_tpcCorrection *m) {PrPD(dX          ,m); if (m) m_dXCorrection = new St_tpcCorrectionC(m);}        
void StTpcdEdxCorrection::SetTpcdEdxCor           (St_tpcCorrection *m) {PrPD(TpcdEdx     ,m); if (m) m_TpcdEdxCor= new St_tpcCorrectionC(m);}           
void StTpcdEdxCorrection::SetTpcLengthCorrection  (St_tpcCorrection *m) {PrPD(TpcLength   ,m); if (m) m_TpcLengthCorrection= new St_tpcCorrectionC(m);}  
void StTpcdEdxCorrection::SettpcGas               (St_tpcGas        *m) {PrPD(tpcGas      ,m); m_tpcGas = m;}               
void StTpcdEdxCorrection::SettpcPressure          (St_tpcCorrection *m) {PrPD(tpcPressure ,m); if (m) m_tpcPressure= new St_tpcCorrectionC(m);}          
void StTpcdEdxCorrection::SettpcMethaneIn         (St_tpcCorrection *m) {PrPD(tpcMethaneIn ,m); if (m) m_tpcMethaneIn= new St_tpcCorrectionC(m);}          
void StTpcdEdxCorrection::SettpcGasTemperature         (St_tpcCorrection *m) {PrPD(tpcGasTemperature ,m); if (m) m_tpcGasTemperature= new St_tpcCorrectionC(m);}          
void StTpcdEdxCorrection::SettpcWaterOut         (St_tpcCorrection *m) {PrPD(tpcWaterOut ,m); if (m) m_tpcWaterOut= new St_tpcCorrectionC(m);}          
void StTpcdEdxCorrection::SetTpcPadTBins         (St_tpcCorrection *m) {PrPD(TpcPadTBins ,m); if (m) m_TpcPadTBins= new St_tpcCorrectionC(m);}          
//void StTpcdEdxCorrection::SettrigDetSums          (St_trigDetSums      *m = 0) {PrPD(SettrigDetSums        ,m); m_trigDetSums = m;}
//void StTpcdEdxCorrection::SettpcGainMonitor       (St_tpcGainMonitor   *m = 0) {PrPD(SettpcGainMonitor     ,m); m_tpcGainMonitor = m;}
#undef PrPP
#undef PrPD
