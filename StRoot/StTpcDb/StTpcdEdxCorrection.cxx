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
#include "St_db_Maker/St_db_Maker.h"
ClassImp(dEdx_t);
ClassImp(StTpcdEdxCorrection);
static St_db_Maker *dbMk = 0;
//________________________________________________________________________________
StTpcdEdxCorrection::StTpcdEdxCorrection(Int_t option, Int_t debug) : 
  m_Mask(option), m_tpcGas(0),// m_trigDetSums(0), m_trig(0),
  //  m_tpcGainMonitor(0),
  m_TpcSecRow(0),
  m_Debug(debug)
{
  assert(gStTpcDb);
  memset (&m_Corrections, 0, kTpcAllCorrections*sizeof(dEdxCorrection_t));
  m_Corrections[kAdcCorrection       ] = dEdxCorrection_t("TpcAdcCorrectionB"   ,"ADC/Clutering nonlinearity correction");
  m_Corrections[kTpcSecRow           ] = dEdxCorrection_t("TpcSecRowB"         	,"Gas gain correction for sector/row");
  m_Corrections[kDrift               ] = dEdxCorrection_t("TpcDriftDistOxygen" 	,"Correction for Electro Attachment due to O2");
  m_Corrections[kMultiplicity        ] = dEdxCorrection_t("TpcMultiplicity"     ,"Global track multiplicity dependence");
  m_Corrections[kzCorrection         ] = dEdxCorrection_t("TpcZCorrectionB"    	,"Variation on drift distance");
  m_Corrections[kdXCorrection        ] = dEdxCorrection_t("TpcdXCorrectionB"   	,"dX correction");
  m_Corrections[ktpcPressure         ] = dEdxCorrection_t("tpcPressureB"       	,"Dependence of the Gain on Gas Density due to Pressure");
  //  m_Corrections[ktpcMethaneIn        ] = dEdxCorrection_t("tpcMethaneIn"       	,"Dependence of the Gain on Methane content");
  m_Corrections[ktpcGasTemperature   ] = dEdxCorrection_t("tpcGasTemperature"  	,"Dependence of the Gain on Gas Density due to Temperature");
  //  m_Corrections[ktpcWaterOut         ] = dEdxCorrection_t("tpcWaterOut"        	,"Dependence of the Gain on Water content");
  //  m_Corrections[kTpcdCharge          ] = dEdxCorrection_t("TpcdCharge"        	,"Dependence of the Gain on total charge accumulated so far");
  //  m_Corrections[kTpcPadTBins         ] = dEdxCorrection_t("TpcPadTBins"        	,"Variation on cluster size");
  m_Corrections[kSpaceCharge         ] = dEdxCorrection_t("SpaceCharge"        	,"Dependence of the Gain on space charge near the wire");
  m_Corrections[kTpcdEdxCor          ] = dEdxCorrection_t("TpcdEdxCor"         	,"dEdx correction wrt Bichsel parameterization"); 
  m_Corrections[kTpcLengthCorrection ] = dEdxCorrection_t("TpcLengthCorrectionB","Variation on Track length and relative error in Ionization");

  if (!m_Mask) m_Mask = -1;
  if (!dbMk) dbMk = (St_db_Maker *) StMaker::GetChain()->Maker("db");
  TDataSet *tpctss  = StMaker::GetChain()->GetDataBase("tpc/tsspars"); assert(tpctss);
  St_tss_tsspar *TssPar = (St_tss_tsspar *) tpctss->Find("tsspar");
  tss_tsspar_st *tsspar = TssPar->GetTable();
  mAdc2GeV[0] = tsspar->ave_ion_pot * tsspar->scale /(tsspar->gain_out*tsspar->wire_coupling_out);
  mAdc2GeV[1] = tsspar->ave_ion_pot * tsspar->scale /(tsspar->gain_in *tsspar->wire_coupling_in );
  // 
#if 0
  TDataSet *rich_calib  = StMaker::GetChain()->GetDataBase("Calibrations/rich"); 
  if (! rich_calib ) gMessMgr->Error() << "StTpcdEdxCorrection:: Cannot find Calibrations/rich" << endm;
  else {
    St_trigDetSums *l_trigDetSums = (St_trigDetSums *) rich_calib->Find("trigDetSums");
    if ( ! l_trigDetSums ) gMessMgr->Error() << "StTpcdEdxCorrection:: Cannot find trigDetSums in Calibrations/rich" << endm;
    else {
      if (!l_trigDetSums->GetNRows()) gMessMgr->Error() << "StTpcdEdxCorrection:: trigDetSums has not data" << endm;
      else {
	trigDetSums_st *l_trig = l_trigDetSums->GetTable();
	UInt_t date = StMaker::GetChain()->GetDateTime().Convert();
	if (date < l_trig->timeOffset) {
	  gMessMgr->Error() << "StTpcdEdxCorrection:: Illegal time for scalers = " 
			    << l_trig->timeOffset << "/" << date
			    << " Run " << l_trig->runNumber << "/" << GetRunNumber() << endm;
	}
	else SettrigDetSums(l_trigDetSums);
      }
    }
  }
#endif
  TDataSet *tpc_calib  = StMaker::GetChain()->GetDataBase("Calibrations/tpc"); assert(tpc_calib);
  if (Debug() > 1) tpc_calib->ls(3);
  St_tpcGas *k_tpcGas = (St_tpcGas *) tpc_calib->Find("tpcGas");
  if (!k_tpcGas || ! k_tpcGas->GetNRows()) {
    gMessMgr->Error() << "=== tpcGas is missing ===" << endm; 
    assert(k_tpcGas);
  }
  SettpcGas(k_tpcGas);

  St_TpcSecRowCor *TpcSecRow = 0;
  St_tpcCorrection *table = 0;
  tpcCorrection_st *cor = 0;
  Int_t N = 0;
  Int_t k = 0;
  Int_t i = 0;
  Int_t npar = 0;
  for (k = 0; k < kTpcAllCorrections; k++) {
    if (! m_Corrections[k].Name ) {CLRBIT(m_Mask,k); continue;}
    if (! TESTBIT(m_Mask,k)) continue;
    //    gMessMgr->Warning() << "StTpcdEdxCorrection: " <<  m_Corrections[k].Name << " is ON" << endm;
    gMessMgr->Warning() << "StTpcdEdxCorrection: " << m_Corrections[k].Name << "/" << m_Corrections[k].Title << endm;
    switch (k) {
    case kTpcSecRow:
      TpcSecRow  = (St_TpcSecRowCor *) tpc_calib->Find("TpcSecRowB"); 
      assert(TpcSecRow); 
      SetTpcSecRow(TpcSecRow);
      break;
    default:
      table = (St_tpcCorrection *) tpc_calib->Find(m_Corrections[k].Name);
      if (! table) {
	gMessMgr->Warning() << " \tis missing" << endm;
	CLRBIT(m_Mask,k); 
	continue;
      }
      cor = table->GetTable();
      N = table->GetNRows();
      if (! cor || ! N) {
	gMessMgr->Warning() << " \tis empty" << endm;
	CLRBIT(m_Mask,k); 
	continue;
      }
      npar = 0;
      for (i = 0; i < N; i++, cor++) {
	if (cor->nrows == 0 && cor->idx == 0) continue;
	npar += cor->npar; 
	if (TMath::Abs(cor->OffSet) > 1.e-7 ||
	    TMath::Abs(cor->min)    > 1.e-7 ||
	    TMath::Abs(cor->max)    > 1.e-7) npar++;
      }
      if (! npar ) {
	gMessMgr->Warning() << " \thas no significant corrections => switch it off" << endm;
	CLRBIT(m_Mask,k); 
	continue;
      }	
      SetCorrection(k,table);
    }
  }
}
//________________________________________________________________________________
StTpcdEdxCorrection::~StTpcdEdxCorrection() {
  for (Int_t k = 0; k < kTpcAllCorrections; k++) SafeDelete(m_Corrections[k].Chair);
}
//________________________________________________________________________________
Int_t  StTpcdEdxCorrection::dEdxCorrection(dEdx_t &CdEdx) { 
  Double_t dEU = CdEdx.dE;
  Double_t dE  = dEU;
#if 0
  Double_t dER = dE;
  Double_t dEP = dE;
  Double_t dET = dE;
  Double_t dEO = dE;
  Double_t dES = dE;
  Double_t dEZ = dE;
  Double_t dEX = dE;
  Double_t dEM = dE;
#endif
  Int_t sector            = CdEdx.sector; 
  Int_t row       	  = CdEdx.row;   
  Double_t dx     	  = CdEdx.dx;    
  if (dE <= 0 || dx <= 0) return 3;
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
  tpcGas_st *gas = m_tpcGas->GetTable();
  Double_t ZdriftDistanceO2 = ZdriftDistance*(*m_tpcGas)[0].ppmOxygenIn;
  Double_t ZdriftDistanceO2W = ZdriftDistanceO2*(*m_tpcGas)[0].ppmWaterOut;
  CdEdx.ZdriftDistanceO2 = ZdriftDistanceO2;
  CdEdx.ZdriftDistanceO2W = ZdriftDistanceO2W;
  
  Double_t gc, ADC, LogPressure, xL2, dXCorr;
  Int_t l, N;
  tpcCorrection_st *cor = 0;
  TpcSecRowCor_st *gain = 0;
  for (Int_t k = 0; k <= kTpcLast; k++) {
    if (! TESTBIT(m_Mask, k)) goto ENDL;
    if (k == kTpcSecRow && ! m_TpcSecRow ||
	k != kTpcSecRow && ! m_Corrections[k].Chair) goto ENDL;
    switch (k) {
    case kAdcCorrection:
      ADC = dE/mAdc2GeV[kTpcOutIn];
      dE = mAdc2GeV[kTpcOutIn]*m_Corrections[k].Chair->CalcCorrection(kTpcOutIn,ADC);
      break;
    case kTpcSecRow:
      gain = m_TpcSecRow->GetTable() + sector - 1;
      gc =  gain->GainScale[row-1];
      CdEdx.SigmaFee = gain->GainRms[row-1];
      if (gc <= 0.0) return 1;
      dE *= gc;
      break;
    case kTpcPadTBins:
      dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(kTpcOutIn,CdEdx.Npads*CdEdx.Ntbins));
    case    ktpcPressure:
      LogPressure = TMath::Log(gas->barometricPressure);
      l = kTpcOutIn;
      if (m_Corrections[k].nrows != 2) l = kTpcOuter;
      dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(l,LogPressure));
      break;
    case    kDrift:  // Blair correction 
      dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(kTpcOutIn,ZdriftDistanceO2));
      break;
    case    kMultiplicity:
      break;
    case    kzCorrection:
      l = kTpcOutIn;
      cor = ((St_tpcCorrection *)  m_Corrections[k].Chair->Table())->GetTable();
      if (cor->nrows == 45) l = row - 1;
      cor += l;
      if (cor->min > 0 && cor->min > ZdriftDistance) return 2;;
      if (cor->max > 0 && ZdriftDistance > cor->max) return 2;;
      dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(l,ZdriftDistance));
      break;
    case    kdXCorrection:
      N = m_Corrections[k].Chair->GetNRows();
      xL2 = TMath::Log2(dx);
      dXCorr = m_Corrections[k].Chair->CalcCorrection(kTpcOutIn,xL2); 
      if (N > 2) dXCorr += m_Corrections[k].Chair->CalcCorrection(2,xL2);
      if (N > 6) dXCorr += m_Corrections[k].Chair->CalcCorrection(5+kTpcOutIn,xL2);
      dE *= TMath::Exp(-dXCorr);
      break;
    case    kTpcdEdxCor:
      break;
    case    ktpcMethaneIn:
      dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(kTpcOuter,gas->percentMethaneIn));
      break;
    case    ktpcGasTemperature:
      dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(kTpcOuter,gas->outputGasTemperature));
      break;
    case    ktpcWaterOut:
      dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(kTpcOuter,gas->ppmWaterOut));
      break;
    case    kTpcdCharge:
      dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(kTpcOutIn,CdEdx.dCharge));
      break;
    default:
      break;
    }
  ENDL:
#if 0
    m_Corrections[k].dE = dE;
#endif
    CdEdx.C[k].dE = dE;
    CdEdx.C[k].dEdx    = CdEdx.C[k].dE/CdEdx.dx;
    CdEdx.C[k].dEdxL   = TMath::Log(CdEdx.C[k].dEdx);
  }    
  
  memcpy (&CdEdx.dE, &CdEdx.C[kTpcLast].dE, sizeof(dE_t));
#if 0
  dER = m_Corrections[kAdcCorrection].dE;
  dES = m_Corrections[kTpcSecRow].dE;
  dEP = m_Corrections[ktpcPressure].dE;
  dET = dE;
  dEM = dE;
  dEO = dE;
  CdEdx.dE      = dE;  // corrected
  CdEdx.dEdx    = CdEdx.dE/CdEdx.dx;
  CdEdx.dEdxL   = TMath::Log(CdEdx.dEdx);
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
#endif
  return 0;
}
//________________________________________________________________________________
Int_t StTpcdEdxCorrection::dEdxTrackCorrection(EOptions opt, Int_t type, dst_dedx_st &dedx) {
  Double_t LogTrackLength = TMath::Log((Double_t) (dedx.ndedx/100));
  Int_t nrows = 0;
  Double_t I70L;
  Int_t k = opt;
  if (! m_Corrections[k].Chair) return 0;
  switch (k) {
  case kTpcLengthCorrection:
    nrows = (((St_tpcCorrection *) m_Corrections[k].Chair->Table())->GetTable())->nrows;
    switch (type) {
    case 0: // I70
      if (nrows > 0) {
	dedx.dedx[0]   *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(0,LogTrackLength));
	dedx.dedx[1]    =             m_Corrections[k].Chair->CalcCorrection(1,LogTrackLength);
      }
      if (nrows > 6) {
	dedx.dedx[0]   *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(6,LogTrackLength));
      }
#ifdef OldClusterFinder
      if (nrows > 10) {
	dedx.dedx[0]   *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(10,LogTrackLength));
      }
#endif      
      break;
    case 1: // fit
      dedx.dedx[0]   *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(4,LogTrackLength));
      dedx.dedx[1]    =             m_Corrections[k].Chair->CalcCorrection(5,LogTrackLength);
#ifdef OldClusterFinder
      if (nrows > 12) {
	dedx.dedx[0]   *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(12,LogTrackLength));
      }
#endif      
      
      break;
    default:
      break;
    }
    
    break;
  case kTpcdEdxCor:
    I70L = TMath::Log(1.e6*dedx.dedx[0]);
    if (I70L > 0) dedx.dedx[0] *= TMath::Exp(-m_Corrections[k].Chair->SumSeries(0,I70L));
    break;
  default:
    break;
  }
  return 0;
}
//________________________________________________________________________________
void StTpcdEdxCorrection::SetCorrection(Int_t k, St_tpcCorrection *m) {
  if (m && k >=0 && k < kTpcAllCorrections && m_Corrections[k].Name) {
    m_Corrections[k].Chair = new St_tpcCorrectionC(m);
    tpcCorrection_st *cor = m->GetTable();
    m_Corrections[k].nrows = cor->nrows;
    gMessMgr->Warning() << "StTpcdEdxCorrection::SetCorrection " << m_Corrections[k].Name << "/" 
			<< m_Corrections[k].Title <<  endm;
    gMessMgr->Warning() << " \thas been set with nrows = " << m_Corrections[k].nrows << endm;
    if (dbMk) {
      TDatime t[2];
      dbMk->GetValidity(m,t);
      gMessMgr->Warning()  << " Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
			   << "  -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;
    }
    if (Debug()) m->Print(0,m_Corrections[k].nrows);
  }
}
//________________________________________________________________________________
void StTpcdEdxCorrection::SetTpcSecRow   (St_TpcSecRowCor *m) {
  if (m) {
    m_TpcSecRow = m;
    gMessMgr->Warning() << "StTpcdEdxCorrection::SetTpcSecRow " << m_Corrections[kTpcSecRow].Name << "/" 
			<< m_Corrections[kTpcSecRow].Title <<  endm; 
    gMessMgr->Warning() << " \tcorrection has been set" << endm;
    if (dbMk) {
      TDatime t[2];
      dbMk->GetValidity(m,t);
      gMessMgr->Warning()  << " Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
			   << "  -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;
    }
  }
}            
//________________________________________________________________________________
//void StTpcdEdxCorrection::SettrigDetSums (St_trigDetSums  *m) {PrPD(SettrigDetSums ,m); m_trigDetSums = m;}
//________________________________________________________________________________
void StTpcdEdxCorrection::SettpcGas      (St_tpcGas       *m) {
  if (m) {
    m_tpcGas = m;
    gMessMgr->Warning() << "StTpcdEdxCorrection::SettpcGas St_tpcGas has been set" << endm; 
    if (dbMk) {
      TDatime t[2];
      dbMk->GetValidity(m,t);
      gMessMgr->Warning()  << " Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
			   << "  -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;
    }

    if (Debug()) m_tpcGas->Print(0,m_tpcGas->GetNRows());
  }
}
//void StTpcdEdxCorrection::SettpcGainMonitor (St_tpcGainMonitor *m) {PrPD(SettpcGainMonitor,m); m_tpcGainMonitor = m;}

