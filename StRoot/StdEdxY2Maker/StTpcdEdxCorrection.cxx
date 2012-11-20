/*! 
  \class dEdxY2_t
  dEdxY2_t class contains data for each cluster which is used during different calibartion steps
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
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/St_tpcAvCurrentC.h"
#include "St_db_Maker/St_db_Maker.h"
ClassImp(dEdxY2_t);
ClassImp(StTpcdEdxCorrection)
//________________________________________________________________________________
StTpcdEdxCorrection::StTpcdEdxCorrection(Int_t option, Int_t debug) : 
  m_Mask(option), m_tpcGas(0),// m_trigDetSums(0), m_trig(0),
  m_TpcSecRowB(0),
  m_TpcSecRowC(0),
  m_Debug(debug)
{
  assert(gStTpcDb);
  memset (&m_Corrections, 0, kTpcAllCorrections*sizeof(dEdxCorrection_t));
  m_Corrections[kAdcCorrection       ] = dEdxCorrection_t("TpcAdcCorrectionB"   ,"ADC/Clustering nonlinearity correction");
  m_Corrections[kEdge                ] = dEdxCorrection_t("TpcEdge"             ,"Dependence of the Gain on distance from Chamber edge");
  m_Corrections[kTpcdCharge          ] = dEdxCorrection_t("TpcdCharge"          ,"ADC/Clustering undershoot correction");
  m_Corrections[kTpcrCharge          ] = dEdxCorrection_t("TpcrCharge"          ,"ADC/Clustering rounding correction");
  m_Corrections[kTpcCurrentCorrection] = dEdxCorrection_t("TpcCurrentCorrection","Correction due to sagg of Voltage due to anode current");
  m_Corrections[kTpcRowQ             ] = dEdxCorrection_t("TpcRowQ"         	,"Gas gain correction for row versus accumulated charge, absolute normalization");
  m_Corrections[kTpcSecRowB          ] = dEdxCorrection_t("TpcSecRowB"         	,"Gas gain correction for sector/row");
  m_Corrections[kTpcSecRowC          ] = dEdxCorrection_t("TpcSecRowC"         	,"Additional Gas gain correction for sector/row");
  m_Corrections[kDrift               ] = dEdxCorrection_t("TpcDriftDistOxygen" 	,"Correction for Electron Attachment due to O2");
  m_Corrections[kMultiplicity        ] = dEdxCorrection_t("TpcMultiplicity"     ,"Global track multiplicity dependence");
  m_Corrections[kzCorrection         ] = dEdxCorrection_t("TpcZCorrectionB"    	,"Variation on drift distance");
  m_Corrections[kdXCorrection        ] = dEdxCorrection_t("TpcdXCorrectionB"   	,"dX correction");
  m_Corrections[ktpcPressure         ] = dEdxCorrection_t("tpcPressureB"       	,"Dependence of the Gain on Gas Density due to Pressure");
  m_Corrections[ktpcMethaneIn        ] = dEdxCorrection_t("tpcMethaneIn"       	,"Dependence of the Gain on Methane content");
  m_Corrections[ktpcGasTemperature   ] = dEdxCorrection_t("tpcGasTemperature"  	,"Dependence of the Gain on Gas Density due to Temperature");
  m_Corrections[ktpcWaterOut         ] = dEdxCorrection_t("tpcWaterOut"        	,"Dependence of the Gain on Water content");
  m_Corrections[kTpcdCharge          ] = dEdxCorrection_t("TpcdCharge"        	,"Dependence of the Gain on total charge accumulated so far");
  m_Corrections[kTpcZDC              ] = dEdxCorrection_t("TpcZDC"        	,"Dependence of the Gain on Zdc CoincidenceRate");
  //  m_Corrections[kTpcPadTBins         ] = dEdxCorrection_t("TpcPadTBins"        	,"Variation on cluster size");
  m_Corrections[kSpaceCharge         ] = dEdxCorrection_t("TpcSpaceCharge"      ,"Dependence of the Gain on space charge near the wire");
  m_Corrections[kPhiDirection        ] = dEdxCorrection_t("TpcPhiDirection"     ,"Dependence of the Gain on interception angle");
  m_Corrections[kTpcdEdxCor          ] = dEdxCorrection_t("TpcdEdxCor"         	,"dEdx correction wrt Bichsel parameterization"); 
  m_Corrections[kTpcLengthCorrection ] = dEdxCorrection_t("TpcLengthCorrectionB","Variation on Track length and relative error in Ionization");

  if (!m_Mask) m_Mask = -1;
  // 
  ReSetCorrections();
}
//________________________________________________________________________________
void StTpcdEdxCorrection::ReSetCorrections() {
  TDataSet *tpc_calib  = StMaker::GetChain()->GetDataBase("Calibrations/tpc"); assert(tpc_calib);
  if (Debug() > 1) tpc_calib->ls(3);
  St_tpcGas *k_tpcGas = (St_tpcGas *) tpc_calib->Find("tpcGas");
  if (!k_tpcGas || ! k_tpcGas->GetNRows()) {
    gMessMgr->Error() << "=== tpcGas is missing ===" << endm; 
    assert(k_tpcGas);
  }
  SettpcGas(k_tpcGas);
  TDatime t[2];					
  if (St_db_Maker::GetValidity(k_tpcGas,t) > 0) {				
    Int_t Nrows = k_tpcGas->GetNRows();					
    LOG_WARN << "StTpcdEdxCorrection::ReSetCorrections found table " << k_tpcGas->GetName() 
	     << " with NRows = " << Nrows << " in db" << endm;		
    LOG_WARN << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()	
	     << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm; 
    if (Nrows > 10) Nrows = 10;						
    if (k_tpcGas->GetRowSize() < 256) k_tpcGas->Print(0,Nrows);		
  }
  St_TpcSecRowCor *TpcSecRow = 0;
  St_tpcCorrection *table = 0;
  tpcCorrection_st *cor = 0;
  Int_t N = 0;
  Int_t k = 0;
  Int_t i = 0;
  Int_t npar = 0;
  for (k = kUncorrected+1; k < kTpcAllCorrections; k++) {
    SafeDelete(m_Corrections[k].Chair);
    if (! m_Corrections[k].Name ) {CLRBIT(m_Mask,k); continue;}
    if (! TESTBIT(m_Mask,k)) continue;
    //    gMessMgr->Warning() << "StTpcdEdxCorrection: " <<  m_Corrections[k].Name << " is ON" << endm;
    gMessMgr->Warning() << "StTpcdEdxCorrection: " << m_Corrections[k].Name << "/" << m_Corrections[k].Title << endm;
    switch (k) {
    case kTpcSecRowB:
      TpcSecRow  = (St_TpcSecRowCor *) tpc_calib->Find("TpcSecRowB"); 
      if (TpcSecRow) SetTpcSecRowB(TpcSecRow);
      else {CLRBIT(m_Mask,k); gMessMgr->Warning() << " \tis missing" << endm;}
      break;
    case kTpcSecRowC:
      TpcSecRow  = (St_TpcSecRowCor *) tpc_calib->Find("TpcSecRowC"); 
      if (TpcSecRow) SetTpcSecRowC(TpcSecRow);
      else {CLRBIT(m_Mask,k); gMessMgr->Warning() << " \tis missing" << endm;}
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
	npar += TMath::Abs(cor->npar); 
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
      if (table) {
	if (St_db_Maker::GetValidity(table,t) > 0) {			
	  Int_t Nrows = table->GetNRows();				
	  LOG_WARN << "StTpcdEdxCorrection::ReSetCorrections found table " << table->GetName() 
		   << " with NRows = " << Nrows << " in db" << endm;	
	  LOG_WARN << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime() 
		   << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm; 
	  if (Nrows > 10) Nrows = 10;					
	  if (table->GetRowSize() < 256) table->Print(0,Nrows);		
	}
      }
    }
  }
}
//________________________________________________________________________________
StTpcdEdxCorrection::~StTpcdEdxCorrection() {
  for (Int_t k = 0; k < kTpcAllCorrections; k++) SafeDelete(m_Corrections[k].Chair);
}
//________________________________________________________________________________
Int_t  StTpcdEdxCorrection::dEdxCorrection(dEdxY2_t &CdEdx, Bool_t doIT) { 
  //  static const Double_t Degree2Rad = TMath::Pi()/180.;
  mdEdx = &CdEdx;
  Double_t dEU = CdEdx.dE;
  Double_t dE  = dEU;
  Int_t sector            = CdEdx.sector; 
  Int_t row       	  = CdEdx.row;   
  Double_t dx     	  = CdEdx.dx;    
  if (dE <= 0 || dx <= 0) return 3;
  
  Double_t ZdriftDistance = CdEdx.ZdriftDistance;
  ESector kTpcOutIn = kTpcOuter;
  if (row <= 13) kTpcOutIn = kTpcInner;
  St_tss_tssparC *tsspar = St_tss_tssparC::instance();
  Float_t gasGain = 1;
  Float_t gainNominal = 0;
  if (row > 13) {
    gainNominal = tsspar->gain_out()*tsspar->wire_coupling_out();
    gasGain = tsspar->gain_out(sector,row)*tsspar->wire_coupling_out();
  } else {
    gainNominal = tsspar->gain_in()*tsspar->wire_coupling_in();
    gasGain = tsspar->gain_in(sector,row) *tsspar->wire_coupling_in();
  }
  if (gasGain <= 0.0) return 4;
  mAdc2GeV = tsspar->ave_ion_pot() * tsspar->scale()/gainNominal;
  Double_t Adc2GeVReal = tsspar->ave_ion_pot() * tsspar->scale()/gasGain;
  tpcGas_st *gas = m_tpcGas->GetTable();
  Double_t ZdriftDistanceO2 = ZdriftDistance*(*m_tpcGas)[0].ppmOxygenIn;
  Double_t ZdriftDistanceO2W = ZdriftDistanceO2*(*m_tpcGas)[0].ppmWaterOut;
  CdEdx.ZdriftDistanceO2 = ZdriftDistanceO2;
  CdEdx.ZdriftDistanceO2W = ZdriftDistanceO2W;
  Double_t gc, ADC, xL2, dXCorr;
  Double_t adcCF = CdEdx.adc;
  Int_t l;
  tpcCorrection_st *cor = 0;
  tpcCorrection_st *corl = 0;
  TpcSecRowCor_st *gain = 0;
  Double_t VarX = 0;
  Double_t iCut = 0;
  Double_t slope = 0;
  Int_t nrows = 0;
  for (Int_t k = kUncorrected; k <= kTpcLast; k++) {
    if (k != kAdcCorrection && CdEdx.lSimulated) goto ENDL;
    if (! TESTBIT(m_Mask, k)) goto ENDL;
    cor = 0;
    if ( m_Corrections[k].Chair) {
      cor = ((St_tpcCorrection *) m_Corrections[k].Chair->Table())->GetTable();
      if (! cor) goto ENDL;
      nrows = cor->nrows;
      l = kTpcOuter;
      if (nrows > 1 && nrows < 45) l = kTpcOutIn;
      else if (nrows == 45) l = row - 1;
      corl = cor + l;
    }
    iCut = 0;
    switch (k) {
    case kAdcCorrection:
      if (CdEdx.lSimulated) {
	dE *= 2.116;//  1.75; // 1.25 is in Trs already <<<< !!!!!!
      } else {
	ADC = dE/mAdc2GeV;
	if (TMath::Abs(ADC - adcCF) > 1) {
	  // check
	}
	dE = Adc2GeVReal*m_Corrections[k].Chair->CalcCorrection(kTpcOutIn,ADC,TMath::Abs(CdEdx.zG));
	if (dE <= 0) return 3;
      }
      goto ENDL;
    case kTpcdCharge:
      slope = m_Corrections[k].Chair->CalcCorrection(kTpcOutIn,row+0.5);
      dE *=  TMath::Exp(-slope*CdEdx.dCharge);
      dE *=  TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(2+kTpcOutIn,CdEdx.dCharge));
      goto ENDL;
    case kTpcZDC: VarX = (CdEdx.Zdc > 0) ? TMath::Log10(CdEdx.Zdc) : 0; break;
    case kTpcCurrentCorrection:
      VarX = CdEdx.Crow;
      break;
    case kTpcrCharge:
      VarX =  CdEdx.rCharge;
      break;
    case kTpcRowQ:
      VarX = CdEdx.Qcm; break;
    case kTpcSecRowB:
    case kTpcSecRowC:
      if (k == kTpcSecRowB)  gain = m_TpcSecRowB->GetTable() + sector - 1;
      else                   gain = m_TpcSecRowC->GetTable() + sector - 1;
      gc =  gain->GainScale[row-1];
      if (gc <= 0.0) return 1;
      dE *= gc;
      CdEdx.Weight = 1;
      if (gain->GainRms[row-1] > 0.1) CdEdx.Weight = 1./(gain->GainRms[row-1]*gain->GainRms[row-1]);
      goto ENDL;
    case kTpcPadTBins:
      VarX = CdEdx.Npads*CdEdx.Ntbins;
      break;
    case    ktpcPressure:
      VarX = TMath::Log(gas->barometricPressure);
      break;
    case    kDrift:  // Blair correction 
      VarX =  ZdriftDistanceO2;
      break;
    case    kMultiplicity:
      VarX = CdEdx.QRatio;
      break;
    case    kzCorrection:
      VarX = ZdriftDistance;
      iCut = 1; // Always cut
      break;
    case    kdXCorrection:
      xL2 = TMath::Log2(dx);
      dXCorr = m_Corrections[k].Chair->CalcCorrection(kTpcOutIn,xL2); 
      if (nrows > 2) dXCorr += m_Corrections[k].Chair->CalcCorrection(2,xL2);
      if (nrows > 6) dXCorr += m_Corrections[k].Chair->CalcCorrection(5+kTpcOutIn,xL2);
      dE *= TMath::Exp(-dXCorr);
      goto ENDL;
    case    kTpcdEdxCor:
      break;
    case    ktpcMethaneIn:
      VarX = gas->percentMethaneIn*1000./gas->barometricPressure;
      break;
    case    ktpcGasTemperature:
      VarX = gas->outputGasTemperature;
      break;
    case    ktpcWaterOut:
      VarX = gas->ppmWaterOut;
      break;
    case   kSpaceCharge: 
      if (cor[2*kTpcOutIn  ].min <= CdEdx.QRatio && CdEdx.QRatio <= cor[2*kTpcOutIn  ].max &&
	  cor[2*kTpcOutIn+1].min <= CdEdx.DeltaZ && CdEdx.DeltaZ <= cor[2*kTpcOutIn+1].max) 
	dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(2*kTpcOutIn  ,CdEdx.QRatio)
			 -m_Corrections[k].Chair->CalcCorrection(2*kTpcOutIn+1,CdEdx.DeltaZ));
      goto ENDL;
    case kEdge:
      VarX = CdEdx.PhiR;
      if (corl->type == 200) {
	VarX = TMath::Abs(CdEdx.edge);
	if (corl->min > 0 && corl->min > VarX    ) return 2;
	if (corl->max > 0 && VarX     > corl->max) return 2;
      }
      break;
    case kPhiDirection:
      VarX = 999.;
      if (TMath::Abs(CdEdx.xyzD[0]) > 1.e-7) VarX = TMath::Abs(CdEdx.xyzD[1]/CdEdx.xyzD[0]);
      break;
    default:
      goto ENDL;
    }
    if (TMath::Abs(corl->npar) >= 100 || iCut) {
      Int_t iok = 2;
      if (corl->min >= corl->max) {
	iok = 0;
      } else {
	for (; l < nrows; l += 2) {
	  corl = cor + l;
	  if (corl->min <= VarX && VarX <= corl->max) {
	    iok = 0;
	    break;
	  }
	}
      }
      if (iok) return iok;
    }
    if (corl->npar%100) dE *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(l,VarX));
  ENDL:
    CdEdx.C[k].dE = dE;
    CdEdx.C[k].dEdx    = CdEdx.C[k].dE/CdEdx.dx;
    CdEdx.C[k].dEdxL   = TMath::Log(CdEdx.C[k].dEdx);
  }    
  
  memcpy (&CdEdx.dE, &CdEdx.C[kTpcLast].dE, sizeof(dE_t));
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
    case 1: // fit
      if (nrows > 1+4*type) {
	dedx.dedx[0]   *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(  4*type,LogTrackLength));
	dedx.dedx[1]    =             m_Corrections[k].Chair->CalcCorrection(1+4*type,LogTrackLength);
      }
      if (nrows > 6+2*type) {
	dedx.dedx[0]   *= TMath::Exp(-m_Corrections[k].Chair->CalcCorrection(6+2*type,LogTrackLength));
      }
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
    gMessMgr->Warning() << "StTpcdEdxCorrection::SetCorrection " << m_Corrections[k].Name
			<< " \thas been set with nrows = " << m_Corrections[k].nrows << endm;
    {
      TDatime t[2];
      St_db_Maker::GetValidity(m,t);
      gMessMgr->Warning()  << " \tValidity:" << t[0].GetDate() << "/" << t[0].GetTime()
			   << "  -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;
    }
    if (Debug()) m->Print(0,m_Corrections[k].nrows);
  }
}
//________________________________________________________________________________
void StTpcdEdxCorrection::SetTpcSecRowB   (St_TpcSecRowCor *m) {
  if (m) {
    m_TpcSecRowB = m;
    gMessMgr->Warning() << "StTpcdEdxCorrection::SetTpcSecRowB " << m_Corrections[kTpcSecRowB].Name << "/" 
			<< m_Corrections[kTpcSecRowB].Title <<  endm; 
    gMessMgr->Warning() << " \tcorrection has been set" << endm;
    {
      TDatime t[2];
      St_db_Maker::GetValidity(m,t);
      gMessMgr->Warning()  << " Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
			   << "  -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;
    }
  }
}            
//________________________________________________________________________________
void StTpcdEdxCorrection::SetTpcSecRowC   (St_TpcSecRowCor *m) {
  if (m) {
    m_TpcSecRowC = m;
    gMessMgr->Warning() << "StTpcdEdxCorrection::SetTpcSecRowC " << m_Corrections[kTpcSecRowC].Name << "/" 
			<< m_Corrections[kTpcSecRowC].Title <<  endm; 
    gMessMgr->Warning() << " \tcorrection has been set" << endm;
    {
      TDatime t[2];
      St_db_Maker::GetValidity(m,t);
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
    {
      TDatime t[2];
      St_db_Maker::GetValidity(m,t);
      gMessMgr->Warning()  << " Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
			   << "  -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;
    }

    if (Debug()) m_tpcGas->Print(0,m_tpcGas->GetNRows());
  }
}
//________________________________________________________________________________
void StTpcdEdxCorrection::Print(Option_t *opt) const {
  if (! mdEdx) return;
  cout << "StTpcdEdxCorrection:: Sector/row/pad " << mdEdx->sector << "/" << mdEdx->row << "/" << mdEdx->pad << endl;
  cout << "Npads/Ntbins " << mdEdx->Npads << "/" << mdEdx->Ntbins 
       << "\tdrift distance / O2 / O2W " << mdEdx->ZdriftDistance << "/" << mdEdx->ZdriftDistanceO2 << "/" << mdEdx->ZdriftDistanceO2W << endl;
  cout << "Local xyz " << mdEdx->xyz[0] << "\t" << mdEdx->xyz[1] << "\t" << mdEdx->xyz[2] << endl;
  cout << "Local xyzD " << mdEdx->xyzD[0] << "\t" << mdEdx->xyzD[1] << "\t" << mdEdx->xyzD[2] << endl;
  cout << "dx " << mdEdx->dx << " dE " << mdEdx->dE << " dE/dx " << mdEdx->dEdx << " log(dE/dx) " << mdEdx->dEdxL << endl;
  for (Int_t k = kUncorrected; k < kTpcAllCorrections; k++) {
    cout << m_Corrections[k].Name << "\t" << m_Corrections[k].Title 
	 << "\tdE " << mdEdx->C[k].dE 
	 << "\tdE/dx " << mdEdx->C[k].dEdx 
	 << "\tlog(dE/dx) " << mdEdx->C[k].dEdxL << endl;
  }
}
