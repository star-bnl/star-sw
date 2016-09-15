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
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/St_TpcEdgeC.h"
#include "StDetectorDbMaker/St_TpcAdcCorrectionBC.h"
#include "StDetectorDbMaker/St_TpcdChargeC.h"
#include "StDetectorDbMaker/St_TpcrChargeC.h"
#include "StDetectorDbMaker/St_TpcCurrentCorrectionC.h"
#include "StDetectorDbMaker/St_TpcRowQC.h"
#include "StDetectorDbMaker/St_TpcSecRowBC.h"
#include "StDetectorDbMaker/St_TpcSecRowCC.h"
#include "StDetectorDbMaker/St_tpcPressureBC.h"
#include "StDetectorDbMaker/St_TpcDriftDistOxygenC.h"
#include "StDetectorDbMaker/St_TpcMultiplicityC.h"
#include "StDetectorDbMaker/St_TpcZCorrectionBC.h"
#include "StDetectorDbMaker/St_tpcMethaneInC.h"
#include "StDetectorDbMaker/St_tpcGasTemperatureC.h"
#include "StDetectorDbMaker/St_tpcWaterOutC.h"
#include "StDetectorDbMaker/St_TpcSpaceChargeC.h"
#include "StDetectorDbMaker/St_TpcPhiDirectionC.h"
#include "StDetectorDbMaker/St_TpcTanLC.h"
#include "StDetectorDbMaker/St_TpcdXCorrectionBC.h"
#include "StDetectorDbMaker/St_TpcEffectivedXC.h" 
#include "StDetectorDbMaker/St_TpcZDCC.h"
#include "StDetectorDbMaker/St_TpcLengthCorrectionBC.h"
#include "StDetectorDbMaker/St_TpcLengthCorrectionMDF.h"
#include "StDetectorDbMaker/St_TpcdEdxCorC.h" 
//________________________________________________________________________________
StTpcdEdxCorrection::StTpcdEdxCorrection(Int_t option, Int_t debug) : 
  m_Mask(option), m_tpcGas(0),// m_trigDetSums(0), m_trig(0),
  mNumberOfRows(-1), mNumberOfInnerRows(-1),
  m_Debug(debug)
{
  assert(gStTpcDb);
  mNumberOfInnerRows      = gStTpcDb->PadPlaneGeometry()->numberOfInnerRows();
  mNumberOfRows           = gStTpcDb->PadPlaneGeometry()->numberOfRows();											          
  if (!m_Mask) m_Mask = -1;
  ReSetCorrections();
}
//________________________________________________________________________________
void StTpcdEdxCorrection::ReSetCorrections() {
  St_tpcGas *tpcGas = (St_tpcGas *) St_tpcGasC::instance()->Table(); //
  //  St_tpcGas *tpcGas = (St_tpcGas *) StMaker::GetChain()->GetDataBase("Calibrations/tpc/tpcGas");
  if (!tpcGas || ! tpcGas->GetNRows()) {
    LOG_ERROR << "=== tpcGas is missing ===" << endm; 
    assert(tpcGas);
  }
  SettpcGas(tpcGas);
  m_Corrections[kUncorrected           ] = dEdxCorrection_t("UnCorrected"         ,""                                                                    ,0); 					       
  m_Corrections[kEdge                  ] = dEdxCorrection_t("TpcEdge"             ,"Gain on distance from Chamber edge"                                 ,St_TpcEdgeC::instance());		     
  m_Corrections[kAdcCorrection         ] = dEdxCorrection_t("TpcAdcCorrectionB"   ,"ADC/Clustering nonlinearity correction"				,St_TpcAdcCorrectionBC::instance());	     
  m_Corrections[kTpcdCharge            ] = dEdxCorrection_t("TpcdCharge"          ,"ADC/Clustering undershoot correction"				,St_TpcdChargeC::instance());		     
  m_Corrections[kTpcrCharge            ] = dEdxCorrection_t("TpcrCharge"          ,"ADC/Clustering rounding correction"					,St_TpcrChargeC::instance());		     
  m_Corrections[kTpcCurrentCorrection  ] = dEdxCorrection_t("TpcCurrentCorrection","Correction due to sagg of Voltage due to anode current"		,St_TpcCurrentCorrectionC::instance());     
  m_Corrections[kTpcRowQ               ] = dEdxCorrection_t("TpcRowQ"             ,"Gas gain correction for row versus accumulated charge,"             ,St_TpcRowQC::instance());		           
  m_Corrections[kTpcSecRowB            ] = dEdxCorrection_t("TpcSecRowB"          ,"Gas gain correction for sector/row"					,St_TpcSecRowBC::instance());		     
  m_Corrections[kTpcSecRowC            ] = dEdxCorrection_t("TpcSecRowC"          ,"Additional Gas gain correction for sector/row"			,St_TpcSecRowCC::instance());		     
  m_Corrections[ktpcPressure           ] = dEdxCorrection_t("tpcPressureB"        ,"Gain on Gas Density due to Pressure"			        ,St_tpcPressureBC::instance());	     
  m_Corrections[ktpcTime               ] = dEdxCorrection_t("tpcTime"       	  ,""									,0);					         
  m_Corrections[kDrift                 ] = dEdxCorrection_t("TpcDriftDistOxygen"  ,"Correction for Electron Attachment due to O2"			,St_TpcDriftDistOxygenC::instance());	     
  m_Corrections[kMultiplicity          ] = dEdxCorrection_t("TpcMultiplicity"     ,"Global track multiplicity dependence"				,St_TpcMultiplicityC::instance());	     
  m_Corrections[kzCorrection           ] = dEdxCorrection_t("TpcZCorrectionB"     ,"Variation on drift distance"					,St_TpcZCorrectionBC::instance());	     
  m_Corrections[ktpcMethaneIn          ] = dEdxCorrection_t("tpcMethaneIn"        ,"Gain on Methane content"					        ,St_tpcMethaneInC::instance());	     
  m_Corrections[ktpcGasTemperature     ] = dEdxCorrection_t("tpcGasTemperature"   ,"Gain on Gas Dens. due to Temperature"			        ,St_tpcGasTemperatureC::instance());	         
  m_Corrections[ktpcWaterOut           ] = dEdxCorrection_t("tpcWaterOut"         ,"Gain on Water content"					        ,St_tpcWaterOutC::instance());		     
  m_Corrections[kSpaceCharge           ] = dEdxCorrection_t("TpcSpaceCharge"      ,"Gain on space charge near the wire"			                ,St_TpcSpaceChargeC::instance());	     
  m_Corrections[kPhiDirection          ] = dEdxCorrection_t("TpcPhiDirection"     ,"Gain on interception angle"				                ,St_TpcPhiDirectionC::instance());	     
  m_Corrections[kTanL                  ] = dEdxCorrection_t("TpcTanL"             ,"Gain on Tan(lambda)"					        ,St_TpcTanLC::instance());		     
  m_Corrections[kdXCorrection          ] = dEdxCorrection_t("TpcdXCorrectionB"    ,"dX correction"							,St_TpcdXCorrectionBC::instance());	     
  m_Corrections[kTpcEffectivedX        ] = dEdxCorrection_t("TpcEffectivedX"      ,"dEdx correction wrt Bichsel parameterization"			,St_TpcEffectivedXC::instance()); 	     
  m_Corrections[kTpcPadTBins           ] = dEdxCorrection_t("TpcPadTBins"         ,"Variation on cluster size"						,0);					     
  m_Corrections[kTpcZDC                ] = dEdxCorrection_t("TpcZDC"        	  ,"Gain on Zdc CoincidenceRate"				        ,St_TpcZDCC::instance());		     
  m_Corrections[kTpcLast               ] = dEdxCorrection_t("Final"        	  ,""								        ,0);					     
  m_Corrections[kTpcLengthCorrection   ] = dEdxCorrection_t("TpcLengthCorrectionB"  ,"Variation vs Track length and relative error in Ionization"	,St_TpcLengthCorrectionBC::instance());     
  m_Corrections[kTpcLengthCorrectionMDF] = dEdxCorrection_t("TpcLengthCorrectionMDF","Variation vs Track length and <log2(dX)> and rel. error in dE/dx" ,St_TpcLengthCorrectionMDF::instance());         
  m_Corrections[kTpcNoAnodeVGainC      ] = dEdxCorrection_t("TpcNoAnodeVGainC"      ,"Remove tpc Anode Voltage gain correction"				,0);					         
  m_Corrections[kTpcdEdxCor            ] = dEdxCorrection_t("TpcdEdxCor"            ,"dEdx correction wrt Bichsel parameterization"			,St_TpcdEdxCorC::instance());               
  for (Int_t k = kUncorrected+1; k < kTpcAllCorrections; k++) {
    if (! m_Corrections[k].Chair) continue;
    LOG_INFO << "StTpcdEdxCorrection: " << m_Corrections[k].Name << "/" << m_Corrections[k].Title << endm;
    if (! TESTBIT(m_Mask,k) || m_Corrections[k].Chair->Table()->IsMarked()) {
      LOG_INFO << " \tis missing" << endm; 
      m_Corrections[k].Chair = 0;
      continue;
    }
    St_tpcCorrectionC *chair =  Correction(k);
    if (! chair ) continue; // not St_tpcCorrectionC
    const St_tpcCorrection *table = (const St_tpcCorrection *) chair->Table();
    const tpcCorrection_st *cor = table->GetTable();
    Int_t N = table->GetNRows();
    if (! cor || ! N) {
      LOG_INFO << " \tis empty" << endm;
      CLRBIT(m_Mask,k); 
      m_Corrections[k].Chair = 0;
      continue;
    }
    Int_t npar = 0;
    for (Int_t i = 0; i < N; i++, cor++) {
      if (cor->nrows == 0 && cor->idx == 0) continue;
      npar += TMath::Abs(cor->npar); 
      if (TMath::Abs(cor->OffSet) > 1.e-7 ||
	  TMath::Abs(cor->min)    > 1.e-7 ||
	  TMath::Abs(cor->max)    > 1.e-7) npar++;
    }
    if (! npar ) {
      LOG_INFO << " \thas no significant corrections => switch it off" << endm;
      CLRBIT(m_Mask,k); 
      m_Corrections[k].Chair = 0;
      continue;
    }	
  }
}
//________________________________________________________________________________
StTpcdEdxCorrection::~StTpcdEdxCorrection() {
  // Can't delete because the chairs are also used in StTpcRSMaker
  //  for (Int_t k = 0; k < kTpcAllCorrections; k++) SafeDelete(m_Corrections[k].Chair);
}
//________________________________________________________________________________
Int_t  StTpcdEdxCorrection::dEdxCorrection(dEdxY2_t &CdEdx, Bool_t doIT) { 
  //  static const Double_t Degree2Rad = TMath::Pi()/180.;
  mdEdx = &CdEdx;
  Double_t dEU = CdEdx.dE;
  Double_t dE  = dEU;
  Int_t sector            = CdEdx.sector; 
  Int_t row       	  = CdEdx.row;   
  Int_t channel           = CdEdx.channel;
  Double_t dx     	  = CdEdx.dx;    
  if (dE <= 0 || dx <= 0) return 3;
  Double_t ZdriftDistance = CdEdx.ZdriftDistance;
  ESector kTpcOutIn = kTpcOuter;
  if (row <= mNumberOfInnerRows) kTpcOutIn = kTpcInner;
  St_tss_tssparC *tsspar = St_tss_tssparC::instance();
  Float_t gasGain = 1;
  Float_t gainNominal = 0;
  if (row > mNumberOfInnerRows) {
    gainNominal = tsspar->gain_out()*tsspar->wire_coupling_out();
    gasGain = tsspar->gain_out(sector,row)*tsspar->wire_coupling_out();
  } else {
    gainNominal = tsspar->gain_in()*tsspar->wire_coupling_in();
    gasGain = tsspar->gain_in(sector,row) *tsspar->wire_coupling_in();
  }
  if (gasGain <= 0.0) return 4;
  //  Double_t gainAVcorr = gasGain/gainNominal;
  mAdc2GeV = tsspar->ave_ion_pot() * tsspar->scale()/gainNominal;
  Double_t Adc2GeVReal = tsspar->ave_ion_pot() * tsspar->scale()/gasGain;
  tpcGas_st *gas = m_tpcGas->GetTable();
  Double_t ZdriftDistanceO2 = ZdriftDistance*(*m_tpcGas)[0].ppmOxygenIn;
  Double_t ZdriftDistanceO2W = ZdriftDistanceO2*(*m_tpcGas)[0].ppmWaterOut;
  CdEdx.ZdriftDistanceO2 = ZdriftDistanceO2;
  CdEdx.ZdriftDistanceO2W = ZdriftDistanceO2W;
  Double_t gc, ADC, xL2, dXCorr;
  Double_t adcCF = CdEdx.adc;
  Int_t l = 0;
  tpcCorrection_st *cor = 0;
  tpcCorrection_st *corl = 0;
  Double_t iCut = 0;
  Double_t slope = 0;
  Int_t nrows = 0;
  Double_t VarXs[kTpcLast] = {-999.};
  VarXs[kTpcZDC]               = (CdEdx.Zdc > 0) ? TMath::Log10(CdEdx.Zdc) : 0;
  VarXs[kTpcCurrentCorrection] = CdEdx.Crow;                                   
  VarXs[kTpcrCharge]           = CdEdx.rCharge;                               
  VarXs[kTpcRowQ]              = CdEdx.Qcm;
  VarXs[kTpcPadTBins]          = CdEdx.Npads*CdEdx.Ntbins;     
  VarXs[ktpcPressure]          = TMath::Log(gas->barometricPressure);     
  VarXs[kDrift]                = ZdriftDistanceO2;      // Blair correction 
  VarXs[kMultiplicity]         = CdEdx.QRatio;     
  VarXs[kzCorrection]          = ZdriftDistance;
  VarXs[ktpcMethaneIn]         = gas->percentMethaneIn*1000./gas->barometricPressure;     
  VarXs[ktpcGasTemperature]    = gas->outputGasTemperature;     
  VarXs[ktpcWaterOut]          = gas->ppmWaterOut;     
  VarXs[kEdge]                 = CdEdx.PhiR;
  VarXs[kPhiDirection]         = (TMath::Abs(CdEdx.xyzD[0]) > 1.e-7) ? TMath::Abs(CdEdx.xyzD[1]/CdEdx.xyzD[0]) : 999.;
  VarXs[kTanL]                  = CdEdx.TanL;     
  for (Int_t k = kUncorrected; k <= kTpcLast; k++) {
    if (k != kAdcCorrection && CdEdx.lSimulated) goto ENDL;
    if (! TESTBIT(m_Mask, k)) goto ENDL;
    if (! m_Corrections[k].Chair) goto ENDL;
    if (k == kTpcSecRowB || k == kTpcSecRowC ) {
      const St_TpcSecRowCor *table = (const St_TpcSecRowCor *) m_Corrections[k].Chair->Table();
      if (! table) goto ENDL;
      const TpcSecRowCor_st *gain = table->GetTable() + sector - 1;
      gc =  gain->GainScale[row-1];
      if (gc <= 0.0) return 1;
      dE *= gc;
      CdEdx.Weight = 1;
      if (gain->GainRms[row-1] > 0.1) CdEdx.Weight = 1./(gain->GainRms[row-1]*gain->GainRms[row-1]);
      goto ENDL;
    } else if (k == kTpcEffectivedX) {
      dx *= (kTpcOutIn == kTpcInner) ? 
	((const St_TpcEffectivedXC* ) m_Corrections[k].Chair)->scaleInner() :
	((const St_TpcEffectivedXC* ) m_Corrections[k].Chair)->scaleOuter();
      goto ENDL;
    }
    cor = ((St_tpcCorrection *) m_Corrections[k].Chair->Table())->GetTable();
    if (! cor) goto ENDL;
    nrows = cor->nrows;
    l = kTpcOuter;
    if (nrows == 2) {if (row <= mNumberOfInnerRows) l = kTpcOutIn;}
    else {
      if (nrows == mNumberOfRows) l = row - 1;
      else if (nrows == 192) {l = 8*(sector-1) + channel - 1; assert(l == (cor+l)->idx-1);}
    }
    corl = cor + l;
    iCut = 0;
    if (k ==  kAdcCorrection) {
      if (CdEdx.lSimulated) {
	dE *= 2.116;//  1.75; // 1.25 is in Trs already <<<< !!!!!!
      } else {
	ADC = adcCF;
        if (ADC <=0) return 3; //HACK to avoid FPE (VP)
	if (corl->type == 12) 
	  dE = Adc2GeVReal*((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(kTpcOutIn,ADC,VarXs[kTanL]);
	else 
	  dE = Adc2GeVReal*((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(kTpcOutIn,ADC,TMath::Abs(CdEdx.zG));
	if (dE <= 0) return 3;
      }
      goto ENDL;
    } else {
      if (k == kTpcdCharge) {
	slope = ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(kTpcOutIn,row+0.5);
	dE *=  TMath::Exp(-slope*CdEdx.dCharge);
	dE *=  TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(2+kTpcOutIn,CdEdx.dCharge));
	goto ENDL;
      } else if (k == kzCorrection) {iCut = 1; // Always cut
      } else if (k == kdXCorrection) {
	xL2 = TMath::Log2(dx);
	dXCorr = ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(kTpcOutIn,xL2); 
	if (nrows > 2) dXCorr += ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(2,xL2);
	if (nrows > 6) dXCorr += ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(5+kTpcOutIn,xL2);
	CdEdx.dxC = TMath::Exp(dXCorr)*CdEdx.dx;
	goto ENDL;
      } else if (k == kSpaceCharge) {
	if (cor[2*kTpcOutIn  ].min <= CdEdx.QRatio && CdEdx.QRatio <= cor[2*kTpcOutIn  ].max &&
	    cor[2*kTpcOutIn+1].min <= CdEdx.DeltaZ && CdEdx.DeltaZ <= cor[2*kTpcOutIn+1].max) 
	  dE *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(2*kTpcOutIn  ,CdEdx.QRatio)
			   -((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(2*kTpcOutIn+1,CdEdx.DeltaZ));
	goto ENDL;
      } else if (k == kEdge) {
	if (corl->type == 200) VarXs[kEdge] = TMath::Abs(CdEdx.edge);
	if (corl->min > 0 && corl->min > VarXs[kEdge]    ) return 2;
	if (corl->max > 0 && VarXs[kEdge]     > corl->max) return 2;
      } 
    }
    if (corl->type == 300) {
      if (corl->min > 0 && corl->min > VarXs[k]    ) VarXs[k] = corl->min;
      if (corl->max > 0 && VarXs[k]     > corl->max) VarXs[k] = corl->max;
    }
    if (TMath::Abs(corl->npar) >= 100 || iCut) {
      Int_t iok = 2;
      if (corl->min >= corl->max) {
	iok = 0;
      } else {
	if (corl->min <= VarXs[k] && VarXs[k] <= corl->max) {
	  iok = 0;
	}
      }
      if (iok) return iok;
    }
    if (corl->npar%100) dE *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l,VarXs[k]));
#if 0
    if (corl->npar%100 
	&& ! (corl->type == 300 && corl->min >= corl->max && VarXs[k] < corl->min)
	)  dE *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l,VarXs[k]));
#endif
  ENDL:
    CdEdx.C[k].dE = dE;
    CdEdx.C[k].dx = dx;
    CdEdx.C[k].dEdx    = CdEdx.C[k].dE/CdEdx.C[k].dx;
    CdEdx.C[k].dEdxL   = TMath::Log(CdEdx.C[k].dEdx);
  }    
  memcpy (&CdEdx.dE, &CdEdx.C[kTpcLast].dE, sizeof(dE_t));
  return 0;
}
//________________________________________________________________________________
Int_t StTpcdEdxCorrection::dEdxTrackCorrection(Int_t type, dst_dedx_st &dedx) {
  Int_t ok = 0;
  if      (m_Corrections[kTpcLengthCorrection   ].Chair) ok = dEdxTrackCorrection(kTpcLengthCorrection   ,type,dedx);
  else if (m_Corrections[kTpcLengthCorrectionMDF].Chair) ok = dEdxTrackCorrection(kTpcLengthCorrectionMDF,type,dedx);
  if      (m_Corrections[kTpcdEdxCor].Chair)             ok = dEdxTrackCorrection(kTpcdEdxCor            ,type,dedx);
  return ok;
}
//________________________________________________________________________________
Int_t StTpcdEdxCorrection::dEdxTrackCorrection(EOptions opt, Int_t type, dst_dedx_st &dedx) {
  Double_t LogTrackLength = TMath::Log((Double_t) (dedx.ndedx/100));
  Double_t dxLog2   = dedx.dedx[2];
  Double_t xx[2] = {LogTrackLength, dxLog2};
  Int_t nrows = 0;
  Double_t I70L;
  Int_t k = opt;
  if (! m_Corrections[k].Chair) return 0;
  switch (k) {
  case kTpcLengthCorrection:
    nrows = ((St_tpcCorrectionC *) m_Corrections[k].Chair)->nrows();
    switch (type) {
    case 0: // I70
    case 1: // dNdx
    case 2: // fit
      if (nrows > 1+2*type) {
	dedx.dedx[0]   *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(  2*type,LogTrackLength));
	dedx.dedx[1]    =             ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(1+2*type,LogTrackLength);
      }
      if (nrows > 6+2*type) {
	dedx.dedx[0]   *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(6+2*type,LogTrackLength));
      }
      break;
    default:
      break;
    }
    break;
  case kTpcLengthCorrectionMDF:
    nrows = ((St_MDFCorrectionC *) m_Corrections[k].Chair)->nrows();
    switch (type) {
    case 0: // I70
    case 1: // dNdx
    case 2: // fit
      if (nrows > 1+2*type) {
	dedx.dedx[0]   *= TMath::Exp(-((St_MDFCorrectionC *)m_Corrections[k].Chair)->Eval(  2*type,xx));
	dedx.dedx[1]    =             ((St_MDFCorrectionC *)m_Corrections[k].Chair)->Eval(1+2*type,xx);
      }
      break;
    default:
      break;
    }
    break;
  case kTpcdEdxCor:
    I70L = TMath::Log(1.e6*dedx.dedx[0]);
    if (I70L > 0) dedx.dedx[0] *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->SumSeries(0,I70L));
    break;
  default:
    break;
  }
  return 0;
}
//________________________________________________________________________________
void StTpcdEdxCorrection::Print(Option_t *opt) const {
  if (! mdEdx) return;
  cout << "StTpcdEdxCorrection:: Sector/row/pad " << mdEdx->sector << "/" << mdEdx->row << "/" << mdEdx->pad << endl;
  cout << "Npads/Ntbins " << mdEdx->Npads << "/" << mdEdx->Ntbins 
       << "\tdrift distance / O2 / O2W " << mdEdx->ZdriftDistance << "/" << mdEdx->ZdriftDistanceO2 << "/" << mdEdx->ZdriftDistanceO2W << endl;
  cout << "Local xyz " << mdEdx->xyz[0] << "\t" << mdEdx->xyz[1] << "\t" << mdEdx->xyz[2] << endl;
  cout << "Local xyzD " << mdEdx->xyzD[0] << "\t" << mdEdx->xyzD[1] << "\t" << mdEdx->xyzD[2] << endl;
  TString Line;
  for (Int_t k = (Int_t)kUncorrected; k <= ((Int_t)kTpcLast)+1; k++) {
    Line  = Form("%2i",k);
    if (k <= (Int_t) kTpcLast) {
      Line += Form("\tdE %10.5g",mdEdx->C[k].dE);
      Line += Form("\tdx  %10.5g",mdEdx->C[k].dx);
      Line += Form("\tdE/dx  %10.5g",mdEdx->C[k].dEdx);
      Line += Form("\tlog(dE/dx)  %10.5g",mdEdx->C[k].dEdxL);
      Line += "\t"; Line += TString(m_Corrections[k].Name); Line += "\t"; Line +=  TString(m_Corrections[k].Title);
    } else {
      Line += Form("\tdE %10.5g",mdEdx->dE);
      Line += Form("\tdx  %10.5g",mdEdx->dx);
      Line += Form("\tdE/dx  %10.5g",mdEdx->dEdx);
      Line += Form("\tlog(dE/dx)  %10.5g",mdEdx->dEdxL);
      Line +=  "\tFinal \t "; 
    }
    cout << Line.Data() << endl;
  }
}
