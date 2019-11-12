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
#include "StDetectorDbMaker/St_TpcAdcCorrectionMDF.h"
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
#include "StDetectorDbMaker/St_TpcPadCorrectionMDF.h"
#include "StDetectorDbMaker/St_TpcdEdxCorC.h" 
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_TpcAvgCurrentC.h"
#include "StDetectorDbMaker/St_TpcAvgPowerSupplyC.h"
#include "StDetectorDbMaker/St_tpcTimeDependenceC.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StDetectorDbMaker/St_beamInfoC.h"
#include "St_db_Maker/St_db_Maker.h"
#include "TUnixTime.h"
//________________________________________________________________________________
StTpcdEdxCorrection::StTpcdEdxCorrection(Int_t option, Int_t debug) : 
  m_Mask(option), m_tpcGas(0),// m_trigDetSums(0), m_trig(0),
  m_Debug(debug)
{
  assert(gStTpcDb);
  if (!m_Mask) m_Mask = -1;
  static const Char_t *FXTtables[] = {"TpcdXCorrectionB",
				      "tpcGainCorrection",
				      "TpcLengthCorrectionMDF",
				      "TpcPadCorrectionMDF",
				      "TpcSecRowB",
				      "TpcZCorrectionB"};
  static Int_t NT = sizeof(FXTtables)/sizeof(const Char_t *);
  Bool_t isFixedTarget = St_beamInfoC::instance()->IsFixedTarget();
  TString flavor("sim+ofl");
  if (isFixedTarget) flavor = "sim+ofl+FXT";
  St_db_Maker *dbMk = (St_db_Maker *) StMaker::GetTopChain()->Maker("db");
  for (Int_t i = 0; i < NT; i++) {
    dbMk->SetFlavor(flavor, FXTtables[i]);
  }
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
  memset (m_Corrections, 0, sizeof(m_Corrections));
  m_Corrections[kUncorrected           ] = dEdxCorrection_t("UnCorrected"         ,""                                                                    ,0); 					       
  m_Corrections[kAdcCorrection         ] = dEdxCorrection_t("TpcAdcCorrectionB"   ,"ADC/Clustering nonlinearity correction"				,St_TpcAdcCorrectionBC::instance());	     
  m_Corrections[kEdge                  ] = dEdxCorrection_t("TpcEdge"             ,"Gain on distance from Chamber edge"                                 ,St_TpcEdgeC::instance());		     
  m_Corrections[kAdcCorrectionMDF      ] = dEdxCorrection_t("TpcAdcCorrectionMDF" ,"ADC/Clustering nonlinearity correction MDF"				,St_TpcAdcCorrectionMDF::instance());	     
  m_Corrections[kTpcdCharge            ] = dEdxCorrection_t("TpcdCharge"          ,"ADC/Clustering undershoot correction"				,St_TpcdChargeC::instance());		     
  m_Corrections[kTpcrCharge            ] = dEdxCorrection_t("TpcrCharge"          ,"ADC/Clustering rounding correction"					,St_TpcrChargeC::instance());		     
  m_Corrections[kTpcCurrentCorrection  ] = dEdxCorrection_t("TpcCurrentCorrection","Correction due to sagg of Voltage due to anode current"		,St_TpcCurrentCorrectionC::instance());     
  m_Corrections[kTpcRowQ               ] = dEdxCorrection_t("TpcRowQ"             ,"Gas gain correction for row versus accumulated charge,"             ,St_TpcRowQC::instance());		           
  m_Corrections[kTpcSecRowB            ] = dEdxCorrection_t("TpcSecRowB"          ,"Gas gain correction for sector/row"					,St_TpcSecRowBC::instance());		     
  m_Corrections[kTpcSecRowC            ] = dEdxCorrection_t("TpcSecRowC"          ,"Additional Gas gain correction for sector/row"			,St_TpcSecRowCC::instance());		     
  m_Corrections[ktpcPressure           ] = dEdxCorrection_t("tpcPressureB"        ,"Gain on Gas Density due to Pressure"			        ,St_tpcPressureBC::instance());	     
  m_Corrections[ktpcTime               ] = dEdxCorrection_t("tpcTime"       	  ,"Unregognized time dependce"						,St_tpcTimeDependenceC::instance()); 
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
  m_Corrections[kTpcPadMDF             ] = dEdxCorrection_t("TpcPadCorrectionMDF" ,"Gain Variation along the anode wire"                                ,St_TpcPadCorrectionMDF::instance());         
  m_Corrections[kTpcLast               ] = dEdxCorrection_t("Final"        	  ,""								        ,0);					     
  m_Corrections[kTpcLengthCorrection   ] = dEdxCorrection_t("TpcLengthCorrectionB"  ,"Variation vs Track length and relative error in Ionization"	,St_TpcLengthCorrectionBC::instance());     
  m_Corrections[kTpcLengthCorrectionMDF] = dEdxCorrection_t("TpcLengthCorrectionMDF","Variation vs Track length and <log2(dX)> and rel. error in dE/dx" ,St_TpcLengthCorrectionMDF::instance());         
  m_Corrections[kTpcNoAnodeVGainC      ] = dEdxCorrection_t("TpcNoAnodeVGainC"      ,"Remove tpc Anode Voltage gain correction"				,0);					         
  m_Corrections[kTpcdEdxCor            ] = dEdxCorrection_t("TpcdEdxCor"            ,"dEdx correction wrt Bichsel parameterization"			,St_TpcdEdxCorC::instance());               
  const St_tpcCorrectionC *chair = 0;
  const St_MDFCorrectionC *chairMDF = 0;
  const St_tpcCorrection  *table = 0;
  const St_MDFCorrection  *tableMDF = 0;
  const tpcCorrection_st *cor = 0;
  const MDFCorrection_st *corMDF = 0;
  Int_t N = 0;  
  Int_t npar = 0;
  Int_t nrows = 0;
  for (Int_t k = kUncorrected+1; k < kTpcAllCorrections; k++) {
    if (! m_Corrections[k].Chair) continue;
    nrows = 0;
    LOG_INFO << "StTpcdEdxCorrection: " << m_Corrections[k].Name << "/" << m_Corrections[k].Title << endm;
    if (! TESTBIT(m_Mask,k) || m_Corrections[k].Chair->Table()->IsMarked()) {
      LOG_INFO << " \tis missing" << endm; 
      goto CLEAR;
    }
    chair    = dynamic_cast<St_tpcCorrectionC *>(m_Corrections[k].Chair);
    chairMDF = dynamic_cast<St_MDFCorrectionC *>(m_Corrections[k].Chair);
    if (! chair && ! chairMDF) {
      LOG_WARN << " \tis not tpcCorrection or MDFCorrection type" << endm;
      m_Corrections[k].nrows = m_Corrections[k].Chair->Table()->GetNRows();
      continue; // not St_tpcCorrectionC
    }
    npar = 0;
    if (chair) {
      table = (const St_tpcCorrection  *) chair->Table();
      if (! table) goto EMPTY;
      cor = table->GetTable();
      N = table->GetNRows();
      if (! cor || ! N) {
	goto EMPTY;
      }
      N = cor->nrows;
      for (Int_t i = 0; i < N; i++, cor++) {
	if (cor->nrows == 0 && cor->idx == 0) continue;
	if (TMath::Abs(cor->npar) > 0       ||
	    TMath::Abs(cor->OffSet) > 1.e-7 ||
	    TMath::Abs(cor->min)    > 1.e-7 ||
	    TMath::Abs(cor->max)    > 1.e-7) {
	  npar++;
	  nrows++;
	}
      }
      if (! npar ) {
	LOG_INFO << " \thas no significant corrections => switch it off" << endm;
	goto CLEAR;
      }
      m_Corrections[k].nrows = nrows;
      continue;
    }
    tableMDF = (const St_MDFCorrection *) chairMDF->Table();
    if (! tableMDF) goto EMPTY;
    corMDF = tableMDF->GetTable();
    N = tableMDF->GetNRows();
    if (! corMDF || ! N) {
      goto EMPTY;
    }
    npar = 0;
    for (Int_t i = 0; i < N; i++, corMDF++) {
      if (corMDF->nrows == 0 && corMDF->idx == 0) continue;
      npar++;
      nrows++;
    }
    if (! npar ) {
      LOG_INFO << " \thas no significant corrections => switch it off" << endm;
      goto CLEAR;
    }
    m_Corrections[k].nrows = nrows;
    continue;
  EMPTY:
    LOG_INFO << " \tis empty" << endm;
  CLEAR:
    CLRBIT(m_Mask,k); 
    m_Corrections[k].Chair = 0;
  }	
  // Use only one ADC correction
  if (m_Corrections[kAdcCorrection         ].Chair && 
      m_Corrections[kAdcCorrectionMDF      ].Chair) {
    LOG_ERROR << " \tTwo ADC corrections activated ? Keep active only AdcCorrectionMDF" << endm;
    m_Corrections[kAdcCorrection         ].Chair = 0;
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
  if (CdEdx.F.dE <= 0.) CdEdx.F.dE = 1;
  Double_t dEU = CdEdx.F.dE;
  Double_t dE  = dEU;
  Int_t sector            = CdEdx.sector; 
  Int_t row       	  = CdEdx.row;   
  Double_t dx     	  = CdEdx.F.dx;    
  Double_t adcCF = CdEdx.adc;
  if (dx <= 0 || (dEU <= 0 && adcCF <= 0)) return 3;
  Int_t channel = St_TpcAvgPowerSupplyC::instance()->ChannelFromRow(sector,row); 
  CdEdx.channel = channel;
  
  CdEdx.Voltage = St_tpcAnodeHVavgC::instance()->voltagePadrow(sector,row);
  CdEdx.Crow    = St_TpcAvgCurrentC::instance()->AvCurrRow(sector,row);
  Double_t    Qcm      = St_TpcAvgCurrentC::instance()->AcChargeRowL(sector,row); // C/cm
  CdEdx.Qcm     = 1e6*Qcm; // uC/cm
  TUnixTime u(StMaker::GetChain()->GetDateTime(), kTRUE); // GMT
  if (! St_trigDetSumsC::GetInstance()) {
    StMaker::GetChain()->AddData(St_trigDetSumsC::instance());
  }
#if 0
  if ( ! St_trigDetSumsC::instance() ) {LOG_ERROR << "StTpcdEdxCorrection::dEdxCorrection Cannot find trigDetSums" << endm;}
  else {
    if (!St_trigDetSumsC::instance()->GetNRows()) {LOG_ERROR << "StTpcdEdxCorrection::dEdxCorrection trigDetSums has not data" << endm;}
    else {
      TUnixTime u2(St_trigDetSumsC::instance()->timeOffset());
      if (u() + 30 < u2()) { 
	LOG_ERROR << "StTpcdEdxCorrection::dEdxCorrection Illegal time for scalers = " 
		  << u2() << "/" << u()
		  << " Run " << St_trigDetSumsC::instance()->runNumber() << "/" << StMaker::GetChain()->GetRunNumber() << endm;
	//	St_trigDetSumsC::instance()->Table()->Print(0,10);
      }
    }
  }
#if 0
  // Check that we have valid time for Power Suppliers
  if (St_TpcAvgPowerSupplyC::instance()->run() > 0) {
    TUnixTime u2(St_trigDetSumsC::instance()->timeOffset()+5*3600); // EDT => GMT
    if (u2() < u() + 30) {
      LOG_ERROR <<  "StTpcdEdxCorrection::dEdxCorrection Illegal TpcAvgPowerSupply stop time = " << u2() << " GMT from local " << u2.GetGString() 
		<< " < event time = " << u()  << " GMT\t=" << StMaker::GetChain()->GetDateTime().AsString() << endm;
      return kStErr;
    }
  }
#endif
#endif  
  Double_t ZdriftDistance = CdEdx.ZdriftDistance;
  ESector kTpcOutIn = kTpcOuter;
  if (! St_tpcPadConfigC::instance()->iTpc(sector)) {
    if (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) kTpcOutIn = kTpcInner;
  } else {
    if (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) kTpcOutIn = kiTpc;
  }
  St_tss_tssparC *tsspar = St_tss_tssparC::instance();
  Float_t gasGain = 1;
  Float_t gainNominal = 0;
  if (row > St_tpcPadConfigC::instance()->innerPadRows(sector)) {
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
  if (VarXs[kEdge] < -1) VarXs[kEdge] = -1;
  if (VarXs[kEdge] >  1) VarXs[kEdge] =  1;    
  VarXs[kPhiDirection]         = (TMath::Abs(CdEdx.xyzD[0]) > 1.e-7) ? TMath::Abs(CdEdx.xyzD[1]/CdEdx.xyzD[0]) : 999.;
  VarXs[kTanL]                 = CdEdx.TanL;     
  VarXs[ktpcTime]              = CdEdx.tpcTime; 
  VarXs[kAdcCorrection] = VarXs[kAdcCorrectionMDF] = adcCF;
  for (Int_t k = kUncorrected; k <= kTpcLast; k++) {
    Int_t l = 0;
    tpcCorrection_st *cor = 0;
    tpcCorrection_st *corl = 0;
    if (CdEdx.lSimulated) {
      if (k == kAdcCorrection) dE *= 2.116;//  1.75; // 1.25 is in Trs already <<<< !!!!!!
      goto ENDL;
    }
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
      if      (kTpcOutIn == kTpcOuter) dx *= ((const St_TpcEffectivedXC* ) m_Corrections[k].Chair)->scaleOuter();
      else if (kTpcOutIn == kTpcInner ||
	       kTpcOutIn == kiTpc )    dx *= ((const St_TpcEffectivedXC* ) m_Corrections[k].Chair)->scaleInner();
      goto ENDL;
    }
    if (k == kTpcPadMDF) {
      l = 2*(sector-1);
      if (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) l += kTpcInner; // for both tpc and iTPC inner sectors
      dE *= TMath::Exp(-((St_TpcPadCorrectionMDF *)m_Corrections[k].Chair)->Eval(l,CdEdx.yrow,CdEdx.xpad));
      goto ENDL;
    }
    if (k == kAdcCorrectionMDF) {
      ADC = adcCF;
      if (ADC <= 0) return 3; //HACK to avoid FPE (VP)
      Double_t xx[2] = {TMath::Log(ADC), (Double_t)(CdEdx.npads+CdEdx.ntmbks)};
      l = kTpcOutIn;
      Int_t nrows = ((St_TpcAdcCorrectionMDF *) m_Corrections[k].Chair)->nrows();
      if (l >= nrows) l = nrows - 1;
      dE = ADC*Adc2GeVReal*((St_TpcAdcCorrectionMDF *) m_Corrections[k].Chair)->Eval(l,xx);
      goto ENDL;
    }
    cor = ((St_tpcCorrection *) m_Corrections[k].Chair->Table())->GetTable();
    if (! cor) goto ENDL;
    nrows = cor->nrows;
    if (nrows <= 3)
      l = TMath::Min(nrows-1, kTpcOutIn);
    else {
      if (nrows == St_tpcPadConfigC::instance()->numberOfRows(sector)) l = row - 1;
      else if (nrows == 192) {l = 8*(sector-1) + channel - 1; assert(l == (cor+l)->idx-1);}
      else if (nrows ==  48) {l = 2*(sector-1) + kTpcOutIn;}
      else if (nrows ==   6) {l =            kTpcOutIn;     if (sector > 12) l+= 3;}
      else if (nrows ==   4) {l = TMath::Min(kTpcOutIn, 1); if (sector > 12) l+= 2;} 
    }
    corl = cor + l;
    iCut = 0;
    if (k == kAdcCorrection) {
      ADC = adcCF;
      if (ADC <= 0) return 3; //HACK to avoid FPE (VP)
      if (corl->type == 12) 
	dE = Adc2GeVReal*((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l,ADC,VarXs[kTanL]);
      else 
	dE = Adc2GeVReal*((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l,ADC,TMath::Abs(CdEdx.zG));
      if (dE <= 0) return 3;
      goto ENDL;
    } else if (k == kTpcdCharge) {
      if (l > 2) l = 1;
      slope = ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l,row+0.5);
      dE *=  TMath::Exp(-slope*CdEdx.dCharge);
      dE *=  TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(2+l,CdEdx.dCharge));
      goto ENDL;
    } else if (k == kzCorrection) {iCut = 1; // Always cut
    } else if (k == kdXCorrection) {
      xL2 = TMath::Log2(dx);
      dXCorr = ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l,xL2); 
      if (TMath::Abs(dXCorr) > 10) return 3;
      if (nrows == 7) {// old schema without iTPC
	dXCorr += ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(2,xL2);
	dXCorr += ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(5+kTpcOutIn,xL2);
      }
      CdEdx.dxC = TMath::Exp(dXCorr)*CdEdx.F.dx;
      dE *= TMath::Exp(-dXCorr);
      goto ENDL;
    } else if (k == kSpaceCharge) {
      if (cor[2*l  ].min <= CdEdx.QRatio && CdEdx.QRatio <= cor[2*l  ].max &&
	  cor[2*l+1].min <= CdEdx.DeltaZ && CdEdx.DeltaZ <= cor[2*l+1].max) 
	dE *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(2*l  ,CdEdx.QRatio)
			 -((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(2*l+1,CdEdx.DeltaZ));
      goto ENDL;
    } else if (k == kEdge) {
      if (corl->type == 200) VarXs[kEdge] = TMath::Abs(CdEdx.edge);
      if (corl->min > 0 && corl->min > VarXs[kEdge]    ) return 2;
    } else if (k == ktpcTime) { // use the correction if you have xmin < xmax && xmin <= x <= xmax
      if (corl->min >= corl->max || corl->min > VarXs[ktpcTime] ||  VarXs[ktpcTime] > corl->max) goto ENDL;
      Double_t xx = VarXs[ktpcTime];
      dE *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l,xx));
      goto ENDL;
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
      if (iok) {
	return iok;
      }
    }
    if (corl->npar%100) {
      Double_t dECor = TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l,VarXs[k]));
#if 0
      if (TMath::IsNaN(dECor)) {
	static Int_t iBreak = 0;
	iBreak++;
      }
#endif
      dE *= dECor;
#if 0
      if (TMath::IsNaN(dE)) {
	static Int_t iBreak = 0;
	iBreak++;
      }
#endif
    }
  ENDL:
    CdEdx.C[k].dE = dE;
    CdEdx.C[k].dx = dx;
    CdEdx.C[k].dEdx    = CdEdx.C[k].dE/CdEdx.C[k].dx;
    CdEdx.C[k].dEdxL   = TMath::Log(CdEdx.C[k].dEdx);
  }   
#if 0 
  if (TMath::IsNaN(CdEdx.C[kTpcLast].dE)) {
    static Int_t iBreak = 0;
    iBreak++;
  }
#endif
  CdEdx.F = CdEdx.C[kTpcLast];
  //  memcpy (&CdEdx.dE, &CdEdx.C[kTpcLast].dE, sizeof(dE_t));
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
  Double_t I70L;
  Int_t k = opt;
  if (! m_Corrections[k].Chair) return 0;
  Int_t l = 2*type;
  Int_t nrows = 0;
  switch (k) {
  case kTpcLengthCorrection:
    nrows = ((St_tpcCorrectionC *) m_Corrections[k].Chair)->nrows(l);
    switch (type) {
    case 0: // I70
    case 1: // dNdx
    case 2: // fit
      if (nrows > l+1) {
	dedx.dedx[0]   *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(  l,LogTrackLength));
	dedx.dedx[1]    =             ((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l+1,LogTrackLength);
      }
      if (nrows > l+6) {
	dedx.dedx[0]   *= TMath::Exp(-((St_tpcCorrectionC *)m_Corrections[k].Chair)->CalcCorrection(l+6,LogTrackLength));
      }
      break;
    default:
      break;
    }
    break;
  case kTpcLengthCorrectionMDF:
    nrows = ((St_MDFCorrectionC *) m_Corrections[k].Chair)->nrows(l);
    if (dedx.det_id > 100 && nrows > l+6) l += 6;
    switch (type) {
    case 0: // I70
    case 1: // dNdx
    case 2: // fit
      if (nrows > l+1) {
	dedx.dedx[0]   *= TMath::Exp(-((St_MDFCorrectionC *)m_Corrections[k].Chair)->Eval(  l,xx));
	dedx.dedx[1]    =             ((St_MDFCorrectionC *)m_Corrections[k].Chair)->Eval(l+1,xx);
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
      Line += Form("\tdE %10.5g",mdEdx->F.dE);
      Line += Form("\tdx  %10.5g",mdEdx->F.dx);
      Line += Form("\tdE/dx  %10.5g",mdEdx->F.dEdx);
      Line += Form("\tlog(dE/dx)  %10.5g",mdEdx->F.dEdxL);
      Line +=  "\tFinal \t "; 
    }
    cout << Line.Data() << endl;
  }
}
