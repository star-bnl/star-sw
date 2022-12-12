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
#include "St_db_Maker/St_db_Maker.h"
#include "TUnixTime.h"
#include "Stiostream.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/St_TpcEdgeC.h"
#include "StDetectorDbMaker/St_TpcAdcCorrectionBC.h"
#include "StDetectorDbMaker/St_TpcAdcCorrectionCC.h"
#include "StDetectorDbMaker/St_TpcAdcCorrectionMDF.h"
#include "StDetectorDbMaker/St_TpcAdcCorrection3MDF.h"
#include "StDetectorDbMaker/St_TpcAdcCorrection4MDF.h"
#include "StDetectorDbMaker/St_TpcAdcCorrection5MDF.h"
#include "StDetectorDbMaker/St_TpcAdcCorrection6MDF.h"
#include "StDetectorDbMaker/St_TpcdChargeC.h"
#include "StDetectorDbMaker/St_TpcrChargeC.h"
#include "StDetectorDbMaker/St_TpcCurrentCorrectionC.h"
#include "StDetectorDbMaker/St_TpcRowQC.h"
#include "StDetectorDbMaker/St_TpcAccumulatedQC.h"
#include "StDetectorDbMaker/St_TpcSecRowBC.h"
#include "StDetectorDbMaker/St_TpcSecRowCC.h"
#include "StDetectorDbMaker/St_tpcPressureBC.h"
#include "StDetectorDbMaker/St_TpcDriftDistOxygenC.h"
#include "StDetectorDbMaker/St_TpcMultiplicityC.h"
#include "StDetectorDbMaker/St_GatingGridC.h"
#include "StDetectorDbMaker/St_GatingGridBC.h"
#include "StDetectorDbMaker/St_TpcZCorrectionBC.h"
#include "StDetectorDbMaker/St_TpcZCorrectionCC.h"
#include "StDetectorDbMaker/St_tpcMethaneInC.h"
#include "StDetectorDbMaker/St_tpcGasTemperatureC.h"
#include "StDetectorDbMaker/St_tpcWaterOutC.h"
#include "StDetectorDbMaker/St_TpcSpaceChargeC.h"
#include "StDetectorDbMaker/St_TpcPhiDirectionC.h"
#include "StDetectorDbMaker/St_TpcTanLC.h"
#include "StDetectorDbMaker/St_TpcAdcIC.h"
#include "StDetectorDbMaker/St_TpcnPadC.h"
#include "StDetectorDbMaker/St_TpcnTbkC.h"
#include "StDetectorDbMaker/St_TpcdZdYC.h"
#include "StDetectorDbMaker/St_TpcdXdYC.h"
#include "StDetectorDbMaker/St_TpcdXCorrectionBC.h"
#include "StDetectorDbMaker/St_TpcEffectivedXC.h" 
#include "StDetectorDbMaker/St_TpcZDCC.h"
#include "StDetectorDbMaker/St_TpcLengthCorrectionBC.h"
#include "StDetectorDbMaker/St_TpcLengthCorrectionMDF.h"
#include "StDetectorDbMaker/St_TpcLengthCorrectionMD2.h"
#include "StDetectorDbMaker/St_TpcLengthCorrectionMDN.h"
#include "StDetectorDbMaker/St_TpcPadCorrectionMDF.h"
#include "StDetectorDbMaker/St_TpcdEdxCorC.h" 
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_TpcAvgCurrentC.h"
#include "StDetectorDbMaker/St_TpcAvgPowerSupplyC.h"
#include "StDetectorDbMaker/St_tpcTimeDependenceC.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StDetectorDbMaker/St_beamInfoC.h"
#include "StDetectorDbMaker/St_starTriggerDelayC.h"
//________________________________________________________________________________
StTpcdEdxCorrection::StTpcdEdxCorrection(Int_t option, Int_t debug) : 
  m_Mask(option), m_tpcGas(0),// m_trigDetSums(0), m_trig(0),
  m_Debug(debug), m_IsSimulation(kFALSE)
{
  assert(gStTpcDb);
  if (!m_Mask) m_Mask = -1;
  static const Char_t *FXTtables[] = {"TpcAdcCorrectionB",         
				      "TpcAdcCorrectionC",         
				      "TpcEdge",            
				      //				      "TpcAdcCorrectionMDF",       
				      "TpcAdcCorrection6MDF",
				      "TpcAdcCorrection5MDF",
				      "TpcAdcCorrection4MDF",
				      "TpcAdcCorrection3MDF",
				      "TpcdCharge",         
				      //				      "TpcrCharge",                
				      "TpcCurrentCorrection",     
				      "TpcRowQ",            
				      "TpcAccumulatedQ",    
				      "TpcSecRowB",                
				      //				      "TpcSecRowC",         
				      "tpcPressureB",       
				      //				      "tpcTime",            
				      "TpcDriftDistOxygen", 
				      "TpcMultiplicity",    
				      "TpcZCorrectionC",           
				      "TpcZCorrectionB",    
				      "tpcMethaneIn",       
				      "tpcGasTemperature",     
				      "tpcWaterOut",        
				      //				      "TpcSpaceCharge",            
				      "TpcPhiDirection",           
				      "TpcTanL",            
				      "TpcdXCorrectionB",   
				      "TpcEffectivedX",     
				      //				      "TpcPadTBins",               
				      "TpcZDC",                    
				      "TpcPadCorrectionMDF",     
				      //				      "TpcAdcI",            
				      //				      "TpcnPad",                   
				      //				      "TpcnTbk",            
				      //				      "TpcdZdY",            
				      //				      "TpcdXdY",            
				      "TpcLengthCorrectionB",
				      "TpcLengthCorrectionMDF",        
				      "TpcLengthCorrectionMD2",              
				      "TpcLengthCorrectionMDN",              
				      "TpcdEdxCor"};
  static Int_t NT = sizeof(FXTtables)/sizeof(const Char_t *);
  m_isFixedTarget = St_beamInfoC::instance()->IsFixedTarget();
  TString flavor("sim+ofl");
#ifdef __TFG__VERSION__
  flavor += "+TFG";
#endif
  if (m_isFixedTarget) flavor += "+FXT";
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
  mTimeBinWidth = 1./StTpcDb::instance()->Electronics()->samplingFrequency();
  mInnerSectorzOffset = StTpcDb::instance()->Dimensions()->zInnerOffset();
  mOuterSectorzOffset = StTpcDb::instance()->Dimensions()->zOuterOffset();
  Double_t trigT0 = 0, elecT0 = 0;
  if (! St_starTriggerDelayC::instance()->Table()->IsMarked()) {// new scheme: offset  = clocks*timebin + t0
    trigT0 = St_starTriggerDelayC::instance()->clocks()*mTimeBinWidth;
    elecT0 = St_starTriggerDelayC::instance()->tZero();
  } else { // old scheme 
    trigT0 = StTpcDb::instance()->triggerTimeOffset()*1e6;         // units are us
    elecT0 = StTpcDb::instance()->Electronics()->tZero();          // units are us 
  }
  m_TrigT0 = trigT0 + elecT0;
  // Chairs
  m_Corrections[kUncorrected           ] = dEdxCorrection_t("UnCorrected"         ,""                                                                   ,0); 					       
  m_Corrections[kAdcCorrection         ] = dEdxCorrection_t("TpcAdcCorrectionB"   ,"ADC/Clustering nonlinearity correction"				,St_TpcAdcCorrectionBC::instance());	     
  m_Corrections[kAdcCorrectionC        ] = dEdxCorrection_t("TpcAdcCorrectionC"   ,"alternative ADC/Clustering nonlinearity correction"			,St_TpcAdcCorrectionCC::instance());	     
  m_Corrections[kEdge                  ] = dEdxCorrection_t("TpcEdge"             ,"Gain on distance from Chamber edge"                                 ,St_TpcEdgeC::instance());		     
  m_Corrections[kAdcCorrectionMDF      ] = dEdxCorrection_t("TpcAdcCorrectionMDF" ,"ADC/Clustering nonlinearity correction MDF"				,St_TpcAdcCorrectionMDF::instance());	     
  m_Corrections[kAdcCorrection3MDF     ] = dEdxCorrection_t("TpcAdcCorrection3MDF","ADC/Clustering nonlinearity correction MDF 3D"			,St_TpcAdcCorrection3MDF::instance());	     
  m_Corrections[kAdcCorrection4MDF     ] = dEdxCorrection_t("TpcAdcCorrection4MDF","ADC/Clustering nonlinearity correction MDF 4D"			,St_TpcAdcCorrection4MDF::instance());	     
  m_Corrections[kAdcCorrection5MDF     ] = dEdxCorrection_t("TpcAdcCorrection5MDF","ADC/Clustering nonlinearity correction MDF+4D"			,St_TpcAdcCorrection5MDF::instance());	     
  m_Corrections[kAdcCorrection6MDF     ] = dEdxCorrection_t("TpcAdcCorrection6MDF","alternative ADC/Clustering nonlinearity correction MDF+4D"		,St_TpcAdcCorrection6MDF::instance());	     
  m_Corrections[kTpcdCharge            ] = dEdxCorrection_t("TpcdCharge"          ,"ADC/Clustering undershoot correction"				,St_TpcdChargeC::instance());		     
  m_Corrections[kTpcrCharge            ] = dEdxCorrection_t("TpcrCharge"          ,"ADC/Clustering rounding correction"					,St_TpcrChargeC::instance());		     
  m_Corrections[kTpcCurrentCorrection  ] = dEdxCorrection_t("TpcCurrentCorrection","Correction due to sagg of Voltage due to anode current"		,St_TpcCurrentCorrectionC::instance());     
  m_Corrections[kTpcRowQ               ] = dEdxCorrection_t("TpcRowQ"             ,"Gas gain correction for row versus accumulated charge,"             ,St_TpcRowQC::instance());
  m_Corrections[kTpcAccumulatedQ       ] = dEdxCorrection_t("TpcAccumulatedQ"     ,"Gas gain correction for HV channel versus accumulated charge,"      ,St_TpcAccumulatedQC::instance());
  m_Corrections[kTpcSecRowB            ] = dEdxCorrection_t("TpcSecRowB"          ,"Gas gain correction for sector/row"					,St_TpcSecRowBC::instance());		     
  m_Corrections[kTpcSecRowC            ] = dEdxCorrection_t("TpcSecRowC"          ,"Additional Gas gain correction for sector/row"			,St_TpcSecRowCC::instance());		     
  m_Corrections[ktpcPressure           ] = dEdxCorrection_t("tpcPressureB"        ,"Gain on Gas Density due to Pressure"			        ,St_tpcPressureBC::instance());	     
  m_Corrections[ktpcTime               ] = dEdxCorrection_t("tpcTime"       	  ,"Unregognized time dependce"						,St_tpcTimeDependenceC::instance()); 
  m_Corrections[kDrift                 ] = dEdxCorrection_t("TpcDriftDistOxygen"  ,"Correction for Electron Attachment due to O2"			,St_TpcDriftDistOxygenC::instance());	     
  m_Corrections[kMultiplicity          ] = dEdxCorrection_t("TpcMultiplicity"     ,"Global track multiplicity dependence"				,St_TpcMultiplicityC::instance());	     
  m_Corrections[kGatingGrid            ] = dEdxCorrection_t("GatingGrid"          ,"Variation due to Gating Grid transperancy"				,St_GatingGridBC::instance());	     
  m_Corrections[kzCorrectionC          ] = dEdxCorrection_t("TpcZCorrectionC"     ,"Variation on drift distance with Gating Grid one"			,St_TpcZCorrectionCC::instance());	     
  m_Corrections[kzCorrection           ] = dEdxCorrection_t("TpcZCorrectionB"     ,"Variation on drift distance without Gating Gird one"		,St_TpcZCorrectionBC::instance());	     
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
  m_Corrections[kAdcI                  ] = dEdxCorrection_t("TpcAdcI"             ,"Gain on Accumulated Adc on a socket)"			        ,St_TpcAdcIC::instance());		     
  m_Corrections[knPad                  ] = dEdxCorrection_t("TpcnPad"             ,"Gain on cluster length in pads"					,St_TpcnPadC::instance());		     
  m_Corrections[knTbk                  ] = dEdxCorrection_t("TpcnTbk"             ,"Gain on cluster length i time buckets"				,St_TpcnTbkC::instance());		     
  m_Corrections[kdZdY                  ] = dEdxCorrection_t("TpcdZdY"             ,"Gain on track dZ/dY"		  		                ,St_TpcdZdYC::instance());		     
  m_Corrections[kdXdY                  ] = dEdxCorrection_t("TpcdXdY"             ,"Gain on track dX/dY"				                ,St_TpcdXdYC::instance());		     
  m_Corrections[kTpcLast               ] = dEdxCorrection_t("Final"        	  ,""								        ,0);					     
  m_Corrections[kTpcLengthCorrection   ] = dEdxCorrection_t("TpcLengthCorrectionB"  ,"Variation vs Track length and relative error in Ionization"	,St_TpcLengthCorrectionBC::instance());     
  m_Corrections[kTpcLengthCorrectionMDF] = dEdxCorrection_t("TpcLengthCorrectionMDF","Variation vs Track length and <log2(dX)> and rel. error in dE/dx" ,St_TpcLengthCorrectionMDF::instance());         
  m_Corrections[kTpcLengthCorrectionMD2] = dEdxCorrection_t("TpcLengthCorrectionMD2","Variation vs Track length and <log2(dX)> for pred. with fixed dx2",St_TpcLengthCorrectionMD2::instance());         
  m_Corrections[kTpcLengthCorrectionMDN] = dEdxCorrection_t("TpcLengthCorrectionMDN","Variation vs Track length and <log2(dX)> for pred. with fixed dx2",St_TpcLengthCorrectionMDN::instance());         
  m_Corrections[kTpcNoAnodeVGainC      ] = dEdxCorrection_t("TpcNoAnodeVGainC"      ,"Remove tpc Anode Voltage gain correction"				,0);					         
  m_Corrections[kTpcdEdxCor            ] = dEdxCorrection_t("TpcdEdxCor"            ,"dEdx correction wrt Bichsel parameterization"			,St_TpcdEdxCorC::instance());               
  const St_tpcCorrectionC *chair = 0;
  const St_MDFCorrectionC *chairMDF = 0;
  const St_MDFCorrection3C *chair3MDF = 0;
  const St_MDFCorrection4C *chair4MDF = 0;
  const St_TpcSecRowCorC  *chairSecRow = 0;
  const St_TpcEffectivedXC *chairEffectivedX = 0;
  const TTable *table = 0;
  TDatime t[2];
  Int_t k = 0;
  TString CommentLine;
  for (k = kUncorrected+1; k < kTpcAllCorrections; k++) {
    if (! m_Corrections[k].Chair) continue;
    CommentLine = Form("StTpcdEdxCorrection: %24s/%66s",m_Corrections[k].Name,m_Corrections[k].Title);
    table = m_Corrections[k].Chair->Table();
    if (! table) continue;
    if (Debug() > 2) table->Print(0,10);
    if (! TESTBIT(m_Mask,k) || m_Corrections[k].Chair->Table()->IsMarked()) {
      CommentLine += " \tis missing";
      CLRBIT(m_Mask,k); 
      SafeDelete(m_Corrections[k].Chair);
      if (Debug()) {
	LOG_WARN << CommentLine.Data() << endm;
      }
      continue;
    }
    if (St_db_Maker::GetValidity(table,t) > 0) {
      CommentLine += Form("\tValidity:%08i.%06i --- %08i.%06i",t[0].GetDate(),t[0].GetTime(),t[1].GetDate(),t[1].GetTime());
    }
    chair    = dynamic_cast<const St_tpcCorrectionC *>(m_Corrections[k].Chair);
    chairMDF = dynamic_cast<const St_MDFCorrectionC *>(m_Corrections[k].Chair);
    chair3MDF = dynamic_cast<const St_MDFCorrection3C *>(m_Corrections[k].Chair);
    chair4MDF = dynamic_cast<const St_MDFCorrection4C *>(m_Corrections[k].Chair);
    Int_t npar = 0;
    if        (chair    ) { npar = chair    ->IsActiveChair();   
    } else if (chairMDF ) { npar = chairMDF ->IsActiveChair();   
    } else if (chair3MDF) { npar = chair3MDF->IsActiveChair();   
    } else if (chair4MDF) { npar = chair4MDF->IsActiveChair();   
    } else {
      chairSecRow = dynamic_cast<const St_TpcSecRowCorC *>(m_Corrections[k].Chair);
      chairEffectivedX = dynamic_cast<const St_TpcEffectivedXC *>(m_Corrections[k].Chair);
      if (! chairSecRow && ! chairEffectivedX) {
	CommentLine +=  "\tis not tpcCorrection, MDFCorrection, TpcEffectivedX, TpcEffectivedX, or TpcSecRowCor types";
      }
      //      if (Debug()) {
      LOG_WARN << CommentLine.Data() << endm;
      //      }
      continue;
    }
    if (! npar ) {
      CommentLine += " \thas no significant corrections => switch it off \tIt is cleaned";
      CLRBIT(m_Mask,k); 
      SafeDelete(m_Corrections[k].Chair);
      if (Debug()) {
	LOG_WARN << CommentLine.Data() << endm;
      }
      continue;
    }
    LOG_WARN << CommentLine.Data() << endm;
  }
  // Check consistency of active chairs
  k = kzCorrection;
  if (m_Corrections[k].Chair) {
    if ( m_Corrections[kzCorrectionC].Chair) { // if kzCorrectionC is already active
      LOG_WARN << "\t" << m_Corrections[kzCorrectionC].Name << " is already active => disanle " << m_Corrections[k].Name << endm;
      CLRBIT(m_Mask,k); 
      SafeDelete(m_Corrections[k].Chair);
    } else {	 // disabled GatingGrid
      LOG_WARN << "\t" << m_Corrections[k].Name << " is activated =>  Disable " << m_Corrections[kGatingGrid].Name << endm;
      CLRBIT(m_Mask,kGatingGrid); 
      SafeDelete(m_Corrections[kGatingGrid].Chair);
    }
  }
  // Use only one set of ADC correction
  Int_t PriorityList[] = { kAdcCorrection6MDF, kAdcCorrection5MDF, kAdcCorrection4MDF, kAdcCorrection3MDF, kAdcCorrectionMDF, kAdcCorrectionC, kAdcCorrection};
  Int_t i = 0;
  for (auto k : PriorityList) {
    i++;
    if (! m_Corrections[k].Chair) continue;
    Int_t j = 0;
    for (auto x : PriorityList) {
      j++;
      if (j <= i) continue;
      if (! m_Corrections[x].Chair) continue;
      if (x == k) continue;
      if (k == kAdcCorrection6MDF && x == kAdcCorrectionC) {
	assert(m_Corrections[x].Chair);
	continue;
      }
      LOG_WARN << "With" << m_Corrections[k].Name << " activated => Deactivate " << m_Corrections[x].Name << endm;
      CLRBIT(m_Mask,x); 
      SafeDelete(m_Corrections[x].Chair);
    }
  }
  // Use only TpcLengthCorrection
  Int_t PriorityListL[] = {kTpcLengthCorrectionMDN,kTpcLengthCorrectionMD2, kTpcLengthCorrectionMDF, kTpcLengthCorrection};
  i = 0;
  for (auto k : PriorityListL) {
    i++;
    if (! m_Corrections[k].Chair) continue;
    Int_t j = 0;
    for (auto x : PriorityListL) {
      j++;
      if (j <= i) continue;
      if (! m_Corrections[x].Chair) continue;
      if (x == k) continue;
      LOG_WARN << "With " << m_Corrections[k].Name << " activated => Deactivate " << m_Corrections[x].Name << endm;
      CLRBIT(m_Mask,x); 
      SafeDelete(m_Corrections[x].Chair);
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
  if (CdEdx.F.dE <= 0.) CdEdx.F.dE = 1;
  Double_t dEU = CdEdx.F.dE;
  Double_t dE  = dEU;
  Int_t sector            = CdEdx.sector; 
  Int_t row       	  = CdEdx.row;   
  Double_t dx     	  = CdEdx.F.dx;    
  Double_t adcCF = CdEdx.adc;
  Int_t iok = 0;
  if (dx <= 0 || (dEU <= 0 && adcCF <= 0)) {
    iok = 3;
    return iok;
  }
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
      iok = kStErr;
      return iok;
    }
  }
#endif
#endif  
  Double_t ZdriftDistance = CdEdx.ZdriftDistance;
  Double_t driftDistance2GG = ZdriftDistance;
  ESector kTpcOutIn = kTpcOuter;
  Float_t gasGain = 1;
  Float_t gainNominal = 0;
  St_tss_tssparC *tsspar = St_tss_tssparC::instance();
  if (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) {
    kTpcOutIn = kTpcInner;
    driftDistance2GG += mInnerSectorzOffset;
    gainNominal = tsspar->gain_in()*tsspar->wire_coupling_in();
    gasGain = tsspar->gain_in(sector,row) *tsspar->wire_coupling_in();
  } else {
    kTpcOutIn = kTpcOuter;
    driftDistance2GG += mOuterSectorzOffset;
    gainNominal = tsspar->gain_out()*tsspar->wire_coupling_out();
    gasGain = tsspar->gain_out(sector,row)*tsspar->wire_coupling_out();
  }
  if (St_tpcPadConfigC::instance()->iTpc(sector) && row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) kTpcOutIn = kiTpc;
  if (gasGain <= 0.0) {
    iok = 3;
    return iok;
  }
  CdEdx.driftTime = driftDistance2GG/gStTpcDb->DriftVelocity(sector)*1e6 - m_TrigT0;// musec
  //  Double_t gainAVcorr = gasGain/gainNominal;
  mAdc2GeV = tsspar->ave_ion_pot() * tsspar->scale()/gainNominal;
  Double_t Adc2GeVReal = tsspar->ave_ion_pot() * tsspar->scale()/gasGain;
  tpcGas_st *gas = m_tpcGas->GetTable();
  Double_t ZdriftDistanceO2 = ZdriftDistance*(*m_tpcGas)[0].ppmOxygenIn;
  Double_t ZdriftDistanceO2W = ZdriftDistanceO2*(*m_tpcGas)[0].ppmWaterOut;
  CdEdx.ZdriftDistanceO2 = ZdriftDistanceO2;
  CdEdx.ZdriftDistanceO2W = ZdriftDistanceO2W;
  Double_t gc, ADC = 0, xL2, dXCorr;
  Double_t slope = 0;
  Int_t nrows = 0;
  Double_t VarXs[kTpcLast] = {-999.};
  VarXs[kAdcCorrection]        = adcCF;
  VarXs[kEdge]                 = TMath::Abs(CdEdx.edge);
  VarXs[kAdcCorrectionMDF]     = adcCF;
  VarXs[kTpcrCharge]           = CdEdx.rCharge;                               
  VarXs[kTpcCurrentCorrection] = CdEdx.Crow;                                   
  VarXs[kTpcRowQ]              = CdEdx.Qcm;
  VarXs[kTpcAccumulatedQ]      = CdEdx.Qcm;
  VarXs[ktpcPressure]          = TMath::Log(gas->barometricPressure);     
  VarXs[ktpcTime]              = CdEdx.tpcTime; 
  VarXs[kDrift]                = ZdriftDistanceO2;      // Blair correction 
  VarXs[kMultiplicity]         = CdEdx.QRatio;     
  VarXs[kGatingGrid]           = CdEdx.driftTime;
  VarXs[kzCorrectionC]         = ZdriftDistance;
  VarXs[kzCorrection]          = ZdriftDistance;
  VarXs[ktpcMethaneIn]         = gas->percentMethaneIn*1000./gas->barometricPressure;     
  VarXs[ktpcGasTemperature]    = gas->outputGasTemperature;     
  VarXs[ktpcWaterOut]          = gas->ppmWaterOut;     
  VarXs[kPhiDirection]         = (TMath::Abs(CdEdx.xyzD[0]) > 1.e-7) ? TMath::Abs(CdEdx.xyzD[1]/CdEdx.xyzD[0]) : 999.;
  VarXs[kTanL]                 = CdEdx.TanL;     
  VarXs[kTpcPadTBins]          = CdEdx.Npads*CdEdx.Ntbks;     
  VarXs[kTpcZDC]               = (CdEdx.Zdc > 0) ? TMath::Log10(CdEdx.Zdc) : 0;
  VarXs[kAdcI]                 = CdEdx.AdcI;  
  VarXs[knPad]                 = CdEdx.Npads;
  VarXs[knTbk]                 = CdEdx.Ntbks; 
  VarXs[kdZdY]                 = CdEdx.dZdY;
  VarXs[kdXdY]                 = CdEdx.dXdY;
  Int_t NLoops = 0;
  Int_t m = 0;
  for (Int_t k = kUncorrected; k <= kTpcLast; k++) {
    Int_t l = 0;
    tpcCorrection_st *cor = 0;
    tpcCorrection_st *corl = 0;
    const St_tpcCorrectionC *chairC;
    if (CdEdx.lSimulated) {
      if (k == kAdcCorrection) dE *= 2.116;//  1.75; // 1.25 is in Trs already <<<< !!!!!!
      goto ENDL;
    }
    if (! TESTBIT(m_Mask, k)) goto ENDL;
    if (! m_Corrections[k].Chair) goto ENDL;
    chairC = dynamic_cast<const St_tpcCorrectionC *>(m_Corrections[k].Chair);
    if (k == kTpcSecRowB || k == kTpcSecRowC ) {
      const St_TpcSecRowCor *table = (const St_TpcSecRowCor *) m_Corrections[k].Chair->Table();
      if (! table) goto ENDL;
      const TpcSecRowCor_st *gain = table->GetTable() + sector - 1;
      gc =  gain->GainScale[row-1];
      if (gc <= 0.0) {
	iok = 1;
	return iok;
      }
      dE *= gc;
      CdEdx.Weight = 1;
      if (gain->GainRms[row-1] > 0.1) CdEdx.Weight = 1./(gain->GainRms[row-1]*gain->GainRms[row-1]);
      goto ENDL;
    } else if (k == kTpcEffectivedX) {
      if      (kTpcOutIn == kTpcOuter) dx *= ((const St_TpcEffectivedXC* ) m_Corrections[k].Chair)->scaleOuter();
      else if (kTpcOutIn == kTpcInner ||
	       kTpcOutIn == kiTpc )    dx *= ((const St_TpcEffectivedXC* ) m_Corrections[k].Chair)->scaleInner();
      goto ENDL;
    } else if (k == kTpcPadMDF) {
      l = 2*(sector-1);
      if (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) l += kTpcInner; // for both tpc and iTPC inner sectors
      dE *= TMath::Exp(-((St_TpcPadCorrectionMDF *)m_Corrections[k].Chair)->Eval(l,CdEdx.yrow,CdEdx.xpad));
      goto ENDL;
    } else if (k == kAdcCorrectionMDF) {
      ADC = adcCF;
      if (ADC <= 0) {
	iok = 3; //HACK to avoid FPE (VP)
	return iok;
      }
      l = kTpcOutIn;
      Int_t nrows = ((St_TpcAdcCorrectionMDF *) m_Corrections[k].Chair)->nrows();
      if (l >= nrows) l = nrows - 1;
      Double_t xx[2] = {TMath::Log(ADC), (Double_t)(CdEdx.Npads+CdEdx.Ntbks)};
      Double_t Cor = ((St_TpcAdcCorrectionMDF *) m_Corrections[k].Chair)->Eval(l,xx);
      dE = ADC*Adc2GeVReal*TMath::Exp(Cor);
      goto ENDL;
    } else if (k == kAdcCorrection3MDF) {
      ADC = adcCF;
      if (ADC <= 0) {
	iok = 3; //HACK to avoid FPE (VP)
	return iok;
      }
      l = kTpcOutIn;
      Int_t nrows = ((St_TpcAdcCorrection3MDF *) m_Corrections[k].Chair)->nrows();
      if (l >= nrows) l = nrows - 1;
      Double_t xx[3] = {(Double_t)  CdEdx.Ntbks, TMath::Abs(CdEdx.zG), TMath::Log(ADC)};
      Double_t Cor = ((St_TpcAdcCorrection3MDF *) m_Corrections[k].Chair)->Eval(l,xx);
      dE = ADC*Adc2GeVReal*TMath::Exp(Cor);
      goto ENDL;
    } else if (k == kAdcCorrection4MDF) {
      ADC = adcCF;
      if (ADC <= 0) {
	iok = 3; //HACK to avoid FPE (VP)
	return iok;
      }
      l = kTpcOutIn;
      Int_t nrows = ((St_TpcAdcCorrection4MDF *) m_Corrections[k].Chair)->nrows();
      if (l >= nrows) l = nrows - 1;
      Double_t xx[4] = {(Double_t)  CdEdx.Ntbks, (Double_t)  CdEdx.Npads, TMath::Abs(CdEdx.zG), TMath::Log(ADC)};
      Double_t Cor = ((St_TpcAdcCorrection4MDF *) m_Corrections[k].Chair)->Eval(l,xx);
      dE = ADC*Adc2GeVReal*TMath::Exp(Cor);
      goto ENDL;
    } else if (k == kAdcCorrection5MDF) {
      l = kTpcOutIn;
      Int_t nrows = ((St_TpcAdcCorrection5MDF *) m_Corrections[k].Chair)->nrows();
      if (l >= nrows) l = nrows - 1;
      Double_t xx[4] = {(Double_t)  CdEdx.Ntbks, (Double_t)  CdEdx.Npads, TMath::Abs(CdEdx.zG), TMath::Log(ADC)};
      Double_t Cor = ((St_TpcAdcCorrection5MDF *) m_Corrections[k].Chair)->Eval(l,xx);
      dE *= TMath::Exp(Cor);
      goto ENDL;
    } else if (k == kAdcCorrection6MDF) { 
      goto ENDL; // kAdcCorrection6MDF is in kAdcCorrectionC
    }
#if 0
    if (k == kGatingGrid)  {
      // Take care about prompt hits and Gating Grid region in Simulation
      if (ZdriftDistance <= 0.0) goto ENDL; // prompt hits 
      Double_t dEcor = ((St_GatingGridC *)m_Corrections[k].Chair)->CalcCorrection(l,VarXs[kGatingGrid]);
      if (dEcor < -9.9) {
	iok = 3; 
	return iok;
      }
      dE *= TMath::Exp(-dEcor);
      goto ENDL;
    }
#endif
    cor = ((St_tpcCorrection *) m_Corrections[k].Chair->Table())->GetTable();
    if (! cor) goto ENDL;
    NLoops = cor->type/100000 + 1;
    nrows = cor->nrows/NLoops;
    if (nrows <= 3) {
      l = TMath::Min(nrows-1, kTpcOutIn);
    } else {
      if (nrows == St_tpcPadConfigC::instance()->numberOfRows(sector)) l = row - 1;
      else if (nrows == 192) {l = 8*(sector-1) + channel - 1; assert(l == (cor+l)->idx-1);}
      else if (nrows ==   8) {l =                channel - 1; assert(l == (cor+l)->idx-1);}
      else if (nrows ==  48) {l = 2*(sector-1) + kTpcOutIn;}
      else if (nrows ==   6) {l =            kTpcOutIn;     if (sector > 12) l+= 3;}
      else if (nrows ==   4) {l = TMath::Min(kTpcOutIn, 1); if (sector > 12) l+= 2;} 
    }
    if (NLoops == 1) {
      corl = cor + l;
      if (k == kAdcCorrection) {
	ADC = adcCF;
	if (ADC <= 0) {
	  iok = 3; //HACK to avoid FPE (VP)
	  return iok;
	}
	if (corl->type == 12) 
	  dE = Adc2GeVReal*chairC->CalcCorrection(l,ADC,VarXs[kTanL]);
	else 
	  dE = Adc2GeVReal*chairC->CalcCorrection(l,ADC,TMath::Abs(CdEdx.zG));
	if (dE <= 0) {
	  iok = 3; 
	  return iok;
	}
	goto ENDL;
      } else if (k == kAdcCorrection6MDF) { 
	goto ENDL; // kAdcCorrection6MDF is in kAdcCorrectionC
      } else if (k == kAdcCorrectionC) {
	ADC = adcCF;
	if (adcCF <= 0) {
	  iok = 3; //HACK to avoid FPE (VP)
	  return iok;
	}
	ADC = chairC->CalcCorrection(l,adcCF);
	if (m_Corrections[kAdcCorrection6MDF].Chair) {
	  Double_t xx[4] = {(Double_t)  CdEdx.Ntbks, (Double_t)  CdEdx.Npads, TMath::Abs(CdEdx.zG), TMath::Log(adcCF)};
	  ADC += ((St_MDFCorrection4C *)m_Corrections[kAdcCorrection6MDF].Chair)->Eval(l,xx);// * TMath::Exp(chairC->a(l)[0]);
	}
	dE = Adc2GeVReal*ADC;
	goto ENDL;
      } else if (k == kTpcdCharge) {
	if (l > 2) l = 1;
	slope = chairC->CalcCorrection(l,row+0.5);
	dE *=  TMath::Exp(-slope*CdEdx.dCharge);
	dE *=  TMath::Exp(-chairC->CalcCorrection(2+l,CdEdx.dCharge));
	goto ENDL;
      } else if (k == kdXCorrection) {
	xL2 = TMath::Log2(dx);
	dXCorr = chairC->CalcCorrection(l,xL2); 
	if (TMath::Abs(dXCorr) > 10) {
	  iok = 3; 
	  return iok;
	}
	if (nrows == 7) {// old schema without iTPC
	  dXCorr += chairC->CalcCorrection(2,xL2);
	  dXCorr += chairC->CalcCorrection(5+kTpcOutIn,xL2);
	}
	CdEdx.dxC = TMath::Exp(dXCorr)*CdEdx.F.dx;
	dE *= TMath::Exp(-dXCorr);
	goto ENDL;
      } else if (k == kSpaceCharge) {
	if (cor[2*l  ].min <= CdEdx.QRatio && CdEdx.QRatio <= cor[2*l  ].max &&
	    cor[2*l+1].min <= CdEdx.DeltaZ && CdEdx.DeltaZ <= cor[2*l+1].max) 
	  dE *= TMath::Exp(-chairC->CalcCorrection(2*l  ,CdEdx.QRatio)
			   -chairC->CalcCorrection(2*l+1,CdEdx.DeltaZ));
	goto ENDL;
      } else if (k == ktpcTime) { // use the correction if you have xmin < xmax && xmin <= x <= xmax
	if (corl->min >= corl->max || corl->min > VarXs[ktpcTime] ||  VarXs[ktpcTime] > corl->max) goto ENDL;
	Double_t xx = VarXs[ktpcTime];
	dE *= TMath::Exp(-chairC->CalcCorrection(l,xx));
	goto ENDL;
      } 
      if (k == kzCorrection || k == kzCorrectionC) {
	// Take care about prompt hits and Gating Grid region in Simulation
	if (ZdriftDistance <= 0.0) goto ENDL; // prompt hits 
	if ((corl->min < corl->max) && (corl->min > VarXs[k] || VarXs[k] > corl->max)) {
	  if (! IsSimulation()) {
	    iok = 2; 
	    return iok;
	  }
	  VarXs[k] = TMath::Min(corl->max, TMath::Max( corl->min, VarXs[k]));
	}
	if (k == kzCorrectionC && corl->type == 20) {
	  Int_t np = TMath::Abs(corl->npar)%100;
	  if (TMath::Abs(corl->a[np]) > 1e-7) {// extra exponent 
	    Double_t dEcor = corl->a[np]*TMath::Exp( corl->a[np+1]*VarXs[k]);
	    if (dEcor < -9.9) {
	      iok = 3; 
	      return iok;
	    }
	    dE *= TMath::Exp(-dEcor);
	  }
	}
      }
      if (corl->type == 200 && (corl->min < corl->max) &&  ! IsSimulation() ) {// cut out the range
	if ((corl->min > VarXs[k] || corl->max < VarXs[k])) {
	  iok = 4; 
	  return iok;
	}
      }
      if (corl->type == 300 && (corl->min < corl->max)) {
	VarXs[k] = TMath::Min(corl->max, TMath::Max( corl->min, VarXs[k])); 
      }
      if (TMath::Abs(corl->npar) >= 100) {
	iok = 2;
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
    }
    if (corl->npar%100) {
      Double_t dECor = 0; 
      if (k == kGatingGrid) {
	dECor = TMath::Exp(-((St_GatingGridBC *)m_Corrections[k].Chair)->CalcCorrection(l,VarXs[k]));
      } else {
	dECor = TMath::Exp(-chairC->CalcCorrection(l,VarXs[k]));
      }
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
    } else  { // repeatable 
      for (m = 0; m < NLoops; m++, l += nrows) {
	corl = cor + l;
	if (corl->min < corl->max && corl->min <= VarXs[k] && VarXs[k] < corl->max) {
	  dE *= TMath::Exp(-chairC->CalcCorrection(l,VarXs[k]));
	  break;
	}	
      }
    }
  ENDL:
    CdEdx.C[k].dE = dE;
    CdEdx.C[k].dx = dx;
    CdEdx.C[k].dEdx    = CdEdx.C[k].dE/CdEdx.C[k].dx;
    CdEdx.C[k].dEdxL   = TMath::Log(CdEdx.C[k].dEdx);
    if (! k) CdEdx.C[k].ddEdxL = 0;
    else     CdEdx.C[k].ddEdxL = CdEdx.C[k].dEdxL - CdEdx.C[k-1].dEdxL;
#if 0
    if (m_Debug) {
      cout << m_Corrections[k].Name;  CdEdx.C[k].Print();
    }
#endif
  }   
#if 0 
  if (TMath::IsNaN(CdEdx.C[kTpcLast].dE)) {
    static Int_t iBreak = 0;
    iBreak++;
  }
#endif
  CdEdx.F = CdEdx.C[kTpcLast];
  //  memcpy (&CdEdx.dE, &CdEdx.C[kTpcLast].dE, sizeof(dE_t));
  return iok;
}
//________________________________________________________________________________
Int_t StTpcdEdxCorrection::dEdxTrackCorrection(Int_t type, dst_dedx_st &dedx, Double_t etaG) {
  Int_t ok = 0;
  if      (m_Corrections[kTpcLengthCorrectionMDN].Chair) ok = dEdxTrackCorrection(kTpcLengthCorrectionMDN,type,dedx, etaG);
  else if (m_Corrections[kTpcLengthCorrectionMD2].Chair) ok = dEdxTrackCorrection(kTpcLengthCorrectionMD2,type,dedx);
  else if (m_Corrections[kTpcLengthCorrectionMDF].Chair) ok = dEdxTrackCorrection(kTpcLengthCorrectionMDF,type,dedx);
  else if (m_Corrections[kTpcLengthCorrection   ].Chair) ok = dEdxTrackCorrection(kTpcLengthCorrection   ,type,dedx);
  if      (m_Corrections[kTpcdEdxCor].Chair)             ok = dEdxTrackCorrection(kTpcdEdxCor            ,type,dedx);
  return ok;
}
//________________________________________________________________________________
Int_t StTpcdEdxCorrection::dEdxTrackCorrection(EOptions opt, Int_t type, dst_dedx_st &dedx, Double_t etaG) {
  Double_t xx[2] = {0};
  Double_t LogTrackLength = TMath::Log((Double_t) (dedx.ndedx/100));
  if (opt != kTpcLengthCorrectionMDN) {
    Double_t dxLog2   = dedx.dedx[2];
    xx[0] = LogTrackLength;
    xx[1] = dxLog2;
  } else {
     Double_t LogNodEdx = TMath::Log((Double_t) (dedx.ndedx%100));
     xx[0] = LogNodEdx;
     xx[1] = etaG;
  }
  Double_t I70L;
  Int_t k = opt;
  if (! m_Corrections[k].Chair) return 0;
  Int_t l = 2*type;
  Int_t nrows = 0;
  const St_tpcCorrectionC *chairC = dynamic_cast<const St_tpcCorrectionC *>(m_Corrections[k].Chair);
  switch (k) {
  case kTpcLengthCorrection:
    nrows = chairC->nrows(l);
    switch (type) {
    case 0: // I70
    case 1: // dNdx
    case 2: // fit
      if (nrows > l+1) {
	dedx.dedx[0]   *= TMath::Exp(-chairC->CalcCorrection(  l,LogTrackLength));
	dedx.dedx[1]    =             chairC->CalcCorrection(l+1,LogTrackLength);
      }
      if (nrows > l+6) {
	dedx.dedx[0]   *= TMath::Exp(-chairC->CalcCorrection(l+6,LogTrackLength));
      }
      break;
    default:
      break;
    }
    break;
  case kTpcLengthCorrectionMDF:
  case kTpcLengthCorrectionMD2:
  case kTpcLengthCorrectionMDN:
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
    if (I70L > 0) dedx.dedx[0] *= TMath::Exp(-chairC->SumSeries(0,I70L));
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
  cout << "Npads/Ntbks " << mdEdx->Npads << "/" << mdEdx->Ntbks 
       << "\tdrift distance / O2 / O2W " << mdEdx->ZdriftDistance << "/" << mdEdx->ZdriftDistanceO2 << "/" << mdEdx->ZdriftDistanceO2W << endl;
  cout << "Local xyz " << mdEdx->xyz[0] << "\t" << mdEdx->xyz[1] << "\t" << mdEdx->xyz[2] << endl;
  cout << "Local xyzD " << mdEdx->xyzD[0] << "\t" << mdEdx->xyzD[1] << "\t" << mdEdx->xyzD[2] << endl;
  TString Line;
  for (Int_t k = (Int_t)kUncorrected; k <= ((Int_t)kTpcLast); k++) {
    Line  = Form("%2i",k);
    static Double_t log10keV = TMath::Log10(1e6);
    if (k <= (Int_t) kTpcLast) {
      Line += Form("\tdE %10.5g keV", 1e6*mdEdx->C[k].dE);
      Line += Form("\tdx %10.5g cm",mdEdx->C[k].dx);
      Line += Form("\tdE/dx  %10.5g keV/cm", 1e6*mdEdx->C[k].dEdx);
      Line += Form("\tlog(dE/dx)  %10.5g",mdEdx->C[k].dEdxL + log10keV);
      Line += Form("\tdlog(dE/dx) %10.5g",mdEdx->C[k].ddEdxL);
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
