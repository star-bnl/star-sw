#include <assert.h>
#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
//___________________Calibrations/ftpc_____________________________________________________________
#include "StDetectorDbFTPCGas.h"
StDetectorDbFTPCGas* StDetectorDbFTPCGas::fgInstance = 0; 
#include "St_ftpcGasSystemC.h"
MakeChairInstance(ftpcGasSystem,Calibrations/ftpc/ftpcGasSystem);
#include "St_ftpcGasOutC.h"
MakeChairInstance(ftpcGasOut,Calibrations/ftpc/ftpcGasOut);
#include "St_ftpcVoltageC.h"
MakeChairInstance(ftpcVoltage,Calibrations/ftpc/ftpcVoltage);
#include "St_ftpcVoltageStatusC.h"
MakeChairInstance(ftpcVoltageStatus,Calibrations/ftpc/ftpcVoltageStatus);
//__________________Calibrations/tpc______________________________________________________________
#include "St_tpcGasC.h"
MakeChairInstance(tpcGas,Calibrations/tpc/tpcGas);
#include "St_tpcGridLeakC.h"
MakeChairInstance(tpcGridLeak,Calibrations/tpc/tpcGridLeak);
#include "St_tpcOmegaTauC.h"
MakeChairInstance(tpcOmegaTau,Calibrations/tpc/tpcOmegaTau);
#include "St_tpcDriftVelocityC.h"
MakeChairInstance(tpcDriftVelocity,Calibrations/tpc/tpcDriftVelocity);
#include "St_TpcSecRowCorC.h"
ClassImp(St_TpcSecRowCorC);
#include "St_TpcSecRowBC.h"
MakeChairInstance2(TpcSecRowCor,St_TpcSecRowBC,Calibrations/tpc/TpcSecRowB);
#include "St_TpcSecRowCC.h"
MakeChairInstance2(TpcSecRowCor,St_TpcSecRowCC,Calibrations/tpc/TpcSecRowC);
#include "St_TpcSecRowXC.h"
MakeChairInstance2(TpcSecRowCor,St_TpcSecRowXC,Calibrations/tpc/TpcSecRowX);
#include "St_tpcCorrectionC.h"
ClassImp(St_tpcCorrectionC);
//________________________________________________________________________________
Double_t St_tpcCorrectionC::CalcCorrection(Int_t i, Double_t x, Double_t z) {
  tpcCorrection_st *cor =  ((St_tpcCorrection *) Table())->GetTable() + i;
  return SumSeries(cor, x, z);
}
//________________________________________________________________________________
Double_t St_tpcCorrectionC::SumSeries(tpcCorrection_st *cor,  Double_t x, Double_t z) {
  Double_t Sum = 0;
  if (! cor) return Sum;
  Int_t N = TMath::Abs(cor->npar)%100;
  if (N == 0) return Sum;
  static Double_t T0, T1, T2;
  // parameterization variable
  Double_t X;
  if (cor->npar  < 0) X = TMath::Exp(x);
  else {
    switch  (cor->type) {
    case 10:// ADC correction offset + poly for ADC
    case 11:// ADC correction offset + poly for log(ADC) and |Z|  
      X = TMath::Log(x);      break;
    case 1: // Tchebyshev [-1,1] 
      if (cor->min < cor->max)   X = -1 + 2*TMath::Max(0.,TMath::Min(1.,(X - cor->min)/( cor->max - cor->min)));
      break;
    case 2: // Shifted TChebyshev [0,1]
      if (cor->min < cor->max)   X = TMath::Max(0.,TMath::Min(1.,(X - cor->min)/( cor->max - cor->min)));
      break;
    case 3: 
      if (TMath::Abs(TMath::Abs(x) - 1) < 1.e-7) X = 0;
      else                                       X = TMath::Log(1. - TMath::Abs(x));
      break;
    case 4:
      if (TMath::Abs(TMath::Abs(x) - 1) < 1.e-7) X = 0;
      else                                       X = TMath::Sign(TMath::Log(1. - TMath::Abs(x)),x);
      break;
    default:      X = x;    break;
    }
  }
  if (cor->type != 1 && cor->type != 2 &&
      cor->min < cor->max) {
    if (X < cor->min) X = cor->min;
    if (X > cor->max) X = cor->max;
  }
  switch (cor->type) {
  case 1: // Tchebyshev [-1,1] 
    T0 = 1;
    Sum = cor->a[0]*T0;
    if (N == 1) break;
    T1 = X;
    Sum += cor->a[1]*T1;
    for (int n = 2; n <= N; n++) {
      T2 = 2*X*T1 - T0;
      Sum += cor->a[n]*T2;
      T0 = T1;
      T1 = T2;
    }
    break;
  case 2: // Shifted TChebyshev [0,1]
    T0 = 1;
    Sum = cor->a[0]*T0;
    if (N == 1) break;
    T1 = 2*X - 1;
    Sum += cor->a[1]*T1;
    for (int n = 2; n <= N; n++) {
      T2 = 2*(2*X - 1)*T1 - T0;
      Sum += cor->a[n]*T2;
      T0 = T1;
      T1 = T2;
    }
    break;
  case 10: // ADC correction offset + poly for ADC
    Sum = cor->a[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + cor->a[n];
    Sum += TMath::Log(1. + cor->OffSet/x);
    Sum  = TMath::Exp(Sum);
    Sum *= x;
    break;
  case 11: // ADC correction offset + poly for log(ADC) and |Z|
    Sum = cor->a[1] + z*cor->a[2] + z*X*cor->a[3] + TMath::Exp(X*(cor->a[4] + X*(cor->a[5] + X*cor->a[6])));
    Sum *= TMath::Exp(-cor->a[0]);
    break;
  default: // polynomials
    Sum = cor->a[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + cor->a[n];
    break;
  }
  return Sum;
}
#include "St_TpcRowQC.h"
MakeChairInstance2(tpcCorrection,St_TpcRowQC,Calibrations/tpc/TpcRowQ);
#include "St_TpcDriftDistOxygenC.h"
MakeChairInstance2(tpcCorrection,St_TpcDriftDistOxygenC,Calibrations/tpc/TpcDriftDistOxygen);
#include "St_TpcMultiplicityC.h"
MakeChairInstance2(tpcCorrection,St_TpcMultiplicityC,Calibrations/tpc/TpcMultiplicity);
#include "St_TpcZCorrectionBC.h"
MakeChairInstance2(tpcCorrection,St_TpcZCorrectionBC,Calibrations/tpc/TpcZCorrectionB);
#include "St_TpcdXCorrectionBC.h"
MakeChairInstance2(tpcCorrection,St_TpcdXCorrectionBC,Calibrations/tpc/TpcdXCorrectionB);
#include "St_tpcPressureBC.h"
MakeChairInstance2(tpcCorrection,St_tpcPressureBC,Calibrations/tpc/tpcPressureB);
#include "St_tpcMethaneInC.h"
MakeChairInstance2(tpcCorrection,St_tpcMethaneInC,Calibrations/tpc/tpcMethaneIn);
#include "St_tpcGasTemperatureC.h"
MakeChairInstance2(tpcCorrection,St_tpcGasTemperatureC,Calibrations/tpc/tpcGasTemperature);
#include "St_tpcWaterOutC.h"
MakeChairInstance2(tpcCorrection,St_tpcWaterOutC,Calibrations/tpc/tpcWaterOut);
#include "St_TpcdChargeC.h"
MakeChairInstance2(tpcCorrection,St_TpcdChargeC,Calibrations/tpc/TpcdCharge);
#include "St_TpcZDCC.h"
MakeChairInstance2(tpcCorrection,St_TpcZDCC,Calibrations/tpc/TpcZDC);
#include "St_TpcSpaceChargeC.h"
MakeChairInstance2(tpcCorrection,St_TpcSpaceChargeC,Calibrations/tpc/TpcSpaceCharge);
#include "St_TpcPhiDirectionC.h"
MakeChairInstance2(tpcCorrection,St_TpcPhiDirectionC,Calibrations/tpc/TpcPhiDirection);
#include "St_TpcdEdxCorC.h"
MakeChairInstance2(tpcCorrection,St_TpcdEdxCorC,Calibrations/tpc/TpcdEdxCor);
#include "St_TpcLengthCorrectionBC.h"
MakeChairInstance2(tpcCorrection,St_TpcLengthCorrectionBC,Calibrations/tpc/TpcLengthCorrectionB);


#include "St_tpcEffectiveGeomC.h"
MakeChairInstance(tpcEffectiveGeom,Calibrations/tpc/tpcEffectiveGeom);
#include "St_tpcElectronicsC.h"
MakeChairInstance(tpcElectronics,Calibrations/tpc/tpcElectronics);
#include "St_tpcPedestalC.h"
MakeChairInstance(tpcPedestal,Calibrations/tpc/tpcPedestal);
#include "St_tpcPadResponseC.h"
MakeChairInstance(tpcPadResponse,Calibrations/tpc/tpcPadResponse);
#include "St_tpcSlowControlSimC.h"
MakeChairInstance(tpcSlowControlSim,Calibrations/tpc/tpcSlowControlSim);
#include "St_tpcHitErrorsC.h"
MakeChairInstance(tpcHitErrors,Calibrations/tpc/tpcHitErrors);
#include "St_tpcGainMonitorC.h"
MakeChairInstance(tpcGainMonitor,Calibrations/tpc/tpcGainMonitor);
#include "St_tpcHighVoltagesC.h"
MakeChairInstance(tpcHighVoltages,Calibrations/tpc/tpcHighVoltages);
#include "St_tpcPadrowT0C.h"
MakeChairInstance(tpcPadrowT0,Calibrations/tpc/tpcPadrowT0);
#include "St_tpcSectorT0offsetC.h"
MakeChairInstance(tpcSectorT0offset,Calibrations/tpc/tpcSectorT0offset);
#include "St_TpcAltroParametersC.h"
MakeChairInstance(TpcAltroParameters,Calibrations/tpc/TpcAltroParameters);
#include "St_tpcAltroParamsC.h"
MakeChairInstance(tpcAltroParams,Calibrations/tpc/tpcAltroParams);
#include "St_asic_thresholdsC.h"
MakeChairInstance(asic_thresholds,Calibrations/tpc/asic_thresholds);
#include "St_asic_thresholds_tpxC.h"
MakeChairInstance(asic_thresholds_tpx,Calibrations/tpc/asic_thresholds_tpx);
#include "St_tpcAnodeHVC.h"
MakeChairInstance(tpcAnodeHV,Calibrations/tpc/tpcAnodeHV);
//________________________________________________________________________________
void  St_tpcAnodeHVC::sockets(Int_t sector, Int_t padrow, Int_t &e1, Int_t &e2, Float_t &f2) {
  e1 = (sector-1)*19;
  e2 = e1;
  f2 = 0;
  // sector=1..24 , padrow=1..45
  // f2 represents signal couplings from neighboring HV sections
  // see: http://www.star.bnl.gov/public/tpc/hard/signals/signal_division.html
  switch (padrow) {
    case  1: e1+= 1; e2+= 2; f2 = 0.00197; break;
    case  2: e1+= 2; break;
    case  3: e1+= 3; e2+= 2; f2 = 0.04547; break;
    case  4: e1+= 3; break;
    case  5: e1+= 4; break;
    case  6: e1+= 4; e2+= 5; f2 = 0.00007; break;
    case  7: e1+= 5; break;
    case  8: e1+= 6; e2+= 5; f2 = 0.04547; break;
    case  9: e1+= 6; break;
    case 10: e1+= 7; break;
    case 11: e1+= 8; e2+= 7; f2 = 0.33523; break;
    case 12: e1+= 8; break;
    case 13: e1+=17; break;
    case 14: e1+= 9; e2+=18; f2 = 0.00312; break;
    case 15:
    case 16: e1+= 9; break;
    case 17: e1+= 9; e2+=10; f2 = 0.40250; break;
    case 18:
    case 19:
    case 20: e1+=10; break;
    case 21: e1+=10; e2+=11; f2 = 0.40250; break;
    case 22:
    case 23:
    case 24: e1+=11; break;
    case 25: e1+=11; e2+=12; f2 = 0.40250; break;
    case 26:
    case 27:
    case 28: e1+=12; break;
    case 29: e1+=12; e2+=13; f2 = 0.40250; break;
    case 30:
    case 31:
    case 32: e1+=13; break;
    case 33: e1+=13; e2+=14; f2 = 0.40250; break;
    case 34:
    case 35:
    case 36: e1+=14; break;
    case 37: e1+=14; e2+=15; f2 = 0.40250; break;
    case 38:
    case 39:
    case 40: e1+=15; break;
    case 41: e1+=15; e2+=16; f2 = 0.40250; break;
    case 42:
    case 43:
    case 44: e1+=16; break;
    case 45: e1+=16; e2+=19; f2 = 0.40250; break;
    default: e1 = 0; e2 = 0; f2 = 0;
  }
}
//________________________________________________________________________________
Float_t St_tpcAnodeHVC::voltagePadrow(Int_t sector, Int_t padrow) const {
  Int_t e1 = 0, e2 = 0;
  Float_t f2 = 0;
  St_tpcAnodeHVC::sockets(sector, padrow, e1, e2, f2);
  if (e1==0) return -99;
  Float_t v1=voltage(e1-1);
  if (f2==0) return v1;
  Float_t v2=voltage(e2-1);
  if (v2==v1) return v1;
  // different voltages on influencing HVs
  // effective voltage is a sum of exponential gains
  Float_t B = (padrow <= 13 ? 13.05e-3 : 10.26e-3);
  Float_t v_eff = TMath::Log((1.0-f2)*TMath::Exp(B*v1) + f2*TMath::Exp(B*v2)) / B;
  return v_eff;
}
#include "St_tpcAnodeHVavgC.h"
MakeChairInstance(tpcAnodeHVavg,Calibrations/tpc/tpcAnodeHVavg);
//________________________________________________________________________________
Float_t St_tpcAnodeHVavgC::voltagePadrow(Int_t sector, Int_t padrow) const {
  Int_t e1 = 0, e2 = 0;
  Float_t f2 = 0;
  St_tpcAnodeHVC::sockets(sector, padrow, e1, e2, f2);
  if (e1==0) return -99;
  Float_t v1=voltage(e1-1);
  if (f2==0) return v1;
  Float_t v2=voltage(e2-1);
  if (v2==v1) return v1;
  // different voltages on influencing HVs
  // effective voltage is a sum of exponential gains
  Float_t B = (padrow <= 13 ? 13.05e-3 : 10.26e-3);
  Float_t v_eff = TMath::Log((1.0-f2)*TMath::Exp(B*v1) + f2*TMath::Exp(B*v2)) / B;
  return v_eff;
}
//________________________________________________________________________________
#include "St_tpcPadGainT0C.h"
MakeChairInstance(tpcPadGainT0,Calibrations/tpc/tpcPadGainT0);
#include "St_tpcSlewingC.h"
MakeChairInstance(tpcSlewing,Calibrations/tpc/tpcSlewing);
#include "St_tpcAcChargeC.h"
MakeChairInstance(tpcAcCharge,Calibrations/tpc/tpcAcCharge);
#include "St_tpcAvCurrentC.h"
MakeChairInstance(tpcAvCurrent,Calibrations/tpc/tpcAvCurrent);
#include "St_TpcResponseSimulatorC.h"
MakeChairInstance(TpcResponseSimulator,Calibrations/tpc/TpcResponseSimulator);
//__________________Calibrations/trg______________________________________________________________
#include "St_defaultTrgLvlC.h"
MakeChairInstance(defaultTrgLvl,Calibrations/trg/defaultTrgLvl);
#include "St_trigDetSumsC.h"
St_trigDetSumsC *St_trigDetSumsC::fgInstance = 0;
ClassImp(St_trigDetSumsC);
//___________________tpc_____________________________________________________________
#include "St_tss_tssparC.h"
MakeChairInstance(tss_tsspar,tpc/tsspars/tsspar);
Float_t St_tss_tssparC::gain_in(Int_t i) {
  return Struct(i)->gain_in;
}
//________________________________________________________________________________
Float_t St_tss_tssparC::gain_in(Int_t sec, Int_t row) {
  Float_t V = St_tpcAnodeHVavgC::instance()->voltagePadrow(sec,row);
  //  Float_t V = St_tpcAnodeHVC::instance()->voltagePadrow(sec,row);
  /* VoltageGFRunIX24DEV.root
     FitP->Draw("mu:y-1170>>I(20,-180,20)","(i&&j&&prob>0.01&&i<=13&&abs(mu)<0.4)/(dmu*dmu)","profg")
     I->Fit("pol1","er","",-100,0)
 FCN=37.5062 FROM MINOS     STATUS=SUCCESSFUL     10 CALLS          64 TOTAL
                     EDM=4.53972e-22    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0          -5.60237e-02   4.91833e-03  -4.98627e-12  -5.46491e-10
   2  p1          -1.13903e-03   8.15059e-05   8.15059e-05  -9.89312e-08

VoltageCGFRunIX29DEV
root.exe [33] FitP->Draw("mu:y-1170>>I(30,-120,30)","(i&&j&&i<=13&&abs(mu)<0.4)/(dmu*dmu)","profg")
(Long64_t)310
root.exe [34] I->Fit("pol3","e")
 FCN=260.02 FROM MINOS     STATUS=SUCCESSFUL     28 CALLS         247 TOTAL
                     EDM=1.48134e-09    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           1.20357e-03   1.68513e-04   3.08081e-10  -5.47566e-08
   2  p1           3.79402e-04   2.49341e-05   6.09866e-11  -9.80306e-07
   3  p2          -2.88086e-06   7.88355e-07   7.52247e-13   2.24169e-04
   4  p3          -1.58229e-07   5.63788e-09   5.63788e-09  -3.41781e-02
     
   */
  Float_t gain = 0;
  if (V > 0) {
    Double_t v = V - 1170;
    gain  = gain_in();
    gain *= TMath::Exp(v*(13.087e-3 -1.13903e-03 -8.47293e-04)+
		       1.20357e-03 + v*(3.79402e-04 + v*(-2.88086e-06 -1.58229e-07*v)));
  }
  return gain;
}
//________________________________________________________________________________
Float_t St_tss_tssparC::gain_out(Int_t i) 	{
  return Struct(i)->gain_out;
}
//________________________________________________________________________________
Float_t St_tss_tssparC::gain_out(Int_t sec, Int_t row) {
  Float_t V = St_tpcAnodeHVavgC::instance()->voltagePadrow(sec,row);
  //  Float_t V = St_tpcAnodeHVC::instance()->voltagePadrow(sec,row);
  /* VoltageGFRunIX24DEV.root
     FitP->Draw("mu:y>>O(28,1260,1400)","(i&&j&&prob>0.01&&i>13&&y>1160&&abs(mu)<0.5)/(dmu*dmu)","profg");
     TF1 *f = new TF1("f","[0]+(x-1390)*([1]+(x-1390)*[2])");
 FCN=62.5827 FROM MIGRAD    STATUS=CONVERGED      66 CALLS          67 TOTAL
                     EDM=7.03069e-21    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           1.52940e-02   2.33532e-03   3.81142e-06   4.66062e-08
   2  p1          -7.77617e-04   8.75985e-05   6.74118e-08   6.85122e-07
   3  p2           9.01092e-06   8.28886e-07   1.42918e-09  -1.86438e-04
   VoltageCGFRunIX29DEV
   FitP->Draw("mu:y-1390>>O(16,-300,20)","(i&&j&&i>13&&abs(mu)<0.4)/(dmu*dmu)","profg")
 O->Fit("pol4","er","",-300,20)
 FCN=3042.89 FROM MINOS     STATUS=SUCCESSFUL     36 CALLS         292 TOTAL
                     EDM=1.0216e-10    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           3.94933e-04   9.26073e-05   9.41672e-10   5.17033e-07
   2  p1           3.68859e-05   7.83681e-06  -8.32111e-11  -1.22507e-05
   3  p2          -2.70508e-06   2.10380e-07  -1.94201e-12   2.17851e-03
   4  p3          -6.01231e-08   1.90217e-09  -8.53783e-15  -5.08756e-01
   5  p4          -2.15159e-10   5.03934e-12   5.03934e-12   3.50816e+01
   */
  Float_t gain = 0;
  if (V > 0) {
    Double_t v = V - 1390;
    gain  = gain_out();
    gain *= TMath::Exp(v*(10.211e-3 + (-7.77617e-04 + v*9.01092e-06) ) +
		       3.94933e-04 + v*(3.68859e-05+v*(-2.70508e-06+v*(-6.01231e-08-2.15159e-10*v))));
  }
  return gain;
}
//________________________________________________________________________________
Float_t St_tss_tssparC::gain(Int_t sec, Int_t row) {
  return row <= 13 ? gain_in(sec,row) : gain_out(sec,row);
}
//__________________Calibrations/tracker______________________________________________________________
#include "St_tpcMaxHitsC.h"
MakeChairInstance(tpcMaxHits,Calibrations/tracker/tpcMaxHits);
//__________________Calibrations/rich______________________________________________________________
#include "StDetectorDbRichScalers.h"
StDetectorDbRichScalers *StDetectorDbRichScalers::fgInstance = 0;
ClassImp(StDetectorDbRichScalers);
#include "St_richvoltagesC.h"
MakeChairInstance(richvoltages,Calibrations/rich/richvoltages);
#include "St_y1MultC.h"
MakeChairInstance(y1Mult,Calibrations/rich/y1Mult);
#include "St_spaceChargeCorC.h"
ClassImp(St_spaceChargeCorC);
MakeChairInstance2(spaceChargeCor,St_spaceChargeCorR1C,Calibrations/rich/spaceChargeCor);
MakeChairInstance2(spaceChargeCor,St_spaceChargeCorR2C,Calibrations/rich/spaceChargeCorR2);
//_________________RunLog_____________________________________________________________
#include "St_MagFactorC.h"
MakeChairInstance(MagFactor,RunLog/MagFactor);
//_________________RunLog/onl_______________________________________________________________
#include "St_starClockOnlC.h"
MakeChairInstance(starClockOnl,RunLog/onl/starClockOnl);
//________________________________________________________________________________
starClockOnl_st *St_starClockOnlC::Struct(Int_t i) {
  starClockOnl_st *s = ((St_starClockOnl* ) instance()->Table())->GetTable(); 
  Int_t N =  getNumRows(); // with i < 0 look for positive frequency
  if (i >= 0 && i < N) return s + i;
  for (Int_t j = 0; j < N; j++, s++) if (s->frequency > 0) break;
  assert(s->frequency > 0 && s->frequency < 1e7);
  return s;
}
#include "St_starMagOnlC.h"
MakeChairInstance(starMagOnl,RunLog/onl/starMagOnl);
#include "St_beamInfoC.h"
MakeChairInstance(beamInfo,RunLog/onl/beamInfo);
#include "St_tpcRDOMasksC.h"
MakeChairInstance(tpcRDOMasks,RunLog/onl/tpcRDOMasks);
//________________________________________________________________________________
UInt_t       St_tpcRDOMasksC::getSectorMask(UInt_t sector) {
  UInt_t MASK = 0x0000; // default is to mask it out
  //UInt_t MASK = 0xFFFF; // change to  ON by default ** THIS WAS A HACK
  if(sector < 1 || sector > 24 || getNumRows() == 0){
    LOG_WARN << "St_tpcRDOMasksC:: getSectorMask : return default mask for " 
	     << "sector= " << sector << " getNumRows()=" << getNumRows() << endm;
    return MASK;
  }
  MASK = mask(((sector + 1) / 2) - 1); // does the mapping from sector 1-24 to packed sectors
  if( sector % 2 == 0){ // if its even relevent bits are 6-11
    MASK = MASK >> 6;
  }
  // Otherwise want lower 6 bits    
  MASK &= 0x000003F; // Mask out higher order bits
  if (sector == 16 && MASK == 0 && runNumber() > 8181000 && runNumber() < 9181000) MASK = 4095;
  return MASK;
}
//________________________________________________________________________________
#include "St_triggerInfoC.h"
MakeChairInstance(triggerInfo,RunLog/onl/triggerInfo);
#include "St_triggerIDC.h"
MakeChairInstance(triggerID,RunLog/onl/triggerID);
//________________________________________________________________________________
#include "St_trigPrescalesC.h"
MakeChairOptionalInstance(trigPrescales,RunLog/onl/trigPrescales);
//________________________________________________________________________________
#include "St_L0TriggerInfoC.h"
MakeChairInstance(L0TriggerInfo,RunLog/onl/L0TriggerInfo);
#include "St_trigL3ExpandedC.h"
MakeChairOptionalInstance(trigL3Expanded,RunLog/onl/trigL3Expanded);
#include "St_dsmPrescalesC.h"
MakeChairOptionalInstance(dsmPrescales,RunLog/onl/dsmPrescales);
#include "St_additionalTriggerIDC.h"
MakeChairOptionalInstance(additionalTriggerID,RunLog/onl/additionalTriggerID);
#include "StDetectorDbTriggerID.h"
StDetectorDbTriggerID *StDetectorDbTriggerID::fgInstance = 0;
//________________________________________________________________________________
map<Int_t,Float_t> StDetectorDbTriggerID::getTotalPrescales() {
  map<Int_t,Float_t> value;
  // First walk forward through the multiple levels of prescales
  for (UInt_t irow=0;irow<getDsmPrescalesNumRows(); ++irow) {
    Int_t trgId = getDsmPrescalesTrgId(irow);
    value[trgId] = Float_t(getDsmPrescalesDsmPrescale(irow));
  }
  
  for (UInt_t irow=0; irow<getL0NumRows(); ++irow) {
    Int_t trgId = getL0OfflineTrgId(irow);
    map<Int_t,Float_t>::iterator p=value.find(trgId);
    if (p != value.end()) {
      (*p).second *= Float_t(getPsL0(irow));
    }
    else {
      value[trgId] = Float_t(getPsL0(irow));
    }
  }
  // For completeness: this one is always unity as far as I can tell
  for (UInt_t irow=0; irow<getSNumRows(); ++irow) {
    UInt_t idxTrigger = getIdxTrigger(irow);
    Int_t trgId = 0;
    for (UInt_t jrow=0; jrow<getIDNumRows(); ++jrow) {
      if (idxTrigger == getIdxTrg(jrow)) {
	trgId = getOfflineTrgId(jrow);
	break;
      }
    }
    map<Int_t,Float_t>::iterator p=value.find(trgId);
    
    if (p != value.end()) {
      (*p).second *= Float_t(getPs(irow));
    }
    else {
      value[trgId] = Float_t(getPs(irow));
    }
  }
  
  // Now deal with L3Expanded
  for (UInt_t irow=0; irow<getTrigL3ExpandedNumRows(); ++irow) {
    Int_t oldtid = getTrigL3ExpandedL3TrgId(irow);
    Int_t newtid = getTrigL3ExpandedL3ExpandedTrgId(irow);
    Float_t l2ps = getTrigL3ExpandedL2Ps(irow);
    
    map<Int_t,Float_t>::iterator p = value.find(oldtid);
    if (p!= value.end()) {
      value[newtid] = ((*p).second)*l2ps;
    }
    else {
      value[newtid] = l2ps;
    }
    
  }
  return value;
}
//________________________________________________________________________________
Float_t StDetectorDbTriggerID::getTotalPrescaleByTrgId(Int_t trgId) {
  map<Int_t,Float_t> theMap = getTotalPrescales();
  map<Int_t,Float_t>::const_iterator p = theMap.find(trgId);
  if (p != theMap.end()) {
    return (*p).second;
  }
  else {
    return 0;
  }
}
//________________________________________________________________________________
#include "StDetectorDbIntegratedTriggerID.h"
StDetectorDbIntegratedTriggerID *StDetectorDbIntegratedTriggerID::fgInstance = 0;
//___________________Conditions/trg_____________________________________________________________
#include "St_trgTimeOffsetC.h"
MakeChairInstance(trgTimeOffset,Conditions/trg/trgTimeOffset);
//___________________Geometry/tpc_____________________________________________________________
#include "St_tpcDimensionsC.h"
MakeChairInstance(tpcDimensions,Geometry/tpc/tpcDimensions);
#include "St_tpcWirePlanesC.h"
MakeChairInstance(tpcWirePlanes,Geometry/tpc/tpcWirePlanes);
#include "St_tpcSectorPositionC.h"
ClassImp(St_tpcSectorPositionC); 
St_tpcSectorPositionC *St_tpcSectorPositionC::fgInstance = 0; 
St_tpcSectorPosition  *St_tpcSectorPositionC::fgTables[24] = {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0};
St_tpcSectorPositionC *St_tpcSectorPositionC::instance() { 
  if (fgInstance) return fgInstance;					
  TDataSet *tpc = StMaker::GetChain()->GetDataBase("Geometry/tpc");
  assert(tpc);
  for (Int_t sec = 1; sec <= 24; sec++) {
    TString path = Form("Sector_%02i/tpcSectorPosition",sec);
    fgTables[sec-1] = (St_tpcSectorPosition  *) tpc->Find(path.Data()); 
    if (! fgTables[sec-1]) {							
      LOG_WARN << "St_tpcSectorPositionC::instance " << tpc->GetTitle() << "/" << path.Data() 
	       << "\twas not found" << endm; 
      assert(fgTables[sec-1]);							
    }						
    {						
      TDatime t[2];							
      St_db_Maker::GetValidity(fgTables[sec-1],t);					        
      Int_t Nrows = fgTables[sec-1]->GetNRows();					
      LOG_WARN << "St_tpcSectorPositionC::instance found table " << fgTables[sec-1]->GetName() 
	       << " with NRows = " << Nrows << " in db" << endm;		
      LOG_WARN << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()	
	       << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm; 
      fgTables[sec-1]->Print(0,1);		
    }
  }			
  fgInstance = new St_tpcSectorPositionC();				
  return fgInstance;							
}
#include "St_tpcFieldCageC.h"
MakeChairInstance(tpcFieldCage,Geometry/tpc/tpcFieldCage);
#include "St_tpcPadPlanesC.h"
MakeChairInstance(tpcPadPlanes,Geometry/tpc/tpcPadPlanes);
#include "St_tpcGlobalPositionC.h"
MakeChairInstance(tpcGlobalPosition,Geometry/tpc/tpcGlobalPosition);
#include "St_tpcFieldCageShortC.h"
MakeChairInstance(tpcFieldCageShort,Geometry/tpc/tpcFieldCageShort);
#include "St_SurveyC.h"
#include "St_SurveyC.h"
ClassImp(St_SurveyC);
#include "StSvtSurveyC.h"
MakeChairInstance2(Survey,StSvtOnGlobal,Geometry/svt/SvtOnGlobal);
MakeChairInstance2(Survey,StSvtShellOnGlobal,Geometry/svt/ShellOnGlobal);
MakeChairInstance2(Survey,StSvtLadderOnSurvey,Geometry/svt/LadderOnSurvey);
MakeChairInstance2(Survey,StSvtLadderOnShell,Geometry/svt/LadderOnShell);
MakeChairInstance2(Survey,StSvtWaferOnLadder,Geometry/svt/WaferOnLadder);
#include "StSsdSurveyC.h"
MakeChairInstance2(Survey,StSsdOnGlobal,Geometry/ssd/SsdOnGlobal);
MakeChairInstance2(Survey,StSsdSectorsOnGlobal,Geometry/ssd/SsdSectorsOnGlobal);
MakeChairInstance2(Survey,StSsdLaddersOnSectors,Geometry/ssd/SsdLaddersOnSectors);
MakeChairInstance2(Survey,StSsdWafersOnLadders,Geometry/ssd/SsdWafersOnLadders);
//________________________________________________________________________________
void St_SurveyC::GetAngles(Double_t &phi, Double_t &the, Double_t &psi, Int_t i) {
  phi = the = psi = 0;  // Korn 14.10-5
  Double_t cosDelta = (r00(i) + r11(i) + r22(i) - 1)/2; // (Tr(R) - 1)/2
  Double_t Delta = TMath::ACos(cosDelta);      
  if (Delta < 0) Delta += 2*TMath::Pi();  
  Double_t sinDelta2 = TMath::Sin(Delta/2);
  if (TMath::Abs(sinDelta2) < 1.e-7) return;
  Double_t c[3] = {
    (r21(i) - r12(i))/(2*sinDelta2), // a32-a23
    (r02(i) - r20(i))/(2*sinDelta2), // a13-a31
    (r10(i) - r01(i))/(2*sinDelta2)  // a21-a12
  };
  Double_t u = TMath::ATan2(c[0],c[1]);
  Double_t v = TMath::ATan(c[2]*TMath::Tan(Delta/2));
  phi = (v - u)/2 - TMath::Pi()/2;
  psi = (v + u)/2 - TMath::Pi()/2;
  the = 2*TMath::ATan2(c[0]*TMath::Sin(v),c[2]*TMath::Sin(u));
  Double_t raddeg = 180./TMath::Pi();
  phi   *= raddeg;
  the   *= raddeg;
  psi   *= raddeg;
}
//________________________________________________________________________________
St_SurveyC   *St_SurveyC::instance(const Char_t *name) {
  TString Name(name);
  if (Name == "SvtOnGlobal")          return (St_SurveyC   *) StSvtOnGlobal::instance();	    
  if (Name == "ShellOnGlobal")        return (St_SurveyC   *) StSvtShellOnGlobal::instance();  
  if (Name == "LadderOnSurvey")       return (St_SurveyC   *) StSvtLadderOnSurvey::instance(); 
  if (Name == "LadderOnShell")        return (St_SurveyC   *) StSvtLadderOnShell::instance();  
  if (Name == "WaferOnLadder")        return (St_SurveyC   *) StSvtWaferOnLadder::instance();  
  if (Name == "SsdOnGlobal")          return (St_SurveyC   *) StSsdOnGlobal::instance();
  if (Name == "SsdSectorsOnGlobal")   return (St_SurveyC   *) StSsdSectorsOnGlobal::instance();
  if (Name == "SsdLaddersOnSectors")  return (St_SurveyC   *) StSsdLaddersOnSectors::instance();
  if (Name == "SsdWafersOnLadders")   return (St_SurveyC   *) StSsdWafersOnLadders::instance();
  return 0;
}
