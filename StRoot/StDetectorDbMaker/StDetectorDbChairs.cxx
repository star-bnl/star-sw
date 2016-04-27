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
Double_t St_tpcCorrectionC::CalcCorrection(Int_t i, Double_t x, Double_t z, Int_t NparMax) {
  tpcCorrection_st *cor =  ((St_tpcCorrection *) Table())->GetTable() + i;
  return SumSeries(cor, x, z, NparMax);
}
//________________________________________________________________________________
Double_t St_tpcCorrectionC::SumSeries(tpcCorrection_st *cor,  Double_t x, Double_t z, Int_t NparMax) {
  Double_t Sum = 0;
  if (! cor) return Sum;
  Int_t N = TMath::Abs(cor->npar)%100;
  if (N == 0) return Sum;
  if (NparMax > 0) N = NparMax;
  static Double_t T0, T1, T2;
  // parameterization variable
  Double_t X = x;
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
#include "St_TpcLengthCorrectionMDF.h"
MakeChairInstance2(MDFCorrection,St_TpcLengthCorrectionMDF,Calibrations/tpc/TpcLengthCorrectionMDF);
ClassImp(St_MDFCorrectionC);
//____________________________________________________________________
Double_t St_MDFCorrectionC::Eval(Int_t k, const Double_t x0, Double_t x1) const {
  Double_t x[2] = {x0, x1};
  return Eval(k,x);
}
//____________________________________________________________________
Double_t St_MDFCorrectionC::Eval(Int_t k, const Double_t *x) const {
  // Evaluate parameterization at point x. Optional argument coeff is
  // a vector of coefficients for the parameterisation, NCoefficients
  // elements long.
  assert(x);
  Double_t returnValue = DMean(k);
  Double_t term        = 0;
  UChar_t    i, j;
  for (i = 0; i < NCoefficients(k); i++) {
    // Evaluate the ith term in the expansion
    term = Coefficients(k)[i];
    for (j = 0; j < NVariables(k); j++) {
      // Evaluate the factor (polynomial) in the j-th variable.
      Int_t    p  =  Powers(k)[i * NVariables(k) + j];
      Double_t y  =  1 + 2. / (XMax(k)[j] - XMin(k)[j])
	* (x[j] - XMax(k)[j]);
      term        *= EvalFactor(k,p,y);
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}
//____________________________________________________________________
Double_t St_MDFCorrectionC::EvalError(Int_t k, const Double_t *x) const {
  // Evaluate parameterization error at point x. Optional argument coeff is
  // a vector of coefficients for the parameterisation, NCoefficients(k)
  // elements long.
  assert(x);
  Double_t returnValue = 0;
  Double_t term        = 0;
  UChar_t    i, j;
#if 0
  for (i = 0; i < NCoefficients(k); i++) {
    //     cout << "Error coef " << i << " -> " << CoefficientsRMS(k)[i] << endl;
  }
#endif
  for (i = 0; i < NCoefficients(k); i++) {
    // Evaluate the ith term in the expansion
    term = CoefficientsRMS(k)[i];
    for (j = 0; j < NVariables(k); j++) {
      // Evaluate the factor (polynomial) in the j-th variable.
      Int_t    p  =  Powers(k)[i * NVariables(k) + j];
      Double_t y  =  1 + 2. / (XMax(k)[j] - XMin(k)[j])
	* (x[j] - XMax(k)[j]);
      term        *= EvalFactor(p,y);
      //	 cout << "i,j " << i << ", " << j << "  "  << p << "  " << y << "  " << EvalFactor(p,y) << "  " << term << endl;
    }
    // Add this term to the final result
    returnValue += term*term;
    //      cout << " i = " << i << " value = " << returnValue << endl;
  }
  returnValue = TMath::Sqrt(returnValue);
  return returnValue;
}
//____________________________________________________________________
Double_t St_MDFCorrectionC::EvalFactor(Int_t k, Int_t p, Double_t x) const {
  // Evaluate function with power p at variable value x
  Int_t    i   = 0;
  Double_t p1  = 1;
  Double_t p2  = 0;
  Double_t p3  = 0;
  Double_t r   = 0;
  
  switch(p) {
  case 1:
    r = 1;
    break;
  case 2:
    r =  x;
    break;
  default:
    p2 = x;
    for (i = 3; i <= p; i++) {
      p3 = p2 * x;
      if (PolyType(k) == kLegendre)
	p3 = ((2 * i - 3) * p2 * x - (i - 2) * p1) / (i - 1);
      else if (PolyType(k) == kChebyshev)
	p3 = 2 * x * p2 - p1;
      p1 = p2;
      p2 = p3;
    }
    r = p3;
  }
  return r;
}
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
#include "St_tpcPadPlanesC.h"
//________________________________________________________________________________
void  St_tpcAnodeHVC::sockets(Int_t sector, Int_t padrow, Int_t &e1, Int_t &e2, Float_t &f2) {
  if (St_tpcPadPlanesC::instance()->padRows() != 45) {
    if (padrow <= St_tpcPadPlanesC::instance()->innerPadRows()) {e1 = e2 = 8; f2 = 0;}
    else                                                        {e1 = e2 = 9; f2 = 0;}
    return;
  }
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
  if (f2 < 0.1) return v1;
  Float_t v2=voltage(e2-1);
  if (TMath::Abs(v2 - v1) > 40) return -99;
  if (TMath::Abs(v2 - v1) <  1) return v1;
  // different voltages on influencing HVs
  // effective voltage is a sum of exponential gains
  Float_t B = (padrow <= St_tpcPadPlanesC::instance()->innerPadRows() ? 13.05e-3 : 10.26e-3);
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
  Float_t B = (padrow <= St_tpcPadPlanesC::instance()->innerPadRows() ? 13.05e-3 : 10.26e-3);
  Float_t v_eff = TMath::Log((1.0-f2)*TMath::Exp(B*v1) + f2*TMath::Exp(B*v2)) / B;
  return v_eff;
}
//________________________________________________________________________________
#include "St_tpcPadGainT0C.h"
MakeChairInstance(tpcPadGainT0,Calibrations/tpc/tpcPadGainT0);
#include "St_tpcPadGainT0BC.h"
MakeChairInstance(tpcPadGainT0B,Calibrations/tpc/tpcPadGainT0B);
#include "St_tpcSlewingC.h"
MakeChairInstance(tpcSlewing,Calibrations/tpc/tpcSlewing);
#include "St_tpcAcChargeC.h"
MakeChairInstance(tpcAcCharge,Calibrations/tpc/tpcAcCharge);
#include "St_tpcAvCurrentC.h"
MakeChairInstance(tpcAvCurrent,Calibrations/tpc/tpcAvCurrent);
#include "St_TpcResponseSimulatorC.h"
MakeChairInstance(TpcResponseSimulator,Calibrations/tpc/TpcResponseSimulator);
#include "St_TpcPadCorrectionC.h"
#include "TPolynomial.h"
MakeChairInstance(TpcPadCorrection,Calibrations/tpc/TpcPadCorrection);
St_TpcPadCorrectionC::St_TpcPadCorrectionC(St_TpcPadCorrection *table) : TChair(table), fFunc(0) {
  Int_t nrows = GetNRows();
  if (nrows) {
    fFunc = new TF1*[nrows]; memset(fFunc, 0, sizeof(fFunc));
    for (Int_t i = 0; i < nrows; i++) {
      Short_t io = Struct(i)->InOut;
      Short_t np = Struct(i)->npads;
      Short_t MuOrSigma  = -1;
      if (Struct(i)->R == 8) MuOrSigma = 0;
      if (Struct(i)->R == 7) MuOrSigma = 1;
      if ((io < 1 || io > 2) ||
	  (np < 1 || np > 7) ||
	  (MuOrSigma < 0 || MuOrSigma > 1)) continue;
      Int_t  indx = 2*(7*(io-1) + np-1)+MuOrSigma;
      assert(indx < nrows);
      fFunc[indx] = TPolynomial::MakePoly(Form("%s_%i_%i",Struct(i)->Type,np,io),Struct(i)->N-1,Struct(i)->R);
      fFunc[indx]->SetParameters(&Struct(i)->a0);
    }
  }
}
St_TpcPadCorrectionC::~St_TpcPadCorrectionC() {
  fgInstance = 0; 
  if (fFunc) {
    Int_t nrows = GetNRows();
    for (Int_t i = 0; i < nrows; i++) 
      SafeDelete(fFunc[i]); 
    delete [] fFunc;
  }
}
#include "St_tpcGainCorrectionC.h"
MakeChairInstance2(tpcCorrection,St_tpcGainCorrectionC,Calibrations/tpc/tpcGainCorrection);
#include "StTpcHitErrors.h"
StTpcHitErrors *StTpcHitErrors::fgInstance = 0;
StTpcHitErrors *StTpcHitErrors::instance() {
  if (! fgInstance) {
    StMaker::GetChain()->GetDataBase("Calibrations/tpc/TpcHitErrors");
    cout << "StTpcHitErrors have been instantiated with" << endl
	 << "StTpcHitErrors fnXZ(" <<  fgInstance->fXZ << "," << fgInstance->fSec << "," << fgInstance->fRow << "," 
	 << fgInstance->fMS << "," << fgInstance->fPrompt << ") = " << fgInstance->fNxz << endl;
  }
  return fgInstance;
}
Double_t StTpcHitErrors::calcError(Int_t iXZ, Int_t sec, Int_t row, Double_t _z,  Double_t _eta, Double_t _tanl, Int_t Npads, Int_t Ntmbks, Double_t AdcL, Double_t xPad) {
  const static Double_t PitchLog[3] = {TMath::Log(0.335), TMath::Log(0.675), TMath::Log(5.78602945878541108e-01)};
  /*
    X[0] =            fit.Npads;
    X[1] =            fit.Ntmbks;
    X[2] = TMath::Tan(fit.phiL);
    X[3] = TMath::Tan(fit.dipL);
    X[4] =            fit.zL;
    X[5] =            fit.AdcL;
    X[6] =            fit.xPad;
  */
  Int_t s = 0, r = 0, p = 0;
  if (sec > 12) s = 1;
  if (row > St_tpcPadPlanesC::instance()->innerPadRows()) r = 1;
  Int_t pitch = s;
  if (iXZ) pitch = 2;
  Double_t Vars[7] = {
    (Double_t ) Npads, // 0 => no. of pads in cluster
    (Double_t ) Ntmbks,// 1 => no. of time buckets in cluster
    -TMath::Tan(_eta), // 2 => tan(phiL)
    _tanl,             // 3 => tan(dipL)
    _z,                // 4 => zL
    TMath::Log(AdcL),  // 5 => Adc counts
    xPad};             // 6=> xPad
  if (s) {// East 
    Vars[3] = - Vars[3];
    Vars[4] = - Vars[4];
  }
  if (Vars[3] > 195) p = 1;
  TMDFParameters *mdf = GetSigmaSQ(iXZ,s,r,p);
  assert(mdf);
  Double_t valueLog = mdf->Eval(Vars);
  return   TMath::Exp(2*(valueLog + PitchLog[pitch]));
}
#include "St_tpcStatusC.h"
MakeChairInstance(tpcStatus,Calibrations/tpc/tpcStatus);
#include "St_TpcAvgCurrentC.h"
MakeChairInstance(TpcAvgCurrent,Calibrations/tpc/TpcAvgCurrent);
//________________________________________________________________________________
Int_t St_TpcAvgCurrentC::ChannelFromRow(Int_t row) {
  if (row <  1 || row > St_tpcPadPlanesC::instance()->padRows()) return -1;
  if (St_tpcPadPlanesC::instance()->padRows() != 45) {
    if (row <= St_tpcPadPlanesC::instance()->innerPadRows()) return 1;
    else return 5;
  }
  if (row <  3) return 1;
  if (row <  7) return 2;
  if (row < 10) return 3;
  if (row < 14) return 4;
  if (row < 22) return 5;
  if (row < 30) return 6;
  if (row < 38) return 7;
  return 8;
}
//________________________________________________________________________________
Int_t St_TpcAvgCurrentC::ChannelFromSocket(Int_t socket) {
  Int_t channel = -1;
  switch (socket) {
  case 1:
  case 2 : channel = 1; break;
  case 3:
  case 4:  channel = 2; break;
  case 5:
  case 6:  channel = 3; break;
  case 7:
  case 8:
  case 17: channel = 4; break;
  case 9:
  case 10:
  case 18: channel = 5; break;
  case 11:
  case 12: channel = 6; break;
  case 13:
  case 14: channel = 7; break;
  case 15:
  case 16:
  case 19: channel = 8; break;
  default:              break;
  }
  return channel;
}
//________________________________________________________________________________
Float_t St_TpcAvgCurrentC::AcChargeL(Int_t sector, Int_t channel) {
  //  static const Double_t RA[2]        = { 154.484, 81.42}; // Outer/ Inner average Radii
  //  static const Double_t WireLenth[2] = {   3.6e5, 1.6e5}; 
  // L Inner = 190222, Outer = 347303
  static Float_t Length[8] = {
    1307.59, //   Channel 1 
    1650.57, //   Channel 2 
    1993.54, //   Channel 3 
    2974.24, //   Channel 4 
    3324.59, //   Channel 5 
    3202.42, //   Channel 6 
    3545.4 , //   Channel 7 
    4398.53};//   Channel 8 

  return AcCharge(sector,channel)/Length[channel-1];
}
//__________________Calibrations/trg______________________________________________________________
#include "St_defaultTrgLvlC.h"
MakeChairInstance(defaultTrgLvl,Calibrations/trg/defaultTrgLvl);
#include "St_trigDetSumsC.h"
St_trigDetSumsC *St_trigDetSumsC::fgInstance = 0;
ClassImp(St_trigDetSumsC);
//___________________tpc_____________________________________________________________
#include "St_tss_tssparC.h"
MakeChairInstance(tss_tsspar,tpc/tsspars/tsspar);
//________________________________________________________________________________
Float_t St_tss_tssparC::gain(Int_t sec, Int_t row) {
  Int_t l = 0;
  Double_t V_nominal = 1390;
  Float_t V = 0;
  Float_t gain = 0;
  if (row <= St_tpcPadPlanesC::instance()->innerPadRows()) {l = 1; V_nominal = 1170;}
  V = St_tpcAnodeHVavgC::instance()->voltagePadrow(sec,row);
  if (V > 0) {
    St_tpcGainCorrectionC *gC = St_tpcGainCorrectionC::instance();
    Double_t v = V - V_nominal;
    if (gC->GetNRows() < l || v < gC->min(l) || v > gC->max(l)) return gain;
    if (gC->min(l) < -150) {
      // if range was expanded below 150 V then use only the linear approximation
      gain  = TMath::Exp(gC->CalcCorrection(l,v, 0., 2));
    } else {
      gain  = TMath::Exp(gC->CalcCorrection(l,v));
    }
  }
  return gain;
}
//__________________Calibrations/tracker______________________________________________________________
#include "St_tpcMaxHitsC.h"
MakeChairInstance(tpcMaxHits,Calibrations/tracker/tpcMaxHits);
//__________________Calibrations/rich______________________________________________________________
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
MakeChairInstance(tpcPadPlanes,Geometry/tpc/tpcPadPlanes);
#include "St_tpcGlobalPositionC.h"
MakeChairInstance(tpcGlobalPosition,Geometry/tpc/tpcGlobalPosition);
#include "St_tpcFieldCageShortC.h"
MakeChairInstance(tpcFieldCageShort,Geometry/tpc/tpcFieldCageShort);
#include "St_tpcHVPlanesC.h"
MakeChairInstance(tpcHVPlanes,Geometry/tpc/tpcHVPlanes);
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
#include "StTpcSurveyC.h"
MakeChairInstance2(Survey,StTpcInnerSectorPosition,Geometry/tpc/TpcInnerSectorPosition);
MakeChairInstance2(Survey,StTpcOuterSectorPosition,Geometry/tpc/TpcOuterSectorPosition);
MakeChairInstance2(Survey,StTpcSuperSectorPosition,Geometry/tpc/TpcSuperSectorPosition);
MakeChairInstance2(Survey,StTpcHalfPosition,Geometry/tpc/TpcHalfPosition);
//________________________________________________________________________________
const TGeoHMatrix &St_SurveyC::GetMatrix(Int_t i) {
  static TGeoHMatrix rot;
  rot.SetRotation(Rotation(i));
  rot.SetTranslation(Translation(i));
  return *&rot;
}
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
  if (Name == "SvtOnGlobal")            return (St_SurveyC   *) StSvtOnGlobal::instance();	    
  if (Name == "ShellOnGlobal")        	return (St_SurveyC   *) StSvtShellOnGlobal::instance();  	
  if (Name == "LadderOnSurvey")       	return (St_SurveyC   *) StSvtLadderOnSurvey::instance(); 	
  if (Name == "LadderOnShell")        	return (St_SurveyC   *) StSvtLadderOnShell::instance();  	
  if (Name == "WaferOnLadder")        	return (St_SurveyC   *) StSvtWaferOnLadder::instance();  	
  if (Name == "SsdOnGlobal")          	return (St_SurveyC   *) StSsdOnGlobal::instance();	
  if (Name == "SsdSectorsOnGlobal")   	return (St_SurveyC   *) StSsdSectorsOnGlobal::instance();	
  if (Name == "SsdLaddersOnSectors")  	return (St_SurveyC   *) StSsdLaddersOnSectors::instance();
  if (Name == "SsdWafersOnLadders")   	return (St_SurveyC   *) StSsdWafersOnLadders::instance(); 
  if (Name == "TpcInnerSectorPosition") return (St_SurveyC   *) StTpcInnerSectorPosition::instance();
  if (Name == "TpcOuterSectorPosition") return (St_SurveyC   *) StTpcOuterSectorPosition::instance();
  if (Name == "TpcSuperSectorPosition") return (St_SurveyC   *) StTpcSuperSectorPosition::instance();
  if (Name == "TpcHalfPosition")        return (St_SurveyC   *) StTpcHalfPosition::instance();
  return 0;
}
//__________________Calibrations/rhic______________________________________________________________
#include "St_vertexSeedC.h"
MakeChairInstance(vertexSeed,Calibrations/rhic/vertexSeed);
//__________________Calibrations/tof______________________________________________________________
#include "St_tofGeomAlignC.h"
MakeChairInstance(tofGeomAlign,Calibrations/tof/tofGeomAlign);
#include "St_tofTrayConfigC.h"
MakeChairInstance(tofTrayConfig,Calibrations/tof/tofTrayConfig);
#include "St_tofStatusC.h"
#if 1
MakeChairInstance(tofStatus,Calibrations/tof/tofStatus);
#else
ClassImp(St_tofStatusC); 
St_tofStatusC *St_tofStatusC::fgInstance = 0; 
St_tofStatusC *St_tofStatusC::instance() { 
  if (! fgInstance) {
    St_tofStatus *table = (St_tofStatus *) StMaker::GetChain()->GetDataBase("Calibrations/tof/tofStatus"); 
    if (! table) {							
      LOG_WARN << "St_tofStatusC::instance " << "Calibrations/tof/tofStatus" << "\twas not found" 
	       << "c struct address " << table->GetTable()
	       << endm; 
      assert(table);							
    }									
    TDatime t[2];								
    if (St_db_Maker::GetValidity(table,t) > 0) {				
      Int_t Nrows = table->GetNRows();					
      LOG_WARN << "St_tofStatusC::instance found table " << table->GetName() 
	       << " with NRows = " << Nrows << " in db" << endm;		
      LOG_WARN << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()	
	       << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;	
      if (Nrows > 10) Nrows = 10;						
      static Int_t maxsize = 256;
      if (table->GetRowSize() < maxsize) 
	table->Print(0,Nrows);		
    }
    fgInstance = new St_tofStatusC(table);				
  }
  LOG_WARN << "St_tofStatusC::instance c struct address " << ((St_tofStatus *) fgInstance->Table())->GetTable() << endm;
  static Bool_t d = kFALSE;
  if (d) {
    fgInstance->Print(0,1);
  }
  return fgInstance;							
}
#endif
//____________________________Calibrations/emc____________________________________________________
#include "St_emcPedC.h"
MakeChairInstance2(emcPed,St_bemcPedC,Calibrations/emc/y3bemc/bemcPed);
MakeChairInstance2(emcPed,St_bprsPedC,Calibrations/emc/y3bprs/bprsPed);
#include "St_emcStatusC.h"
MakeChairInstance2(emcStatus,St_bemcStatusC,Calibrations/emc/y3bemc/bemcStatus);
MakeChairInstance2(emcStatus,St_bprsStatusC,Calibrations/emc/y3bprs/bprsStatus);
#include "St_emcCalibC.h"
MakeChairInstance2(emcCalib,St_bemcCalibC,Calibrations/emc/y3bemc/bemcCalib);
MakeChairInstance2(emcCalib,St_bprsCalibC,Calibrations/emc/y3bprs/bprsCalib);
#include "St_emcGainC.h"
MakeChairInstance2(emcGain,St_bemcGainC,Calibrations/emc/y3bemc/bemcGain);
MakeChairInstance2(emcGain,St_bprsGainC,Calibrations/emc/y3bprs/bprsGain);

#include "St_smdPedC.h"
MakeChairInstance2(smdPed,St_bsmdePedC,Calibrations/smd/y3bsmde/bsmdePed);
MakeChairInstance2(smdPed,St_bsmdpPedC,Calibrations/smd/y3bsmdp/bsmdpPed);
#include "St_smdStatusC.h"
MakeChairInstance2(smdStatus,St_bsmdeStatusC,Calibrations/smd/y3bsmde/bsmdeStatus);
MakeChairInstance2(smdStatus,St_bsmdpStatusC,Calibrations/smd/y3bsmdp/bsmdpStatus);
#include "St_smdCalibC.h"
MakeChairInstance2(smdCalib,St_bsmdeCalibC,Calibrations/smd/y3bsmde/bsmdeCalib);
MakeChairInstance2(smdCalib,St_bsmdpCalibC,Calibrations/smd/y3bsmdp/bsmdpCalib);
#include "St_smdGainC.h"
MakeChairInstance2(smdGain,St_bsmdeGainC,Calibrations/smd/y3bsmde/bsmdeGain);
MakeChairInstance2(smdGain,St_bsmdpGainC,Calibrations/smd/y3bsmdp/bsmdpGain);
#include "St_emcTriggerStatusC.h"
MakeChairInstance2(emcTriggerStatus,St_bemcTriggerStatusC,Calibrations/emc/trigger/bemcTriggerStatus);
#include "St_emcTriggerPedC.h"
MakeChairInstance2(emcTriggerPed,St_bemcTriggerPedC,Calibrations/emc/trigger/bemcTriggerPed);
#include "St_emcTriggerLUTC.h"
MakeChairInstance2(emcTriggerLUT,St_bemcTriggerLUTC,Calibrations/emc/trigger/bemcTriggerLUT);
#include "St_bemcMapC.h"
MakeChairInstance(bemcMap,Calibrations/emc/map/bemcMap);
#include "St_bprsMapC.h"
MakeChairInstance(bprsMap,Calibrations/prs/map/bprsMap);
#include "St_bsmdeMapC.h"
MakeChairInstance(bsmdeMap,Calibrations/smde/map/bsmdeMap);
#include "St_bsmdpMapC.h"
MakeChairInstance(bsmdpMap,Calibrations/smdp/map/bsmdpMap);
