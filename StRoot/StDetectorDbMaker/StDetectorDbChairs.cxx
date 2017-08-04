#include <assert.h>
#include <string.h>
#include "StDetectorDbMaker.h"
#include "TEnv.h"
#include "TF1.h"
#include "TCernLib.h"
#include "St_db_Maker/St_db_Maker.h"
#if 0
#include "tables/St_tpcCorrection_Table.h"
#include "tables/St_tpcSectorT0offset_Table.h"
#include "tables/St_tofTrayConfig_Table.h"
#define DEBUGTABLE(STRUCT) PrintTable(#STRUCT,table )
#define makeString(PATH) # PATH
#define CHECKTABLE(C_STRUCT) \
  if (table->InheritsFrom("St_" makeSTRING(C_STRUCT))) {	  \
    St_ ## C_STRUCT  *t = (St_ ## C_STRUCT  *) table ;	      \
    ## C_STRUCT ## _st *s = t->GetTable(); Nrows = s->nrows;    \
    ## C_STRUCT ## _st def = {0};				      \
    iprt = kFALSE;					      \
    Int_t shift = 0; \
    Int_t NrowSize = t->GetRowSize(); \
    if (! strcmp(makeSTRING(C_STRUCT),"Survey")) {shift = 4; NrowSize = 12*8;}\
    if (! strcmp(makeSTRING(C_STRUCT),"tpcSectorT0offset")) {for (Int_t i = 0; i < 24; i++) def->t0[i] = -22.257;} \
    if (! strcmp(makeSTRING(C_STRUCT),"tofTrayConfig")) {def->entries = 120; for (Int_t i = 0; i < 120; i++) {def->iTray[i] = i+1; def->nModules[i] = 32;} \

    for (Int_t i = 0; i < table->GetNRows(); i++, s++) {	      \
      if (memcmp(&def+shift, s+shift,  NrowSize)) {iprt = kTRUE; break;}   \
    }								      \
  } 
//___________________Debug Print out  _____________________________________________________________
void PrintTable(const Char_t *str, TTable *table) {
  TDatime t[2];
  Bool_t iprt = kTRUE;
  if (St_db_Maker::GetValidity(table,t) > 0) {
    Int_t Nrows = table->GetNRows();
    LOG_WARN << "St_" << str << "C::instance found table " << table->GetName()
	     << " with NRows = " << Nrows << " in db" << endm;
    LOG_WARN << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	     << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endm;
    if (table->InheritsFrom("St_tpcCorrection")) {
      St_tpcCorrection *t = (St_tpcCorrection *) table;
      tpcCorrection_st *s = t->GetTable(); Nrows = s->nrows;}
    if (Nrows > 10) Nrows = 10;
    CHECKTABLE(tpcCorrection);
    CHECKTABLE(tpcHVPlanes);
    CHECKTABLE(Survey);
    CHECKTABLE(tpcSectorT0offset);
    CHECKTABLE(tofTrayConfig);
    if (iprt) {
      if (table->GetRowSize() < 512) table->Print(0,Nrows);
    } else {
      LOG_WARN << "Default table" << endm;
    }
  }
}
#endif
#include "StarChairDefs.h"
static Int_t _debug = 0;
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
#include "St_TpcEffectivedXC.h"
MakeChairInstance(TpcEffectivedX,Calibrations/tpc/TpcEffectivedX);
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
MakeChairOptionalInstance2(TpcSecRowCor,St_TpcSecRowCC,Calibrations/tpc/TpcSecRowC);
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
    case 12:// ADC correction offset + poly for log(ADC) and TanL    
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
    case 5:
      if (x < 1e-7) X = -16.118;
      else          X = TMath::Log(x);
      break;
    default:      X = x;    break;
    }
  }
  if (cor->type != 1 && cor->type != 2 &&
      cor->min < cor->max) {
    if (X < cor->min) X = cor->min;
    if (X > cor->max) X = cor->max;
  }
  static TF1 *f1000 = 0, *f1100 = 0, *f1200 = 0, *f1300 = 0;
  TF1 *f = 0;
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
    Sum = cor->a[1] + z*cor->a[2] + z*X*cor->a[3] + TMath::Exp(X*(cor->a[4] + X*cor->a[5]) + cor->a[6]);
    Sum *= TMath::Exp(-cor->a[0]);
    break;
  case 12: // ADC correction offset + poly for log(ADC) and TanL
    Sum = cor->a[1] + z*cor->a[2] + z*z*cor->a[3] + TMath::Exp(X*(cor->a[4] + X*cor->a[5]) + cor->a[6]);
    Sum *= TMath::Exp(-cor->a[0]);
    break;
  case 1000:
  case 1100:
  case 1200:
  case 1300:
      if (cor->type == 1000) {
	if (! f1000) f1000 = new TF1("f1000","gaus(9)+pol0(3)"); 
	f = f1000;
      } else if (cor->type == 1100) {
	if (! f1100) f1100 = new TF1("f1100","gaus+pol1(3)"); 
	f = f1100;
      } else if (cor->type == 1200) {
	if (! f1200) f1200 = new TF1("f1200","gaus+pol2(3)"); 
	f = f1200;
      } else if (cor->type == 1300) {
	if (! f1300) f1300 = new TF1("f1300","gaus+pol3(3)"); 
	f = f1300;
      }
      assert(f);
      f->SetParameters(cor->a);
      Sum = f->Eval(X);
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
#include "St_TpcEdgeC.h"
MakeChairInstance2(tpcCorrection,St_TpcEdgeC,Calibrations/tpc/TpcEdge);
#include "St_TpcAdcCorrectionBC.h"
MakeChairInstance2(tpcCorrection,St_TpcAdcCorrectionBC,Calibrations/tpc/TpcAdcCorrectionB);
#include "St_tpcMethaneInC.h"
MakeChairInstance2(tpcCorrection,St_tpcMethaneInC,Calibrations/tpc/tpcMethaneIn);
#include "St_tpcTimeBucketCorC.h"
MakeChairInstance2(tpcCorrection,St_tpcTimeBucketCorC,Calibrations/tpc/tpcTimeBucketCor);
#include "St_tpcGasTemperatureC.h"
MakeChairInstance2(tpcCorrection,St_tpcGasTemperatureC,Calibrations/tpc/tpcGasTemperature);
#include "St_tpcWaterOutC.h"
MakeChairInstance2(tpcCorrection,St_tpcWaterOutC,Calibrations/tpc/tpcWaterOut);
#include "St_TpcdChargeC.h"
MakeChairOptionalInstance2(tpcCorrection,St_TpcdChargeC,Calibrations/tpc/TpcdCharge);
#include "St_TpcrChargeC.h"
MakeChairOptionalInstance2(tpcCorrection,St_TpcrChargeC,Calibrations/tpc/TpcrCharge);
#include "St_TpcTanLC.h"
MakeChairInstance2(tpcCorrection,St_TpcTanLC,Calibrations/tpc/TpcTanL);
#include "St_TpcCurrentCorrectionC.h"
//MakeChairInstance2(tpcCorrection,St_TpcCurrentCorrectionC,Calibrations/tpc/TpcCurrentCorrection);
ClassImp(St_TpcCurrentCorrectionC);
St_TpcCurrentCorrectionC *St_TpcCurrentCorrectionC::fgInstance = 0;
St_TpcCurrentCorrectionC *St_TpcCurrentCorrectionC::instance() {
  if (fgInstance) return fgInstance;
  St_tpcCorrection *table = (St_tpcCorrection *) StMaker::GetChain()->GetDataBase("Calibrations/tpc/TpcCurrentCorrectionX");
  if (! table)      table = (St_tpcCorrection *) StMaker::GetChain()->GetDataBase("Calibrations/tpc/TpcCurrentCorrection");
  assert(table);	  DEBUGTABLE(tpcCorrection);	
  fgInstance = new St_TpcCurrentCorrectionC(table);
  return fgInstance;
}
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
#include "St_TpcPadCorrectionMDF.h"
MakeChairInstance2(MDFCorrection,St_TpcPadCorrectionMDF,Calibrations/tpc/TpcPadCorrectionMDF);
ClassImp(St_MDFCorrectionC);
St_MDFCorrectionC *St_MDFCorrectionC::fgMDFCorrectionC = 0;
//____________________________________________________________________
St_MDFCorrectionC::St_MDFCorrectionC(St_MDFCorrection *table) : TChair(table), fFunc(0) {
  UInt_t N = table->GetNRows(); 
  fFunc = new TF1*[N]; 
  memset(fFunc, 0, N*sizeof(TF1*));
}
//____________________________________________________________________
St_MDFCorrectionC::~St_MDFCorrectionC() {
  UInt_t N = Table()->GetNRows(); 
  for (UInt_t i = 0; i < N; i++) {SafeDelete(fFunc[i]);}
  delete [] fFunc;
}
//____________________________________________________________________
Double_t St_MDFCorrectionC::MDFunc(Double_t *x, Double_t *p) {
  // Evaluate parameterization at point x. Optional argument coeff is
  // a vector of coefficients for the parameterisation, NCoefficients
  // elements long.
  assert(x);
  UInt_t k = p[0];
  assert(k >= 0 && k < fgMDFCorrectionC->getNumRows());
  Double_t returnValue = fgMDFCorrectionC->DMean(k);
  Double_t term        = 0;
  UChar_t    i, j;
  for (i = 0; i < fgMDFCorrectionC->NCoefficients(k); i++) {
    // Evaluate the ith term in the expansion
    term = fgMDFCorrectionC->Coefficients(k)[i];
    for (j = 0; j < fgMDFCorrectionC->NVariables(k); j++) {
      // Evaluate the factor (polynomial) in the j-th variable.
      Int_t    p  =  fgMDFCorrectionC->Powers(k)[i * fgMDFCorrectionC->NVariables(k) + j];
      Double_t y  =  1 + 2. / (fgMDFCorrectionC->XMax(k)[j] - fgMDFCorrectionC->XMin(k)[j])
	* (x[j] - fgMDFCorrectionC->XMax(k)[j]);
      term        *= fgMDFCorrectionC->EvalFactor(k,p,y);
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}

//____________________________________________________________________
Double_t St_MDFCorrectionC::Eval(Int_t k, Double_t x0, Double_t x1) const {
  Double_t x[2] = {x0, x1};
  return Eval(k,x);
}
//____________________________________________________________________
Double_t St_MDFCorrectionC::Eval(Int_t k, Double_t *x) const {
  // Evaluate parameterization at point x. Optional argument coeff is
  // a vector of coefficients for the parameterisation, NCoefficients
  // elements long.
  assert(x);
  if (! fFunc[k]) {
    fgMDFCorrectionC = (St_MDFCorrectionC *) this;
    if (NVariables(k) == 1) {
      fFunc[k] = new TF1(Form("%s_%i",Table()->GetName(),k),St_MDFCorrectionC::MDFunc,
			 XMin(k)[0],XMax(k)[0],1);
      fFunc[k]->SetParameter(0,k);
      fFunc[k]->Save(XMin(k)[0],XMax(k)[0],0,0,0,0);
    } else if (NVariables(k) == 2) {
      fFunc[k] = new TF2(Form("%s_%i",Table()->GetName(),k),St_MDFCorrectionC::MDFunc,
			 XMin(k)[0],XMax(k)[0],XMin(k)[1],XMax(k)[1],1);
      fFunc[k]->SetParameter(0,k);
      ((TF2 *) fFunc[k])->Save(XMin(k)[0],XMax(k)[0],XMin(k)[1],XMax(k)[1],0,0);
    } else if (NVariables(k) == 3) {
      fFunc[k] = new TF3(Form("%s_%i",Table()->GetName(),k),St_MDFCorrectionC::MDFunc,
			 XMin(k)[0],XMax(k)[0],XMin(k)[1],XMax(k)[1],XMin(k)[2],XMax(k)[2],1);
      fFunc[k]->SetParameter(0,k);
      ((TF3 *) fFunc[k])->Save(XMin(k)[0],XMax(k)[0],XMin(k)[1],XMax(k)[1],XMin(k)[2],XMax(k)[2]);
    }
  }
  Double_t xx[3];
  for (Int_t v = 0; v < NVariables(k); v++) {
    xx[v] = TMath::Max(XMin(k)[v], TMath::Min(XMax(k)[v], x[v]));
  }
  Double_t returnValue = fFunc[k]->GetSave(xx); 
  return returnValue;
}
//____________________________________________________________________
Double_t St_MDFCorrectionC::EvalError(Int_t k, Double_t *x) const {
  // Evaluate parameterization error at point x. Optional argument coeff is
  // a vector of coefficients for the parameterisation, NCoefficients(k)
  // elements long.
  assert(x);
  Double_t returnValue = 0;
  Double_t term        = 0;
  UChar_t    i, j;
  for (i = 0; i < NCoefficients(k); i++) {
    // Evaluate the ith term in the expansion
    term = CoefficientsRMS(k)[i];
    for (j = 0; j < NVariables(k); j++) {
      // Evaluate the factor (polynomial) in the j-th variable.
      Int_t    p  =  Powers(k)[i * NVariables(k) + j];
      Double_t y  =  1 + 2. / (XMax(k)[j] - XMin(k)[j])
	* (x[j] - XMax(k)[j]);
      term        *= EvalFactor(p,y);
    }
    // Add this term to the final result
    returnValue += term*term;
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
MakeChairAltInstance(tpcEffectiveGeom,Calibrations/tpc/tpcEffectiveGeom,Calibrations/tpc/tpcEffectiveGeomB,gEnv->GetValue("NewTpcAlignment",0));
#include "St_tpcElectronicsC.h"
MakeChairAltInstance(tpcElectronics,Calibrations/tpc/tpcElectronics,Calibrations/tpc/tpcElectronicsB,gEnv->GetValue("NewTpcAlignment",0));
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
MakeChairAltInstance(tpcPadrowT0,Calibrations/tpc/tpcPadrowT0,Calibrations/tpc/tpcPadrowT0B,gEnv->GetValue("NewTpcAlignment",0));
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
#include "St_TpcAvgPowerSupplyC.h"
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
Float_t St_tpcAnodeHVC::voltage(Int_t i) const {
  if (! St_TpcAvgPowerSupplyC::instance()->Table()->IsMarked()) {
     LOG_ERROR << "St_tpcAnodeHVC::voltage(" << i << " is called but the valid St_TpcAvgPowerSupplyC::instance() exists" << endm;
  }
  return Struct(i)->voltage;
}
//________________________________________________________________________________
Float_t St_tpcAnodeHVC::voltagePadrow(Int_t sector, Int_t padrow) const {
  if (! St_TpcAvgPowerSupplyC::instance()->Table()->IsMarked()) {
    return St_TpcAvgPowerSupplyC::instance()->voltagePadrow(sector,padrow);
  }
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
MakeChairOptionalInstance(TpcAvgPowerSupply,Calibrations/tpc/TpcAvgPowerSupply);
//________________________________________________________________________________
Float_t St_TpcAvgPowerSupplyC::voltagePadrow(Int_t sector, Int_t padrow) const {
  Int_t e1 = 0, e2 = 0;
  Float_t f2 = 0;
  St_tpcAnodeHVC::sockets(sector, padrow, e1, e2, f2);
  if (e1==0) return -99;
  Int_t ch1 = St_TpcAvgCurrentC::ChannelFromSocket((e1-1)%19+1);
  Float_t v1=Voltage()[8*(sector-1)+ch1-1] ;
  if (f2==0) return v1;
  Int_t ch2 = St_TpcAvgCurrentC::ChannelFromSocket((e2-1)%19 + 1);
  if (ch1 == ch2) return v1;
  Float_t v2=Voltage()[8*(sector-1)+ch2-1] ;
  if (v2==v1) return v1;
  // different voltages on influencing HVs
  // effective voltage is a sum of exponential gains
  Float_t B = (padrow <= St_tpcPadPlanesC::instance()->innerPadRows() ? 13.05e-3 : 10.26e-3);
  Float_t v_eff = TMath::Log((1.0-f2)*TMath::Exp(B*v1) + f2*TMath::Exp(B*v2)) / B;
  return v_eff;
}
//________________________________________________________________________________
Float_t St_TpcAvgPowerSupplyC::AcChargeL(Int_t sector, Int_t channel) {
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

#include "St_tpcAnodeHVavgC.h"
MakeChairInstance(tpcAnodeHVavg,Calibrations/tpc/tpcAnodeHVavg);
//________________________________________________________________________________
Float_t St_tpcAnodeHVavgC::voltage(Int_t i) const {
  if (! St_TpcAvgPowerSupplyC::instance()->Table()->IsMarked()) {
     LOG_ERROR << "St_tpcAnodeHVavgC::voltage(" << i << " is called but the valid St_TpcAvgPowerSupplyC::instance() exists" << endm;
  }
  return Struct(i)->voltage;
}
//________________________________________________________________________________
Bool_t St_tpcAnodeHVavgC::tripped(Int_t sector, Int_t padrow) const {
  if (! St_TpcAvgPowerSupplyC::instance()->Table()->IsMarked()) {
    return St_TpcAvgPowerSupplyC::instance()->tripped(sector,padrow);
  }
  return (voltage() < -100);
}
//________________________________________________________________________________
Float_t St_tpcAnodeHVavgC::voltagePadrow(Int_t sector, Int_t padrow) const {
  if (! St_TpcAvgPowerSupplyC::instance()->Table()->IsMarked()) {
    return St_TpcAvgPowerSupplyC::instance()->voltagePadrow(sector,padrow);
  }
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
    fFunc = new TF1*[nrows]; memset(fFunc, 0, nrows*sizeof(TF1*));
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
    LOG_INFO << "StTpcHitErrors have been instantiated with\n"
	     << "StTpcHitErrors fnXZ(" <<  fgInstance->fXZ << "," << fgInstance->fSec << "," << fgInstance->fRow << "," 
	     << fgInstance->fMS << "," << fgInstance->fPrompt << ") = " << fgInstance->fNxz << endm;
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
    (Double_t )Npads,  // 0 => no. of pads in cluster
    (Double_t )Ntmbks, // 1 => no. of time buckets in cluster
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
  if (! St_TpcAvgPowerSupplyC::instance()->Table()->IsMarked()) {
    return St_TpcAvgPowerSupplyC::instance()->AcChargeL(sector,channel);
  }
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
//________________________________________________________________________________
Float_t St_TpcAvgCurrentC::AvCurrent(Int_t sector, Int_t channel) {
  if (! St_TpcAvgPowerSupplyC::instance()->Table()->IsMarked()) {
    return St_TpcAvgPowerSupplyC::instance()->AvCurrent(sector,channel);
  }
  return (sector > 0 && sector <= 24 && channel > 0 && channel <= 8) ? 
    Struct()->AvCurrent[8*(sector-1)+channel-1] :     0;
}
//________________________________________________________________________________
Float_t St_TpcAvgCurrentC::AcCharge(Int_t sector, Int_t channel) {
  if (! St_TpcAvgPowerSupplyC::instance()->Table()->IsMarked()) {
    return St_TpcAvgPowerSupplyC::instance()->AcCharge(sector,channel);
  }
  return (sector > 0 && sector <= 24 && channel > 0 && channel <= 8) ? 
    Struct()->AcCharge[8*(sector-1)+channel-1] :     0;
}
//__________________Calibrations/trg______________________________________________________________
#include "St_defaultTrgLvlC.h"
MakeChairInstance(defaultTrgLvl,Calibrations/trg/defaultTrgLvl);
#include "St_trigDetSumsC.h"
St_trigDetSumsC *St_trigDetSumsC::fgInstance = 0;
St_trigDetSumsC *St_trigDetSumsC::instance() {
  if (fgInstance) return fgInstance;
  St_trigDetSums *table = 0;
  TDataSet *event = StMaker::GetChain()->GetDataSet("StEvent");
  if (event) table = (St_trigDetSums *) event->Find("trigDetSums");
  if (! table) table = (St_trigDetSums *) StMaker::GetChain()->GetDataBase("Calibrations/rich/trigDetSums");
  assert(table);
  fgInstance = new St_trigDetSumsC(table);
  fgInstance->SetName("trigDetSumsC");
  StMaker::GetChain()->AddData(fgInstance);
  return fgInstance;
}
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
  St_tpcGainCorrectionC *gC = St_tpcGainCorrectionC::instance();
  Int_t NRows = gC->GetNRows();
  if (l >= NRows) return gain;
  V = St_tpcAnodeHVavgC::instance()->voltagePadrow(sec,row);
  if (V > 0) {
    Double_t v = V - V_nominal;
#if 0
    // Hack for Run XVI
    if ( gC->min(l) > -1 && v > -100 && v < 0) {
      if (l == 0) v =   0;
      else        v = -70;
    } else 
#endif
    if (v < gC->min(l) || v > gC->max(l)) return gain;
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
MakeChairAltInstance(trgTimeOffset,Conditions/trg/trgTimeOffset,Conditions/trg/trgTimeOffsetB,gEnv->GetValue("NewTpcAlignment",0));
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
#include "StSstSurveyC.h"
MakeChairInstance2(Survey,StsstOnOsc,Geometry/sst/sstOnOsc);
MakeChairInstance2(Survey,StsstLadderOnSst,Geometry/sst/sstLadderOnSst);
MakeChairInstance2(Survey,StsstSensorOnLadder,Geometry/sst/sstSensorOnLadder);
#include "StTpcSurveyC.h"
MakeChairAltInstance2(Survey,StTpcInnerSectorPosition,Geometry/tpc/TpcInnerSectorPosition,Geometry/tpc/TpcInnerSectorPositionB,gEnv->GetValue("NewTpcAlignment",0));
MakeChairAltInstance2(Survey,StTpcOuterSectorPosition,Geometry/tpc/TpcOuterSectorPosition,Geometry/tpc/TpcOuterSectorPositionB,gEnv->GetValue("NewTpcAlignment",0));
MakeChairAltInstance2(Survey,StTpcSuperSectorPosition,Geometry/tpc/TpcSuperSectorPosition,Geometry/tpc/TpcSuperSectorPositionB,gEnv->GetValue("NewTpcAlignment",0));
MakeChairInstance2(Survey,StTpcHalfPosition,Geometry/tpc/TpcHalfPosition);
MakeChairInstance2(Survey,StTpcPosition,Geometry/tpc/TpcPosition);
//____________________________Geometry/gmt____________________________________________________
#include "StGmtSurveyC.h"
MakeChairInstance2(Survey,StGmtOnTpc,Geometry/gmt/GmtOnTpc);
MakeChairInstance2(Survey,StGmtOnModule,Geometry/gmt/GmtOnModule);
//____________________________Geometry/ist____________________________________________________
#include "StIstSurveyC.h"
MakeChairInstance2(Survey,StidsOnTpc,Geometry/ist/idsOnTpc);                      
MakeChairInstance2(Survey,StpstOnIds,Geometry/ist/pstOnIds);
MakeChairInstance2(Survey,StistOnPst,Geometry/ist/istOnPst);
MakeChairInstance2(Survey,StLadderOnIst,Geometry/ist/istLadderOnIst);
MakeChairInstance2(Survey,StistSensorOnLadder,Geometry/ist/istSensorOnLadder);
//____________________________Geometry/pxl____________________________________________________
#include "StPxlSurveyC.h"
MakeChairInstance2(Survey,StPxlpstOnIds,Geometry/pxl/pstOnIds);
MakeChairInstance2(Survey,StpxlOnPst,Geometry/pxl/pxlOnPst);
MakeChairInstance2(Survey,StpxlHalfOnPxl,Geometry/pxl/pxlHalfOnPxl);
MakeChairInstance2(Survey,StpxlSectorOnHalf,Geometry/pxl/pxlSectorOnHalf);
MakeChairInstance2(Survey,StpxlLadderOnSector,Geometry/pxl/pxlLadderOnSector);
MakeChairInstance2(Survey,StpxlSensorOnLadder,Geometry/pxl/pxlSensorOnLadder);
#include "St_pxlControlC.h"
MakeChairInstance(pxlControl,Geometry/pxl/pxlControl);
#include "St_pxlSensorTpsC.h"
MakeChairInstance(pxlSensorTps,Geometry/pxl/pxlSensorTps);
//________________________________________________________________________________
void St_SurveyC::Normalize(TGeoHMatrix &rot) {
#if 0
  Double_t *rA = rot.GetRotationMatrix();
  Double_t r[9] = {rA[0], rA[3], rA[6],
		   rA[1], rA[4], rA[7],
		   rA[2], rA[5], rA[8]};
  TGeoMatrix::Normalize(&r[0]);
  TGeoMatrix::Normalize(&r[3]);
  TGeoMatrix::Normalize(&r[6]);
  Double_t rB[9] = {r[0], r[3], r[6],
		    r[1], r[4], r[7],
		    r[2], r[5], r[8]};
  rot.SetRotation(rB);
#else
  Double_t normfactor = rot.Determinant();
  if (normfactor <= 1E-10) return;
  if (TMath::Abs(normfactor)-1 <= 1e-10) return;
  normfactor = TMath::Power(TMath::Abs(normfactor), -1./3);
  TCL::vscale(rot.GetRotationMatrix(), normfactor, rot.GetRotationMatrix(), 9);
#endif
}
//________________________________________________________________________________
const TGeoHMatrix &St_SurveyC::GetMatrix(Int_t i) {
  static TGeoHMatrix rot;
  rot.SetName(Table()->GetName());
  rot.SetRotation(Rotation(i));
  rot.SetTranslation(Translation(i));
  Normalize(rot);
  return *&rot;
}
//________________________________________________________________________________
const TGeoHMatrix &St_SurveyC::GetMatrix4Id(Int_t id) {
  static TGeoHMatrix rot("UnKnown");
  for (UInt_t i = 0; i < getNumRows(); i++) {
    if (Id(i) == id) {
      rot = GetMatrix(i);
      rot.SetName(Form("%s_%i",Table()->GetName(),id));
      //      Table()->Print(i,1);
      return *&rot;
    }
  }
  cout << "St_SurveyC::GetMatrix4Id(" << id << ") entry has not been found" << endl;
  const TTable *table = Table();
  Int_t Nrows = table->GetNRows();
  table->Print(0,Nrows);
  assert(0);
  return *&rot;
}
//________________________________________________________________________________
const TGeoHMatrix &St_SurveyC::GetMatrixR(Int_t i) {
  static TGeoHMatrix rot;
  Double_t rotations[9] = {
    r00(i), r01(i),      0,
    r10(i), r11(i),      0,
    0     ,      0, r22(i)};
  rot.SetName(Form("%s_%i",Table()->GetName(),i));
  rot.SetRotation(rotations);
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
  if (Name == "idsOnTpc")               return (St_SurveyC   *) StidsOnTpc::instance();	    
  if (Name == "pstOnIds")        	return (St_SurveyC   *) StpstOnIds::instance();  	
  if (Name == "istOnPst")        	return (St_SurveyC   *) StistOnPst::instance();  	
  if (Name == "LadderOnIst")       	return (St_SurveyC   *) StLadderOnIst::instance(); 	
  if (Name == "LadderOnShell")        	return (St_SurveyC   *) StSvtLadderOnShell::instance();  	
  if (Name == "istSensorOnLadder")      return (St_SurveyC   *) StistSensorOnLadder::instance();  	
  return 0;
}
//__________________Calibrations/rhic______________________________________________________________
#include "St_vertexSeedC.h"
MakeChairInstance(vertexSeed,Calibrations/rhic/vertexSeed);
//__________________Calibrations/tof______________________________________________________________
#include "St_tofCorrC.h"
ClassImp(St_tofCorrC);
St_tofCorrC::St_tofCorrC(TTable *table) : TChair(table), mCalibType(NOTSET) {
  Int_t N = 0;
  if (table) N = getNumRows();
  mCalibType = calibtype(N);  
  mIndxArray.Set(N);
  mNusedArray.Set(N);
}
//________________________________________________________________________________
Float_t St_tofCorrC::Correction(Int_t N, Float_t *xArray, Float_t x, Float_t *yArray, Short_t &NN) {
  Float_t dcorr = -9999;
  if (! NN) {// Sort
    if (N <= 0 || ! xArray || ! yArray) return dcorr;
    NN = N;
    Bool_t IsSorted = kTRUE;
    Int_t nonzero = 0;
    for (Int_t bin = N-1; bin >= 0; bin--) {
      if (! nonzero && TMath::Abs(xArray[bin]) < 1e-7 && TMath::Abs(yArray[bin]) < 1e-7) {
	NN--; // trailing entries
	if (! IsSorted) break;
	continue;
      }
      nonzero++;
      if (bin > 0 && xArray[bin] < xArray[bin-1]) IsSorted = kFALSE;
    }
    if (! NN) {
      LOG_ERROR << " St_tofCorrC::Correction xArray[" << (Int_t ) NN << "] is empty" << endm;
      return dcorr;
    }
    if (! IsSorted) {
      LOG_WARN << " St_tofCorrC::Correction xArray[" << (Int_t ) NN << "] has not been sorted" << endm;
      TArrayI Indx(NN);
      TMath::Sort((Int_t) NN,xArray,Indx.GetArray(),0);
      TArrayF X(NN,xArray);
      TArrayF Y(NN,yArray);
      for (Int_t i = 0; i < NN; i++) {
	xArray[i] = X[Indx[i]];
	yArray[i] = Y[Indx[i]];
      }
      LOG_WARN << " St_tofCorrC::Correction xArray[" << (Int_t ) NN << "] is sorted now" << endm;
    }
  }
  if (NN == 1) {return yArray[NN-1];}
  if (x < xArray[0] || x > xArray[NN-1]) {
    if (TMath::Abs(x) < 1e-7) dcorr = 0; // Simulation
    return dcorr;
  }
  Int_t bin = TMath::BinarySearch(NN, xArray, x);
  if (bin >= 0 && bin < NN) {
    if (bin == NN) bin--;
    Double_t x1 = xArray[bin];
    Double_t x2 = xArray[bin+1];
    Double_t y1 = yArray[bin];
    Double_t y2 = yArray[bin+1];
    dcorr = y1 + (x-x1)*(y2-y1)/(x2-x1);
  }
  return dcorr;
}
//________________________________________________________________________________
Int_t St_tofCorrC::Index(Int_t tray, Int_t module, Int_t cell) {
  assert( tray && module );
  Int_t i = -1;
  switch (mCalibType) {
  case CELLCALIB:  assert(cell); i = cell - 1 + mNCell*(module - 1 + mNModule*(tray - 1))  ; break;
  case MODULECALIB: i =                                 module - 1 + mNModule*(tray - 1)   ; break;
  case BOARDCALIB:  i =                                (module - 1 + mNModule*(tray - 1))/4; break;
  default: assert(0); break;
  }
  return i;
}
#include "St_tofTotbCorrC.h"
MakeChairInstance(tofTotbCorr,Calibrations/tof/tofTotbCorr);
St_tofTotbCorrC::St_tofTotbCorrC(St_tofTotbCorr *table) : St_tofCorrC(table) {
  Int_t N = 0;
  if (table) N = getNumRows();
  for (Int_t i =  0; i < N; i++) {
    Int_t j = Index(trayId(i), moduleId(i), cellId(i));
    if (_debug) {
      Table()->Print(i,1);
      cout << "i = " << i << "\ttray = " << trayId(i) << "\tmodule = " << moduleId(i) << "\tcellId = " << cellId(i) << "\tindex = " << j << endl;
    }
    if (j >= 0) {
      if (! mIndxArray[j]) mIndxArray[j] = i;
      else {
	Int_t m = mIndxArray[j];
	LOG_ERROR << "St_tofTotbCorrC duplicated rows " 
		  << m << " tray:" << trayId(m) << " module:" << moduleId(m) 
		  << " cell:" << cellId(m) << " tot[0] = " << tot(m)[0] << " corr[0] = " << corr(m)[0] << " and \n"
		  << "                                                       "
		  << i << " tray:" << trayId(i) << " module:" << moduleId(i) 
		  << " cell:" << cellId(i) << " tot[0] = " << tot(i)[0] << " corr[0] = " << corr(i)[0] << endm;
      }
    }
  }
}
//________________________________________________________________________________
Float_t  St_tofTotbCorrC::Corr(Int_t tray, Int_t module, Int_t cell, Float_t x) {
  Int_t idx = Index(tray,module,cell);
  Int_t i = mIndxArray[idx]; 
  if (i < 0) return 0;
  Int_t Tray   = trayId(i);
  Int_t Module = moduleId(i);
  Int_t Cell   = cellId(i);
  assert(i >= 0 && Tray == tray && Module == module && Cell == cell);
  Float_t dcorr = St_tofCorrC::Correction(mNBinMax, tot(i), x, corr(i),mNusedArray[idx]);
  if (dcorr <= -9999.0) {
    LOG_ERROR << "St_tofTotbCorrC::Corr(" << tray << "," << module << "," << cell << "," << x << ") is rejected.";
    if (x < tot(i)[0] || x > tot(i)[mNusedArray[idx]-1]) LOG_ERROR << " Out of the range [" << tot(i)[0] << "," << tot(i)[mNusedArray[idx]-1] << "].";
    LOG_ERROR << endm;
  }
  return dcorr;
}
#include "St_tofZbCorrC.h"
MakeChairInstance(tofZbCorr,Calibrations/tof/tofZbCorr);
St_tofZbCorrC::St_tofZbCorrC(St_tofZbCorr *table) : St_tofCorrC(table) {
  Int_t N = 0;
  if (table) N = getNumRows();
  for (Int_t i =  0; i < N; i++) {
    Int_t j = Index(trayId(i), moduleId(i), cellId(i));
    if (j >= 0) {
      if (! mIndxArray[j]) mIndxArray[j] = i;
      else {
	Int_t m = mIndxArray[j];
	LOG_ERROR << "St_tofZbCorrC duplicated rows " 
		  << m << " tray:" << trayId(m) << " module:" << moduleId(m) 
		  << " cell:" << cellId(m) << " z[0] = " << z(m)[0] << " corr[0] = " << corr(m)[0] << " and \n"
		  << "                                                       "
		  << i << " tray:" << trayId(i) << " module:" << moduleId(i) 
		  << " cell:" << cellId(i) << " z[0] = " << z(i)[0] << " corr[0] = " << corr(i)[0] << endm;
      }
    }
  }
}
//________________________________________________________________________________
Float_t St_tofZbCorrC::Corr(Int_t tray, Int_t module, Int_t cell, Float_t x) {
  Int_t idx = Index(tray,module,cell);
  Int_t i = mIndxArray[idx]; 
  if (i < 0) return 0;
  Int_t Tray   = trayId(i);
  Int_t Module = moduleId(i);
  Int_t Cell   = cellId(i);
  assert(i >= 0 && Tray == tray && Module == module && Cell == cell);
  Float_t dcorr = St_tofCorrC::Correction(mNBinMax, z(i), x, corr(i),mNusedArray[idx]);
  if (dcorr <= -9999.0) {
    LOG_ERROR << "tofZbCorrC::Corr(" << tray << "," << module << "," << cell << "," << x << ") is rejected.";
    if (x < z(i)[0] || x > z(i)[mNusedArray[idx]-1]) LOG_ERROR << " Out of the range [" << z(i)[0] << "," << z(i)[mNusedArray[idx]-1] << "].";
    LOG_ERROR << endm;

  }
  return dcorr;
}
#include "St_tofGeomAlignC.h"
MakeChairInstance(tofGeomAlign,Calibrations/tof/tofGeomAlign);
#include "St_tofStatusC.h"
MakeChairInstance(tofStatus,Calibrations/tof/tofStatus);
#include "St_pvpdStrobeDefC.h"
MakeChairInstance(pvpdStrobeDef,Calibrations/tof/pvpdStrobeDef);
#include "St_tofCamacDaqMapC.h"
MakeChairInstance(tofCamacDaqMap,Calibrations/tof/tofCamacDaqMap);
#include "St_tofDaqMapC.h"
MakeChairInstance(tofDaqMap,Calibrations/tof/tofDaqMap);
#include "St_tofTrayConfigC.h"
MakeChairInstance(tofTrayConfig,Calibrations/tof/tofTrayConfig);
#include "St_tofINLCorrC.h"
MakeChairInstance(tofINLCorr,Calibrations/tof/tofINLCorr);
#include "St_tofINLSCorrC.h"
MakeChairInstance(tofINLSCorr,Calibrations/tof/tofINLSCorr);
#include "St_tofModuleConfigC.h"
MakeChairInstance(tofModuleConfig,Calibrations/tof/tofModuleConfig);
#include "St_tofPedestalC.h"
MakeChairInstance(tofPedestal,Calibrations/tof/tofPedestal);
#include "St_tofr5MaptableC.h"
MakeChairInstance(tofr5Maptable,Calibrations/tof/tofr5Maptable);
#include "St_tofTDIGOnTrayC.h"
MakeChairInstance(tofTDIGOnTray,Calibrations/tof/tofTDIGOnTray);
#include "St_tofTOffsetC.h"
MakeChairInstance(tofTOffset,Calibrations/tof/tofTOffset);
Float_t St_tofTOffsetC::t0(Int_t tray, Int_t module, Int_t cell) const {
  //        [mNTray][mNModule][mNCell]
  Int_t j = cell - 1 + mNCell*(module - 1); 
  return T0(tray-1)[j];
}
#include "St_tofTrgWindowC.h"
MakeChairInstance(tofTrgWindow,Calibrations/tof/tofTrgWindow);
#include "St_tofTzeroC.h"
MakeChairInstance(tofTzero,Calibrations/tof/tofTzero);
#include "St_vpdDelayC.h"
MakeChairInstance(vpdDelay,Calibrations/tof/vpdDelay);
#include "St_vpdTotCorrC.h"
MakeChairInstance(vpdTotCorr,Calibrations/tof/vpdTotCorr);
Float_t  St_vpdTotCorrC::Corr(Int_t i, Float_t x) {
  assert(tubeId(i) == i + 1);
  Int_t idx = i;
  Float_t dcorr = St_tofCorrC::Correction(128, tot(i), x, corr(i),mNusedArray[idx]);
  if (dcorr <= -9999.0) {
    LOG_ERROR << "St_vpdTotCorrC::Corr(" << i << "," << x << ") is rejected.";
    if (x < tot(i)[0] || x > tot(i)[mNusedArray[idx]-1]) LOG_ERROR << " Out of the range [" << tot(i)[0] << "," << tot(i)[mNusedArray[idx]-1] << "].";
    LOG_ERROR << endm;
  }
  return dcorr;
}
#include "St_vpdSimParamsC.h"
MakeChairInstance(vpdSimParams,Calibrations/tof/vpdSimParams);
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
//____________________________Calibrations/ist____________________________________________________
#include "St_istPedNoiseC.h"
MakeChairInstance(istPedNoise,Calibrations/ist/istPedNoise);
#include "St_istChipConfigC.h"
MakeChairInstance(istChipConfig,Calibrations/ist/istChipConfig);
#include "St_istControlC.h"
MakeChairInstance(istControl,Calibrations/ist/istControl);
#include "St_istGainC.h"
MakeChairInstance(istGain,Calibrations/ist/istGain);
#include "St_istMappingC.h"
MakeChairInstance(istMapping,Calibrations/ist/istMapping);
//____________________________Calibrations/pxl____________________________________________________
#include "St_pxlHotPixelsC.h"
//MakeChairInstance(pxlHotPixels,Calibrations/pxl/pxlHotPixels);
St_pxlHotPixelsC *St_pxlHotPixelsC::fgInstance = 0;
map<UInt_t,Short_t> St_pxlHotPixelsC::mMapHotPixels;
St_pxlHotPixelsC *St_pxlHotPixelsC::instance() {
  if (! fgInstance) {
    St_pxlHotPixels *table = (St_pxlHotPixels *) StMaker::GetChain()->GetDataBase("Calibrations/pxl/pxlHotPixels");
    if (! table) {							
      LOG_WARN << "St_pxlHotPixelsC::instance Calibrations/pxl/pxlHotPixels\twas not found" << endm;
      assert(table);			 			       
    }									
    DEBUGTABLE(pxlHotPixels);
    fgInstance = new St_pxlHotPixelsC(table);	
    
    mMapHotPixels.clear();
    for(Int_t i=0; i<10000; i++){ 
      if(fgInstance->hotPixel()[i]>0){ 
	mMapHotPixels.insert ( std::pair<UInt_t, Short_t>(fgInstance->hotPixel()[i],i) ); 
      } 
      else break;
    }
    LOG_INFO << "St_pxlHotPixelsC have been instantiated with" << mMapHotPixels.size() << endm;
  }
  return fgInstance;
}
//________________________________________________________________________________
Int_t  St_pxlHotPixelsC::pixelHot(Int_t sector, Int_t ladder, Int_t sensor, Int_t row, Int_t column) const {
  map<UInt_t,Short_t>::const_iterator got;
  got = mMapHotPixels.find(1000000*((sector-1)*40+(ladder-1)*10+sensor) + 1000*row + column);
  if ( got == mMapHotPixels.end() ) {
    return 0;
  }
  else {
    return 1;
  }
}
//________________________________________________________________________________
#include "St_pxlRowColumnStatusC.h"
MakeChairInstance(pxlRowColumnStatus,Calibrations/pxl/pxlRowColumnStatus);
#include "St_pxlBadRowColumnsC.h"
MakeChairInstance(pxlBadRowColumns,Calibrations/pxl/pxlBadRowColumns);
#include "St_pxlSensorStatusC.h"
MakeChairInstance(pxlSensorStatus,Calibrations/pxl/pxlSensorStatus);
