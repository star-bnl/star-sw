#include <assert.h>
#include "TMath.h"
#include "TString.h"
#include "Stiostream.h"
#include "St_svtHybridDriftVelocityC.h"
ClassImp(St_svtHybridDriftVelocityC);
St_svtHybridDriftVelocityC *St_svtHybridDriftVelocityC::fgsvtHybridDriftVelocityC = 0;
Double_t St_svtHybridDriftVelocityC::mAnodePitch  = 0.0250;
Double_t St_svtHybridDriftVelocityC::mWaferLength = 2.9928;
Double_t St_svtHybridDriftVelocityC::mWaferWidth  = 3.0000;
Double_t St_svtHybridDriftVelocityC::mNoAnodes    = 2*St_svtHybridDriftVelocityC::mWaferWidth/St_svtHybridDriftVelocityC::mAnodePitch;
Double_t St_svtHybridDriftVelocityC::mSamplingFrequency = 25000000.0;
static const Int_t NB    =  3;
static const Int_t NL    = 16;
static const Int_t NW    =  7;
static const Int_t NH    =  2;
static svtHybridDriftVelocity_st *pointers[3][16][7][2];
static svtHybridDriftVelocity_st *DataOld = 0;
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::STcheb(Int_t N, Double_t *par, Double_t x) {// N polynome degree, dimension is par[N+1]
  if (N < 0 || N > 12) return 0;
  Double_t T0 = 1;
  Double_t T1 = 2*x - 1;
  Double_t T2;
  Double_t Sum = par[0]*T0;
  if (N >= 1) {
    T1 = 2*x - 1;
    Sum += par[1]*T1;
    for (int n = 2; n <= N; n++) {
      T2 = 2*(2*x - 1)*T1 - T0;
      Sum += par[n]*T2;
      T0 = T1;
      T1 = T2;
    }
  }
  return Sum;
}
//________________________________________________________________________________
St_svtHybridDriftVelocityC::St_svtHybridDriftVelocityC (St_svtHybridDriftVelocity *table) : TChair(table) {
  if (fgsvtHybridDriftVelocityC) delete fgsvtHybridDriftVelocityC; fgsvtHybridDriftVelocityC = this;
  Init();
}
//________________________________________________________________________________
void St_svtHybridDriftVelocityC::Init() {
  memset (&pointers[0][0][0][0], 0, NB*NL*NW*NH*sizeof(svtHybridDriftVelocity_st *));
  St_svtHybridDriftVelocity *Table =  (St_svtHybridDriftVelocity *) GetThisTable();
  if (Table) {
    svtHybridDriftVelocity_st *Data =  Table->GetTable();
    DataOld = Data;
    Int_t N = Table->GetNRows();
    for (Int_t j = 0; j < N; j++, Data++) {
      if (Data->barrel < 1 || Data->barrel > NB ||
	  Data->ladder < 1 || Data->ladder > NL ||
	  Data->wafer  < 1 || Data->wafer  > NW ||
	  Data->hybrid < 1 || Data->hybrid > NH) continue;
      pointers[Data->barrel-1][Data->ladder-1][Data->wafer-1][Data->hybrid-1] = Data;
    }
  }
}
//________________________________________________________________________________
svtHybridDriftVelocity_st *St_svtHybridDriftVelocityC::p(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid) {
  svtHybridDriftVelocity_st *p = 0;
  if (!  GetThisTable()) return p;
  if (((St_svtHybridDriftVelocity *)GetThisTable())->GetTable() != DataOld) Init();
  if (barrel >= 1 && barrel <=  NB &&
      ladder >= 1 && ladder <=  NL &&
      wafer  >= 1 && wafer  <=  NW && 
      hybrid >= 1 && hybrid <=  NH)  p = pointers[barrel-1][ladder-1][wafer-1][hybrid-1];
  return p;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::uHat(svtHybridDriftVelocity_st *p, Double_t timeBin) {
  if (! p) return -99;
  Double_t u = 1 - (timeBin - p->tmin)/(p->tmax - p->tmin);
  if (p->hybrid == 1) u = -u;
  return u;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::vHat(svtHybridDriftVelocity_st *p, Double_t anode) {
  if (! p) return -99;
  Double_t v = 1 - anode/mNoAnodes;
  if (p->hybrid == 1) v = -v;
  return v;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::CalcDriftLength(svtHybridDriftVelocity_st *p, Double_t timeBin, Double_t anode) {
  if (! p) return -99;
  Double_t u  = uHat(p,timeBin);
  Double_t uD = mWaferLength*u;
  Double_t *coef =  &p->v0;
  Int_t nu = p->npar%10;
  if (nu > 0) uD -= STcheb(nu-1, coef, TMath::Abs(u));
  Int_t nv = (p->npar/10)%10;
  if (nv > 0) {
    Double_t v  = vHat(p,anode);
     uD -= STcheb(nv-1, &coef[nu], TMath::Abs(v));
  }
  return uD;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::UnCalcDriftLength(svtHybridDriftVelocity_st *p, Double_t x) {
  if (! p) return -99;
  Double_t d = TMath::Min(mWaferLength,TMath::Max(0.,x));
  return  p->tmin + (p->tmax - p->tmin)*d/mWaferLength;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::DriftVelocity(svtHybridDriftVelocity_st *p) {
  return p ? mSamplingFrequency*mWaferLength/(p->tmax - p->tmin): -1;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::CalcU(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t timeBin, Double_t anode) {
  return CalcDriftLength(barrel, ladder, wafer, hybrid, timeBin, anode);
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::CalcV(Int_t hybrid, Double_t x) {
  Double_t vd = CalcTransLength(x);
  if (hybrid == 1) vd = vd - WaferWidth();
  else             vd = WaferWidth()  - vd;
  return vd;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::UnCalcU(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t ud) {
  if (hybrid == 1) ud = ud + WaferLength();
  else             ud = WaferLength() - ud;
  return UnCalcDriftLength(barrel, ladder, wafer, hybrid, ud);
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::UnCalcV(Int_t hybrid, Double_t vd) {
  if (hybrid == 1) vd = vd + WaferWidth();
  else             vd = WaferWidth()  - vd;
  return UnCalcTransLength(vd);
}
//________________________________________________________________________________
Bool_t St_svtHybridDriftVelocityC::IsValidDriftRegion(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t timeBin) {
  svtHybridDriftVelocity_st *pp = p(barrel, ladder, wafer, hybrid);
  if (! pp) return kFALSE;
  Int_t I = (pp->npar/100)%10;
  if (! I) return kTRUE;
  Double_t u  = TMath::Abs(uHat(pp,timeBin));
  if (u >= pp->dtmin && u <= pp->dtmax) return kTRUE;
  return kFALSE;
}
