#include <assert.h>
#include "TMath.h"
#include "St_svtCorrectionC.h"
ClassImp(St_svtCorrectionC);
//________________________________________________________________________________
Double_t St_svtCorrectionC::STcheb(Int_t N, Double_t *par, Double_t x) {// N polynome degree, dimension is par[N+1]
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
svtCorrection_st *St_svtCorrectionC::pCorrection(Int_t layer, Int_t ladder, Int_t wafer, Int_t hybrid) {
  St_svtCorrection *Table =  (St_svtCorrection *) GetThisTable();
  if (! Table) return 0;
  svtCorrection_st *Data =  Table->GetTable();
  static Int_t N = 0;
  static svtCorrection_st *pointers[6][16][7][2];
  if (N == 0 || ! Table->IsMarked()) {
    N = Table->GetNRows();
    memset (pointers,0, 6*16*7*2*sizeof(svtCorrection_st *));
    Table->Mark();
  }
  assert(layer  >= 1 && layer  <=  6);
  assert(ladder >= 1 && ladder <= 16);
  assert(wafer  >= 1 && wafer  <=  7);
  svtCorrection_st *p = pointers[layer-1][ladder-1][wafer-1][hybrid-1];
  if (! p) {
    for (Int_t i = 0; i < N; i++) {
      if (Data[i].layer == layer && 
	  Data[i].ladder == ladder && 
	  Data[i].wafer  == wafer && 
	  Data[i].hybrid == hybrid) {
	if (Data[i].Npar > -1) {
	  p = Data + i;
	  pointers[layer-1][ladder-1][wafer-1][hybrid-1] = p;
	}
	break;
      }
    }
  }
  return p;
}
//________________________________________________________________________________
Double_t St_svtCorrectionC::CalcCorrection(Int_t layer, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t u) {
  svtCorrection_st *p = pCorrection(layer, ladder, wafer, hybrid);
  return p ? STcheb(p->Npar, p->param, TMath::Abs(u/3.)) : 0;
}
