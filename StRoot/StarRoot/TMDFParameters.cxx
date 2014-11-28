#include "Riostream.h"
#include "TMDFParameters.h"
#include "TMath.h"
#include <assert.h>
TMDFParameters* TMDFParameters::fgTMDFParameters = 0;
using namespace std;
ClassImp(TMDFParameters);
//________________________________________________________________________________
TArrayD *TMDFParameters::GetTerms(Double_t *x) {
  TArrayD *TV = new TArrayD[fNvar];
  for (Int_t i = 0; i < fNvar; i++) {
    Int_t N = fMaxPower[i];
    Double_t xx = x[i];
    if (xx < fVmin[i]) xx = fVmin[i];
    if (xx > fVmax[i]) xx = fVmax[i];
    Double_t X = 1 + 2./(fVmax[i] - fVmin[i])*(xx - fVmax[i]);
    Int_t nn = N+1;
    if (nn < 3) nn = 3;
    TV[i] = TArrayD(nn);
    Tcheb(X,N,TV[i].GetArray());
  }
  return TV;
}
//________________________________________________________________________________
Double_t TMDFParameters::Eval(Double_t *x) {
  TArrayD *TV = GetTerms(x);
  Double_t value = fMean;
  for (Int_t i = 0; i < fNcoef; i++) {
    Double_t term = fCoef[i];
    Int_t p = fCode[i];
    for (Int_t j = fNvar - 1; j >= 0; j--) {
      Int_t k = p%10; assert(k >= 0);
      term *= TV[j][k-1]; 
      p /= 10;
    }
    value += term;
  }
  delete [] TV;
  return value;
}
//________________________________________________________________________________
Double_t TMDFParameters::dEval(Double_t *x) {
  TArrayD *TV = GetTerms(x);
  Double_t value = 0;
  for (Int_t i = 0; i < fNcoef; i++) {
    Double_t term = fdCoef[i];
    Int_t p = fCode[i];
    for (Int_t j = fNvar - 1; j >= 0; j--) {
      Int_t k = p%10; assert(k >= 0);
      term *= TV[j][k-1]; 
      p /= 10;
    }
    value += term*term;
  }
  delete [] TV;
  return TMath::Sqrt(value);
}
//________________________________________________________________________________
Double_t *TMDFParameters::Tcheb(Double_t x, Int_t N, Double_t *T) {
  T[0] = 1; T[1] = T[2] = 0;
  for (Int_t j = 1; j <= N; j++) {
    if (j == 1) T[j] = x; 
    else        T[j] = 2 * x * T[j-1] - T[j-2];
  }
  return &T[0];
}
//________________________________________________________________________________
Double_t TMDFParameters::Func(Double_t *x, Double_t *p) {
  if (! p) return Instance()->Eval(x);
  TArrayD X(Instance()->Nvar(),x);
  Int_t j = 0;
  for (Int_t i = 0; i < Instance()->Nvar(); i++) {
    if (p[i] > -999.0) X[i] = p[i];
    else               X[i] = x[j++];
  }
  return Instance()->Eval(X.GetArray());
}
//________________________________________________________________________________
TF1 *TMDFParameters::ProjectionX(Int_t code) {
  if (code < 0 || code >= Instance()->Nvar()) return 0;
  TF1 *f = new TF1(Form("Func%i",code),Func,Vmin(code),Vmax(code),4);
  Double_t params[4]; 
  for (Int_t i = 0; i < Instance()->Nvar(); i++) {
    params[i] = -999.;
    if (i != code) params[i] = Vmax(i) - (Vmean(i) - 1)*(Vmin(i) - Vmax(i))/2.;
  }
  f->SetParameters(params);
  return f;
}
//________________________________________________________________________________
TF2 *TMDFParameters::ProjectionXY(Int_t code1, Int_t code2) {
  if (code1 == code2) return 0;
  if (code1 < 0 || code1 >= Instance()->Nvar()) return 0;
  if (code2 < 0 || code2 >= Instance()->Nvar()) return 0;
  TF2 *f = new TF2(Form("Func%i_%i",code1,code2),Func,Vmin(code1),Vmax(code1),Vmin(code2),Vmax(code2),4);
  Double_t params[4]; 
  for (Int_t i = 0; i < Instance()->Nvar(); i++) {
    params[i] = -999.;
    if (i != code1 && i != code2) params[i] = Vmax(i) - (Vmean(i) - 1)*(Vmin(i) - Vmax(i))/2.;
  }
  f->SetParameters(params);
  return f;
}
//________________________________________________________________________________
void TMDFParameters::Print(Option_t */* option */) const {
  cout << "Sample statistics:" << "\n"
       << "------------------" << "\n"
       << "          ";          
  for (Int_t i = 0; i < fNvar; i++)
    cout << " " << setw(10) << i+1 << "\n";
  cout << "\n" << " Max:   " << "\n";
  for (Int_t i = 0; i < fNvar; i++)
    cout << " " << setw(10) << setprecision(4)
         << fVmax[i] << "\n";
  cout << "\n" << " Min:   " << "\n";
  for (Int_t i = 0; i < fNvar; i++)
    cout << " " << setw(10) << setprecision(4)
         << fVmin[i] << "\n";
  cout << "\n" << " Mean:  " << "\n";
  for (Int_t i = 0; i < fNvar; i++)
    cout << " " << setw(10) << setprecision(4)
         << fVmean[i] << "\n";
  cout << "\n";
  cout << "Coefficients:" << "\n"
       << "-------------" << "\n"
       << "   #         Value        Error   Powers" << "\n"
       << " ---------------------------------------" << "\n";
  for (Int_t i = 0; i < fNcoef; i++) {
    cout << " " << setw(3) << i << "  "
	 << setw(12) << fCoef[i] << "  "
	 << setw(12) << fdCoef[i] << "  " << "\n";
    Int_t p = fCode[i];
    TArrayI Power(fNvar);
    for (Int_t j = fNvar - 1; j >= 0; j--) {Power[j] = p%10; p /= 10;}
    for (Int_t j = 0; j < fNvar; j++) cout << " " << setw(3) << Power[j] - 1 << "\n";
    cout << endl;
  }
}
