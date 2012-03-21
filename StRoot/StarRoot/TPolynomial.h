#ifndef __TPolynomial_h__
#define __TPolynomial_h__
#include "Riostream.h"
#include "TMath.h"
#include "TString.h"
#include "TF1.h"

namespace TPolynomial {
  void MakePolySeries(Double_t x, Int_t type, Int_t Np, Double_t *P);
  Double_t CalcPoly(Double_t *x, Double_t *par);
  TF1 *MakePoly(TString Name = "Tcheb", Int_t Npar = 3, Int_t R = 1, Double_t xmin = -0.5, Double_t xmax = 0.5);
  TF1 *MakePol(const Int_t N=2, const Char_t *X = "log(x)", TString type = "PL", Int_t i0=0);
  void GetFunc(TF1 *func);
};
#endif
