#include "TPolynomial.h"
using namespace std;
//________________________________________________________________________________
void TPolynomial::MakePolySeries(Double_t x, Int_t type, Int_t Np, Double_t *P) {
  // Recurrent formulae to polynoms
  P[0] = 1.;
  for (Int_t i = 1; i < Np; i++) {
    Int_t n = i - 1;
    if (!n) {
      switch (type) {
      case 3: P[n+1] = -x + 1; break; //  Laguerre
      case 4: P[n+1] =    2*x; break; //  Hermite
      case 0: // Polymonial
      case 1: // Tchebyshev
      case 2: // Legendre
      default:	P[n+1] = x; break;
      }
    }
    else {
      switch (type) {
      case 1:  P[n+1] =        2*x*P[n] -   P[n-1];        break;//Tchebyshev[-1,1]: P_0 = 1; P_1 = x; P_n+1 = 2*x*P_n - P_n-1
      case 2:  P[n+1] =((2.*n+1)*x*P[n] - n*P[n-1])/(n+1); break;//Legendre  [-1,1]: P_0 = 1, P_1 = x, P_n+1 = (2*n+1)/(n+1)*x*P[n]-n/(n+1)*P_n-1
      case 3:  P[n+1] =  (2*n+1-x)*P[n]-n*n*P[n-1];        break;//Laguerre        : P_n+1 = (2*n+1-x)*P_n - n**2*P_n-1
      case 4:  P[n+1] =        2*x*P[n]-2*n*P[n-1];        break;//Hermite         : P_n+1 = 2*x*P_n - 2*n*P_n-1
      case 0:
      default: P[n+1] =            P[n]*x;                 break; // Polymonial    : P_n+1 = P_n *x;
      }
    }
  }
}
//________________________________________________________________________________
Double_t TPolynomial::CalcPoly(Double_t *x, Double_t *par) {
  //  Int_t key = Npar + 1 + 100*(100*R + T);
  Int_t key = TMath::Nint(par[0]);
  Int_t R    =  key/(100*100);
  key -= R*(100*100);
  Int_t T    =  key/100;
  key -= T*100;
  Int_t n    =  key; // no. of coeff.
  Double_t X;
  Double_t scale = 1;
  Int_t i1 = 1;
  Int_t i2 = n;
  switch (R) {
  case 1:  X = 2*x[0]; break; // [-0.5,0.5] => [-1,1]
  case 2:  X = TMath::Log(x[0]); break;
  case 3:  X = TMath::Exp(x[0]); break;
  case 4:  X = 2*x[0] - 1;       break; // [0,1] => [-1,1]
  case 6:  scale = x[0]*(1-TMath::Abs(x[0])); i1 = 0; i2--;
  case 5:  X = 2*TMath::Power(x[0],2) - 1; break; // [-1,1]
  case 8:  scale = 2*x[0]*(1-TMath::Abs(2*x[0])); i1 = 0; i2--;
  case 7:  X = 8*TMath::Power(x[0],2) - 1; break; // [-.5,0.5]
  default: X =   x[0]; break;
  }
  Int_t np   = n+i1;
  Double_t *P = new Double_t[np];
  TPolynomial::MakePolySeries(X,T,np,P);
  Int_t ip = 1;
  Double_t val = par[ip]*P[0]; ip++;
  for (Int_t i = i1; ip < n; i++, ip++) {
    val += scale*par[ip]*P[i];
  }
  delete [] P;
  return val;
}
//________________________________________________________________________________
TF1 *TPolynomial::MakePoly(TString Name, Int_t Npar, Int_t R, Double_t xmin, Double_t xmax) {
  // param[0] = RRTTnn
  // R = 0: X => x
  // R = 1: X => 2*x
  // R = 2: X => log(x)
  // R = 3: X => exp(x);
  // R = 4: X => 2*x - 1                                                           
  // R = 5; X => 2*x**2 - 1; f = a_0 +   P(X); use only const + even series member [-1,1]
  // R = 6: X => 2*x**2 - 1; f = a_0 + x*P(X); use only const + odd  series member
  // R = 7; X => 8*x**2 - 1; f = a_0 +   P(X); use only const + even series member [-.5,0.5]
  // R = 8: X => 8*x**2 - 1; f = a_0 + x*P(X); use only const + odd  series member
  Int_t T = 0;
  if (Name.BeginsWith("Pol",TString::kIgnoreCase)) T = 0; // Polymonial
  if (Name.BeginsWith("Tch",TString::kIgnoreCase)) T = 1; // Tchebyshev
  if (Name.BeginsWith("Leg",TString::kIgnoreCase)) T = 2; // Legendre
  if (Name.BeginsWith("Lag",TString::kIgnoreCase)) T = 3; // Laguerre
  if (Name.BeginsWith("Her",TString::kIgnoreCase)) T = 4; // Hermite
  Int_t key = Npar + 1 + 100*(100*R + T);
  TF1 *f = new TF1(Form("%s_%i_%i",Name.Data(),R,Npar),TPolynomial::CalcPoly,xmin,xmax,Npar+1);
  f->FixParameter(0,key);
  for (Int_t i = 1; i <= Npar; i++) f->SetParameter(i,0);
  return f;
}
//________________________________________________________________________________
TF1 *TPolynomial::MakePol(const Int_t N, const Char_t *X, TString type, Int_t i0) {
  TString Func("");
  if (! type.CompareTo("PL",TString::kIgnoreCase)) {// polynoms
    Func = Form("[%i]",N); cout << Func << endl;
    for (int n = N-1+i0; n>=i0; n--) {
      TString temp;
      if (n < N-1) temp = Form("%s*(%s)+[%i]",X,Func.Data(),n); 
      else 	   temp = Form("%s*%s+[%i]",X,Func.Data(),n); 
      Func = temp; cout << Func << endl;
    }
  }
  else {
    TString T0("1"); Func = Form("[0]");
    TString T1("");
    TString T2("");
    TString xx("");
    if (N >= 1) {
      if (! type.CompareTo("TCheb",TString::kIgnoreCase)) xx = Form("%s",X);  // Tchebyshev        : T_0 = 1; T_1 = x;
      else                                                xx = Form("(2*%s-1)",X); // Shifted Tchebyshev: S_0 = 1; S_1 = 2*x - 1 
      T1 = xx;
      Func += Form("+[1]*%s",T1.Data()); cout << Func << endl;
      for (int n = 2; n<=N; n++) {
	T2 = Form("(2*%s*%s-%s)",xx.Data(),T1.Data(),T0.Data());       // T_n+1 = 2*x*T_n - T_n-1
	//     T2 = Form("(2*(2*%s-1)*%s-%s)",X,T1.Data(),T0.Data());  // S_n+1 = 2*(2*x-1)*S_n - S_n-1
	cout << T0 << "\t" << T1 << "\t" << T2 << endl;
	Func += Form("+[%i]*%s",n,T2.Data()); cout << Func << endl;
	T0 = T1;
	T1 = T2;
      }
    }
  }
  TF1 *fun = 0;
  if (Func != "") {fun = new TF1(Form("%s%i",type.Data(),N),Func.Data()); cout << "Make " << fun->GetName() << endl;}
  return fun;
}
//________________________________________________________________________________
void TPolynomial::GetFunc(TF1 *func) {
  if (! func) return;
  TString Func(func->GetTitle());
  Int_t Npar = func->GetNpar();
  for (Int_t p = 0; p < Npar; p++) {
    TString par("");
    par += func->GetParameter(p);
    Func.ReplaceAll(Form("[%i]",p),par.Data());
  }
  cout << Func << endl;
}
