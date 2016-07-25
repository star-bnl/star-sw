#include <assert.h>
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TF1.h"
#endif
//_______ banch of inv. Laplca transoformations
struct MapL_t {
  Int_t key;
  Char_t *Name;
};
static MapL_t MapL[] = {
  {  1, "1/s"},
  {  2, "1/s**2"},
  {  3, "1/s**n"},
  {  4, "1/sqrt(s)"},
  {  5, "s**(-3/2)"},
  {  6, "s**(-(n+1)/2"},
  {  8, "1/(s-a)"},
  {  9, "1/(s-a)**2"},
  { 10, "1/(s-a)**n"},
  { 12, "1/(s-a)/(s-b)"},
  { 13, "s/(s-a)/(s-b)"},
  { 14, "1/(s-a)/(s-b)/(s-c)"},
  { 15, "1/(s**2 + a**2)"},
  { 16, "s/(s**2 + a**2)"},
  { 17, "1/(s**2 - a**2)"},
  { 18, "s/(s**2 - a**2)"},
  { 19, "1/s/(s**2 + a**2)"},
  { 20, "1/s**2/(s**2 - a**2)"},
  { 21, "1/(s**2 + a**2)**2"},
  { 22, "s/(s**2 + a**2)**2"},
  { 23, "s**2/(s**2 + a**2)**2"},
  { 24, "(s**2 - a**2)/(s**2 + a**2)**2"},
  { 25, "s/(s**2 + a**2)/(s**2 + b**2)"},
  { 26, "1/((s-a)**2 + b**2)"},
  { 27, "(s-a)/((s-a)**2 - b**2)"},
  {101, "1/s"},
  {102, "1/(s-a)"},
  {103, "1/s/(s-a)"},
  {104, "(s + b)/s/(s - a)"},
  {106, "(s+c)/(s-a)/(s-b)"},
  {301, "1/s**2"},
  {302, "1/(s-a)**2"},
  {307, "1/s/(s-a)**2"},
  {310, "1/(s-a)**2/(s-b)"}
};
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Int_t n=0) {
  Double_t val = 0;
  switch (key) {
  case 1:// key is formula no. in Table 8.4-1 of Korn
  case 101:
    // 1/s
    val = 1;
    break;
  case 2:
  case 301: //             3.1 in Table 8.4-2
    // 1/s**2
    val = t;
    break;
  case 3:
    //  case 7: // n != float
    // 1/s**n 
    assert(n);
    if (n == 0) val = 1;
    else        val = TMath::Power(t,n-1)/TMath::Factorial(n-1);
    break;
  case 4:
    // 1/sqrt(s)
    val = 1./TMath::Sqrt(TMath::Pi()*t);
    break;
  case 5:
    // s**(-3/2)
    val = 2.*TMath::Sqrt(t/TMath::Pi());
    break;
  case 6:
    // s**-(n+1/2)
    val = TMath::Power(2.,n)*TMath::Power(t,n-0.5)/TMath::Sqrt(TMath::Pi());
    for (int i = 1; i <= n; i++) val /= 2*n - 1;
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key," << n << ")" << endl;
  }
  return val;
}
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Double_t a, Int_t n = 0) {
  Double_t val = 0;
  switch (key) {
  case 8:
  case 102:
    // 1/(s - a)
    if (a == 0.) val = InvLaplace(t,1);
    else         val = TMath::Exp(a*t);
    break;
  case 9:
  case 302:
    // 1/(s-a)**2
    if (a == 0.) val = InvLaplace(t,2);
    else         val = t*TMath::Exp(a*t);
    break;
  case 10:
    //  case 11:
    // 1/(s - a)**n
    if (a == 0) val = InvLaplace(t,3,n);
    else        val = TMath::Power(t,n-1)*TMath::Exp(a*t)/TMath::Factorial(n-1);
    break;
  case 103:
    // 1/s/(s-a)
    if (a != 0) val = (TMath::Exp(a*t) - 1)/a;
    else        val = InvLaplace(t,2);
    break;
  case 307:
    // 1/s/(s-a)**2
    if (a != 0) val = 1./(a*a)*(1 + (a*t - 1)*TMath::Exp(a*t));
    else        val = InvLaplace(t,3,3);
    break;
  case 15:
    // 1/(s**2 + a**2)
    if (a != 0) val = TMath::Sin(a*t)/a;
    else        val = InvLaplace(t,2);
    break;
  case 16:
    // s/(s**2 + a**2)
    if (a != 0) val = TMath::Cos(a*t);
    else        val = InvLaplace(t,1);
    break;
  case 17:
    // 1/(s**2 - a**2)
    if (a != 0) val = TMath::SinH(a*t)/a;
    else        val = InvLaplace(t,2);
    break;
  case 18:
    // s/(s**2 - a**2)
    if (a != 0) val = TMath::CosH(a*t);
    else        val = InvLaplace(t,1);
    break;
  case 19:
    // 1/s/(s**2 + a**2)
    if (a != 0) val = (1. - TMath::Cos(a*t))/(a*a);
    else        val = InvLaplace(t,3,3);
    break;
  case 20:
    // 1/s**2/(s**2 - a**2)
    if (a != 0) val = (a*t - TMath::Sin(a*t))/(a*a*a);
    else        val = InvLaplace(t,3,4);
    break;
  case 21:
    // 1/(s**2 + a**2)**2
    if (a != 0) val = (TMath::Sin(a*t) - a*t*TMath::Cos(a*t))/(2*a*a*a);
    else        val = InvLaplace(t,3,4);
    break;
  case 22:
    // s/(s**2 + a**2)**2
    if (a != 0) val = t*TMath::Sin(a*t)/(2*a);
    else        val = InvLaplace(t,3,3);
    break;
  case 23:
    // s**2/(s**2 + a**2)**2
    if (a != 0) val = (TMath::Sin(a*t) + a*t*TMath::Cos(a*t))/(2*a);
    else        val = InvLaplace(t,2);
    break;
  case 24:
    // (s**2 - a**2)/(s**2 + a**2)**2
    if (a != 0) val = t*TMath::Cos(a*t);
    else        val = InvLaplace(t,2);
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key,a )" << endl;
  }
  return val;
}
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Double_t a, Double_t b) {
  Double_t val = 0;
  switch (key) {
  case 25:
    // s/(s**2 + a**2)/(s**2 + b**2)
    if (a*a != b*b) val = (TMath::Cos(a*t) - TMath::Cos(b*t))/(b*b - a*a);
    else            val = InvLaplace(t,22,a,b);
    break;
  case 26:
    // 1/((s-a)**2 + b**2)
    if (b != 0) val = TMath::Exp(a*t)*TMath::Sin(b*t)/b;
    else        val = InvLaplace(t,9,a);
    break;
  case 27:
    // (s - a)/((s-a)**2 - b**2)
    if (b != 0) val = TMath::Exp(a*t)*TMath::Cos(b*t);
    else        val = InvLaplace(t,8,a);
    break;
  case 104:
    // (s + b)/s/(s - a)
    if (a != 0)  val  = (1 + b/a)*TMath::Exp(a*t) - b/a;
    else         val  = InvLaplace(t,1) + b*InvLaplace(t,2);
    break;
  case 12:
    // 1/(s-a)/(s-b)
    if (a != b) val = 1./(a-b)*(TMath::Exp(a*t)-TMath::Exp(b*t));
    else        val = InvLaplace(t,302,a,2);
    break;
  case 13:
    // s/(s-a)/(s-b)
    if (a == 0)   val = InvLaplace(t,8,b);
    else {
      if (b == 0) val = InvLaplace(t,8,a);
      else {
	if (a != b)  val = (a*TMath::Exp(a*t) - b*TMath::Exp(b*t))/(a-b);
	else         val = InvLaplace(t,8,a) + a*InvLaplace(t,9,a);
      }
    }
    break;
  case 310:
    // 1/(s-a)**2/(s-b) => (A + A_1*t)*exp(a*t) + B*exp(b*t); A = -1/(a-b)**2; A1 = 1/(a-b); B = -A;
    //                     (t/(a-b) - 1/(a-b)**2)*exp(a*t) + 1/(a-b)**2*exp(b*t)
    //                    1/(a-b)**2*(((a-b)*t -1)*exp(a*t)) + exp(b*t)
    if (a != b) val = TMath::Power(a-b,-2)*(((a-b)*t - 1)*TMath::Exp(a*t) + TMath::Exp(b*t)); 
    else        val = InvLaplace(t,10,a,3);
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key,a,b)" << endl;
  }
  return val;
}
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Double_t a, Double_t b, Double_t c) {
  Double_t val = 0;
  switch (key) {
  case 14:
    // 1/(s-a)/(s-b)/(s-c)
    if (a != b && a != c && a != b) 
      val = - ((b-c)*TMath::Exp(a*t) + (c-a)*TMath::Exp(b*t) + (a-b)*TMath::Exp(c*t))/
	((a-b)*(b-c)*(c-a));
    else {
      if ( a == b && a == c) 
	val = InvLaplace(t,10, a, 3);
      else {
	if (a == c) val = InvLaplace(t,310, a, b);
	if (a == b) val = InvLaplace(t,310, a, c);
	if (b == c) val = InvLaplace(t,310, b, a);
      }
    }
    break;
  case 106:
    // (s+c)/(s-a)/(s-b) = s/(s-a)/(s-b) + c/(s-a)/(s-b)
    val = InvLaplace(t,13,a,b) + c*InvLaplace(t,12,a,b);
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key,a,b,c)" << endl;
  }
  return val;
}
//________________________________________________________________________________
void InvLaplace() {
  Int_t N = sizeof(MapL)/sizeof(MapL_t);
  for (Int_t i = 0; i < N; i++) 
    cout << "key \t" << MapL[i].key << " ==> " << MapL[i].Name << endl;
}
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t mn, Double_t *params) {
  // Calculate inv. Laplace transofrmation for U_m(p)/V_n(p)
  //  U_m and V_m are polinoms power m and n, respectively, and m < n
  // params[0:m-1] are roots for U_m and
  // params[m:m+n-1] are roots for V_n
  Int_t m = mn/10;
  Int_t n = mn%10;
  Double_t value = 0;
  if (m >= n) return value;
  TString u("");
  if (m == 0) u = "1";
  else {
    for (Int_t i = 0; i < m; i++) {
      if (i != 0) u += "*";
      u += Form("(x-[%i])",i);
    }
  }
  TFormula U("U",u);
  U.SetParameters(params);
  U.Print();
  TString v("");
  for (Int_t i = 0; i < n; i++) {
    if (i != 0) v += "+";
    Int_t k = 0;
    for (Int_t j = 0; j < n; j++) {
      if (i == j) continue;
      if (k != 0) v += "*";
      k++;
      v += Form("(x-[%i])",j);
    }
  }
  TFormula V("V",v);
  V.SetParameters(&params[m]);
  V.Print();
  for (Int_t i = 0; i < n; i++) {
    Double_t pi = params[m+i];
    value += U.Eval(pi)/V.Eval(pi)*TMath::Exp(pi*t);
  }
  return value;
}
//________________________________________________________________________________
Double_t shaperP(Double_t *x, Double_t *par) {
  /* Pulser response (in time bins)
     Pulser itself = [2]*(1 - exp(-[0]*(x-[1])))+(1-[2])*(1 - exp(-[3]*(x-[1])))"
     derivative     "[2]* [0]*exp(-[0]*(x-[1])) +(1-[2])* [3]*exp(-[3]*(x-[1]))"
   0 a= 1/tau1       2.48771e-01   2.84490e+00   3.57620e-04   2.99469e-05 unit 1/100 ns => tau1 = 402 ns
   1    Shift        1.70133e+02   1.61923e+00   3.92587e-04  -3.74836e-05
   2    fract        3.11708e-01   5.93669e-01   9.58660e-04  -1.96876e-05
   3 b= 1/tau2       9.33658e-01   9.21941e+00   9.23060e-04   4.00249e-05 unit 1/100 ns => tau2 = 107 ns

   For integral

   P(t) =  1 - fract*exp(-a*t) - (1-fract)*exp(-b*t); frac1 = 1 - fract;
   P(s) =  1/s - fract/(s+a) - frac1/(s+b);
   Shaper(s) = (s + p)/(s + f);
   
   S(s) = P(s)*Shaper(s) = [1/s - fract/(s+a) - frac1/(s+b)]*(s+p)/(s+f) =
   (s+p)/(s+f)/s - fract*(s+p)/(s+f)/(s+a) - frac1*(s+p)/(s+f)/(s+b) =
   1/(s+f) + p/(s*(s+f)) - fract*(s+p)/(s+f)/(s+a) - frac1*(s+p)/(s+f)/(s+b)

   For derivatives
   P(t) =  a*fract*exp(-a*t) + b*(1-fract)*exp(-b*t); frac1 = 1 - fract;
   P(s) = a*fact/(s+a) + b*(1-fract)/(s+b);
   S(s) = (a*fact/(s+a) + b*(1-fract)/(s+b))*(s+p)/(s+f) =
   = a*fact*(s+p)/(s+f)/(s+a) + b*(1-fract)*(s+p)/(s+f)/(s+b)

  */
  Double_t a = par[0];
  Double_t b = par[3];
  Double_t fract = par[2];
  Double_t frac1 = 1 - fract;
  Double_t t = x[0] - par[1];  //  cout << "t\t" << t << endl;
  if (t <= 0) return 0;
  Double_t p = par[5];
  Double_t f = par[6];
#if 0
  Double_t val1 = InvLaplace(t,102,-f); // cout << "val1\t" << val1 << endl;
  Double_t val2 = p*InvLaplace(t,103,-f); // cout << "val2\t" << val2 << endl;
  Double_t val3 = fract*InvLaplace(t,106, -a, -f, p); // cout << "val3\t" << val3 << endl;
  Double_t val4 = frac1*InvLaplace(t,106, -b, -f, p); // cout << "val4\t" << val4 << endl;
  Double_t Value = val1 + val2 - val3 - val4; // cout << "Value\t" << Value << endl;
#else
  Double_t val1 = a*fract*InvLaplace(t,106,-a,-f,p); 
  Double_t val2 = b*frac1*InvLaplace(t,106,-b,-f,p);
  Double_t Value = val1 + val2;
#endif
  return Value;
}
//________________________________________________________________________________
Double_t shaperF(Double_t *x, Double_t *par) {
  return par[0] + par[1]*shaperP(x,&par[2]);
}
//________________________________________________________________________________
TF1 *ShaperP() {
  TF1 *shaperPf = new TF1("shaperPf",shaperP,160,270,7);
  shaperPf->SetParName(0,"a");     shaperPf->SetParameter(0,2.48771e-01);
  shaperPf->SetParName(1,"Shift"); shaperPf->SetParameter(1,1.70133e+02);
  shaperPf->SetParName(2,"fract"); shaperPf->SetParameter(2,3.11708e-01);
  shaperPf->SetParName(3,"b");     shaperPf->SetParameter(3,9.33658e-01);
  shaperPf->SetParName(4,"c");     shaperPf->FixParameter(4,0);
  shaperPf->SetParName(5,"p");     shaperPf->SetParameter(5,1.);
  shaperPf->SetParName(6,"f");     shaperPf->SetParameter(6,10.);
  return shaperPf;
}
//________________________________________________________________________________
TF1 *ShaperF() {
  TF1 *shaperFf = new TF1("shaperFf",shaperF,0,170,9);
  shaperFf->SetParName(0,"base");     shaperFf->SetParameter(0,0);
  shaperFf->SetParName(1,"Norm");     shaperFf->SetParameter(1,10);
  shaperFf->SetParName(2,"a");     shaperFf->SetParameter(2,2.48771e-01);
  shaperFf->SetParName(3,"Shift"); shaperFf->SetParameter(3,40);
  shaperFf->SetParName(4,"fract"); shaperFf->SetParameter(4,3.11708e-01);
  shaperFf->SetParName(5,"b");     shaperFf->SetParameter(5,9.33658e-01);
  shaperFf->SetParName(6,"c");     shaperFf->FixParameter(6,0);
  shaperFf->SetParName(7,"p");     shaperFf->SetParameter(7,1.);
  shaperFf->SetParName(8,"f");     shaperFf->SetParameter(8,10.);
  return shaperFf;
}
