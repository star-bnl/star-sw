#include "TF1.h"
  static const Int_t NpGaus = 15;
  static Double_t parGaus[2][NpGaus] = {
    {0.027191, 1.398055,2.204621, //inner  FCN=569.038 
     0.305265, 0.293169,1.217812,
     0.002452,-2.654352,1.160718,
     0.263998, 0.104958,0.841211,
    -0.248429, 0.365819,1.037271},
    {0.013926, 2.815513,2.627633, // outer FCN=806.736
     0.070128, 1.012561,1.636476,
     0.057834,-0.293179,1.399835,
     0.162166,-0.042593,1.021048,
     0.000293, 4.041144,3.571061}
};
void fGaus(Double_t x, Double_t *val, Int_t l){
  val[0] = val[1] = 0;
  for (Int_t j=0; j<NpGaus; j+=3){
    Double_t dev = (x - parGaus[l][j+1])/parGaus[l][j+2];
    val[1] += parGaus[l][j]*exp(-0.5*dev*dev)*(-dev/parGaus[l][j+2]);
    val[0] += parGaus[l][j]*exp(-0.5*dev*dev);
  }
}
//________________________________________________________________________________
Double_t g5funcI(Double_t *x,Double_t *par) {
  Double_t val[2];
  fGaus(x[0],val,0);
  return par[0]*val[0];
}
//________________________________________________________________________________
Double_t g5funcO(Double_t *x,Double_t *par) {
  Double_t val[2];
  fGaus(x[0],val,1);
  return par[0]*val[0];
}
void g5() {
  TF1 *g5I = new TF1("g5I","gaus(0)+gaus(3)+gaus(6)+gaus(9)+gaus(12)");
  Int_t l = 0;
  for (Int_t j=0; j<NpGaus; j+=3){
    g5I->SetParameter(j  ,parGaus[l][j  ]*3.13835e+06);
    g5I->SetParameter(j+1,parGaus[l][j+1]);
    g5I->SetParameter(j+2,parGaus[l][j+2]);
  }
  TF1 *g5O = new TF1("g5O","gaus(0)+gaus(3)+gaus(6)+gaus(9)+gaus(12)");
  l = 1;
  for (Int_t j=0; j<NpGaus; j+=3){
    g5I->SetParameter(j  ,parGaus[l][j  ]*9.97143e+06);
    g5I->SetParameter(j+1,parGaus[l][j+1]);
    g5I->SetParameter(j+2,parGaus[l][j+2]);
  }
  TF1 *sI = new TF1("sI",g5funcI,-15,25,1);
  sI->SetParameter(0,1);
  sI->SetLineColor(1);
  sI->Draw("same");
  TF1 *sO = new TF1("sO",g5funcO,-15,25,1);
  sO->SetParameter(0,1);
  sO->SetLineColor(2);
  sO->Draw("same");
}
