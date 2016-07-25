#include "TCanvas.h"
#include "TH1.h"
#include "TF1.h"
TF1 *Fit6g(TH1 *CShape_py) {
  if (! CShape_py) return 0;
  //  TCanvas *c1 = new TCanvas();
  double params[18];
  const Int_t NG = 6;
  TF1 *g[NG];
  g[0] = new TF1("g1","gaus");
  g[1] = new TF1("g2","gaus(0)+gaus(3)");
  g[2] = new TF1("g3","gaus(0)+gaus(3)+gaus(6)");
  g[3] = new TF1("g4","gaus(0)+gaus(3)+gaus(6)+gaus(9)");
  g[4] = new TF1("g5","gaus(0)+gaus(3)+gaus(6)+gaus(9)+gaus(12)");
  g[5] = new TF1("g6","gaus(0)+gaus(3)+gaus(6)+gaus(9)+gaus(12)+gaus(15)");
  CShape_py->Fit(g[0]->GetName()); //c1->Update();
  for (Int_t i=1; i<NG; i++) {
    g[i-1]->GetParameters(params);
    params[3*i] = 100;
    params[3*i+1] = i;
    params[3*i+2] = 1;
    g[i]->SetParameters(params);
    CShape_py->Fit(g[i]->GetName());    CShape_py->Draw();//c1->Update();
  }
  Double_t norm = CShape_py->Integral()*CShape_py->GetBinWidth(1);
  printf("norm = %f\n", norm);
  char param[2000] = {""};
  char error[2000] = {""};
  sprintf(param,"Double_t parGaus[NpGaus] = {");
  sprintf(error,"Double_t perGaus[NpGaus] = {");
  int l;
  for (int i=0; i<18; i++) {
    double par = g[NG-1]->GetParameter(i);
    double err = g[NG-1]->GetParError(i);
    if (i%3 == 0) {
      l = strlen(param);
      sprintf(&param[l],"\n");
      par /= norm; err /= norm;
    }
    l = strlen(param);
    sprintf(&param[l],"%12.5e,",par);
    l = strlen(error);
    sprintf(&error[l],"%12.5e,",err);
  }
  printf("%s};\n",param);
  printf("%s};\n",error);
  return g[NG-1];
}
