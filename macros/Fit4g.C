#include "TCanvas.h"
#include "TH1.h"
#include "TF1.h"
TF1 *Fit4g(TH1 *CShape_py) {
  if (! CShape_py) return 0;
  //  TCanvas *c1 = new TCanvas();
  double params[12];
  TF1 *g = new TF1("g","gaus");
  CShape_py->Fit("g"); //c1->Update();
  TF1 *g2 = new TF1("g2","gaus(0)+gaus(3)");
  g->GetParameters(params);
  params[4] =1;
  g2->SetParameters(params);
  CShape_py->Fit("g2");  //c1->Update();
  params[3] = 100;
  params[4] = 1;
  params[5] = 1;
  params[4] = 0;
  g2->SetParameters(params);
  CShape_py->Fit("g2");  //c1->Update();
  TF1 *g3 = new TF1("g3","gaus(0)+gaus(3)+gaus(6)");
  g2->GetParameters(params);
  params[6] = 1;
  params[7] = 0;
  params[8] = 1;
  g3->SetParameters(params);
  CShape_py->Fit("g3");
  TF1 *g4 = new TF1("g4","gaus(0)+gaus(3)+gaus(6)+gaus(9)");
  g3->GetParameters(params);
  params[9]=0;
  params[10]=0;
  params[11]=1;
  g4->SetParameters(params);
  CShape_py->Fit("g4","im");
  Double_t norm = CShape_py->Integral()*CShape_py->GetBinWidth(1);
  printf("norm = %f\n", norm);
  g4->Draw();
  char param[200] = {""};
  char error[200] = {""};
  sprintf(param,"Double_t parGaus[NpGaus] = {");
  sprintf(error,"Double_t perGaus[NpGaus] = {");
  for (int i=0; i<12; i++) {
    double par = g4->GetParameter(i);
    double err = g4->GetParError(i);
    if (i%3 == 0) {
      par /= norm; err /= norm;
    }
    int l = strlen(param);
    sprintf(&param[l],"%f,",par);
    l = strlen(error);
    sprintf(&error[l],"%f,",err);
  }
  printf("%s};\n",param);
  printf("%s};\n",error);
  return g4;
}
