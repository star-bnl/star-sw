#include "TCanvas.h"
#include "TH1.h"
#include "TF1.h"
TF1 *Fit5g(TH1 *CShape_py) {
  if (! CShape_py) return 0;
  //  TCanvas *c1 = new TCanvas();
  double params[15];
  TF1 *g = new TF1("g","gaus");
  CShape_py->Fit("g"); //c1->Update();
  TF1 *g2 = new TF1("g2","gaus(0)+gaus(3)");
  g->GetParameters(params);
  params[3] = 100;
  params[4] = 1;
  params[5] = 1;
  g2->SetParameters(params);
  CShape_py->Fit("g2");    CShape_py->Draw();//c1->Update();
  TF1 *g3 = new TF1("g3","gaus(0)+gaus(3)+gaus(6)");
  g2->GetParameters(params); 
  params[6] = 1;
  params[7] = 0;
  params[8] = 1;
  g3->SetParameters(params);
  CShape_py->Fit("g3");   CShape_py->Draw();
  TF1 *g4 = new TF1("g4","gaus(0)+gaus(3)+gaus(6)+gaus(9)");
  g3->GetParameters(params);
  params[9]=1;
  params[10]=0;
  params[11]=1;
  g4->SetParameters(params);
  CShape_py->Fit("g4");   CShape_py->Draw();
  g4->GetParameters(params);
  TF1 *g5 = new TF1("g5","gaus(0)+gaus(3)+gaus(6)+gaus(9)+gaus(12)");
  params[12]=1;
  params[13]=1;
  params[14]=1;
  g5->SetParameters(params);
  CShape_py->Fit("g5","im");   CShape_py->Draw();
  Double_t norm = CShape_py->Integral()*CShape_py->GetBinWidth(1);
  printf("norm = %f\n", norm);
  char param[2000] = {""};
  char error[2000] = {""};
  sprintf(param,"Double_t parGaus[NpGaus] = {");
  sprintf(error,"Double_t perGaus[NpGaus] = {");
  int l;
  for (int i=0; i<15; i++) {
    double par = g5->GetParameter(i);
    double err = g5->GetParError(i);
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
  return g5;
}
