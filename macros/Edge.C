#include "TROOT.h"
#include "TMath.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TAxis.h"
#include "TArrayD.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TCanvas.h"
#include "Ask.h"
TGraph *grNpMin = 0;
TGraphErrors *grSigma = 0;
//________________________________________________________________________________
Double_t funcTanH(Double_t *x, Double_t *p) {
  return p[0]+p[1]*TMath::TanH(p[2]*(x[0]-p[3]));
} 
//________________________________________________________________________________
TF1 *SatTanH(TString opt = "I") {
  TString fName("SatTanH"); fName += opt;
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
  if (! f) {
    f = new TF1(fName,funcTanH,1, 10, 4);
    f->SetParNames("offset","scale","slope","shift");
    Double_t pars[2][4] = {
      {  0.062714,  0.0064137,     8.6146,     5.5759}, //NpdNI_1
      {  0.065534,  0.0090713,     8.9499,     5.4324}  //NpdNO_1
    };
    Int_t io = 0;
    if (opt == "O") io = 1;
    f->SetParameters(pars[io]);
    
  }
  return f;

}
//________________________________________________________________________________
TGraph  *Edge(TH2 *hist = 0) {
  TF1::InitStandardFunctions();
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  TGraph *gr = 0;
  if (! hist) return gr;
  TAxis *xa = hist->GetXaxis();
  TString opt("I");
  Int_t io = 0;
  if (TString(hist->GetName()).Contains("O")) {
    opt = "O";
    io = 1;
  }
  cout << "hist\t" << hist->GetName() << "\twith opt =" << opt.Data() << endl;
  Double_t parNpmin[2][2] = {
    {    3.7616,   -0.98436}, //   NpdNI = log(N/Np) = pars[0] + pars[1]*log(Np)
    {    3.9606,   -0.97581} // NpdNO
  };
  Double_t parSigma[2][4] = {
    {   0.60648,   -0.18784,   0.020405, -0.0007576}, //grSigma MpdN
    {   0.57417,   -0.17191,   0.017883, -0.00063043} //grSigma MpdNO
  };
  TF1 *Gaus = new TF1("Gaus","TMath::Exp([0]-0.5*TMath::Power((x-[1])/[2],2))",-5,5);
  Int_t nx = xa->GetNbins();
  TArrayD x(nx);
  TArrayD y(nx);
  TArrayD sigma(nx);
  TArrayD dsigma(nx);
  TF1 *gaus = (TF1 *) gROOT->GetListOfFunctions()->FindObject("gaus");
  TF1 *pol3 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol3");
  pol3->SetParameters(parSigma[io]);
  TF1 *sat  = SatTanH(opt);
  for (Int_t i = 1; i <= nx; i++) {
    TH1D *proj = hist->ProjectionY(Form("%s_b%i",hist->GetName(),i),i,i);
    x[i-1] = xa->GetBinCenter(i);
    Int_t j = proj->FindFirstBinAbove(0.0);
    y[i-1] = hist->GetYaxis()->GetBinCenter(j);
    Double_t mu = sat->Eval(x[i-1]);
    Double_t sigmaD =  pol3->Eval(x[i-1]);
    Gaus->SetParameter(0, 0);
    Gaus->FixParameter(1,  mu);
    Gaus->FixParameter(2, sigmaD);
    //    proj->Fit(gaus,"rmie","",y[i-1],10.0);
    proj->Fit(Gaus,"mie");
    sigma[i-1] = Gaus->GetParameter(2);
    dsigma[i-1] = Gaus->GetParError(2);
    c1->Update();
    if (Ask()) break;
  }
  gr = new TGraph(nx, x.GetArray(), y.GetArray());
  gr->Draw("axp");
  grNpMin = gr;
  grSigma = new TGraphErrors(nx, x.GetArray(), sigma.GetArray(), 0, dsigma.GetArray());
  return gr;
}
