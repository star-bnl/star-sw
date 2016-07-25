#include "RConfig.h"
#include "TMath.h"
#include "Riostream.h"
Double_t GausConstant[6] = {0.2482, 0.315, 0.2153, 0.200, 0.1861, 0.1734};
Double_t GausMean[6]     = {2.3170, 2.385, 2.4730, 2.566, 2.5990, 2.6580};
Double_t GausSigma[6]    = {1.326 , 1.464, 1.6360, 1.830, 1.9830, 2.1700};
Double_t cutT[6]         = {3.6   , 3.7  , 4     , 4.5  , 4.5   , 5     };
Double_t ExpConstant[6]  = {  1.46831,  1.44213,  1.4916,  1.73945,   1.54719,   1.70843};
Double_t ExpSlope[6]      ={-0.569438, -0.55952, -0.55763, -0.57317, -0.544514, -0.54852};
Double_t mTau = 55.e-9;
Double_t mTimeBinWidth = 1.0658037919167309e-07;
Double_t landauCut = 3.6;
Double_t landauConstant = 0.2703;
Double_t landauMean = 2.256;
Double_t landauSigma =  1.197;
Double_t expConstant = 1.56538/10.;
Double_t expSlope = -0.589033;
Double_t p = 3;
Double_t tmax = p - 1;
TCanvas *c1 = 0;
//________________________________________________________________________________
Double_t Int(Double_t *x, Double_t *par=0 ) {  
  Double_t tau = par[0];
  Double_t width = par[1];
  Double_t t = x[0]*width/mTau;
  Double_t Delta = width/mTau;
  Double_t t1 = t - Delta/2.;
  Double_t t2 = t1 + Delta;
  Double_t val = TMath::Gamma(p,t2) - TMath::Gamma(p,t1);
  //  cout << "Called" << endl;
  return val;
  //  return TMath::Power(tau,-p+1)*val/3.;
}
//________________________________________________________________________________
Double_t Intx(Double_t *x, Double_t *par=0 ) {  
  Double_t tau = par[0];
  Double_t width = par[1];
  Double_t t = x[0]*width/mTau;
  return Int(x,par)*t;
}
//________________________________________________________________________________
Double_t Shape(Double_t *x, Double_t *par=0 ) {
  Double_t t = 1e-9*x[0]/mTau;
  Double_t val = 0;
  if (t <= 0) return val;
  return t*t*TMath::Exp(-t)/3.;
}
//________________________________________________________________________________
Double_t Trs(Double_t *x, Double_t *p) {
  Double_t timeBinT = x[0]*mTimeBinWidth;
  Double_t signalTime = 0;
  Double_t t=(timeBinT-signalTime)/mTau;
  Double_t val = 0;
  if(t<=landauCut)
    val = landauConstant*TMath::Exp(-(t-landauMean)*(t-landauMean))/(2.*landauSigma*landauSigma)*mTimeBinWidth/mTau;
  else
    val = expConstant*TMath::Exp(t*expSlope)*mTimeBinWidth/mTau;
  //  cout << "t =" <<  t << "\t val=" << val << endl;
  return val;
}
//________________________________________________________________________________
Double_t TrsO(Double_t *x, Double_t *p) {
  Double_t timeBinT = x[0]*mTimeBinWidth;
  Double_t signalTime = 0;
  Double_t t=(timeBinT-signalTime)/mTau;
  Double_t val = 0;
  if(t<=landauCut)
    val = landauConstant*TMath::Exp(-(t-landauMean)*(t-landauMean))/(2.*landauSigma*landauSigma)*mTimeBinWidth/mTau;
  else
    val = 10*expConstant*TMath::Exp(t*expSlope)*mTimeBinWidth/mTau;
  //  cout << "t =" <<  t << "\t val=" << val << endl;
  return val;
}
//________________________________________________________________________________
Double_t TrsG(Double_t *x, Double_t *p) {
  Double_t timeBinT = x[0]*mTimeBinWidth;
  Double_t signalTime = 0;
  Double_t t=(timeBinT-signalTime)/mTau;
  Double_t val = 0;
  Int_t index = p[1];
  if(index > 5)index=5;
  Double_t GausSigma2 = GausSigma[index]*GausSigma[index];
  if(t<=cutT[index])
    val=GausConstant[index]*exp(-(t-GausMean[index])*(t-GausMean[index])/(2.*GausSigma2))*mTimeBinWidth/mTau;
  else
    val=ExpConstant[index]*exp(t*ExpSlope[index])*mTimeBinWidth/mTau;
  return val;
}
//________________________________________________________________________________
void Shaper() {
  Double_t xmin = -.5; Double_t ymin = 0.;
  Double_t xmax =  6.5; Double_t ymax = 0.75;
  Char_t *Names[6] = {"Shape","Int","Intx","Trs","TrsO","TrsG"};
  if (! c1) c1 = new TCanvas();
  c1->SetGrid();
  TH1F *frame = c1->DrawFrame(xmin,ymin,xmax,ymax);
  frame->SetTitle("Shaper");
  frame->SetXTitle("Time (ns)                   ");
  frame->SetYTitle("Response");
  TLegend *leg = new TLegend(0.65,0.7,0.9,0.9,"");
  Double_t tau = mTau/mTimeBinWidth;
  TF1 *func = 0;
  for (Int_t i = 3; i < 10; i++) {
    func = 0;
    if (i == 0)  {func = new TF1(Form("f%s",Names[i]),Shape,xmin,xmax,1); func->SetParameter(0,tau);}
    if (i == 1)  {func = new TF1(Form("f%s",Names[i]),Int,xmin,xmax,2); func->SetParameters(tau,mTimeBinWidth);}
    if (i == 2)  {func = new TF1(Form("f%s",Names[i]),Intx,xmin,xmax,2); func->SetParameters(tau,mTimeBinWidth);}
    if (i == 3)  {func = new TF1(Form("f%s",Names[i]),Trs,xmin,xmax,1); func->SetParameter(0,tau);}
    if (i == 4)  {func = new TF1(Form("f%s",Names[i]),TrsO,xmin,xmax,1); func->SetParameter(0,tau);}
    if (i >  4)  {
      func = new TF1(Form("f%s%i",Names[5],i-5),TrsG,xmin,xmax,2); 
      func->SetParameter(0,tau); 
      func->SetParameter(1,i-5);
    }
    if (func) { 
      func->SetLineColor(i+1);
      leg->AddEntry(func,func->GetName());
      func->Draw("same");
    }
  }
  leg->Draw();
}
