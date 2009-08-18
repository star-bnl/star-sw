#include <TMath.h>
#include <TGraphErrors.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TStyle.h>
#include <TF1.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TPad.h>
#include <TLegend.h>
#include <TMultiGraph.h>
#include <TColor.h>
#include <assert.h>

#include <fstream>
using namespace std;

#include <StEmcPool/StPhotonCommon/PhotonAnalysisSettings.h>

#include "macros/systematics/systematics_rda.h"
#include "macros/systematics/systematics_dau.h"
#include "macros/systematics/systematics_pp.h"

#include "AnaCuts.h"
#include "Pi0Analysis.h"

#include "PhotonAnalysisUtil.h"

ClassImp(PhotonAnalysisUtil)

double pqcdlowpt(double *x,double *par)
{
  double ret=par[0]*TMath::Power(1.+x[0],par[1]);
  return ret;
}
double pqcdhighpt(double *x,double *par)
{
  double ret=par[0]*TMath::Power(1.+x[0],par[1]);
  return ret;
}
double sumpqcd(double *x,double *par)
{
  //transition (SW=1 is old)
  //params: 2x low, 2x hi, 2x trans
  double SW=1. - 1./(1.+TMath::Exp(TMath::Abs(par[4])*(x[0]-par[5])));
  double ret=SW*pqcdhighpt(x,&par[2]);
  ret+=(1.-SW)*pqcdlowpt(x,par);
  //ret=pqcdlowpt(x,par);
  return ret;
}
void printPoints(TGraphErrors *g)
{
  for(Int_t i=0;i<g->GetN();i++){
    double x,y,ey;
    g->GetPoint(i,x,y);
    ey=g->GetErrorY(i);
    cout<<x<<"   "<<y<<"   "<<ey<<endl;
  }
}
void divideGraphWithFunction(TGraphErrors *graph,TF1 *func)
{
  for(int i=0;i<graph->GetN();i++){
    double x=0.;
    double y=0.;
    graph->GetPoint(i,x,y);
    double ey=graph->GetErrorY(i);
    graph->SetPoint(i,x,y/func->Eval(x));
    graph->SetPointError(i,0.,ey/func->Eval(x));
  }
}
void divideGraphWithGraph(TGraphErrors *graph,TGraph *denom)
{
  for(int i=0;i<graph->GetN();i++){
    double x=0.;
    double y=0.;
    graph->GetPoint(i,x,y);
    double ey=graph->GetErrorY(i);
    graph->SetPoint(i,x,y/denom->Eval(x));
    graph->SetPointError(i,0.,ey/denom->Eval(x));
  }
}
void getRatio(TH1F* h_ratio,TH1F* h_eff,TH1F* h_effSingle,TF1 *f_piondecay)
{
  //copy, don't want to mess with the pointers:
  TH1F *h_eff_copy=new TH1F(*h_eff);
  for(int i=1;i<=h_eff->GetNbinsX();i++){
    h_eff->SetBinError(i,0.);
  }
  TH1F *h_effSingle_copy=new TH1F(*h_effSingle);
  for(int i=1;i<=h_effSingle->GetNbinsX();i++){
    h_effSingle_copy->SetBinError(i,0.);
  }
  //correct for single photon fraction:
  h_ratio->Add(f_piondecay,-1.);
  for(int i=1;i<=h_ratio->GetNbinsX();i++){
    //cout<<i<<endl;
    //if(h_ratio->GetBinContent(i)<0.) cout<<"negative"<<endl;
  }
  h_eff_copy->Divide(h_effSingle_copy);
  h_ratio->Multiply(h_eff_copy);
  h_ratio->Add(f_piondecay);
}
void removeThesePoints(TGraphErrors *g,int trig)
{
  if(trig==1){
    g->RemovePoint(0);
    g->RemovePoint(0);
    //additional for pp:
    g->RemovePoint(g->GetN()-1);
  }
  if(trig==2){
    g->RemovePoint(0);
    g->RemovePoint(0);
    g->RemovePoint(0);
    g->RemovePoint(0);
    g->RemovePoint(g->GetN()-1);
    g->RemovePoint(g->GetN()-1);
  }
  if(trig==3){
    g->RemovePoint(0);
    g->RemovePoint(0);
    g->RemovePoint(0);
    g->RemovePoint(0);
    g->RemovePoint(0);
  }
}

void getPhotonSpectrum(const PhotonAnalysisSettings &settings) {
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);

  gStyle->SetErrorX(0);

  //bool subtractNEUTRONS=kTRUE;

  //neutron data:
  //hadron data for Levy function, nucl-ex/0601033:
  //-->  d^2N/(2*pi*pT*N*dpT*dy) = B/((1+((mT - m0)/nT))^n)
  // {p-dAu; pbar-dAu; p-pp; pbar-pp} and m0 = m_neutron = 1.0 GeV.
  double B[]={0.3,0.23,0.072,0.061};//->[0]
  //double eB[]={0.01,0.01,0.005,0.005};
  double T[]={0.205,0.215,0.179,0.173};//->[1]
  //double eT[]={0.004,0.005,0.006,0.006};
  double n[]={11.00,12.55,10.87,10.49};//->[2]
  //double en[]={0.29,0.41,0.43,0.40};
  double BR=35.8/63.9;//->p + pi- (63.9%) : ->n + pi0 (35.8%)
  double c_feeddown=0.8+0.2*BR;
  double CPVloss=1;//0.98;//for minbias
  TF1 *f_nbar=new TF1("f_nbar","[3]*[4]*[0]/TMath::Power((1.+(sqrt(x*x+1.) - 1.)/([1]*[2])),[2])",0.,15.);
assert(f_nbar);
    if (settings.name == "pp05") f_nbar->SetParameters(B[3],T[3],n[3],c_feeddown,CPVloss);
    if (settings.name == "dAu")  f_nbar->SetParameters(B[1],T[1],n[1],c_feeddown,CPVloss);

  //get direct gammas pQCD:
  ifstream pQCDphotons(TString(settings.input_datapoints_dir + "/pQCD_Werner/rhic_cteq6_gamma_inv_sc1.dat").Data());
  float ppx[100];
  float ppy[100];
  Int_t iii=0;
  cout<<"pqcd photons:"<<endl;
  while(iii<28){
    if(!pQCDphotons.good()) break;
    float dummy=0.;
    pQCDphotons>>ppx[iii]>>dummy>>dummy>>ppy[iii];
    if (settings.name == "pp05") ppy[iii]*=1.e-09;//convert to mb
    if (settings.name == "dAu")  ppy[iii]*=1.e-09*2.21e3*7.5/42.0; //convert to mb * sigma_dAu * Nbin / sigma_inel    
    iii++;
  }
  TGraph *g_dirgamma=new TGraph(iii,ppx,ppy);
assert(g_dirgamma);
  //get direct gammas pQCD: scale 0.5*pT
  ifstream pQCDphotons05(TString(settings.input_datapoints_dir + "/pQCD_Werner/rhic_cteq6_gamma_inv_sc05.dat").Data());
  float ppx05[100];
  float ppy05[100];
  Int_t iii05=0;
  while(iii05<28){
    if(!pQCDphotons05.good()) break;
    float dummy=0.;
    pQCDphotons05>>ppx05[iii05]>>dummy>>dummy>>ppy05[iii05];
    if (settings.name == "pp05") ppy05[iii05]*=1.e-09;//convert to mb
    if (settings.name == "dAu")  ppy05[iii05]*=1.e-09*2.21e3*7.5/42.0; //convert to mb * sigma_dAu * Nbin / sigma_inel    
    iii05++;
  }
  TGraph *g_dirgamma05=new TGraph(iii05,ppx05,ppy05);
assert(g_dirgamma05);
  //get direct gammas pQCD: scale 2*pT
  ifstream pQCDphotons2(TString(settings.input_datapoints_dir + "/pQCD_Werner/rhic_cteq6_gamma_inv_sc2.dat").Data());
  float ppx2[100];
  float ppy2[100];
  Int_t iii2=0;
  while(iii2<28){
    if(!pQCDphotons2.good()) break;
    float dummy=0.;
    pQCDphotons2>>ppx2[iii2]>>dummy>>dummy>>ppy2[iii2];
    if (settings.name == "pp05") ppy2[iii2]*=1.e-09;//convert to mb
    if (settings.name == "dAu")  ppy2[iii2]*=1.e-09*2.21e3*7.5/42.0; //convert to mb * sigma_dAu * Nbin / sigma_inel    
    iii2++;
  }
  TGraph *g_dirgamma2=new TGraph(iii2,ppx2,ppy2);
assert(g_dirgamma2);
  //get phenix pions in pp or dAu
    TString phenixFile;
    if (settings.name == "pp05") phenixFile = settings.input_datapoints_dir + "/phenix_xsec_pp.dat";
    if (settings.name == "dAu") phenixFile = settings.input_datapoints_dir + "/phenix.dat";
  ifstream phenix(phenixFile.Data());
  float phex[100];
  float phey[100];
  float ephey[100];
  Int_t iphe=0;
  while(iphe<16){
    if(!phenix.good()) break;
    if (settings.name == "pp05") phenix>>phex[iphe]>>phey[iphe];
    if (settings.name == "dAu") phenix>>phex[iphe]>>phey[iphe]>>ephey[iphe];
    //is in mb already!!
    iphe++;
  }
  TGraphErrors *phenix_pp=new TGraphErrors(iphe,phex,phey);
assert(phenix_pp);
  phenix_pp->SetMarkerStyle(24);
  phenix_pp->SetLineWidth(6);
  phenix_pp->SetName("phenix");

  //frank's spin2006 pions:
  ifstream frank(TString(settings.input_datapoints_dir + "/frank_pp05_new.dat").Data());
  float frx[100];
  float fry[100];
  float frex[100];
  float frey[100];
  Int_t ifr=0;
  while(ifr<12){
    if(!frank.good()) break;
    frank>>frx[ifr]>>fry[ifr]>>frey[ifr];
    frex[ifr]=0.;
    ifr++;
  }
  TGraphErrors *frank_pp=new TGraphErrors(ifr,frx,fry,frex,frey);
assert(frank_pp);
  frank_pp->SetMarkerStyle(25);
  frank_pp->SetMarkerColor(4);
  frank_pp->SetName("frank_pp");

  //sasha's pions:
  float x_pp2005_sasha[]={1.20358,1.70592,2.21238,2.71916,3.4032,4.4224,5.43571,6.44468,7.45056,8.83794,10.8659,12.8831,15.2692 };
  float xe_pp2005_sasha[]={0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.};
  float y_pp2005_sasha[]={0.212971,0.0337464,0.00811345,0.00248195,0.000467495,6.37683e-05,1.4487e-05,3.67539e-06,1.26723e-06,3.3676e-07,8.01941e-08,1.93813e-08,5.56059e-09};
  float ye_pp2005_sasha[]={0.00463771,0.000949529,0.000323778,0.000139023,3.68181e-05,7.61527e-07,2.28359e-07,9.19064e-08,4.93116e-08,8.16844e-09,3.66273e-09,1.75057e-09,7.36722e-10};

  TGraphErrors *sasha_pp05=new TGraphErrors(13,x_pp2005_sasha,y_pp2005_sasha,xe_pp2005_sasha,ye_pp2005_sasha);
assert(sasha_pp05);
  sasha_pp05->SetMarkerStyle(8);
  sasha_pp05->SetMarkerColor(TColor::GetColor(8,80,8));
  sasha_pp05->SetName("sasha_pp05");

  //get pions KKP  scale pT
  ifstream pQCDpions(TString(settings.input_datapoints_dir + "/pQCD_Werner/klaus_pi0inv_200_kkp_1.dat").Data());
  float pionx[100];
  float piony[100];
  Int_t ipion=0;
  while(ipion<28){
    if(!pQCDpions.good()) break;
    pQCDpions>>pionx[ipion]>>piony[ipion];
    if (settings.name == "pp05") piony[iii]*=1.e-09;//convert to mb
    if (settings.name == "dAu")  piony[iii]*=1.e-09*2.21e3*7.5/42.0; //convert to mb * sigma_dAu * Nbin / sigma_inel    
    ipion++;
  }
  TGraphErrors *kkp=new TGraphErrors(ipion,pionx,piony);
assert(kkp);
  kkp->SetLineColor(54);
  kkp->SetName("kkp");

  //get pions KKP  scale 0.5*pT
  ifstream pQCDpions05(TString(settings.input_datapoints_dir + "/pQCD_Werner/klaus_pi0inv_200_kkp_05.dat").Data());
  float pionx05[100];
  float piony05[100];
  Int_t ipion05=0;
  while(ipion05<28){
    if(!pQCDpions05.good()) break;
    pQCDpions05>>pionx05[ipion05]>>piony05[ipion05];
    if (settings.name == "pp05") piony05[iii05]*=1.e-09;//convert to mb
    if (settings.name == "dAu")  piony05[iii05]*=1.e-09*2.21e3*7.5/42.0; //convert to mb * sigma_dAu * Nbin / sigma_inel    
    ipion05++;
  }
  TGraphErrors *kkp05=new TGraphErrors(ipion05,pionx05,piony05);
assert(kkp05);
  kkp05->SetLineStyle(2);
  kkp05->SetLineColor(54);
  kkp05->SetName("kkp05");

  //get pions KKP  scale 2*pT
  ifstream pQCDpions2(TString(settings.input_datapoints_dir + "/pQCD_Werner/klaus_pi0inv_200_kkp_2.dat").Data());
  float pionx2[100];
  float piony2[100];
  Int_t ipion2=0;
  while(ipion2<28){
    if(!pQCDpions2.good()) break;
    pQCDpions2>>pionx2[ipion2]>>piony2[ipion2];
    if (settings.name == "pp05") piony2[iii2]*=1.e-09;//convert to mb
    if (settings.name == "dAu")  piony2[iii2]*=1.e-09*2.21e3*7.5/42.0; //convert to mb * sigma_dAu * Nbin / sigma_inel    
    ipion2++;
  }
  TGraphErrors *kkp2=new TGraphErrors(ipion2,pionx2,piony2);
assert(kkp2);
  kkp2->SetLineStyle(2);
  kkp2->SetLineColor(54);
  kkp2->SetName("kkp2");

  TFile f_decaybg(settings.input_decaybackground_file,"READ");
  const TH1F *h_decaybg_read=(const TH1F*)f_decaybg.Get("gamma");
  const TH1F *h_decaypion_read=(const TH1F*)f_decaybg.Get("gamma_pion");
assert(h_decaybg_read);
assert(h_decaypion_read);
    TH1F *h_decaybg = new TH1F(*h_decaybg_read);
    TH1F *h_decaypion = new TH1F(*h_decaypion_read);
assert(h_decaybg);
assert(h_decaypion);
    f_decaybg.Close();
    h_decaybg_read = 0;
    h_decaypion_read = 0;

  TF1 *fit_decay=new TF1("fit_decay","[0]/TMath::Power(x,[1])+[2]",.3,15.);
  TF1 *fit_piondecay=new TF1(*fit_decay);
assert(fit_decay);
assert(fit_piondecay);
  fit_decay->SetParameters(1.,1.,.5);
  fit_piondecay->SetParameters(1.,1.,.5);
  h_decaybg->Fit(fit_decay,"R0Q");
  h_decaypion->Fit(fit_piondecay,"R0Q");

  //take ratio gamma_direct/pion and divide by gamma/pi and then +1:
  for(Int_t i=0;i<iii;i++){
    ppy[i]=ppy[i]/piony[i];
    ppy[i]=ppy[i]/fit_decay->Eval(ppx[i]);
    ppy[i]+=1.;
    ppy05[i]=ppy05[i]/piony05[i];
    ppy05[i]=ppy05[i]/fit_decay->Eval(ppx05[i]);
    ppy05[i]+=1.;
    ppy2[i]=ppy2[i]/piony2[i];
    ppy2[i]=ppy2[i]/fit_decay->Eval(ppx2[i]);
    ppy2[i]+=1.;
  }
  TGraphErrors *g_photonpqcd=new TGraphErrors(iii,ppx,ppy);
assert(g_photonpqcd);
  g_photonpqcd->SetName("g_photonpqcd");
  TGraphErrors *g_photonpqcd05=new TGraphErrors(iii05,ppx05,ppy05);
assert(g_photonpqcd05);
  g_photonpqcd05->SetName("g_photonpqcd05");
  TGraphErrors *g_photonpqcd2=new TGraphErrors(iii2,ppx2,ppy2);
assert(g_photonpqcd2);
  g_photonpqcd2->SetName("g_photonpqcd2");

  AnaCuts *cuts=new AnaCuts(settings.name.Data());
assert(cuts);

  //get inv mass hists
  TFile f(settings.input_pion_file.Data(),"READ");
    const TH2F *h_minvMB = (const TH2F*)f.Get("h_minvMB");
    const TH2F *h_minvHT1 = (const TH2F*)f.Get("h_minvHT1");
    const TH2F *h_minvHT2 = (const TH2F*)f.Get("h_minvHT2");
    const TH1F *h_events = (const TH1F*)f.Get("h_events");
assert(h_minvMB);
assert(h_minvHT1);
assert(h_minvHT2);
assert(h_events);
  TH2F *h_mb=new TH2F(*h_minvMB);
  TH2F *h_ht1=new TH2F(*h_minvHT1);
  TH2F *h_ht2=new TH2F(*h_minvHT2);
  TH1F *h_ev=new TH1F(*h_events);
assert(h_mb);
assert(h_ht1);
assert(h_ht2);
assert(h_ev);
    f.Close();
    h_minvMB = 0;
    h_minvHT1 = 0;
    h_minvHT2 = 0;
  //get anti-neutron
  TFile file_nbar(settings.input_nbareff_file.Data(),"READ");
    const TH1F *h_effnbarMB = (const TH1F*)file_nbar.Get("h_effMB");
    const TH1F *h_effnbarHT1 = (const TH1F*)file_nbar.Get("h_effHT1");
    const TH1F *h_effnbarHT2 = (const TH1F*)file_nbar.Get("h_effHT2");
assert(h_effnbarMB);
assert(h_effnbarHT1);
assert(h_effnbarHT2);
  TH1F *h_nbarEffMB=new TH1F(*h_effnbarMB);
  TH1F *h_nbarEffHT1=new TH1F(*h_effnbarHT1);
  TH1F *h_nbarEffHT2=new TH1F(*h_effnbarHT2);
assert(h_nbarEffMB);
assert(h_nbarEffHT1);
assert(h_nbarEffHT2);
  h_nbarEffMB->Sumw2();
  h_nbarEffHT1->Sumw2();
  h_nbarEffHT2->Sumw2();
    file_nbar.Close();
    h_effnbarMB = 0;
    h_effnbarHT1 = 0;
    h_effnbarHT2 = 0;

  //get prescales
  int trigger=0;
  Float_t numberOfMB=0;
  Float_t numberOfHT1=0;
  Float_t numberOfHT2=0;
  for(Int_t i=1;i<=h_ev->GetNbinsX();i++)
    {
      trigger=(Int_t)h_ev->GetBinCenter(i);
      if(trigger&1) numberOfMB+=(Int_t)h_ev->GetBinContent(i);
      if(trigger&2) numberOfHT1+=(Int_t)h_ev->GetBinContent(i);
      if(trigger&4) numberOfHT2+=(Int_t)h_ev->GetBinContent(i);
    }

  cout<<"number of mb: "<<numberOfMB<<endl;
    if (settings.name == "dAu") {
	numberOfMB/=0.93;
	cout<<"nmb after 63% vertex eff.: "<<numberOfMB<<endl;
    }

  Float_t psMB=1;
  Float_t psHT1=1;
  Float_t psHT2=1;

    if (settings.name == "pp05") {
        psMB=32742;
	psHT1=3.89;
	psHT2=1.;
    }
    if (settings.name == "dAu") {
	psMB=383.;
	psHT1=9.65;
	psHT2=1.;
    }

  //mb events for hightower:
  float nMBwithHT=1;
    if (settings.name == "pp05") {
	nMBwithHT = 328871;
    }
    if (settings.name == "dAu") {
	nMBwithHT = numberOfMB;
    }
  //get efficiencies+acceptance
  TFile g(settings.input_pioneff_file.Data(),"READ");
    const TH1F *h_effpionMB = (const TH1F*)g.Get("h_effMB");
    const TH1F *h_effpionHT1 = (const TH1F*)g.Get("h_effHT1");
    const TH1F *h_effpionHT2 = (const TH1F*)g.Get("h_effHT2");
assert(h_effpionMB);
assert(h_effpionHT1);
assert(h_effpionHT2);
  TH1F *h_emb=new TH1F(*h_effpionMB);
  TH1F *h_eht1=new TH1F(*h_effpionHT1);
  TH1F *h_eht2=new TH1F(*h_effpionHT2);
assert(h_emb);
assert(h_eht1);
assert(h_eht2);
    g.Close();
    h_effpionMB = 0;
    h_effpionHT1 = 0;
    h_effpionHT2 = 0;

  //bin corrections
  TFile binf(settings.input_binwidth_file,"READ"); 
    const TH1F *h4mb = (const TH1F*)binf.Get("h4mb");
    const TH1F *h4ht1 = (const TH1F*)binf.Get("h4ht1");
    const TH1F *h4ht2 = (const TH1F*)binf.Get("h4ht2");
assert(h4mb);
assert(h4ht1);
assert(h4ht2);
  TH1F *h_binmb=new TH1F(*h4mb);
  TH1F *h_binht1=new TH1F(*h4ht1);
  TH1F *h_binht2=new TH1F(*h4ht2);
assert(h_binmb);
assert(h_binht1);
assert(h_binht2);
    binf.Close();
    h4mb = 0;
    h4ht1 = 0;
    h4ht2 = 0;

  h_binmb->Sumw2();
  h_binht1->Sumw2();
  h_binht2->Sumw2();
  for(Int_t i=1;i<=h_binmb->GetNbinsX();i++) h_binmb->SetBinError(i,0);
  for(Int_t i=1;i<=h_binht1->GetNbinsX();i++) h_binht1->SetBinError(i,0);
  for(Int_t i=1;i<=h_binht2->GetNbinsX();i++) h_binht2->SetBinError(i,0);

  //corrections, all multiplicative, in case of
  //pions:
  TF1 *pion_cpv_corrMB=0;
  TF1 *pion_cpv_corrHT1=0;
  TF1 *pion_cpv_corrHT2=0;
  //photons:
  TF1 *gamma_cpv_corrMB=0;
  TF1 *gamma_cpv_corrHT1=0;
  TF1 *gamma_cpv_corrHT2=0;
  TF1 *gamma_cont_corrMB=0;
  TF1 *gamma_cont_corrHT1=0;
  TF1 *gamma_cont_corrHT2=0;
  //missing material
  //pions:
  TF1 *pion_conv_corrMB=0;
  TF1 *pion_conv_corrHT1=0;
  TF1 *pion_conv_corrHT2=0;
  //photons:
  TF1 *gamma_conv_corrMB=0;
  TF1 *gamma_conv_corrHT1=0;
  TF1 *gamma_conv_corrHT2=0;
    if (settings.name == "pp05") {
	pion_cpv_corrMB = new TF1("pion_cpv_corrMB","1./(1.-0.01*(0.3+0.0*x))",0.,15.);
        pion_cpv_corrHT1 = new TF1("pion_cpv_corrHT1","1./(1.-0.01*(-0.1+0.16*x))",0.,15.);
        pion_cpv_corrHT2 = new TF1("pion_cpv_corrHT2","1./(1.-0.01*(-0.2+0.18*x))",0.,15.);
        gamma_cpv_corrMB = new TF1("gamma_cpv_corrMB","1./(1.-0.01*(2.8+0.0*x))",0.,15.);
        gamma_cpv_corrHT1 = new TF1("gamma_cpv_corrHT1","1./(1.-0.01*(0.2+1.1*x))",0.,15.);
        gamma_cpv_corrHT2 = new TF1("gamma_cpv_corrHT2","1./(1.-0.01*(0.4+1.1*x))",0.,15.);
        gamma_cont_corrMB = new TF1("gamma_cont_corrMB","0.985",0.,15.);
        gamma_cont_corrHT1 = new TF1("gamma_cont_corrHT1","0.98",0.,15.);
        gamma_cont_corrHT2 = new TF1("gamma_cont_corrHT2","0.96",0.,15.);
        pion_conv_corrMB = new TF1("pion_conv_corrMB","1.1",0.,15.);
        pion_conv_corrHT1 = new TF1("pion_conv_corrHT1","1.1",0.,15.);
        pion_conv_corrHT2 = new TF1("pion_conv_corrHT2","1.1",0.,15.);
        gamma_conv_corrMB = new TF1("gamma_conv_corrMB","1.05",0.,15.);
        gamma_conv_corrHT1 = new TF1("gamma_conv_corrHT1","1.05",0.,15.);
        gamma_conv_corrHT2 = new TF1("gamma_conv_corrHT2","1.05",0.,15.);
    }
    if (settings.name == "dAu") {
	pion_cpv_corrMB = new TF1("pion_cpv_corrMB","1./(1.-0.01*(0.4+0.05*x))",0.,15.);
        pion_cpv_corrHT1 = new TF1("pion_cpv_corrHT1","1./(1.-0.01*(0.4+0.07*x))",0.,15.);
        pion_cpv_corrHT2 = new TF1("pion_cpv_corrHT2","1./(1.-0.01*(0.5+0.06*x))",0.,15.);
        gamma_cpv_corrMB = new TF1("gamma_cpv_corrMB","1./(1.-0.01*(4.1+0.4*x))",0.,15.);
        gamma_cpv_corrHT1 = new TF1("gamma_cpv_corrHT1","1./(1.-0.01*(3.4+0.5*x))",0.,15.);
        gamma_cpv_corrHT2 = new TF1("gamma_cpv_corrHT2","1./(1.-0.01*(4.9+0.3*x))",0.,15.);
        gamma_cont_corrMB = new TF1("gamma_cont_corrMB","0.98",0.,15.);
        gamma_cont_corrHT1 = new TF1("gamma_cont_corrHT1","0.98",0.,15.);
        gamma_cont_corrHT2 = new TF1("gamma_cont_corrHT2","0.965",0.,15.);
        pion_conv_corrMB = new TF1("pion_conv_corrMB","1.15",0.,15.);
        pion_conv_corrHT1 = new TF1("pion_conv_corrHT1","1.15",0.,15.);
        pion_conv_corrHT2 = new TF1("pion_conv_corrHT2","1.15",0.,15.);
        gamma_conv_corrMB = new TF1("gamma_conv_corrMB","1.08",0.,15.);
        gamma_conv_corrHT1 = new TF1("gamma_conv_corrHT1","1.08",0.,15.);
        gamma_conv_corrHT2 = new TF1("gamma_conv_corrHT2","1.08",0.,15.);
    }
assert(pion_cpv_corrMB);
assert(pion_cpv_corrHT1);
assert(pion_cpv_corrHT2);
assert(gamma_cpv_corrMB);
assert(gamma_cpv_corrHT1);
assert(gamma_cpv_corrHT2);
assert(gamma_cont_corrMB);
assert(gamma_cont_corrHT1);
assert(gamma_cont_corrHT2);
assert(pion_conv_corrMB);
assert(pion_conv_corrHT1);
assert(pion_conv_corrHT2);
assert(gamma_conv_corrMB);
assert(gamma_conv_corrHT1);
assert(gamma_conv_corrHT2);

  //get yield
  Pi0Analysis *pi0=new Pi0Analysis(settings.output_invmassplots_file.Data(),settings.output_invmassplotseta_file.Data(),settings.name.Data());
assert(pi0);
  pi0->init(settings.output_pionhistograms_file.Data());
  TH1F *pionYieldMB=new TH1F(*pi0->getYield(h_mb,"mb"));
  TH1F *pionYieldHT1=new TH1F(*pi0->getYield(h_ht1,"ht1"));
  TH1F *pionYieldHT2=new TH1F(*pi0->getYield(h_ht2,"ht2"));
assert(pionYieldMB);
assert(pionYieldHT1);
assert(pionYieldHT2);
  pi0->storeCanvases(settings.output_pioncanvases_file.Data());


  cout<<"***************************************"<<endl;
  cout<<"got yield, dividing by rapidity bite!!!"<<endl;
  float dy_gamma=cuts->rapidityMaxCUT - cuts->rapidityMinCUT;
  float dy_pion=cuts->rapPionMaxCUT - cuts->rapPionMinCUT;
  cout<<"***************************************"<<endl;
  cout<<endl;
  cout<<"    pion bite is "<<dy_pion<<endl;
  cout<<"    gamma bite is "<<dy_gamma<<endl;
  cout<<endl;
  cout<<"***************************************"<<endl;

  pionYieldMB->Scale(1./dy_pion);
  pionYieldHT1->Scale(1./dy_pion);
  pionYieldHT2->Scale(1./dy_pion);

  //set yield to zero
  /*
  for(Int_t i=0;i<pionYieldHT2->GetNbinsX();i++)
    {
      if(i<1)
	{
	  pionYieldMB->SetBinContent(i+1,0);
	  pionYieldMB->SetBinError(i+1,0);
	}
      if(i<4)
        {
          pionYieldHT1->SetBinContent(i+1,0);
          pionYieldHT1->SetBinError(i+1,0);
	}
      if(i>12)
	{
	  pionYieldHT1->SetBinContent(i+1,0);
          pionYieldHT1->SetBinError(i+1,0);
	}
      if(i<6) 
	{
	  pionYieldHT2->SetBinContent(i+1,0);
          pionYieldHT2->SetBinError(i+1,0);
	}
    }
  */

  //set colors:
  pionYieldMB->SetMarkerStyle(8);
  pionYieldMB->SetMarkerSize(1.0);
  pionYieldHT1->SetMarkerStyle(8);
  pionYieldHT1->SetMarkerSize(1.0); 
  pionYieldHT1->SetMarkerColor(4);
  pionYieldHT2->SetMarkerStyle(8);
  pionYieldHT2->SetMarkerSize(1.0);
  pionYieldHT2->SetMarkerColor(2);
  
  TF1 *scale=new TF1("scale","x",0.,15.);
assert(scale);

  pionYieldMB->SetNameTitle("pionYieldMB","corrected yield MB");
  pionYieldMB->Divide(h_emb);
  pionYieldMB->Scale(psMB/(psMB*numberOfMB*2.*TMath::Pi()));
  pionYieldMB->Divide(scale);
  pionYieldMB->Multiply(h_binmb);
  pionYieldMB->Multiply(pion_cpv_corrMB);
  pionYieldMB->Multiply(pion_conv_corrMB);

  pionYieldHT1->SetNameTitle("pionYieldHT1","corrected yield HT1");
  pionYieldHT1->Divide(h_eht1);
  pionYieldHT1->Scale(psHT1/(psMB*nMBwithHT*2.*TMath::Pi()));
  pionYieldHT1->Divide(scale);
  pionYieldHT1->Multiply(h_binht1);
  pionYieldHT1->Multiply(pion_cpv_corrHT1);
  pionYieldHT1->Multiply(pion_conv_corrHT1);

  pionYieldHT2->SetNameTitle("pionYieldHT2","corrected yield HT2");
  pionYieldHT2->Divide(h_eht2);
  pionYieldHT2->Scale(psHT2/(psMB*nMBwithHT*2.*TMath::Pi()));
  pionYieldHT2->Divide(scale);
  pionYieldHT2->Multiply(h_binht2);
  pionYieldHT2->Multiply(pion_cpv_corrHT2);
  pionYieldHT2->Multiply(pion_conv_corrHT2);

  //create pion yield for double ratio:
  TH1F *pionYieldMBratio=new TH1F(*pionYieldMB);
  TH1F *pionYieldHT1ratio=new TH1F(*pionYieldHT1);
  TH1F *pionYieldHT2ratio=new TH1F(*pionYieldHT2);
assert(pionYieldMBratio);  
assert(pionYieldHT1ratio);  
assert(pionYieldHT2ratio);  

  TH1F *pionXsMB=new TH1F(*pionYieldMB);
  TH1F *pionXsHT1=new TH1F(*pionYieldHT1);
  TH1F *pionXsHT2=new TH1F(*pionYieldHT2);
assert(pionXsMB);
assert(pionXsHT1);
assert(pionXsHT2);
    if (settings.name=="pp05") {
	pionXsMB->Scale((26.1/0.85));//times xs_bbc/bbc_eff
	pionXsHT1->Scale((26.1/0.85));
	pionXsHT2->Scale((26.1/0.85));
    }
    if (settings.name=="dAu") {
	pionXsMB->Scale(2.21e3);//times cross section
	pionXsHT1->Scale(2.21e3);
	pionXsHT2->Scale(2.21e3);
    }


  TH1F *pionXsMBnoErr=new TH1F(*pionXsMB);
  TH1F *pionXsHT1noErr=new TH1F(*pionXsHT1);
  TH1F *pionXsHT2noErr=new TH1F(*pionXsHT2);
assert(pionXsMBnoErr);
assert(pionXsHT1noErr);
assert(pionXsHT2noErr);
  for(int i=1;i<=pionXsMBnoErr->GetNbinsX();i++){
    pionXsMBnoErr->SetBinError(i,0.);
  }
  for(int i=1;i<=pionXsHT1noErr->GetNbinsX();i++){
    pionXsHT1noErr->SetBinError(i,0.);
  }
  for(int i=1;i<=pionXsHT2noErr->GetNbinsX();i++){
    pionXsHT2noErr->SetBinError(i,0.);
  }

  TGraphErrors *g_pionXsMB=new TGraphErrors(pionXsMB);
assert(g_pionXsMB);
  g_pionXsMB->SetName("g_pionXsMB");
  removeThesePoints(g_pionXsMB,1);

  TGraphErrors *g_pionXsHT1=new TGraphErrors(pionXsHT1);
assert(g_pionXsHT1);
  g_pionXsHT1->SetName("g_pionXsHT1");
  removeThesePoints(g_pionXsHT1,2);

  TGraphErrors *g_pionXsHT2=new TGraphErrors(pionXsHT2);
assert(g_pionXsHT2);
  g_pionXsHT2->SetName("g_pionXsHT2");
  removeThesePoints(g_pionXsHT2,3);

  if(1){
    cout<<endl<<"xsec: x  y  ex  ey"<<endl;
    cout<<"minbias"<<endl;
    printPoints(g_pionXsMB);
    cout<<endl<<"hightower-1"<<endl;
    printPoints(g_pionXsHT1);
    cout<<endl<<"hightower-2"<<endl;
    printPoints(g_pionXsHT2);
    cout<<endl;
  }
  

  TMultiGraph *m_pions_fit=new TMultiGraph();
assert(m_pions_fit);
  m_pions_fit->SetName("m_pions_fit");
  m_pions_fit->SetMinimum(5.0e-9);
  m_pions_fit->SetMaximum(0.99);


  m_pions_fit->Add(g_pionXsMB);
  m_pions_fit->Add(g_pionXsHT1);
  m_pions_fit->Add(g_pionXsHT2);

  TF1 *fitQCD=new TF1("fitQCD",sumpqcd,1.,15.,6);
assert(fitQCD);
    if (settings.name=="pp05") fitQCD->SetParameters(600.,-8.2,4.,-8.5,2.,2.);
    if (settings.name=="dAu")  fitQCD->SetParameters(100.,-9.,1.,-8.5,1.,6.);
  fitQCD->FixParameter(4,2.);
cout << "111" << endl;
//  m_pions_fit->Fit(fitQCD,"RV");
cout << "222" << endl;
  
  bool inclPhenix=true;
  bool inclFrank=true;
  bool inclSasha=true;
  bool inclPqcd=true;

  TCanvas *compare=new TCanvas("compare","compare;p_{T}:xsec (mb)",600,750);
assert(compare);
  compare->Divide(1, 2, 0, 0);

  compare->cd(1);
  gPad->SetLogy();

  TMultiGraph *m_pions=new TMultiGraph();
assert(m_pions);
  m_pions->SetName("m_pions");

  if(inclPqcd){
    m_pions->Add(kkp,"c");
    m_pions->Add(kkp05,"c");
    m_pions->Add(kkp2,"c");
  }
  if(inclSasha){
    m_pions->Add(sasha_pp05);
  }
  if(inclFrank){
    m_pions->Add(frank_pp);
  }
  if(inclPhenix){
    m_pions->Add(phenix_pp);
  }

  //g_pionXsMB->Print();
  //cout<<endl<<endl;
  //g_pionXsHT1->Print();
  //cout<<endl<<endl;
  //g_pionXsHT2->Print();
  //cout<<endl;

  m_pions->Add(g_pionXsMB);
  m_pions->Add(g_pionXsHT1);
  m_pions->Add(g_pionXsHT2);

  m_pions->SetMinimum(1.0e-9);
  m_pions->SetMaximum(1.);

  m_pions->Draw("ap");

  //fitQCD->Draw("same");

  m_pions->GetXaxis()->SetLabelSize(0.);
  m_pions->GetXaxis()->SetTitle("p_{T} [GeV/c]");
  m_pions->GetYaxis()->SetTitle("Ed^{3}#sigma/dp^{3} [mb GeV^{-2} c^{3}]");

  TLegend *leg=new TLegend(.5,.5,.85,.85);
assert(leg);

  if(inclPhenix) leg->AddEntry(phenix_pp,"PHENIX p+p","p");
  if(inclFrank) leg->AddEntry(frank_pp,"STAR preliminary (upd.)","p");
  if(inclSasha) leg->AddEntry(sasha_pp05,"O.Grebenyuk p+p","p");
  leg->AddEntry(g_pionXsMB,"p+p minimum bias","p");
  leg->AddEntry(g_pionXsHT1,"hightower 1","p");
  leg->AddEntry(g_pionXsHT2,"hightower 2","p");
  if(inclPqcd){
    leg->AddEntry(kkp,"kkp + CTEQ6m, #mu=p_{T}","l");
    leg->AddEntry(kkp2,"#mu=2p_{T},p_{T}/2","l");
    leg->Draw("same");
  }

  leg->SetFillColor(0);
  leg->Draw();

/*
  compare->cd();
  padb->Draw();
  padb->cd();
*/
    compare->cd(2);

  TGraphErrors *sasha_pp05_overPqcd=new TGraphErrors(*sasha_pp05);
assert(sasha_pp05_overPqcd);
  divideGraphWithGraph(sasha_pp05_overPqcd,kkp);
  TGraphErrors *phenix_pp05_overPqcd=new TGraphErrors(*phenix_pp);
assert(phenix_pp05_overPqcd);
  divideGraphWithGraph(phenix_pp05_overPqcd,kkp);
  TGraphErrors *g_pionXsMB_overPqcd=new TGraphErrors(*g_pionXsMB);
assert(g_pionXsMB_overPqcd);
  divideGraphWithGraph(g_pionXsMB_overPqcd,kkp);
  TGraphErrors *g_pionXsHT1_overPqcd=new TGraphErrors(*g_pionXsHT1);
assert(g_pionXsHT1_overPqcd);
  divideGraphWithGraph(g_pionXsHT1_overPqcd,kkp);
  TGraphErrors *g_pionXsHT2_overPqcd=new TGraphErrors(*g_pionXsHT2);
assert(g_pionXsHT2_overPqcd);
  divideGraphWithGraph(g_pionXsHT2_overPqcd,kkp);
  TGraphErrors *frank_pp05_overPqcd=new TGraphErrors(*frank_pp);
assert(frank_pp05_overPqcd);
  divideGraphWithGraph(frank_pp05_overPqcd,kkp);

  TGraphErrors *kkp05_ratio=new TGraphErrors(*kkp05);
assert(kkp05_ratio);
  divideGraphWithGraph(kkp05_ratio,kkp);
  TGraphErrors *kkp_ratio=new TGraphErrors(*kkp);
assert(kkp_ratio);
  divideGraphWithGraph(kkp_ratio,kkp);
  TGraphErrors *kkp2_ratio=new TGraphErrors(*kkp2);
assert(kkp2_ratio);
  divideGraphWithGraph(kkp2_ratio,kkp);

  //systematic errors:
  TGraphErrors *g_pionXsMB_sys=new TGraphErrors(*g_pionXsMB_overPqcd);
assert(g_pionXsMB_sys);
  set_sys_pp_pion(g_pionXsMB_sys);
  TGraphErrors *g_pionXsHT1_sys=new TGraphErrors(*g_pionXsHT1_overPqcd);
assert(g_pionXsHT1_sys);
  set_sys_pp_pion(g_pionXsHT1_sys);
  TGraphErrors *g_pionXsHT2_sys=new TGraphErrors(*g_pionXsHT2_overPqcd);
assert(g_pionXsHT2_sys);
  set_sys_pp_pion(g_pionXsHT2_sys);
  
  TMultiGraph *m_pions_over_pqcd=new TMultiGraph();
assert(m_pions_over_pqcd);

  m_pions_over_pqcd->SetMinimum(0.0);
  m_pions_over_pqcd->SetMaximum(2.5);

  if(inclPqcd){
    m_pions_over_pqcd->Add(kkp05_ratio,"c");
    m_pions_over_pqcd->Add(kkp_ratio,"c");
    m_pions_over_pqcd->Add(kkp2_ratio,"c");
  }
  m_pions_over_pqcd->Add(g_pionXsMB_overPqcd);
  m_pions_over_pqcd->Add(g_pionXsHT1_overPqcd);
  m_pions_over_pqcd->Add(g_pionXsHT2_overPqcd);
  //m_pions_over_pqcd->Add(g_pionXsMB_sys,"c");
  //m_pions_over_pqcd->Add(g_pionXsHT1_sys,"c");
  //m_pions_over_pqcd->Add(g_pionXsHT2_sys,"c");
  if(inclPhenix) m_pions_over_pqcd->Add(phenix_pp05_overPqcd);
  if(inclFrank) m_pions_over_pqcd->Add(frank_pp05_overPqcd);
  if(inclSasha) m_pions_over_pqcd->Add(sasha_pp05_overPqcd);

  m_pions_over_pqcd->Draw("ap");
  m_pions_over_pqcd->GetXaxis()->SetTitle("p_{T} [GeV/c]");
  m_pions_over_pqcd->GetYaxis()->SetTitle("Data / theory");

  compare->SaveAs(settings.output_pionxsec_file.Data());

  TMultiGraph *m_pions_over_fit=new TMultiGraph();
assert(m_pions_over_fit);
  m_pions_over_fit->SetMinimum(0.01);
  m_pions_over_fit->SetMaximum(1.99);

  TGraphErrors *g_pionXsMB_overFit=new TGraphErrors(*g_pionXsMB);
assert(g_pionXsMB_overFit);
  divideGraphWithFunction(g_pionXsMB_overFit,fitQCD);
  TGraphErrors *g_pionXsHT1_overFit=new TGraphErrors(*g_pionXsHT1);
assert(g_pionXsHT1_overFit);
  divideGraphWithFunction(g_pionXsHT1_overFit,fitQCD);
  TGraphErrors *g_pionXsHT2_overFit=new TGraphErrors(*g_pionXsHT2);
assert(g_pionXsHT2_overFit);
  divideGraphWithFunction(g_pionXsHT2_overFit,fitQCD);

  m_pions_over_fit->Add(g_pionXsMB_overFit);
  m_pions_over_fit->Add(g_pionXsHT1_overFit);
  m_pions_over_fit->Add(g_pionXsHT2_overFit);

  m_pions_over_fit->Draw("ap");

  compare->SaveAs(settings.output_pionxsecoverfit_file.Data());



  TCanvas *compare2=new TCanvas("compare2","compare2;p_{T};yield divided by fit",600,300);
assert(compare2);
  compare2->cd();

  //divide by fit:
  TGraphErrors *frank_pp2=new TGraphErrors(*frank_pp);  
assert(frank_pp2);
  divideGraphWithFunction(frank_pp2,fitQCD);
  TGraphErrors *sasha_pp2=new TGraphErrors(*sasha_pp05);
assert(sasha_pp2);
  divideGraphWithFunction(sasha_pp2,fitQCD);
  TGraphErrors *phenix_pp2=new TGraphErrors(*phenix_pp);
assert(phenix_pp2);
  divideGraphWithFunction(phenix_pp2,fitQCD);
  TGraphErrors *g_pionXsMBcopy=new TGraphErrors(*g_pionXsMB);
assert(g_pionXsMBcopy);
  divideGraphWithFunction(g_pionXsMBcopy,fitQCD);
  TGraphErrors *g_pionXsHT1copy=new TGraphErrors(*g_pionXsHT1);
assert(g_pionXsHT1copy);
  divideGraphWithFunction(g_pionXsHT1copy,fitQCD);
  TGraphErrors *g_pionXsHT2copy=new TGraphErrors(*g_pionXsHT2);
assert(g_pionXsHT2copy);
  divideGraphWithFunction(g_pionXsHT2copy,fitQCD);

  inclPhenix=true;
  inclFrank=false;
  inclSasha=false;
  inclPqcd=false;

  TMultiGraph *m_pions2=new TMultiGraph();
assert(m_pions2);
  m_pions2->SetName("m_pions2");
  if(inclSasha) m_pions2->Add(sasha_pp2);
  if(inclFrank) m_pions2->Add(frank_pp2);
  if(inclPhenix) m_pions2->Add(phenix_pp2);
  
  m_pions2->Add(g_pionXsMBcopy);
  m_pions2->Add(g_pionXsHT1copy);
  m_pions2->Add(g_pionXsHT2copy);

  m_pions2->SetMinimum(0.000001);
  m_pions2->SetMaximum(3.);
  m_pions2->Draw("ap");

  TLegend *legg=new TLegend(.25,.55,.65,.85);
assert(legg);
  legg->AddEntry(g_pionXsMBcopy,"minimum bias","p");
  legg->AddEntry(g_pionXsHT1copy,"hightower 1","p");
  legg->AddEntry(g_pionXsHT2copy,"hightower 2","p");
  if(inclFrank) legg->AddEntry(frank_pp2,"Frank's p+p (upd.)","p");
  if(inclSasha) legg->AddEntry(sasha_pp2,"Sasha's p+p","p");
  if(inclPhenix) legg->AddEntry(phenix_pp,"PHENIX p+p","p");
  legg->Draw("same");
  legg->SetFillColor(0);

  compare2->cd(0);
  compare2->SaveAs(settings.output_pionxsecratio_file.Data());


  //********************************************
  //   Get double ratio:
  //********************************************

  //pion decay photon eff:
  TFile gg(settings.input_pioneff_file.Data(),"READ");  
    const TH1F *h_effDaughtersMB = (const TH1F*)gg.Get("h_effDaughtersMB");
    const TH1F *h_effDaughtersHT1 = (const TH1F*)gg.Get("h_effDaughtersHT1");
    const TH1F *h_effDaughtersHT2 = (const TH1F*)gg.Get("h_effDaughtersHT2");
assert(h_effDaughtersMB);
assert(h_effDaughtersHT1);
assert(h_effDaughtersHT2);
  TH1F *effGammaMB=new TH1F(*h_effDaughtersMB);
  TH1F *effGammaHT1=new TH1F(*h_effDaughtersHT1);
  TH1F *effGammaHT2=new TH1F(*h_effDaughtersHT2);
assert(effGammaMB);
assert(effGammaHT1);
assert(effGammaHT2);
    gg.Close();  
    h_effDaughtersMB = 0;
    h_effDaughtersHT1 = 0;
    h_effDaughtersHT2 = 0;

  //single photon eff:
  TFile gg_single(settings.input_gammaeff_file.Data(),"READ");
    const TH1F *h_effgammaMB = (const TH1F*)gg_single.Get("h_effMB");
    const TH1F *h_effgammaHT1 = (const TH1F*)gg_single.Get("h_effHT1");
    const TH1F *h_effgammaHT2 = (const TH1F*)gg_single.Get("h_effHT2");
assert(h_effgammaMB);
assert(h_effgammaHT1);
assert(h_effgammaHT2);
  TH1F *effGammaSingleMB=new TH1F(*h_effgammaMB);
  TH1F *effGammaSingleHT1=new TH1F(*h_effgammaHT1);
  TH1F *effGammaSingleHT2=new TH1F(*h_effgammaHT2);
assert(effGammaSingleMB);
assert(effGammaSingleHT1);
assert(effGammaSingleHT2);
    gg_single.Close();
    h_effgammaMB = 0;
    h_effgammaHT1 = 0;
    h_effgammaHT2 = 0;

  //raw neutral clusters:
  TFile ff(settings.input_pion_file.Data(),"READ");
    const TH1F *h_gammaMB = (const TH1F*)ff.Get("h_gammaMB");
    const TH1F *h_gammaHT1 = (const TH1F*)ff.Get("h_gammaHT1");
    const TH1F *h_gammaHT2 = (const TH1F*)ff.Get("h_gammaHT2");
assert(h_gammaMB);
assert(h_gammaHT1);
assert(h_gammaHT2);
  TH1F *gammaYieldMB=new TH1F(*h_gammaMB);
  TH1F *gammaYieldHT1=new TH1F(*h_gammaHT1);
  TH1F *gammaYieldHT2=new TH1F(*h_gammaHT2);
assert(gammaYieldMB);
assert(gammaYieldHT1);
assert(gammaYieldHT2);
    ff.Close();
    h_gammaMB = 0;
    h_gammaHT1 = 0;
    h_gammaHT2 = 0;

  //divide rap. bite:
  gammaYieldMB->Scale(1./dy_gamma);
  gammaYieldHT1->Scale(1./dy_gamma);
  gammaYieldHT2->Scale(1./dy_gamma);


  for(Int_t i=1;i<=gammaYieldMB->GetNbinsX();i++){
    gammaYieldMB->SetBinContent(i,gammaYieldMB->GetBinContent(i)/gammaYieldMB->GetXaxis()->GetBinWidth(i));
    gammaYieldMB->SetBinError(i,gammaYieldMB->GetBinError(i)/gammaYieldMB->GetXaxis()->GetBinWidth(i));
  }
  gammaYieldMB->Scale(psMB/(psMB*numberOfMB*2.*TMath::Pi()));
  gammaYieldMB->Divide(scale);// ../pT
  gammaYieldMB->Multiply(h_binmb);
  gammaYieldMB->Divide(effGammaMB);
  gammaYieldMB->Multiply(gamma_cpv_corrMB);
  gammaYieldMB->Multiply(gamma_cont_corrMB);
  gammaYieldMB->Multiply(gamma_conv_corrMB);

  gammaYieldMB->SetMarkerStyle(8);
  gammaYieldMB->SetMarkerSize(1.);
  TGraphErrors *g_inclPhotonsMB=new TGraphErrors(gammaYieldMB);
assert(g_inclPhotonsMB);

  for(Int_t i=1;i<=gammaYieldHT1->GetNbinsX();i++){
    gammaYieldHT1->SetBinContent(i,gammaYieldHT1->GetBinContent(i)/gammaYieldHT1->GetXaxis()->GetBinWidth(i));
    gammaYieldHT1->SetBinError(i,gammaYieldHT1->GetBinError(i)/gammaYieldHT1->GetXaxis()->GetBinWidth(i));
  }
  gammaYieldHT1->Scale(psHT1/(psMB*nMBwithHT*2.*TMath::Pi()));
  gammaYieldHT1->Divide(scale);// ../pT
  gammaYieldHT1->Multiply(h_binht1);
  gammaYieldHT1->Divide(effGammaHT1);
  gammaYieldHT1->Multiply(gamma_cpv_corrHT1);
  gammaYieldHT1->Multiply(gamma_cont_corrHT1);
  gammaYieldHT1->Multiply(gamma_conv_corrHT1);

  gammaYieldHT1->SetMarkerStyle(8);
  gammaYieldHT1->SetMarkerSize(1.);
  gammaYieldHT1->SetMarkerColor(4);
  TGraphErrors *g_inclPhotonsHT1=new TGraphErrors(gammaYieldHT1);
assert(g_inclPhotonsHT1);

  for(Int_t i=1;i<=gammaYieldHT2->GetNbinsX();i++){
    gammaYieldHT2->SetBinContent(i,gammaYieldHT2->GetBinContent(i)/gammaYieldHT2->GetXaxis()->GetBinWidth(i));
    gammaYieldHT2->SetBinError(i,gammaYieldHT2->GetBinError(i)/gammaYieldHT2->GetXaxis()->GetBinWidth(i));
  }
  gammaYieldHT2->Scale(psHT2/(psMB*nMBwithHT*2.*TMath::Pi()));
  gammaYieldHT2->Divide(scale);// ../pT
  gammaYieldHT2->Multiply(h_binht2);
  gammaYieldHT2->Divide(effGammaHT2);
  gammaYieldHT2->Multiply(gamma_cpv_corrHT2);
  gammaYieldHT2->Multiply(gamma_cont_corrHT2);
  gammaYieldHT2->Multiply(gamma_conv_corrHT2);

  gammaYieldHT2->SetMarkerStyle(8);
  gammaYieldHT2->SetMarkerSize(1.);
  gammaYieldHT2->SetMarkerColor(2);
  TGraphErrors *g_inclPhotonsHT2=new TGraphErrors(gammaYieldHT2);
assert(g_inclPhotonsHT2);

  removeThesePoints(g_inclPhotonsMB,1);
  removeThesePoints(g_inclPhotonsHT1,2);
  removeThesePoints(g_inclPhotonsHT2,2);

  TMultiGraph *m_incl=new TMultiGraph();
assert(m_incl);
  m_incl->SetName("m_incl");
  m_incl->SetTitle("inclusive photon invariant yield 0<y<1;p_{T} (GeV/c);#frac{1}{2#piNp_{T}} #frac{d^{2}N}{dydp_{T}}");
  m_incl->Add(g_inclPhotonsMB);
  m_incl->Add(g_inclPhotonsHT1);
  m_incl->Add(g_inclPhotonsHT2);

  //m_incl->Fit(fitQCD,"R0");

  m_incl->SetMinimum(1.e-11);
  m_incl->SetMaximum(10.);
  TCanvas *c_incl=new TCanvas("c_incl","c_incl",600,400);
assert(c_incl);
  gPad->SetLogy();
  m_incl->Draw("ap");
  c_incl->SaveAs(settings.output_inclphotonyield_file.Data());



  //get ratio:
  TH1F *gammaYieldMBratio=new TH1F(*gammaYieldMB);
  TH1F *gammaYieldHT1ratio=new TH1F(*gammaYieldHT1);
  TH1F *gammaYieldHT2ratio=new TH1F(*gammaYieldHT2);
assert(gammaYieldMBratio);
assert(gammaYieldHT1ratio);
assert(gammaYieldHT2ratio);
  gammaYieldMBratio->SetName("gammaYieldMBratio");
  gammaYieldHT1ratio->SetName("gammaYieldHT1ratio");
  gammaYieldHT2ratio->SetName("gammaYieldHT2ratio");

  gammaYieldMBratio->Divide(pionYieldMBratio);
  gammaYieldHT1ratio->Divide(pionYieldHT1ratio);
  gammaYieldHT2ratio->Divide(pionYieldHT2ratio);


  //correct gamma over pion ratio, using two efficiencies:
  getRatio(gammaYieldMBratio,effGammaMB,effGammaSingleMB,fit_piondecay);
  getRatio(gammaYieldHT1ratio,effGammaHT1,effGammaSingleHT1,fit_piondecay);
  getRatio(gammaYieldHT2ratio,effGammaHT2,effGammaSingleHT2,fit_piondecay);

  TH1F *gammaYieldMBratio_incl=new TH1F(*gammaYieldMBratio);
  TH1F *gammaYieldHT1ratio_incl=new TH1F(*gammaYieldHT1ratio);
  TH1F *gammaYieldHT2ratio_incl=new TH1F(*gammaYieldHT2ratio);
assert(gammaYieldMBratio_incl);
assert(gammaYieldHT1ratio_incl);
assert(gammaYieldHT2ratio_incl);

  TH1F *gammaYieldMBratioNoErr=new TH1F(*gammaYieldMBratio);
  TH1F *gammaYieldHT1ratioNoErr=new TH1F(*gammaYieldHT1ratio);
  TH1F *gammaYieldHT2ratioNoErr=new TH1F(*gammaYieldHT2ratio);
assert(gammaYieldMBratioNoErr);
assert(gammaYieldHT1ratioNoErr);
assert(gammaYieldHT2ratioNoErr);
  for(int i=1;i<=gammaYieldMBratioNoErr->GetNbinsX();i++)gammaYieldMBratioNoErr->SetBinError(i,0.);
  for(int i=1;i<=gammaYieldHT1ratioNoErr->GetNbinsX();i++)gammaYieldHT1ratioNoErr->SetBinError(i,0.);
  for(int i=1;i<=gammaYieldHT2ratioNoErr->GetNbinsX();i++)gammaYieldHT2ratioNoErr->SetBinError(i,0.);

  TGraphErrors *g_ratioMB=new TGraphErrors(gammaYieldMBratio);
  TGraphErrors *g_ratioHT1=new TGraphErrors(gammaYieldHT1ratio);
  TGraphErrors *g_ratioHT2=new TGraphErrors(gammaYieldHT2ratio);
assert(g_ratioMB);
assert(g_ratioHT1);
assert(g_ratioHT2);
  g_ratioMB->SetName("g_ratioMB");
  g_ratioHT1->SetName("g_ratioHT1");
  g_ratioHT2->SetName("g_ratioHT2");


  removeThesePoints(g_ratioMB,1);
  removeThesePoints(g_ratioHT1,2);
  removeThesePoints(g_ratioHT2,3);


  TCanvas *c_ratio=new TCanvas("c_ratio","c_ratio",400,200);
assert(c_ratio);

  TMultiGraph *m_ratio=new TMultiGraph("m_ratio","p+p 2005;p_{T};#gamma/#pi^{0}");
assert(m_ratio);
  m_ratio->Add(g_ratioMB);
  m_ratio->Add(g_ratioHT1);
  m_ratio->Add(g_ratioHT2);

  m_ratio->Draw("ap");
  m_ratio->SetMinimum(.001);
  m_ratio->SetMaximum(1.5);

  TLegend *leg3=new TLegend(.35,.65,.65,.85);
assert(leg3);
  leg3->AddEntry(g_ratioMB,"minimum bias","p");
  leg3->AddEntry(g_ratioHT1,"hightower 1","p");
  leg3->AddEntry(g_ratioHT2,"hightower 2","p");
  leg3->AddEntry(fit_decay,"decay background (total)","l");
  leg3->AddEntry(fit_piondecay,"decay background (#pi^{0})","l");
  leg3->SetFillColor(0);
  leg3->Draw("same");

  fit_decay->SetLineColor(13);
  fit_decay->SetLineWidth(1);
  fit_decay->SetLineColor(1);
  fit_decay->Draw("same");
  fit_piondecay->SetLineColor(13);
  fit_piondecay->SetLineWidth(1);
  fit_piondecay->SetLineStyle(2);
  fit_piondecay->SetLineColor(1);
  fit_piondecay->Draw("same");

  c_ratio->SaveAs(settings.output_gammaoverpion_file.Data());

  //create fully corrected incl. photons:
  gammaYieldMBratio_incl->Multiply(pionYieldMBratio);
  gammaYieldHT1ratio_incl->Multiply(pionYieldHT1ratio);
  gammaYieldHT2ratio_incl->Multiply(pionYieldHT2ratio);
  //gammaYieldMBratio_incl->Scale(7.5);
  //gammaYieldHT1ratio_incl->Scale(7.5);
  //gammaYieldHT2ratio_incl->Scale(7.5);
  
  TGraphErrors *g_incl_corrMB=new TGraphErrors(gammaYieldMBratio_incl);
  TGraphErrors *g_incl_corrHT1=new TGraphErrors(gammaYieldHT1ratio_incl);
  TGraphErrors *g_incl_corrHT2=new TGraphErrors(gammaYieldHT2ratio_incl);
assert(g_incl_corrMB);
assert(g_incl_corrHT1);
assert(g_incl_corrHT2);

  TCanvas *c_incl_corr=new TCanvas("c_incl_corr","c_incl_corr",400,300);
assert(c_incl_corr);
  gPad->SetLogy();
  TMultiGraph *m_incl_corr=new TMultiGraph();
assert(m_incl_corr);
  m_incl_corr->Add(g_incl_corrMB);
  m_incl_corr->Add(g_incl_corrHT1);
  m_incl_corr->Add(g_incl_corrHT2);

  m_incl_corr->SetMinimum(1.e-11);
  m_incl_corr->SetMaximum(1.);

  m_incl_corr->Draw("apX");
  c_incl_corr->SaveAs(settings.output_inclphotonyieldcorr_file.Data());

  TCanvas *c_doubleratio=new TCanvas("c_doubleratio","c_doubleratio",400,300);
assert(c_doubleratio);
  gStyle->SetOptStat(0);
  c_doubleratio->cd(1);

  TH1F *gammaYieldMBdoubleratio=new TH1F(*gammaYieldMBratio);
  TH1F *gammaYieldHT1doubleratio=new TH1F(*gammaYieldHT1ratio);
  TH1F *gammaYieldHT2doubleratio=new TH1F(*gammaYieldHT2ratio);
assert(gammaYieldMBdoubleratio);
assert(gammaYieldHT1doubleratio);
assert(gammaYieldHT2doubleratio);

  gammaYieldMBdoubleratio->Divide(fit_decay);
  gammaYieldHT1doubleratio->Divide(fit_decay);
  gammaYieldHT2doubleratio->Divide(fit_decay);

  TGraphErrors *g_doubleRatioMB=new TGraphErrors(gammaYieldMBdoubleratio);
assert(g_doubleRatioMB);
  g_doubleRatioMB->SetName("g_doubleRatioMB");
  g_doubleRatioMB->SetMarkerStyle(8);
  TGraphErrors *g_doubleRatioHT1=new TGraphErrors(gammaYieldHT1doubleratio);
assert(g_doubleRatioHT1);
  g_doubleRatioHT1->SetName("g_doubleRatioHT1");
  g_doubleRatioHT1->SetMarkerStyle(8);
  TGraphErrors *g_doubleRatioHT2=new TGraphErrors(gammaYieldHT2doubleratio);
assert(g_doubleRatioHT2);
  g_doubleRatioHT2->SetName("g_doubleRatioHT2");
  g_doubleRatioHT2->SetMarkerStyle(8);

  removeThesePoints(g_doubleRatioMB,1);
  removeThesePoints(g_doubleRatioHT1,2);
  removeThesePoints(g_doubleRatioHT2,3);

  TMultiGraph *m_doubleratio=new TMultiGraph();
assert(m_doubleratio);
  m_doubleratio->SetName("m_doubleratio");
  m_doubleratio->SetMinimum(.5);
  m_doubleratio->SetMaximum(2.75);

  cout<<endl;
  g_doubleRatioHT1->Print();
  cout<<endl<<endl;
  g_doubleRatioHT2->Print();
  cout<<endl;

  //m_doubleratio->Add(g_doubleRatioMB,"p");
  m_doubleratio->Add(g_doubleRatioHT1,"p");
  m_doubleratio->Add(g_doubleRatioHT2,"p");

  g_photonpqcd->SetLineWidth(2);
  g_photonpqcd->SetLineColor(2);
  g_photonpqcd05->SetLineWidth(2);
  g_photonpqcd05->SetLineColor(2);
  g_photonpqcd05->SetLineStyle(2);
  g_photonpqcd2->SetLineWidth(2);
  g_photonpqcd2->SetLineColor(2);
  g_photonpqcd2->SetLineStyle(2);

  m_doubleratio->Add(g_photonpqcd,"c");
  m_doubleratio->Add(g_photonpqcd05,"c");
  m_doubleratio->Add(g_photonpqcd2,"c");

  //appropriate fit to photon pqcd result
  TF1 *fitGamma2=new TF1("fitGamma2","1.+[0]*TMath::Power(x,[1])",2.,15.);
assert(fitGamma2);
  g_photonpqcd->Fit(fitGamma2,"R0");

  m_doubleratio->Draw("a");
  
  m_doubleratio->GetXaxis()->SetTitle("p_{T} (GeV/c)");
  m_doubleratio->GetYaxis()->SetTitle("1 + #gamma_{dir}/#gamma_{incl}");
  m_doubleratio->GetXaxis()->SetRangeUser(2.,16.);
  

  TLegend *leg5=new TLegend(.15,.6,.6,.8);
assert(leg5);
  leg5->AddEntry(g_doubleRatioHT1,"hightower-1","p");
  leg5->AddEntry(g_doubleRatioHT2,"hightower-2","p");
  leg5->AddEntry(g_photonpqcd,"NLO (CTEQ6+KKP) #mu=p_{T}","l");
  leg5->AddEntry(g_photonpqcd05,"#mu=2p_{T}, #mu=p_{T}/2","l");
  leg5->SetFillColor(0);
  leg5->Draw("same");

  c_doubleratio->cd(0);  
  c_doubleratio->SaveAs(settings.output_gammadoubleratio_file.Data());

    {
    // calculating antineutron contamination C0
    TH1F *h_nbarContMB = new TH1F(*h_nbarEffMB);
    TH1F *h_nbarContHT1 = new TH1F(*h_nbarEffHT1);
    TH1F *h_nbarContHT2 = new TH1F(*h_nbarEffHT2);
assert(h_nbarContMB);
assert(h_nbarContHT1);
assert(h_nbarContHT2);
    h_nbarContMB->Multiply(f_nbar);
    h_nbarContHT1->Multiply(f_nbar);
    h_nbarContHT2->Multiply(f_nbar);

    h_nbarContMB->Divide(gammaYieldMB);
    h_nbarContHT1->Divide(gammaYieldHT1);
    h_nbarContHT2->Divide(gammaYieldHT2);

    TH1F *effGammaMBnoerr = new TH1F(*effGammaMB);
    TH1F *effGammaHT1noerr = new TH1F(*effGammaHT1);
    TH1F *effGammaHT2noerr = new TH1F(*effGammaHT2);
assert(effGammaMBnoerr);
assert(effGammaHT1noerr);
assert(effGammaHT2noerr);
    for (Int_t i = 1;i <= effGammaMBnoerr->GetXaxis()->GetNbins();i++) effGammaMBnoerr->SetBinError(i, 0);
    for (Int_t i = 1;i <= effGammaHT1noerr->GetXaxis()->GetNbins();i++) effGammaHT1noerr->SetBinError(i, 0);
    for (Int_t i = 1;i <= effGammaHT2noerr->GetXaxis()->GetNbins();i++) effGammaHT2noerr->SetBinError(i, 0);
    h_nbarContMB->Divide(effGammaMBnoerr);
    h_nbarContHT1->Divide(effGammaHT1noerr);
    h_nbarContHT2->Divide(effGammaHT2noerr);
    
    TCanvas *c_nbar_cont = new TCanvas("c_nbar_cont", "Antineutron contamination");
assert(c_nbar_cont);
    TH1F *h_nbar_cont = new TH1F("h_nbar_cont", "Antineutron contamination C_{0};p_{T} [GeV/c];C_{0}", 1000, 0, 10);
assert(h_nbar_cont);
    h_nbar_cont->Draw();
    h_nbar_cont->GetYaxis()->SetRangeUser(0, 1.5);

    h_nbarContMB->SetMarkerStyle(kOpenCircle);
    h_nbarContMB->SetMarkerSize(1.0);
    h_nbarContMB->SetMarkerColor(kBlack);
    h_nbarContMB->SetLineStyle(kSolid);
    h_nbarContMB->SetLineWidth(1);
    h_nbarContMB->SetLineColor(kBlack);

    h_nbarContHT1->SetMarkerStyle(kOpenSquare);
    h_nbarContHT1->SetMarkerSize(1.0);
    h_nbarContHT1->SetMarkerColor(kBlue);
    h_nbarContHT1->SetLineStyle(kSolid);
    h_nbarContHT1->SetLineWidth(1);
    h_nbarContHT1->SetLineColor(kBlue);

    h_nbarContHT2->SetMarkerStyle(kOpenTriangleUp);
    h_nbarContHT2->SetMarkerSize(1.0);
    h_nbarContHT2->SetMarkerColor(kRed);
    h_nbarContHT2->SetLineStyle(kSolid);
    h_nbarContHT2->SetLineWidth(1);
    h_nbarContHT2->SetLineColor(kRed);

    h_nbarContMB->Draw("SAME");
    h_nbarContHT1->Draw("SAME");
    h_nbarContHT2->Draw("SAME");

    // extreme case - at maximum C0 reaches unity
    Float_t maxCont = h_nbarContMB->GetMaximum();
    TH1F *h_nbarContHT2_extreme = new TH1F(*h_nbarContHT2);
assert(h_nbarContHT2_extreme);
    h_nbarContHT2_extreme->Scale(1.0/maxCont);
    h_nbarContHT2_extreme->SetLineStyle(kDashed);
    h_nbarContHT2_extreme->SetLineWidth(1);
    h_nbarContHT2_extreme->SetLineColor(kRed);
    h_nbarContHT2_extreme->Draw("SAME HIST C");

    // another case - at 1 < pT < 4 GeV/c direct photons vanish, 
    // therefore an excess of MinBias double ratio over unity in that region
    // is attributed to antineutrons, so that C0 = 1 - (1/R)
    TH1F *h_corr = new TH1F(*gammaYieldMBdoubleratio);
assert(h_corr);
    for (Int_t i = 1;i < h_corr->GetXaxis()->GetNbins();i++) {
	h_corr->SetBinContent(i, 1.0);
	h_corr->SetBinError(i, 0.0);
    }
    h_corr->Divide(gammaYieldMBdoubleratio);
    //h_corr->Scale(-1.0);
    for (Int_t i = 1;i < h_corr->GetXaxis()->GetNbins();i++) {
	h_corr->SetBinContent(i, 1.0 - h_corr->GetBinContent(i));
    }
    h_corr->Divide(h_nbarContMB);
    h_corr->GetXaxis()->SetRangeUser(1.0, 4.0);
    TF1 *f_corr = new TF1("f_corr", "[0]");
    f_corr->SetRange(1.0, 4.0);
    h_corr->Fit(f_corr, "RQN");
    Float_t corrCoef = f_corr->GetParameter(0);
    TH1F *h_nbarContHT2_corr = new TH1F(*h_nbarContHT2);
assert(h_nbarContHT2_corr);
    h_nbarContHT2_corr->Scale(corrCoef);
    h_nbarContHT2_corr->SetLineStyle(kSolid);
    h_nbarContHT2_corr->SetLineWidth(1);
    h_nbarContHT2_corr->SetLineColor(17);
    h_nbarContHT2_corr->SetFillColor(17);
    h_nbarContHT2_corr->SetFillStyle(1001);
    h_nbarContHT2_corr->Draw("SAME HIST AC");

    TLegend *leg = new TLegend(0.5, 0.5, 0.8, 0.8);
assert(leg);
    leg->AddEntry(h_nbarContMB, "MinBias", "P");
    leg->AddEntry(h_nbarContHT1, "HighTower-1", "P");
    leg->AddEntry(h_nbarContHT2, "HighTower-2", "P");
    leg->AddEntry(h_nbarContHT2_extreme, "extreme case", "L");
    leg->AddEntry(h_nbarContHT2_corr, "corrected", "F");
    leg->Draw();

    h_nbar_cont->Draw("SAME");
    c_nbar_cont->SaveAs(settings.output_nbarcont_file.Data());
    }

  TCanvas *c_dirphoton=new TCanvas("c_dirphoton","c_dirphoton",400,300);
assert(c_dirphoton);
  gStyle->SetOptStat(0);
  c_dirphoton->cd(1);

  TH1F *dirphotonYieldMB=new TH1F(*gammaYieldMBdoubleratio);
  TH1F *dirphotonYieldHT1=new TH1F(*gammaYieldHT1doubleratio);
  TH1F *dirphotonYieldHT2=new TH1F(*gammaYieldHT2doubleratio);
assert(dirphotonYieldMB);
assert(dirphotonYieldHT1);
assert(dirphotonYieldHT2);

  TH1F *dirphotonYieldMBnoErr=new TH1F(*dirphotonYieldMB);
assert(dirphotonYieldMBnoErr);
  for(int i=1;i<=dirphotonYieldMBnoErr->GetNbinsX();i++){
    dirphotonYieldMBnoErr->SetBinError(i,0.);
  }
  TH1F *dirphotonYieldHT1noErr=new TH1F(*dirphotonYieldHT1);
assert(dirphotonYieldHT1noErr);
  for(int i=1;i<=dirphotonYieldHT1noErr->GetNbinsX();i++){
    dirphotonYieldHT1noErr->SetBinError(i,0.);
  }
  TH1F *dirphotonYieldHT2noErr=new TH1F(*dirphotonYieldHT2);
assert(dirphotonYieldHT2noErr);
  for(int i=1;i<=dirphotonYieldHT1noErr->GetNbinsX();i++){
    dirphotonYieldHT2noErr->SetBinError(i,0.);
  }

  TF1 *f_unity=new TF1("f_unity","1.",0.,15.);
assert(f_unity);
  dirphotonYieldMB->Add(f_unity,-1.);
  dirphotonYieldHT1->Add(f_unity,-1.);
  dirphotonYieldHT2->Add(f_unity,-1.);


  dirphotonYieldMB->Divide(dirphotonYieldMBnoErr);
  dirphotonYieldHT1->Divide(dirphotonYieldHT1noErr);
  dirphotonYieldHT2->Divide(dirphotonYieldHT2noErr);
  dirphotonYieldMB->Multiply(gammaYieldMBratioNoErr);
  dirphotonYieldHT1->Multiply(gammaYieldHT1ratioNoErr);
  dirphotonYieldHT2->Multiply(gammaYieldHT2ratioNoErr);
  dirphotonYieldMB->Multiply(pionXsMBnoErr);
  dirphotonYieldHT1->Multiply(pionXsHT1noErr);
  dirphotonYieldHT2->Multiply(pionXsHT2noErr);


  TGraphErrors *g_dirphotonMB=new TGraphErrors(dirphotonYieldMB);
assert(g_dirphotonMB);
  g_dirphotonMB->SetName("g_dirphotonMB");
  g_dirphotonMB->SetMarkerStyle(8);
  TGraphErrors *g_dirphotonHT1=new TGraphErrors(dirphotonYieldHT1);
assert(g_dirphotonHT1);
  g_dirphotonHT1->SetName("g_dirphotonHT1");
  g_dirphotonHT1->SetMarkerStyle(8);
  TGraphErrors *g_dirphotonHT2=new TGraphErrors(dirphotonYieldHT2);
assert(g_dirphotonHT2);
  g_dirphotonHT2->SetName("g_dirphotonHT2");
  g_dirphotonHT2->SetMarkerStyle(8);


  removeThesePoints(g_dirphotonMB,1);
  removeThesePoints(g_dirphotonHT1,2);
  removeThesePoints(g_dirphotonHT2,3);

  gPad->SetLogy();

  TMultiGraph *m_dirphoton=new TMultiGraph();
assert(m_dirphoton);
  m_dirphoton->SetName("m_dirphoton");
  m_dirphoton->SetMinimum(1.0e-11);
  m_dirphoton->SetMaximum(0.1);

  m_dirphoton->Add(g_dirgamma,"c");
  m_dirphoton->Add(g_dirgamma05,"c");
  m_dirphoton->Add(g_dirgamma2,"c");

  cout<<"direct photons:"<<endl;
  g_dirphotonHT1->Print();
  cout<<endl;
  g_dirphotonHT2->Print();
  cout<<endl;

  m_dirphoton->Add(g_dirphotonHT1,"p");
  m_dirphoton->Add(g_dirphotonHT2,"p");

  m_dirphoton->Draw("a");
  
  m_dirphoton->GetXaxis()->SetTitle("p_{T} (GeV/c)");
  m_dirphoton->GetYaxis()->SetTitle("1 - R^{-1}");
  m_dirphoton->GetXaxis()->SetRangeUser(2.,16.);
  

  TLegend *leght=new TLegend(.15,.6,.6,.8);
assert(leght);
  leght->AddEntry(g_dirphotonHT1,"hightower-1","p");
  leght->AddEntry(g_dirphotonHT2,"hightower-2","p");
  leght->SetFillColor(0);
  leght->Draw("same");

  c_dirphoton->cd(0);  
  c_dirphoton->SaveAs(settings.output_gammadirphoton_file.Data());
}


