#include <iostream>
#include <fstream>
using namespace std;

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TMath.h"
#include "TPostScript.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TLatex.h"
#include "TTree.h"
#include "TLine.h"

#include "CalibrationHelperFunctions.h"
#include "CalibrationHelperFunctions.cxx"

void drawTower(TH1* h, TF1* f, int id, CalibrationHelperFunctions* helper);
double fit_function(double *x, double *par);
double fit_function2(double *x, double *par);
double background_only_fit(double *x, double *par);
int lookup_crate(float,float);
int lookup_crateslice(float,float);
//bool isBadTower2011(int id);

//

//void electron_master_temp(const char* file_list="electrons.list",const char* output="electronmaster.root", const char* dbDate="1999-01-01 00:11:00", const char* geantfile="geant_func.root", const char* gfname="mip.gains.final", const char* ngname="electron.gains", const char* postscript="electrons.ps"){
void electron_drawfits(const char* infile="finalelec2009.root", const char* gfname="mip.gains", const char* ngname="electron2009.gains", const char* postscript="electrons2009.ps", const char* ffile="electron2009.fits"){
  //**********************************************//
  //Load Libraries                                //
  //**********************************************//
  gROOT->Macro("LoadLogger.C");
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDaqLib");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");//***
  gSystem->Load("StEmcSimulatorMaker");//***
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StEmcOfflineCalibrationMaker");

  cout<<"input:          "<<infile<<endl;
  cout<<"plots:          "<<postscript<<endl;


  //**********************************************//
  //Initialize Stuff                              //
  //**********************************************//

  CalibrationHelperFunctions *helper = new CalibrationHelperFunctions();

  
  const char* dbDate="2009-01-01 00:11:00";
  StBemcTablesWriter *bemctables = new StBemcTablesWriter();
  bemctables->loadTables(dbDate,"sim");
  StEmcDecoder* decoder = bemctables->getDecoder();

  const int ntowers = 4800;
  const int nrings = 40;
  const int ncrates = 30;
  const int ncrateslices = ncrates*20;
  float gains[ntowers];
  int status[ntowers];
  float peaks[ntowers];
  float peakerr[ntowers];
  float gainerr[ntowers];


  ifstream gainfile(gfname);
  while(1){
    int id,stat;
    float peak,err,gain,eta,theta;
    gainfile >> id >> peak >> err >> stat;
    if(!gainfile.good())break;
    eta = helper->getEta(id);
    theta = helper->getTheta(id);
    gain = 0.264*(1+0.056*eta*eta)/(sin(theta)*peak);
    peaks[id-1] = peak;
    gains[id-1] = gain;
    status[id-1] = stat;
    peakerr[id-1] = err;
    //cout<<id<<" "<<gain;
    //if(isBadTower2011(id)) status[id-1] = 3;
  }


  TPostScript *ps = new TPostScript(postscript);

  TFile *f = new TFile(infile,"read");


  TH1 *crate_histo[ncrates];
  TH1 *crate_histo_tof[ncrates];
  TF1 *crate_fit[ncrates];
  TH1 *electron_histo[ntowers];
  //TF1 *fit[ntowers];
  //TH1 *prs_histo[ntowers];
  TH1 *ring_histo[nrings];
  TH1 *ring_histo_tof[nrings];
  TH2 *ringp_histo[nrings];
  TH2 *ringp_histo_tof[nrings];
  TF1 *ringfit[nrings];
  TF1 *ringfittof[nrings];
  //TH1 *ring2_histo[nrings/2];
  //TH1 *ring2_histo_tof[nrings/2];
  //TH1 *ring_histo_pos[nrings];
  //TH1 *ring_histo_neg[nrings];
  //TH1 *ring_histo_pos_tof[nrings];
  //TH1 *ring_histo_neg_tof[nrings];
  TH1 *crateslice_histo[ncrates][nrings/2];
  TH1 *crateslice_histo_tof[ncrates][nrings/2];
  TF1 *crateslicefit[ncrates][nrings/2];
  TF1 *crateslicefittof[ncrates][nrings/2];

  double par3[nrings];
  double par3err[nrings];
  double par3_tof[nrings];
  double par3err_tof[nrings];

  TGraphErrors *gr[nrings];
  TGraphErrors *grtof[nrings];


  //TH2F* ring_pve[nrings];
  TH2F* jan_pve[6];
  float eta;
  int etaindex, geantetaindex;
  TH1F* ringprec = new TH1F("ringprec","",40,-1.,1.);
  TH1F* ringprec2 = new TH1F("ringprec2","",40,-1.,1.);
  //TH1F* ring2prec = (TH1F*)f->Get("ring2prec");
  //TH1F* ring2prec2 = (TH1F*)f->Get("ring2prec2");

  TH1F* ringprec_tof = new TH1F("ringprec_tof","",40,-1.,1.);
  /*TH1F* ringprec_pos = (TH1F*)f->Get("ringprec_pos");
  TH1F* ringprec_neg = (TH1F*)f->Get("ringprec_neg");
  TH1F* ringprec_pos_tof = (TH1F*)f->Get("ringprec_pos_tof");
  TH1F* ringprec_neg_tof = (TH1F*)f->Get("ringprec_neg_tof");*/
  TH1F* ringprec2_tof = new TH1F("ringprec2_tof","",40,-1.,1.);
  /*TH1F* ringprec2_pos = (TH1F*)f->Get("ringprec2_pos");
  TH1F* ringprec2_neg = (TH1F*)f->Get("ringprec2_neg");
  TH1F* ringprec2_pos_tof = (TH1F*)f->Get("ringprec2_pos_tof");
  TH1F* ringprec2_neg_tof = (TH1F*)f->Get("ringprec2_neg_tof");*/

  TH1F* crateprec = new TH1F("crateprec","",30,0,30);
  TH1F* crateprec_tof = new TH1F("crateprec_tof","",30,0,30);
  TH1F* cratesliceprec = new TH1F("cratesliceprec","",600,0,600);
  TH1F* cratesliceprec_tof = new TH1F("cratesliceprec_tof","",600,0,600);

  // compare fit parameters
  TH1F* hPed_ring = new TH1F("hPed_ring","Pedestal magnitude",40,-1.,1.);
  TH1F* hSlope_ring = new TH1F("hSlope_ring","Slope magnitude",40,-1.,1.);
  TH1F* hPeaks_ring = new TH1F("hPeaks_ring","Peak magnitude",40,-1.,1.);
  TH1F* hMean_ring = new TH1F("hMean_ring","Peak mean",40,-1.,1.);
  TH1F* hSigma_ring = new TH1F("hSigma_ring","Peak sigma",40,-1.,1.);
  TH1F* hChi2_ring = new TH1F("hChi2_ring","Chi2/NDF",40,-1.,1.);
  TH1F* hPed_ring_tof = new TH1F("hPed_ring_tof","Pedestal magnitude",40,-1.,1.);
  TH1F* hSlope_ring_tof = new TH1F("hSlope_ring_tof","Slope magnitude",40,-1.,1.);
  TH1F* hPeaks_ring_tof = new TH1F("hPeaks_ring_tof","Peak magnitude",40,-1.,1.);
  TH1F* hMean_ring_tof = new TH1F("hMean_ring_tof","Peak mean",40,-1.,1.);
  TH1F* hSigma_ring_tof = new TH1F("hSigma_ring_tof","Peak sigma",40,-1.,1.);
  TH1F* hChi2_ring_tof = new TH1F("hChi2_ring_tof","Chi2/NDF",40,-1.,1.);
  TH1F* hPed_crate = new TH1F("hPed_crate","Pedestal magnitude",30,0,30);
  TH1F* hSlope_crate = new TH1F("hSlope_crate","Slope magnitude",30,0,30);
  TH1F* hPeaks_crate = new TH1F("hPeaks_crate","Peak magnitude",30,0,30);
  TH1F* hMean_crate = new TH1F("hMean_crate","Peak mean",30,0,30);
  TH1F* hSigma_crate = new TH1F("hSigma_crate","Peak sigma",30,0,30);
  TH1F* hChi2_crate = new TH1F("hChi2_crate","Chi2/NDF",30,0,30);
  TH1F* hPed_crate_tof = new TH1F("hPed_crate_tof","Pedestal magnitude",30,0,30);
  TH1F* hSlope_crate_tof = new TH1F("hSlope_crate_tof","Slope magnitude",30,0,30);
  TH1F* hPeaks_crate_tof = new TH1F("hPeaks_crate_tof","Peak magnitude",30,0,30);
  TH1F* hMean_crate_tof = new TH1F("hMean_crate_tof","Peak mean",30,0,30);
  TH1F* hSigma_crate_tof = new TH1F("hSigma_crate_tof","Peak sigma",30,0,30);
  TH1F* hChi2_crate_tof = new TH1F("hChi2_crate_tof","Chi2/NDF",30,0,30);

  TH2F *hChi2EtaPhi = new TH2F("hChi2EtaPhi","Chi2/NDF",40,-1,1,120,-TMath::Pi(),TMath::Pi());

  TH2F *petafinal = (TH2F*)f->Get("petafinal");
  TH1D *pfinal = petafinal->ProjectionX();
  pfinal->Scale(1./pfinal->Integral("width"));
  TH1D *etafinal = petafinal->ProjectionY();
  etafinal->Scale(1./etafinal->Integral("width"));
  TH2F *petafinal_tof = (TH2F*)f->Get("petafinal_tof");
  TH1D *pfinal_tof = petafinal_tof->ProjectionX();
  //pfinal_tof->Scale(1./pfinal_tof->Integral("width"));
  TH1D *etafinal_tof = petafinal_tof->ProjectionY();
  //etafinal_tof->Scale(1./etafinal_tof->Integral("width"));


  char name[100];
  char title[100];
  for(int i = 0; i < ncrates; i++){
    sprintf(name,"crate_%i",i+1);
    crate_histo[i] = (TH1F*)f->Get(name);
    sprintf(name,"crate_tof_%i",i+1);
    crate_histo_tof[i] = (TH1F*)f->Get(name);
  }


  /*for(int i = 0; i < nrings/2; i++){
    sprintf(name,"ring2_histo_%i",i);
    //ring2_histo[i] = (TH1F*)f->Get(name);
    ring2_histo[i] = new TH1F(name,"",60,0.,3.0);
    ring2_histo[i]->SetXTitle("E/p");
    sprintf(name,"ring2_histo_tof_%i",i);
    ring2_histo_tof[i] = new TH1F(name,"",60,0.,3.0);
    ring2_histo_tof[i]->SetXTitle("E/p");
    }*/


  for(int i=0; i<nrings;i++)
    {
      sprintf(name,"ring_histo_%i",i);
      ring_histo[i] = (TH1F*)f->Get(name);
      sprintf(name,"ring_histo_tof_%i",i);
      ring_histo_tof[i] = (TH1F*)f->Get(name);

      sprintf(name,"ringp_histo_%i",i);
      ringp_histo[i] = (TH2F*)f->Get(name);
      sprintf(name,"ringp_histo_tof_%i",i);
      ringp_histo_tof[i] = (TH2F*)f->Get(name);

      gr[i] = new TGraphErrors();
      grtof[i] = new TGraphErrors();

      /*sprintf(name,"ring_histo_pos_%i",i);
      ring_histo_pos[i] = (TH1F*)f->Get(name);
      sprintf(name,"ring_histo_neg_%i",i);
      ring_histo_neg[i] = (TH1F*)f->Get(name);
      sprintf(name,"ring_histo_pos_tof_%i",i);
      ring_histo_pos_tof[i] = (TH1F*)f->Get(name);
      sprintf(name,"ring_histo_neg_tof_%i",i);
      ring_histo_neg_tof[i] = (TH1F*)f->Get(name);*/

    }

  for(int i = 0; i < ncrates; i++){
    for(int j = 0; j < nrings/2; j++){
      sprintf(name,"crateslice_%i_%i",i+1,j+1);
      crateslice_histo[i][j] = (TH1F*)f->Get(name);
      sprintf(name,"crateslice_tof_%i_%i",i+1,j+1);
      crateslice_histo_tof[i][j] = (TH1F*)f->Get(name);
    }    
  }

  //global graphics functions
  gStyle->SetOptStat("ouen");
  gStyle->SetOptFit(111);
  gStyle->SetCanvasColor(10);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);
  gStyle->SetStatColor(0);


  //**********************************************//
  //Fit Tower Histograms                          //
  //**********************************************//
  /*
  for(int i=0; i<ntowers; i++){
	  
    if(i%600 == 0) cout<<"fitting tower "<<i+1<<" of "<<ntowers<<endl;
		
    sprintf(name,"fit_%i",i+1);
		
		//this fit is for the electron tree
    fit[i] = new TF1(name,fit_function,0.,140.,6);
    fit[i]->SetParameter(1,65.);
    fit[i]->SetParameter(2,10.);
    fit[i]->SetParameter(3,10.); //relative height of peak to bg
    fit[i]->SetParameter(4,10.);
    fit[i]->SetParameter(5,3.);
    fit[i]->SetParNames("Constant","Mean","Sigma","Peak Ratio","Bg Mean","Bg Sigma");
		
    fit[i]->SetLineColor(kGreen);
    fit[i]->SetLineWidth(0.6);
		
    electron_histo[i]->Fit(fit[i],"rq");
  }
  */
  //**********************************************//
  //Fit Ring Histograms                           //
  //**********************************************//

  const Int_t npbins = 14;
  Int_t pbins[npbins+1] = {1,2,3,4,5,6,8,10,12,14,16,21,26,36,45};
  //TCanvas *c2 = new TCanvas();
  TH1F *htemp = new TH1F("htemp","",10,1.5,6.);
  htemp->SetMaximum(1.2);
  htemp->SetXTitle("p");
  htemp->SetYTitle("E/p mean");
  //htemp->DrawCopy();
  TCanvas *c = new TCanvas("c","",100,100,600.,800.);

  //TF1* ringfit = new TF1("ringfit","pol1(0) + gaus(2)",0.3,1.7);

  for(int i=0; i<nrings; i++){

    if(i%2==0)
      {
	c->Update();
	ps->NewPage();
	c->Clear();
	c->Divide(1,2);
      }
    c->cd((i%2)+1);
    //c->cd(1);

    //cout<<"fitting ring "<<i+1<<" of "<<nrings<<endl;
		
    sprintf(name,"ring_fit_%i",i);

    ring_histo[i]->Sumw2();
    ring_histo_tof[i]->Sumw2();
    /*ring_histo_pos[i]->Sumw2();
    ring_histo_neg[i]->Sumw2();
    ring_histo_pos_tof[i]->Sumw2();
    ring_histo_neg_tof[i]->Sumw2();*/
    //ring_histo[i]->Rebin(3);

    //Int_t binmin = ring_histo[i]->FindBin(0.3);
    //Int_t binmax = ring_histo[i]->FindBin(1.7);
    //ring_histo[i]->Scale(1./ring_histo[i]->Integral(binmin,binmax,"width"));
    //if(i>0 && i < nrings-1) ring_histo_tof[i]->Scale(1./ring_histo_tof[i]->Integral(binmin,binmax,"width"));

    ringfit[i] = new TF1(name,"pol1(0) + gaus(2)",0.2,1.7);
    ringfit[i]->SetParLimits(0,0,10.0*ring_histo[i]->GetBinContent(1)+10.);
    ringfit[i]->SetParLimits(1,-10000,0);
    ringfit[i]->SetParLimits(2,0,10.0*ring_histo[i]->GetMaximum());
    ringfit[i]->SetParLimits(3,0,10);
    ringfit[i]->SetParameter(0,ring_histo[i]->GetBinContent(1));
    ringfit[i]->SetParameter(1,-ring_histo[i]->GetBinContent(1)/6.0);
    ringfit[i]->SetParameter(2,ring_histo[i]->GetMaximum());
    ringfit[i]->SetParameter(3,0.95);
    ringfit[i]->SetParameter(4,0.15);
    ringfit[i]->SetParNames("constant1","Slope","constant2","Mean","Sigma");
 
    ringfit[i]->SetLineColor(kBlack);
    ringfit[i]->SetLineWidth(0.6);
    
    ring_histo[i]->Fit(ringfit[i],"rql0","",0.2,1.7);
    ring_histo[i]->DrawCopy();
    ringfit[i]->DrawCopy("same");

    par3[i] = ringfit[i]->GetParameter(3);
    par3err[i] = ringfit[i]->GetParError(3);

    ringprec->SetBinContent(i+1,(ringfit[i]->GetParameter(3)));
    ringprec->SetBinError(i+1,ringfit[i]->GetParameter(4));
    ringprec2->SetBinContent(i+1,(ringfit[i]->GetParameter(3)));
    ringprec2->SetBinError(i+1,ringfit[i]->GetParError(3));
    //ew[i] = 4066/(60*(fit[i]->GetParameter(2))*(fit[i]->GetParameter(2)));

    float mean = ringfit[i]->GetParameter(3);
    float merr = ringfit[i]->GetParError(3);
    cout<<"ring "<<i<<" "<<mean<<" "<<merr/mean<<" "<<ring_histo[i]->GetEntries()<<endl;

    hPed_ring->SetBinContent(i+1,ringfit[i]->GetParameter(0)); hPed_ring->SetBinError(i+1,ringfit[i]->GetParError(0));
    hSlope_ring->SetBinContent(i+1,ringfit[i]->GetParameter(1)); hSlope_ring->SetBinError(i+1,ringfit[i]->GetParError(1));
    hPeaks_ring->SetBinContent(i+1,ringfit[i]->GetParameter(2)); hPeaks_ring->SetBinError(i+1,ringfit[i]->GetParError(2));
    hMean_ring->SetBinContent(i+1,ringfit[i]->GetParameter(3)); hMean_ring->SetBinError(i+1,ringfit[i]->GetParError(3));
    hSigma_ring->SetBinContent(i+1,ringfit[i]->GetParameter(4)); hSigma_ring->SetBinError(i+1,ringfit[i]->GetParError(4));
    hChi2_ring->SetBinContent(i+1,ringfit[i]->GetChisquare()/(Double_t)ringfit[i]->GetNDF());
    
    if(i>0 && i < nrings-1)
      {
	sprintf(name,"ring_fit_tof_%i",i);
	ringfittof[i] = new TF1(name,"pol1(0) + gaus(2)",0.3,1.7);
	ringfittof[i]->SetParLimits(0,0,10.0*ring_histo_tof[i]->GetBinContent(1)+10.);
	ringfittof[i]->SetParLimits(1,-10000,10000);
	ringfittof[i]->SetParLimits(2,0,10.0*ring_histo_tof[i]->GetMaximum());
	ringfittof[i]->SetParLimits(3,0,10);
	ringfittof[i]->SetParameter(0,ring_histo_tof[i]->GetBinContent(1));
	ringfittof[i]->SetParameter(1,-ring_histo_tof[i]->GetBinContent(1)/6.0);
	ringfittof[i]->SetParameter(2,ring_histo_tof[i]->GetMaximum());
	ringfittof[i]->SetParameter(3,0.95);
	ringfittof[i]->SetParameter(4,0.15);

	ring_histo_tof[i]->SetLineColor(kViolet);
	ringfittof[i]->SetLineColor(kViolet);
	ringfittof[i]->SetLineWidth(0.6);
	ring_histo_tof[i]->Fit(ringfittof[i],"rql0","",0.2,1.7);
	ring_histo_tof[i]->DrawCopy("same");
	ringfittof[i]->DrawCopy("same");

	par3_tof[i] = ringfittof[i]->GetParameter(3);
	par3err_tof[i] = ringfittof[i]->GetParError(3);

	ringprec_tof->SetBinContent(i+1,ringfittof[i]->GetParameter(3));
	ringprec_tof->SetBinError(i+1,ringfittof[i]->GetParameter(4));
	ringprec2_tof->SetBinContent(i+1,ringfittof[i]->GetParameter(3));
	ringprec2_tof->SetBinError(i+1,ringfittof[i]->GetParError(3));
	
	hPed_ring_tof->SetBinContent(i+1,ringfittof[i]->GetParameter(0)); hPed_ring_tof->SetBinError(i+1,ringfittof[i]->GetParError(0));
	hSlope_ring_tof->SetBinContent(i+1,ringfittof[i]->GetParameter(1)); hSlope_ring_tof->SetBinError(i+1,ringfittof[i]->GetParError(1));
	hPeaks_ring_tof->SetBinContent(i+1,ringfittof[i]->GetParameter(2)); hPeaks_ring_tof->SetBinError(i+1,ringfittof[i]->GetParError(2));
	hMean_ring_tof->SetBinContent(i+1,ringfittof[i]->GetParameter(3)); hMean_ring_tof->SetBinError(i+1,ringfittof[i]->GetParError(3));
	hSigma_ring_tof->SetBinContent(i+1,ringfittof[i]->GetParameter(4)); hSigma_ring_tof->SetBinError(i+1,ringfittof[i]->GetParError(4));
	hChi2_ring_tof->SetBinContent(i+1,ringfittof[i]->GetChisquare()/(Double_t)ringfittof[i]->GetNDF());
      }
    else
      {
	par3_tof[i] = par3[i];
	par3err_tof[i] = par3err[i];
	ringfittof[i] = 0;
      }

    /*for(Int_t k = 0; k < npbins; k++)
      {
	TH1F* ptemp = (TH1F*)ringp_histo[i]->ProjectionX("ptemp",pbins[k],pbins[k+1]-1);
	ringfit->SetParLimits(0,0,10.0*ptemp->GetBinContent(1)+10.);
	ringfit->SetParLimits(1,-10000,10000);
	ringfit->SetParLimits(2,0,10.0*ptemp->GetMaximum());
	ringfit->SetParLimits(3,0,10);
	ringfit->SetParameter(0,ptemp->GetBinContent(1));
	ringfit->SetParameter(1,ptemp->GetBinContent(1)/6.0);
	ringfit->SetParameter(2,ptemp->GetMaximum());
	ringfit->SetParameter(3,0.95);
	ringfit->SetParameter(4,0.15);
	ptemp->Fit(ringfit,"rql0","",0.3,1.7);

	Double_t minp = ringp_histo[i]->GetYaxis()->GetBinLowEdge(pbins[k]);
	Double_t maxp = ringp_histo[i]->GetYaxis()->GetBinUpEdge(pbins[k+1]-1);
	gr[i]->SetPoint(k,(maxp+minp)*0.5,ringfit->GetParameter(3));
	gr[i]->SetPointError(k,(maxp-minp)*0.5,ringfit->GetParError(3));

	if(i>0 && i < nrings-1)
	  {
	    ptemp = (TH1F*)ringp_histo_tof[i]->ProjectionX("ptemp",pbins[k],pbins[k+1]-1);
	    ringfit->SetParLimits(0,0,10.0*ptemp->GetBinContent(1)+10.);
	    ringfit->SetParLimits(1,-10000,10000);
	    ringfit->SetParLimits(2,0,10.0*ptemp->GetMaximum());
	    ringfit->SetParLimits(3,0,10);
	    ringfit->SetParameter(0,ptemp->GetBinContent(1));
	    ringfit->SetParameter(1,ptemp->GetBinContent(1)/6.0);
	    ringfit->SetParameter(2,ptemp->GetMaximum());
	    ringfit->SetParameter(3,0.95);
	    ringfit->SetParameter(4,0.15);
	    ptemp->Fit(ringfit,"rql0","",0.3,1.7);

	    minp = ringp_histo_tof[i]->GetYaxis()->GetBinLowEdge(pbins[k]);
	    maxp = ringp_histo_tof[i]->GetYaxis()->GetBinUpEdge(pbins[k+1]-1);
	    grtof[i]->SetPoint(k,(maxp+minp)*0.5,ringfit->GetParameter(3));
	    grtof[i]->SetPointError(k,(maxp-minp)*0.5,ringfit->GetParError(3));
	  }
      }
    c->cd(2);
    if(i==0 || i==nrings-1) htemp->SetMinimum(0.3);
    else htemp->SetMinimum(0.8);
    htemp->DrawCopy();
    gr[i]->Draw("P");
    if(i>0 && i < nrings-1)
      {
	grtof[i]->SetMarkerColor(kViolet);
	grtof[i]->SetLineColor(kViolet);
	grtof[i]->Draw("P");
	}*/

    /*c->cd(2);
    ring_histo_pos[i]->SetLineColor(kRed);
    ringfit->SetLineColor(kRed);
    ring_histo_pos[i]->Fit(ringfit,"rql0","",0.3,1.7);
    ring_histo_pos[i]->DrawCopy("same");
    ringfit->DrawCopy("same");
    ringprec_pos->SetBinContent(i+1,ringfit->GetParameter(3));
    ringprec_pos->SetBinError(i+1,ringfit->GetParameter(4));
    ringprec2_pos->SetBinContent(i+1,ringfit->GetParameter(3));
    ringprec2_pos->SetBinError(i+1,ringfit->GetParError(3));
    ring_histo_neg[i]->SetLineColor(kBlue);
    ringfit->SetLineColor(kBlue);
    ring_histo_neg[i]->Fit(ringfit,"rql0","",0.3,1.7);
    ring_histo_neg[i]->DrawCopy("same");
    ringfit->DrawCopy("same");
    ringprec_neg->SetBinContent(i+1,ringfit->GetParameter(3));
    ringprec_neg->SetBinError(i+1,ringfit->GetParameter(4));
    ringprec2_neg->SetBinContent(i+1,ringfit->GetParameter(3));
    ringprec2_neg->SetBinError(i+1,ringfit->GetParError(3));
    if(i>1 && i < nrings-1)
      {
	ring_histo_pos_tof[i]->SetLineColor(kMagenta);
	ringfit->SetLineColor(kMagenta);
	ring_histo_pos_tof[i]->Fit(ringfit,"rql0","",0.3,1.7);
	ring_histo_pos_tof[i]->DrawCopy("same");
	ringfit->DrawCopy("same");
	ringprec_pos_tof->SetBinContent(i+1,ringfit->GetParameter(3));
	ringprec_pos_tof->SetBinError(i+1,ringfit->GetParameter(4));
	ringprec2_pos_tof->SetBinContent(i+1,ringfit->GetParameter(3));
	ringprec2_pos_tof->SetBinError(i+1,ringfit->GetParError(3));
	ring_histo_neg_tof[i]->SetLineColor(kCyan);
	ringfit->SetLineColor(kCyan);
	ring_histo_neg_tof[i]->Fit(ringfit,"rql0","",0.3,1.7);
	ring_histo_neg_tof[i]->DrawCopy("same");
	ringfit->DrawCopy("same");
	ringprec_neg_tof->SetBinContent(i+1,ringfit->GetParameter(3));
	ringprec_neg_tof->SetBinError(i+1,ringfit->GetParameter(4));
	ringprec2_neg_tof->SetBinContent(i+1,ringfit->GetParameter(3));
	ringprec2_neg_tof->SetBinError(i+1,ringfit->GetParError(3));
	}*/

  }

  /*TF1 *ring2fit = new TF1("ring2fit","pol1(0) + gaus(2)",0.3,1.7);

  for(int i = 0; i < nrings/2; i++){

    if(i%2==0)
      {
	c->Update();
	ps->NewPage();
	c->Clear();
	c->Divide(1,2);
      }
    c->cd((i%2)+1);

    ring2_histo[i]->Add(ring_histo[2*i]);
    ring2_histo[i]->Add(ring_histo[2*i+1]);

    ring2_histo_tof[i]->Add(ring_histo_tof[2*i]);
    ring2_histo_tof[i]->Add(ring_histo_tof[2*i+1]);

    //sprintf(name,"ring2_fit_%i",i);
    //ring2fit = new TF1(name,"pol1(0) + gaus(2)",0.3,1.7);

    //ring2_histo[i]->Rebin(3);

    ring2fit->SetParLimits(0,0,10.0*ring2_histo[i]->GetBinContent(1));
    ring2fit->SetParLimits(1,-10000,0);
    ring2fit->SetParLimits(2,0,10.0*ring2_histo[i]->GetMaximum());
    ring2fit->SetParLimits(3,0,10);
    ring2fit->SetParLimits(4,0.17,0.175);
    ring2fit->SetParameter(0,ring2_histo[i]->GetBinContent(1));
    ring2fit->SetParameter(1,-ring2_histo[i]->GetBinContent(1)/6.0);
    ring2fit->SetParameter(2,ring2_histo[i]->GetMaximum());
    ring2fit->SetParameter(3,0.95);
    ring2fit->SetParameter(4,0.11245);
    ring2fit->SetParNames("constant1","Slope","constant2","Mean","Sigma");

    ring2_histo[i]->Fit(ring2fit,"rql0","",0.3,1.7);
    ring2_histo[i]->DrawCopy();
    ring2fit->SetLineColor(kBlack);
    ring2fit->DrawCopy("same");

    ring2prec->SetBinContent(i+1,(ring2fit->GetParameter(3)));
    ring2prec->SetBinError(i+1,ring2fit->GetParameter(4));
    ring2prec2->SetBinContent(i+1,(ring2fit->GetParameter(3)));
    ring2prec2->SetBinError(i+1,ring2fit->GetParError(3));

    cout<<"ring2 "<<i<<" "<<ring2fit->GetParameter(3)<<" "<<ring2fit->GetParError(3)<<endl;

    ring2fit->SetParLimits(0,0,10.0*ring2_histo_tof[i]->GetBinContent(1));
    ring2fit->SetParLimits(1,-10000,0);
    ring2fit->SetParLimits(2,0,10.0*ring2_histo_tof[i]->GetMaximum());
    ring2fit->SetParLimits(3,0,10);
    ring2fit->SetParLimits(4,0.17,0.175);
    ring2fit->SetParameter(0,ring2_histo_tof[i]->GetBinContent(1));
    ring2fit->SetParameter(1,-ring2_histo_tof[i]->GetBinContent(1)/6.0);
    ring2fit->SetParameter(2,ring2_histo_tof[i]->GetMaximum());
    ring2fit->SetParameter(3,0.95);
    ring2fit->SetParameter(4,0.11245);

    ring2_histo_tof[i]->Fit(ring2fit,"rql0","",0.3,1.7);
    ring2_histo_tof[i]->SetLineColor(kViolet);
    ring2_histo_tof[i]->DrawCopy("same");
    ring2fit->SetLineColor(kViolet);
    ring2fit->DrawCopy("same");
  }

  c->Update();
  //ps->Close();
  */


  //**********************************************//
  //Fit Crate Slice Histograms                    //
  //**********************************************//

  ofstream fitfile(ffile);

  for(int i = 0; i < ncrates; i++){
    for(int j = 0; j < nrings/2; j++){
      if(j == 0)
	{
	  c->Update();
	  ps->NewPage();
	  c->Clear();
	  c->Divide(4,5);
	}
      c->cd(j+1);
      
      sprintf(name,"crateslice_fit_%i_%i",i,j);
      crateslice_histo[i][j]->Sumw2();
      crateslice_histo[i][j]->Rebin(2);
      
      crateslicefit[i][j] = new TF1(name,"pol1(0) + gaus(2)",0.25,1.6);
      //crateslicefit[i][j]->SetParLimits(0,0,10.0*crateslice_histo[i][j]->GetBinContent(1)+10.);
      crateslicefit[i][j]->SetParLimits(1,-10000,0);
      crateslicefit[i][j]->SetParLimits(2,0,10.0*crateslice_histo[i][j]->GetMaximum());
      crateslicefit[i][j]->SetParLimits(3,0,10);
      crateslicefit[i][j]->SetParameter(0,crateslice_histo[i][j]->GetBinContent(2));
      crateslicefit[i][j]->SetParameter(1,-crateslice_histo[i][j]->GetBinContent(2)/3.0);
      crateslicefit[i][j]->SetParameter(2,crateslice_histo[i][j]->GetMaximum());
      crateslicefit[i][j]->SetParameter(3,0.96134);
      crateslicefit[i][j]->SetParameter(4,0.141123);
      crateslicefit[i][j]->SetParNames("constant1","Slope","constant2","Mean","Sigma");
      crateslicefit[i][j]->SetLineColor(kBlack);
      crateslicefit[i][j]->SetLineWidth(0.6);
      
      
      crateslice_histo[i][j]->Fit(crateslicefit[i][j],"rql0ww","",0.25,1.5);
      
      crateslice_histo[i][j]->DrawCopy();
      crateslicefit[i][j]->DrawCopy("same");
      
      float mean = crateslicefit[i][j]->GetParameter(3);
      float merr = crateslicefit[i][j]->GetParError(3);
      cratesliceprec->SetBinContent((i-1)*20+j+1,mean);
      cratesliceprec->SetBinError((i-1)*20+j+1,merr);
      //cout<<"crate "<<i+1<<" "<<mean<<" "<<merr/mean<<endl;

      fitfile << i*20+j << " " << mean << endl;
      
      sprintf(name,"crateslicefittof_%i_%i",i,j);
      crateslice_histo_tof[i][j]->Sumw2();
      crateslice_histo_tof[i][j]->Rebin(2);
      crateslice_histo_tof[i][j]->SetLineColor(kViolet);
      
      crateslicefittof[i][j] = new TF1(name,"pol1(0) + gaus(2)",0.25,1.5);
      //crateslicefittof[i][j]->SetParLimits(0,0,10.0*crateslice_histo_tof[i][j]->GetBinContent(1)+10.);
      crateslicefittof[i][j]->SetParLimits(1,-10000,0);
      crateslicefittof[i][j]->SetParLimits(2,0,10.0*crateslice_histo_tof[i][j]->GetMaximum());
      crateslicefittof[i][j]->SetParLimits(3,0,10);
      crateslicefittof[i][j]->SetParameter(0,crateslice_histo_tof[i][j]->GetBinContent(2));
      crateslicefittof[i][j]->SetParameter(1,-crateslice_histo_tof[i][j]->GetBinContent(2)/3.0);
      crateslicefittof[i][j]->SetParameter(2,crateslice_histo_tof[i][j]->GetMaximum());
      crateslicefittof[i][j]->SetParameter(3,0.96134);
      crateslicefittof[i][j]->SetParameter(4,0.141123);
      crateslicefittof[i][j]->SetParNames("constant1","Slope","constant2","Mean","Sigma");
      crateslicefittof[i][j]->SetLineColor(kViolet);
      crateslicefittof[i][j]->SetLineWidth(0.6);
      
      
      crateslice_histo_tof[i][j]->Fit(crateslicefittof[i][j],"rql0","",0.2,1.7);
      
      crateslice_histo_tof[i][j]->DrawCopy("same");
      crateslicefittof[i][j]->DrawCopy("same");
    
    }
  }
  



  ofstream newgain(ngname);

  TH2F *hNewGains = new TH2F("hNewGains","New Gains",120,-1*TMath::Pi(),TMath::Pi(),40,-1,1); hNewGains->SetXTitle("phi");hNewGains->SetYTitle("eta");

  float gains2[ntowers];
  float gerr2[ntowers];
  for(int i = 0; i < ntowers; i++){
    float phi = helper->getPhi(i+1);
    float eta = helper->getEta(i+1);
    int crate, sequence;
    decoder->GetCrateFromTowerId(i+1,crate,sequence);
    geantetaindex = ((TMath::Nint(fabs(eta) * 1000.0) + 25)/50 - 1);
    if(TMath::Abs(eta) > 0.968) eta += 0.005 * TMath::Abs(eta)/eta;
    etaindex = ((TMath::Nint(eta * 1000.0) + 25)/50 + 19);
    //float adjust = ring2fit[(int)etaindex/2]->GetParameter(3);
    //float adjust = par3[etaindex];
    //cout<<etaindex<<" "<<(int)etaindex/2<<" "<<adjust<<endl;
    float ng = 0;
    float ne = 0;
    if(status[i] == 1){
      float og = gains[i];
      float adjust = crateslicefit[crate-1][geantetaindex]->GetParameter(3);
      float aerr = crateslicefit[crate-1][geantetaindex]->GetParError(3);
      cout << "Adjust: " << adjust << "   " << aerr << endl;
      hChi2EtaPhi->Fill(eta,phi,crateslicefit[crate-1][geantetaindex]->GetChisquare()/(Double_t)crateslicefit[crate-1][geantetaindex]->GetNDF());
      if(geantetaindex == 19)
	{
	  adjust = ringfit[etaindex]->GetParameter(3);
	  aerr = ringfit[etaindex]->GetParError(3);
	  hChi2EtaPhi->Fill(eta,phi,ringfit[etaindex]->GetChisquare()/(Double_t)ringfit[etaindex]->GetNDF());
	}
      ng = og/adjust;
      //float aerr = ring2fit[(int)etaindex/2]->GetParError(3);
      //float aerr = par3err[etaindex];
      float gerr = peakerr[i]*gains[i]/peaks[i];
      ne = sqrt(pow(og*aerr/(adjust*adjust),2) + pow(gerr/adjust,2));
    }
    newgain << i+1 << " " << ng << " " << ne << " " << status[i] << endl;
    cout << i+1 << " " << ng << " " << ne << " " << status[i] << endl;
    gains2[i] = ng;
    gerr2[i] = ne;
    if(status[i]==1)
      hNewGains->Fill(phi,eta,ng);

  }

  newgain.close();

  //**********************************************//
  //Fit Crate Histograms                           //
  //**********************************************//
  /*
  TF1 *crate_fit = new TF1("crate_fit","pol1(0) + gaus(2)",0.3,1.7);

  for(int i = 0; i < ncrates; i++){

    if(i%2==0)
      {
	c->Update();
	ps->NewPage();
	c->Clear();
	c->Divide(1,2);
      }
    c->cd((i%2)+1);

    //sprintf(name,"crate_fit_%i",i);
    crate_histo[i]->Sumw2();
    crate_histo_tof[i]->Sumw2();
    //crate_histo[i]->Rebin(4);

    //Int_t binmin = crate_histo[i]->FindBin(0.3);
    //Int_t binmax = crate_histo[i]->FindBin(1.7);
    //crate_histo[i]->Scale(1./crate_histo[i]->Integral(binmin,binmax,"width"));
    //if(i>0 && i < nrings-1) crate_histo_tof[i]->Scale(1./crate_histo_tof[i]->Integral(binmin,binmax,"width"));

    //crate_fit = new TF1(name,"pol1(0) + gaus(2)",0.3,1.7);
    crate_fit->SetParLimits(0,0,10.0*crate_histo[i]->GetBinContent(1)+10.);
    crate_fit->SetParLimits(1,-10000,0);
    crate_fit->SetParLimits(2,0,10.0*crate_histo[i]->GetMaximum());
    crate_fit->SetParLimits(3,0,10);
    crate_fit->SetParameter(0,crate_histo[i]->GetBinContent(1));
    crate_fit->SetParameter(1,-crate_histo[i]->GetBinContent(1)/6.0);
    crate_fit->SetParameter(2,crate_histo[i]->GetMaximum());
    crate_fit->SetParameter(3,0.929);
    crate_fit->SetParameter(4,0.156);
    crate_fit->SetParNames("constant1","Slope","constant2","Mean","Sigma");
    crate_fit->SetLineColor(kBlack);
    crate_fit->SetLineWidth(0.6);

    crate_histo[i]->DrawCopy();
    crate_histo[i]->Fit(crate_fit,"rql0","",0.3,1.8);
    crate_fit->DrawCopy("same");

    float mean = crate_fit->GetParameter(3);
    float merr = crate_fit->GetParError(3);
    crateprec->SetBinContent(i+1,mean);
    crateprec->SetBinError(i+1,merr);

    hPed_crate->SetBinContent(i+1,crate_fit->GetParameter(0)); hPed_crate->SetBinError(i+1,crate_fit->GetParError(0));
    hSlope_crate->SetBinContent(i+1,crate_fit->GetParameter(1)); hSlope_crate->SetBinError(i+1,crate_fit->GetParError(1));
    hPeaks_crate->SetBinContent(i+1,crate_fit->GetParameter(2)); hPeaks_crate->SetBinError(i+1,crate_fit->GetParError(2));
    hMean_crate->SetBinContent(i+1,crate_fit->GetParameter(3)); hMean_crate->SetBinError(i+1,crate_fit->GetParError(3));
    hSigma_crate->SetBinContent(i+1,crate_fit->GetParameter(4)); hSigma_crate->SetBinError(i+1,crate_fit->GetParError(4));
    hChi2_crate->SetBinContent(i+1,crate_fit->GetChisquare()/(Double_t)crate_fit->GetNDF());
    
    
    crate_fit->SetParLimits(0,0,10.0*crate_histo_tof[i]->GetBinContent(1)+10.);
    crate_fit->SetParLimits(1,-10000,0);
    crate_fit->SetParLimits(2,0,10.0*crate_histo_tof[i]->GetMaximum());
    crate_fit->SetParLimits(3,0,10);
    crate_fit->SetParameter(0,crate_histo_tof[i]->GetBinContent(1));
    crate_fit->SetParameter(1,-crate_histo_tof[i]->GetBinContent(1)/6.0);
    crate_fit->SetParameter(2,crate_histo_tof[i]->GetMaximum());
    crate_fit->SetParameter(3,0.929);
    crate_fit->SetParameter(4,0.156);
    crate_histo_tof[i]->SetLineColor(kViolet);

    crate_histo_tof[i]->DrawCopy("same");
    crate_histo_tof[i]->Fit(crate_fit,"rql0","",0.3,1.8);
    crate_fit->SetLineColor(kViolet);
    crate_fit->DrawCopy("same");

    crateprec_tof->SetBinContent(i+1,crate_fit->GetParameter(3));
    crateprec_tof->SetBinError(i+1,crate_fit->GetParError(3));
    //cout<<"crate "<<i+1<<" "<<mean<<" "<<merr/mean<<endl;

    hPed_crate_tof->SetBinContent(i+1,crate_fit->GetParameter(0)); hPed_crate_tof->SetBinError(i+1,crate_fit->GetParError(0));
    hSlope_crate_tof->SetBinContent(i+1,crate_fit->GetParameter(1)); hSlope_crate_tof->SetBinError(i+1,crate_fit->GetParError(1));
    hPeaks_crate_tof->SetBinContent(i+1,crate_fit->GetParameter(2)); hPeaks_crate_tof->SetBinError(i+1,crate_fit->GetParError(2));
    hMean_crate_tof->SetBinContent(i+1,crate_fit->GetParameter(3)); hMean_crate_tof->SetBinError(i+1,crate_fit->GetParError(3));
    hSigma_crate_tof->SetBinContent(i+1,crate_fit->GetParameter(4)); hSigma_crate_tof->SetBinError(i+1,crate_fit->GetParError(4));
    hChi2_crate_tof->SetBinContent(i+1,crate_fit->GetChisquare()/(Double_t)crate_fit->GetNDF());
  }
  
*/

  /*
   ///////////////////////////////////////
   //Using new gains regenerate by crate//
  ///////////////////////////////////////
  ofstream newgain(ngname);

  float gains3[ntowers];
  float gerr3[ntowers];
  for(int i = 0; i < ntowers; i++){
    float eta = helper->getEta(i+1);
    float phi = helper->getPhi(i+1);
    int crate = lookup_crate(eta,phi);
    float adjust = crate_fit[crate-1]->GetParameter(3);
    float og = bemctables->calib(1,i)*gains[i]*gains2[i];
    float ng = og;
    float aerr = crate_fit[crate-1]->GetParError(3);
    float ne = sqrt(pow(gains[i]*gerr2[i],2) + pow(gains2[i]*gainerr[i],2));
    if(fabs(adjust-1)/aerr > 1.5){
      ne = sqrt(pow(ne/(adjust),2)+pow(og*aerr/(adjust*adjust),2));
      ng /= adjust;
    }
    newgain << i+1 << " " << ng << " " << ne << " " << status[i] << endl;
    gains3[i] = ng;
    gerr3[i] = ne;
  }

  newgain.close();
  */
  
  c->Update();
  ps->NewPage();
  c->Clear();
  c->Divide(1,2);
  c->cd(1);
  ringprec->SetMaximum(1.25);
  ringprec->SetMinimum(0.75);
  ringprec->Draw();
  ringprec_tof->SetLineColor(kViolet);
  ringprec_tof->Draw("same");
  /*ringprec_pos->SetLineColor(kRed);
    ringprec_pos->Draw("same");
    ringprec_neg->SetLineColor(kBlue);
    ringprec_neg->Draw("same");
    ringprec_pos_tof->SetLineColor(kMagenta);
    ringprec_pos_tof->Draw("same");
    ringprec_neg_tof->SetLineColor(kCyan);
    ringprec_neg_tof->Draw("same");*/
  c->cd(2);
  ringprec2->SetMaximum(1.25);
  ringprec2->SetMinimum(0.75);
  ringprec2->Draw();
  ringprec2_tof->SetLineColor(kViolet);
  ringprec2_tof->Draw("same");
  /*ringprec2_pos->SetLineColor(kRed);
    ringprec2_pos->Draw("same");
    ringprec2_neg->SetLineColor(kBlue);
    ringprec2_neg->Draw("same");
    ringprec2_pos_tof->SetLineColor(kMagenta);
    ringprec2_pos_tof->Draw("same");
    ringprec2_neg_tof->SetLineColor(kCyan);
    ringprec2_neg_tof->Draw("same");*/
  /*c->Update();
  ps->NewPage();
  c->Clear();
  c->Divide(1,2);
  c->cd(1);
  crateprec->SetMaximum(1.25);
  crateprec->SetMinimum(0.75);
  crateprec->Draw();
  crateprec_tof->SetLineColor(kViolet);
  crateprec_tof->Draw("same");
  c->Update();
  ps->NewPage();
  c->Clear();
  c->Divide(2,3);
  c->cd(1);
  hPed_ring->Draw();
  hPed_ring_tof->SetLineColor(kViolet);
  hPed_ring_tof->Draw("same");
  c->cd(1);
  hPed_ring->Draw();
  hPed_ring_tof->SetLineColor(kViolet);
  hPed_ring_tof->Draw("same");
  c->cd(2);
  hSlope_ring->Draw();
  hSlope_ring_tof->SetLineColor(kViolet);
  hSlope_ring_tof->Draw("same");
  c->cd(3);
  hPeaks_ring->Draw();
  hPeaks_ring_tof->SetLineColor(kViolet);
  hPeaks_ring_tof->Draw("same");
  c->cd(4);
  hMean_ring->Draw();
  hMean_ring_tof->SetLineColor(kViolet);
  hMean_ring_tof->Draw("same");
  c->cd(5);
  hSigma_ring->Draw();
  hSigma_ring_tof->SetLineColor(kViolet);
  hSigma_ring_tof->Draw("same");
  c->cd(6);
  hChi2_ring->Draw();
  hChi2_ring_tof->SetLineColor(kViolet);
  hChi2_ring_tof->Draw("same");
  c->Update();
  ps->NewPage();
  c->Clear();
  c->Divide(2,3);
  c->cd(1);
  hPed_crate->Draw();
  hPed_crate_tof->SetLineColor(kViolet);
  hPed_crate_tof->Draw("same");
  c->cd(1);
  hPed_crate->Draw();
  hPed_crate_tof->SetLineColor(kViolet);
  hPed_crate_tof->Draw("same");
  c->cd(2);
  hSlope_crate->Draw();
  hSlope_crate_tof->SetLineColor(kViolet);
  hSlope_crate_tof->Draw("same");
  c->cd(3);
  hPeaks_crate->Draw();
  hPeaks_crate_tof->SetLineColor(kViolet);
  hPeaks_crate_tof->Draw("same");
  c->cd(4);
  hMean_crate->Draw();
  hMean_crate_tof->SetLineColor(kViolet);
  hMean_crate_tof->Draw("same");
  c->cd(5);
  hSigma_crate->Draw();
  hSigma_crate_tof->SetLineColor(kViolet);
  hSigma_crate_tof->Draw("same");
  c->cd(6);
  hChi2_crate->Draw();
  hChi2_crate_tof->SetLineColor(kViolet);
  hChi2_crate_tof->Draw("same");
  c->Update();*/

  c->Update();
  ps->NewPage();
  c->Clear();
  c->Divide(1,2);
  c->cd(1);
  hNewGains->SetMaximum(0.1);
  hNewGains->SetMinimum(0);
  hNewGains->Draw("colz");

  ps->Close();

  TCanvas *c3a = new TCanvas();
  c3a->cd();
  hNewGains->Draw("colz");

  TCanvas *c4 = new TCanvas("c4","",100,100,600.,800.);
  c4->Divide(1,2);
  c4->cd(1);
  gPad->SetLogy();
  pfinal->Draw();
  pfinal_tof->SetLineColor(kViolet);
  pfinal_tof->Draw("same");
  c4->cd(2);
  etafinal->Draw();
  etafinal_tof->SetLineColor(kViolet);
  etafinal_tof->Draw("same");

  TCanvas *c5 = new TCanvas("c5","",700,1200);
  gPad->SetRightMargin(0.16);
  hChi2EtaPhi->DrawCopy("colz");

}

//constrained double-Gaussian to fit the background
double background_only_fit(double *x, double *par){
  double par3 = par[0]/1.5;
  double par4 = par[1] + 10.;
  double par5 = par[2] * 6.5;
	
  double fitval = 0;
  if(par[2] != 0){
    double arg1 = (x[0]-par[1])/par[2];
    double arg2 = (x[0]-par4)/par5;
    fitval = par[0]*TMath::Exp(-0.5*arg1*arg1) + par3*TMath::Exp(-0.5*arg2*arg2);
  }
	
  return fitval;
}

double fit_function(double *x, double *par){
	//6-parameter fit includes
	//3 param electron Gaussian
	//par3 = relative height of peak/bg ~ 10
	//par4 = mean of main bg Gaussian ~ 10
	//par5 = width of main bg Gaussian ~ 3
	
  double par3 = 0;
  if(par[3] != 0)par3 = par[0] / par[3];
  double par6 = par3/1.5;
  double par7 = par[4] + 10.;
  double par8 = par[5] * 6.5;
	
  double fitval = 0;
  if(par[2] != 0 && par[5] != 0){
    double arg1 = (x[0]-par[1])/par[2];
    double arg2 = (x[0]-par[4])/par[5];
    double arg3 = (x[0]-par7)/par8;
    fitval = par[0]*TMath::Exp(-0.5*arg1*arg1) + par3*TMath::Exp(-0.5*arg2*arg2) + par6*TMath::Exp(-0.5*arg3*arg3);
  }
	
  return fitval;
}

double fit_function2(double *x, double *par){
	//6-parameter fit includes
	//3 param electron Gaussian
	//par3 = relative height of peak/bg ~ 10
	//par4 = mean of main bg Gaussian ~ 10
	//par5 = width of main bg Gaussian ~ 3
	
  double par3 = 0;
  if(par[3] != 0)par3 = par[0] / par[3];
  double par6 = par[6];
  double par7 = par[4] + 10.;
  double par8 = par[7];
	
  double fitval = 0;
  if(par[2] != 0 && par[5] != 0 && par8 != 0){
    double arg1 = (x[0]-par[1])/par[2];
    double arg2 = (x[0]-par[4])/par[5];
    double arg3 = (x[0])/par8;
    fitval = par[0]*TMath::Exp(-0.5*arg1*arg1) + par3*TMath::Exp(-0.5*arg2*arg2) + par6*TMath::Exp(-arg3);
  }
  return fitval;
}

int lookup_crate(float eta,float phi)
{
    if (eta < 0){
      if (phi < -2.72) return 5;
      else if (phi < -2.30) return 6;
      else if (phi < -1.88) return 7;
      else if (phi < -1.46) return 8;
      else if( phi < -1.04) return 9;
      else if( phi < -0.62) return 10;
      else if( phi < -0.20) return 11;
      else if( phi <  0.22) return 12;
      else if( phi <  0.64) return 13;
      else if( phi <  1.06) return 14;
      else if( phi <  1.48) return 15;
      else if( phi <  1.90) return 1;
      else if( phi <  2.32) return 2;
      else if( phi <  2.74) return 3;
      else             return 4;
    }else{
      if   (phi < -2.72) return 20;
      else if( phi < -2.30) return 21;
      else if( phi < -1.88) return 22;
      else if( phi < -1.46) return 23;
      else if( phi < -1.04) return 24;
      else if( phi < -0.62) return 25;
      else if( phi < -0.20) return 26;
      else if( phi <  0.22) return 27;
      else if( phi <  0.64) return 28;
      else if( phi <  1.06) return 29;
      else if( phi <  1.48) return 30;
      else if( phi <  1.90) return 16;
      else if( phi <  2.32) return 17;
      else if( phi <  2.74) return 18;
      else             return 19;
    }
}

int lookup_crateslice(float eta, float phi) // 0-572
{

  if(eta < -0.95) return 0;
  if(eta > 0.95) return 571;

  int tempcrate = -1;  // 0-14
  if (phi < -2.72) tempcrate = 0;
  else if (phi < -2.30) tempcrate = 1;
  else if (phi < -1.88) tempcrate = 2;
  else if (phi < -1.46) tempcrate = 3;
  else if( phi < -1.04) tempcrate = 4;
  else if( phi < -0.62) tempcrate = 5;
  else if( phi < -0.20) tempcrate = 6;
  else if( phi <  0.22) tempcrate = 7;
  else if( phi <  0.64) tempcrate = 8;
  else if( phi <  1.06) tempcrate = 9;
  else if( phi <  1.48) tempcrate = 10;
  else if( phi <  1.90) tempcrate = 11;
  else if( phi <  2.32) tempcrate = 12;
  else if( phi <  2.74) tempcrate = 13;
  else                  tempcrate = 14;

  int tempeta = -1; // 0-37
  if(TMath::Abs(eta) > 0.968) eta += 0.005 * TMath::Abs(eta)/eta;
  tempeta = ((TMath::Nint(eta * 1000.0) + 25)/50 + 19);
  tempeta -= 1;

  if(tempeta == -1) cout << "Error: tempeta is -1!" << endl;

  return tempeta*15+tempcrate+1;

}

/*bool isBadTower2011(int id)
{
  switch(id)
    {
    case(240): case(266): case(431): case(484): case(504): case(539): case(616): case(633): 
    case(637): case(638): case(671): case(674): case(758): case(760): case(790): case(832): 
    case(873): case(1171): case(1187): case(1207): case(1219): case(1304): case(1312): 
    case(1397): case(1405): case(1427): case(1612): case(1654): case(1773): case(1976):
    case(2415): case(2590): case(2811): case(2834): case(2969): case(3071): case(3494): 
    case(3718): case(4057): case(4059): case(4223): 
      return true;
    default: 
      return false;
    }
    }*/


