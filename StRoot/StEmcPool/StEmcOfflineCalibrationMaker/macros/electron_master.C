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

void electron_master(const char* file_list="electrons.list",const char* output="electronmaster.root", const char* dbDate="1999-01-01 00:09:00", const char* geantfile="geant_func.root"){
  //**********************************************//
  //Load Libraries                                //
  //**********************************************//
  gROOT->Macro("LoadLogger.C");
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
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

  gSystem->Load("StEmcOfflineCalibrationMaker");

  cout<<"input filelist: "<<file_list<<endl;
  cout<<"output:          "<<output<<endl;
  cout<<"db Date:        "<<dbDate<<endl;
  cout<<"geant curve:    "<<geantfile<<endl;


  //**********************************************//
  //Initialize Stuff                              //
  //**********************************************//

  CalibrationHelperFunctions *helper = new CalibrationHelperFunctions();
	
  StBemcTablesWriter *bemctables = new StBemcTablesWriter();
  bemctables->loadTables(dbDate,"sim");

  //chain all input files together
  char file[300];
  TChain* tree = new TChain("skimTree");
  ifstream filelist(file_list);
  while(1){
    filelist >> file;
    if(!filelist.good()) break;
    cout<<file<<endl;
    tree->Add(file);
  }

  const int ntowers = 4800;
  const int nrings = 40;

  TFile* geant_file = new TFile(geantfile,"READ");
  TF1* geant_fit = (TF1*)geant_file->Get("geant_fit");

  TFile outfile(output,"RECREATE");

  TH1 *electron_histo[ntowers];
  TF1 *fit[ntowers];
  TH1 *prs_histo[ntowers];
  TH1 *ring_histo[nrings];
  TF1 *ringfit[nrings];
  float eta;
  int etaindex;
  TH1F* ringprec = new TH1F("ringprec","",40,-1.0,1.0);
  ringprec->SetYTitle("E/p");
  ringprec->SetXTitle("#eta");
  double ew[nrings];

  TH2F *energyleak = new TH2F("energyleak","",20,0.0,0.03,20,0.0,1.0);
  TH2F *findbg = new TH2F("findbg","",20,0.0,0.03,30,0.0,5.0);
  TH1F *energymean = new TH1F("energymean","",20,0.0,0.03);
  TH1F *leakmean = new TH1F("leakmean","",20,0.0,0.03);
  energyleak->SetXTitle("#DeltaR");
  energyleak->SetYTitle("leaked energy / total energy");
  findbg->SetXTitle("#DeltaR");
  findbg->SetYTitle("Total energy / track momentum");
  TH2F *tevsp = new TH2F("tevsp","",20,0.0,15.0,20,0.0,15.0);
  TH1F *pmean = new TH1F("pmean","",20,0.0,15.0);
  tevsp->SetXTitle("track momentum (GeV)");
  tevsp->SetYTitle("Total Energy (GeV)");
  TH2F *tevspcent = new TH2F("tevspcent","",20,0.0,15.0,20,0.0,15.0);
  tevspcent->SetXTitle("track momentum (GeV)");
  tevspcent->SetYTitle("Energy in central tower (GeV)");
  TH1F *cmean = new TH1F("cmain","",20,0.0,15.0);
  TH2F *sistertracks = new TH2F("sistertracks","",20,0.0,8.0,20,0.0,8.0);
  sistertracks->SetXTitle("Track momentum (GeV)");
  sistertracks->SetYTitle("Neighbor momentum (GeV)");
  TH2F* dEdxvsp = new TH2F("dEdxvsp","",40,0.15,0.85,40,-5.5,-5.3);
  dEdxvsp->SetXTitle("Log(p)");
  dEdxvsp->SetYTitle("Log(dE/dx)");
  TH2F* energyleak2 = new TH2F("energyleak2","",20,0.0,0.03,20,0.0,1.0);
  TH1F* energymean2 = new TH1F("energymean2","",20,0.0,0.03);
  TH1F* towermult = new TH1F("towermult","",9,0.0,9.0);
  energyleak2->SetXTitle("#DeltaR");
  energyleak2->SetYTitle("leaked energy/total energy");
  towermult->SetXTitle("Neighbors with energy");
  TH2F* multvsp = new TH2F("multvsp","",20,0.0,20.0,9,0.0,9.0);
  multvsp->SetXTitle("Track momentum (GeV)");
  multvsp->SetYTitle("Neighbors with energy");
  TH1F* multmean = new TH1F("multmean","",20,0.0,20.0);
  TH2F* tep3 = new TH2F("tep3","2 < p < 3",20,0.0,0.03,20,0.0,4.0);
  TH2F* tep5 = new TH2F("tep5","3 < p < 5",20,0.0,0.03,20,0.0,4.0);
  TH2F* tep10 = new TH2F("tep10","5 < p < 10",20,0.0,0.03,20,0.0,4.0);
  tep3->SetXTitle("DeltaR");
  tep3->SetYTitle("Total energy / track momentum");
  tep5->SetXTitle("DeltaR");
  tep5->SetYTitle("Total energy / track momentum");
  tep10->SetXTitle("DeltaR");
  tep10->SetYTitle("Total energy / track momentum");
  TH1F* tep3mean = new TH1F("tep3mean","",20,0,0.03);
  TH1F* tep5mean = new TH1F("tep5mean","",20,0,0.03);
  TH1F* tep10mean = new TH1F("tep10mean","",20,0,0.03);
  TH1F* multen = new TH1F("multen","",40,0.0,1.0);
  multen->SetXTitle("Energy in neighbor towers (GeV)");
  TH1F* east_histo = new TH1F("east_histo","Electron E/p in East",40,0.0,2.0);
  TH1F* west_histo = new TH1F("west_histo","Electron E/p in West",40,0.0,2.0);
  TH1F* all_histo = new TH1F("all_histo","Electron E/p",40,0.0,2.0);

   //create the tower histograms
  char name[100];
  char name1[100];
  for(int i=0; i<ntowers; i++){
    sprintf(name,"electron_histo_%i",i+1);
    electron_histo[i] = new TH1D(name,"",30,0.,140.);
    electron_histo[i]->SetXTitle("#frac{ADC}{GeV * Sin(#theta)}");
    sprintf(name1,"prs_histo_%i",i+1);
    prs_histo[i] = new TH1D(name1,"",60,0.,500.);
    prs_histo[i]->SetXTitle("ADC");
  }
  char name2[100];
  for(int i=0; i<nrings;i++)
    {
      sprintf(name2,"ring_histo_%i",i);
      //ring_histo[i] = new TH1D(name2,"",30,0.,140.0);
      //ring_histo[i]->SetXTitle("ADC / GeV Sin(#theta)");
      ring_histo[i] = new TH1D(name2,"",50,0.,3.0);
      ring_histo[i]->SetXTitle("E/p");
    }

  //global graphics functions
  gStyle->SetOptStat("oue");
  gStyle->SetOptFit(111);
  gStyle->SetCanvasColor(10);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);
  gStyle->SetStatColor(0);

  StEmcOfflineCalibrationTrack* track = new StEmcOfflineCalibrationTrack();
  StEmcOfflineCalibrationCluster* cluster = new StEmcOfflineCalibrationCluster();
  tree->SetBranchAddress("clusters",&cluster);

  //**********************************************//
  //Loop Over Tracks, Fill Histograms             //
  //**********************************************//

  int nentries = tree->GetEntries();
  for(int j=0; j<nentries; j++){
    tree->GetEntry(j);
    track = &(cluster->centralTrack);
    TClonesArray *tracks = cluster->tracks;

    if((track->tower_adc[0] - track->tower_pedestal[0]) < 1.5 * track->tower_pedestal_rms[0])continue;
    if(track->p < 1.0)continue;
    if(j%200000 == 0) cout<<"reading "<<j<<" of "<<nentries<<endl;
					
    //change the fiducial cut to a square with diagonal = 0.06 in deta, dphi space
    float squarefid = 0.02;//0.03/TMath::Sqrt(2.0);
    //if(TMath::Abs(track->deta) > squarefid || TMath::Abs(track->dphi) > squarefid)continue;

    //calculate geant scaled, pedestal subtracted adc
    double dR = TMath::Sqrt(track->deta*track->deta + track->dphi*track->dphi);
    double geant_scale = geant_fit->Eval(dR);
    double scaled_adc = (track->tower_adc[0] - track->tower_pedestal[0]) * geant_scale / track->p;
    //cout<<scaled_adc<<endl;
    int index = track->tower_id[0];
    //cout<<index<<" "<<helper->getTheta(index)<<endl;
    //optionally scale by sin theta to put all peaks nominally in the same place = 4066/60 = 67
    //scaled_adc = scaled_adc/TMath::Sin(helper->getTheta(index));

    //fill tower and prs histo
    if(index <= ntowers)electron_histo[index - 1]->Fill(scaled_adc);
    if(index <= ntowers && (track->preshower_adc[0] - track->preshower_pedestal[0]) > 1.5 * track->preshower_pedestal_rms[0])prs_histo[index - 1]->Fill(track->preshower_adc[0] - track->preshower_pedestal[0]);

    //figure out eta and etaindex
    eta = helper->getEta(index);
    if(TMath::Abs(eta) > 0.968) eta += 0.005 * TMath::Abs(eta)/eta;
    etaindex = ((TMath::Nint(eta * 1000.0) + 25)/50 + 19);
    //cout<<eta<<" "<<index<<endl;
    //if(eta > 0.224 && eta < 0.226)cout<<etaindex<<endl;
    //get gain to calculate E/p
    //1.15 corrects to proper full scale
    double tgain = bemctables->calib(1,track->tower_id[0])*1.15;
    double mipgains[40] = {0.0153384,0.0199435,0.0213435,0.0195892,0.0200876,0.0182992,0.0178393,0.0174514,0.0174794,0.0164953,0.0158151,0.0166528,0.0162374,0.0146685,0.0149648,0.0153254,0.0146326,0.0151221,0.0145512,0.0148793,0.0150701,0.0145633,0.0150305,0.0154839,0.0142929,0.0147277,0.0150759,0.0158363,0.0156365,0.0160211,0.0164128,0.0172054,0.017725,0.0172588,0.0177851,0.0187099,0.0200016,0.0200538,0.0169604,0.014336};
    ring_histo[etaindex]->Fill(scaled_adc*mipgains[etaindex]);

    if(track->tower_id[0] <= 2400)west_histo->Fill(scaled_adc*tgain);
    if(track->tower_id[0] > 2400)east_histo->Fill(scaled_adc*tgain);
    all_histo->Fill(scaled_adc*tgain);

    //calculate leaked adcs
    double leaked_adc = 0;
    for(int k = 1; k < 9; k++)
      {
	if((track->tower_adc[k] - track->tower_pedestal[k]) > 1.5 * track->tower_pedestal_rms[k]) leaked_adc +=(track->tower_adc[k] - track->tower_pedestal[k]);
      }

    float leaked = leaked_adc/(leaked_adc + track->tower_adc[0] - track->tower_pedestal[0]);
    float leakedenergy = 0;
    float nTowerneighbors = 0;
    //calculate leaked energy, the towen < 5 removes trigger towers
    for(int k = 1; k < 9; k++)
      {
	float sgain = bemctables->calib(1,track->tower_id[k]);
	float towen = (track->tower_adc[k] - track->tower_pedestal[k])*sgain;
	if(track->tower_adc[k] > track->tower_pedestal[k])
	  {
	    if(towen < 5) leakedenergy += towen;
	  }
	if((track->tower_adc[k] - track->tower_pedestal[k]) > 2 * track->tower_pedestal_rms[k] && towen < 5)
	  {
	    nTowerneighbors++;
	    multen->Fill(towen);
	  }
      }

    //find tracks hitting surrounding towers
    float sisterp = 0;
    int numsis = tracks->GetEntries();
    if(numsis > 0)
      {
	for(int h = 0; h<numsis;h++)
	  {
	    dumtrack = (StEmcOfflineCalibrationTrack*)tracks->At(h);
	    if(dumtrack)sisterp += dumtrack->p;
	  }
	sistertracks->Fill(track->p,sisterp);
      }

    if((track->tower_adc[0] - track->tower_pedestal[0]) > 1.5 * track->tower_pedestal_rms[0] && bemctables->calib(1,track->tower_id[0])> 0)
      {
	float leaked = leakedenergy/(leakedenergy + (track->tower_adc[0] - track->tower_pedestal[0])*bemctables->calib(1,track->tower_id[0]));
	//float leaked1 = leakedenergy/track->p;
	float leaked1 = (leakedenergy + (track->tower_adc[0] - track->tower_pedestal[0])*bemctables->calib(1,track->tower_id[0]))/track->p;
	if(numsis == 0)
	  {
	    energyleak->Fill(dR,leaked);
	    findbg->Fill(dR,leaked1);
	    tevsp->Fill(track->p,(leakedenergy + (track->tower_adc[0] - track->tower_pedestal[0])*bemctables->calib(1,track->tower_id[0])));
	    tevspcent->Fill(track->p,((track->tower_adc[0] - track->tower_pedestal[0])*bemctables->calib(1,track->tower_id[0]))*geant_scale);
	    towermult->Fill(nTowerneighbors);
	    multvsp->Fill(track->p,nTowerneighbors);
	    if(track->p >= 2 && track->p <= 3)
	      {
		tep3->Fill(dR,leaked1);
	      }
	    else if(track->p > 3 && track->p <= 5)
	      {
		tep5->Fill(dR,leaked1);
	      }
	    else if(track->p > 5 && track->p < 10)
	      {
		tep10->Fill(dR,leaked1);
	      }
	  }
	else if(numsis!=0)
	  {
	    energyleak2->Fill(dR,leaked);
	  }
      }
    dEdxvsp->Fill(TMath::Log10(track->p),TMath::Log10(track->dEdx));


  }
  cout<<"processed electron tree"<<endl;

  double ew[21];
  for(int h=0;h<21;h++){
    TH1D* projection = energyleak->ProjectionY("projection",h,h);
    float mean = projection->GetMean();
    energymean->SetBinContent(h,mean);
    TH1D* projection1 = findbg->ProjectionY("projection1",h,h);
    float mean1 = projection1->GetMean();
    leakmean->SetBinContent(h,mean1);
    TH1D* projection2 = tevsp->ProjectionY("projection2",h,h);
    float mean2 = projection2->GetMean();
    pmean->SetBinContent(h,mean2);
    ew[h] = projection2->GetRMS();
    TH1D* projection3 = tevspcent->ProjectionY("projection3",h,h);
    float mean3 = projection3->GetMean();
    cmean->SetBinContent(h,mean3);
    TH1D* projection4 = energyleak2->ProjectionY("projection4",h,h);
    float mean4 = projection4->GetMean();
    energymean2->SetBinContent(h,mean4);
    TH1D* projection5 = multvsp->ProjectionY("projection5",h,h);
    float mean5 = projection5->GetMean();
    multmean->SetBinContent(h,mean5);
    TH1D* projection6 = tep3->ProjectionY("projection6",h,h);
    float mean6 = projection6->GetMean();
    tep3mean->SetBinContent(h,mean6);
    TH1D* projection7 = tep3->ProjectionY("projection7",h,h);
    float mean7 = projection7->GetMean();
    tep5mean->SetBinContent(h,mean7);
    TH1D* projection8 = tep3->ProjectionY("projection8",h,h);
    float mean8 = projection8->GetMean();
    tep10mean->SetBinContent(h,mean8);
  }

  TF1* fitleak = new TF1("fitleak","[0]",0,0.03);
  fitleak->SetLineWidth(0.1);
  leakmean->Fit(fitleak,"rq");


  //**********************************************//
  //Fit Tower Histograms                          //
  //**********************************************//

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

  //**********************************************//
  //Fit Ring Histograms                           //
  //**********************************************//

  for(int i=0; i<nrings; i++){
	  
    cout<<"fitting ring "<<i+1<<" of "<<nrings<<endl;
		
    sprintf(name,"ring_fit_%i",i);
    /*
    ringfit[i] = new TF1(name,fit_function,0.,140.,6);
    ringfit[i]->SetParameter(1,65.);
    ringfit[i]->SetParameter(2,10.);
    ringfit[i]->SetParameter(3,10.); //relative height of peak to bg
    ringfit[i]->SetParameter(4,10.);
    ringfit[i]->SetParameter(5,3.);
    ringfit[i]->SetParNames("Constant","Mean","Sigma","Peak Ratio","Bg Mean","Bg Sigma");
    */		
    ringfit[i] = new TF1(name,fit_function2,0.1,2.5,8);
    ringfit[i]->SetParameter(1,1.);
    ringfit[i]->SetParameter(2,0.2);
    ringfit[i]->SetParameter(3,1.5); //relative height of peak to bg
    ringfit[i]->SetParameter(4,0.25);
    ringfit[i]->SetParameter(5,0.15);
    ringfit[i]->SetParameter(7,0.8);
    ringfit[i]->SetParNames("Constant","Mean","Sigma","Peak Ratio","Bg Mean","Bg Sigma","Bg2 constant","Bg2 decay");
    	
    ringfit[i]->SetLineColor(kGreen);
    ringfit[i]->SetLineWidth(0.6);
		
    ring_histo[i]->Fit(ringfit[i],"rq");
		
    ringprec->SetBinContent(i+1,(ringfit[i]->GetParameter(1)));
    ringprec->SetBinError(i+1,ringfit[i]->GetParameter(2));
    //ew[i] = 4066/(60*(fit[i]->GetParameter(2))*(fit[i]->GetParameter(2)));
  }

  outfile.Write();
  outfile.Close();

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
