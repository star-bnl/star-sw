#include <iostream>
#include <fstream>
#include <set>
using namespace std;

#include "CalibrationHelperFunctions.cxx"

void mip_ring_fitter(const char* file_list="", const char* skimfile="mipskimfile.root") 
{
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

  const int ntowers=4800;
  const int nrings=40;
  const bool lookForSwaps=false;

  CalibrationHelperFunctions* helper = new CalibrationHelperFunctions();

	cout<<"input filelist:  "<<file_list<<endl;
	cout<<"histogram file:  "<<skimfile<<endl;
	
	//chain all input files together
	char file[300];
	TChain* calib_tree = new TChain("calibTree");
	ifstream filelist(file_list);
	TFile *test_file;
	while(1){
		filelist >> file;
		if(!filelist.good()) break;
		cout<<file<<endl;
		calib_tree->Add(file);
	}
	
	StEmcOfflineCalibrationEvent* myEvent = new StEmcOfflineCalibrationEvent();
	calib_tree->SetBranchAddress("event_branch",&myEvent);
	StEmcOfflineCalibrationTrack* mip;
	
	//create the 4800 mip histograms
	TH1* ring_histo[nrings];
	char name[100];
	for(int k=0; k<nrings; k++){
		sprintf(name,"ring_histo_%i",k+1);
		ring_histo[k] = new TH1D(name,name,250,-50.5,199.5);
	}
	
	//keep track of all hit towers and exclude any with >1 track/tower
	set<int> track_towers;
	set<int> excluded_towers;
	
	unsigned int nentries = calib_tree->GetEntries();
	for(unsigned int i=0; i<nentries; i++){
		if(i%100000 == 0) cout<<"processing "<<i<<" of "<<nentries<<endl;
		
		track_towers.clear();
		excluded_towers.clear();
		
		calib_tree->GetEntry(i);
	
		//we're only working with events from vertexIndex==0
		if(TMath::Abs(myEvent->vz[0]) > 30.)	continue;
		
		//do a quick loop over tracks to get excluded towers
		for(int j=0; j<myEvent->tracks->GetEntries(); j++){
			mip = (StEmcOfflineCalibrationTrack*)myEvent->tracks->At(j);
			int id = mip->tower_id[0];

			if(track_towers.find(id) != track_towers.end()){
				excluded_towers.insert(id);
			}
			else{
				track_towers.insert(id);
			}
		}   
		
		//select on runnumbers to look for stability
		//if(myEvent->run < 7135000) continue;
				
		for(int j=0; j<myEvent->tracks->GetEntries(); j++){
			mip = (StEmcOfflineCalibrationTrack*)myEvent->tracks->At(j);
						
/*			if(lookForSwaps){
				for(int tower=1; tower<9; tower++){
					double pedsub = mip->tower_adc[tower] - mip->tower_pedestal[tower];
					
					if(mip->p < 1.)												continue;
					if(mip->tower_status[tower] != 1)							continue;
					if(TMath::Abs(pedsub) < 1.5*mip->tower_pedestal_rms[tower])	continue;
					if(mip->tower_id[0] != mip->tower_id_exit)					continue;
					if(mip->vertexIndex > 0)									continue;
					
					int index = mip->tower_id[0];
					mip_histo[index-1]->Fill(pedsub);
				}
			}
			else
			{
				double pedsub = mip->tower_adc[0] - mip->tower_pedestal[0];
				
				if(mip->p < 1.)											continue;
				if(mip->tower_status[0] != 1)							continue;
				if(mip->highest_neighbor > 2.)							continue;
				if(TMath::Abs(pedsub) < 1.5*mip->tower_pedestal_rms[0])	continue;
				if(mip->tower_id[0] != mip->tower_id_exit)				continue;
				if(mip->vertexIndex > 0)								continue;
				if(excluded_towers.find(mip->tower_id[0]) != excluded_towers.end()) continue;

		
				int index = mip->tower_id[0];
				mip_histo[index-1]->Fill(pedsub);
			}*/
			
			//preshower histograms for Rory
			//double pedsub = mip->preshower_adc[0] - mip->preshower_pedestal[0];
			double pedsub = mip->tower_adc[0] - mip->tower_pedestal[0];
			//double pedsub = mip->tower_adc[0];
			if(mip->p < 1.)continue;
			if(mip->tower_status[0] != 1)continue;
			if(mip->highest_neighbor > 2.)continue;
			if(TMath::Abs(pedsub) < 1.5*mip->tower_pedestal_rms[0])continue;
			if(mip->tower_id[0] != mip->tower_id_exit)continue;
			if(mip->vertexIndex > 0)continue;
			if(excluded_towers.find(mip->tower_id[0]) != excluded_towers.end()) continue;
			
			//cout<<mip->tower_pedestal_rms[0]<<endl;
			int index = mip->tower_id[0];
			double eta = helper->getEta(index);
			if(TMath::Abs(eta) > 0.968) eta += 0.005 * TMath::Abs(eta)/eta;
			int etaindex = ((TMath::Nint(eta * 1000.0) + 25) / 50 + 19);
			ring_histo[etaindex]->Fill(pedsub);
			
		}
	}
	
	TFile* output_file = new TFile(skimfile,"RECREATE");
	//for(int k=0; k<ntowers; k++) mip_histo[k]->Write();
	for(int k=0; k<nrings;k++) ring_histo[k]->Write();
	output_file->Close();
	/*
	//draw the histograms and their fits
	TPostScript* ps = new TPostScript(postscript);
	TCanvas* c = new TCanvas("c","",100,100,600.,800.);
	int pad = 16;
	for(int i=0; i<nrings; i++){
		if(pad%15 == 1){
			c->Update();
			ps->NewPage();
			c->Clear();
			c->Divide(3,5);
			pad = 1;
		}
		c->cd(pad);
	  
		if(i%600 == 0) cout<<"fitting tower "<<i+1<<" of "<<ntowers<<endl;
		
		sprintf(name,"fit_%i",i+1);
		
		//this fit is for the electron tree
//		fit[i] = new TF1(name,"gaus",0.,160.);
//		fit[i] = new TF1(name,"gaus(0)+gaus(3)",0.,140.);
//		fit[i]->SetParameter(1,65.);
//		fit[i]->SetParameter(2,10.);
//		fit[i]->SetParameter(4,10.);
//		fit[i]->SetParameter(5,7.);
		fit[i] = new TF1(name,fit_function,0.,140.,6);
		fit[i]->SetParameter(1,65.);
		fit[i]->SetParameter(2,10.);
		fit[i]->SetParameter(3,10.); //relative height of peak to bg
		fit[i]->SetParameter(4,10.);
		fit[i]->SetParameter(5,3.);
		fit[i]->SetParNames("Constant","Mean","Sigma","Peak Ratio","Bg Mean","Bg Sigma");
		
		//this fit is for the hadron tree
//		fit[i] = new TF1(name,"landau",0.,140.);
//		fit[i] = new TF1(name,"gaus(0)+gaus(3)",0.,140.);
//		fit[i] = new TF1(name,fitf,0.,140.,3);
//		fit[i]->SetParameter(1,10.);
//		fit[i]->SetParameter(2,3.);
//		fit[i]->SetParameter(4,15.);
//		fit[i]->SetParameter(5,25.);
		
		fit[i]->SetLineColor(kGreen);
		fit[i]->SetLineWidth(0.6);
		
		ring_histo[i]->Fit(fit[i],"rq");
		
		drawTower(ring_histo[i],fit[i],i, helper);
		
		pad++;
	}
	
	ps->Close();
	*/
	//print gains to a file
	//	ofstream gains(gainfile);
	//char line[500];
	/*	for(int i=0; i<nrings; i++){
		float gain = 1./fit[i]->GetParameter(1);
//		cout<<i+1<<"  "<<gain<<"  "<<helper->getTheta(i+1)<<"  "<<TMath::Sin(helper->getTheta(i+1))<<endl;
		gain /= TMath::Sin(helper->getTheta(i+1));		
		if(!helper->isGoodTower2006(i+1)) gain = 0.;
		if(gain<0.) gain=0.;
		
		sprintf(line,"%-4i     % 1.6f",i+1,gain);
		gains<<line<<endl;
		}*/
}

void drawTower(TH1* h, TF1* f, int id, CalibrationHelperFunctions* helper){	
	//calculate a few quantities
	double peak = f->GetParameter(1);
	double histo_height = h->GetBinContent(h->GetMaximumBin());
	if(histo_height == 0) histo_height = 1.;
	
	//histogram options
	h->SetXTitle("ADC/gev*sin(#theta)");
	h->Draw("esame");

	//draw a line through the location of the MIP peak
	TLine *gaussian_peak = new TLine(peak,0.,peak,histo_height+15);
	gaussian_peak->SetLineColor(kGreen);
	gaussian_peak->SetLineWidth(2.0);
	gaussian_peak->Draw("same");
	
	//write the tower number on the plot
	char tower_title[100];
	float eta = (float)((id - 20) * 2 + 1)/40;
	sprintf(tower_title,"eta = %f",eta);
	TLatex title_latex;
	title_latex.SetTextSize(0.15);
	if(!helper->isGoodTower2006(id)) title_latex.SetTextColor(kRed);
	title_latex.DrawTextNDC(0.13,0.78,tower_title);
	
	//draw the peak Gaussian
	TF1 *f2 = new TF1(tower_title,"gaus",0.,140.);
	f2->FixParameter(0,f->GetParameter(0));
	f2->FixParameter(1,f->GetParameter(1));
	f2->FixParameter(2,f->GetParameter(2));
	f2->SetLineWidth(0.6);
	f2->Draw("same");
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
	
	double par3 = par[0] / par[3];
	double par6 = par3/1.5;
	double par7 = par[4] + 10.;
	double par8 = par[5] * 6.5;
	
	double fitval = 0;
	if(par[2] != 0){
		double arg1 = (x[0]-par[1])/par[2];
		double arg2 = (x[0]-par[4])/par[5];
		double arg3 = (x[0]-par7)/par8;
		fitval = par[0]*TMath::Exp(-0.5*arg1*arg1) + par3*TMath::Exp(-0.5*arg2*arg2) + par6*TMath::Exp(-0.5*arg3*arg3);
	}
	
	return fitval;


}
