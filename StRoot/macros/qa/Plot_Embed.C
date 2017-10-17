

///////////////////////////////////////////////////////////////////////////////////////////////
// $Id: Plot_Embed.C,v 1.1 2007/04/27 15:03:04 cristina Exp $
// owner: Cristina
//
// What it does: This Macro reads two diferent scanned files and compare them. It takes the 3D histogram (nch,pt, nfit)
// and makes the projection on the z-axis (nfit) and give a 1D  plot for Nfit for both files
// with extension .gif - When running this macro, include 2 bins for Pt:pt1 and pt2, check the location for the imput  files and where
// the output will be saved
//Plot created are: Dca, Nfit, EnergyLoss, Efficiecy, Delta_Vertex (X, Y, Z), reference multilicity
//              
/////////////////////////////////////////////////////////////////////////////////////////////////



#ifndef __CINT__
#include "TROOT.h"
#include "TSystem.h"
#include <iostream.h>
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TTreeHelper.h"
#endif

void plot_Nfit(float pt1 = 0.2, float pt2 = 2.0, int nch1 = 0, int nch2 = 1000, Int_t id=6){

gROOT->LoadMacro("~/mymacros/Utility.C");
gROOT->LoadMacro("~/mymacros/UtilityHist.C");
gStyle->SetOptStat(0);
gStyle->SetOptDate(0);
//gStyle->SetOptTitle(0);	
gStyle->SetOptFit(0);	

char* oFile1 = "~/outputs/Phi/out_scan_embed_mc_Phi1.root";
char* oFile2 = "~/outputs/out_scan_embed_mc_KMinus.root";

//Opening file 1
 f1 = new TFile(oFile1);

   TH3D* hDca = hMcDca->Clone(); hDca->SetName("hDca");
   TH3D* hNfit = hNfit3->Clone();hNfit->SetName("hNfit");
   TH2F* dEdx_ep = (TH2F *) ((TH2F *) f1->Get("dEdx"))->Clone();

 //Opening file 2

f2 = new TFile(oFile2);

   TH3D* hDcaR = hMcDcaR->Clone();hDcaR->SetName("hDcaR");
   TH3D* hNfit = hNfit3->Clone();hNfit->SetName("hNfit");
   TH2F* dEdx_pi = (TH2F *) ((TH2F *) f2->Get("dEdx"))->Clone();

   // --------------------------------------Plotting DCA-----------------------

	TCanvas *c= new TCanvas("c","DCA",400, 400);
	
	c->SetGridx(0);
	c->SetGridy(0);
	c->SetLeftMargin(0.15);
	c->SetRightMargin(0.05);
	c->cd;

//Bins for Multiplicity - for oFile1
	TH1D *hX =  (TH1D*)hDca->Project3D("X");
	Int_t bin_nch1 = hX->FindBin(nch1);
	Int_t bin_nch2 = hX->FindBin(nch2);

//Bins for Pt - for oFile1
	TH1D *hY =  (TH1D*)hDca->Project3D("Y");
	Int_t bin_pt1 = hY->FindBin(pt1);
	Int_t bin_pt2 = hY->FindBin(pt2);

//ProjectionZ (DCA) for oFile1 is done in nch bins and pt bins found above

	TH1D *hDca1 =(TH1D*) hDca->ProjectionZ("hDca1",bin_nch1, bin_nch2, bin_pt1, bin_pt2);
	
	hDca1->SetLineColor(2);// Data in Red - eplus
	hDca1->SetXTitle("Dca (cm)");
	hDca1->SetYTitle("Counts");
	hDca1->GetYaxis()->SetTitleOffset(1.8);
	hDca1->GetYaxis()->SetDecimals();
       
	
	float mscale = hDca->GetMaximum();
	
	
//Bins for Multiplicity - for oFile2
	TH1D *hX_r =  (TH1D*)hDcaR->Project3D("X");
	Int_t bin_nch1_r = hX_r->FindBin(nch1);
	Int_t bin_nch2_r = hX_r->FindBin(nch2);

//Bins for Pt -  for oFile2
	TH1D *hY_r =  (TH1D*)hDcaR->Project3D("Y");
	Int_t bin_pt1_r = hY_r->FindBin(pt1);
	Int_t bin_pt2_r = hY_r->FindBin(pt2);

//ProjectionZ (DCA)  for oFile2

	TH1D *hDca1_r=(TH1D*)hDcaR->ProjectionZ("hDca1_r",bin_nch1_r, bin_nch2_r, bin_pt1_r, bin_pt2_r);

       	Double_t Sum_Real = hDca1_r->GetSum();
	Double_t Sum_MC = hDca1->GetSum();
	hDca1_r->Scale(1./Sum_Real); //Scaling  Data
	hDca1->Scale(1./Sum_MC); 
	
	hDca1->Draw();
	hDca1_r->Draw("same")

	//-----------------------------------Plotting Nfit-------------------------------------


	TCanvas *c1= new TCanvas("c1","NFIT",400, 400);
	c1->SetGridx(0);
	c1->SetGridy(0);
	c1->SetLeftMargin(0.15);
	c1->SetRightMargin(0.05);
	c1->cd;


 //Bins for Multiplicity - oFile 1
	TH1D *hX =  (TH1D*)hNfit->Project3D("X");
	Int_t multL = hX->FindBin(nch1);
	Int_t multH = hX->FindBin(nch2);


// Bins for Pt - oFile 1
	TH1D *hY =  (TH1D*)hNfit->Project3D("Y");
	Int_t bin_pt1 = hY->FindBin(pt1);
	Int_t bin_pt2 = hY->FindBin(pt2);


// Projection Z(Nfit) for oFile 1
	TH1D *hNfitz =(TH1D*) hNfit->ProjectionZ("hNfitz",multL, multH, bin_pt1, bin_pt2);

	hNfitz->SetLineColor(2);
	hNfitz->SetXTitle("Nfit");
	hNfitz->SetYTitle("Counts");
	hNfitz->GetYaxis()->SetDecimals();
	hNfitz->GetYaxis()->SetTitleOffset(1.7);
	
	Double_t Sum_MC= hNfitz->GetSum();


//Bins for Multiplicity - oFile 2
	
	TH1D *hX_r = (TH1D*) hNfitR->Project3D("X");
	Int_t multLr = hX_r->FindBin(nch1);
	Int_t multHr = hX_r->FindBin(nch2);


//Bins for Pt -  oFile 2
	TH1D *hY_r =  (TH1D*)hNfitR->Project3D("Y");
	Int_t bin_pt1_r = hY_r->FindBin(pt1);
	Int_t bin_pt2_r = hY_r->FindBin(pt2);

// Projection Z(Nfit) for oFile 2

	TH1D *hNfitz_r = (TH1D*)hNfitR->ProjectionZ("hNfitz_r",multLr, multHr, bin_pt1_r, bin_pt2_r);

	Double_t sum_Real = hNfitz_r->GetSum();
	hNfitz_r->Scale(1./sum_Real); //Scaling real Data
	hNfitz->Scale(1./Sum_MC); //Scaling MC Data

	//------------------- Plotting Global Variables-----------------------------
	// Delta Vertex position (dvx, dvY, dvZ and vx_vy)
	
	TCanvas *c2= new TCanvas("c2","Delta Vertex Position",400, 400);
	c2->SetGridx(0);
	c2->SetGridy(0);
	c2->cd;
	
	v_xy->Draw();


	TCanvas *c3= new TCanvas("c3","Delta Vertex Position",400, 400);
	c3->SetGridx(0);
	c3->SetGridy(0);
	c3->SetLeftMargin(0.15);
	c3->SetRightMargin(0.05);
	c3->cd;
	c3->Divide(3,1);

	c3_1->cd(); //3 plots in the same canvas
 	dvx->Draw();

	c3_2->cd()
	dvy->Draw();
	
	c3_3->cd()
	dvz->Draw();

	//--------------Plotting Reference Multiplicity---------------------------

	TCanvas *c4= new TCanvas("c4","Reference Multiplicity",400, 400);
	c4->SetGridx(0);
	c4->SetGridy(0);
	c4->cd;
	
	hMult ->Fill(*nPos + *nNeg); // coming from MuDst ??
	hMult->Draw();

	//----------------------Energy Loss------------------
	//For Primary Tracks
	
	TCanvas *c5= new TCanvas("c5","Energy Loss",400, 400);
	c5->SetGridx(0);
	c5->SetGridy(0);
	c5->cd;

        hPtM_E_Pr ->Fill(PtPrMatched[itr],PtPrMatched[itr]-PtMcMatched[itr]);
	hPtM_E_Pr ->SetLineColor(9);
	hPtM_E_Pr ->Draw();

	//For Global Tracks
	TCanvas *c6= new TCanvas("c6","Energy Loss",400, 400);
	c6->SetGridx(0);
	c6->SetGridy(0);
	c6->cd;

	hPtM_E_Gl -> Fill (PtPrGlobal[itr],PtPrGlobal[itr]-PtMcGlobal[itr]);
	hPtM_E_Gl -> SetLineColor(8);
	hPtM_E_Gl -> Draw();

	//-------------------------Efficiency--------------------------

	TCanvas *c7= new TCanvas("c7","Energy Loss",400, 400);
	c7->SetGridx(0);
	c7->SetGridy(0);
	c7->cd;

	//for Primary / Embedded_MC


	//for Global / Embedded_MC

	//-------------------------MIPS----------------------
	
	TCanvas *c8= new TCanvas("c8","Energy Loss",400, 400);
	c8->SetGridx(0);
	c8->SetGridy(0);
	c8->Divide(2,1);
	c8_1->cd();	
	
	dEdx_ep->SetMarkerColor(2);//E-plus
	//dEdx_ep->Draw();
	dEdx_pi->ProfileX();
	//dEdx_pi->Draw("same");//Pi 
	dEdx_pi->SetXTitle("Momentum P (GeV/c)");
	dEdx_pi->SetYTitle("dE/dx");
	c8->cd;
	
	 	
	//Bins for Pt - Pion
	TH1D *dEdx_pi_x = (TH1D*)dEdx_pi->ProjectionX();
	Int_t bin_pt1_r = dEdx_pi_x->FindBin(pt1);
	Int_t bin_pt2_r = dEdx_pi_x->FindBin(pt2);

	//ProjectionY (dEdx) - Pion
 
	TH1D *dEdx_pi_y = (TH1D*)dEdx_pi->ProjectionY("py",bin_pt1_r, bin_pt2_r);
	dEdx_pi_y->SetAxisRange(0.,4.,"X");
	dEdx_pi_y->SetXTitle("dE / dx");
	
	// Gaussian Fit for Pion Data

	Double_t par[6];
	g1    = new TF1("g1","gaus",0.8.,1.6);
   	g2    = new TF1("g2","gaus",1.7,1.9.);
   	PiFitPiE2 = new TF1("PiFitPiE2","([0]/[2]*exp(-0.5*pow((x-[1])/[2],2))+[3]/[5]*exp(-0.5*pow((x-[4])/[5],2)))/sqrt(6.283)");//Fitting pi Data with 2 Gaussians

		PiFitPiE2->SetRange(0.0,4.0);
		dEdx_pi_y->Fit(g1,"R");
		dEdx_pi_y->Fit(g2,"R");
   
	g1->GetParameters(&par[0]);
   	g2->GetParameters(&par[3]);
 
	PiFitPiE2->SetParameters(par);
	PiFitPiE2->SetLineWidth(2);
	PiFitPiE2->SetLineColor(3);
	dEdx_pi_y->Fit(PiFitPiE2,"R");
	keyLine(.5,.7,"dEdx Pi",1);


	//----------------PROJECTION Y  DATA-----------------------------------------
	
	c8_2->cd();
 
	//Bins for Pt -EPlus

	TH1D *dEdx_ep_x = (TH1D*)dEdx_ep->ProjectionX();
	Int_t bin_pt1 = dEdx_ep_x->FindBin(pt1);
	Int_t bin_pt2 = dEdx_ep_x->FindBin(pt2); 

//ProjectionY (dEdx) - EPlus

	TH1D *dEdx_ep_y = (TH1D*)dEdx_ep->ProjectionY("py1",bin_pt1, bin_pt2);
	dEdx_ep_y->SetAxisRange(0.,4.,"X");
	dEdx_ep_y->Draw();

// Gaussian Fit for ME_plus Data	

	g6  = new TF1("g6","gaus",.6,4.);
	g6->SetLineWidth(2);
	g6->SetLineColor(2);
	dEdx_ep_y->Fit(g6,"R");
	Double_t Mean = g6->GetParameters(&par[2]);
	
	cout<< "Mean is : "<<Mean<<endl;
	keyLine(.5,.8,"dEdx EPlus",2);
	keyLine(.5, .6, "Mean Value : Mean",2) ;
	
	cout<<"bin_pt1_r"<<bin_pt1_r<<endl;
	cout<<"bin_pt2_r"<<bin_pt2_r<<endl;
	cout<<"bin_pt1"<<bin_pt1<<endl;
	cout<<"bin_pt2"<<bin_pt2<<endl;
	
	
	//------------------------------------------------------------------------
char title[100], gif[100];

 sprintf(gif,"~/Plots/dedx_%.2fpT%.2f_%d.jpg",pt1,pt2);

dEdx_ep_y->SetTitle(title);
dEdx_pi_y->SetTitle(title);
dEdx_ep_y->Draw();


	hDca1->SetTitle(title);
	hDca->Write();
	hNfit->Write();
	hMult->Write();
return;
}
