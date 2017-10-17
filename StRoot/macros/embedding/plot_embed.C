//First run scan_embed.C to generate root file with all the histograms
// V. May 31 2007 - Cristina
//==================================================
//V2 Revised February 2009 - Cristina 
// plots of dca and nfit (eta and pt) dependant simultaneously are being added
//============================================================

  
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
#include "TText.h"
#include "TLatex.h"
#include "TAttLine.h"
#include "TCanvas.h"
#endif

static Int_t iCanvas = 1 ;
//P08id_testXXXXXX_Phi_PiPlus_histo_040709.rootP08id_test_Phi_PiPlus_histo_031509.root"

void plot_embed(
		const Char_t* inputFileName = "~/Embed/P08id_testXXXXXX_Phi_PiPlus_histo_040709.root",
		const Int_t id=11
		)
{
  Int_t plot = 0;
  const TString PathSave = "/Embed/";

  //id -> is for the reconstructed track  or Daughter
  
  gROOT->LoadMacro("StRoot/macros/embedding/Utility.C");
  //  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);
  
  //Labels ->These will appear in plots and when saving them

  Char_t *Prod ="P08id";
  Char_t *Particle = "Phi";//This is the particle embedded
  Char_t *Daughter ="Pion";//reconstructing on this Daughter
  Char_t *Date= "031509";
  
  char  text1[80];
  sprintf(text1,"P08id Phi");//this is going to show in all the histograms
  TLatex t;
  t.SetTextSize(0.07);
  t.SetTextColor(1);
  t.SetNDC();

  char title[100];
  char gif[400];
 
  float mass2 = -1.0;
  TString tag("K"); 
  TString tagTex("K");

  if (id == 8)  { tag = "Piplus";  tagTex = "#pi^{+}"; Char_t * Daughter = "Piplus" ;mass2 = 0.019;}
  if (id == 9)  { tag = "Piminus"; tagTex = "#pi^{-}"; Char_t * Daughter = "Piminus"; mass2 = 0.019;}
  if (id == 11) { tag = "Kplus";   tagTex = "K^{+}";   Char_t * Daughter = "Kplus"; mass2 = 0.245;}
  if (id == 12) { tag = "Kminus";  tagTex = "K^{-}";   Char_t * Daughter = "Kminus";mass2 = 0.245;}
  if (id == 14) { tag = "Proton";  tagTex = "p";       Char_t * Daughter = "Proton";mass2 = 0.880;}
  if (id == 15) { tag = "Pbar";    tagTex = "#bar{p}"; Char_t * Daughter = "Pbar"; mass2 = 0.880;}
  if (id == 50) { tag = "Phi";     tagTex = "#phi";    mass2 = 1.020;}
  if (id == 2)  { tag = "Eplus";   tagTex = "e^{+}";   mass2 = 0.511;}
  if (id == 1)  { tag = "Dmeson";  tagTex = "D";       mass2 = 1.864;}


  //==============================================================
  //Cloning Histograms
  //==============================================================

  
  TFile* file = TFile::Open(inputFileName);
  if(!file || !file->IsOpen()){
    cout << "can't open " << inputFileName << endl;
    return;
  }

  TH3D *hDca1   = (TH3D*) file-> Get("hDca");//DCA
  TH3D *hNfit1  = (TH3D*) file-> Get("hNfit");//Nfit
 

  TH2D *hPtM_E1 = (TH2D*) file-> Get("hPtM_E");//Energy Loss
  TH2F *dedx1  = (TH2F*) file-> Get("dedx");

  TH3D *hDcaG1  = (TH3D*) file-> Get("hDcaG");//DCA
  TH3D *hNfitG1 = (TH3D*) file-> Get("hNfitG");//Nfit

  TH2F *dedxG1 = (TH2F*) file-> Get("dedxG");
  TH1D *PhiG1  = (TH1D*) file-> Get("PhiG");

  TH2D *vxy1   = (TH2D*) file-> Get("vxy");

  TH1D *vz1    = (TH1D*) file-> Get("vz");
  TH1D *dvx1   = (TH1D*) file-> Get("dvx");
  TH1D *dvy1   = (TH1D*) file-> Get("dvy");
  TH1D *dvz1   = (TH1D*) file-> Get("dvz");
 
  TH1D *PhiMc1   = (TH1D*) file-> Get("PhiMc");
  TH1D *EtaMc1   = (TH1D*) file-> Get("EtaMc");
  TH1D *YMc1     = (TH1D*) file-> Get("YMc");
  TH1D *PtMc1    = (TH1D*) file-> Get("PtMc");
  TH3D *PtYPhiMc1= (TH3D*) file-> Get("PtYPhiMc");
 
  TH1D *PhiM1    = (TH1D*) file-> Get("PhiM");
  TH1D *EtaM1    = (TH1D*) file-> Get("EtaM");
  TH1D *YM1      = (TH1D*) file-> Get("YM");
  TH1D *PtM1     = (TH1D*) file-> Get("PtM");
  TH3D *PtYPhiM1 = (TH3D*) file-> Get("PtYPhiM");

  TH2D *PtM_eff1 = (TH2D*) file->Get("PtM_E");//efficiency

  //if you have MuDst hist
 
  TH2D *dedx1R   = (TH2D*) file-> Get("dedxR");
 
  //3D histos

  TH3D *hDcaM = (TH3D*) file-> Get("hPtEtaDcaM");//DCA
  TH3D *hDcaG = (TH3D*) file-> Get("hPtEtaDcaG");//DCA
  TH3D *hDcaR = (TH3D*) file-> Get("hPtEtaDcaR");
  
  TH3D *hNfitM  = (TH3D*) file-> Get("hPtEtaNfitM");//Nfit
  TH3D *hNfitR  = (TH3D*) file-> Get("hPtEtaNfitR");//
  TH3D *hNfitG  = (TH3D*) file-> Get("hPtEtaNfitG");//Nfit


  const int nch1 = 0;
  const int nch2 = 1000;
  
  //The Following bins are  for the 3X3 matrix of eta,pt d

  const  int nEtaBin=3;
  const Double_t eta[nEtaBin+1]= {0.2,0.5,0.8,1.0};
  
  const  int nPtBin=3;
  const Double_t pt[nPtBin+1]  = {0.5,0.6,0.8,1.0};
  
  
  //=============================================================
  //Delta Vertex position
  //=========================================================
  /*
  TCanvas *cdv= new TCanvas("cdv","Delta Vertex position",700, 500);
  cdv->Divide(3, 1);

  cdv_1->cd();
  // dvx1->Rebin(2);
  dvx1->SetXTitle("#Delta VX");
  dvx1->SetAxisRange(-1.5, 1.5, "X");
  dvx1->Draw();

  cdv_2->cd();
  dvy1->SetXTitle("#Delta VY");
  dvy1->SetAxisRange(-1.5, 1.5, "X");
  dvy1->Draw();

  cdv_3->cd();
  dvz1->SetXTitle("#Delta VZ");
  dvz1->SetAxisRange(-1.5, 1.5, "X");
  dvz1->Draw();
  

  t.DrawLatex(0.3, 0.9, text1);
  
  cdv->Update();
 
if (plot)
{
  sprintf(gif,"~/Embed/%s/%s/DeltaVertex_%s_%s_%s_%sXXXXXXXXX.gif",Prod, Particle, Prod, Particle,Daughter, Date);
  // sprintf(gif,"~/Embed/Test_DeltaVertex_%s_%s_%s_%sXXXXXXXXX.gif", Prod, Particle,Daughter, Date);
  // sprintf(gif,PathSave+"Test.gif");//, Prod, Particle,Daughter, Date);
  cdv->SaveAs(gif);
} 
 
// return;


  //=============================================================
  //Vertex position
  //=========================================================
  
    
  TCanvas *cv= new TCanvas("cv","Vertex position",600, 400);
  cv->Divide(2,1);

  cv_1->cd();
  vz1->Rebin(2);
  vz1->SetXTitle("Vertex Z");
  vz1->Draw();

  cv_2->cd();

  vxy1->Draw("colz");
  vxy1->SetAxisRange(-1.5, 1.5, "X");
  vxy1->SetAxisRange(-1.5, 1.5, "Y");
  vxy1->SetXTitle ("vertex X");
  vxy1->SetYTitle ("vertex Y");

  //keyLine(.2, 1.05,Prod,1);
  //keyLine(.2, 1.00,Particle,1);
  t.DrawLatex(0.3, 0.9, text1);

  cv->Update();
  
  if (plot)
    {
      sprintf(gif,"~/Embed/%s/%s/Vertex_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle, Daughter,Date);
      cv->SaveAs(gif);
    }
  // return;
  
  //=============================================================
  //dEdx
  //=========================================================
   
  
  TCanvas *cdedx= new TCanvas("cdedx","dEdx vs P",600, 600);
  // gStyle->SetPalette(1);

  cdedx->SetGridx(0);
  cdedx->SetGridy(0);
  cdedx->SetLeftMargin(0.15);
  cdedx->SetRightMargin(0.05);
  cdedx->cd();

  dedxG1->SetTitle("dEdx vs P ");
  dedxG1->SetXTitle("Momentum  (GeV/c)");
  dedxG1->SetYTitle("dE/dx (x10^{-6})");
 
  dedxG1->SetMarkerStyle(1);
  dedxG1->SetMarkerSize(0.2);
  dedxG1->SetMarkerColor(4);
  dedxG1->SetAxisRange(0, 8., "X");
  dedxG1->SetAxisRange(0, 8., "Y");
 
  
  dedx1->SetTitle("dEdx vs P ");
  dedx1->SetXTitle("Momentum  (GeV/c)");
  dedx1->SetYTitle("dE/dx (x10^{-6})");
  dedx1->SetAxisRange(0, 8., "X");
  dedx1->SetAxisRange(0, 8., "Y");
  dedx1->SetMarkerStyle(1);
  dedx1->SetMarkerSize(2);
  dedx1->SetMarkerColor(2);
  
  dedx1R->SetAxisRange(0, 8., "X");
  dedx1R->SetAxisRange(0, 8., "Y");
  dedx1R->SetMarkerStyle(1);
  dedx1R->SetMarkerSize(0.2);
  dedx1R->SetMarkerColor(1);

  //dedx1R->Draw();
  dedxG1->Draw();//"colz");
  dedx1->Draw("same"); 
  // dedx1R->Draw("same");
  t.SetTextSize(0.05);
  t.DrawLatex(0.3, 0.9, text1);
  //keyLine(.2, 1.05,Prod,1);
  //keyLine(.2, 1.00,Particle,1);
  
  keySymbol(.4, .87, "Matched Tracks_"+tagTex,2,20, 0.03);
  keySymbol(.4, .82, "Ghost Tracks",4,20,0.03); 
  //   keySymbol(.4, .82, "MuDst Tracks_"+tagTex,1,20,0.03);
  
  cdedx->Update();
  if(plot)
    {
      
      // sprintf(gif,"~/Embed/%s/%s/Dedx_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle,Daughter, Date);
      // cdedx->SaveAs(gif);
    }
      // return;
  

  //=============================================================
  //pt vs y of mc tracks
  //=========================================================
   
  	  
  TCanvas *c11= new TCanvas("c11","PT VS Y",400, 400);
  
  c11->SetGridx(0);
  c11->SetGridy(0);
  c11->cd();

  TH2D *hPt_Y =  (TH2D*)PtYPhiMc1->Project3D("XY");
  hPt_Y->SetTitle("pT Vs Y of MC tracks");
  hPt_Y->SetYTitle ("pT (GeV/c)");
  hPt_Y->SetXTitle ("Y");
  hPt_Y->SetAxisRange(-1.2, 1.2, "X");
  hPt_Y->SetAxisRange(0, 2.5, "Y");
  hPt_Y->Draw();
  
   t.SetTextSize(0.05);
  t.DrawLatex(0.3, 0.9, text1);

  c11->Update();
  // sprintf(gif,"~/embedding/%s/%s/pT_Y_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle,Daughter, Date);
  // c11->SaveAs(gif);

	  
  //=============================================================
  //pt vs phi of mc tracks
  //=========================================================	
	 
  
	  
  TCanvas *c12= new TCanvas("c12","pT VS PHI",400, 400);

  c12->SetGridx(0);
  c12->SetGridy(0);
  c12->cd();

  TH2D *hPt_Phi =  (TH2D*)PtYPhiMc1->Project3D("XZ");
  hPt_Phi->SetTitle("pT Vs Phi of MC tracks");
  hPt_Phi->SetYTitle ("p_{T} (GeV/c)");
  hPt_Phi->SetXTitle ("Phi");
  hPt_Phi->SetAxisRange(-3.4, 3.4, "X");
  hPt_Phi->SetAxisRange(0, 2.5, "Y");
  hPt_Phi->Draw();
  t.SetTextSize(0.05);
  t.DrawLatex(0.3, 0.9, text1);
  // keySymbol(.2, 1.05,Prod,1);
  // keySymbol(.4, 1.05,Particle,1);
  
  c12->Update();
  //  sprintf(gif,"~/embedding/%s/%s/pT_Phi_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle, Daughter, Date);
  // c12->SaveAs(gif);
  // return;
		     
  //==================================================
  //y vs phi of mc tracks
  //===================================================
  TCanvas *cyphi= new TCanvas("cyphi","Y VS PHI",400, 400);

  cyphi->SetGridx(0);
  cyphi->SetGridy(0);
  cyphi->cd();

  TH2D *hY_Phi =  (TH2D*)PtYPhiMc1->Project3D("YZ");
  hY_Phi->SetTitle("Y Vs Phi of MC tracks");
  hY_Phi->SetYTitle ("Y (GeV/c)");
  hY_Phi->SetXTitle ("Phi");
  hY_Phi->SetAxisRange(-3.4, 3.4, "X");
  hY_Phi->SetAxisRange(-1.2, 1.2, "Y");
  hY_Phi->Draw();

  // keySymbol(.2, 1.05,Prod,1);
  // keySymbol(.4, 1.05,Particle,1);
  t.SetTextSize(0.05);
  t.DrawLatex(0.3, 0.9, text1);
  cyphi->Update();
  if (plot)
    {
      
      sprintf(gif,"~/embedding/%s/%s/Y_phi_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle, Daughter,Date);
      cyphi->SaveAs(gif);
    }

  //  return;
  
  //=====================================================
  //pt
  //=========================================================
  
  TCanvas *cpt= new TCanvas("cpt","Pt",500, 500);

  cpt->SetGridx(0);
  cpt->SetGridy(0);
  cpt->cd();
  gStyle->SetOptDate(0); 

  PtM1->Rebin(2); 
  PtM1->Scale(1/2);
  PtM1->Scale(PtM1->GetSum());
  
  PtMc1->Rebin(2); 
  PtMc1->Scale(1/2);
  PtMc1->Scale(PtMc1->GetSum());

  PtMc1->SetLineColor(1);
  PtMc1->SetTitle("p_{T}");
  PtMc1->SetXTitle ("pT (GeV/c)");

  PtMc1->SetAxisRange(0.0, 8, "X");
  PtMc1->SetAxisRange(0.0, 1.2*PtM1->GetMaximum(), "Y");
  PtMc1->Draw();
  
  PtM1->SetLineColor(2);
  PtM1->Draw("same");
  
  t.SetTextSize(0.05);
  t.DrawLatex(0.3, 0.9, text1);

  keyLine(.4, 0.80,"MC tracks",1);
  keyLine(.4, 0.75,"MatGlobal RC tracks "+tagTex,2);
 
  cpt->Update();
  
  if (plot)
    {
      sprintf(gif,"~/embedding/%s/%s/pT_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle,Daughter, Date);
      cpt->SaveAs(gif);
    }
  // return;
  
  //======================================================
  //phi
  //==============================================================

  
  TCanvas *cphi= new TCanvas("cphi","PHI",500, 500);
  cphi->SetGridx(0);
  cphi->SetGridy(0);
 

  cphi->cd();

  PhiMc1->Rebin(2);
  PhiMc1->SetLineColor(1);
  PhiMc1->Scale(1/2);
  PhiMc1->Scale(PhiMc1->GetSum());

  PhiMc1->SetTitle("Phi");
  PhiMc1->SetXTitle ("Phi");
  PhiMc1->Draw();
  
  //Reconstructed
  PhiM1->Rebin(2);
  PhiM1->Scale(1/2);
  PhiM1->Scale(PhiM1->GetSum());
   
  PhiMc1->SetAxisRange(-3.3, 3.3, "X");
  PhiMc1->SetAxisRange(0, 1.2*PhiMc1->GetMaximum(), "Y");
  
  PhiM1->SetLineColor(2);
  PhiM1->Draw("same");

  PhiG1->Rebin(2);
  PhiG1->Scale(1/2);
  
  PhiG1->Scale(PhiMc1->GetSum()/PhiG1->GetSum()*0.4);//this is just for dAu run 8
  PhiG1->SetLineColor(3);
  PhiG1->Draw("same");

  t.SetTextSize(0.05);
  t.DrawLatex(0.3, 0.9, text1);

  keyLine(.3, 0.18,"MC tracks",1);
  keyLine(.3, 0.13,"MatGlobal RC tracks "+tagTex,2);
  keyLine(.3, 0.10,"Ghost tracks (rescaled)",3);
 
  cphi->Update();
  if (plot)
    {
      sprintf(gif,"~/embedding/%s/%s/Phi_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle,Daughter,  Date);
      cphi->SaveAs(gif);
    }
  // return;
  
  //========================================================
  //rapidity
  //======================================================
	      
  TCanvas *cy= new TCanvas("cy","Rapidity",500, 500);
	      
  cy->SetGridx(0);
  cy->SetGridy(0);
  cy->cd();

  //embedded

  
  YMc1->SetLineColor(1);
  YMc1->SetTitle("Rapidity");
  YMc1->SetXTitle ("y");
  yMc1->SetAxisRange(-1.2, 1.2, "X");
  YMc1->SetAxisRange(0, 1.4* YMc1->GetMaximum(), "Y");
  YMc1->Draw();

  //Reco

  YM1->SetLineColor(2);
  YM1->Draw("same");

 
  t.SetTextSize(0.05);
  t.DrawLatex(0.3, 0.9, text1);

  keyLine(.3, 0.21,"MC tracks",1);
  keyLine(.3, 0.15,"MatGlobal RC tracks "+tagTex,2);
   
 
  cy->Update();
  
  if (plot)
    {
      sprintf(gif,"~/embedding/%s/%s/Rapidity_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle,Daughter, Date);
      cy->SaveAs(gif);
    }
  // return;
 
      */
  //============================================================
  //Dca vs Eta vs pt
  //============================================================
  		
   TCanvas *cde= new TCanvas("cde","DCA_Eta",900, 900);
		
   cde->Divide(nPtBin,nEtaBin);//etabin
   cde->SetGridx(0);
   cde->SetGridy(0);
  
   //Matched  (Bins for Multiplicity)
   
   TH1D *hpt =  (TH1D*)hDcaM->Project3D("X");
   
   
   for (Int_t ipt=0; ipt <nPtBin ; ipt++)
     {
       Int_t bin_pt1 = hpt->FindBin(pt[ipt]);
       Int_t bin_pt2 = hpt->FindBin(pt[ipt+1]);//this should be the same for both graphs (for 3 graphs)
       
       
       TString nameM = "hDcaM";
       TString nameR = "hDcaR";
       TString nameG = "hDcaG";
  
      TH1D *hEtaM = (TH1D*)hDcaM->Project3D("Y");
      TH1D *hEtaR = (TH1D*)hDcaR->Project3D("Y");
      TH1D *hEtaG = (TH1D*)hDcaG->Project3D("Y");


      for(int ieta=0; ieta<nEtaBin ; ieta++)
	{
	  cde->cd(3*ipt+ieta+1);
	
	  Double_t SumMC=0;
	  Double_t SumR=0;
	  Double_t SumG=0;
	 
	  Int_t  bin_eta1 = hEtaM->FindBin(eta[ieta]);
	  Int_t  bin_eta2 = hEtaM->FindBin(eta[ieta+1]);

	  TH1D *hDcaMNew= (TH1D*)hDcaM->ProjectionZ(nameM+ieta,bin_pt1, bin_pt2, bin_eta1, bin_eta2);

	  // hDcaMNew->Sumw2();

	  //  maxMc = hDcaMNew->GetMaximum();
	  //SumMC = hDcaMNew->GetSum();
	 
	  for(int kkk=1;kkk<hDcaMNew->GetNbinsX();kkk++)
	    {
	      	      SumMC += hDcaMNew->GetBinContent(kkk);
	    }
	  
	  hDcaMNew->Scale(1./SumMC);
	  hDcaMNew->SetLineColor(2);
	  hDcaMNew->SetXTitle("Dca (cm)");
      
	  sprintf(title," %.2f GeV < pT < %.2f GeV -  %.2f GeV/c < Eta < %.2f GeV/c",  pt[ipt],pt[ipt+1], eta[ieta],eta[ieta+1]);
	  hDcaMNew->SetTitle(title);
	 
	  //----Now MuDSt

	  Int_t  bin_eta1R = hEtaR->FindBin(eta[ieta]);
	  Int_t  bin_eta2R = hEtaR->FindBin(eta[ieta+1]);

	  TH1D *hDcaRNew= (TH1D*)hDcaR->ProjectionZ(nameR+ieta,bin_pt1, bin_pt2, bin_eta1R, bin_eta2R);
	  // hDcaRNew->Sumw2();

	  //	  SumR =  hDcaRNew->GetSum();
	  for(int kkk=1;kkk<hDcaRNew->GetNbinsX();kkk++)
	    {
	      SumR += hDcaRNew->GetBinContent(kkk);
	  
	    }

	  hDcaRNew->Scale(1./SumR);
	 

	  //---Now Ghost

	  Int_t  bin_eta1G = hEtaG->FindBin(eta[ieta]);
	  Int_t  bin_eta2G = hEtaG->FindBin(eta[ieta+1]);

	  TH1D *hDcaGNew = (TH1D*)hDcaG->ProjectionZ(nameG+ieta, bin_pt1, bin_pt2, bin_eta1G, bin_eta2G);
	  //  hDcaGNew->Sumw2();

	  // SumG = hDcaGNew->GetSum();

	  for(int kkk=1; kkk<hDcaGNew->GetNbinsX();kkk++)
	    {
	      SumG += hDcaGNew->GetBinContent(kkk);
	     
	    }


	  hDcaGNew->Scale(1./SumG);
	  hDcaGNew->SetLineColor(4);
	 
	  hDcaMNew->Draw();
	  hDcaRNew->Draw("esame");
	  // hDcaGNew->Draw("esame");


	  t.SetTextSize(0.045);
	  t.DrawLatex(0.4, 0.9, text1);  

	  t.SetTextSize(0.045);
	  t.SetTextColor(2);
	  t.DrawLatex(0.4, 0.8, "Matched Global "+tagTex);
	  
	  t.SetTextSize(0.045);
	  t.SetTextColor(1);
	  t.DrawLatex(0.4, 0.75, "MuDst");
	  
	  t.SetTextSize(0.045);
	  t.SetTextColor(1);
	  t.DrawLatex(0.4, 0.70, "Ghost Tracks");
	
	  cde->Update();
	     
	  }
	

    }
  cde->Update();
  //  sprintf(gif," ~/embedding/%s/%s/dca_eta_pt_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle, Daughter,Date);
  // cde->SaveAs(gif);
  return;
  
  //===============================================
  //nfit vs eta vs pt
  //====================================================

  TCanvas *cn= new TCanvas("cn","NFIT",1000, 800);
  cn->Divide(nPtBin,nEtaBin);
  cn->SetGridx(0);
  cn->SetGridy(0);

  //Bins for Multiplicity -Matched tracks

  TH1D *hpt =  (TH1D*)hNfitM->Project3D("X");

  for (Int_t ipt=0; ipt <nPtBin ; ipt++)
    {
          	
      Int_t bin_pt1 = hpt->FindBin(pt[ipt]);
      Int_t bin_pt2 = hpt->FindBin(pt[ipt+1]);//this should be the same for both graphs (for 3 graphs)
  
      TString nameM = "hNfitM";
      TString nameR = "hNfitR";
      TString nameG = "hNfitG";

      TH1D *hEtaM = (TH1D*)hNfitM->Project3D("Y");
      TH1D *hEtaR = (TH1D*)hNfitR->Project3D("Y");
      TH1D *hEtaG = (TH1D*)hNfitG->Project3D("Y");


      for(int ieta=0; ieta<nEtaBin ; ieta++)

	{
	  cn->cd(3*ipt+ieta+1);

	  Double_t SumM=0;
	  Double_t SumR=0;
	  Double_t SumG=0;

	  Int_t  bin_eta1 = hEtaM->FindBin(eta[ieta]);
	  Int_t  bin_eta2 = hEtaM->FindBin(eta[ieta+1]);


	  TH1D *hNfitMNew= (TH1D*)hNfitM->ProjectionZ(nameM+ieta,bin_pt1, bin_pt2, bin_eta1, bin_eta2);
	  hNfitMNew->Sumw2();
	  
	  for(int kkk=1;kkk<hNfitMNew->GetNbinsX();kkk++)SumM += hNfitMNew->GetBinContent(kkk);
	  cout<<SumM<<endl;

	  hNfitMNew->Scale(1./SumM);
	  hNfitMNew ->SetLineColor(2);
	  hNfitMNew->Draw();

	  hNfitMNew->SetXTitle("Nfit");

	  sprintf(title," %.2f GeV < pT < %.2f GeV -  %.2f GeV/c < Eta < %.2f GeV/c",  pt[ipt],pt[ipt+1], eta[ieta],eta[ieta+1]);
	  hNfitMNew->SetTitle(title);

	  //----Now MuDSt

	  Int_t  bin_eta1R = hEtaR->FindBin(eta[ieta]);
	  Int_t  bin_eta2R = hEtaR->FindBin(eta[ieta+1]);

	  TH1D *hNfitRNew= (TH1D*)hNfitR->ProjectionZ(nameR+ieta,bin_pt1, bin_pt2, bin_eta1R, bin_eta2R);
	 
	  hNfitRNew->Sumw2();
	  SumR =  hNfitRNew->GetSum();

	  for(int kkk=1;kkk<hNfitR->GetNbinsX();kkk++)SumR += hNfitR->GetBinContent(kkk);
	  cout<<SumR<<endl;
	  hNfitRNew->Scale(1./SumR);
	  hNfitRNew->Draw("esame");


	  //Now Ghost

	  Int_t  bin_eta1G = hEtaG->FindBin(eta[ieta]);
	  Int_t  bin_eta2G = hEtaG->FindBin(eta[ieta+1]);


	  TH1D *hNfitGNew = (TH1D*)hNfitG->ProjectionZ(nameG+ieta,bin_pt1, bin_pt2, bin_eta1, bin_eta2);
	  hNfitGNew->Sumw2();

	  SumG =  hNfitRNew->GetSum();
	  for(int kkk=1;kkk<hNfitGNew->GetNbinsX();kkk++)SumG += hNfitGNew->GetBinContent(kkk);
	  cout<<SumG<<endl;

	  hNfitGNew->Scale(1./SumG);
	  hNfitGNew ->SetLineColor(4);
	  //  hNfitGNew->Draw("same");

	  t.SetTextSize(0.045);
	  t.DrawLatex(0.4, 0.9, text1);  

	  t.SetTextSize(0.045);
	  t.SetTextColor(2);
	  t.DrawLatex(0.4, 0.8, "Matched Global "+tagTex);
	  
	  t.SetTextSize(0.045);
	  t.SetTextColor(1);
	  t.DrawLatex(0.4, 0.75, "MuDst");
	  
	  cn->Update();
	  //  keyLine(0.2, 0.80,"Ghost Tracks",4);
	}

    }
  cn->Update();
  sprintf(gif,"~/embedding/%s/%s/Nfit_eta_pt_%s_%s_%s_%s.gif",Prod, Particle, Prod, Particle,Daughter, Date);
  cn->SaveAs(gif);
  
  return;      
}


  //=====================================================================
  //Following you can use them if need them.
  // Inludes, MIPS, Efficiency, Energy Lost, Dca (just Pt dependant), and NFit ( just pt dependant)//

/*
  //==================================================================================
  //MIPS (just for pions)
  //=============================================================
	  
  if (id==8 || id==9)
    {
    
      TCanvas *c9= new TCanvas("c9","MIPS",500, 500);
    
      c9->SetGridx(0);
      c9->SetGridy(0);
      c9->SetLeftMargin(0.15);
      c9->SetRightMargin(0.05);
      c9->cd();
  
      double pt1 = 0.4;
      double pt2 = 0.6;
  
      dedxG1 -> ProjectionX("rpx");
  
      int blG = rpx->FindBin(pt1);
      int bhG = rpx->FindBin(pt2);
  
      cout<<blG<<endl;
      cout<<bhG<<endl;
  
      dedxG1->ProjectionY("rpy",blG,bhG);
      rpy->SetTitle("MIPS");
      rpy->SetMarkerStyle(22);
      //   rpy->SetMarkerColor(2);
      rpy->SetAxisRange(1.3, 4, "X");
  
      //dedxG1->Draw();
  
      dedx1->ProjectionX("mpx");
  
      int blm = mpx->FindBin(pt1);
      int bhm = mpx->FindBin(pt2);
  
      cout<<blm<<endl;
      cout<<bhm<<endl;
  
      dedx1->ProjectionY("mpy", blm,bhm);
  
      mpy->SetAxisRange(0.5, 6, "X");
      mpy->SetMarkerStyle(22);
      mpy->SetMarkerColor(2);
  
      float max_rpy = rpy->GetMaximum();
      max_rpy /= 1.*mpy->GetMaximum();
      mpy->Scale(max_rpy);
  
      cout<<"max_rpy is: "<<max_rpy<<endl;
      cout<<"mpy is: "<<mpy<<endl;

      rpy->Sumw2();
      mpy->Sumw2();
  
      rpy->Fit("gaus","","",1,4);
      mpy->Fit("gaus","","", 1, 4);
      mpy->GetFunction("gaus")->SetLineColor(2);

      rpy->SetAxisRange(0.5 ,6.0, "X");
      mpy->Draw();
      rpy->Draw("same");

      float mipMc = mpy->GetFunction("gaus")->GetParameter(1);//mean
      float mipGhost  = rpy->GetFunction("gaus")->GetParameter(1);

      float sigmaMc = mpy->GetFunction("gaus")->GetParameter(2);//mean
      float sigmaGhost  = rpy->GetFunction("gaus")->GetParameter(2);

      char  label1[80];
      char  label2[80];
      char  label3[80];
      char  label4[80];


      sprintf(label1,"mip MC %.3f",mipMc);
      sprintf(label2,"mip Ghost %.3f",mipGhost);
      sprintf(label3,"sigma MC %.3f",sigmaMc);
      sprintf(label4,"sigma Ghost %.3f",sigmaGhost);

      keySymbol(.5, .9, label1,2,1);
      keySymbol(.5, .85, label3,2,1);

      keySymbol(.5, .75, label2,1,1);
      keySymbol(.5, .70, label4,1,1);

      keyLine(.2, 1.05,text1,1);

      char name[30];
      sprintf(name,"%.2f GeV/c < Pt < %.2f GeV/c",pt1, pt2);
      keySymbol(0.3, 0.65, name, 1, 1, 0.04);


      c9->Update();

    }//close if pion

	  
  //  pfx1->SetMarkerStyle(2,1);
 


  //================================================================
  //DCA (pt)
  //==================================================================
  /*

  TCanvas *c= new TCanvas("c","DCA",400, 800);
  c->Divide(1,nPtBin);
  c->SetGridx(0);
  c->SetGridy(0);

  //Matched  (Bins for Multiplicity)

  TH1D *hX1 =  (TH1D*)hDca1->Project3D("X");

  Int_t bin_nch1 = hX1->FindBin(nch1);
  Int_t bin_nch2 = hX1->FindBin(nch2);//this should be the same for both graphs (for 3 graphs)

  cout<<bin_nch1<<" "<<bin_nch2<<endl;

  //Bins for Pt 

  TString name1 = "hDca1";
  TString namer1 = "hDcar1";

  TString namexx = "hDca";


  TH1D *hY1 =  (TH1D*)hDca1->Project3D("Y");
  TH1D *hY1_r = (TH1D*)hDca1r->Project3D("Y");

  TH1D *hYG = (TH1D*)hDcaG1->Project3D("Y");

  for(Int_t i=0; i<nPtBin ; i++)
  {
  c->cd(i+1);

  Double_t Sum1_MC=0;
  Double_t Sum1_Real=0;
  Double_t Sum_MC=0;

  Int_t  bin_ptl_1 = hY1->FindBin(pt[i]);
  Int_t  bin_pth_1 = hY1->FindBin(pt[i+1]);

  TH1D *hDcaNew1= (TH1D*)hDca1->ProjectionZ(name1+i,bin_nch1, bin_nch2, bin_ptl_1, bin_pth_1);
  //Sum1_MC =  hDcaNew1 ->GetSum();
  for(int kkk=1;kkk<hDcaNew1->GetNbinsX();kkk++)Sum1_MC += hDcaNew1->GetBinContent(kkk);
  cout<<Sum1_MC<<endl;

  hDcaNew1->Scale(1./Sum1_MC);
  hDcaNew1 ->SetLineColor(2);
  hDcaNew1->Draw();

  hDcaNew1->SetXTitle("Dca (cm)");

  //sprintf(title," %.2f GeV < pT < %.2f GeV, %d < nch < %d", pt[i],pt[i+1],nch1,nch2);
  sprintf(title," %.2f GeV/c < pT < %.2f GeV/c", pt[i],pt[i+1]);
  //hDcaNew1->SetTitle("Scale factor = 1.38");
  hDcaNew1->SetTitle(title);

  //----Now MuDSt

  Int_t  bin_ptrl_1r = hY1_r->FindBin(pt[i]);
  Int_t  bin_ptrh_1r = hY1_r->FindBin(pt[i+1]);

  TH1D *hDca_r1= (TH1D*)hDca1r->ProjectionZ(namer1+i,bin_nch1, bin_nch2, bin_ptrl_1r, bin_ptrh_1r);
  //Sum1_Real =  hDca_r1 ->GetSum();
  for(int kkk=1;kkk<hDca_r1->GetNbinsX();kkk++)Sum1_Real += hDca_r1->GetBinContent(kkk);
  cout<<Sum1_Real<<endl;

  hDca_r1->Scale(1./Sum1_Real);
  //hDca_r1->Draw("same");

  //Now Previous Embedding

  Int_t  bin_ptl = hYG->FindBin(pt[i]);
  Int_t  bin_pth = hYG->FindBin(pt[i+1]);

  TH1D *hDcaNew = (TH1D*)hDcaG1->ProjectionZ(namexx+i,bin_nch1, bin_nch2, bin_ptl, bin_pth);
  //Sum_MC =  hDcaNew ->GetSum();
  for(int kkk=1;kkk<hDcaNew->GetNbinsX();kkk++)Sum_MC += hDcaNew->GetBinContent(kkk);
  cout<<Sum_MC<<endl;

  hDcaNew->Scale(1./Sum_MC);
  hDcaNew ->SetLineColor(4);
  hDcaNew->Draw("same");

  //keySymbol(.4, .95,text1,1,1);
  keyLine(0.4, 0.90,"MatGlobal Tracks",2);
  //keyLine(0.4, 0.85,"MuDst",1);
  keyLine(0.4, 0.80,"Ghost Tracks",4);
  }


  c->Update();
  c->SaveAs("/home/mcsuarez/Embed/P08id/dca_f1.38.gif");

		


  //===================================================
  //NFIT 
  //=============================================================

  TCanvas *c1= new TCanvas("c1","NFIT",400, 800);
  c1->Divide(1,nPtBin);
  c1->SetGridx(0);
  c1->SetGridy(0);

  //Bins for Multiplicity -Matched tracks

  TH1D *hX1 =  (TH1D*)hNfit1->Project3D("X");

  Int_t bin_nch1 = hX1->FindBin(nch1);
  Int_t bin_nch2 = hX1->FindBin(nch2);//this should be the same for both graphs (for 3 graphs)

  //Bins for Pt 

  TString name_nfit1 = "hNfit1";
  TString name_nfitr1 = "hNfitr1";
  TString name_nfit = "hNfit";

  TH1D *hY1 =  (TH1D*)hNfit1->Project3D("Y");
  TH1D *hY1_r = (TH1D*)hNfit1r->Project3D("Y");
  TH1D *hYG = (TH1D*)hNfitG1->Project3D("Y");

  for(Int_t i=0; i<nPtBin ; i++)

  {
  c1->cd(i+1);

  Double_t Sum1_Nfit_MC = 0;
  Double_t Sum1_Nfit_Real = 0;
  Double_t Sum_Nfit_MC = 0;

  Int_t  bin_ptl_1 = hY1->FindBin(pt[i]);
  Int_t  bin_pth_1 = hY1->FindBin(pt[i+1]);

  TH1D *hNfitNew1= (TH1D*)hNfit1->ProjectionZ(name_nfit1+i,bin_nch1, bin_nch2, bin_ptl_1, bin_pth_1);
  //Sum1_Nfit_MC =  hNfitNew1 ->GetSum();
  for(int kkk=1;kkk<hNfitNew1->GetNbinsX();kkk++)Sum1_Nfit_MC += hNfitNew1->GetBinContent(kkk);
  cout<<Sum1_Nfit_MC<<endl;

  hNfitNew1->Scale(1./Sum1_Nfit_MC);
  hNfitNew1 ->SetLineColor(2);
  hNfitNew1->Draw();

  hNfitNew1->SetXTitle("Nfit");

  //sprintf(title," %.2f GeV < pT < %.2f GeV, %d < nch < %d", pt[i],pt[i+1],nch1,nch2);
  sprintf(title," %.2f GeV/c < pT < %.2f GeV/c", pt[i],pt[i+1],nch1,nch2);
  //hNfitNew1->SetTitle("Scale factor = 1.38");
  hNfitNew1->SetTitle(title);

  //----Now MuDSt

  Int_t  bin_ptrl_1r = hY1_r->FindBin(pt[i]);
  Int_t  bin_ptrh_1r = hY1_r->FindBin(pt[i+1]);

  TH1D *hNfit_r1= (TH1D*)hNfit1r->ProjectionZ(name_nfitr1+i,bin_nch1, bin_nch2, bin_ptrl_1r, bin_ptrh_1r);
  //Sum1_Nfit_Real =  hNfit_r1 ->GetSum();

  for(int kkk=1;kkk<hNfit_r1->GetNbinsX();kkk++)Sum1_Nfit_Real += hNfit_r1->GetBinContent(kkk);
  cout<<Sum1_Nfit_Real<<endl;
  hNfit_r1->Scale(1./Sum1_Nfit_Real);
  //hNfit_r1->Draw("same");

  //Now Previous Embedding

  Int_t  bin_ptl = hYG->FindBin(pt[i]);
  Int_t  bin_pth = hYG->FindBin(pt[i+1]);

  TH1D *hNfitNew = (TH1D*)hNfitG1->ProjectionZ(name_nfit+i,bin_nch1, bin_nch2, bin_ptl, bin_pth);
  //Sum_Nfit_MC =  hNfitNew ->GetSum();
  for(int kkk=1;kkk<hNfitNew->GetNbinsX();kkk++)Sum_Nfit_MC += hNfitNew->GetBinContent(kkk);
  cout<<Sum_Nfit_MC<<endl;

  hNfitNew->Scale(1./Sum_Nfit_MC);
  hNfitNew ->SetLineColor(4);
  hNfitNew->Draw("same");

  //keySymbol(.2, .95,text1,1,1);
  keyLine(0.2, 0.90,"MatGlobal Tracks",2);
  //keyLine(0.2, 0.85,"MuDst",1);
  keyLine(0.2, 0.80,"Ghost Tracks",4);
  }

  c1->Update();
  c1->SaveAs("/home/mcsuarez/Embed/P08id/nfit_f1.38.gif");
		
  return;      



  // ===============================================================
  //Efficiency vs pT
  //this make sense just when reconstructed particels is the same as embedded (?)

  void PlotEfficiency(const TH1* hData, const TH1* hMC)
  {
  TCanvas *c1= new TCanvas(Form("c%d", iCanvas), Form("c%d", iCanvas));
  hData->Rebin(2);
  hMC->Rebin(2);

  TH1* hRatio = (TH1D*) hData->Clone();
  hRatio->Divide(hMC);
  hRatio->SetLineColor(1);
  hRatio->SetMarkerStyle(23);
  hRatio->SetMarkerColor(1);
  hRatio->SetXTitle ("p_{T} (GeV/c)");
  hRatio->SetAxisRange(0.0, 6.0, "X");

  hRatio->Draw();

  c1->cd();
  c1->Update();
  c1->SaveAs("eff.gif");

  //  PtM2->Rebin(2);
  //  PtMc2->Rebin(2);
  // 
  //  PtM2->Divide(PtMc2);
  //  PtM2->SetLineColor(9);
  //  PtM2->SetMarkerStyle(21);
  //  PtM2->SetMarkerColor(9);
  //  PtM2->Draw("same");
  // 
  //  PtM3->Rebin(2);
  //  PtMc3->Rebin(2);
  // 
  //  PtM3->Divide(PtMc2);
  //  PtM3->SetLineColor(2);
  //  PtM3->SetMarkerStyle(22);
  //  PtM3->SetMarkerColor(2);
  //  PtM3->Draw("same");


  //============================================================
  // Vertex xx yy zz
  //Not too much sense

  TCanvas *cv1= new TCanvas("cv1","Vertex position",600, 400);
  
  cv1->Divide(3,1);
  
  cv1_1->cd();
 
  vxx1->SetXTitle("MonteCarlo Vertex X");
  vxx1->SetYTitle("Matched Vertex X");

  vxx1->Draw();

  cv1_2->cd();
   
  vyy1->SetXTitle("MonteCarlo Vertex Y");
  vyy1->SetYTitle("Matched Vertex Y");
  vyy1->Draw();

  cv1_3->cd();
   
  vzz1->SetXTitle("MonteCarlo Vertex Z");
  vzz1->SetYTitle("Matched Vertex Z");
  vzz1->Draw();

  cv1->Update();
  cv1->SaveAs("/home/mcsuarez/Embed/P08id/Vertex_xyz.gif");



  //========================================================================
  //Energy loss
  //=================================================================

  TCanvas *c7= new TCanvas("c7","Energy Loss",400, 400);

  c7->SetGridx(0);
  c7->SetGridy(0);
  c7->SetLeftMargin(0.20);
  c7->SetRightMargin(0.05);
  c7->cd();

  hPtM_E1->ProfileX("pfx");
  pfx->SetAxisRange(-0.01, 0.01, "Y");
  pfx->SetAxisRange(0, 2.5, "X");
  pfx->GetYaxis()->SetDecimals();
  pfx->SetMarkerStyle(23);
  pfx->SetMarkerSize(0.038);
  pfx->SetMarkerColor(4);
  pfx->SetLineColor(4);
  pfx->SetXTitle ("Pt-Reco");
  pfx->SetYTitle ("ptM - PtMc");
  pfx->SetTitleOffset(2,"Y");

  pfx->Draw();


  hPtM_E1->ProfileX("pfx1");
  pfx1->SetAxisRange(-0.007, 0.007, "Y");
  pfx1->GetYaxis()->SetDecimals();
  pfx1->SetLineColor(2);
  pfx1->SetMarkerSize(0.035);
  pfx1->SetXTitle ("Pt-Reco");
  pfx1->SetYTitle ("ptM - PtMc");
  pfx1->SetTitleOffset(2,"Y");

  pfx1->Draw("same");


  c7->Update();
  c7->SaveAs("/home/mcsuarez/Embed/P08id/eloss_f1.38.gif");


  //====================================================
  //Dedx (p08ic)
  //=======================================================

  
  TCanvas *c88= new TCanvas("c88","dEdx vs P (P08ic)", 600, 600);

  c88->SetGridx(0);
  c88->SetGridy(0);
  c88->SetLeftMargin(0.15);
  c88->SetRightMargin(0.05);
  c88->cd();
  gStyle->SetOptDate(0);

  dedx1R->SetTitle("dEdx vs P (P08ic)");
  dedx1R->SetXTitle("Momentum P (GeV/c)");
  dedx1R->SetYTitle("dE/dx (x10^{-6})");
  dedx1R->SetAxisRange(0, 5., "X");
  dedx1R->SetAxisRange(0, 8., "Y");
  dedx1R->SetMarkerColor(1);
  dedx1R->Draw();//"colz");

  c88->Update();
  c88->SaveAs("dedx_p08ic.gif");
  */
