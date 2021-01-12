#include <TFile.h>
#include <TStyle.h>
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TString.h>
#include <iostream>

using namespace std;

void SetDiffStyle(TH1* h, Int_t mLumi=0, Int_t mCharge=0);
//_________________
void MakeLumiDependence(const Char_t* in15LumiFName = "oGmtTree_2014AuAu15_db5.root",
			const Char_t* inMidLumiFName = "oGmtTree_2014AuAu200Mid_db5.root",
			const Char_t* inHiLumiFName = "oGmtTree_2014AuAu200Hi_db5.root",
			const Char_t* inLowLumiFName = "oGmtTree_2014AuAu200Low_db5.root",
			const Char_t* oFileName = "oLumiDependence_db5.root") {
  gStyle->SetOptStat(0);
  //gStyle->SetOptTit(0);

  TFile* mInFile[4];
  mInFile[0] = TFile::Open(in15LumiFName);
  mInFile[1] = TFile::Open(inMidLumiFName);
  mInFile[2] = TFile::Open(inHiLumiFName);
  mInFile[3] = TFile::Open(inLowLumiFName);
  TFile* oFile = new TFile(oFileName,"recreate");

  //Sum of charges
  TH1F* hUdiffVsModuleSum1 = (TH1F*)mInFile[0]->Get("hUdiffVsBarrel_1");
  hUdiffVsModuleSum1->SetNameTitle("hUdiffVsModuleSum1","hUdiffVsModuleSum1");
  TH1F* hUdiffVsModuleSum2 = (TH1F*)mInFile[1]->Get("hUdiffVsBarrel_1");
  hUdiffVsModuleSum2->SetNameTitle("hUdiffVsModuleSum2","hUdiffVsModuleSum2");
  TH1F* hUdiffVsModuleSum3 = (TH1F*)mInFile[2]->Get("hUdiffVsBarrel_1");
  hUdiffVsModuleSum3->SetNameTitle("hUdiffVsModuleSum3","hUdiffVsModuleSum3");
  TH1F* hUdiffVsModuleSum4 = (TH1F*)mInFile[3]->Get("hUdiffVsBarrel_1");
  hUdiffVsModuleSum4->SetNameTitle("hUdiffVsModuleSum4","hUdiffVsModuleSum4");

  TH1F* hVdiffVsModuleSum1 = (TH1F*)mInFile[0]->Get("hVdiffVsBarrel_1");
  hVdiffVsModuleSum1->SetNameTitle("hVdiffVsModuleSum1","hVdiffVsModuleSum1");
  TH1F* hVdiffVsModuleSum2 = (TH1F*)mInFile[1]->Get("hVdiffVsBarrel_1");
  hVdiffVsModuleSum2->SetNameTitle("hVdiffVsModuleSum2","hVdiffVsModuleSum2");
  TH1F* hVdiffVsModuleSum3 = (TH1F*)mInFile[2]->Get("hVdiffVsBarrel_1");
  hVdiffVsModuleSum3->SetNameTitle("hVdiffVsModuleSum3","hVdiffVsModuleSum3");
  TH1F* hVdiffVsModuleSum4 = (TH1F*)mInFile[3]->Get("hVdiffVsBarrel_1");
  hVdiffVsModuleSum4->SetNameTitle("hVdiffVsModuleSum4","hVdiffVsModuleSum4");

  //Positive charge
  TH1F* hUdiffVsModulePosCharge1 = (TH1F*)mInFile[0]->Get("hUdiffVsBarrelPosCharge_1");
  hUdiffVsModulePosCharge1->SetNameTitle("hUdiffVsModulePosCharge1","hUdiffVsModulePosCharge1");
  TH1F* hUdiffVsModulePosCharge2 = (TH1F*)mInFile[1]->Get("hUdiffVsBarrelPosCharge_1");
  hUdiffVsModulePosCharge2->SetNameTitle("hUdiffVsModulePosCharge2","hUdiffVsModulePosCharge2");
  TH1F* hUdiffVsModulePosCharge3 = (TH1F*)mInFile[2]->Get("hUdiffVsBarrelPosCharge_1");
  hUdiffVsModulePosCharge3->SetNameTitle("hUdiffVsModulePosCharge3","hUdiffVsModulePosCharge3");
  TH1F* hUdiffVsModulePosCharge4 = (TH1F*)mInFile[3]->Get("hUdiffVsBarrelPosCharge_1");
  hUdiffVsModulePosCharge4->SetNameTitle("hUdiffVsModulePosCharge4","hUdiffVsModulePosCharge4");

  TH1F* hVdiffVsModulePosCharge1 = (TH1F*)mInFile[0]->Get("hVdiffVsBarrelPosCharge_1");
  hVdiffVsModulePosCharge1->SetNameTitle("hVdiffVsModulePosCharge1","hVdiffVsModulePosCharge1");
  TH1F* hVdiffVsModulePosCharge2 = (TH1F*)mInFile[1]->Get("hVdiffVsBarrelPosCharge_1");
  hVdiffVsModulePosCharge2->SetNameTitle("hVdiffVsModulePosCharge2","hVdiffVsModulePosCharge2");
  TH1F* hVdiffVsModulePosCharge3 = (TH1F*)mInFile[2]->Get("hVdiffVsBarrelPosCharge_1");
  hVdiffVsModulePosCharge3->SetNameTitle("hVdiffVsModulePosCharge3","hVdiffVsModulePosCharge3");
  TH1F* hVdiffVsModulePosCharge4 = (TH1F*)mInFile[3]->Get("hVdiffVsBarrelPosCharge_1");
  hVdiffVsModulePosCharge4->SetNameTitle("hVdiffVsModulePosCharge4","hVdiffVsModulePosCharge4");

  //Negative charge
  TH1F* hUdiffVsModuleNegCharge1 = (TH1F*)mInFile[0]->Get("hUdiffVsBarrelNegCharge_1");
  hUdiffVsModuleNegCharge1->SetNameTitle("hUdiffVsModuleNegCharge1","hUdiffVsModuleNegCharge1");
  TH1F* hUdiffVsModuleNegCharge2 = (TH1F*)mInFile[1]->Get("hUdiffVsBarrelNegCharge_1");
  hUdiffVsModuleNegCharge2->SetNameTitle("hUdiffVsModuleNegCharge2","hUdiffVsModuleNegCharge2");
  TH1F* hUdiffVsModuleNegCharge3 = (TH1F*)mInFile[2]->Get("hUdiffVsBarrelNegCharge_1");
  hUdiffVsModuleNegCharge3->SetNameTitle("hUdiffVsModuleNegCharge3","hUdiffVsModuleNegCharge3");
  TH1F* hUdiffVsModuleNegCharge4 = (TH1F*)mInFile[3]->Get("hUdiffVsBarrelNegCharge_1");
  hUdiffVsModuleNegCharge4->SetNameTitle("hUdiffVsModuleNegCharge4","hUdiffVsModuleNegCharge4");

  TH1F* hVdiffVsModuleNegCharge1 = (TH1F*)mInFile[0]->Get("hVdiffVsBarrelNegCharge_1");
  hVdiffVsModuleNegCharge1->SetNameTitle("hVdiffVsModuleNegCharge1","hVdiffVsModuleNegCharge1");
  TH1F* hVdiffVsModuleNegCharge2 = (TH1F*)mInFile[1]->Get("hVdiffVsBarrelNegCharge_1");
  hVdiffVsModuleNegCharge2->SetNameTitle("hVdiffVsModuleNegCharge2","hVdiffVsModuleNegCharge2");
  TH1F* hVdiffVsModuleNegCharge3 = (TH1F*)mInFile[2]->Get("hVdiffVsBarrelNegCharge_1");
  hVdiffVsModuleNegCharge3->SetNameTitle("hVdiffVsModuleNegCharge3","hVdiffVsModuleNegCharge3");
  TH1F* hVdiffVsModuleNegCharge4 = (TH1F*)mInFile[3]->Get("hVdiffVsBarrelNegCharge_1");
  hVdiffVsModuleNegCharge4->SetNameTitle("hVdiffVsModuleNegCharge4","hVdiffVsModuleNegCharge4");

  Int_t    mModuleBins = 8;
  Double_t mModuleMin = -0.5;
  Double_t mModuleMax = 7.5;

  //Differences
  //Sum of charges
  TH1F* hUdiffSumHiMinus15 = new TH1F("hUdiffSumHiMinus15",
				      "Sum <uD-uP> vs module for AuAu200Hi - AuAu15;Module # ;<uD-uP> (cm)",
				      mModuleBins,mModuleMin,mModuleMax);
  hUdiffSumHiMinus15->Sumw2();
  TH1F* hUdiffSumMidMinus15 = new TH1F("hUdiffSumMidMinus15",
				       "Sum <uD-uP> vs module for AuAu200Mid - AuAu15;Module # ;<uD-uP> (cm)",
				       mModuleBins,mModuleMin,mModuleMax);
  hUdiffSumMidMinus15->Sumw2();
  TH1F* hUdiffSumLowMinus15 = new TH1F("hUdiffSumLowMinus15",
				       "Sum <uD-uP> vs module for AuAu200Low - AuAu15;Module # ;<uD-uP> (cm)",
				       mModuleBins,mModuleMin,mModuleMax);
  hUdiffSumMidMinus15->Sumw2();

  TH1F* hVdiffSumHiMinus15 = new TH1F("hVdiffSumHiMinus15",
				      "Sum <vD-vP> vs module for AuAu200Hi - AuAu15;Module # ;<vD-vP> (cm)",
				      mModuleBins,mModuleMin,mModuleMax);
  hVdiffSumHiMinus15->Sumw2();
  TH1F* hVdiffSumMidMinus15 = new TH1F("hVdiffSumMidMinus15",
				       "Sum <vD-vP> vs module for AuAu200Mid - AuAu15;Module # ;<vD-vP> (cm)",
				       mModuleBins,mModuleMin,mModuleMax);
  hVdiffSumMidMinus15->Sumw2();
  TH1F* hVdiffSumLowMinus15 = new TH1F("hVdiffSumLowMinus15",
				       "Sum <vD-vP> vs module for AuAu200Low - AuAu15;Module # ;<vD-vP> (cm)",
				       mModuleBins,mModuleMin,mModuleMax);
  hVdiffSumLowMinus15->Sumw2();

  //Positive charge
  TH1F* hUdiffPosChargeHiMinus15 = new TH1F("hUdiffPosChargeHiMinus15",
					    "Pos charge <uD-uP> vs module for AuAu200Hi - AuAu15;Module # ;<uD-uP> (cm)",
					    mModuleBins,mModuleMin,mModuleMax);
  hUdiffPosChargeHiMinus15->Sumw2();
  TH1F* hUdiffPosChargeMidMinus15 = new TH1F("hUdiffPosChargeMidMinus15",
					     "Pos charge <uD-uP> vs module for AuAu200Mid - AuAu15;Module # ;<uD-uP> (cm)",
					     mModuleBins,mModuleMin,mModuleMax);
  hUdiffPosChargeMidMinus15->Sumw2();
  TH1F* hUdiffPosChargeLowMinus15 = new TH1F("hUdiffPosChargeLowMinus15",
				       "Pos charge <uD-uP> vs module for AuAu200Low - AuAu15;Module # ;<uD-uP> (cm)",
				       mModuleBins,mModuleMin,mModuleMax);
  hUdiffPosChargeLowMinus15->Sumw2();

  
  TH1F* hVdiffPosChargeHiMinus15 = new TH1F("hVdiffPosChargeHiMinus15",
					    "Pos charge <vD-vP> vs module for AuAu200Hi - AuAu15;Module # ;<vD-vP> (cm)",
					    mModuleBins,mModuleMin,mModuleMax);
  hVdiffPosChargeHiMinus15->Sumw2();
  TH1F* hVdiffPosChargeMidMinus15 = new TH1F("hVdiffPosChargeMidMinus15",
					     "Pos charge <vD-vP> vs module for AuAu200Mid - AuAu15;Module # ;<vD-vP> (cm)",
					     mModuleBins,mModuleMin,mModuleMax);
  TH1F* hVdiffPosChargeLowMinus15 = new TH1F("hVdiffPosChargeLowMinus15",
				       "Pos charge <vD-vP> vs module for AuAu200Low - AuAu15;Module # ;<vD-vP> (cm)",
				       mModuleBins,mModuleMin,mModuleMax);
  hVdiffPosChargeLowMinus15->Sumw2();

  //Negative charge
  TH1F* hUdiffNegChargeHiMinus15 = new TH1F("hUdiffNegChargeHiMinus15",
					    "Neg charge <uD-uP> vs module for AuAu200Hi - AuAu15;Module # ;<uD-uP> (cm)",
					    mModuleBins,mModuleMin,mModuleMax);
  hUdiffNegChargeHiMinus15->Sumw2();
  TH1F* hUdiffNegChargeMidMinus15 = new TH1F("hUdiffNegChargeMidMinus15",
					     "Neg charge <uD-uP> vs module for AuAu200Mid - AuAu15;Module # ;<uD-uP> (cm)",
					     mModuleBins,mModuleMin,mModuleMax);
  hUdiffNegChargeMidMinus15->Sumw2();
  TH1F* hUdiffNegChargeLowMinus15 = new TH1F("hUdiffNegChargeLowMinus15",
				       "Neg charge <uD-uP> vs module for AuAu200Low - AuAu15;Module # ;<uD-uP> (cm)",
				       mModuleBins,mModuleMin,mModuleMax);
  hUdiffNegChargeLowMinus15->Sumw2();
  
  TH1F* hVdiffNegChargeHiMinus15 = new TH1F("hVdiffNegChargeHiMinus15",
					    "Neg charge <vD-vP> vs module for AuAu200Hi - AuAu15;Module # ;<vD-vP> (cm)",
					    mModuleBins,mModuleMin,mModuleMax);
  hVdiffNegChargeHiMinus15->Sumw2();
  TH1F* hVdiffNegChargeMidMinus15 = new TH1F("hVdiffNegChargeMidMinus15",
					     "Neg charge <vD-vP> vs module for AuAu200Mid - AuAu15;Module # ;<vD-vP> (cm)",
					     mModuleBins,mModuleMin,mModuleMax);
  hVdiffNegChargeMidMinus15->Sumw2();
  TH1F* hVdiffNegChargeLowMinus15 = new TH1F("hVdiffNegChargeLowMinus15",
				       "Neg charge <vD-vP> vs module for AuAu200Low - AuAu15;Module # ;<vD-vP> (cm)",
				       mModuleBins,mModuleMin,mModuleMax);
  hVdiffNegChargeLowMinus15->Sumw2();


  //Sum of charges
  hUdiffSumHiMinus15  -> Add(hUdiffVsModuleSum3, hUdiffVsModuleSum1, 1, -1);
  hUdiffSumMidMinus15 -> Add(hUdiffVsModuleSum2, hUdiffVsModuleSum1, 1, -1);
  hUdiffSumLowMinus15 -> Add(hUdiffVsModuleSum4, hUdiffVsModuleSum1, 1, -1);
  hVdiffSumHiMinus15  -> Add(hVdiffVsModuleSum3, hVdiffVsModuleSum1, 1, -1);
  hVdiffSumMidMinus15 -> Add(hVdiffVsModuleSum2, hVdiffVsModuleSum1, 1, -1);
  hVdiffSumLowMinus15 -> Add(hVdiffVsModuleSum4, hVdiffVsModuleSum1, 1, -1);

  SetDiffStyle(hUdiffSumHiMinus15, 1, 0);
  SetDiffStyle(hUdiffSumMidMinus15, 0, 0);
  SetDiffStyle(hUdiffSumLowMinus15, 2, 0);
  SetDiffStyle(hVdiffSumHiMinus15, 1, 0);
  SetDiffStyle(hVdiffSumMidMinus15, 0, 0);
  SetDiffStyle(hVdiffSumLowMinus15, 2, 0);

  //Positive charge
  hUdiffPosChargeHiMinus15  -> Add(hUdiffVsModulePosCharge3, hUdiffVsModulePosCharge1, 1, -1);
  hUdiffPosChargeMidMinus15 -> Add(hUdiffVsModulePosCharge2, hUdiffVsModulePosCharge1, 1, -1);
  hUdiffPosChargeLowMinus15 -> Add(hUdiffVsModulePosCharge4, hUdiffVsModulePosCharge1, 1, -1);

  hVdiffPosChargeHiMinus15  -> Add(hVdiffVsModulePosCharge3, hVdiffVsModulePosCharge1, 1, -1);
  hVdiffPosChargeMidMinus15 -> Add(hVdiffVsModulePosCharge2, hVdiffVsModulePosCharge1, 1, -1);
  hVdiffPosChargeLowMinus15 -> Add(hVdiffVsModulePosCharge4, hVdiffVsModulePosCharge1, 1, -1);

  SetDiffStyle(hUdiffPosChargeHiMinus15, 1, 1);
  SetDiffStyle(hUdiffPosChargeMidMinus15, 0, 1);
  SetDiffStyle(hUdiffPosChargeLowMinus15, 2, 1);
  SetDiffStyle(hVdiffPosChargeHiMinus15, 1, 1);
  SetDiffStyle(hVdiffPosChargeMidMinus15, 0, 1);
  SetDiffStyle(hVdiffPosChargeLowMinus15, 2, 1);

  //Negative charge
  hUdiffNegChargeHiMinus15  -> Add(hUdiffVsModuleNegCharge3, hUdiffVsModuleNegCharge1, 1, -1);
  hUdiffNegChargeMidMinus15 -> Add(hUdiffVsModuleNegCharge2, hUdiffVsModuleNegCharge1, 1, -1);
  hUdiffNegChargeLowMinus15 -> Add(hUdiffVsModuleNegCharge4, hUdiffVsModuleNegCharge1, 1, -1);
  hVdiffNegChargeHiMinus15  -> Add(hVdiffVsModuleNegCharge3, hVdiffVsModuleNegCharge1, 1, -1);
  hVdiffNegChargeMidMinus15 -> Add(hVdiffVsModuleNegCharge2, hVdiffVsModuleNegCharge1, 1, -1);
  hVdiffNegChargeLowMinus15 -> Add(hVdiffVsModuleNegCharge4, hVdiffVsModuleNegCharge1, 1, -1);

  SetDiffStyle(hUdiffNegChargeHiMinus15, 1, 2);
  SetDiffStyle(hUdiffNegChargeMidMinus15, 0, 2);
  SetDiffStyle(hUdiffNegChargeLowMinus15, 2, 2);
  SetDiffStyle(hVdiffNegChargeHiMinus15, 1, 2);
  SetDiffStyle(hVdiffNegChargeMidMinus15, 0, 2);
  SetDiffStyle(hVdiffNegChargeLowMinus15, 2, 2);

  TCanvas* cDiffCanvas = new TCanvas("cDiffCanvas","cDiffCanvas",1200,800);
  cDiffCanvas->Divide(3,2);

  TLegend *lLumiLegend, *lChargeLegend;
  lLumiLegend = new TLegend(0.2,0.6,0.7,0.95);
  lLumiLegend->SetFillColor(kWhite);
  lLumiLegend->AddEntry(hUdiffSumLowMinus15, Form("AuAu200 LowLumi - AuAu15"),"p");
  lLumiLegend->AddEntry(hUdiffSumMidMinus15, Form("AuAu200 MidLumi - AuAu15"),"p");
  lLumiLegend->AddEntry(hUdiffSumHiMinus15, Form("AuAu200 HiLumi - AuAu15"),"p");


  lChargeLegend = new TLegend(0.2,0.6,0.7,0.95);
  lChargeLegend->SetFillColor(kWhite);
  lChargeLegend->AddEntry(hUdiffSumHiMinus15, Form("Sum of charges"),"p");
  lChargeLegend->AddEntry(hUdiffPosChargeHiMinus15, Form("Positive charge"),"p");
  lChargeLegend->AddEntry(hUdiffNegChargeHiMinus15, Form("Negative charge"),"p");

  cDiffCanvas->cd(1);
  hUdiffSumHiMinus15->Draw();
  hUdiffSumMidMinus15->Draw("same");
  hUdiffSumLowMinus15->Draw("same");
  lLumiLegend->Draw();
  gPad->SetGrid();

  cDiffCanvas->cd(2);
  hUdiffPosChargeHiMinus15->Draw();
  hUdiffPosChargeMidMinus15->Draw("same");
  hUdiffPosChargeLowMinus15->Draw("same");
  lChargeLegend->Draw();
  gPad->SetGrid();

  cDiffCanvas->cd(3);
  hUdiffNegChargeHiMinus15->Draw();
  hUdiffNegChargeMidMinus15->Draw("same");
  hUdiffNegChargeLowMinus15->Draw("same");
  gPad->SetGrid();

  cDiffCanvas->cd(4);
  hVdiffSumHiMinus15->Draw();
  hVdiffSumMidMinus15->Draw("same");
  hVdiffSumLowMinus15->Draw("same");
  gPad->SetGrid();

  cDiffCanvas->cd(5);
  hVdiffPosChargeHiMinus15->Draw();
  hVdiffPosChargeMidMinus15->Draw("same");
  hVdiffPosChargeLowMinus15->Draw("same");
  gPad->SetGrid();

  cDiffCanvas->cd(6);
  hVdiffNegChargeHiMinus15->Draw();
  hVdiffNegChargeMidMinus15->Draw("same");
  hVdiffNegChargeLowMinus15->Draw("same");
  gPad->SetGrid();
  
  
  oFile->Write();
  cDiffCanvas->Write();
  oFile->Close();
}

//_________________
void SetDiffStyle(TH1* histo, Int_t mLumi, Int_t mCharge) {
  
  //mLumi: 0 = 200Mid - 15; 1 = 200Hi - 15;
  //mCharge: 0 = Sum; 1 = Pos; 2 = Neg
  Int_t mMarkerStyle, mColor, mLineWidth;
  Double_t mMarkerSize;

  Double_t mYaxisMin = -0.5;
  Double_t mYaxisMax = 0.5;

  if(mLumi<0 || mLumi>2) {
    cout << "Wrong luminosity: " << mLumi << endl;
  }
  if(mCharge<0 || mCharge>2) {
    cout << "Wrong charge: " << mCharge << endl;
  }

  mLineWidth = 2;
  mMarkerSize = 1.3;

  if(mLumi == 0) {
    mMarkerStyle = 24;
  }
  else if(mLumi == 1) {
    mMarkerStyle = 20;
  }
  else {
    mMarkerStyle = 22;
  }

  if(mCharge == 0) {
    mColor = 1;
  }
  else if(mCharge == 1) {
    mColor = 2;
  }
  else {
    mColor = 4;
  }

  histo->SetLineWidth(mLineWidth);
  histo->SetLineColor(mColor);
  histo->SetMarkerStyle(mMarkerStyle);
  histo->SetMarkerSize(mMarkerSize);
  histo->SetMarkerColor(mColor);

  histo->GetYaxis()->SetRangeUser(mYaxisMin,mYaxisMax);
}
