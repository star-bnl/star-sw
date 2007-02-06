///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotLYZ.C,v 1.4 2007/02/06 19:00:53 posk Exp $
//
// Plot histograms from flow.LeeYang.Zeros.root
//
// by Markus Oldenberg and Art Poskanzer
//
///////////////////////////////////////////////
#include "TFile.h"
#include "TCanvas.h"
#include "TH1F.h"
#include "TLine.h"
#include "TMath.h"
#include <iostream.h>
#include <iomanip.h>

void plotLYZ(TString fileName = "flow.hist.root", Bool_t firstPass = kFALSE, char* ext = "") {

  Bool_t mevSim, FTPC = kFALSE;
  //Bool_t mevSim = kTRUE;
  Bool_t FTPC = kTRUE;

  gROOT->SetStyle("Pub");                              // set style
  gStyle->SetFrameLineWidth(2);
  gStyle->SetLineWidth(2);
  gStyle->SetLineColor(kBlue);
  gStyle->SetHistLineWidth(1);
  gStyle->SetFuncWidth(2);
//   gStyle->SetMarkerSize(2);
//   gStyle->SetMarkerStyle(kFullCircle);
//   gStyle->SetMarkerColor(kRed);
  gROOT->ForceStyle();

  TFile *file = new TFile(fileName, "READ");

  Int_t maxSel = 2;
  Int_t maxHar = 4; // 4
  Int_t maxHarPlot = 2;
  float ptMax = mevSim ? 2. : 6.;
  float v1 = 4.0; // 4. for dir9 mevSim

  Float_t max, min, r0;

  // Get Ntheta
  TString histNameNtheta("FlowLYZ_r0theta_Sel1_Har2");
  TH1D* histNtheta = new TH1D;
  histNtheta = (TH1D*)file->Get(histNameNtheta);
  if (!histNtheta) {
    cout << "### Can't find file " << histNameNtheta << endl;
    return;
  }
  int maxTheta = histNtheta->GetNbinsX();
  cout << "maxTheta = " << maxTheta << endl << endl;
  if (maxTheta > 5) maxTheta = 5;
  delete histNtheta;

  // Make the canvases
  TCanvas *canMult = new TCanvas("MeanMultiplicity", "MeanMultiplicity");
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,fileName.Data());  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now.AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();

  TCanvas *canGtheta = new TCanvas("Gtheta", "Gtheta", 700, 900);
  canGtheta->Divide(maxHarPlot*maxSel, maxTheta);

  TCanvas *canGthetaZoom = new TCanvas("Gtheta_Zoom", "Gtheta_Zoom", 700, 900);
  canGthetaZoom->Divide(maxHarPlot*maxSel, maxTheta);

  TCanvas *can_r0theta = new TCanvas("r0", "r0");
  can_r0theta->Divide(maxSel, maxHarPlot);

  TCanvas *can_vEta = new TCanvas("v(eta)", "v(eta)", 700, 900);
  can_vEta->Divide(maxSel, maxHar);

  TCanvas *can_vPt = new TCanvas("v(pt)", "v(pt)", 700, 900);
  can_vPt->Divide(maxSel, maxHar);

  TCanvas *can_vr0 = new TCanvas("vr0", "vr0");
  can_vr0->Divide(maxSel);

  TCanvas *can_v = new TCanvas("v", "v");
  can_v->Divide(maxSel);

  TCanvas *can_centX = new TCanvas("centX", "centX");
  can_centX->Divide(maxSel, maxHarPlot);

  TCanvas *can_centY = new TCanvas("centY", "centY");
  can_centY->Divide(maxSel, maxHarPlot);

  TCanvas *can_centQ = new TCanvas("centQ", "centQ");
  can_centQ->Divide(maxSel, maxHarPlot);

  // Make the histograms
  TH1D *histMult = new TH1D;
  TH1F **hist_v = new TH1F*[maxSel];
  TH1F **hist_vr0 = new TH1F*[maxSel];
  TH1F **hist_v = new TH1F*[maxSel];
  TH1F **hist_vEta = new TH1F*[maxSel*maxHar];
  TH1F **hist_vPt = new TH1F*[maxSel*maxHar];
  TH1D **hist_r0th = new TH1D*[maxSel*maxHarPlot];
  TH1F **histG = new TH1F*[maxSel*maxHarPlot*maxTheta];
  TH1F **histGZoom = new TH1F*[maxSel*maxHarPlot*maxTheta];
  TH1D **hist_centX = new TH1D*[maxSel*maxHarPlot];
  TH1D **hist_centY = new TH1D*[maxSel*maxHarPlot];
  TH1D **hist_centQ = new TH1D*[maxSel*maxHarPlot];

  TLine **r0Line = new TLine*[maxSel*maxHarPlot*maxTheta];
  TLine **r0LineZoom = new TLine*[maxSel*maxHarPlot*maxTheta]; 
  TLine *ptZeroLine = new TLine(0., 0., ptMax, 0.);
  TLine *pt5Line = new TLine(0., 5., 2., 5.);
  TLine *ptV1Line = new TLine(0., v1, 2., v1);
  TLine *etaZeroLine = new TLine(-1.5, 0., 1.5, 0.);
  TLine *etaZeroLineFTPC = new TLine(-4.5, 0., 4.5, 0.);
  TLine *GZeroLine = new TLine(0., 0., 0.35, 0.);
  TLine *eta5Line = new TLine(-4.5, 5., 4.5, 5.);
  TLine *etaV1LinePos = new TLine(0., v1, 4.5, v1);
  TLine *etaV1LineNeg = new TLine(-4.5, -v1, 0., -v1);
  TLine *vLine = new TLine(0.5, 0., maxHar+0.5, 0.);
  TLine *recentZeroLine = new TLine(0.5, 0., 3.5, 0.);
  TLine *recentedZeroLine = new TLine(0.5, 0., 2.5, 0.);

  // Multiplicity
  TString histName("FlowLYZ_Mult");
  cout << histName << endl;
  graphPad->cd();
  histMult = (TH1D*)file->Get(histName);
  if (!mevSim) { histMult->Fit("gaus"); }
  histMult->Draw();
  Double_t entries = histMult->GetEntries();
  TString* entriesChar = new TString("entries= ");
  *entriesChar += (int)entries;
  TLatex l;
  l.SetNDC();
  l.SetTextSize(0.05);
  l.DrawLatex(0.65,0.8,entriesChar->Data());

  float _v, vErr;  
  for (Int_t sel = 0; sel < maxSel; sel++) {

    if (!firstPass) {
      TString histName("FlowLYZ_v_Sel");
      histName += sel+1;
      cout << histName << endl;
      hist_v[sel] = (TH1F*)file->Get(histName);
      can_v->cd(sel+1); 
      hist_v[sel]->SetMinimum(0.);
      hist_v[sel]->Draw();
      vLine->Draw();
      for (int j=1; j<=maxHar; j++) {
	_v = hist_v[sel]->GetBinContent(j);
	vErr = hist_v[sel]->GetBinError(j);
	cout << setprecision(3) << "Sel = " << sel+1 << ": v" << j << " from pt = (" << _v <<
	  " +/- " << vErr << ") %" << endl;
      }
    }

    TString histName("FlowLYZ_vr0_Sel");
    histName += sel+1;
    cout << histName << endl;
    hist_vr0[sel] = (TH1F*)file->Get(histName);
    can_vr0->cd(sel+1); 
    hist_vr0[sel]->SetMinimum(0.);
    hist_vr0[sel]->Draw();
    for (int j=1; j<=maxHar; j++) {
      _v = hist_vr0[sel]->GetBinContent(j);
      vErr = hist_vr0[sel]->GetBinError(j);
      cout << setprecision(3) << "Sel= " << sel+1 << ": v" << j << " from r0 = (" << _v <<
	" +/- " << vErr << ") %" << endl;
    }

    for (Int_t har = 0; har < maxHarPlot; har++) {
      int n = sel + har;
     
      TString histName("FlowLYZ_r0theta_Sel");
      histName += sel+1;
      histName += "_Har";
      histName += har+1;
      cout << histName << endl;
      hist_r0th[n] = (TH1D*)file->Get(histName);
      can_r0theta->cd(1+sel+har*maxSel);
      //hist_r0th[n]->Fit("pol0");
      hist_r0th[n]->SetMinimum(0.);
      hist_r0th[n]->Draw();
      for (int th=1; th<=maxTheta; th++) {
	_v = hist_r0th[n]->GetBinContent(th);
	vErr = hist_r0th[n]->GetBinError(th);
	if (TMath::IsNaN(vErr)) {
	  vErr = 0.;
	  hist_r0th[n]->SetBinError(th, 0.);
	}
	cout << setprecision(3) << "Sel=" << sel+1 << ", Har=" << har+1 <<": r0" << th << " = "
	     << _v << " +/- " << vErr << endl;
      }

      if (!firstPass) {
	TString histName("FlowCentX_Sel");
	histName += sel+1;
	histName += "_Har";
	histName += har+1;
	hist_centX[n] = (TH1D*)file->Get(histName);
	if (hist_centX[n]) {
	  cout << histName << endl;
	  can_centX->cd(1+sel+har*maxSel);
	  hist_centX[n]->Draw();
	  recentZeroLine->Draw();
	}
	
	TString histName("FlowCentY_Sel");
	histName += sel+1;
	histName += "_Har";
	histName += har+1;
	hist_centY[n] = (TH1D*)file->Get(histName);
	if (hist_centY[n]) {
	  cout << histName << endl;
	  can_centY->cd(1+sel+har*maxSel);
	  hist_centY[n]->Draw();
	  recentZeroLine->Draw();
	}
      }

      TString histName("FlowQCent_Sel");
      histName += sel+1;
      histName += "_Har";
      histName += har+1;
      hist_centQ[n] = (TH1D*)file->Get(histName);
      if (hist_centQ[n]) {
	cout << histName << endl;
	can_centQ->cd(1+sel+har*maxSel);
	hist_centQ[n]->Draw();
	recentedZeroLine->Draw();
      }

      min = 0.00001;
      float expan = 1.2;
      for (Int_t theta = 0; theta < maxTheta; theta++) {
       	TString histName("FlowLYZ_Gtheta");
	histName += theta;
	histName += "_Sel";
	histName += sel+1;
	histName += "_Har";
	histName += har+1;
	cout << histName << endl;
	histG[theta] = (TH1F*)file->Get(histName);
	canGtheta->cd(1+har+sel*maxHarPlot+theta*maxSel*maxHarPlot);
	TVirtualPad::Pad()->SetLogy();
	histG[theta]->SetMinimum(min);
	histG[theta]->DrawCopy("PH");
	r0 = hist_r0th[n]->GetBinContent(theta+1);
	r0Line[theta] = new TLine(r0, 0., r0, 1.);
	r0Line[theta]->Draw();
	
	min = 0.000001;
	max = 1.;
	histGZoom[theta] = (TH1F*)file->Get(histName);
	canGthetaZoom->cd(1+har+sel*maxHarPlot+theta*maxSel*maxHarPlot);
	TVirtualPad::Pad()->SetLogy();
	histGZoom[theta]->SetMaximum(max);
	histGZoom[theta]->SetMinimum(min);
	histGZoom[theta]->SetAxisRange(r0/expan, r0*expan, "X");
	histGZoom[theta]->Draw("PH");
	r0LineZoom[theta] = new TLine(r0, 0., r0, max);
	r0LineZoom[theta]->Draw();

      }
    }

    if (!firstPass) {
      for (Int_t har = 0; har < maxHar; har++) {
	int n = sel + har;

	TString histName("FlowLYZ_vEta_Sel");
	histName += sel+1;
	histName += "_Har";
	histName += har+1;
	cout << histName << endl;
	hist_vEta[n] = (TH1D*)file->Get(histName);
	can_vEta->cd(sel+1+har*maxSel);
	hist_vEta[n]->SetMaximum(10.);
	hist_vEta[n]->SetMinimum(-10.);
	hist_vEta[n]->Draw("E");
	if (FTPC) { etaZeroLineFTPC->Draw(); }
	else { etaZeroLine->Draw(); }
	if (mevSim) {
	  if (har==1) {
	    eta5Line->Draw();
	  } else if (har==0) {
	    etaV1LinePos->Draw();
	    etaV1LineNeg->Draw();
	  }
	}
	
	TString histName("FlowLYZ_vPt_Sel");
	histName += sel+1;
	histName += "_Har";
	histName += har+1;
	cout << histName << endl;
	hist_vPt[n] = (TH1D*)file->Get(histName);
	can_vPt->cd(sel+1+har*maxSel);
	if (mevSim) {
	  hist_vPt[n]->SetMaximum(10.);
	  hist_vPt[n]->SetMinimum(-10.);
	} else {
	  hist_vPt[n]->SetMaximum(20.);
	  hist_vPt[n]->SetMinimum(-20.);
	}
	hist_vPt[n]->Draw("E");
	ptZeroLine->Draw();
	if (mevSim) {
	  if (har==1) {
	    pt5Line->Draw();
	  } else if (har==0) {
	    ptV1Line->Draw();
	  }
	}
      }
    }
  }

  if (strstr(ext,"ps")) {
    canGtheta->SaveAs("FlowLYZ_Gtheta.ps");
    canGthetaZoom->SaveAs("FlowLYZ_GthetaZoom.ps");
    can_r0theta->SaveAs("FlowLYZ_r0.ps");
    canMult->SaveAs("FlowLYZ_Mult.ps");
    can_vEta->SaveAs("FlowLYZ_vEta.ps");
    can_vPt->SaveAs("FlowLYZ_vPt.ps");
    can_vr0->SaveAs("FlowLYZ_vro.ps");
    can_v->SaveAs("FlowLYZ_v.ps");
    can_centX->SaveAs("FlowCentX.ps");
    can_centY->SaveAs("FlowCentY.ps");
    can_centQ->SaveAs("FlowCentQ.ps");
  } else if (strstr(ext,"gif")) {
    canGtheta->SaveAs("FlowLYZ_Gtheta.gif");
    canGthetaZoom->SaveAs("FlowLYZ_GthetaZoom.gif");
    can_r0theta->SaveAs("FlowLYZ_r0.gif");
    canMult->SaveAs("FlowLYZ_Mult.gif");
    can_vEta->SaveAs("FlowLYZ_vEta.gif");
    can_vPt->SaveAs("FlowLYZ_vPt.gif");
    can_vr0->SaveAs("FlowLYZ_vro.gif");
    can_v->SaveAs("FlowLYZ_v.gif");
  }

  return;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: plotLYZ.C,v $
// Revision 1.4  2007/02/06 19:00:53  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.2  2006/03/22 22:02:14  posk
// Updates to macros.
//
//
///////////////////////////////////////////////////////////////////////////////
