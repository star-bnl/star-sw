#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"
#include "commonmacro/ua1.h"

void finalSpectra(
		 const char* inCentral=
		 "links/P01hi.central.2000.hist/finish_cut88778999_iter3.hist.root",
		 const char* inMinbias=
		 "links/P01hi.minbias.2000.hist/finish_cut97778999_iter3.hist.root",
		 const char* inPeripheral=
		 "links/P01hi.minbias.2000.hist/finish_cut57778999_iter3.hist.root",
		 const char* psDir="psFinal"
		 )
{
  gSystem->Clear();

  TFile* rCentral=new TFile(inCentral);
  TFile* rMinbias=new TFile(inMinbias);
  TFile* rPeripheral=new TFile(inPeripheral);
  
  int DOKLUDGE=1;

 
  //
  // get the graphs
  //
  char name[200],title[200],txt[500];
  char* sign=0; // "Minus"
  int bin=0;

  setName(name,"gSpecCorrected",bin,sign);

  TGraphAsymmErrors* gCentralX=(TGraphAsymmErrors*)rCentral->Get(name);
  TGraphAsymmErrors* gMinbiasX=(TGraphAsymmErrors*)rMinbias->Get(name);
  TGraphAsymmErrors* gPeripheralX=(TGraphAsymmErrors*)rPeripheral->Get(name);
  
  //
  // kludges
  //
  if(DOKLUDGE){
    cout << ">>>>WARNING DOING KLUDGE" << endl;
    cout << ">>>>WARNING DOING KLUDGE" << endl;
    cout << ">>>>WARNING DOING KLUDGE" << endl;

    kludgeBackground(gCentralX,0.08);
    kludgeBackground(gMinbiasX,0.08);
    kludgeBackground(gPeripheralX,0.05);

    kludgeSystematics(gCentralX);
    kludgeSystematics(gMinbiasX);
    kludgeSystematics(gPeripheralX);

  }

  
  // low pt stuff
  TGraphAsymmErrors* gCentralLowPt=makeHMinus(2);
  TGraphAsymmErrors* gMinbiasLowPt=makeHMinus(0);
  TGraphAsymmErrors* gPeripheralLowPt=makeHMinus(1);

  // ua1
  float sigmaUA1_200=42;
  float ua1Scale200 = 2.*3.14159/sigmaUA1_200;

  float sigmaUA1_130=40.5;
  float ua1Scale130= 2.*3.14159/sigmaUA1_130;

  TGraphErrors* gUA1Data200=ua1Data200();
  scale(gUA1Data200,ua1Scale200);

  TF1* fUA1Fit130 = ua1Fit130(ua1Scale130);


  // remove some error bars

  gCentralLowPt=removeXErrors(gCentralLowPt);
  gMinbiasLowPt=removeXErrors(gMinbiasLowPt);
  gPeripheralLowPt=removeXErrors(gPeripheralLowPt);


  //  gStyles
  gStyle->SetOptStat(0); gStyle->SetOptTitle(0);
  gStyle->SetPadTickX(1); 
  gStyle->SetPadTickY(1);
  
  // follow manuel in the fonts
  int font = 42;
  gStyle->SetTextFont(font); gStyle->SetTitleFont(font);
  gStyle->SetLabelFont(font,"x"); gStyle->SetLabelFont(font,"y");
  
  // TLatex
  TLatex* ltx=new TLatex;

  TCanvas* c1=new TCanvas("c1","c1",400,500);

  // draw the spectra
  double xMin=0,xMax=6;
  double yMin=1e-7,yMax=10000;
  double textSize=0.03;
  double titleSize=0.04;
  double markerSize=1;
  double labelSize=0.03;
  double yTitleOffset=1.2;
  double xTitleOffset=0.8;
  double tickSize=9990;
  
  TAxis* axis;

  c1->cd(); gPad->SetLogy(); 

  int centralMarker=29;
  int minbiasMarker=8;
  int peripheralMarker=22;
  int hMinusMarker=21;
  int ua1Marker=4;


  // central
  //
  gCentral=removeXErrors(gCentralX);

  gCentral->SetMinimum(yMin); gCentral->SetMaximum(yMax);
  gCentral->SetMarkerStyle(centralMarker); 
  gCentral->SetMarkerSize(markerSize+.2);
  gCentral->Draw("ap");


  strcpy(txt,"1/p_{T} d^{2}N^{(h^{-}+h^{+})/2}/dp_{T}d#eta #cbar_{|#eta|<0.5}(GeV/c)^{-2}");

  // yaxis
  axis=gCentral->GetYaxis();
  axis->SetTitle(txt);
  axis->SetTitleSize(titleSize);
  axis->SetTitleOffset(yTitleOffset);
  axis->SetLabelSize(labelSize);

  // xaxis
  axis=gCentral->GetXaxis();
  axis->SetTitle("p_{T} (GeV/c)");
  axis->SetTitleSize(titleSize);
  axis->SetTitleOffset(xTitleOffset);
  axis->SetLabelOffset(0.003);
  axis->SetLabelSize(labelSize);
  axis->SetLimits(xMin,xMax);

  
  // low pt central
  gCentralLowPt->SetMarkerStyle(centralMarker);
  gCentralLowPt->SetMarkerSize(markerSize);
  gCentralLowPt->Draw("p");


  // draw bin widths
  // drawAxisBins(gCentralX,tickSize,yMax);
  //  TLine* line=new TLine;
  // line->DrawLineNDC((2/6)*(1./.8)+.1,.8,2/6*(1./.8)+.1 ,.9);


  // minbias
  //
  // remove x errors
  gMinbias=removeXErrors(gMinbiasX);

  gMinbias->SetMarkerStyle(minbiasMarker); 
  gMinbias->SetMarkerSize(markerSize);
  gMinbias->Draw("p");

  gMinbiasLowPt->SetMarkerStyle(minbiasMarker);
  gMinbiasLowPt->SetMarkerSize(markerSize);
  gMinbiasLowPt->Draw("p");

  // peripheral
  //
  // remove x errors
  gPeripheral=removeXErrors(gPeripheralX);

  gPeripheral->SetMarkerStyle(peripheralMarker); 
  gPeripheral->SetMarkerSize(markerSize);
  gPeripheral->Draw("p");

  gPeripheralLowPt->SetMarkerStyle(peripheralMarker);
  gPeripheralLowPt->SetMarkerSize(markerSize);
  gPeripheralLowPt->Draw("p");
  
  // ua1
  //
  

  gUA1Data200->SetMarkerStyle(ua1Marker);
  gUA1Data200->SetMarkerSize(markerSize);

  //fUA1Fit130->SetLineColor();

  gUA1Data200->Draw("p");
  fUA1Fit130->SetRange(.1,6);
  fUA1Fit130->Draw("same");


  //
  // legend
  //
  TLegend* l=new TLegend(0.5,0.7,0.8,0.8);
  l->SetTextSize(textSize);
  l->SetBorderSize(0); l->SetFillColor(4000); // transparent
  l->AddEntry(gCentral,"central 0-5%","p");
  l->AddEntry(gPeripheral,"periperhal 60-80%","p");
  l->AddEntry(gMinbias,"minimum bias","p");  
  l->AddEntry(gUA1Data200,"ua1 200 GeV data","p");
  l->AddEntry(fUA1Fit130,"ua1 130 GeV fit","l");
  l->Draw();

  Print(c1,psDir,"spectra");

}

void drawLine(TAxis* a)
{
  TLine* line = new TLine;
  line->SetLineStyle(2);

  line->DrawLine(a->GetXmin(),1,a->GetXmax(),1);
}
