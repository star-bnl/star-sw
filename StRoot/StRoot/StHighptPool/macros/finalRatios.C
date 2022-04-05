#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"
#include "commonmacro/ua1.h"

void finalRatios(
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

  TGraphAsymmErrors* gCentral=(TGraphAsymmErrors*)rCentral->Get(name);
  TGraphAsymmErrors* gMinbias=(TGraphAsymmErrors*)rMinbias->Get(name);
  TGraphAsymmErrors* gPeripheral=(TGraphAsymmErrors*)rPeripheral->Get(name);

  //
  // kludges
  //
  if(DOKLUDGE){
    cout << ">>>>WARNING DOING KLUDGE" << endl;
    cout << ">>>>WARNING DOING KLUDGE" << endl;
    cout << ">>>>WARNING DOING KLUDGE" << endl;

    kludgeBackground(gCentral,0.08);
    kludgeBackground(gMinbias,0.08);
    kludgeBackground(gPeripheral,0.05);

    kludgeSystematics(gCentral);
    kludgeSystematics(gMinbias);
    kludgeSystematics(gPeripheral);

  }


  //  gStyles
  gStyle->SetOptStat(0); gStyle->SetOptTitle(0);
  //gStyle->SetPadTickX(1); 
  gStyle->SetPadTickY(1);

  // follow manuel in the fonts
  int font = 42;
  gStyle->SetTextFont(font); gStyle->SetTitleFont(font);
  gStyle->SetLabelFont(font,"x"); gStyle->SetLabelFont(font,"y");


  //
  // divide the canvas into 3 pads
  //
  TCanvas* c1=new TCanvas("c1","c1",400,600);
  
  TPad* p1=new TPad("p1","p1",0.01,0.67,1,1,0,0,0);
  p1->SetBottomMargin(0); p1->Draw();
  
  TPad* p2=new TPad("p2","p2",0.01,0.33,1,.67,0,0,0);
  p2->SetTopMargin(0); p2->SetBottomMargin(0); p2->Draw();
  
  TPad* p3=new TPad("p3","p3",0.01,0.01,1,.33,0,0,0);
  p3->SetTopMargin(0); p3->Draw();

  // TLatex
  TLatex* ltx=new TLatex;
  double xText=2.2,textSize=0.05,titleSize=0.05,iTextSize=0.03;
  
  double yMin=0,yMax=1.6;
  double xMin=0,xMax=6.2;
  double tickSize =.07;
  double markerSize = 0.8;
  int markerStyle=4;
  int lowPtMarkerStyle=4;

  TCanvas* c2=new TCanvas("c2","c2",400,400);

  //
  //------------------------Minbias/ua1
  //
  double geom=7200; 
  double Asq = 197*197;
  double geomScale=geom/Asq;

  TGraphAsymmErrors* minbiasUA1X=makeUA1Ratio(gMinbias,0,geomScale);
  // chop off x errors
  TGraphAsymmErrors* minbiasUA1=removeXErrors(minbiasUA1X);

  TGraphAsymmErrors* minbiasUA1Err=
    makeUA1ScaleError(gMinbias,geomScale,geomScale,geomScale);

  TGraphAsymmErrors* hMinbias=makeHMinus(0);
  TGraphAsymmErrors* hMinbiasUA1=makeUA1Ratio(hMinbias,0,geomScale);
  TGraphAsymmErrors* hMinbiasUA1Err=
     makeUA1ScaleError(hMinbias,geomScale,geomScale,geomScale);
  TGraphAsymmErrors* hMinbiasUA1=removeXErrors(hMinbiasUA1);
  p1->cd();
  
  minbiasUA1->SetMarkerStyle(markerStyle); 
  minbiasUA1->SetMarkerSize(markerSize);
  minbiasUA1->SetMinimum(yMin); minbiasUA1->SetMaximum(yMax);
  minbiasUA1->Draw("ap");
  minbiasUA1Err->Draw("[]");

  hMinbiasUA1->SetMarkerStyle(lowPtMarkerStyle); 
  hMinbiasUA1->SetMarkerSize(markerSize);
  hMinbiasUA1->Draw("p");
  hMinbiasUA1Err->Draw("[]");
  

  TAxis* axis=minbiasUA1->GetXaxis();
  axis->SetLimits(xMin,xMax);

  // draw bins on axis
  drawAxisBins(minbiasUA1X,tickSize,minbiasUA1->GetYaxis()->GetXmax());
				
  drawLine(axis); 
  
  // label
  ltx->SetTextSize(textSize);
  strcpy(txt,"#frac{ 1/p_{T} d^{2} N/dp_{T}d#eta(STAR minbias) #sigma_{geom}}{ 2#pi Ed^{3}#sigma/dp^{3} (UA1) A^{2}}");
  ltx->DrawLatex(xText,.3,txt);
  

  //******* individual plot
  
  c2->cd();
  minbiasUA1->SetMinimum(yMin); minbiasUA1->SetMaximum(yMax);
  minbiasUA1->SetMarkerSize(1);
  minbiasUA1->Draw("ap");
  minbiasUA1Err->Draw("[]");

  hMinbiasUA1->SetMarkerStyle(lowPtMarkerStyle); 
  hMinbiasUA1->SetMarkerSize(1);
  hMinbiasUA1->Draw("p");
  hMinbiasUA1Err->Draw("[]");

  minbiasUA1->GetXaxis()->SetTitle("p_{T} (GeV/c)");

  drawLine(minbiasUA1->GetXaxis()); 

  ltx->SetTextSize(iTextSize);
  strcpy(txt,"#frac{ 1/p_{T} d^{2} N/dp_{T}d#eta(STAR minbias) #sigma_{geom}}{ 2#pi Ed^{3}#sigma/dp^{3} (UA1) A^{2}}");
  ltx->DrawLatex(xText,.20,txt);
				  
  Print(c2,psDir,"minbiasOverUA1.ps");
  
  
  //
  //---------------------- central/ua1
  //
  double TAA=26;
  double TAAScale=1./TAA; 
  double TAAScaleLow=1./(TAA+1),TAAScaleHigh=1./(TAA-1);

  TGraphAsymmErrors* centralUA1X=makeUA1Ratio(gCentral,0,TAAScale);
  TGraphAsymmErrors* centralUA1Err=
    makeUA1ScaleError(gCentral,TAAScale,TAAScaleLow,TAAScaleHigh);
  TGraphAsymmErrors* centralUA1=removeXErrors(centralUA1X);
  
  // low pt
  TGraphAsymmErrors* hCentral=makeHMinus(2);
  TGraphAsymmErrors* hCentralUA1=makeUA1Ratio(hCentral,0,TAAScale);
  TGraphAsymmErrors* hCentralUA1Err=
     makeUA1ScaleError(hCentral,TAAScale,TAAScaleLow,TAAScaleHigh);
  TGraphAsymmErrors* hCentralUA1=removeXErrors(hCentralUA1);

  p2->cd();
  centralUA1->SetMarkerStyle(markerStyle); 
  centralUA1->SetMinimum(yMin); centralUA1->SetMaximum(yMax-0.01);
  centralUA1->Draw("ap");

  centralUA1Err->Draw("[]");

  hCentralUA1->SetMarkerStyle(lowPtMarkerStyle); 
  hCentralUA1->SetMarkerSize(markerSize);
  hCentralUA1->Draw("p"); 
  hCentralUA1Err->Draw("[]");

  axis=centralUA1->GetXaxis();
  axis->SetLimits(xMin,xMax);
  drawLine(axis);
  
  // draw bins on axis
  drawAxisBins(centralUA1X,tickSize,minbiasUA1->GetYaxis()->GetXmax());

  // label
  ltx->SetTextSize(textSize);
  strcpy(txt,"#frac{ 1/p_{T} d^{2}N/dp_{T}d#eta(STAR central)}{ 2#pi Ed^{3}#sigma/dp^{3} (UA1) T_{AA}  }");
  ltx->DrawLatex(xText,1.2,txt);

  strcpy(txt,"STAR h^{-}");
  ltx->DrawLatex(1.2,.35,txt);

  //***** individual plot
  c2->Clear();
  c2->cd();
  centralUA1->SetMinimum(yMin); centralUA1->SetMaximum(yMax);
  centralUA1->SetMarkerSize(1);
  centralUA1->Draw("ap");
  centralUA1Err->Draw("[]");
  hCentralUA1->SetMarkerSize(1);
  hCentralUA1->Draw("p"); 
  hCentralUA1Err->Draw("[]");
  centralUA1->GetXaxis()->SetTitle("p_{T} (GeV/c)");
  
  drawLine(centralUA1->GetXaxis()); 

  ltx->SetTextSize(iTextSize);
  strcpy(txt,"#frac{ 1/p_{T} d^{2}N/dp_{T}d#eta(STAR central)}{ 2#pi Ed^{3}#sigma/dp^{3} (UA1) T_{AA}  }");
  ltx->DrawLatex(xText,1.2,txt);

  Print(c2,psDir,"centralOverUA1.ps");

  //
  //---------------- central/peripheral
  //
  TGraphAsymmErrors* centralPeriphX=divide(gCentral,gPeripheral);
  TGraphAsymmErrors* centralPeriph=removeXErrors(centralPeriphX);
  
  TGraphAsymmErrors* hPeriph=makeHMinus(1);
  TGraphAsymmErrors* hCentralPeriph=divide(hCentral,hPeriph);

  p3->cd();
  double sc=0;
  double* y=centralPeriph->GetY();

  sc=1./y[2];
  scale(centralPeriph,sc); // scale at 2 for now
  //  scale(hCentralPeriph,sc);
  

  

  centralPeriph->SetMarkerStyle(markerStyle); 
  centralPeriph->SetMarkerSize(markerSize);
  centralPeriph->SetMinimum(0); centralPeriph->SetMaximum(yMax-0.01);
  centralPeriph->Draw("ap");

  hCentralPeriph->SetMarkerStyle(lowPtMarkerStyle);
  hCentralPeriph->SetMarkerSize(markerSize);
  hCentralPeriph->Draw("p");

  drawLine(axis);
  
  axis=centralPeriph->GetXaxis();
  axis->SetLimits(xMin,xMax);
  
  axis->SetTitle("p_{T} (GeV/c)");
  axis->SetTitleSize(titleSize);
  axis->SetLabelOffset(0.003);

  drawAxisBins(centralPeriphX,tickSize,minbiasUA1->GetYaxis()->GetXmax());

  // label
  ltx->SetTextSize(textSize);
  strcpy(txt,"#frac{ STAR central }{ STAR peripheral } (normalized to 1 @ 2GeV/c)");
  ltx->DrawLatex(xText,1.3,txt);

  Print(c1,psDir,"ratios");

  //**** inidividual plot
  c2->Clear(); c2->cd();

  centralPeriph->SetMarkerStyle(markerStyle); 
  centralPeriph->SetMinimum(0); centralPeriph->SetMaximum(yMax);
  centralPeriph->SetMarkerSize(1);
  centralPeriph->Draw("ap");
  centralPeriph->GetXaxis()->SetTitle("p_{T} (GeV/c)");

  hCentralPeriph->SetMarkerStyle(lowPtMarkerStyle);
  hCentralPeriph->SetMarkerSize(1);
  // hCentralPeriph->Draw("p");
  

  drawLine(axis);

  ltx->SetTextSize(iTextSize);
  strcpy(txt,"#frac{ STAR central }{ STAR peripheral } (normalized to 1 @ 2GeV/c)");
  ltx->DrawLatex(1,1.3,txt);

  Print(c2,psDir,"centralOverPeriph");

  

}

void drawLine(TAxis* a)
{
  TLine* line = new TLine;
  line->SetLineStyle(2);

  line->DrawLine(a->GetXmin(),1,a->GetXmax(),1);
}

