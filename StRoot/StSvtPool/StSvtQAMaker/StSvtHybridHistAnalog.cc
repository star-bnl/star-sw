/***************************************************************************
 *
 * $Id: StSvtHybridHistAnalog.cc,v 1.3 2007/07/12 20:09:44 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Histogram BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridHistAnalog.cc,v $
 * Revision 1.3  2007/07/12 20:09:44  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.2  2004/05/12 17:47:57  perev
 * WarnOff
 *
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include "StSvtHybridHistAnalog.hh"
#include "TH1.h"
#include "TStyle.h"
#include "TPaveText.h"
#include "TMath.h"
ClassImp(StSvtHybridHistAnalog)

StSvtHybridHistAnalog::StSvtHybridHistAnalog() : StSvtHybridObject()
{}

StSvtHybridHistAnalog::StSvtHybridHistAnalog(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{}

StSvtHybridHistAnalog::StSvtHybridHistAnalog(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup) : 
  StSvtHybridObject()
{
  TString nameA = TString(name) + "_a";
  TString nameB = TString(name) + "_b";
  TString nameC = TString(name) + "_c";

  histA = new  TH1F(nameA, title, nbinsx, xlow, xup);
  histB = new  TH1F(nameB, title, nbinsx, xlow, xup);
  histC = new  TH1F(nameC, title, nbinsx, xlow, xup);
}

StSvtHybridHistAnalog::StSvtHybridHistAnalog(int barrel, int ladder, int wafer, int hybrid, 
				 Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  TString nameA = TString(name) + "_a";
  TString nameB = TString(name) + "_b";
  TString nameC = TString(name) + "_c";

  histA = new  TH1F(nameA, title, nbinsx, xlow, xup);
  histB = new  TH1F(nameB, title, nbinsx, xlow, xup);
  histC = new  TH1F(nameC, title, nbinsx, xlow, xup);
}

StSvtHybridHistAnalog::~StSvtHybridHistAnalog()
{
  delete histA;
  delete histB;
  delete histC;
}

TH1F* StSvtHybridHistAnalog::getHist(int n)
{
  switch (n)  {

  case 1:
    return histA;
  case 2:
    return histB;
  case 3:
    return histC;
  }

  return 0;
}

void StSvtHybridHistAnalog::Fill(float x, int anode)
{
  if (anode <=80) 
    histA->Fill(x);
  else if ((anode >80) && (anode <=160))
    histB->Fill(x);
  else if (anode >160) 
    histC->Fill(x);
}

void StSvtHybridHistAnalog::Draw(const char* option)
{
  float max, min;
  float bin_c, total_a, total_sq_a, total_b, total_sq_b, total_c, total_sq_c;
  float mean_a, rms_a, mean_b, rms_b, mean_c, rms_c, content; 
  int   i, counter_a, counter_b, counter_c;
  char  text[50];
  int binMax, binMin;

  TPaveText* pave;

    total_a = 0;
    total_sq_a = 0;
    counter_a = 0;
    total_b = 0;
    total_sq_b = 0;
    counter_b = 0;
    total_c = 0;
    total_sq_c = 0;
    counter_c = 0;

    gStyle->SetOptStat(0);

    for (i = 1;i<=250;i++) {
      content = (float)histA->GetBinContent(i);
      bin_c = (float)histA->GetBinCenter(i); 
      if (histA->GetBinLowEdge(i)>0) {
	total_a += bin_c*content;
	total_sq_a += (bin_c*bin_c)*content;
	counter_a += (int)content;
      }
      content = (float)histB->GetBinContent(i);
      bin_c = (float)histB->GetBinCenter(i); 
      if (histB->GetBinLowEdge(i)>0) {
	total_b += bin_c*content;
	total_sq_b += (bin_c*bin_c)*content;
	counter_b += (int)content;
      }
      content = (float)histC->GetBinContent(i);
      bin_c = (float)histC->GetBinCenter(i); 
      if (histC->GetBinLowEdge(i)>0) {
	total_c += bin_c*content;
	total_sq_c += (bin_c*bin_c)*content;
	counter_c += (int)content;
      }
    }
    mean_a = (float)total_a/counter_a;
    rms_a = TMath::Sqrt((float)total_sq_a/counter_a-mean_a*mean_a);
    mean_b = (float)total_b/counter_b;
    rms_b = TMath::Sqrt((float)total_sq_b/counter_b-mean_b*mean_b);
    mean_c = (float)total_c/counter_c;
    rms_c = TMath::Sqrt((float)total_sq_c/counter_c-mean_c*mean_c);

    binMax = histA->GetMaximumBin();
    max = histA->GetBinContent(binMax);

    binMax = histB->GetMaximumBin();
    if (max < histB->GetBinContent(binMax))
      max = histA->GetBinContent(binMax);

    binMax = histC->GetMaximumBin();
    if (max < histC->GetBinContent(binMax))
      max = histC->GetBinContent(binMax);

    binMin = histA->GetMinimumBin();
    min = histA->GetBinContent(binMin);

    binMin = histB->GetMinimumBin();
    if (min > histB->GetBinContent(binMin))
      min = histB->GetBinContent(binMin);

    binMin = histC->GetMinimumBin();
    if (min > histC->GetBinContent(binMin))
      min = histC->GetBinContent(binMin);

    histA->SetMaximum(max+10.);
    histA->SetMinimum(min-10.);

    if ( !strncmp(option, "LADDER", strlen("LADDER")) ) {
      gStyle->SetOptTitle(0);
      gStyle->SetStatH(.2);
      gStyle->SetStatW(.3);
      histA->GetXaxis()->SetTitleSize(.07);
      histA->GetYaxis()->SetTitleSize(.07);
      histA->SetLabelSize(.07,"X");
      histA->SetLabelSize(.07,"Y");
      histA->SetNdivisions(406);
    }
    else {
      gStyle->SetTitleW(.75);
      gStyle->SetOptTitle(1);
      gStyle->SetStatH();
      gStyle->SetStatW();
      histA->GetXaxis()->SetTitleSize(.04);
      histA->GetYaxis()->SetTitleSize(.04);
      histA->SetLabelSize(.04,"X");
      histA->SetLabelSize(.04,"Y");
      histA->SetNdivisions(510);
    }

    histA->SetLineColor(2);
    histA->SetLineStyle(1);
    histA->SetLineWidth(3);
    histA->Draw();    
    histB->SetLineColor(3);
    histB->SetLineStyle(2);
    histB->SetLineWidth(3);
    histB->Draw("same");
    histC->SetStats(0);
    histC->SetLineColor(4);
    histC->SetLineStyle(3);
    histC->SetLineWidth(3);
    histC->Draw("same");

    pave = new TPaveText(.78,.82,.98,.98,"brNDC");
    pave->SetTextColor(2);
    pave->SetTextSize(0);
    pave->SetFillColor(10);
    sprintf(text,"Nent = %d",counter_a);
    pave->AddText(text);
    sprintf(text,"Mean = %f",mean_a);
    pave->AddText(text);
    sprintf(text,"RMS = %f",rms_a);
    pave->AddText(text);
    pave->Draw();

    pave = new TPaveText(.78,.64,.98,.80,"brNDC");
    pave->SetTextColor(3);
    pave->SetTextSize(0);
    pave->SetFillColor(10);
    sprintf(text,"Nent = %d",counter_b);
    pave->AddText(text);
    sprintf(text,"Mean = %f",mean_b);
    pave->AddText(text);
    sprintf(text,"RMS = %f",rms_b);
    pave->AddText(text);
    pave->Draw();

    pave = new TPaveText(.78,.46,.98,.62,"brNDC");
    pave->SetTextColor(4);
    pave->SetTextSize(0);
    pave->SetFillColor(10);
    sprintf(text,"Nent = %d",counter_c);
    pave->AddText(text);
    sprintf(text,"Mean = %f",mean_c);
    pave->AddText(text);
    sprintf(text,"RMS = %f",rms_c);
    pave->AddText(text);
    pave->Draw();
}

void StSvtHybridHistAnalog::Reset(int n)
{
  if (!n) {
    histA->Reset();
    histB->Reset();
    histC->Reset();
  }
  else {
    switch (n)  {      
    case 1:
      histA->Reset();
    case 2:
      histB->Reset();
    case 3:
      histC->Reset();
    }
  }    
}

