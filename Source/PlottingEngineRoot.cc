#include <iostream>
#include <iomanip>
#include <cmath>

#include <TCanvas.h>
#include <TAxis.h> 

#include "PlottingEngineRoot.hh"

namespace Garfield {

PlottingEngineRoot plottingEngine;

PlottingEngineRoot::PlottingEngineRoot() {

  garfieldStyle = new TStyle("Garfield","Garfield Style");
  SetDefaultStyle();
 
  f1 = new TF1("f1", "sin(x)", 0., 1.);
  f2 = new TF1("f2", "cos(x)", 0., 1.);
   
}

PlottingEngineRoot::~PlottingEngineRoot() {

  delete garfieldStyle;
  
}

void 
PlottingEngineRoot::PlotSignal(Sensor* s, const std::string label) {

  if (s == 0) {
    std::cerr << "PlottingEngineRoot::PlotSignal:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return;
  }
  
  int nBins;
  double t0;
  double dt;
  s->GetTimeWindow(t0, dt, nBins);
    
  if (hSignal != 0) {
    delete hSignal;
    hSignal = 0;
  }
  hSignal = new TH1F("hSignal", label.c_str(), nBins, t0, t0 + nBins * dt);
  hSignal->GetXaxis()->SetTitle("time [ns]");
  hSignal->GetYaxis()->SetTitle("signal");
  
  double sig = 0.;  
  for (int i = nBins; i--;) {
    sig = s->GetSignal(label, i);
    hSignal->SetBinContent(i, sig);
  }
  hSignal->Draw();
  
}

void 
PlottingEngineRoot::SetDefaultStyle() {

  garfieldStyle->Reset();  
  garfieldStyle->SetCanvasBorderMode(0);
  garfieldStyle->SetCanvasColor(0);
  garfieldStyle->SetCanvasPreferGL(kTRUE);
  garfieldStyle->SetPadBorderMode(0);
  garfieldStyle->SetPadColor(0);
  garfieldStyle->SetFrameBorderMode(0);
  garfieldStyle->SetTitleBorderSize(0);
  garfieldStyle->SetTitleColor(1, "xyz");
  garfieldStyle->SetTitleColor(1, "t");
  garfieldStyle->SetTitleFillColor(0);
  garfieldStyle->SetTitleFont(132, "xyz");
  garfieldStyle->SetTitleFont(132, "t");
  garfieldStyle->SetStatBorderSize(0);    
  garfieldStyle->SetStatColor(0);
  garfieldStyle->SetStatFont(132);
  garfieldStyle->SetStatFontSize(0.025);
  garfieldStyle->SetStatX(0.88);
  garfieldStyle->SetStatY(0.88);
  garfieldStyle->SetStatW(0.25);
  garfieldStyle->SetStatH(0.10);
  garfieldStyle->SetOptStat(111110);
  garfieldStyle->SetLabelFont(132, "xyz");
  garfieldStyle->SetPaperSize(26, 20);
  garfieldStyle->SetFuncWidth(2);
  garfieldStyle->SetHistLineColor(kOrange);
  garfieldStyle->SetPalette(1);
  
  garfieldStyle->cd();  
    
}

int 
PlottingEngineRoot::GetRootColor(std::string color) {

  // Convert to upper-case
  for (unsigned int i = 0; i < color.length(); ++i) {
    color[i] = toupper(color[i]);
  }

  if (color == "ORANGE") {
    return kOrange;
  } else if (color == "DARK-GREEN") {
    return kGreen + 2;
  } else {
    return kBlack;
  }

}

}
