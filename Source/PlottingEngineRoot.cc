#include <iostream>
#include <iomanip>

#include <TCanvas.h>
#include <TAxis.h> 

#include "PlottingEngineRoot.hh"

namespace Garfield {

PlottingEngineRoot plottingEngine;

PlottingEngineRoot::PlottingEngineRoot() :
  garfieldStyle(0) {

}

PlottingEngineRoot::~PlottingEngineRoot() {

  if (garfieldStyle != 0) {
    delete garfieldStyle;
    garfieldStyle = 0;
  }

}

void 
PlottingEngineRoot::SetDefaultStyle() {

  if (garfieldStyle != 0) {
    garfieldStyle->cd();
    return;
  }
  garfieldStyle = new TStyle("Garfield","Garfield Style");
  SetDefaultStyle();
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
