#include <iostream>
#include <iomanip>

#include <TCanvas.h>
#include <TAxis.h> 

#include "PlottingEngineRoot.hh"

namespace Garfield {

PlottingEngineRoot plottingEngine;

PlottingEngineRoot::PlottingEngineRoot() :
  garfieldStyle(0) {

  colorElectronDefault = "orange";
  colorHoleDefault = "red";
  colorIonDefault = "dark-red";
  colorPhotonDefault = "blue";
  colorChargedParticleDefault = "dark-green";
  colorLine1Default = "dark-blue";
  colorLine2Default = "olive";

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
  garfieldStyle->SetGridColor(kGray);
  garfieldStyle->SetGridStyle(3);
  garfieldStyle->SetGridWidth(1);
  garfieldStyle->SetPadGridX(kTRUE);
  garfieldStyle->SetPadGridY(kTRUE);
  garfieldStyle->SetTitleStyle(0);
  garfieldStyle->SetTitleBorderSize(0);
  garfieldStyle->SetTitleColor(1, "xyz");
  garfieldStyle->SetTitleColor(1, "t");
  garfieldStyle->SetTitleFillColor(0);
  garfieldStyle->SetTitleFont(132, "xyz");
  garfieldStyle->SetTitleFont(132, "t");
  garfieldStyle->SetStatStyle(0);
  garfieldStyle->SetStatBorderSize(0);    
  garfieldStyle->SetStatColor(0);
  garfieldStyle->SetStatFont(132);
  garfieldStyle->SetStatFontSize(0.03);
  garfieldStyle->SetStatX(0.88);
  garfieldStyle->SetStatY(0.88);
  garfieldStyle->SetStatW(0.25);
  garfieldStyle->SetStatH(0.15);
  garfieldStyle->SetOptStat(111110);
  garfieldStyle->SetLabelFont(132, "xyz");
  garfieldStyle->SetPaperSize(TStyle::kA4);
  garfieldStyle->SetFuncWidth(2);
  garfieldStyle->SetHistLineColor(kOrange - 3);
  garfieldStyle->SetPalette(1);
  garfieldStyle->SetAxisColor(kBlack, "X");
  garfieldStyle->SetAxisColor(kBlack, "Y");
  garfieldStyle->SetAxisColor(kBlack, "Z");
  
  garfieldStyle->cd();  
    
}

int
PlottingEngineRoot::GetRootColorLine1() {

  int rootcol;
  if (!GetRootColor(colorLine1, rootcol)) {
    colorLine1 = colorLine1Default;
    GetRootColor(colorLine1, rootcol);
  }
  return rootcol;

}

int
PlottingEngineRoot::GetRootColorLine2() {

  int rootcol;
  if (!GetRootColor(colorLine2, rootcol)) {
    colorLine2 = colorLine2Default;
    GetRootColor(colorLine2, rootcol);
  }
  return rootcol;

}

int 
PlottingEngineRoot::GetRootColorElectron() {

  int rootcol;
  if (!GetRootColor(colorElectron, rootcol)) {
    colorElectron = colorElectronDefault;
    GetRootColor(colorElectron, rootcol);
  }
  return rootcol;

}

int 
PlottingEngineRoot::GetRootColorHole() {

  int rootcol;
  if (!GetRootColor(colorHole, rootcol)) {
    colorHole = colorHoleDefault;
    GetRootColor(colorHole, rootcol);
  }
  return rootcol;

}

int 
PlottingEngineRoot::GetRootColorIon() {

  int rootcol;
  if (!GetRootColor(colorIon, rootcol)) {
    colorIon = colorIonDefault;
    GetRootColor(colorIon, rootcol);
  }
  return rootcol;

}

int
PlottingEngineRoot::GetRootColorPhoton() {

  int rootcol;
  if (!GetRootColor(colorPhoton, rootcol)) {
    colorPhoton = colorPhotonDefault;
    GetRootColor(colorPhoton, rootcol);
  }
  return rootcol;

}

int
PlottingEngineRoot::GetRootColorChargedParticle() {

  int rootcol;
  if (!GetRootColor(colorChargedParticle, rootcol)) {
    colorChargedParticle = colorChargedParticleDefault;
    GetRootColor(colorChargedParticle, rootcol);
  }
  return rootcol;

}

bool 
PlottingEngineRoot::GetRootColor(std::string color, int& rootcol) {

  rootcol = kBlack;

  // Convert to upper-case.
  for (unsigned int i = 0; i < color.length(); ++i) {
    color[i] = toupper(color[i]);
  }

  if (color == "BLACK") {
    rootcol = kBlack;
    return true;
  }
  if (color == "RED") {
    rootcol = kRed + 1;
    return true;
  } else if (color == "GREEN") {
    rootcol = kGreen + 2;
    return true;
  } else if (color == "BLUE") {
    rootcol = kBlue + 2;
    return true;
  } else if (color == "DARK-RED") {
    rootcol = kRed + 3;
    return true;
  } else if (color == "DARK-GREEN") {
    rootcol = kGreen + 3;
    return true;
  } else if (color == "DARK-BLUE") {
    rootcol = kBlue + 3;
    return true;
  } else if (color == "ORANGE") {
    rootcol = kOrange - 3;
    return true;
  } else if (color == "PURPLE") {
    rootcol = kViolet - 7;
    return true;
  } else if (color == "CYAN") {
    rootcol = kCyan + 3;
    return true;
  } else if (color == "OLIVE") {
    rootcol = kSpring + 4;
    return true;
  }
  if (debug) {
    std::cerr << className << "::GetRootColor:\n";
    std::cerr << "    Unknown color (" << color << ").\n";
  }
  return false;

}

}
