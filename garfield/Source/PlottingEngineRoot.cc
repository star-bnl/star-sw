#include <iostream>
#include <iomanip>

#include <TCanvas.h>
#include <TAxis.h>

#include "PlottingEngineRoot.hh"

namespace Garfield {

PlottingEngineRoot plottingEngine;

PlottingEngineRoot::PlottingEngineRoot() 
    : PlottingEngine(), 
      m_garfieldStyle(NULL) {

  m_colorElectronDefault = "orange";
  m_colorHoleDefault = "red";
  m_colorIonDefault = "dark-red";
  m_colorPhotonDefault = "blue";
  m_colorChargedParticleDefault = "dark-green";
  m_colorLine1Default = "dark-blue";
  m_colorLine2Default = "olive";
}

PlottingEngineRoot::~PlottingEngineRoot() {

  if (m_garfieldStyle != NULL) {
    delete m_garfieldStyle;
    m_garfieldStyle = NULL;
  }
}

void PlottingEngineRoot::SetDefaultStyle() {

  if (m_garfieldStyle != NULL) {
    m_garfieldStyle->cd();
    return;
  }
  m_garfieldStyle = new TStyle("Garfield", "Garfield Style");
  m_garfieldStyle->Reset();
  m_garfieldStyle->SetFillColor(1);
  m_garfieldStyle->SetFillStyle(1001);
  m_garfieldStyle->SetCanvasBorderMode(0);
  m_garfieldStyle->SetCanvasColor(0);
  m_garfieldStyle->SetCanvasPreferGL(kTRUE);
  m_garfieldStyle->SetCanvasDefH(600);
  m_garfieldStyle->SetCanvasDefW(600);
  m_garfieldStyle->SetPadBorderMode(0);
  m_garfieldStyle->SetPadColor(0);
  m_garfieldStyle->SetPadLeftMargin(0.1);
  m_garfieldStyle->SetPadBottomMargin(0.1);
  m_garfieldStyle->SetPadRightMargin(0.1);
  m_garfieldStyle->SetPadTopMargin(0.05);
  m_garfieldStyle->SetPadTickX(1);
  m_garfieldStyle->SetPadTickY(1);
  m_garfieldStyle->SetFrameFillColor(0);
  m_garfieldStyle->SetFrameBorderMode(0);
  m_garfieldStyle->SetDrawBorder(0);
  m_garfieldStyle->SetLegendBorderSize(0);

  m_garfieldStyle->SetGridColor(kGray);
  m_garfieldStyle->SetGridStyle(3);
  m_garfieldStyle->SetGridWidth(1);
  m_garfieldStyle->SetPadGridX(kTRUE);
  m_garfieldStyle->SetPadGridY(kTRUE);

  // const int font = 132;
  const int font = 42;
  m_garfieldStyle->SetTextFont(font);
  m_garfieldStyle->SetTitleStyle(0);
  m_garfieldStyle->SetTitleBorderSize(0);
  m_garfieldStyle->SetTitleColor(1, "xyz");
  m_garfieldStyle->SetTitleColor(1, "t");
  m_garfieldStyle->SetTitleFillColor(0);
  m_garfieldStyle->SetTitleFont(font, "xyz");
  m_garfieldStyle->SetTitleFont(font, "t");
  m_garfieldStyle->SetTitleOffset(1.2, "xyz");
  m_garfieldStyle->SetTitleSize(0.03, "xyz");
  m_garfieldStyle->SetTitleSize(0.05, "t");

  m_garfieldStyle->SetStatStyle(0);
  m_garfieldStyle->SetStatBorderSize(0);
  m_garfieldStyle->SetStatColor(0);
  m_garfieldStyle->SetStatFont(font);
  m_garfieldStyle->SetStatFontSize(0.03);
  m_garfieldStyle->SetStatX(0.88);
  m_garfieldStyle->SetStatY(0.88);
  m_garfieldStyle->SetStatW(0.25);
  m_garfieldStyle->SetStatH(0.1);
  m_garfieldStyle->SetOptStat(111110);
  m_garfieldStyle->SetStatFormat("6.3g");
  m_garfieldStyle->SetLabelFont(font, "xyz");
  m_garfieldStyle->SetLabelSize(0.03, "xyz");
  m_garfieldStyle->SetLabelOffset(0.01, "xyz");
  m_garfieldStyle->SetOptTitle(0);
  m_garfieldStyle->SetPaperSize(TStyle::kA4);
  m_garfieldStyle->SetFuncWidth(2);
  m_garfieldStyle->SetHistLineColor(kOrange - 3);
  m_garfieldStyle->SetPalette(1);
  m_garfieldStyle->SetAxisColor(kBlack, "X");
  m_garfieldStyle->SetAxisColor(kBlack, "Y");
  m_garfieldStyle->SetAxisColor(kBlack, "Z");
  const double lw = 2;
  m_garfieldStyle->SetLineWidth(lw);
  m_garfieldStyle->SetFrameLineWidth(lw);
  m_garfieldStyle->SetHistLineWidth(lw);
  m_garfieldStyle->SetFuncWidth(lw);
  m_garfieldStyle->SetGridWidth(lw);

  m_garfieldStyle->cd();
}

int PlottingEngineRoot::GetRootColorLine1() {

  int rootcol = 0;
  if (!GetRootColor(m_colorLine1, rootcol)) {
    m_colorLine1 = m_colorLine1Default;
    GetRootColor(m_colorLine1, rootcol);
  }
  return rootcol;
}

int PlottingEngineRoot::GetRootColorLine2() {

  int rootcol = 0;
  if (!GetRootColor(m_colorLine2, rootcol)) {
    m_colorLine2 = m_colorLine2Default;
    GetRootColor(m_colorLine2, rootcol);
  }
  return rootcol;
}

int PlottingEngineRoot::GetRootColorElectron() {

  int rootcol = 0;
  if (!GetRootColor(m_colorElectron, rootcol)) {
    m_colorElectron = m_colorElectronDefault;
    GetRootColor(m_colorElectron, rootcol);
  }
  return rootcol;
}

int PlottingEngineRoot::GetRootColorHole() {

  int rootcol = 0;
  if (!GetRootColor(m_colorHole, rootcol)) {
    m_colorHole = m_colorHoleDefault;
    GetRootColor(m_colorHole, rootcol);
  }
  return rootcol;
}

int PlottingEngineRoot::GetRootColorIon() {

  int rootcol = 0;
  if (!GetRootColor(m_colorIon, rootcol)) {
    m_colorIon = m_colorIonDefault;
    GetRootColor(m_colorIon, rootcol);
  }
  return rootcol;
}

int PlottingEngineRoot::GetRootColorPhoton() {

  int rootcol = 0;
  if (!GetRootColor(m_colorPhoton, rootcol)) {
    m_colorPhoton = m_colorPhotonDefault;
    GetRootColor(m_colorPhoton, rootcol);
  }
  return rootcol;
}

int PlottingEngineRoot::GetRootColorChargedParticle() {

  int rootcol = 0;
  if (!GetRootColor(m_colorChargedParticle, rootcol)) {
    m_colorChargedParticle = m_colorChargedParticleDefault;
    GetRootColor(m_colorChargedParticle, rootcol);
  }
  return rootcol;
}

bool PlottingEngineRoot::GetRootColor(std::string color, int& rootcol) {

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
  if (m_debug) {
    std::cerr << m_className << "::GetRootColor:\n";
    std::cerr << "    Unknown color (" << color << ").\n";
  }
  return false;
}
}
