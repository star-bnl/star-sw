#include <iostream>
#include <string>
#include <sstream>
#include <cmath>

#include <TAxis.h>

#include "Plotting.hh"
#include "Medium.hh"
#include "ViewMedium.hh"

namespace Garfield {

ViewMedium::ViewMedium() :
  debug(false),
  canvas(0), hasExternalCanvas(false),
  medium(0),
  eMin(0.), eMax(1000.), bMin(0.), bMax(1.e5),
  vMin(0.), vMax(0.),
  nFunctions(0) {
  
  className = "ViewMedium";

  functions.clear();
  plottingEngine.SetDefaultStyle();

}

ViewMedium::~ViewMedium() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;

}

void
ViewMedium::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void
ViewMedium::SetMedium(Medium* m) {

  if (m == 0) {
    std::cerr << className << "::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  medium = m;

}

void
ViewMedium::SetElectricFieldRange(const double emin, const double emax) {

  if (emin >= emax || emin < 0.) {
    std::cerr << className << "::SetElectricFieldRange:\n";
    std::cerr << "    Incorrect field range.\n";
    return;
  }

  eMin = emin; eMax = emax;

}

void
ViewMedium::SetMagneticFieldRange(const double bmin, const double bmax) {

  if (bmin >= bmax || bmin < 0.) { 
    std::cerr << className << "::SetMagneticFieldRange:\n";
    std::cerr << "    Incorrect field range.\n";
    return;
  }

  bMin = bmin; bMax = bmax;

}

void
ViewMedium::SetVelocityRange(const double vmin, const double vmax) {

  if (vmin >= vmax || vmin < 0.) {
    std::cerr << className << "::SetVelocityRange:\n";
    std::cerr << "    Incorrect range.\n";
    return;
  }

  vMin = vmin; vMax = vmax;

}

void
ViewMedium::SetVelocityRange() {

  vMin = vMax = 0.;

}

void 
ViewMedium::PlotElectronVelocity(const bool keep) {

  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", "drift velocity [cm/ns]", 0);
  canvas->Update();
  
}

void 
ViewMedium::PlotHoleVelocity(const bool keep) {

  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", "drift velocity [cm/ns]", 10);
  canvas->Update();

}

void 
ViewMedium::PlotIonVelocity(const bool keep) {

  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", "drift velocity [cm/ns]", 20);
  canvas->Update();

}

void 
ViewMedium::PlotElectronTownsend(const bool keep) {

  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Townsend coefficient [1/cm]", 3);
  canvas->Update();

}

void 
ViewMedium::PlotHoleTownsend(const bool keep) {

  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Townsend coefficient [1/cm]", 13);
  canvas->Update();

}

void 
ViewMedium::PlotElectronAttachment(const bool keep) {

  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Attachment coefficient [1/cm]", 4);
  canvas->Update();

}

void 
ViewMedium::PlotHoleAttachment(const bool keep) {

  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Attachment coefficient [1/cm]", 14);
  canvas->Update();
  
}

void
ViewMedium::SetupCanvas() {

  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle("Medium View");
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();

}

void
ViewMedium::AddFunction(const double xmin, const double xmax, 
                        const double ymin, const double ymax,
                        const bool keep,
                        const std::string xlabel, const std::string ylabel,
                        const int type) {

  if (medium == 0) {
    std::cerr << className << "::AddFunction:\n";
    std::cerr << "    Medium is not defined.\n";
    return;
  }

  int idx = 0;
  std::string fname = "fMediumView_0";
  while (gROOT->GetListOfFunctions()->FindObject(fname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << "fMediumView_";
    ss  << idx;
    fname = ss.str();
  }

  if (!keep) {
    functions.clear();
    nFunctions = 0;
  }

  TF1 fNew(fname.c_str(), this, &ViewMedium::EvaluateFunction, 
            xmin, xmax, 1, "ViewMedium", "EvaluateFunction");
  functions.push_back(fNew);
  ++nFunctions;

  const std::string title = medium->GetName() + ";" + 
                            xlabel + ";" + ylabel;
  functions.back().SetRange(xmin, xmax);
  if ((fabs(ymax - ymin) > 0.)) {
    functions.back().SetMinimum(ymin);
    functions.back().SetMaximum(ymax);
  }
  functions.back().GetXaxis()->SetTitle(xlabel.c_str());
  functions.back().GetYaxis()->SetTitle(ylabel.c_str());
  functions.back().SetTitle(title.c_str());
  functions.back().SetParameter(0, type);
  if (type < 10) {
    functions.back().SetLineColor(kOrange);
  } else if (type < 20) {
    functions.back().SetLineColor(kGreen + 2);
  } else {
    functions.back().SetLineColor(kRed);
  }
  if (keep && nFunctions > 1) {
    functions[0].Draw("");
    for (int i = 1; i < nFunctions; ++i) {
     functions[i].Draw("same");
    }
  } else {
    functions.back().Draw("");
  }
 
}

double
ViewMedium::EvaluateFunction(double* pos, double* par) {

  if (medium == 0) return 0.;
  
  int type = int(par[0]);
  const double x = pos[0];
  double y = 0.;

  // Auxiliary variables
  double a = 0., b = 0., c = 0.;

  switch (type) {
    case 0:
      // Electron drift velocity
      if (!medium->ElectronVelocity(x, 0, 0, 0, 0, 0, a, b, c)) return 0.;
      y = fabs(a);
      break;
    case 1:
      // Electron transverse diffusion
      if (!medium->ElectronDiffusion(x, 0, 0, 0, 0, 0, a, b)) return 0.;
      y = b;
      break;
    case 2:
      // Electron longitudinal diffusion
      if (!medium->ElectronDiffusion(x, 0, 0, 0, 0, 0, a, b)) return 0.;
      y = a;
      break;
    case 3:
      // Electron Townsend coefficient
      if (!medium->ElectronTownsend(x, 0, 0, 0, 0, 0, a)) return 0.;
      y = a;
      break;
    case 4:
      // Electron attachment coefficient
      if (!medium->ElectronAttachment(x, 0, 0, 0, 0, 0, a)) return 0.;
      y = a;
      break;
    case 10:
      // Hole drift velocity
      if (!medium->HoleVelocity(x, 0, 0, 0, 0, 0, a, b, c)) return 0.;
      y = a;
      break;
    case 11:
      // Hole transverse diffusion
      if (!medium->HoleDiffusion(x, 0, 0, 0, 0, 0, a, b)) return 0.;
      y = b;
      break;
    case 12:
      // Hole longitudinal diffusion
      if (!medium->HoleDiffusion(x, 0, 0, 0, 0, 0, a, b)) return 0.;
      y = a;
      break;
    case 13:
      // Hole Townsend coefficient
      if (!medium->HoleTownsend(x, 0, 0, 0, 0, 0, a)) return 0.;
      y = a;
      break;
    case 14:
      // Hole attachment coefficient
      if (!medium->HoleAttachment(x, 0, 0, 0, 0, 0, a)) return 0.;
      y = a;
      break;
    case 20:
      // Ion drift velocity
      if (!medium->IonVelocity(x, 0, 0, 0, 0, 0, a, b, c)) return 0.;
      y = fabs(a);
      break;
    case 21:
      // Ion transverse diffusion
      if (!medium->IonDiffusion(x, 0, 0, 0, 0, 0, a, b)) return 0.;
      y = b;
      break;
    case 22:
      // Ion longitudinal diffusion
      if (!medium->IonDiffusion(x, 0, 0, 0, 0, 0, a, b)) return 0.;
      y = a;
      break;
    default:
      std::cerr << className << "::EvaluateFunction:\n";
      std::cerr << "    Unknown type of transport coefficient requested.\n"; 
      std::cerr << "    Program bug!\n";
      return 0.;
  }

  return y;
    
}

}
