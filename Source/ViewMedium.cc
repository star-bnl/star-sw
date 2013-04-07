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
  nFunctions(0), nGraphs(0) {
  
  className = "ViewMedium";

  functions.clear();
  graphs.clear();
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
ViewMedium::SetFunctionRange(const double vmin, const double vmax) {

  if (vmin >= vmax || vmin < 0.) {
    std::cerr << className << "::SetFunctionRange:\n";
    std::cerr << "    Incorrect range.\n";
    return;
  }

  vMin = vmin; vMax = vmax;

}

void
ViewMedium::SetFunctionRange() {

  vMin = vMax = 0.;

}

void 
ViewMedium::PlotElectronVelocity() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", "drift velocity [cm/ns]", 0);
  canvas->Update();
  
}

void 
ViewMedium::PlotHoleVelocity() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", "drift velocity [cm/ns]", 10);
  canvas->Update();

}

void 
ViewMedium::PlotIonVelocity() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", "drift velocity [cm/ns]", 20);
  canvas->Update();

}

void
ViewMedium::PlotElectronDiffusion() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", 
              "diffusion coefficient [#sqrt{cm}]", 1);
  keep = true;
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]",
              "diffusion coefficient [#sqrt{cm}]", 2);

  canvas->Update();

}
 
void
ViewMedium::PlotHoleDiffusion() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", 
              "diffusion coefficient [#sqrt{cm}]", 11);
  keep = true;
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]",
              "diffusion coefficient [#sqrt{cm}]", 12);
  canvas->Update();

}

void
ViewMedium::PlotIonDiffusion() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", 
              "diffusion coefficient [#sqrt{cm}]", 21);
  keep = true;
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]",
              "diffusion coefficient [#sqrt{cm}]", 22);
  canvas->Update();

}

void
ViewMedium::PlotElectronTownsend() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Townsend coefficient [1/cm]", 3);
  canvas->Update();

}

void 
ViewMedium::PlotHoleTownsend() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Townsend coefficient [1/cm]", 13);
  canvas->Update();

}

void 
ViewMedium::PlotElectronAttachment() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Attachment coefficient [1/cm]", 4);
  canvas->Update();

}

void 
ViewMedium::PlotHoleAttachment() {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Attachment coefficient [1/cm]", 14);
  canvas->Update();
  
}

void
ViewMedium::PlotElectronCrossSections() {

  SetupCanvas();

}

void
ViewMedium::SetupCanvas() {

  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle("Medium View");
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();
  gPad->SetLeftMargin(0.15);

}

void
ViewMedium::AddFunction(const double xmin, const double xmax, 
                        const double ymin, const double ymax,
                        const bool keep,
                        const std::string xlabel, const std::string ylabel,
                        const int type) {

  // Make sure the medium pointer is set.
  if (medium == 0) {
    std::cerr << className << "::AddFunction:\n";
    std::cerr << "    Medium is not defined.\n";
    return;
  }

  // Look for an unused function name.
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
    graphs.clear();
    nGraphs = 0;
  }

  // Create a TF1 and add it to the list of functions.
  TF1 fNew(fname.c_str(), this, &ViewMedium::EvaluateFunction, 
            xmin, xmax, 1, "ViewMedium", "EvaluateFunction");
  fNew.SetNpx(1000);
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
  functions.back().GetXaxis()->SetTitleOffset(1.2);
  functions.back().GetYaxis()->SetTitle(ylabel.c_str());
  functions.back().SetTitle(title.c_str());
  functions.back().SetParameter(0, type);
  
  // Set the color and marker style.
  int color;
  int marker = 20;
  if (type == 2 || type == 4) {
    color = plottingEngine.GetRootColorLine1();
  } else if (type == 12 || type == 14 || type == 22) {
    color = plottingEngine.GetRootColorLine2();
  } else if (type < 10) {
    color = plottingEngine.GetRootColorElectron();
  } else if (type < 20) {
    color = plottingEngine.GetRootColorHole();
  } else {
    color = plottingEngine.GetRootColorIon();
  }
  functions.back().SetLineColor(color);
  // Get the field grid.
  std::vector<double> efields;
  std::vector<double> bfields;
  std::vector<double> bangles;
  medium->GetFieldGrid(efields, bfields, bangles);
  const int nEfields = efields.size();
  const int nBfields = bfields.size();
  const int nBangles = bangles.size();
  bool withGraph = true;
  if (nEfields <= 0 || nBfields <= 0 || nBangles <= 0) {
    withGraph = false;
  }
  // TODO: plots for different B fields
  // bool withBfield = false;
  // if (nBfields > 1) {
  //   withBfield = true;
  // } else if (nBfields == 1 && bfields[0] > 0.) {
  //   withBfield = true;
  // }

  if (withGraph) {
    TGraph graph(nEfields);
    graph.SetMarkerStyle(marker);
    graph.SetMarkerColor(color);
    bool ok = true;
    for (int i = 0; i < nEfields; ++i) {
      double value = 0.;
      switch (type) {
        case 0:
          // Electron drift velocity along E
          ok = medium->GetElectronVelocityE(i, 0, 0, value);
          value = medium->ScaleVelocity(value);
          break;
        case 1:
          // Electron transverse diffusion
          ok = medium->GetElectronTransverseDiffusion(i, 0, 0, value);
          value = medium->ScaleDiffusion(value);
          break;
        case 2:
          // Electron longitudinal diffusion
          ok = medium->GetElectronLongitudinalDiffusion(i, 0, 0, value);
          value = medium->ScaleDiffusion(value);
          break;
        case 3:
          // Electron Townsend coefficient
          ok = medium->GetElectronTownsend(i, 0, 0, value);
          value = medium->ScaleTownsend(exp(value));
          break;
        case 4:
          // Electron attachment coefficient
          ok = medium->GetElectronAttachment(i, 0, 0, value);
          value = medium->ScaleAttachment(exp(value));
          break;
        case 10:
          // Hole drift velocity along E
          ok = medium->GetHoleVelocityE(i, 0, 0, value);
          value = medium->ScaleVelocity(value);
          break;
        case 11:
          // Hole transverse diffusion
          ok = medium->GetHoleTransverseDiffusion(i, 0, 0, value);
          value = medium->ScaleDiffusion(value);
          break;
        case 12:
          // Hole longitudinal diffusion
          ok = medium->GetHoleLongitudinalDiffusion(i, 0, 0, value);
          value = medium->ScaleDiffusion(value);
          break;
        case 13:
          // Hole Townsend coefficient
          ok = medium->GetHoleTownsend(i, 0, 0, value);
          value = medium->ScaleTownsend(exp(value));
          break;
        case 14:
          // Hole attachment coefficient
          ok = medium->GetHoleAttachment(i, 0, 0, value);
          value = medium->ScaleAttachment(exp(value));
          break;
        case 20:
          // Ion drift velocity
          ok = medium->GetIonMobility(i, 0, 0, value);
          value *= medium->UnScaleElectricField(efields[i]);
          break;
        case 21:
          // Ion transverse diffusion
          ok = medium->GetIonTransverseDiffusion(i, 0, 0, value);
          value = medium->ScaleDiffusion(value);
          break;
        case 22:
          // Ion longitudinal diffusion
          ok = medium->GetIonLongitudinalDiffusion(i, 0, 0, value);
          value = medium->ScaleDiffusion(value);
          break;
        default:
          ok = false;
          break;
      }
      if (!ok) {
        withGraph = false;
        break;
      }
      graph.SetPoint(i, medium->UnScaleElectricField(efields[i]), value);
    }
    if (ok) {
      graphs.push_back(graph);
      ++nGraphs;
    } else {
      std::cerr << className << "::AddFunction:\n";
      std::cerr << "    Error retrieving data table.\n";
      std::cerr << "    Suppress plotting of graph.\n";
    }
  }
        
  if (keep && nFunctions > 1) {
    functions[0].GetYaxis()->SetTitleOffset(1.5);
    functions[0].Draw("");
    for (int i = 1; i < nFunctions; ++i) {
     functions[i].Draw("lsame");
    }
  } else {
    functions.back().GetYaxis()->SetTitleOffset(1.5);
    functions.back().Draw("");
  }
  if (nGraphs > 0) {
    for (int i = 0; i < nGraphs; ++i) { 
      graphs[i].Draw("p");
    }
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
