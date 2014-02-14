
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
  eMin(0.), eMax(1000.), bMin(0.), bMax(5.), aMin(0.), aMax(3.14),
  vMin(0.), vMax(0.),
  efield(500.), bfield(1.e2), angle(0.),
  etolerance(1.), btolerance(0.01), atolerance(0.05),
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
ViewMedium::SetBAngleRange(const double amin, const double amax) {

  if (amin >= amax || amin < 0.) { 
    std::cerr << className << "::SetBAngleRange:\n";
    std::cerr << "    Incorrect field range.\n";
    return;
  }

  aMin = amin; aMax = amax;

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
ViewMedium::PlotElectronVelocity(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  double min = 0., max = 0.;
  std::string title = ""; 
  if(xaxis == 'e') {title = "electric field [V/cm]"; min = eMin; max = eMax;}
  if(xaxis == 'b') {title = "magnetic field [T]"; min = bMin; max = bMax;}
  if(xaxis == 'a') {title = "magnetic field angle [rad]"; min = aMin; max = aMax;}
  AddFunction(min, max, vMin, vMax, keep,
              title, "drift velocity [cm/ns]", 0, xaxis, e, b, a);
  keep = true;  
  AddFunction(min, max, vMin, vMax, keep,
              title, "drift velocity [cm/ns]", 23, xaxis, e, b, a);
  keep = true;
  AddFunction(min, max, vMin, vMax, keep,
              title, "drift velocity [cm/ns]", 24, xaxis, e, b, a);
  canvas->Update();
}

void 
ViewMedium::PlotHoleVelocity(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  double min = 0., max = 0.;
  std::string title = ""; 
  if(xaxis == 'e') {title = "electric field [V/cm]"; min = eMin; max = eMax;}
  if(xaxis == 'b') {title = "magnetic field [T]"; min = bMin; max = bMax;}
  if(xaxis == 'a') {title = "magnetic field angle [rad]"; min = aMin; max = aMax;}
  AddFunction(eMin, eMax, vMin, vMax, keep,
              title, "drift velocity [cm/ns]", 10, xaxis, e, b, a);
  keep = true;
  AddFunction(eMin, eMax, vMin, vMax, keep,
              title, "drift velocity [cm/ns]", 25, xaxis, e, b, a);
  keep = true;
  AddFunction(eMin, eMax, vMin, vMax, keep,
              title, "drift velocity [cm/ns]", 26, xaxis, e, b, a);
  canvas->Update();

}

void 
ViewMedium::PlotIonVelocity(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", "drift velocity [cm/ns]", 20, xaxis, e, b, a);
  canvas->Update();

}

void
ViewMedium::PlotElectronDiffusion(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", 
              "diffusion coefficient [#sqrt{cm}]", 1, xaxis, e, b, a);
  keep = true;
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]",
              "diffusion coefficient [#sqrt{cm}]", 2, xaxis, e, b, a);

  canvas->Update();

}
 
void
ViewMedium::PlotHoleDiffusion(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", 
              "diffusion coefficient [#sqrt{cm}]", 11, xaxis, e, b, a);
  keep = true;
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]",
              "diffusion coefficient [#sqrt{cm}]", 12, xaxis, e, b, a);
  canvas->Update();

}

void
ViewMedium::PlotIonDiffusion(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]", 
              "diffusion coefficient [#sqrt{cm}]", 21, xaxis, e, b, a);
  keep = true;
  AddFunction(eMin, eMax, vMin, vMax, keep,
              "electric field [V/cm]",
              "diffusion coefficient [#sqrt{cm}]", 22, xaxis, e, b, a);
  canvas->Update();

}

void
ViewMedium::PlotElectronTownsend(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Townsend coefficient [1/cm]", 3, xaxis, e, b, a);
  canvas->Update();

}

void 
ViewMedium::PlotHoleTownsend(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Townsend coefficient [1/cm]", 13, xaxis, e, b, a);
  canvas->Update();

}

void 
ViewMedium::PlotElectronAttachment(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Attachment coefficient [1/cm]", 4, xaxis, e, b, a);
  canvas->Update();

}

void 
ViewMedium::PlotHoleAttachment(const char xaxis, const double e, const double b, const double a) {

  bool keep = false;
  SetupCanvas();
  AddFunction(eMin, eMax, 0., 0., keep,
              "electric field [V/cm]", "Attachment coefficient [1/cm]", 14, xaxis, e, b, a);
  canvas->Update();
  
}

void
ViewMedium::PlotElectronCrossSections(const char xaxis, const double e, const double b, const double a) {

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
                        const int type, const char xaxis, const double e, const double b, const double a) {

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

  //Set global variables used later on (effectively we will only use two)
  efield = e;
  bfield = b;
  angle = a;

  // Create a TF1 and add it to the list of functions.
  TF1 fNew(fname.c_str(), this, &ViewMedium::EvaluateFunction, 
            xmin, xmax, 2, "ViewMedium", "EvaluateFunction");
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
  functions.back().SetParameter(1, xaxis);
  // Set the color and marker style.
  int color;
  int marker = 20;
  if (type == 2 || type == 4) {
    color = plottingEngine.GetRootColorLine1();
  } else if (type == 12 || type == 14 || type == 22) {
    color = plottingEngine.GetRootColorLine2();
  } else if (type < 10) {
    color = plottingEngine.GetRootColorElectron();
  }
  else if(type == 23){
    color = kGreen;
  }
  else if(type == 24){
    color = kRed;
  } else if (type < 20 || type == 25 || type == 26) {
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
  // withBfield variable doesn't make sense anymore; to be removed
  bool withBfield = false;
  if (nBfields > 1) {
     withBfield = true;
   } else if (nBfields == 1 && bfields[0] > 0.) {
     withBfield = true;
   }

  if (withGraph) {
    int n = 0;
    TGraph graph;
    if(xaxis == 'e'){    //plot with respect to E field
      graph.Set(nEfields);
      n = nEfields;
    }
    else if(xaxis == 'b'){  //plot wrt B field
      graph.Set(nBfields);
      n = nBfields;
    }
    else if(xaxis == 'a'){  //plot wrt angle
      graph.Set(nBangles);
      n = nBangles;
    }
    else {
      std::cerr << className << "::AddFunction:\n";
      std::cerr << "    Error specifying X-axis.\n";
      std::cerr << "    Invalid parameter type.\n";
      return;
    }
    graph.SetMarkerStyle(marker);
    graph.SetMarkerColor(color);
    bool ok = true;
    
    int epoint = -1;
    int bpoint = -1;
    int apoint = -1;
    //if one of these stays -1, this means there is no corresponding point in the table
    
    //search for matching point in table with a certain accuracy
    for(int i = 0; i < n; ++i) {
      if(fabs(efields[i] - efield) <= etolerance) {epoint = i; break;}
    }
    for(int i = 0; i < n; ++i) {
      if(fabs(bfields[i] - bfield) <= btolerance) {bpoint = i; break;}
    }
    for(int i = 0; i < n; ++i) {
      if(fabs(bangles[i] - angle) <= atolerance) {apoint = i; break;}
    }
    if((xaxis == 'e' && (bpoint == -1 || apoint == -1)) || (xaxis == 'b' && (epoint == -1 || apoint == -1)) || (xaxis == 'a' && (epoint == -1 || bpoint == -1)))      ok = false;

    for (int i = 0; i < n; ++i) { 
      double value = 0.;
      // auxiliary variables
      double alongx = 0, alongy = 0., alongz = 0.;
      if (!ok) {
        withGraph = false;
        break;
      } 
      switch (type) {
        case 0:
          // Electron drift velocity along E
          if(xaxis == 'e') ok = medium->GetElectronVelocityE(i, bpoint, apoint, value);
          else if(xaxis == 'b') ok = medium->GetElectronVelocityE(epoint, i, apoint, value);
          else if(xaxis == 'a') ok = medium->GetElectronVelocityE(epoint, bpoint, i , value);
          else value = 0.;
          value = medium->ScaleVelocity(value);
          break;
        case 1:
          // Electron transverse diffusion
          ok = medium->GetElectronTransverseDiffusion(i, bpoint, apoint, value);
          value = medium->ScaleDiffusion(value);
          break;
          case 2:
            // Electron longitudinal diffusion
            ok = medium->GetElectronLongitudinalDiffusion(i, bpoint, apoint, value);
            value = medium->ScaleDiffusion(value);
            break;
          case 3:
            // Electron Townsend coefficient
            ok = medium->GetElectronTownsend(i, bpoint, apoint, value);
            value = medium->ScaleTownsend(exp(value));
            break;
          case 4:
            // Electron attachment coefficient
            ok = medium->GetElectronAttachment(i, bpoint, apoint, value);
            value = medium->ScaleAttachment(exp(value));
            break;
          case 10:
            // Hole drift velocity along E
            ok = medium->GetHoleVelocityE(i, bpoint, apoint, value);
            value = medium->ScaleVelocity(value);
            break;
          case 11:
            // Hole transverse diffusion
            ok = medium->GetHoleTransverseDiffusion(i, bpoint, apoint, value);
            value = medium->ScaleDiffusion(value);
            break;
          case 12:
            // Hole longitudinal diffusion
            ok = medium->GetHoleLongitudinalDiffusion(i, bpoint, apoint, value);
            value = medium->ScaleDiffusion(value);
            break;
          case 13:
            // Hole Townsend coefficient
            ok = medium->GetHoleTownsend(i, bpoint, apoint, value);
            value = medium->ScaleTownsend(exp(value));
            break;
          case 14:
            // Hole attachment coefficient
            ok = medium->GetHoleAttachment(i, bpoint, apoint, value);
            value = medium->ScaleAttachment(exp(value));
            break;
          case 20:
            // Ion drift velocity
            ok = medium->GetIonMobility(i, bpoint, apoint, value);
            value *= medium->UnScaleElectricField(efields[i]);
            break;
          case 21:
            // Ion transverse diffusion
            ok = medium->GetIonTransverseDiffusion(i,bpoint, apoint, value);
            value = medium->ScaleDiffusion(value);
            break;
          case 22:
            // Ion longitudinal diffusion
            ok = medium->GetIonLongitudinalDiffusion(i, bpoint, apoint, value);
            value = medium->ScaleDiffusion(value);
            break;
          case 23:
            // Electron drift velocity along B
            if(xaxis == 'e') 
              ok = medium->ElectronVelocity(efields[i]*cos(bangles[apoint]), efields[i]*sin(bangles[apoint]),
                    0, bfields[bpoint], 0, 0, value, alongy, alongz);
            else if(xaxis == 'b') 
              ok = medium->ElectronVelocity(efields[epoint]*cos(bangles[apoint]), efields[epoint]*sin(bangles[apoint]), 
                    0, bfields[i], 0, 0, value, alongy, alongz);
            else if(xaxis == 'a'){
              ok = medium->ElectronVelocity(efields[epoint]*cos(bangles[i]), efields[epoint]*sin(bangles[i]), 
                    0, bfields[bpoint], 0, 0, value, alongy, alongz);
            }
            else value = 0.;
            value = fabs(medium->ScaleVelocity(value));
            break;
           case 24:
            // Electron drift velocity along ExB
            if(xaxis == 'e') 
              ok = medium->ElectronVelocity(efields[i]*cos(bangles[apoint]), efields[i]*sin(bangles[apoint]), 
                     0, bfields[bpoint], 0, 0, alongx, alongy, value);
            else if(xaxis == 'b') 
              ok = medium->ElectronVelocity(efields[epoint]*cos(bangles[apoint]), efields[epoint]*sin(bangles[apoint]), 
                     0, bfields[i], 0, 0, alongx, alongy, value);
            else if(xaxis == 'a') 
              ok = medium->ElectronVelocity(efield*cos(bangles[i]), efield*sin(bangles[i]), 
                     0, bfield, 0, 0, alongx, alongy, value);
            else value = 0.;
            value = fabs(medium->ScaleVelocity(value));
            break;
           case 25:
            // Hole drift velocity along B
            if(xaxis == 'e') 
              ok = medium->HoleVelocity(efields[i]*cos(bangles[apoint]), efields[i]*sin(bangles[apoint]), 
                     0, bfields[bpoint], 0, 0, value, alongy, alongz);
            else if(xaxis == 'b') 
              ok = medium->HoleVelocity(efields[epoint]*cos(bangles[apoint]), efields[epoint]*sin(bangles[apoint]), 
                     0, bfields[i], 0, 0, value, alongy, alongz);
            else if(xaxis == 'a')
              ok = medium->HoleVelocity(efields[epoint]*cos(bangles[i]), efields[epoint]*sin(bangles[i]), 
                     0, bfields[bpoint], 0, 0, value, alongy, alongz);
           break;
           case 26:
            // Hole velocity along ExB
            if(xaxis == 'e') 
              ok = medium->HoleVelocity(efields[i]*cos(bangles[apoint]), efields[i]*sin(bangles[apoint]), 
                     0, bfields[bpoint], 0, 0, alongx, alongy, value);
            else if(xaxis == 'b') 
              ok = medium->HoleVelocity(efields[epoint]*cos(bangles[apoint]), efields[epoint]*sin(bangles[apoint]), 
                     0, bfields[i], 0, 0, alongx, alongy, value);
            else if(xaxis == 'a') 
              ok = medium->HoleVelocity(efield*cos(bangles[i]), efield*sin(bangles[i]), 
                     0, bfield, 0, 0, alongx, alongy, value);
            else value = 0.;
            value = fabs(medium->ScaleVelocity(value));
            break;
        }
        if(xaxis == 'e') graph.SetPoint(i, medium->UnScaleElectricField(efields[i]), value);
        else if(xaxis == 'b') graph.SetPoint(i, bfields[i], value);
        else if(xaxis == 'a') graph.SetPoint(i, bangles[i], value);
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
ViewMedium::EvaluateFunction(double* pos, double* par) {    //to be modified to include B and angle
  
  if (medium == 0) return 0.;
  int type = int(par[0]);
  char xaxis = char(par[1]);
  const double x = pos[0];
  double y = 0.;

  // Auxiliary variables
  double value = 0., a = 0., b = 0., c = 0., alongx = 0., alongy = 0., alongz = 0.;

  switch (type) {
    case 0:
      // Electron drift velocity
      if(xaxis == 'e'){    //plot with respect to E field
        if (!medium->ElectronVelocity(x, 0, 0, bfield*cos(angle), bfield*sin(angle), 0, a, b, c)) return 0.;
      }
      else if(xaxis == 'b'){  //plot wrt B field
        if (!medium->ElectronVelocity(efield, 0, 0, x*cos(angle), x*sin(angle), 0, a, b, c)) return 0.;
      }
      else if(xaxis == 'a'){  //plot wrt angle
        if (!medium->ElectronVelocity(efield, 0, 0, bfield*cos(x), bfield*sin(x), 0, a, b, c)) return 0.;        
      }
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
    case 23:
     // Electron drift velocity along B
     if(xaxis == 'e'){    //plot with respect to E field
      if (!medium->ElectronVelocity(x*cos(angle), x*sin(angle), 0, bfield, 0, 0, value, alongy, alongz)) return 0.;
     }
     else if(xaxis == 'b'){  //plot wrt B field
       if (!medium->ElectronVelocity(efield*cos(angle), efield*sin(angle), 0, x, 0, 0, value, alongy, alongz)) return 0.;
     }
     else if(xaxis == 'a'){  //plot wrt angle
       if (!medium->ElectronVelocity(efield*cos(x), efield*sin(x), 0, bfield, 0, 0, value, alongy, alongz)) return 0.; 
     }
     y = fabs(value);
     break;
    case 24:
     // Electron drift velocity along ExB
     if(xaxis == 'e'){    //plot with respect to E field
       if (!medium->ElectronVelocity(x*cos(angle), x*sin(angle), 0, bfield, 0, 0, alongx, alongy, value)) return 0.;
     }
     else if(xaxis == 'b'){  //plot wrt B field
       if (!medium->ElectronVelocity(efield*cos(angle), efield*sin(angle), 0, x, 0, 0, alongx, alongy, value)) return 0.;
     }
     else if(xaxis == 'a'){  //plot wrt angle
       if (!medium->ElectronVelocity(efield*cos(x), efield*sin(x), 0, bfield, 0, 0, alongx, alongy, value)) return 0.; 
     }
     y = fabs(value);
     break;
     case 25:
     // Hole drift velocity along B
     if(xaxis == 'e'){    //plot with respect to E field
      if (!medium->HoleVelocity(x*cos(angle), x*sin(angle), 0, bfield, 0, 0, value, alongy, alongz)) return 0.;
     }
     else if(xaxis == 'b'){  //plot wrt B field
       if (!medium->HoleVelocity(efield*cos(angle), efield*sin(angle), 0, x, 0, 0, value, alongy, alongz)) return 0.;
     }
     else if(xaxis == 'a'){  //plot wrt angle
       if (!medium->HoleVelocity(efield*cos(x), efield*sin(x), 0, bfield, 0, 0, value, alongy, alongz)) return 0.; 
     }
     y = fabs(value);
     break;
    case 26:
     // Hole drift velocity along ExB
     if(xaxis == 'e'){    //plot with respect to E field
       if (!medium->HoleVelocity(x*cos(angle), x*sin(angle), 0, bfield, 0, 0, alongx, alongy, value)) return 0.;
     }
     else if(xaxis == 'b'){  //plot wrt B field
       if (!medium->HoleVelocity(efield*cos(angle), efield*sin(angle), 0, x, 0, 0, alongx, alongy, value)) return 0.;
     }
     else if(xaxis == 'a'){  //plot wrt angle
       if (!medium->HoleVelocity(efield*cos(x), efield*sin(x), 0, bfield, 0, 0, alongx, alongy, value)) return 0.; 
     }
     y = fabs(value);
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
