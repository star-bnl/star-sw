#include <iostream>

#include "Plotting.hh"
#include "ViewDrift.hh"

namespace Garfield {

ViewDrift::ViewDrift() :
  debug(false),
  label("Drift Lines"),
  canvas(0), hasExternalCanvas(false),
  xMin(-1.), yMin(-1.), zMin(-1.), 
  xMax( 1.), yMax( 1.), zMax( 1.),
  view(0),
  nDriftLines(0), nTracks(0) {

  plottingEngine.SetDefaultStyle();
  colorElectron        = plottingEngine.GetRootColor("orange");
  colorIon             = plottingEngine.GetRootColor("red");
  colorHole            = plottingEngine.GetRootColor("red");
  colorPhoton          = plottingEngine.GetRootColor("blue");
  colorChargedParticle = plottingEngine.GetRootColor("dark-green");
  driftLines.clear();
  tracks.clear(); 

}

ViewDrift::~ViewDrift() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;
  if (view != 0) delete view;

}

void
ViewDrift::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void 
ViewDrift::SetArea(double xmin, double ymin, double zmin, 
                   double xmax, double ymax, double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    std::cout << "ViewDrift::SetArea:\n";
    std::cout << "    Null area range not permitted.\n";
    return;
  }
  xMin = std::min(xmin, xmax);
  yMin = std::min(ymin, ymax);
  zMin = std::min(zmin, zmax);
  xMax = std::max(xmin, xmax);
  yMax = std::max(ymin, ymax);
  zMax = std::max(zmin, zmax);
  
}

void
ViewDrift::Clear() {

  driftLines.clear();
  nDriftLines = 0;

}

void
ViewDrift::NewElectronDriftLine(const int np, int& id,
                      const double x0, const double y0, const double z0) {

  // Create a new electron drift line and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    TPolyLine3D p(1);
    p.SetLineColor(colorElectron);
    p.SetPoint(0, x0, y0, z0);
    driftLines.push_back(p);
  } else {
    TPolyLine3D p(np);
    p.SetLineColor(colorElectron);
    p.SetPoint(0, x0, y0, z0);
    driftLines.push_back(p);
  }
  // Return the index of this drift line.
  id = nDriftLines;
  ++nDriftLines;

} 

void
ViewDrift::NewIonDriftLine(const int np, int& id,
                 const double x0, const double y0, const double z0) {

  // Create a new ion drift line and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    TPolyLine3D p(1);
    p.SetLineColor(colorIon);
    p.SetPoint(0, x0, y0, z0);
    driftLines.push_back(p);
  } else {
    TPolyLine3D p(np);
    p.SetLineColor(colorIon);
    p.SetPoint(0, x0, y0, z0);
    driftLines.push_back(p);
  }
  // Return the index of this drift line.
  id = nDriftLines;
  ++nDriftLines;

}

void
ViewDrift::NewPhotonTrack(const double x0, const double y0, const double z0,
                          const double x1, const double y1, const double z1) {

  // Create a new photon track (line between start and end point).
  TPolyLine3D p(2);
  p.SetLineColor(colorPhoton);
  p.SetLineStyle(7);
  p.SetPoint(0, x0, y0, z0);
  p.SetPoint(1, x1, y1, z1);
  driftLines.push_back(p);
  ++nDriftLines;

}

void
ViewDrift::NewChargedParticleTrack(const int np, int& id,
                     const double x0, const double y0, const double z0) {

  // Create a new track and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    TPointSet3D p(1);
    p.SetMarkerColor(colorChargedParticle);
    p.SetPoint(0, x0, y0, z0);
    tracks.push_back(p);
  } else {
    TPointSet3D p(np);
    p.SetMarkerColor(colorChargedParticle);
    p.SetPoint(0, x0, y0, z0);
    tracks.push_back(p);
  }
  // Return the index of this drift line.
  id = nTracks;
  ++nTracks;

}

void
ViewDrift::SetDriftLinePoint(const int iL, const int iP, 
                             const double x, const double y, const double z) {
  
  if (iL < 0 || iL >= nDriftLines) {
    std::cerr << "ViewDrift::SetDriftLinePoint:\n";
    std::cerr << "    Drift line index " << iL << " is out of range.\n";
    return;
  }

  if (iP < 0) return;
  
  driftLines[iL].SetPoint(iP, x, y, z);

}

void
ViewDrift::AddDriftLinePoint(const int iL,
                             const double x, const double y, const double z) {

  if (iL < 0 || iL >= nDriftLines) {
    std::cerr << "ViewDrift::AddDriftLinePoint:\n";
    std::cerr << "    Drift line index " << iL << " is out of range.\n";
    return;
  }

  driftLines[iL].SetNextPoint(x, y, z);

}

void
ViewDrift::SetTrackPoint(const int iL, const int iP, 
                         const double x, const double y, const double z) {
  
  if (iL < 0 || iL >= nTracks) {
    std::cerr << "ViewDrift::SetTrackPoint:\n";
    std::cerr << "    Track index " << iL << " is out of range.\n";
    return;
  }

  if (iP < 0) return;
  
  tracks[iL].SetPoint(iP, x, y, z);

}

void
ViewDrift::AddTrackPoint(const int iL,
                         const double x, const double y, const double z) {

  if (iL < 0 || iL >= nTracks) {
    std::cerr << "ViewDrift::AddTrackPoint:\n";
    std::cerr << "    Track index " << iL << " is out of range.\n";
    return;
  }

  tracks[iL].SetNextPoint(x, y, z);

}

void
ViewDrift::Plot() {
  
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle(label.c_str());
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();

  if (view == 0) view = TView::CreateView(1, 0, 0);
  view->SetRange(xMin, yMin, zMin, xMax, yMax, zMax);
  view->ShowAxis();
  view->Top();

  for (int i = nDriftLines; i--;) {
    driftLines[i].Draw("same");
  }
  for (int i = nTracks; i--;) {
    tracks[i].Draw("same");
  }
  canvas->Update();

}

void
ViewDrift::SetElectronColor(const std::string color) {

  colorElectron = plottingEngine.GetRootColor(color);

}

void
ViewDrift::SetIonColor(const std::string color) {
  
  colorIon = plottingEngine.GetRootColor(color);

}

void
ViewDrift::SetHoleColor(const std::string color) {

  colorHole = plottingEngine.GetRootColor(color);

}

void
ViewDrift::SetPhotonColor(const std::string color) {

  colorPhoton = plottingEngine.GetRootColor(color);

}

void
ViewDrift::SetChargedParticleColor(const std::string color) {

  colorChargedParticle = plottingEngine.GetRootColor(color);

}

}
