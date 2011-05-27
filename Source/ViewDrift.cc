#include <iostream>

#include "Plotting.hh"
#include "ViewDrift.hh"

namespace Garfield {

ViewDrift::ViewDrift() :
  className("ViewDrift"), debug(false),
  label("Drift Lines"),
  canvas(0), hasExternalCanvas(false),
  xMin(-1.), yMin(-1.), zMin(-1.), 
  xMax( 1.), yMax( 1.), zMax( 1.),
  view(0),
  nDriftLines(0), nTracks(0),
  nExcMarkers(0), excPlot(0), 
  nIonMarkers(0), ionPlot(0), 
  nAttMarkers(0), attPlot(0),
  markerSizeCluster(1.), markerSizeCollision(1.) {

  driftLines.clear();
  tracks.clear();
  excMarkers.clear();
  ionMarkers.clear();
  attMarkers.clear();

}

ViewDrift::~ViewDrift() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;
  if (view != 0) delete view;
  delete excPlot; excPlot = 0;
  delete ionPlot; ionPlot = 0;
  delete attPlot; attPlot = 0;

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
    std::cout << className << "::SetArea:\n";
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

  tracks.clear();
  nTracks = 0;

  nExcMarkers = nIonMarkers = nAttMarkers = 0;
  excMarkers.clear();
  ionMarkers.clear();
  attMarkers.clear();

  delete excPlot; excPlot = 0;
  delete ionPlot; ionPlot = 0;
  delete attPlot; attPlot = 0;

}

void
ViewDrift::SetClusterMarkerSize(const double size) {

  if (size > 0.) {
    markerSizeCluster = size;
  } else {
    std::cerr << className << "::SetClusterMarkerSize:\n";
    std::cerr << "    Size must be positive.\n";
  }

}

void
ViewDrift::SetCollisionMarkerSize(const double size) {

  if (size > 0.) {
    markerSizeCollision = size;
  } else {
    std::cerr << className << "::SetCollisionMarkerSize:\n";
    std::cerr << "    Size must be positive.\n";
  }

}
  
void
ViewDrift::NewElectronDriftLine(const int np, int& id,
                      const double x0, const double y0, const double z0) {

  int col = plottingEngine.GetRootColorElectron();
  // Create a new electron drift line and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    TPolyLine3D p(1);
    p.SetLineColor(col);
    p.SetPoint(0, x0, y0, z0);
    driftLines.push_back(p);
  } else {
    TPolyLine3D p(np);
    p.SetLineColor(col);
    p.SetPoint(0, x0, y0, z0);
    driftLines.push_back(p);
  }
  // Return the index of this drift line.
  id = nDriftLines;
  ++nDriftLines;

} 

void
ViewDrift::NewHoleDriftLine(const int np, int& id,
                      const double x0, const double y0, const double z0) {

  int col = plottingEngine.GetRootColorHole();
  // Create a new hole drift line and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    TPolyLine3D p(1);
    p.SetLineColor(col);
    p.SetPoint(0, x0, y0, z0);
    driftLines.push_back(p);
  } else {
    TPolyLine3D p(np);
    p.SetLineColor(col);
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

  int col = plottingEngine.GetRootColorIon();
  // Create a new ion drift line and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    TPolyLine3D p(1);
    p.SetLineColor(col);
    p.SetPoint(0, x0, y0, z0);
    driftLines.push_back(p);
  } else {
    TPolyLine3D p(np);
    p.SetLineColor(col);
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

  int col = plottingEngine.GetRootColorPhoton();
  // Create a new photon track (line between start and end point).
  TPolyLine3D p(2);
  p.SetLineColor(col);
  p.SetLineStyle(7);
  p.SetPoint(0, x0, y0, z0);
  p.SetPoint(1, x1, y1, z1);
  driftLines.push_back(p);
  ++nDriftLines;

}

void
ViewDrift::NewChargedParticleTrack(const int np, int& id,
                     const double x0, const double y0, const double z0) {

  int col = plottingEngine.GetRootColorChargedParticle();
  // Create a new track and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    TPointSet3D p(1);
    p.SetMarkerColor(col);
    p.SetMarkerStyle(20);
    p.SetMarkerSize(markerSizeCluster);
    p.SetPoint(0, x0, y0, z0);
    tracks.push_back(p);
  } else {
    TPointSet3D p(np);
    p.SetMarkerColor(col);
    p.SetMarkerStyle(20);
    p.SetMarkerSize(markerSizeCluster);
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
    std::cerr << className << "::SetDriftLinePoint:\n";
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
    std::cerr << className << "::AddDriftLinePoint:\n";
    std::cerr << "    Drift line index " << iL << " is out of range.\n";
    return;
  }

  driftLines[iL].SetNextPoint(x, y, z);

}

void
ViewDrift::SetTrackPoint(const int iL, const int iP, 
                         const double x, const double y, const double z) {
  
  if (iL < 0 || iL >= nTracks) {
    std::cerr << className << "::SetTrackPoint:\n";
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
    std::cerr << className << "::AddTrackPoint:\n";
    std::cerr << "    Track index " << iL << " is out of range.\n";
    return;
  }

  tracks[iL].SetNextPoint(x, y, z);

}

void
ViewDrift::AddExcitationMarker(
    const double x, const double y, const double z) {

  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  excMarkers.push_back(newMarker);
  ++nExcMarkers;

}

void
ViewDrift::AddIonisationMarker(
    const double x, const double y, const double z) {
  
  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  ionMarkers.push_back(newMarker);
  ++nIonMarkers;

}

void
ViewDrift::AddAttachmentMarker(
    const double x, const double y, const double z) {

  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  attMarkers.push_back(newMarker);
  ++nAttMarkers;

}

void
ViewDrift::Plot() {
  
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle(label.c_str());
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();

  if (canvas->GetView() == 0) {
    if (view == 0) view = TView::CreateView(1, 0, 0);
    view->SetRange(xMin, yMin, zMin, xMax, yMax, zMax);
    view->ShowAxis();
    view->Top();
    canvas->SetView(view);
  }

  for (int i = nDriftLines; i--;) {
    driftLines[i].Draw("same");
  }
  for (int i = nTracks; i--;) {
    tracks[i].Draw("same");
  }

  if (excPlot != 0) {
    delete excPlot; excPlot = 0;
  }
  excPlot = new TPointSet3D(nExcMarkers);
  excPlot->SetMarkerColor(plottingEngine.GetRootColorLine2());
  excPlot->SetMarkerStyle(20);
  excPlot->SetMarkerSize(markerSizeCollision);
  for (int i = 0; i < nExcMarkers; ++i) {
    // excPlot->SetPoint(i + 1, 
    excPlot->SetNextPoint(
                      excMarkers[i].x, excMarkers[i].y, excMarkers[i].z);
  }
  if (ionPlot != 0) {
    delete ionPlot; ionPlot = 0;
  }
  ionPlot = new TPointSet3D(nIonMarkers);
  ionPlot->SetMarkerColor(plottingEngine.GetRootColorIon());
  ionPlot->SetMarkerStyle(20);
  ionPlot->SetMarkerSize(markerSizeCollision);
  for (int i = 0; i < nIonMarkers; ++i) {
    // ionPlot->SetPoint(i + 1, 
    ionPlot->SetNextPoint(
                      ionMarkers[i].x, ionMarkers[i].y, ionMarkers[i].z);
  }
  if (attPlot != 0) {
    delete attPlot; attPlot = 0;
  }
  attPlot = new TPointSet3D(nAttMarkers);
  attPlot->SetMarkerColor(plottingEngine.GetRootColorLine1());
  attPlot->SetMarkerStyle(20);
  attPlot->SetMarkerSize(markerSizeCollision);
  for (int i = 0; i < nAttMarkers; ++i) {
    // attPlot->SetPoint(i + 1, 
    attPlot->SetNextPoint(
                      attMarkers[i].x, attMarkers[i].y, attMarkers[i].z);
  }
  excPlot->Draw("same");
  ionPlot->Draw("same");
  attPlot->Draw("same");
  canvas->Update();

}

}
