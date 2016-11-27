#include <iostream>

#include <TAxis.h>
#include <TH1F.h>

#include "Plotting.hh"
#include "ViewDrift.hh"

namespace Garfield {

ViewDrift::ViewDrift()
    : m_className("ViewDrift"),
      m_debug(false),
      m_label("Drift Lines"),
      m_canvas(NULL),
      m_hasExternalCanvas(false),
      m_xMin(-1.),
      m_yMin(-1.),
      m_zMin(-1.),
      m_xMax(1.),
      m_yMax(1.),
      m_zMax(1.),
      m_view(NULL),
      m_excPlot(NULL),
      m_ionPlot(NULL),
      m_attPlot(NULL),
      m_markerSizeCluster(1.),
      m_markerSizeCollision(1.) {

  m_driftLines.reserve(1000);
  m_driftLinePlots.reserve(1000);
  m_tracks.reserve(100);
  m_trackPlots.reserve(100);
  m_excMarkers.reserve(1000);
  m_ionMarkers.reserve(1000);
  m_attMarkers.reserve(1000);
}

ViewDrift::~ViewDrift() {

  if (!m_hasExternalCanvas && m_canvas) delete m_canvas;
  if (m_view) delete m_view;
  Clear();
}

void ViewDrift::SetCanvas(TCanvas* c) {

  if (c == NULL) return;
  if (!m_hasExternalCanvas && m_canvas) {
    delete m_canvas;
    m_canvas = NULL;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewDrift::SetArea(const double& xmin, const double& ymin, 
                        const double& zmin, 
                        const double& xmax, const double& ymax,
                        const double& zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    std::cout << m_className << "::SetArea:\n";
    std::cout << "    Null area range not permitted.\n";
    return;
  }
  m_xMin = std::min(xmin, xmax);
  m_yMin = std::min(ymin, ymax);
  m_zMin = std::min(zmin, zmax);
  m_xMax = std::max(xmin, xmax);
  m_yMax = std::max(ymin, ymax);
  m_zMax = std::max(zmin, zmax);
}

void ViewDrift::Clear() {

  m_driftLines.clear();
  m_tracks.clear();

  m_excMarkers.clear();
  m_ionMarkers.clear();
  m_attMarkers.clear();

  if (m_excPlot) {
    delete m_excPlot;
    m_excPlot = NULL;
  }
  if (m_ionPlot) {
    delete m_ionPlot;
    m_ionPlot = NULL;
  }
  if (m_attPlot) {
    delete m_attPlot;
    m_attPlot = NULL;
  }
  const unsigned int nTrackPlots = m_trackPlots.size();
  for (unsigned int i = 0; i < nTrackPlots; ++i) {
    if (m_trackPlots[i]) delete m_trackPlots[i];
  }
  const unsigned int nTrackLinePlots = m_trackLinePlots.size();
  for (unsigned int i = 0; i < nTrackLinePlots; ++i) {
    if (m_trackLinePlots[i]) delete m_trackLinePlots[i];
  }
  const unsigned int nDriftLinePlots = m_driftLinePlots.size();
  for (unsigned int i = 0; i < nDriftLinePlots; ++i) {
    if (m_driftLinePlots[i]) delete m_driftLinePlots[i];
  }
}

void ViewDrift::SetClusterMarkerSize(const double& size) {

  if (size > 0.) {
    m_markerSizeCluster = size;
  } else {
    std::cerr << m_className << "::SetClusterMarkerSize:\n";
    std::cerr << "    Size must be positive.\n";
  }
}

void ViewDrift::SetCollisionMarkerSize(const double& size) {

  if (size > 0.) {
    m_markerSizeCollision = size;
  } else {
    std::cerr << m_className << "::SetCollisionMarkerSize:\n";
    std::cerr << "    Size must be positive.\n";
  }
}

void ViewDrift::NewElectronDriftLine(const unsigned int np, int& id,
                                     const double x0, const double y0,
                                     const double z0) {

  const int col = plottingEngine.GetRootColorElectron();
  // Create a new electron drift line and add it to the list.
  driftLine d;
  if (np <= 0) {
    // Number of points is not yet known.
    std::vector<marker> p(1);
    p[0].x = x0;
    p[0].y = y0;
    p[0].z = z0;
    d.vect = p;
  } else {
    std::vector<marker> p(np);
    for (unsigned int i = 0; i < np; ++i) {
      p[i].x = x0;
      p[i].y = y0;
      p[i].z = z0;
    }
    d.vect = p;
  }
  d.n = col;
  m_driftLines.push_back(d);
  // Return the index of this drift line.
  id = m_driftLines.size() - 1;
}

void ViewDrift::NewHoleDriftLine(const unsigned int np, int& id,
                                 const double x0, const double y0,
                                 const double z0) {

  const int col = plottingEngine.GetRootColorHole();
  driftLine d;
  marker m0;
  m0.x = x0;
  m0.y = y0;
  m0.z = z0;
  if (np <= 0) {
    // Number of points is not yet known.
    std::vector<marker> p(1, m0);
    d.vect = p;
  } else {
    std::vector<marker> p(np, m0);
    d.vect = p;
  }
  d.n = col;
  m_driftLines.push_back(d);
  // Return the index of this drift line.
  id = m_driftLines.size() - 1;
}

void ViewDrift::NewIonDriftLine(const unsigned int np, int& id, const double x0,
                                const double y0, const double z0) {

  const int col = 47;
  driftLine d;
  marker m0;
  m0.x = x0;
  m0.y = y0;
  m0.z = z0;
  if (np <= 0) {
    // Number of points is not yet known.
    std::vector<marker> p(1, m0);
    d.vect = p;
  } else {
    std::vector<marker> p(np, m0);
    d.vect = p;
  }
  d.n = col;
  m_driftLines.push_back(d);
  // Return the index of this drift line.
  id = m_driftLines.size() - 1;
}

void ViewDrift::NewPhotonTrack(const double x0, const double y0,
                               const double z0, const double x1,
                               const double y1, const double z1) {

  const int col = plottingEngine.GetRootColorPhoton();
  // Create a new photon track (line between start and end point).
  TPolyLine3D p(2);
  p.SetLineColor(col);
  p.SetLineStyle(7);
  p.SetNextPoint(x0, y0, z0);
  p.SetNextPoint(x1, y1, z1);
}

void ViewDrift::NewChargedParticleTrack(const unsigned int np, int& id,
                                        const double x0, const double y0,
                                        const double z0) {


  // Create a new track and add it to the list.
  track newTrack;
  if (np <= 0) {
    // Number of points is not yet known.
    newTrack.vect.resize(1);
  } else {
    newTrack.vect.resize(np);
  }
  newTrack.vect[0].x = x0;
  newTrack.vect[0].y = y0;
  newTrack.vect[0].z = z0;
  m_tracks.push_back(newTrack);
  // Return the index of this drift line.
  id = m_tracks.size() - 1;
}

void ViewDrift::SetDriftLinePoint(const unsigned int iL, const unsigned int iP,
                                  const double x, const double y,
                                  const double z) {

  if (iL >= m_driftLines.size()) {
    std::cerr << m_className << "::SetDriftLinePoint:\n";
    std::cerr << "    Drift line index " << iL << " is out of range.\n";
    return;
  }
  m_driftLines[iL].vect[iP].x = x;
  m_driftLines[iL].vect[iP].y = y;
  m_driftLines[iL].vect[iP].z = z;
}

void ViewDrift::AddDriftLinePoint(const unsigned int iL, const double x,
                                  const double y, const double z) {

  if (iL >= m_driftLines.size()) {
    std::cerr << m_className << "::AddDriftLinePoint:\n";
    std::cerr << "    Drift line index " << iL << " is out of range.\n";
    return;
  }
  marker m;
  m.x = x;
  m.y = y;
  m.z = z;
  m_driftLines[iL].vect.push_back(m);
}

void ViewDrift::SetTrackPoint(const unsigned int iL, const unsigned int iP,
                              const double x, const double y, const double z) {

  if (iL >= m_tracks.size()) {
    std::cerr << m_className << "::SetTrackPoint:\n";
    std::cerr << "    Track index " << iL << " is out of range.\n";
    return;
  }
  m_tracks[iL].vect[iP].x = x;
  m_tracks[iL].vect[iP].y = y;
  m_tracks[iL].vect[iP].z = z;
}

void ViewDrift::AddTrackPoint(const unsigned int iL, const double x,
                              const double y, const double z) {

  if (iL >= m_tracks.size()) {
    std::cerr << m_className << "::AddTrackPoint:\n";
    std::cerr << "    Track index " << iL << " is out of range.\n";
    return;
  }
  marker newPoint;
  newPoint.x = x;
  newPoint.y = y;
  newPoint.z = z; 
  m_tracks[iL].vect.push_back(newPoint);
}

void ViewDrift::AddExcitationMarker(const double x, const double y,
                                    const double z) {

  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_excMarkers.push_back(newMarker);
}

void ViewDrift::AddIonisationMarker(const double x, const double y,
                                    const double z) {

  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_ionMarkers.push_back(newMarker);
}

void ViewDrift::AddAttachmentMarker(const double x, const double y,
                                    const double z) {

  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_attMarkers.push_back(newMarker);
}

void ViewDrift::Plot(const bool twod, const bool axis) {

  if (twod) {
    Plot2d(axis);
  } else {
    Plot3d(axis);
  }
}

void ViewDrift::Plot2d(const bool axis) {

  std::cout << m_className << "::Plot:\n";
  std::cout << "    Plotting in 2D.\n";
  if (m_canvas == NULL) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle(m_label.c_str());
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();
  m_canvas->Update();

  const unsigned int nDriftLines = m_driftLines.size();
  for (unsigned int i = 0; i < nDriftLines; ++i) {
    const unsigned int nPoints = m_driftLines[i].vect.size();
    TGraph t(nPoints);
    for (unsigned int j = 0; j < nPoints; ++j) {
      t.SetPoint(j, m_driftLines[i].vect[j].x, m_driftLines[i].vect[j].y);
    }
    t.SetLineColor(m_driftLines[i].n);
    t.SetLineWidth(1);
    if (i == 0) {
      t.GetXaxis()->SetLimits(m_xMin, m_xMax);
      t.GetHistogram()->SetMaximum(m_yMax);
      t.GetHistogram()->SetMinimum(m_yMin);
      if (axis) {
        t.DrawClone("ALsame");
      } else {
        t.DrawClone("Lsame");
      }
    } else {
      t.DrawClone("Lsame");
    }
    m_canvas->Update();
  }

  const int trackCol = plottingEngine.GetRootColorChargedParticle();
  const unsigned int nTracks = m_tracks.size();
  for (unsigned int i = 0; i < nTracks; ++i) {
    const unsigned int nPoints = m_tracks[i].vect.size();
    TGraph t(nPoints);
    for (unsigned int j = 0; j < nPoints; ++j) {
      t.SetPoint(j, m_tracks[i].vect[j].x, m_tracks[i].vect[j].y);
    }
    t.SetLineColor(trackCol);
    t.SetLineWidth(2);
    if (i == 0) {
      t.GetXaxis()->SetLimits(m_xMin, m_xMax);
      t.GetHistogram()->SetMaximum(m_yMax);
      t.GetHistogram()->SetMinimum(m_yMin);
      if (axis && m_driftLines.empty()) {
        t.DrawClone("ALsame");
      } else {
        t.DrawClone("Lsame");
      }
    } else {
      t.DrawClone("Lsame");
    }
    m_canvas->Update();
  }

}

void ViewDrift::Plot3d(const bool axis) {

  std::cout << m_className << "::Plot:\n";
  std::cout << "    Plotting in 3D.\n";
  if (m_canvas == NULL) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle(m_label.c_str());
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();
  if (axis) {
    if (m_canvas->GetView() == NULL) {
      if (m_view == NULL) m_view = TView::CreateView(1, 0, 0);
      m_view->SetRange(m_xMin, m_yMin, m_zMin, m_xMax, m_yMax, m_zMax);
      m_view->ShowAxis();
      m_view->Top();
      m_canvas->SetView(m_view);
    }
  }

  const unsigned int nDriftLines = m_driftLines.size();
  for (unsigned int i = 0; i < nDriftLines; ++i) {
    const unsigned int nPoints = m_driftLines[i].vect.size();
    TPolyLine3D* t = new TPolyLine3D(nPoints);
    for (unsigned int j = 0; j < nPoints; ++j) {
      t->SetNextPoint(m_driftLines[i].vect[j].x, m_driftLines[i].vect[j].y,
                      m_driftLines[i].vect[j].z);
    }
    t->SetLineColor(m_driftLines[i].n);
    m_driftLinePlots.push_back(t);
    t->Draw("same");
  }
  const int trackCol = plottingEngine.GetRootColorChargedParticle();
  const unsigned int nTracks = m_tracks.size();
  for (unsigned int i = 0; i < nTracks; ++i) {
    const unsigned int nPoints = m_tracks[i].vect.size();
    TPolyMarker3D* t = new TPolyMarker3D(nPoints);
    TPolyLine3D* l = new TPolyLine3D(nPoints);
    for (unsigned int j = 0; j < nPoints; ++j) {
      t->SetNextPoint(m_tracks[i].vect[j].x, m_tracks[i].vect[j].y,
                      m_tracks[i].vect[j].z);
      l->SetNextPoint(m_tracks[i].vect[j].x, m_tracks[i].vect[j].y,
                      m_tracks[i].vect[j].z);
    }
    t->SetMarkerStyle(20);
    t->SetMarkerColor(trackCol);
    t->SetMarkerSize(m_markerSizeCluster);
    t->Draw("same");
    m_trackPlots.push_back(t);
    l->SetLineColor(trackCol);
    l->SetLineWidth(1);
    l->Draw("same");
    m_trackLinePlots.push_back(l);
  }
  if (m_excPlot) {
    delete m_excPlot;
    m_excPlot = NULL;
  }
  if (!m_excMarkers.empty()) {
    const unsigned int nExcMarkers = m_excMarkers.size();
    m_excPlot = new TPolyMarker3D(nExcMarkers);
    m_excPlot->SetMarkerColor(plottingEngine.GetRootColorLine2());
    m_excPlot->SetMarkerStyle(20);
    m_excPlot->SetMarkerSize(m_markerSizeCollision);
    for (unsigned int i = 0; i < nExcMarkers; ++i) {
      m_excPlot->SetNextPoint(m_excMarkers[i].x, m_excMarkers[i].y,
                              m_excMarkers[i].z);
    }
    m_excPlot->Draw("same");
  }
  if (m_ionPlot) {
    delete m_ionPlot;
    m_ionPlot = NULL;
  }
  if (!m_ionMarkers.empty()) {
    const unsigned int nIonMarkers = m_ionMarkers.size();
    m_ionPlot = new TPolyMarker3D(nIonMarkers);
    m_ionPlot->SetMarkerColor(plottingEngine.GetRootColorIon());
    m_ionPlot->SetMarkerStyle(20);
    m_ionPlot->SetMarkerSize(m_markerSizeCollision);
    for (unsigned int i = 0; i < nIonMarkers; ++i) {
      m_ionPlot->SetNextPoint(m_ionMarkers[i].x, m_ionMarkers[i].y,
                              m_ionMarkers[i].z);
    }
    m_ionPlot->Draw("same");
  }
  if (m_attPlot) {
    delete m_attPlot;
    m_attPlot = NULL;
  }
  if (!m_attMarkers.empty()) {
    const unsigned int nAttMarkers = m_attMarkers.size();
    m_attPlot = new TPolyMarker3D(nAttMarkers);
    m_attPlot->SetMarkerColor(plottingEngine.GetRootColorLine1());
    m_attPlot->SetMarkerStyle(20);
    m_attPlot->SetMarkerSize(m_markerSizeCollision);
    for (unsigned int i = 0; i < nAttMarkers; ++i) {
      m_attPlot->SetNextPoint(m_attMarkers[i].x, m_attMarkers[i].y,
                              m_attMarkers[i].z);
    }
    m_attPlot->Draw("same");
  }
  m_canvas->Update();
}
}
