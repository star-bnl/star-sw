#include <iostream>

#include <TAxis.h>
#include <TH1F.h>

#include "Plotting.hh"
#include "ViewDrift.hh"

namespace Garfield {

ViewDrift::ViewDrift() {

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
  Clear();
}

void ViewDrift::SetCanvas(TCanvas* c) {

  if (!c) return;
  if (!m_hasExternalCanvas && m_canvas) {
    delete m_canvas;
    m_canvas = nullptr;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewDrift::SetArea(const double xmin, const double ymin, 
                        const double zmin, 
                        const double xmax, const double ymax,
                        const double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    std::cerr << m_className << "::SetArea: Null area range not permitted.\n";
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

  m_excPlot.reset();
  m_ionPlot.reset();
  m_attPlot.reset();

  m_trackPlots.clear();
  m_trackLinePlots.clear();
}

void ViewDrift::SetClusterMarkerSize(const double size) {

  if (size > 0.) {
    m_markerSizeCluster = size;
  } else {
    std::cerr << m_className << "::SetClusterMarkerSize: Size must be > 0.\n";
  }
}

void ViewDrift::SetCollisionMarkerSize(const double size) {

  if (size > 0.) {
    m_markerSizeCollision = size;
  } else {
    std::cerr << m_className << "::SetCollisionMarkerSize: Size must be > 0.\n";
  }
}

void ViewDrift::NewElectronDriftLine(const unsigned int np, int& id,
                                     const double x0, const double y0,
                                     const double z0) {

  // Create a new electron drift line and add it to the list.
  DriftLine d;
  if (np <= 0) {
    // Number of points is not yet known.
    d.points.resize(1);
    d.points.front().x = x0;
    d.points.front().y = y0;
    d.points.front().z = z0;
  } else {
    d.points.resize(np);
    for (unsigned int i = 0; i < np; ++i) {
      d.points[i].x = x0;
      d.points[i].y = y0;
      d.points[i].z = z0;
    }
  }
  d.n = plottingEngine.GetRootColorElectron();
  m_driftLines.push_back(std::move(d));
  // Return the index of this drift line.
  id = m_driftLines.size() - 1;
}

void ViewDrift::NewHoleDriftLine(const unsigned int np, int& id,
                                 const double x0, const double y0,
                                 const double z0) {

  DriftLine d;
  Marker m0;
  m0.x = x0;
  m0.y = y0;
  m0.z = z0;
  if (np <= 0) {
    // Number of points is not yet known.
    d.points.push_back(m0);
  } else {
    d.points.assign(np, m0);
  }
  d.n = plottingEngine.GetRootColorHole();
  m_driftLines.push_back(std::move(d));
  // Return the index of this drift line.
  id = m_driftLines.size() - 1;
}

void ViewDrift::NewIonDriftLine(const unsigned int np, int& id, const double x0,
                                const double y0, const double z0) {

  DriftLine d;
  Marker m0;
  m0.x = x0;
  m0.y = y0;
  m0.z = z0;
  if (np <= 0) {
    // Number of points is not yet known.
    d.points.push_back(m0);
  } else {
    d.points.assign(np, m0);
  }
  d.n = 47;
  m_driftLines.push_back(std::move(d));
  // Return the index of this drift line.
  id = m_driftLines.size() - 1;
}

void ViewDrift::NewPhotonTrack(const double x0, const double y0,
                               const double z0, const double x1,
                               const double y1, const double z1) {

  // Create a new photon track (line between start and end point).
  TPolyLine3D p(2);
  p.SetLineColor(plottingEngine.GetRootColorPhoton());
  p.SetLineStyle(7);
  p.SetNextPoint(x0, y0, z0);
  p.SetNextPoint(x1, y1, z1);
}

void ViewDrift::NewChargedParticleTrack(const unsigned int np, int& id,
                                        const double x0, const double y0,
                                        const double z0) {

  // Create a new track and add it to the list.
  std::vector<Marker> track(std::max(1U, np));
  track[0].x = x0;
  track[0].y = y0;
  track[0].z = z0;
  m_tracks.push_back(std::move(track));
  // Return the index of this track.
  id = m_tracks.size() - 1;
}

void ViewDrift::SetDriftLinePoint(const unsigned int iL, const unsigned int iP,
                                  const double x, const double y,
                                  const double z) {

  if (iL >= m_driftLines.size()) {
    std::cerr << m_className << "::SetDriftLinePoint: Index out of range.\n";
    return;
  }
  m_driftLines[iL].points[iP].x = x;
  m_driftLines[iL].points[iP].y = y;
  m_driftLines[iL].points[iP].z = z;
}

void ViewDrift::AddDriftLinePoint(const unsigned int iL, const double x,
                                  const double y, const double z) {

  if (iL >= m_driftLines.size()) {
    std::cerr << m_className << "::AddDriftLinePoint: Index out of range.\n";
    return;
  }
  Marker m;
  m.x = x;
  m.y = y;
  m.z = z;
  m_driftLines[iL].points.push_back(std::move(m));
}

void ViewDrift::SetTrackPoint(const unsigned int iL, const unsigned int iP,
                              const double x, const double y, const double z) {

  if (iL >= m_tracks.size()) {
    std::cerr << m_className << "::SetTrackPoint: Index out of range.\n";
    return;
  }
  m_tracks[iL][iP].x = x;
  m_tracks[iL][iP].y = y;
  m_tracks[iL][iP].z = z;
}

void ViewDrift::AddTrackPoint(const unsigned int iL, const double x,
                              const double y, const double z) {

  if (iL >= m_tracks.size()) {
    std::cerr << m_className << "::AddTrackPoint: Index out of range.\n";
    return;
  }
  Marker newPoint;
  newPoint.x = x;
  newPoint.y = y;
  newPoint.z = z; 
  m_tracks[iL].push_back(std::move(newPoint));
}

void ViewDrift::AddExcitationMarker(const double x, const double y,
                                    const double z) {

  Marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_excMarkers.push_back(std::move(newMarker));
}

void ViewDrift::AddIonisationMarker(const double x, const double y,
                                    const double z) {

  Marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_ionMarkers.push_back(std::move(newMarker));
}

void ViewDrift::AddAttachmentMarker(const double x, const double y,
                                    const double z) {

  Marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_attMarkers.push_back(std::move(newMarker));
}

void ViewDrift::Plot(const bool twod, const bool axis) {

  if (twod) {
    Plot2d(axis);
  } else {
    Plot3d(axis);
  }
}

void ViewDrift::Plot2d(const bool axis) {

  std::cout << m_className << "::Plot: Plotting in 2D.\n";
  if (!m_canvas) {
    m_canvas = new TCanvas();
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();
  m_canvas->Update();

  for (const auto& driftLine : m_driftLines) {
    const auto nPoints = driftLine.points.size();
    TGraph t(nPoints);
    for (unsigned int j = 0; j < nPoints; ++j) {
      t.SetPoint(j, driftLine.points[j].x, driftLine.points[j].y);
    }
    t.SetLineColor(driftLine.n);
    t.SetLineWidth(1);
    if (&driftLine == &m_driftLines.front()) {
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

  const auto trackCol = plottingEngine.GetRootColorChargedParticle();
  for (const auto& track : m_tracks) {
    const auto nPoints = track.size();
    TGraph t(nPoints);
    for (unsigned int j = 0; j < nPoints; ++j) {
      t.SetPoint(j, track[j].x, track[j].y);
    }
    t.SetLineColor(trackCol);
    t.SetLineWidth(2);
    if (&track == &m_tracks.front()) {
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

  std::cout << m_className << "::Plot: Plotting in 3D.\n";
  if (!m_canvas) {
    m_canvas = new TCanvas();
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();
  if (axis) {
    if (!m_canvas->GetView()) {
      m_view.reset(TView::CreateView(1, 0, 0));
      m_view->SetRange(m_xMin, m_yMin, m_zMin, m_xMax, m_yMax, m_zMax);
      m_view->ShowAxis();
      m_view->Top();
      m_canvas->SetView(m_view.get());
    }
  }

  for (const auto& driftLine : m_driftLines) {
    TPolyLine3D t(driftLine.points.size());
    for (const auto& point : driftLine.points) {
      t.SetNextPoint(point.x, point.y, point.z);
    }
    t.SetLineColor(driftLine.n);
    m_driftLinePlots.push_back(std::move(t));
    m_driftLinePlots.back().Draw("same");
  }

  const int trackCol = plottingEngine.GetRootColorChargedParticle();
  for (const auto& track : m_tracks) {
    const unsigned int nPoints = track.size();
    TPolyMarker3D t(nPoints);
    TPolyLine3D l(nPoints);
    for (const auto& point : track) {
      t.SetNextPoint(point.x, point.y, point.z);
      l.SetNextPoint(point.x, point.y, point.z);
    }
    t.SetMarkerStyle(20);
    t.SetMarkerColor(trackCol);
    t.SetMarkerSize(m_markerSizeCluster);
    t.Draw("same");
    m_trackPlots.push_back(std::move(t));
    l.SetLineColor(trackCol);
    l.SetLineWidth(1);
    l.Draw("same");
    m_trackLinePlots.push_back(std::move(l));
  }

  if (!m_excMarkers.empty()) {
    m_excPlot.reset(new TPolyMarker3D(m_excMarkers.size()));
    m_excPlot->SetMarkerColor(plottingEngine.GetRootColorLine2());
    m_excPlot->SetMarkerStyle(20);
    m_excPlot->SetMarkerSize(m_markerSizeCollision);
    for (const auto& point : m_excMarkers) {
      m_excPlot->SetNextPoint(point.x, point.y, point.z);
    }
    m_excPlot->Draw("same");
  } else {
    m_excPlot.reset(nullptr);
  }

  if (!m_ionMarkers.empty()) {
    m_ionPlot.reset(new TPolyMarker3D(m_ionMarkers.size()));
    m_ionPlot->SetMarkerColor(plottingEngine.GetRootColorIon());
    m_ionPlot->SetMarkerStyle(20);
    m_ionPlot->SetMarkerSize(m_markerSizeCollision);
    for (const auto& point : m_ionMarkers) {
      m_ionPlot->SetNextPoint(point.x, point.y, point.z);
    }
    m_ionPlot->Draw("same");
  } else {
    m_ionPlot.reset(nullptr);
  }

  if (!m_attMarkers.empty()) {
    m_attPlot.reset(new TPolyMarker3D(m_attMarkers.size()));
    m_attPlot->SetMarkerColor(plottingEngine.GetRootColorLine1());
    m_attPlot->SetMarkerStyle(20);
    m_attPlot->SetMarkerSize(m_markerSizeCollision);
    for (const auto& point : m_attMarkers) {
      m_attPlot->SetNextPoint(point.x, point.y, point.z);
    }
    m_attPlot->Draw("same");
  } else {
    m_attPlot.reset(nullptr);
  }
  m_canvas->Update();
}
}
