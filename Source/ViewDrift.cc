#include <iostream>
#include <RQ_OBJECT.h>
#include "Plotting.hh"
#include "ViewDrift.hh"
#include "TMultiGraph.h"
#include "TH1F.h"
#include "TAxis.h"

namespace Garfield {

ViewDrift::ViewDrift() :
  m_className("ViewDrift"), m_debug(false),
  m_label("Drift Lines"),
  m_canvas(0), m_hasExternalCanvas(false),
  m_xMin(-1.), m_yMin(-1.), m_zMin(-1.), 
  m_xMax( 1.), m_yMax( 1.), m_zMax( 1.),
  m_view(0),
  m_nDriftLines(0), m_nTracks(0),
  m_nExcMarkers(0), m_excPlot(0), 
  m_nIonMarkers(0), m_ionPlot(0), 
  m_nAttMarkers(0), m_attPlot(0),
  m_markerSizeCluster(1.), m_markerSizeCollision(1.) {

  m_driftLines.clear();
  m_tracks.clear();
  m_excMarkers.clear();
  m_ionMarkers.clear();
  m_attMarkers.clear();

}

ViewDrift::~ViewDrift() {

  if (!m_hasExternalCanvas && m_canvas != 0) delete m_canvas;
  if (m_view != 0) delete m_view;
  delete m_excPlot; m_excPlot = 0;
  delete m_ionPlot; m_ionPlot = 0;
  delete m_attPlot; m_attPlot = 0;

}

void
ViewDrift::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!m_hasExternalCanvas && m_canvas != 0) {
    delete m_canvas;
    m_canvas = 0;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;

}

void 
ViewDrift::SetArea(double xmin, double ymin, double zmin, 
                   double xmax, double ymax, double zmax) {

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

void
ViewDrift::Clear() {

  m_driftLines.clear();
  m_nDriftLines = 0;

  m_tracks.clear();
  m_nTracks = 0;

  m_nExcMarkers = m_nIonMarkers = m_nAttMarkers = 0;
  m_excMarkers.clear();
  m_ionMarkers.clear();
  m_attMarkers.clear();

  delete m_excPlot; m_excPlot = 0;
  delete m_ionPlot; m_ionPlot = 0;
  delete m_attPlot; m_attPlot = 0;

}

void
ViewDrift::SetClusterMarkerSize(const double size) {

  if (size > 0.) {
    m_markerSizeCluster = size;
  } else {
    std::cerr << m_className << "::SetClusterMarkerSize:\n";
    std::cerr << "    Size must be positive.\n";
  }

}

void
ViewDrift::SetCollisionMarkerSize(const double size) {

  if (size > 0.) {
    m_markerSizeCollision = size;
  } else {
    std::cerr << m_className << "::SetCollisionMarkerSize:\n";
    std::cerr << "    Size must be positive.\n";
  }

}
  
void
ViewDrift::NewElectronDriftLine(const unsigned int np, int& id,
                      const double x0, const double y0, const double z0) {

  int col = plottingEngine.GetRootColorElectron();
   // Create a new electron drift line and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    driftLine d;
    std::vector<marker> p(1); 
    p[0].x =x0;
    p[0].y =y0;
    p[0].z =z0;
    d.vect=p;
    d.n=col;
    m_driftLines.push_back(d);
    std::cout<<"Error: Number of points not specified for plotting"<<std::endl;
  } else {
    std::vector<marker> p(np);
    for (unsigned int i = 0; i < np; ++i) {
      p[i].x = x0;
      p[i].y = y0;
      p[i].z = z0;
    }
    driftLine d;
    d.vect=p;
    d.n=col;
    m_driftLines.push_back(d);
    //p.SetLineColor(col);
    //p.SetPoint(0,x0,y0,z0);
    //m_driftLines.push_back(p);
  }
  // Return the index of this drift line.
  id = m_nDriftLines;
  ++m_nDriftLines;

} 

void
ViewDrift::NewHoleDriftLine(const unsigned int np, int& id,
                      const double x0, const double y0, const double z0) {

  int col = plottingEngine.GetRootColorHole(); 
  if (np <= 0) {
    // Number of points is not yet known.
    driftLine d;
    std::vector<marker> p(1); 
    p[0].x =x0;
    p[0].y =y0;
    p[0].z =z0;
    d.vect=p;
    d.n=col;
    m_driftLines.push_back(d);
    std::cout<<"Error: Number of points not specified for plotting"<<std::endl;
  } else {
    std::vector<marker> p(np);
    for (unsigned int i = 0; i < np; ++i) {
      p[i].x = x0;
      p[i].y = y0;
      p[i].z = z0;
    }
    driftLine d;
    d.vect=p;
    d.n=col;
    m_driftLines.push_back(d);
  }
  // Return the index of this drift line.
  id = m_nDriftLines;
  ++m_nDriftLines;

}

void
ViewDrift::NewIonDriftLine(const unsigned int np, int& id,
                 const double x0, const double y0, const double z0) {

  int col = 47;
  if (np <= 0) {
    // Number of points is not yet known.
    driftLine d;
    std::vector<marker> p(1); 
    p[0].x =x0;
    p[0].y =y0;
    p[0].z =z0;
    d.vect=p;
    d.n=col;
    m_driftLines.push_back(d);
    std::cout<<"Error: Number of points not specified for plotting"<<std::endl;
  } else {
    std::vector<marker> p(np);
    for (unsigned int i = 0; i < np; ++i) {
      p[i].x = x0;
      p[i].y = y0;
      p[i].z = z0;
    }
    driftLine d;
    d.vect=p;
    d.n=col;
    m_driftLines.push_back(d);
  }
  // Return the index of this drift line.
  id = m_nDriftLines;
  ++m_nDriftLines;
}

void
ViewDrift::NewPhotonTrack(const double x0, const double y0, const double z0,
                          const double x1, const double y1, const double z1) {

  int col = plottingEngine.GetRootColorPhoton();
  // Create a new photon track (line between start and end point).
  TPolyLine3D p(2);
  p.SetLineColor(col);
  p.SetLineStyle(7);
  p.SetNextPoint(x0, y0, z0);
  p.SetNextPoint(x1, y1, z1);
  //m_driftLines.push_back(p);
  ++m_nDriftLines;

}

void
ViewDrift::NewChargedParticleTrack(const unsigned int np, int& id,
                     const double x0, const double y0, const double z0) {

  int col = plottingEngine.GetRootColorChargedParticle();
  // Create a new track and add it to the list.
  if (np <= 0) {
    // Number of points is not yet known.
    TPointSet3D p(1);
    p.SetMarkerColor(col);
    p.SetMarkerStyle(20);
    p.SetMarkerSize(m_markerSizeCluster);
    p.SetPoint(0, x0, y0, z0);
    m_tracks.push_back(p);
  } else {
    TPointSet3D p(np);
    p.SetMarkerColor(col);
    p.SetMarkerStyle(20);
    p.SetMarkerSize(m_markerSizeCluster);
    p.SetPoint(0, x0, y0, z0);
    m_tracks.push_back(p);
  }
  // Return the index of this drift line.
  id = m_nTracks;
  ++m_nTracks;

}

void
ViewDrift::SetDriftLinePoint(const unsigned int iL, const unsigned int iP, 
                             const double x, const double y, const double z) {
  
  if (iL >= m_nDriftLines) {
    std::cerr << m_className << "::SetDriftLinePoint:\n";
    std::cerr << "    Drift line index " << iL << " is out of range.\n";
    return;
  }
  m_driftLines[iL].vect[iP].x=x;
  m_driftLines[iL].vect[iP].y=y;
  m_driftLines[iL].vect[iP].z=z;
}

void
ViewDrift::AddDriftLinePoint(const unsigned int iL,
                             const double x, const double y, const double z) {

  if (iL >= m_nDriftLines) {
    std::cerr << m_className << "::AddDriftLinePoint:\n";
    std::cerr << "    Drift line index " << iL << " is out of range.\n";
    return;
  }
  marker m;
  m.x=x;
  m.y=y;
  m.z=z;

  m_driftLines[iL].vect.push_back(m);

}

void
ViewDrift::SetTrackPoint(const unsigned int iL, const unsigned int iP, 
                         const double x, const double y, const double z) {
  
  if (iL >= m_nTracks) {
    std::cerr << m_className << "::SetTrackPoint:\n";
    std::cerr << "    Track index " << iL << " is out of range.\n";
    return;
  }
  m_tracks[iL].SetPoint(iP, x, y, z);

}

void
ViewDrift::AddTrackPoint(const unsigned int iL,
                         const double x, const double y, const double z) {

  if (iL >= m_nTracks) {
    std::cerr << m_className << "::AddTrackPoint:\n";
    std::cerr << "    Track index " << iL << " is out of range.\n";
    return;
  }
  m_tracks[iL].SetNextPoint(x, y, z);

}

void
ViewDrift::AddExcitationMarker(
    const double x, const double y, const double z) {

  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_excMarkers.push_back(newMarker);
  ++m_nExcMarkers;

}

void
ViewDrift::AddIonisationMarker(
    const double x, const double y, const double z) {
  
  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_ionMarkers.push_back(newMarker);
  ++m_nIonMarkers;

}

void
ViewDrift::AddAttachmentMarker(
    const double x, const double y, const double z) {

  marker newMarker;
  newMarker.x = x;
  newMarker.y = y;
  newMarker.z = z;
  m_attMarkers.push_back(newMarker);
  ++m_nAttMarkers;

}

void 
ViewDrift::Plot(const bool twod, const bool axis) {

  if (twod) {
    Plot2d(axis);
  } else {
    Plot3d(axis);
  }

}

void
ViewDrift::Plot2d(const bool axis) {

  std::cout<<"Plotting in 2D..."<<std::endl;  
  if (m_canvas == 0) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle(m_label.c_str());
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();
  m_canvas->Update();

  for (unsigned int i = 0; i < m_nDriftLines; ++i) {
    TGraph t(m_driftLines[i].vect.size());
    for(int j = m_driftLines[i].vect.size(); j--;){
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

}

void
ViewDrift::Plot3d(const bool axis) {

  std::cout<<"Plotting in 3D..."<<std::endl;
  
  if (m_canvas == 0) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle(m_label.c_str());
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();

  if (axis) {
    if (m_canvas->GetView() == 0) {
      if (m_view == 0) m_view = TView::CreateView(1, 0, 0);
      m_view->SetRange(m_xMin, m_yMin, m_zMin, m_xMax, m_yMax, m_zMax);
      m_view->ShowAxis();
      m_view->Top();
      m_canvas->SetView(m_view);
    }
  }
  for (unsigned int i = 0; i < m_nDriftLines; ++i) {
    TPointSet3D t(m_driftLines[i].vect.size());
    for(int j = m_driftLines[i].vect.size(); j--;){
      t.SetPoint(j, m_driftLines[i].vect[j].x, m_driftLines[i].vect[j].y,m_driftLines[i].vect[j].z);
    }
      t.SetMarkerColor(m_driftLines[i].n);
    if (i == 0) {
      t.DrawClone("Lsame");
    } else {
      t.DrawClone("Lsame");
    }
  }
  m_canvas->Update();

  for (int i = m_nTracks; i--;) {
    m_tracks[i].Draw("Lsame");
  }

  if (m_excPlot != 0) {
    delete m_excPlot; m_excPlot = 0;
  }
  m_excPlot = new TPointSet3D(m_nExcMarkers);
  m_excPlot->SetMarkerColor(plottingEngine.GetRootColorLine2());
  m_excPlot->SetMarkerStyle(20);
  m_excPlot->SetMarkerSize(m_markerSizeCollision);
  for (unsigned int i = 0; i < m_nExcMarkers; ++i) {
    m_excPlot->SetNextPoint(
                      m_excMarkers[i].x, m_excMarkers[i].y, m_excMarkers[i].z);
  }
  if (m_ionPlot != 0) {
    delete m_ionPlot; m_ionPlot = 0;
  }
  m_ionPlot = new TPointSet3D(m_nIonMarkers);
  m_ionPlot->SetMarkerColor(plottingEngine.GetRootColorIon());
  m_ionPlot->SetMarkerStyle(20);
  m_ionPlot->SetMarkerSize(m_markerSizeCollision);
  for (unsigned int i = 0; i < m_nIonMarkers; ++i) {
    m_ionPlot->SetNextPoint(
                      m_ionMarkers[i].x, m_ionMarkers[i].y, m_ionMarkers[i].z);
  }
  if (m_attPlot != 0) {
    delete m_attPlot; m_attPlot = 0;
  }
  m_attPlot = new TPointSet3D(m_nAttMarkers);
  m_attPlot->SetMarkerColor(plottingEngine.GetRootColorLine1());
  m_attPlot->SetMarkerStyle(20);
  m_attPlot->SetMarkerSize(m_markerSizeCollision);
  for (unsigned int i = 0; i < m_nAttMarkers; ++i) {
    m_attPlot->SetNextPoint(
                      m_attMarkers[i].x, m_attMarkers[i].y, m_attMarkers[i].z);
  }
  m_excPlot->Draw("same");
  m_ionPlot->Draw("same");
  m_attPlot->Draw("same");
  m_canvas->Update();

}

}
