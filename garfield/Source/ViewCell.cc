#include <iostream>
#include <cmath>

#include <TMarker.h>
#include <TEllipse.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TGeoPgon.h>

#include "ComponentAnalyticField.hh"
#include "Plotting.hh"
#include "ViewCell.hh"

namespace Garfield {

ViewCell::ViewCell()
    : m_className("ViewCell"),
      m_debug(false),
      m_useWireMarker(true),
      m_label("Cell Layout"),
      m_canvas(NULL),
      m_hasExternalCanvas(false),
      m_hasUserArea(false),
      m_xMin(-1.),
      m_yMin(-1.),
      m_zMin(-1.),
      m_xMax(1.),
      m_yMax(1.),
      m_zMax(1.),
      m_component(NULL),
      m_geoManager(NULL) {

  plottingEngine.SetDefaultStyle();
}

ViewCell::~ViewCell() {

  if (!m_hasExternalCanvas && m_canvas != NULL) delete m_canvas;
  Reset();

}

void ViewCell::SetComponent(ComponentAnalyticField* comp) {

  if (comp == NULL) {
    std::cerr << m_className << "::SetComponent:\n";
    std::cerr << "    Component pointer is null.\n";
    return;
  }

  m_component = comp;
}

void ViewCell::SetCanvas(TCanvas* c) {

  if (c == NULL) return;
  if (!m_hasExternalCanvas && m_canvas != NULL) {
    delete m_canvas;
    m_canvas = NULL;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewCell::SetArea(const double xmin, const double ymin, 
                       const double zmin, 
                       const double xmax, const double ymax, 
                       const double zmax) {

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
  m_hasUserArea = true;
}

void ViewCell::SetArea() { m_hasUserArea = false; }

void ViewCell::Plot2d() {

  if (!Plot(false)) {
    std::cerr << m_className << "::Plot2d:\n";
    std::cerr << "    Error creating 2d plot.\n";
  }
}

void ViewCell::Plot3d() {

  if (!Plot(true)) {
    std::cerr << m_className << "::Plot3d:\n";
    std::cerr << "    Error creating 3d plot.\n";
  }
}

bool ViewCell::Plot(const bool use3d) {

  if (m_component == NULL) {
    std::cerr << m_className << "::Plot:\n";
    std::cerr << "    Component is not defined.\n";
    return false;
  }

  double pmin = 0., pmax = 0.;
  if (!m_component->GetVoltageRange(pmin, pmax)) {
    std::cerr << m_className << "::Plot:\n";
    std::cerr << "    Component is not ready.\n";
    return false;
  }

  // Get the bounding box
  double x0 = m_xMin, y0 = m_yMin, z0 = m_zMin;
  double x1 = m_xMax, y1 = m_yMax, z1 = m_zMax;
  if (!m_hasUserArea) {
    if (!m_component->GetBoundingBox(x0, y0, z0, x1, y1, z1)) {
      std::cerr << m_className << "::Plot:\n";
      std::cerr << "    Bounding box cannot be determined.\n";
      std::cerr << "    Call SetArea first.\n";
      return false;
    }
  }
  // Get the max. half-length in z.
  const double dx = std::max(fabs(x0), fabs(x1));
  const double dy = std::max(fabs(y0), fabs(y1));
  const double dz = std::max(fabs(z0), fabs(z1));

  if (m_canvas == NULL) {
    m_canvas = new TCanvas();
    if (!use3d) m_canvas->SetTitle(m_label.c_str());
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  if (!use3d) {
    m_canvas->Range(x0 - 0.1 * (x1 - x0), y0 - 0.1 * (y1 - y0),
                    x1 + 0.1 * (x1 - x0), y1 + 0.1 * (y1 - y0));
  }
  m_canvas->cd();

  // Get the cell type.
  const std::string cellType = m_component->GetCellType();

  // Get the periodicities.
  double sx = 0., sy = 0.;
  const bool perX = m_component->GetPeriodicityX(sx);
  const bool perY = m_component->GetPeriodicityY(sy);
  // Determine the number of periods present in the cell.
  int nMaxX = 0, nMinX = 0;
  int nMaxY = 0, nMinY = 0;
  if (perX) {
    nMinX = int(x0 / sx) - 1;
    nMaxX = int(x1 / sx) + 1;
  }
  if (perY) {
    nMinY = int(y0 / sy) - 1;
    nMaxY = int(y1 / sy) + 1;
  }

  if (use3d) {
    Reset();
    m_geoManager = new TGeoManager("ViewCellGeoManager", m_label.c_str());
    TGeoMaterial* matVacuum = new TGeoMaterial("Vacuum", 0., 0., 0.); 
    TGeoMaterial* matMetal = new TGeoMaterial("Metal", 63.546, 29., 8.92);
    TGeoMedium* medVacuum = new TGeoMedium("Vacuum", 1, matVacuum);
    TGeoMedium* medMetal = new TGeoMedium("Metal", 1, matMetal);
    m_media.push_back(medVacuum);
    m_media.push_back(medMetal);
    TGeoVolume* world = m_geoManager->MakeBox("World", medVacuum, 
                                              1.05 * dx, 1.05 * dy, 1.05 * dz);
    m_geoManager->SetTopVolume(world);
    m_volumes.push_back(world);
  }
  // Get the number of wires.
  const int nWires = m_component->GetNumberOfWires();
  int nWireTypes = 0;
  std::vector<std::string> wireTypes;
  wireTypes.clear();
  // Loop over the wires.
  for (int i = nWires; i--;) {
    double xw = 0., yw = 0., dw = 0., vw = 0., lw = 0., qw = 0.;
    std::string lbl;
    int type = -1;
    int nTrap;
    m_component->GetWire(i, xw, yw, dw, vw, lbl, lw, qw, nTrap);
    // Check if other wires with the same label already exist.
    if (nWireTypes == 0) {
      wireTypes.push_back(lbl);
      type = 0;
      ++nWireTypes;
    } else {
      for (int j = nWireTypes; j--;) {
        if (lbl == wireTypes[j]) {
          type = j;
          break;
        }
      }
      if (type < 0) {
        wireTypes.push_back(lbl);
        type = nWireTypes;
        ++nWireTypes;
      }
    }
    for (int nx = nMinX; nx <= nMaxX; ++nx) {
      for (int ny = nMinY; ny <= nMaxY; ++ny) {
        const double x = xw + nx * sx;
        const double y = yw + ny * sy;
        if (x + 0.5 * dw <= x0 || x - 0.5 * dw >= x1 || y + 0.5 * dw <= y0 ||
            y - 0.5 * dw >= y1) {
          continue;
        }
        if (use3d) {
          TGeoVolume* wire = m_geoManager->MakeTube("Wire", m_media[1], 
                                                    0., 0.5 * dw, 
                                                    std::min(0.5 * lw, dz));
          switch (type) {
            case 0:
              wire->SetLineColor(kBlue);
              break;
            case 1:
              wire->SetLineColor(kRed + 2);
              break;
            case 2:
              wire->SetLineColor(kPink + 3);
              break;
            case 3: 
              wire->SetLineColor(kCyan + 3);
              break;
            default: 
              wire->SetLineColor(kBlue + type);
              break;
          }
          m_volumes.push_back(wire);
          m_geoManager->GetTopVolume()->AddNode(wire, 1, 
                                                new TGeoTranslation(x, y, 0.));
        } else {
          PlotWire(x, y, dw, type);
        }
      }
    }
  }

  // Draw lines at the positions of the x planes.
  const int nPlanesX = m_component->GetNumberOfPlanesX();
  for (int i = nPlanesX; i--;) {
    double xp = 0., vp = 0.;
    std::string lbl;
    m_component->GetPlaneX(i, xp, vp, lbl);
    for (int nx = nMinX; nx <= nMaxX; ++nx) {
      const double x = xp + nx * sx;
      if (x < x0 || x > x1) continue;
      if (use3d) {
        const double width = 0.01;
        TGeoVolume* plane = m_geoManager->MakeBox("PlaneX", m_media[1], 
                                                  width, dy, dz);
        plane->SetLineColor(kGreen + 2);
        m_volumes.push_back(plane);
        m_geoManager->GetTopVolume()->AddNode(plane, 1, 
                                              new TGeoTranslation(x, 0., 0.));
      } else {
        PlotLine(x, y0, x, y1);
      }
    }
  }

  // Draw lines at the positions of the y planes.
  const int nPlanesY = m_component->GetNumberOfPlanesY();
  for (int i = nPlanesY; i--;) {
    double yp = 0., vp = 0.;
    std::string lbl;
    m_component->GetPlaneY(i, yp, vp, lbl);
    for (int ny = nMinY; ny <= nMaxY; ++ny) {
      const double y = yp + ny * sy;
      if (y < y0 || y > y1) continue;
      if (use3d) {
        const double width = 0.01;
        TGeoVolume* plane = m_geoManager->MakeBox("PlaneY", m_media[1], 
                                                  dx, width, dz);
        plane->SetLineColor(kGreen + 2);
        m_volumes.push_back(plane);
        m_geoManager->GetTopVolume()->AddNode(plane, 1, 
                                              new TGeoTranslation(0., y, 0.));
      } else {
        PlotLine(x0, y, x1, y);
      }
    }
  }

  double rt = 0., vt = 0.;
  int nt = 0;
  std::string lbl;
  if (m_component->GetTube(rt, vt, nt, lbl)) {
    if (use3d) {
      if (nt <= 0) {
        // Round tube
        TGeoVolume* tube = m_geoManager->MakeTube("Tube", m_media[1], 
                                                  0.98 * rt, 1.02 * rt, dz);
        tube->SetLineColor(kGreen + 2);
        m_volumes.push_back(tube);
        m_geoManager->GetTopVolume()->AddNode(tube, 1, 
                                              new TGeoTranslation(0., 0., 0.));
      } else {
        TGeoVolume* tube = m_geoManager->MakePgon("Tube", m_media[1],
                                                  0., 360., nt, 2);
        TGeoPgon* pgon = dynamic_cast<TGeoPgon*>(tube->GetShape());
        pgon->DefineSection(0, -dz, 0.98 * rt, 1.02 * rt);
        pgon->DefineSection(1, +dz, 0.98 * rt, 1.02 * rt);
        tube->SetLineColor(kGreen + 2);
        m_volumes.push_back(tube);
        m_geoManager->GetTopVolume()->AddNode(tube, 1, 
                                              new TGeoTranslation(0., 0., 0.));
      }
    } else {
      PlotTube(0., 0., rt, nt);
    }
  }

  if (use3d) {
    m_geoManager->CloseGeometry();
    m_geoManager->GetTopNode()->Draw("ogl");
  } else {
    m_canvas->Update();
  }

  return true;
}

void ViewCell::PlotWire(const double x, const double y, const double d,
                        const int type) {

  if (m_useWireMarker) {
    int markerStyle = 1;
    if (type == 0) {
      markerStyle = kFullCircle;
    } else if (type == 1) {
      markerStyle = kOpenCircle;
    } else if (type == 2) {
      markerStyle = kFullSquare;
    } else if (type == 3) {
      markerStyle = kOpenSquare;
    } else {
      markerStyle = 26 + type;
    }
    TMarker* marker = new TMarker(x, y, markerStyle);
    marker->Draw("P");
    return;
  }

  TEllipse* circle = new TEllipse(x, y, 0.5 * d);
  circle->Draw("");
}

void ViewCell::PlotLine(const double x0, const double y0, 
                        const double x1, const double y1) {

  TLine* line = new TLine(x0, y0, x1, y1);
  line->Draw("");
}

void ViewCell::PlotTube(const double x0, const double y0, const double r,
                        const int n) {

  if (n <= 0) {
    TEllipse* circle = new TEllipse(x0, y0, r);
    circle->SetFillStyle(0);
    circle->Draw("");
    return;
  }

  TPolyLine* pline = new TPolyLine(n + 1);
  for (int i = 0; i <= n; ++i) {
    const double x = x0 + r * cos(i * TwoPi / double(n));
    const double y = y0 + r * sin(i * TwoPi / double(n));
    pline->SetPoint(i, x, y);
  }
  pline->Draw("");
}

void ViewCell::Reset() {

 for (std::vector<TGeoVolume*>::iterator it = m_volumes.begin();
      it != m_volumes.end(); ++it) {
   if (*it) {
     TGeoShape* shape = (*it)->GetShape();
     if (shape) delete shape;
     delete *it;
   }
 }
 m_volumes.clear();
 for (std::vector<TGeoMedium*>::iterator it = m_media.begin();
      it != m_media.end(); ++it) {
   if (*it) {
     TGeoMaterial* material = (*it)->GetMaterial();
     if (material) delete material;
     delete *it;
   }
 }
 m_media.clear();

 if (m_geoManager) {
   delete m_geoManager;
   m_geoManager = NULL;
 }

}

}
