#include <iostream>
#include <cmath>

#include <TMarker.h>
#include <TEllipse.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TGeoPgon.h>
#include <TGeoBBox.h>

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
      m_geo(NULL) {

  plottingEngine.SetDefaultStyle();
}

ViewCell::~ViewCell() {

  if (!m_hasExternalCanvas && m_canvas) delete m_canvas;
  if (m_geo) delete m_geo;

}

void ViewCell::SetComponent(ComponentAnalyticField* comp) {

  if (!comp) {
    std::cerr << m_className << "::SetComponent: Null pointer.\n";
    return;
  }

  m_component = comp;
}

void ViewCell::SetCanvas(TCanvas* c) {

  if (!c) return;
  if (!m_hasExternalCanvas && m_canvas) {
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
    std::cerr << m_className << "::SetArea: Null area range not permitted.\n";
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

void ViewCell::Plot2d() {

  if (!Plot(false)) {
    std::cerr << m_className << "::Plot2d: Error creating plot.\n";
  }
}

void ViewCell::Plot3d() {

  if (!Plot(true)) {
    std::cerr << m_className << "::Plot3d: Error creating plot.\n";
  }
}

bool ViewCell::Plot(const bool use3d) {

  if (!m_component) {
    std::cerr << m_className << "::Plot: Component is not defined.\n";
    return false;
  }

  double pmin = 0., pmax = 0.;
  if (!m_component->GetVoltageRange(pmin, pmax)) {
    std::cerr << m_className << "::Plot: Component ist not ready.\n";
    return false;
  }

  // Get the bounding box
  double x0 = m_xMin, y0 = m_yMin, z0 = m_zMin;
  double x1 = m_xMax, y1 = m_yMax, z1 = m_zMax;
  if (!m_hasUserArea) {
    if (!m_component->GetBoundingBox(x0, y0, z0, x1, y1, z1)) {
      std::cerr << m_className << "::Plot:\n"
                << "    Bounding box cannot be determined.\n"
                << "    Call SetArea first.\n";
      return false;
    }
  }
  // Get the max. half-length in z.
  const double dx = std::max(fabs(x0), fabs(x1));
  const double dy = std::max(fabs(y0), fabs(y1));
  const double dz = std::max(fabs(z0), fabs(z1));

  if (!m_canvas) {
    m_canvas = new TCanvas();
    if (!use3d) m_canvas->SetTitle(m_label.c_str());
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();

  if (!use3d) {
    bool empty = false;
    if (!gPad || (gPad->GetListOfPrimitives()->GetSize() == 0 &&
                  gPad->GetX1() == 0 && gPad->GetX2() == 1 &&
                  gPad->GetY1() == 0 && gPad->GetY2() == 1)) {
      empty = true;
    }
    const double bm = m_canvas->GetBottomMargin();
    const double lm = m_canvas->GetLeftMargin();
    const double rm = m_canvas->GetRightMargin();
    const double tm = m_canvas->GetTopMargin();
    if (!empty) {
      TPad* pad = new TPad("cell", "", 0, 0, 1, 1);
      pad->SetFillStyle(0);
      pad->SetFrameFillStyle(0);
      pad->Draw();
      pad->cd();
    }
    gPad->Range(x0 - (x1 - x0) * (lm / (1. - rm - lm)),
                y0 - (y1 - y0) * (bm / (1. - tm - lm)),
                x1 + (x1 - x0) * (rm / (1. - rm - lm)),
                y1 + (y1 - y0) * (tm / (1. - tm - lm)));
  }

  // Get the cell type.
  const std::string cellType = m_component->GetCellType();

  // Get the periodicities.
  double sx = 0., sy = 0.;
  const bool perX = m_component->GetPeriodicityX(sx);
  const bool perY = m_component->GetPeriodicityY(sy);
  // Determine the number of periods present in the cell.
  const int nMinX = perX ? int(x0 / sx) - 1 : 0;
  const int nMaxX = perX ? int(x1 / sx) + 1 : 0; 
  const int nMinY = perY ? int(y0 / sy) - 1 : 0;
  const int nMaxY = perY ? int(y1 / sy) + 1 : 0;

  if (use3d) {
    if (!m_geo) {
      m_geo = new TGeoManager("ViewCellGeoManager", m_label.c_str());
      TGeoMaterial* matVacuum = new TGeoMaterial("Vacuum", 0., 0., 0.); 
      TGeoMaterial* matMetal = new TGeoMaterial("Metal", 63.546, 29., 8.92);
      TGeoMedium* medVacuum = new TGeoMedium("Vacuum", 0, matVacuum);
      TGeoMedium* medMetal = new TGeoMedium("Metal", 1, matMetal);
      m_geo->AddMaterial(matVacuum);
      m_geo->AddMaterial(medMetal->GetMaterial());
      TGeoVolume* world = m_geo->MakeBox("World", medVacuum,
                                         1.05 * dx, 1.05 * dy, 1.05 * dz);
      m_geo->SetTopVolume(world);
    } else {
      TGeoVolume* top = m_geo->GetTopVolume();
      TGeoBBox* box = dynamic_cast<TGeoBBox*>(top);
      double halfLenghts[3] = {1.05 * dx, 1.05 * dy, 1.05 * dz};
      if (box) box->SetDimensions(halfLenghts);
    }
  }

  // Get the number of wires.
  const unsigned int nWires = m_component->GetNumberOfWires();
  std::vector<std::string> wireTypes;
  // Loop over the wires.
  for (unsigned int i = 0; i < nWires; ++i) {
    double xw = 0., yw = 0., dw = 0., vw = 0., lw = 0., qw = 0.;
    std::string lbl;
    int type = -1;
    int nTrap;
    m_component->GetWire(i, xw, yw, dw, vw, lbl, lw, qw, nTrap);
    // Check if other wires with the same label already exist.
    if (wireTypes.empty()) {
      wireTypes.push_back(lbl);
      type = 0;
    } else {
      const unsigned int nWireTypes = wireTypes.size();
      for (unsigned int j = 0; j < nWireTypes; ++j) {
        if (lbl == wireTypes[j]) {
          type = j;
          break;
        }
      }
      if (type < 0) {
        type = wireTypes.size();
        wireTypes.push_back(lbl);
      }
    }
    for (int nx = nMinX; nx <= nMaxX; ++nx) {
      const double x = xw + nx * sx;
      if (x + 0.5 * dw <= x0 || x - 0.5 * dw >= x1) continue;
      for (int ny = nMinY; ny <= nMaxY; ++ny) {
        const double y = yw + ny * sy;
        if (y + 0.5 * dw <= y0 || y - 0.5 * dw >= y1) continue;
        if (use3d) {
          TGeoVolume* wire = m_geo->MakeTube("Wire", m_geo->GetMedium("Metal"),
                                             0., 0.5 * dw, 
                                             std::min(0.5 * lw, dz));
          switch (type) {
            case 0:
              wire->SetLineColor(kGray + 2);
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
          m_geo->GetTopVolume()->AddNode(wire, 1, 
                                                new TGeoTranslation(x, y, 0.));
        } else {
          PlotWire(x, y, dw, type);
        }
      }
    }
  }

  // Draw lines at the positions of the x planes.
  const unsigned int nPlanesX = m_component->GetNumberOfPlanesX();
  for (unsigned int i = 0; i < nPlanesX; ++i) {
    double xp = 0., vp = 0.;
    std::string lbl;
    m_component->GetPlaneX(i, xp, vp, lbl);
    for (int nx = nMinX; nx <= nMaxX; ++nx) {
      const double x = xp + nx * sx;
      if (x < x0 || x > x1) continue;
      if (use3d) {
        const double width = std::min(0.01 * dx, 0.01 * dy);
        TGeoVolume* plane = m_geo->MakeBox("PlaneX", m_geo->GetMedium("Metal"),
                                           width, dy, dz);
        plane->SetLineColor(kGreen - 5);
        plane->SetTransparency(75);
        m_geo->GetTopVolume()->AddNode(plane, 1, 
                                       new TGeoTranslation(x, 0., 0.));
      } else {
        TLine line;
        line.SetDrawOption("same");
        line.DrawLine(x, y0, x, y1);
      }
    }
  }

  // Draw lines at the positions of the y planes.
  const unsigned int nPlanesY = m_component->GetNumberOfPlanesY();
  for (unsigned int i = 0; i < nPlanesY; ++i) {
    double yp = 0., vp = 0.;
    std::string lbl;
    m_component->GetPlaneY(i, yp, vp, lbl);
    for (int ny = nMinY; ny <= nMaxY; ++ny) {
      const double y = yp + ny * sy;
      if (y < y0 || y > y1) continue;
      if (use3d) {
        const double width = std::min(0.01 * dx, 0.01 * dy);
        TGeoVolume* plane = m_geo->MakeBox("PlaneY", m_geo->GetMedium("Metal"),
                                           dx, width, dz);
        plane->SetLineColor(kGreen - 5);
        plane->SetTransparency(75);
        m_geo->GetTopVolume()->AddNode(plane, 1, 
                                       new TGeoTranslation(0., y, 0.));
      } else {
        TLine line;
        line.SetDrawOption("same");
        line.DrawLine(x0, y, x1, y);
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
        TGeoVolume* tube = m_geo->MakeTube("Tube", m_geo->GetMedium("Metal"),
                                           0.98 * rt, 1.02 * rt, dz);
        tube->SetLineColor(kGreen + 2);
        m_geo->GetTopVolume()->AddNode(tube, 1, 
                                       new TGeoTranslation(0., 0., 0.));
      } else {
        TGeoVolume* tube = m_geo->MakePgon("Tube", m_geo->GetMedium("Metal"),
                                                  0., 360., nt, 2);
        TGeoPgon* pgon = dynamic_cast<TGeoPgon*>(tube->GetShape());
        pgon->DefineSection(0, -dz, 0.98 * rt, 1.02 * rt);
        pgon->DefineSection(1, +dz, 0.98 * rt, 1.02 * rt);
        tube->SetLineColor(kGreen + 2);
        m_geo->GetTopVolume()->AddNode(tube, 1,  
                                       new TGeoTranslation(0., 0., 0.));
      }
    } else {
      PlotTube(0., 0., rt, nt);
    }
  }

  if (use3d) {
    m_geo->CloseGeometry();
    m_geo->GetTopNode()->Draw("ogl");
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
    TMarker marker;
    marker.SetMarkerStyle(markerStyle);
    marker.SetDrawOption("Psame");
    marker.DrawMarker(x, y);
    return;
  }

  TEllipse circle;
  circle.SetDrawOption("same");
  circle.DrawEllipse(x, y, 0.5 * d, 0.5 * d, 0, 0, 360); 
}

void ViewCell::PlotTube(const double x0, const double y0, const double r,
                        const int n) {

  if (n <= 0) {
    TEllipse circle;
    circle.SetDrawOption("same");
    circle.SetFillStyle(0);
    circle.DrawEllipse(x0, y0, r, r, 0, 0, 360); 
    return;
  }

  double* x = new double[n + 1];
  double* y = new double[n + 1];
  for (int i = 0; i <= n; ++i) {
    const double phi = i * TwoPi / double(n);
    x[i] = x0 + r * cos(phi);
    y[i] = y0 + r * sin(phi);
  }
  TPolyLine pline;
  pline.SetDrawOption("same");
  pline.DrawPolyLine(n + 1, x, y);
  delete[] x;
  delete[] y;
}

}
