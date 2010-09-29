#include <iostream>
#include <cmath>

#include <TMarker.h>
#include <TEllipse.h>
#include <TLine.h>
#include <TPolyLine.h>

#include "ComponentAnalyticField.hh"
#include "Plotting.hh"
#include "ViewCell.hh"

namespace Garfield {

ViewCell::ViewCell() :
  debug(false), useWireMarker(true),
  label("Cell Layout"),
  canvas(0), hasExternalCanvas(false),
  hasUserArea(false),
  xMin(-1.), yMin(-1.), zMin(-1.), 
  xMax( 1.), yMax( 1.), zMax( 1.),
  component(0) {

  plottingEngine.SetDefaultStyle();

}

ViewCell::~ViewCell() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;

}

void
ViewCell::SetComponent(ComponentAnalyticField* comp) {

  if (comp == 0) {
    std::cerr << "ViewCell::SetComponent:\n";
    std::cerr << "    Component pointer is null.\n";
    return;
  }

  component = comp;

} 
  
void
ViewCell::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void 
ViewCell::SetArea(double xmin, double ymin, double zmin, 
                   double xmax, double ymax, double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    std::cout << "ViewCell::SetArea:\n";
    std::cout << "    Null area range not permitted.\n";
    return;
  }
  xMin = std::min(xmin, xmax);
  yMin = std::min(ymin, ymax);
  zMin = std::min(zmin, zmax);
  xMax = std::max(xmin, xmax);
  yMax = std::max(ymin, ymax);
  zMax = std::max(zmin, zmax);
  hasUserArea = true;
 
}

void
ViewCell::SetArea() {

  hasUserArea = false;

}

void
ViewCell::Clear() {

}

void
ViewCell::Plot() {

  if (component == 0) {
    std::cerr << "ViewCell::Plot:\n";
    std::cerr << "    Component is not defined.\n";
    return;
  }
  
  double pmin = 0., pmax = 0.;
  if (!component->GetVoltageRange(pmin, pmax)) {
    std::cerr << "ViewCell::Plot:\n";
    std::cerr << "    Component is not ready.\n";
    return;
  }

  // Get the bounding box
  double x0 = xMin, y0 = yMin, z0 = zMin;
  double x1 = xMax, y1 = yMax, z1 = zMax;
  if (!hasUserArea) {
    if (!component->GetBoundingBox(x0, y0, z0, x1, y1, z1)) {
      std::cerr << "ViewCell::Plot:\n";
      std::cerr << "    Bounding box cannot be determined.\n";
      std::cerr << "    Call SetArea first.\n";
      return;
    }
  }

  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle(label.c_str());
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->Range(x0 - 0.1 * (x1 - x0), y0 - 0.1 * (y1 - y0), 
                x1 + 0.1 * (x1 - x0), y1 + 0.1 * (y1 - y0));
  canvas->cd();

  // Get the cell type.
  bool hasTube = false;
  std::string cellType = component->GetCellType();
  if (cellType == "D1" || cellType == "D2" || cellType == "D3") {
    hasTube = true;
  }

  // Get the periodicities.
  double sx = 0., sy = 0.;
  const bool perX = component->GetPeriodicityX(sx);
  const bool perY = component->GetPeriodicityY(sy);
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

  // Get the number of wires.
  int nWires = component->GetNumberOfWires();
  // Loop over the wires.
  for (int i = nWires; i--;) {
    double xw = 0., yw = 0., dw = 0., vw = 0., lw = 0., qw = 0.;
    char label;
    component->GetWire(i, xw, yw, dw, vw, label, lw, qw);
    for (int nx = nMinX; nx <= nMaxX; ++nx) {
      for (int ny = nMinY; ny <= nMaxY; ++ny) {
        double x = xw + nx * sx;
        double y = yw + ny * sy;
        if (x + 0.5 * dw <= x0 ||
            x - 0.5 * dw >= x1 ||
            y + 0.5 * dw <= y0 ||
            y - 0.5 * dw >= y1) {
          continue;
        }
        PlotWire(x, y, dw);
      }
    }
  }

  // Draw lines at the positions of the x planes.
  int nPlanesX = component->GetNumberOfPlanesX();
  for (int i = nPlanesX; i--;) {
    double xp = 0., vp = 0.;
    char label;
    component->GetPlaneX(i, xp, vp, label);
    for (int nx = nMinX; nx <= nMaxX; ++nx) {
      double x = xp + nx * sx;
      if (x <= x0 || x >= x1) continue;
      PlotLine(x, y0, x, y1);
    }
  }
  
  // Draw lines at the positions of the y planes.
  int nPlanesY = component->GetNumberOfPlanesY();
  for (int i = nPlanesY; i--;) {
    double yp = 0., vp = 0.;
    char label;
    component->GetPlaneY(i, yp, vp, label);
    for (int ny = nMinY; ny <= nMaxY; ++ny) {
      double y = yp + ny * sy;
      if (y < y0 || y > y1) continue;
      PlotLine(x0, y, x1, y);
    }
  }

  double rt = 0., vt = 0.;
  int nt = 0;
  char label;
  if (component->GetTube(rt, vt, nt, label)) {
    PlotTube(0., 0., rt, nt);
  }
  
  canvas->Update();

}

void
ViewCell::PlotWire(const double x, const double y, const double d) {

  if (useWireMarker) {
    TMarker* marker = new TMarker(x, y, 4);
    marker->Draw("P");
    return;
  }
  
  TEllipse* circle = new TEllipse(x, y, 0.5 * d);
  circle->Draw("");

}

void
ViewCell::PlotLine(const double x0, const double y0,
                   const double x1, const double y1) {

  TLine* line = new TLine(x0, y0, x1, y1);
  line->Draw("");

}

void
ViewCell::PlotTube(const double x0, const double y0,
                   const double r, const int n) {

  if (n <= 0) {
    TEllipse* circle = new TEllipse(x0, y0, r);
    circle->SetFillStyle(0);
    circle->Draw("");
    return;
  }

  TPolyLine* pline = new TPolyLine(n + 1);
  for (int i = 0; i <= n; ++i) {
    double x = x0 + r * cos(i * TwoPi / double(n));
    double y = y0 + r * sin(i * TwoPi / double(n));
    pline->SetPoint(i, x, y);
  }
  pline->Draw("");
  
}

}
