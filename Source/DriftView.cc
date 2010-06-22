#include <iostream>
#include <string>

#include "DriftView.hh"

namespace Garfield {

DriftView::DriftView() :
  debug(false),
  label("Drift View"),
  canvas(0), hasExternalCanvas(false),
  xMin(-1.), yMin(-1.), zMin(-1.), 
  xMax( 1.), yMax( 1.), zMax( 1.),
  view(0),
  nDriftLines(0) {

}

DriftView::~DriftView() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;

}

void
DriftView::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void 
DriftView::SetArea(double xmin, double ymin, double zmin, 
                   double xmax, double ymax, double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    std::cout << "DriftView::SetArea:" << std::endl;
    std::cout << "    Null area range not permitted." << std::endl;
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
DriftView::Clear() {

  driftLines.clear();
  nDriftLines = 0;

}

void
DriftView::NewElectronDriftLine(const int n) {

  if (n <= 0) {
    std::cerr << "DriftView::NewElectronDriftLine:" << std::endl;
    std::cerr << "    Drift line size must be greater than zero." << std::endl;
    return;
  }
  TPolyLine3D p(n);
  p.SetLineColor(kOrange);
  driftLines.push_back(p);
  ++nDriftLines;

} 

void
DriftView::NewIonDriftLine(const int n) {

  if (n <= 0) {
    std::cerr << "DriftView::NewIonDriftLine:" << std::endl;
    std::cerr << "    Drift line size must be greater than zero." << std::endl;
    return;
  }
  TPolyLine3D p(n);
  p.SetLineColor(kRed);
  driftLines.push_back(p);
  ++nDriftLines;

}

void
DriftView::NewPhotonTrack(const double x0, const double y0, const double z0,
                          const double x1, const double y1, const double z1) {

  TPolyLine3D p(2);
  p.SetLineColor(kBlue);
  p.SetLineStyle(7);
  p.SetPoint(0, x0, y0, z0);
  p.SetPoint(1, x1, y1, z1);
  driftLines.push_back(p);
  ++nDriftLines;

}

void
DriftView::SetPoint(const int i, 
                    const double x, const double y, const double z) {
  
  if (i < 0) return;
  if (nDriftLines <= 0) {
    std::cerr << "DriftView::SetPoint:" << std::endl;
    std::cerr << "    No drift lines present." << std::endl;
    return;
  }

  driftLines.back().SetPoint(i, x, y, z);

}

void
DriftView::Plot() {
  
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle(label.c_str());
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();

  if (view == 0) view = TView::CreateView(1, 0, 0);
  view->SetRange(xMin, yMin, zMin, xMax, yMax, zMax);

  for (int i = nDriftLines; i--;) {
    driftLines[i].Draw("same");
  }
  canvas->Update();

}

}
