#include <iostream>
#include <string>

#include "DriftView.hh"

namespace Garfield {

DriftView::DriftView():
  debug(false),
  xMin(-1.), yMin(-1.), zMin(-1.), 
  xMax(1.),  yMax(1.),  zMax(1.),
  frame("frame", "", 10, xMin, xMax, 10, yMin, yMax, 10, zMin, zMax),
  nDriftLines(0) {

  frame.SetStats(kFALSE);
  frame.SetXTitle("x");
  frame.SetYTitle("y");
  frame.SetZTitle("z");

  canvas = new TCanvas("DriftView", "Drift View");
  
}

DriftView::~DriftView() {

  if (canvas != 0) delete canvas;

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
  frame.SetBins(10, xMin, xMax, 10, yMin, yMax, 10, zMin, zMax);
  
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
  
  if (canvas == 0) canvas = new TCanvas("DriftView", "Drift View");
  canvas->cd();

  frame.Draw();
  for (int i = nDriftLines; i--;) {
    driftLines[i].Draw();
  }
  canvas->Update();

}

}
