#include <iostream>
#include <string>

#include "DriftView.hh"

namespace Garfield {

DriftView::DriftView():
  debug(false),
  canvas("DriftView", "Drift View"), 
  xMin(-1.), yMin(-1.), zMin(-1.), 
  xMax(1.),  yMax(1.),  zMax(1.),
  frame("frame", "", 10, xMin, xMax, 10, yMin, yMax, 10, zMin, zMax),
  nDriftLines(0) {

  frame.SetStats(kFALSE);
  frame.SetXTitle("x");
  frame.SetYTitle("y");
  frame.SetZTitle("z");
  
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
  xMin = std::min(xmin,xmax);
  yMin = std::min(ymin,ymax);
  zMin = std::min(zmin,zmax);
  xMax = std::max(xmin,xmax);
  yMax = std::max(ymin,ymax);
  zMax = std::max(zmin,zmax);
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

  frame.Draw();
  for (int i = nDriftLines; i--;) {
    driftLines[i].Draw();
  }
  canvas.Update();

}

}
