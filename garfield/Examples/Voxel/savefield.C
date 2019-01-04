#include <iostream>
#include <fstream>
#include <cmath>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TH1D.h>

#include "MediumSilicon.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"

#include "ComponentAnalyticField.hh"

using namespace Garfield;

int main(int argc, char * argv[]) {

  TApplication app("app", &argc, argv);

  // Define the medium.
  MediumSilicon si;
  si.SetTemperature(300.);
  si.SetLatticeMobilityModelSentaurus();
  si.SetSaturationVelocityModelCanali();
  si.SetHighFieldMobilityModelCanali();

  // Thickness of silicon [cm]
  constexpr double gap = 50.e-4;
  // Build the geometry.
  SolidBox box(0., gap / 2., 0., 2., gap / 2., 2.);
  GeometrySimple geo;
  geo.AddSolid(&box, &si);

  // Make a component with analytic weighting field for a strip or pixel.
  constexpr double pitch = 50.e-4;
  ComponentAnalyticField wField;
  wField.SetGeometry(&geo);
  wField.AddPlaneY(0., -1., "back");
  wField.AddPlaneY(gap, 0., "front");
  wField.AddPixelOnPlaneY(0., -0.5 * pitch, 0.5 * pitch, 
                              -0.5 * pitch, 0.5 * pitch, "pixel");
  wField.AddReadout("pixel");

  const double xmin = 0.;
  const double xmax = 2 * pitch;
  const unsigned int nX = 100;
  const double dx = (xmax - xmin) / nX;
  const double zmin = 0.;
  const double zmax = 2 * pitch;
  const unsigned int nZ = 100;
  const double dz = (zmax - zmin) / nZ;
  const double ymin = 0.;
  const double ymax = gap;
  const unsigned int nY = 50;
  const double dy = (ymax - ymin) / nY;

  const std::string filename = "wfield.txt";
  std::ofstream outfile;
  outfile.open(filename.c_str(), std::ios::out);
  for (unsigned int i = 0; i < nX; ++i) {
    const double x = (i + 0.5) * dx;
    std::cout << i << " (x = " << x << ")\n";
    for (unsigned int j = 0; j < nY; ++j) {
      const double y = (j + 0.5) * dy;
      for (unsigned int k = 0; k < nZ; ++k) {
        const double z = (k + 0.5) * dz;
        double wx = 0., wy = 0., wz = 0.;
        wField.WeightingField(x, y, z, wx, wy, wz, "pixel");
        double v = wField.WeightingPotential(x, y, z, "pixel");
        outfile << i << "  " << j << "  " << k << "  " 
                << wx << "  " << wy << "  " << wz << "  " << v << "  0\n"; 
      }
    }
  }
  outfile.close();
}
