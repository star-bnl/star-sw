#include <iostream>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>

#include "MediumSilicon.hh"
#include "ComponentVoxel.hh"
#include "Sensor.hh"

#include "ViewField.hh"
#include "Plotting.hh"

using namespace Garfield;

int main(int argc, char *argv[]) {

  plottingEngine.SetDefaultStyle();
  TApplication app("app", &argc, argv);

  // Define the medium.
  MediumSilicon* si = new MediumSilicon();

  // Setup the mesh.
  const unsigned int nX = 2 * 110 + 1;
  const unsigned int nY = 2 * 200 + 1;
  const double xMin =   -0.5e-4;
  const double xMax =  110.5e-4;
  const double yMin =   -0.5e-4;
  const double yMax =  200.5e-4;
  const double zMin = -100.e-4;
  const double zMax =  100.e-4;

  ComponentVoxel* efield = new ComponentVoxel();
  efield->SetMesh(nX, nY, 1, xMin, xMax, yMin, yMax, zMin, zMax);
  // Load the field map.
  efield->LoadElectricField("Efield.txt", "XY", false, false, 1.0e-4);
  efield->EnablePeriodicityX();
  efield->SetMedium(0, si);
  efield->PrintRegions();
  efield->EnableInterpolation();

  // Create a sensor.
  Sensor* sensor = new Sensor();
  sensor->AddComponent(efield);
  sensor->SetArea(0.0, 0.0, -100.0e-4, 110.0e-4, 200.0e-4, 100e-4);

  ViewField *view = new ViewField();
  view->SetSensor(sensor);
  view->SetElectricFieldRange(0.0, 200000.0);
  view->PlotContour("e");
  app.Run(true);

}
