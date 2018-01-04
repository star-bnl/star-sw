#include <iostream>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>

#include "ComponentAnalyticField.hh"
#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "Sensor.hh"
#include "ViewField.hh"
#include "ViewCell.hh"
#include "Plotting.hh"

using namespace Garfield;

typedef void(*setupFunction)(ComponentAnalyticField*, double& xmin, double& xmax, double& ymin, double& ymax);

//==================================================
// Spiral
//==================================================
void spiral(ComponentAnalyticField* cmp,
            double& xmin, double& xmax, double& ymin, double& ymax) {

  const double d = 0.01;
  const unsigned int n = 100;
  for (unsigned int i = 0; i < n; ++i) {
    const double f = double(i) / n;
    const double r = 1. + 2 * f;
    const double phi = 4 * Pi * f;
    const double x = r * cos(phi);
    const double y = r * sin(phi);
    const std::string label = i % 2 == 0 ? "p" : "s";
    const double v = i % 2 == 0 ? -i * 10 : i * 10;
    cmp->AddWire(x, y, d, v, label);
  }
  xmin = ymin = -3.05;
  xmax = ymax = +3.05;
}

//==================================================
// Hexagon
//==================================================
void hextube(ComponentAnalyticField* cmp,
             double& xmin, double& xmax, double& ymin, double& ymax) {

  cmp->AddTube(2., 0., 6, "t");
  cmp->AddWire(0., 0., 100.e-4, 5000., "s");
  xmin = ymin = -2.05;
  xmax = ymax =  2.05;
}

//==================================================
// Two wires, no periodicity
//==================================================
void b2x(ComponentAnalyticField* cmp,
         double& xmin, double& xmax, double& ymin, double& ymax) {

  cmp->AddPlaneX(-1.,    0., "p");
  cmp->AddPlaneX( 1., 1000., "q");
  const double d = 0.01;
  cmp->AddWire(0.0, 0.0, d, 2000., "s");
  cmp->AddWire(0.5, 0.5, d, 2000., "p");
  xmin = -1.05;
  xmax =  1.05;
  ymin = -0.8;
  ymax =  1.3;
}

//==================================================
// MWPC
//==================================================
void mwpc(ComponentAnalyticField* cmp,
          double& xmin, double& xmax, double& ymin, double& ymax) {

  const double gap = 0.5;
  const double pitch = 0.2;
  cmp->SetPeriodicityY(pitch);

  cmp->AddPlaneX( 0.,  0., "p");
  cmp->AddPlaneX(gap,  0., "q");

  const double xw = 0.5 * gap;
  const double dw = 30.e-4;
  const double vw = 2650.;
  // const double vw = 2650.;
  const double tension = 40.;
  const double length = 32.;
  const double rho = 19.4;
  cmp->AddWire(xw, 0., dw, vw, "s", length, tension, rho);

  xmin = -0.01 * gap;
  xmax =  1.01 * gap;
  ymin = -5 * pitch;
  ymax =  5 * pitch;
}


//==================================================
// ALICE TPC O-ROC
//==================================================
void oroc(ComponentAnalyticField* cmp,
          double& xmin, double& xmax, double& ymin, double& ymax) {
 
  const double gap = 0.3;
  // Periodicity
  const double period = 0.25;
  cmp->SetPeriodicityX(period);

  // Sense (anode) wires.
  const double ys = gap;
  const double ds = 20.e-4;
  const double vs = 1700.;
  cmp->AddWire(0., ys, ds, vs, "s");

  // Cathode wires.
  const double yc = 2 * gap;
  const double dc = 75.e-4;
  cmp->AddWire(0.5 * period, yc, dc, 0., "c");

  // Gate wires.
  const double yg = 2 * gap + 0.3;
  const double dg = 75.e-4;
  const double vg = -100.;
  cmp->AddWire(0.25 * period, yg, dg, vg, "g");
  cmp->AddWire(0.75 * period, yg, dg, vg, "g");

  // Planes.
  cmp->AddPlaneY(0., 0., "p");
  const double yp = 2.;
  const double vp = -570.;
  cmp->AddPlaneY(yp, vp, "q");

  xmin = -5 * period;
  xmax =  5 * period;
  ymin = -0.1;
  // ymax = 1.5 * yg;
  ymax = yp + 0.1;
}

//==================================================
// Circle
//==================================================
void circle(ComponentAnalyticField* cmp,
            double& xmin, double& xmax, double& ymin, double& ymax) {

  cmp->AddPlaneX(-50., 0., "p");
  cmp->AddPlaneX( 50., 0., "p");
  cmp->AddPlaneY(-50., 0., "p");
  cmp->AddPlaneY( 50., 0., "p");
  const double r = 5.;
  const unsigned int n = 16;
  const double d = 0.01;
  for (unsigned int i = 0; i < 16; ++i) {
    const double f = double(i) / n;
    const double phi = TwoPi * f;
    const double x = r * cos(phi);
    const double y = r * sin(phi);
    cmp->AddWire(x, y, d, 1000., "p"); 
  }
  cmp->AddWire(0., 0., d, 4500., "s");
  xmin = ymin = -1.05 * r;
  xmax = ymax =  1.05 * r;
}

int main(int argc, char * argv[]) {

  TApplication app("app", &argc, argv);
  plottingEngine.SetDefaultStyle();

  // Setup the gas.
  MediumMagboltz* gas = new MediumMagboltz();
  gas->SetComposition("ne", 85.72, "co2", 9.52, "n2", 4.76);

  // Build the geometry
  GeometrySimple* geo = new GeometrySimple();
  SolidBox* box = new SolidBox(0., 0., 0., 100., 100., 100.);
  // Add the solid to the geometry, together with the gas
  geo->AddSolid(box, gas);

  // Setup the electric field 
  ComponentAnalyticField cmp;

  // Make a sensor
  Sensor sensor;
  sensor.AddComponent(&cmp);
  sensor.SetArea();

  // Plot the potential.
  TCanvas canvas("c", "", 600, 600);
  ViewCell cellView;
  cellView.SetCanvas(&canvas);
  cellView.SetComponent(&cmp);
  ViewField fieldView;
  fieldView.SetCanvas(&canvas);

  std::vector<setupFunction> cells;
  cells.push_back(&spiral);
  cells.push_back(&b2x);
  cells.push_back(&mwpc);
  cells.push_back(&oroc);
  cells.push_back(&circle);
  cells.push_back(&hextube);

  for (auto function : cells) {
    cmp.Clear();
    cmp.SetGeometry(geo);
    double xmin = 0., xmax = 0.;
    double ymin = 0., ymax = 0.;
    function(&cmp, xmin, xmax, ymin, ymax);
    std::cout << cmp.GetCellType() << "\n";
    fieldView.SetSensor(&sensor);
    fieldView.SetArea(xmin, ymin, xmax, ymax);
    fieldView.PlotContour();
    cellView.SetArea(xmin, ymin, -1., xmax, ymax, 1.);
    cellView.Plot2d();
    std::cout << "Press ENTER to continue.\n";
    std::cin.get();
  }
  app.Run(kTRUE);
}
