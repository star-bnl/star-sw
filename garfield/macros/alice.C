/* Heinrich Schindler 04/22/13
  root.exe lGarfield.C alice.C+
 */
#include <iostream>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TH1F.h>
#include <TH2F.h>
#include "TSystem.h"
#include "ComponentAnalyticField.hh"
#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "Sensor.hh"
#include "AvalancheMicroscopic.hh"
#include "AvalancheMC.hh"
#include "ViewField.hh"
#include "ViewDrift.hh"
#include "ViewSignal.hh"
#include "FundamentalConstants.hh"
#include "Random.hh"

using namespace Garfield;
void alice() {
  
  // Switch between IROC and OROC
  const bool iroc = false;
  
  // Use nitrogen or not
  const bool withN2 = true;

  // Voltage settings
  const double vSens = 1700.;
  const double vGate = -100.;
  
  double gap = 0.3;     // O-ROCs
  if (iroc) gap = 0.2;  // I-ROCs
  
  // y coordinates of the wires
  const double ySens = gap;
  const double yCath = 2. * gap;
  const double yGate = 2. * gap + 0.3;
  
  // Plane coordinates
  const double yPlane = 2.;
  const double vPlane = -570.;
  
  // Periodicity
  const double period = 0.25;
  const int nRep = 10;
   
  const int nc = 1;
  const int ng = 2;
  
  const double perSens = period;
  const double perCath = period / nc;
  const double perGate = period / ng;
  
  // Wire diameters
  const double dSens = 0.0020;
  const double dCath = 0.0075;
  const double dGate = 0.0075;
  
  const double length = 86.;

  // Setup the gas
  const double pressure = 750.;
  const double temperature = 293.15;
  const double density = LoschmidtNumber * 
                         (pressure / AtmosphericPressure) *
                         (273.15 / temperature);
  const double pt = (temperature / 273.16) * (760. / pressure);
  // Make a gas medium
  MediumMagboltz* gas = new MediumMagboltz();
  // Set the temperature [K] and pressure [Torr]
  gas->SetTemperature(temperature);
  gas->SetPressure(pressure);
  gas->SetMaxElectronEnergy(200.);
  gas->EnableDebugging();
  // Specify the gas mixture
  if (withN2) {
    gas->SetComposition("ne", 85.72, "co2", 9.52, "n2", 4.76);
  } else {
    gas->SetComposition("ne", 90., "co2", 10.);
  }
  // Set the ion mobility [cm2 / (V ns)]
  const Char_t *path = gSystem->ExpandPathName("$GARFIELD_HOME/Data/IonMobility_Ne+_Ne.txt");
  gas->LoadIonMobility(path);
  delete path;
  gas->Initialise();
  gas->DisableDebugging();
  
  // Build the geometry
  GeometrySimple* geo = new GeometrySimple();
  SolidBox* box = new SolidBox(0.,            yPlane / 2., 0., 
                               nRep * period, yPlane / 2., 2. * gap);
  // Add the solid to the geometry, together with the gas
  geo->AddSolid(box, gas);

  // Setup the electric field
  ComponentAnalyticField* comp = new ComponentAnalyticField();
  // Periodicity
  comp->SetPeriodicityX(nRep * perSens);
  // Wire grids
  for (int i = 0; i < nRep; ++i) {
    comp->AddWire((i - 2.) * perSens, ySens, dSens, vSens, "s");
  }
  for (int i = 0; i < nRep * nc; ++i) {
    comp->AddWire((i + 0.5) * perCath, yCath, dCath, 0., "c");
  }
  for (int i = 0; i < nRep * ng; ++i) {
    comp->AddWire((i + 0.5) * perGate, yGate, dGate, vGate, "g");
  }
  // Planes
  comp->AddPlaneY(0., 0., "p");
  comp->AddPlaneY(yPlane, vPlane, "q");
  // Calculate signals for pad plane
  comp->AddReadout("p");
 
  comp->SetGeometry(geo);

  // Make a sensor
  Sensor* sensor = new Sensor();
  sensor->AddComponent(comp);
  sensor->AddElectrode(comp, "p");
  sensor->SetTimeWindow(0., 50., 1000);
  sensor->ClearSignal();

  // Plot the potential
  const bool plotField = true;  
  if (plotField) {
    ViewField* fView = new ViewField;
    fView->SetSensor(sensor);
    fView->SetArea(-nRep * period / 2., 0.,           
                    nRep * period / 2., 1.5 * yGate); 
    fView->PlotSurface();
  }
  //  return;
  AvalancheMicroscopic* aval = new AvalancheMicroscopic();
  aval->SetSensor(sensor);
  // aval->EnableSignalCalculation();

  AvalancheMC* drift = new AvalancheMC();
  drift->SetDistanceSteps(0.001);
  drift->SetSensor(sensor);
  drift->EnableSignalCalculation();

  const int nBinsAngular = 36;
  const int nBinsRadial = 100;
  TH1F* hAngle = new TH1F("hAngle", "Angular distribution",
                          nBinsAngular, -180, 180);
  TH2F* hIon = new TH2F("hIon", "Ion starting points",
                         nBinsRadial, dSens / 2., 4. * dSens,
                         nBinsAngular, -180, 180); 
  hAngle->StatOverflows();
  hIon->StatOverflows();

  const double xmin = -0.12;
  const double xmax =  0.12;
  
  const int nElectrons = 10;
  double x0, y0, z0, t0, e0;
  double x1, y1, z1, t1, e1;
  int status;
  double r, phi;
  int nEndpoints;
  for (int i = nElectrons; i--;) {
    std::cout << "Event " << i << std::endl;
    std::cout << "    Electron avalanche..." << std::endl;
    x0 = xmin + RndmUniform() * (xmax - xmin);
    y0 = yGate + 0.1;
    z0 = 0.;
    t0 = 0.;
    e0 = 0.1;
    aval->AvalancheElectron(x0, y0, z0, t0, e0, 0., 0., 0.);
    nEndpoints = aval->GetNumberOfElectronEndpoints();
    std::cout << "    Ion drift..." << std::endl;
    for (int j = nEndpoints; j--;) {
      aval->GetElectronEndpoint(j, x0, y0, z0, t0, e0,
                                   x1, y1, z1, t1, e1, status);
      phi = atan((y1 - ySens) / x1) * 180. / Pi;
      if (x1 > 0.) {
        phi = -phi + 90.;
      } else {
        phi = -phi - 90.;
      }
      if (x1 > xmin && x1 < xmax) hAngle->Fill(phi);
      r = sqrt(pow(y0 - ySens, 2) + x0 * x0);
      phi = atan((y0 - ySens) / x0) * 180. / Pi;
      if (x0 > 0.) phi = -phi + 90;
      else phi = -phi - 90.;
      if (x0 > xmin && x0 < xmax && y0 < yGate) hIon->Fill(r, phi);
      drift->DriftIon(x0, y0, z0, t0);
    }
    sensor->NewSignal();
  }

  TCanvas* c1 = new TCanvas();
  hAngle->Draw();
  TCanvas* c2 = new TCanvas();
  hIon->Draw();

  TCanvas* c3 = new TCanvas();
  ViewSignal* sView = new ViewSignal();
  sView->SetSensor(sensor);
  sView->PlotSignal("p");
}
