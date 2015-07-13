/* Heinrich Schindler 04/22/13
  root.exe lGarfield.C iroc.C+
 */

#include <iostream>
#include <fstream>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TH1F.h>
#include <TH2F.h>
#include "TSystem.h"

#include "ViewField.hh"
#include "ViewDrift.hh"

#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "ComponentAnalyticField.hh"
#include "Sensor.hh"

#include "AvalancheMicroscopic.hh"
#include "AvalancheMC.hh"

#include "FundamentalConstants.hh"
#include "Random.hh"
#include "Plotting.hh"

using namespace Garfield;

void iroc() {

  plottingEngine.SetDefaultStyle();

  // TApplication app("app", &argc, argv);
  
  // Voltage settings
  const double vSens = 1550.;
  const double vGate = -100.;
  
  const double gap = 0.2;
  
  // y coordinates of the wires
  const double ySens = gap;
  const double yCath = 2 * gap;
  const double yGate = 2 * gap + 0.3;
  
  // Cathode plane, voltage adjusted to give drift field of 400 V/cm
  const double yPlane = 1.8;
  const double vPlane = -528.;

  // Periodicities
  const int nRep = 10;
  const int ng = 2;
  const double perSens = 0.25;
  const double perCath = perSens;
  const double perGate = perSens / ng;
  
  // Wire diameters
  const double dSens = 20.e-4;
  const double dCath = 75.e-4;
  const double dGate = 75.e-4;
  
  // Setup the gas.
  const double pressure = (1000. / 1013.25) * 760.;
  const double temperature = 273.15 + 19.5;
  const double density = LoschmidtNumber * 
                         (pressure / AtmosphericPressure) *
                         (273.15 / temperature);
  const double pt = (temperature / 273.15) * (760. / pressure);
  // Create a gas medium.  
  MediumMagboltz* gas = new MediumMagboltz();
  // Set the temperature [K] and pressure [Torr].
  gas->SetTemperature(temperature);
  gas->SetPressure(pressure);
  gas->SetMaxElectronEnergy(300.);
  // Specify the gas mixture.
  // gas->SetComposition("ne", 90., "co2", 10.);
  gas->SetComposition("ne", 85.7, "co2", 9.5, "n2", 4.8);
  const double rPenning = 0.57;
  const double lambdaPenning = 0.e-4;
  gas->EnablePenningTransfer(rPenning, lambdaPenning, "ne");
  const Char_t *path = gSystem->ExpandPathName("$GARFIELD_HOME/Data/IonMobility_Ne+_Ne.txt");
  gas->LoadIonMobility(path);
  delete path;
  gas->Initialise();
  
  // Build the geometry, in this case just a box.
  GeometrySimple* geo = new GeometrySimple();
  SolidBox* box = new SolidBox(0., yGate / 2., 0., 
                               nRep * perSens, yGate / 2., 2 * gap);
  // Add the solid to the geometry, together with the medium inside.
  geo->AddSolid(box, gas);

  // Setup the electric field.
  ComponentAnalyticField* comp = new ComponentAnalyticField();
  comp->SetPeriodicityX(nRep * perSens);
  // Add the anode wires.
  for (int i = 0; i < nRep; ++i) {
    comp->AddWire((i - 2.) * perSens, ySens, dSens, vSens, "s");
  }
  // Add the cathode wires.
  for (int i = 0; i < nRep; ++i) {
    comp->AddWire(perCath * (i + 0.5), yCath, dCath, 0., "c");
  }
  // Add the gate wires.
  for (int i = 0; i < nRep * ng; ++i) {
    comp->AddWire(perGate * (i + 0.5), yGate, dGate, vGate, "g");
  }
  comp->AddPlaneY(0., 0., "p");
  comp->AddPlaneY(yPlane, vPlane, "q");
  
  comp->AddReadout("s");
  comp->AddReadout("p");
 
  comp->SetGeometry(geo);

  // Make a sensor
  Sensor* sensor = new Sensor();
  sensor->AddComponent(comp);
  sensor->AddElectrode(comp, "p");
  sensor->SetTimeWindow(0., 50., 1000);
  sensor->ClearSignal();
  
  AvalancheMicroscopic* aval = new AvalancheMicroscopic();
  aval->SetSensor(sensor);
  //aval->EnableNullCollisionSteps();

  AvalancheMC* drift = new AvalancheMC();
  drift->SetDistanceSteps(0.0005);
  drift->SetSensor(sensor);

  const int nBinsAngular = 360;
  const int nBinsRadial = 100;
  const int nBinsGain = 100;
  TH1::StatOverflows();
  TH1F* hAngle = new TH1F("hAngle", "Angular distribution",
                          nBinsAngular, -180, 180);
  TH2F* hRadial = new TH2F("hRadial", "Ion starting points",
                           nBinsRadial, dSens / 2., 3. * dSens,
                           nBinsAngular, -180, 180); 
  TH1F* hElectrons = new TH1F("hElectrons", "Number of electrons",
                              nBinsGain, 0, 50000.);
  TH1F* hIons = new TH1F("hIons", "Number of ions",
                         nBinsGain, 0, 50000.);

  const double xmin = -0.12;
  const double xmax =  0.12;
  
  const int nEvents = 1000;
  double x0, y0, z0, t0, e0;
  double x1, y1, z1, t1, e1;
  int status;
  double r, phi;
  int ne, ni;
  int nEndpoints;
  for (int i = nEvents; i--;) {
    gas->ResetCollisionCounters();
    sensor->NewSignal();
    x0 = xmin + RndmUniform() * (xmax - xmin);
    y0 = yGate - 0.1;
    z0 = t0 = 0.;
    e0 = 0.1;
    aval->AvalancheElectron(x0, y0, z0, t0, e0, 0., 0., 0.);
    aval->GetAvalancheSize(ne, ni);
    if (ne > 0) hElectrons->Fill(ne);    
    if (ni > 0) hIons->Fill(ni);
    nEndpoints = aval->GetNumberOfElectronEndpoints();
    if (i % 10 == 0) std::cout << i << "/" << nEvents << ": "
                               << ne << " electrons, "
                               << ni << " ions" << std::endl; 
    if (ne <= 0) continue;
    for (int j = nEndpoints; j--;) {
      aval->GetElectronEndpoint(j, x0, y0, z0, t0, e0,
                                   x1, y1, z1, t1, e1, status);
      phi = atan((y1 - ySens) / x1) * 180. / Pi;
      if (x1 > 0.) {
        phi = -phi + 90.;
      } else {
        phi = -phi - 90.;
      }
      if (x1 > xmin && x1 < xmax) {
        hAngle->Fill(phi);
      }
      r = sqrt(pow(y0 - ySens, 2) + x0 * x0);
      phi = atan((y0 - ySens) / x0) * 180. / Pi;
      if (x0 > 0.) {
        phi = -phi + 90;
      } else {
        phi = -phi - 90.;
      }
      if (x0 > xmin && x0 < xmax) hRadial->Fill(r, phi);
      // drift->DriftIon(x0, y0, z0, t0);
    }
    // std::cout << "Next avalanche..." << std::endl;
  }

  // app.Run(kTRUE);

}
