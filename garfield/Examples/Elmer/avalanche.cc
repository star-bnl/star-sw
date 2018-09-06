/**
 * avalanche.cc
 * General program flow based on example code from the Garfield++ website.
 *
 * Demonstrates electron avalanche and induced signal readout with
 * 2D finite-element visualization in Garfield++ with a LEM. 
 * LEM parameters are from:
 * C. Shalem et. al. Nucl. Instr. Meth. A, 558, 475 (2006).
 *
*/
#include <iostream>
#include <cmath>

#include <TCanvas.h>
#include <TApplication.h>
#include <TFile.h>

#include "MediumMagboltz.hh"
#include "ComponentElmer.hh"
#include "Sensor.hh"
#include "ViewField.hh"
#include "Plotting.hh"
#include "ViewFEMesh.hh"
#include "ViewSignal.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"
#include "AvalancheMicroscopic.hh"

using namespace Garfield;

int main(int argc, char* argv[]) {

  TApplication app("app", &argc, argv);

  // Set relevant LEM parameters.
  // LEM thickness in cm
  const double lem_th = 0.04;      
  // Copper thickness
  const double lem_cpth = 0.0035;  
  // LEM pitch in cm
  const double lem_pitch = 0.07;   
  // X-width of drift simulation will cover between +/- axis_x
  const double axis_x = 0.1;  
  // Y-width of drift simulation will cover between +/- axis_y
  const double axis_y = 0.1;  
  const double axis_z = 0.25 + lem_th / 2 + lem_cpth;


  // Define the medium.
  MediumMagboltz* gas = new MediumMagboltz();
  // Set the temperature (K)
  gas->SetTemperature(293.15);  
  // Set the pressure (Torr)
  gas->SetPressure(740.);       
  // Allow for drifting in this medium
  gas->EnableDrift();           
  // Specify the gas mixture (Ar/CO2 70:30)
  gas->SetComposition("ar", 70., "co2", 30.);  

  // Import an Elmer-created field map.
  ComponentElmer* elm = new ComponentElmer(
      "gemcell/mesh.header", "gemcell/mesh.elements", "gemcell/mesh.nodes",
      "gemcell/dielectrics.dat", "gemcell/gemcell.result", "cm");
  elm->EnablePeriodicityX();
  elm->EnableMirrorPeriodicityY();
  elm->SetMedium(0, gas);
  // Import the weighting field for the readout electrode.
  // elm->SetWeightingField("gemcell/gemcell_WTlel.result", "wtlel");

  // Set up a sensor object.
  Sensor* sensor = new Sensor();
  sensor->AddComponent(elm);
  sensor->SetArea(-axis_x, -axis_y, -axis_z, axis_x, axis_y, axis_z);
  // sensor->AddElectrode(elm, "wtlel");
  // Set the signal binning.
  const double tEnd = 500.0;
  const int nsBins = 500;
  // sensor->SetTimeWindow(0., tEnd / nsBins, nsBins);

  // Create an avalanche object
  AvalancheMicroscopic* aval = new AvalancheMicroscopic();
  aval->SetSensor(sensor);
  aval->SetCollisionSteps(100);
  // aval->EnableSignalCalculation();

  // Set up the object for drift line visualization.
  ViewDrift* viewDrift = new ViewDrift();
  viewDrift->SetArea(-axis_x, -axis_y, -axis_z, axis_x, axis_y, axis_z);
  aval->EnablePlotting(viewDrift);

  // Set the electron start parameters.
  // Starting z position for electron drift
  const double zi = 0.5 * lem_th + lem_cpth + 0.1;  
  double ri = (lem_pitch / 2) * RndmUniform();
  double thetai = RndmUniform() * TwoPi;
  double xi = ri * cos(thetai);
  double yi = ri * sin(thetai);
  // Calculate the avalanche.
  aval->AvalancheElectron(xi, yi, zi, 0., 0., 0., 0., 0.);
  std::cout << "... avalanche complete with "
            << aval->GetNumberOfElectronEndpoints() << " electron tracks.\n";

  // Extract the calculated signal.
  double bscale = tEnd / nsBins;  // time per bin
  double sum = 0.;  // to keep a running sum of the integrated signal

  // Create ROOT histograms of the signal and a file in which to store them.
  TFile* f = new TFile("avalanche_signals.root", "RECREATE");
  TH1F* hS = new TH1F("hh", "hh", nsBins, 0, tEnd);        // total signal
  TH1F* hInt = new TH1F("hInt", "hInt", nsBins, 0, tEnd);  // integrated signal

  // Fill the histograms with the signals.
  //  Note that the signals will be in C/(ns*binWidth), and we will divide by e
  // to give a signal in e/(ns*binWidth).
  //  The total signal is then the integral over all bins multiplied by the bin
  // width in ns.
  for (int i = 0; i < nsBins; i++) {
    double wt = sensor->GetSignal("wtlel", i) / ElementaryCharge;
    sum += wt;
    hS->Fill(i * bscale, wt);
    hInt->Fill(i * bscale, sum);
  }

  // Write the histograms to the TFile.
  hS->Write();
  hInt->Write();
  f->Close();

  // Plot the signal.
  const bool plotSignal = false;
  if (plotSignal) {
    TCanvas* cSignal = new TCanvas("signal", "Signal");
    ViewSignal* vSignal = new ViewSignal();
    vSignal->SetSensor(sensor);
    vSignal->SetCanvas(cSignal);
    vSignal->PlotSignal("wtlel");
  }

  // Plot the geometry, field and drift lines.
  TCanvas* cGeom = new TCanvas("geom", "Geometry/Avalanche/Fields");
  const bool plotContours = false;
  if (plotContours) {
    ViewField* vf = new ViewField();
    vf->SetSensor(sensor);
    vf->SetCanvas(cGeom);
    vf->SetArea(-axis_x, -axis_y, axis_x, axis_y);
    vf->SetNumberOfContours(40);
    vf->SetNumberOfSamples2d(30, 30);
    vf->SetPlane(0, -1, 0, 0, 0, 0);
    vf->PlotContour("v");
  }

  // Set up the object for FE mesh visualization.
  ViewFEMesh* vFE = new ViewFEMesh();
  vFE->SetArea(-axis_x, -axis_z, 0., axis_x, axis_z, 0.);
  vFE->SetCanvas(cGeom);
  vFE->SetComponent(elm);
  vFE->SetPlane(0, -1, 0, 0, 0, 0);
  vFE->SetFillMesh(true);
  vFE->SetColor(1, kGray);
  vFE->SetColor(2, kYellow + 3);
  vFE->SetColor(3, kYellow + 3);
  if (!plotContours) {
    vFE->EnableAxes();
    vFE->SetXaxisTitle("x (cm)");
    vFE->SetYaxisTitle("z (cm)");
    vFE->SetViewDrift(viewDrift);
    vFE->Plot();
  }

  app.Run(kTRUE);

  return 0;
}
