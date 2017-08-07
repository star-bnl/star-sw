#include <iostream>

#include <TApplication.h>
#include <TH1F.h>
#include <TCanvas.h>

#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "MediumMagboltz.hh"
#include "ComponentConstant.hh"
#include "Sensor.hh"
#include "TrackSrim.hh"
#include "Random.hh"

using namespace Garfield;

void checkhwf() {
  const double work = 30.;
  const double fano = 0.3;
  TH1F* hwf = new TH1F("hwf", "Heed work Fano", 200, 0, 200);
  for (unsigned int i = 0; i < 10000000; ++i) {
    double rnd = 1.e6 * (RndmHeedWF(1.e-6 * work, fano));
    hwf->Fill(rnd);
  }
  TCanvas* chwf = new TCanvas("chwf", "Heed Work Fano", 100, 100, 800, 800);
  chwf->cd();
  hwf->Draw();
  chwf->Update();
  double mean = hwf->GetMean();
  double rms = hwf->GetRMS();
  const double r = rms / mean;
  std::cout << "Histogram mean: " << mean << ", Fano: " << r * r << "\n";
}

void track() {

  // Define the medium.
  MediumMagboltz* gas = new MediumMagboltz();
  gas->SetComposition("ar", 70., "co2", 30.);
  // Set temperature [K] and pressure [Torr].
  gas->SetPressure(760.0);
  gas->SetTemperature(293.15);

  // Define the geometry.
  const double width = 10.;
  const double length = 0.0150;
  SolidBox* box = new SolidBox(length / 2., 0., 0., 
                               length / 2., width / 2., width / 2.);
  GeometrySimple* geo = new GeometrySimple();
  geo->AddSolid(box, gas);

  // Make a component (with constant electric field).
  ComponentConstant* comp = new ComponentConstant();
  comp->SetGeometry(geo);
  comp->SetElectricField(1000., 0., 0.);
  
  // Make a sensor.
  Sensor* sensor = new Sensor();
  sensor->AddComponent(comp);
  
  // Create a track class.
  TrackSrim* tr = new TrackSrim();
  // Connect the track to a sensor.
  tr->SetSensor(sensor);
  // Read SRIM output from file.
  const std::string file = "alpha_ArCO2_70_30.txt";
  if (!tr->ReadFile(file)) {
    std::cerr << "Reading SRIM file failed.\n";
    return;
  }
  // Set the initial kinetic energy of the particle (in eV).
  tr->SetKineticEnergy(1.47e6);
  // Set the W value and Fano factor of the gas.
  tr->SetWorkFunction(30.0);
  tr->SetFanoFactor(0.3);
  // Set A and Z of the gas (not sure what's the correct mixing law).
  const double za = 0.7 * (18. / 40.) + 0.3 * (22. / 44.);
  tr->SetAtomicMassNumbers(22. / za, 22);
  // Specify how many electrons we want to be grouped to a cluster.
  tr->SetTargetClusterSize(500);
  // tr->SetClustersMaximum(1000);

  // Make some plots of the SRIM data.
  tr->PlotEnergyLoss();
  tr->PlotRange();
  tr->PlotStraggling();
  // Print a table of the SRIM data.
  tr->Print();

  // Setup histograms.
  TH1F* hX = new TH1F("hX", "x-end", 100, 0, 0.01);
  TH1F* hY = new TH1F("hY", "y-end", 100, -0.01, 0.01);
  TH1F* hZ = new TH1F("hZ", "z-end", 100, -0.01, 0.01);
  TH1F* hNe = new TH1F("hNe", "Electrons", 100, 48000, 50000);

  // Generate tracks.
  const unsigned int nTracks = 1000;
  for (unsigned int i = 0; i < nTracks; ++i) {
    if (!tr->NewTrack(0., 0., 0., 0., 1., 0., 0.)) {
      std::cerr << "Generating clusters failed; skipping this track.\n";
      continue;
    }
    // Retrieve the clusters.
    unsigned int netot = 0;
    double xc, yc, zc, tc, ec, ekin;
    while (true) {
      int ne = 0;
      const bool done = !tr->GetCluster(xc, yc, zc, tc, ne, ec, ekin);
      if (done) {
        hX->Fill(xc);
        hY->Fill(yc);
        hZ->Fill(zc);
        hNe->Fill(netot);
        break;
      }
      // Count the total number of electrons.
      netot += ne;
    }
  }

  // Plot the histograms.
  TCanvas* c = new TCanvas("c", "SRIM", 100, 100, 800, 800);
  c->Divide(2, 2);
  c->cd(1); hX->Draw();
  c->cd(2); hY->Draw();
  c->cd(3); hZ->Draw();
  c->cd(4); hNe->Draw();
  c->Update();

}

int main(int argc, char *argv[]) {

  // Application
  TApplication app("app", &argc, argv);

  // Run a couple of tests
  // checkhwf();
  // Produce tracks.
  track();
  std::cout << "Done.\n";

  // Start loop
  app.Run();
  return 0;
}
