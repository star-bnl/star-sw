#include <iostream>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TH1F.h>

#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "ComponentConstant.hh"
#include "Sensor.hh"
#include "TrackHeed.hh"
#include "Plotting.hh"
#include "Random.hh"

using namespace Garfield;

int main(int argc, char * argv[]) {

  randomEngine.Seed(123456);
  TApplication app("app", &argc, argv);
  plottingEngine.SetDefaultStyle();

  // Histograms
  TH1::StatOverflows(kTRUE); 
  TH1F* hElectrons = new TH1F("hElectrons", "Number of electrons",
                              200, 0, 200);
  TH1F* hEdep = new TH1F("hEdep", "Energy Loss",
                         100, 0., 10.);

  // Make a medium
  MediumMagboltz* gas = new MediumMagboltz();
  gas->SetComposition("ar", 90., "co2", 10.);
  gas->SetTemperature(293.15);
  gas->SetPressure(760.);

  // Detector geometry
  // Gap [cm]
  const double width = 1.;
  SolidBox* box = new SolidBox(width / 2., 0., 0., width / 2., 10., 10.);
  GeometrySimple* geo = new GeometrySimple();
  geo->AddSolid(box, gas);

  // Make a component
  ComponentConstant* comp = new ComponentConstant();
  comp->SetGeometry(geo);
  comp->SetElectricField(100., 0., 0.);

  // Make a sensor
  Sensor* sensor = new Sensor();
  sensor->AddComponent(comp);

  // Track class
  TrackHeed* track = new TrackHeed();
  track->SetSensor(sensor);
  track->SetParticle("pi");
  track->SetMomentum(120.e9);

  const int nEvents = 10000;
  track->EnableDebugging();
  for (int i = 0; i < nEvents; ++i) {
    if (i == 1) track->DisableDebugging();
    if (i % 1000 == 0) std::cout << i << "/" << nEvents << "\n";
    // Initial position and direction 
    double x0 = 0., y0 = 0., z0 = 0., t0 = 0.;
    double dx0 = 1., dy0 = 0., dz0 = 0.; 
    track->NewTrack(x0, y0, z0, t0, dx0, dy0, dz0);
    // Cluster coordinates
    double xc = 0., yc = 0., zc = 0., tc = 0.;
    // Number of electrons produced in a collision
    int nc = 0;
    // Energy loss in a collision
    double ec = 0.;
    // Dummy variable (not used at present)
    double extra = 0.;
    // Total energy loss along the track
    double esum = 0.;
    // Total number of electrons produced along the track
    int nsum = 0;
    // Loop over the clusters.
    while (track->GetCluster(xc, yc, zc, tc, nc, ec, extra)) {
      esum += ec;
      nsum += nc;
    }
    hElectrons->Fill(nsum);
    hEdep->Fill(esum * 1.e-3);
  }
 
  TCanvas* c1 = new TCanvas();
  hElectrons->GetXaxis()->SetTitle("number of electrons"); 
  hElectrons->Draw();
  c1->SaveAs("edep.pdf");

  TCanvas* c2 = new TCanvas();
  hEdep->GetXaxis()->SetTitle("energy loss [keV]");
  hEdep->Draw();
  c2->SaveAs("ne.pdf");

  app.Run(kTRUE); 

}
