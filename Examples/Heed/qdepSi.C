#include <iostream>
#include <fstream>
#include <cmath>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TGraph.h>
#include <TH1F.h>
#include <TAxis.h>

#include "MediumSilicon.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "ComponentConstant.hh"
#include "Sensor.hh"
#include "TrackHeed.hh"
#include "Plotting.hh"

using namespace Garfield;

int main(int argc, char * argv[]) {

  TApplication app("app", &argc, argv);
  plottingEngine.SetDefaultStyle();

  MediumSilicon* si = new MediumSilicon();
  const double chamberWidth = 50.e-4;

  SolidBox* box = new SolidBox(0., 0., 0., 2., 2., chamberWidth);
  GeometrySimple* geo = new GeometrySimple();
  geo->AddSolid(box, si);

  // Make a component
  ComponentConstant* comp = new ComponentConstant();
  comp->SetGeometry(geo);
  comp->SetElectricField(0., 0., 20.);

  // Make a sensor
  Sensor* sensor = new Sensor();
  sensor->AddComponent(comp);

  TH1F* hNe = new TH1F("hNe", "", 150, 0., 15000.);
  hNe->StatOverflows();
  TH1F* hNc = new TH1F("hNc", "", 350, -0.5, 349.5);
  // Track class
  TrackHeed track;

  track.SetSensor(sensor);
  track.SetParticle("pion");
  track.SetBetaGamma(10.);
  const unsigned int nTracks = 10000;
  for (unsigned int i = 0; i < nTracks; ++i) {
    if (i % 1000 == 0) std::cout << "Track " << i << "\n";
    track.NewTrack(0., 0., 0., 0., 0., 0., 1.);
    double x = 0., y = 0., z = 0., t = 0.;
    int n = 0;
    double e = 0., dummy = 0.;
    unsigned int nsum = 0;
    unsigned int ncls = 0;
    while (track.GetCluster(x, y, z, t, n, e, dummy)) {
      nsum += n;
      ++ncls;
    }
    hNe->Fill(nsum);
    hNc->Fill(ncls);
  }

  TCanvas* cNe = new TCanvas("cNe", "", 600, 600);
  cNe->cd();
  hNe->Draw("hist");
  TCanvas* cNc = new TCanvas("cNc", "", 600, 600);
  cNc->cd();
  hNc->Draw("hist");
  app.Run(kTRUE);

}
