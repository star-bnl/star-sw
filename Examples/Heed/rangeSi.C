#include <iostream>
#include <fstream>
#include <cmath>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TH1F.h>
#include <TGraph.h>

#include "MediumSilicon.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "ComponentConstant.hh"
#include "Sensor.hh"
#include "TrackHeed.hh"
#include "FundamentalConstants.hh"

using namespace Garfield;

int main(int argc, char * argv[]) {

  TApplication app("app", &argc, argv);

  // Make a medium
  MediumSilicon* si = new MediumSilicon();
  const double rho = si->GetMassDensity();
  std::cout << "Density: " << rho << std::endl; 
  // Make a drift volume
  const double length = 100.;
  GeometrySimple* geo = new GeometrySimple();
  SolidBox* box = new SolidBox(0., 0., length, length, length, length);
  geo->AddSolid(box, si);
  
  // Make a component with constant drift field
  ComponentConstant* comp = new ComponentConstant();
  comp->SetGeometry(geo);
  const double field = 10.;
  comp->SetElectricField(0., 0., field);  

  // Make a sensor
  Sensor* sensor = new Sensor();
  sensor->AddComponent(comp);
  
  // Heed
  TrackHeed* track = new TrackHeed();
  track->SetSensor(sensor);
  
  // Histograms
  TH1::StatOverflows();
  const int nBins = 1000;
  double zMin =   0.;
  double zMax = 50.;
  TH1F* hLong = new TH1F("hLong", "Longitudinal", nBins, zMin, zMax);

  TCanvas* cHist = new TCanvas();
  TGraph* gRange95 = new TGraph();

  const unsigned int nEvents = 1e6;
  double de = 10.;
  const double emin = 30.;
  const double emax = 5000.;
  double e0 = emin;
  while (e0 <= emax) {
    // Estimate the range [mg / cm2].
    double rp = 69.7e-3 * pow(e0 * 1.e-3, 1.6);
    rp /= rho;
    zMax = 2 * rp;
    if (e0 < 500.) zMax += rp;
    if (e0 < 100.) zMax += 3 * rp;
    hLong->SetBins(nBins, zMin, zMax);
    hLong->Reset("ICE");
    hLong->ResetStats();
    std::cout << "Primary energy: " << e0 << " eV" << std::endl;
    double nEntries = 0.;
    for (unsigned int i = 0; i < nEvents; ++i) {
      int np = 0;
      track->TransportDeltaElectron(0., 0., 0., 0., e0, 0., 0., 1., np);
      double x1, y1, z1, t1, e1, dx1, dy1, dz1;
      if (np <= 1) continue;
      for (int j = np - 1; j--;) {
        track->GetElectron(j, x1, y1, z1, t1, e1, dx1, dy1, dz1);
        if (fabs(z1) < 1.e-8) continue;
        hLong->Fill(z1 * 1.e4);
        nEntries += 1.; 
      }
    }

    const double fraction = 0.95;

    double sum = 0.;
    int iUp = 0;
    for (int j = 1; j <= nBins; ++j) {
      sum += hLong->GetBinContent(j);
      if (sum >= fraction * nEntries) {
        iUp = j; 
        break;
      }
    }
    if (iUp <= 1) {
      std::cerr << "    Unable to determine the range.\n";
      // continue;
    }
    int iLow = nBins;
    sum = 0.;
    for (int j = nBins; j--;) {
      sum += hLong->GetBinContent(j);
      if (sum >= (1. - fraction) * nEntries) {
        iLow = j;
        break;
      }
    }
    const double r95 = 0.5 * (hLong->GetBinCenter(iUp) +
                              hLong->GetBinCenter(iLow));
    const double r95rho = r95 * rho * 1.e2;
    std::cout << "    Range: " << r95rho << " ug/cm2\n";
    if (gRange95 != 0) {
      delete gRange95; gRange95 = 0;
      gRange95 = new TGraph();
      gRange95->SetMarkerStyle(20);
      gRange95->SetMarkerSize(2);
      gRange95->SetMarkerColor(kBlue + 2);
    }
    gRange95->SetPoint(0, r95, 0.5 * (hLong->GetBinContent(iUp) +
                                      hLong->GetBinContent(iLow)));
    cHist->cd();
    hLong->Draw();
    gRange95->Draw("psame");
    cHist->Update();

    std::ofstream outfile;  
    outfile.open("r95_Heed_Si.txt", std::ios::out | std::ios::app);
    outfile << e0 << "  " << r95 << "  " << r95rho << "\n";
    outfile.close();

    e0 += de;
    if (e0 >= 100.) de = 50.;
    if (e0 >= 200.) de = 100.;
    if (e0 >= 1000.) de = 1000.;
  }

  // app.Run(kTRUE);

}
