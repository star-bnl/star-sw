#include <iostream>

#include <TAxis.h> 

#include "ViewSignal.hh"
#include "Plotting.hh"

namespace Garfield {

ViewSignal::ViewSignal() :
  debug(false), sensor(0),
  canvas(0), hasExternalCanvas(false),
  hSignal(0) {

}

ViewSignal::~ViewSignal() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;
  if (hSignal != 0) delete hSignal;

}

void
ViewSignal::SetSensor(Sensor* s) {

  if (s == 0) {
    printf("ViewSignal::SetSensor:\n");
    printf("    Sensor pointer is null.\n");
    return;
  }

  sensor = s; 

}

void
ViewSignal::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void 
ViewSignal::PlotSignal(const std::string label) {

  if (sensor == 0) {
    std::cerr << "ViewSignal::PlotSignal:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }
  
  // Setup the canvas
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle("Signal");
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();

  int nBins;
  double t0, dt;
  sensor->GetTimeWindow(t0, dt, nBins);
  
  if (hSignal != 0) {
    delete hSignal;
    hSignal = 0;
  }
  hSignal = new TH1D("hSignal", label.c_str(), nBins, t0, t0 + nBins * dt);
  hSignal->GetXaxis()->SetTitle("time [ns]");
  hSignal->GetYaxis()->SetTitle("signal");
  
  double sig = 0.;  
  for (int i = nBins; i--;) {
    sig = sensor->GetSignal(label, i);
    hSignal->SetBinContent(i, sig);
  }
  
  hSignal->Draw();
  canvas->Update();

}

}
