#include <iostream>

#include <TAxis.h> 

#include "Plotting.hh"
#include "Sensor.hh"
#include "ViewSignal.hh"

namespace Garfield {

ViewSignal::ViewSignal() :
  className("ViewSignal"), debug(false), sensor(0),
  canvas(0), hasExternalCanvas(false),
  hSignal(0), gCrossings(0) {

  plottingEngine.SetDefaultStyle();

}

ViewSignal::~ViewSignal() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;
  if (hSignal != 0) delete hSignal;
  if (gCrossings != 0) delete gCrossings;

}

void
ViewSignal::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
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
    std::cerr << className << "::PlotSignal:\n";
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
  hSignal->SetLineColor(plottingEngine.GetRootColorLine1());
  hSignal->GetXaxis()->SetTitle("time [ns]");
  hSignal->GetYaxis()->SetTitle("signal [fC / ns]");
  
  double sig = 0.;  
  for (int i = nBins; i--;) {
    sig = sensor->GetSignal(label, i);
    hSignal->SetBinContent(i + 1, sig);
  }

  if (gCrossings != 0) {
    delete gCrossings;
    gCrossings = 0;
  }

  // Get threshold crossings.
  const int nCrossings = sensor->GetNumberOfThresholdCrossings();
  if (nCrossings > 0) {
    gCrossings = new TGraph(nCrossings);
    gCrossings->SetMarkerStyle(20);
    gCrossings->SetMarkerColor(plottingEngine.GetRootColorLine1());
    double time = 0., level = 0.;
    bool rise = true;
    for (int i = nCrossings; i--;) {
      if (sensor->GetThresholdCrossing(i, time, level, rise)) {
        gCrossings->SetPoint(i, time, level);
      }
    }
  }   
  
  hSignal->Draw();
  if (nCrossings > 0) gCrossings->Draw("psame");
  canvas->Update();

}

}
