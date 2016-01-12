#include <iostream>

#include <TAxis.h>

#include "Plotting.hh"
#include "Sensor.hh"
#include "ViewSignal.hh"

namespace Garfield {

ViewSignal::ViewSignal()
    : m_className("ViewSignal"),
      m_debug(false),
      m_sensor(0),
      m_canvas(0),
      m_hasExternalCanvas(false),
      m_hSignal(0),
      m_gCrossings(0) {

  plottingEngine.SetDefaultStyle();
}

ViewSignal::~ViewSignal() {

  if (!m_hasExternalCanvas && m_canvas != 0) delete m_canvas;
  if (m_hSignal != 0) delete m_hSignal;
  if (m_gCrossings != 0) delete m_gCrossings;
}

void ViewSignal::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << m_className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  m_sensor = s;
}

void ViewSignal::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!m_hasExternalCanvas && m_canvas != 0) {
    delete m_canvas;
    m_canvas = 0;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewSignal::PlotSignal(const std::string label) {

  if (m_sensor == 0) {
    std::cerr << m_className << "::PlotSignal:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }

  // Setup the canvas
  if (m_canvas == 0) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle("Signal");
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();

  int nBins;
  double t0, dt;
  m_sensor->GetTimeWindow(t0, dt, nBins);

  if (m_hSignal != 0) {
    delete m_hSignal;
    m_hSignal = 0;
  }
  m_hSignal = new TH1D("hSignal", label.c_str(), nBins, t0, t0 + nBins * dt);
  m_hSignal->SetLineColor(plottingEngine.GetRootColorLine1());
  m_hSignal->GetXaxis()->SetTitle("time [ns]");
  m_hSignal->GetYaxis()->SetTitle("signal [fC / ns]");

  double sig = 0.;
  for (int i = nBins; i--;) {
    sig = m_sensor->GetSignal(label, i);
    m_hSignal->SetBinContent(i + 1, sig);
  }

  if (m_gCrossings != 0) {
    delete m_gCrossings;
    m_gCrossings = 0;
  }

  // Get threshold crossings.
  const int nCrossings = m_sensor->GetNumberOfThresholdCrossings();
  if (nCrossings > 0) {
    m_gCrossings = new TGraph(nCrossings);
    m_gCrossings->SetMarkerStyle(20);
    m_gCrossings->SetMarkerColor(plottingEngine.GetRootColorLine1());
    double time = 0., level = 0.;
    bool rise = true;
    for (int i = nCrossings; i--;) {
      if (m_sensor->GetThresholdCrossing(i, time, level, rise)) {
        m_gCrossings->SetPoint(i, time, level);
      }
    }
  }

  m_hSignal->Draw("");
  if (nCrossings > 0) m_gCrossings->Draw("psame");
  m_canvas->Update();
}
}
