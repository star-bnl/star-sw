#include <iostream>
#include <sstream>

#include <TAxis.h>

#include "Plotting.hh"
#include "Sensor.hh"
#include "ViewSignal.hh"

namespace Garfield {

ViewSignal::ViewSignal()
    : m_className("ViewSignal"),
      m_debug(false),
      m_sensor(NULL),
      m_canvas(NULL),
      m_hasExternalCanvas(false),
      m_hSignal(NULL),
      m_gCrossings(NULL) {

  plottingEngine.SetDefaultStyle();
}

ViewSignal::~ViewSignal() {

  if (!m_hasExternalCanvas && m_canvas) delete m_canvas;
  if (m_hSignal) delete m_hSignal;
  if (m_gCrossings) delete m_gCrossings;
}

void ViewSignal::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  m_sensor = s;
}

void ViewSignal::SetCanvas(TCanvas* c) {

  if (!c) return;
  if (!m_hasExternalCanvas && m_canvas) {
    delete m_canvas;
    m_canvas = NULL;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewSignal::PlotSignal(const std::string& label) {

  if (!m_sensor) {
    std::cerr << m_className << "::PlotSignal:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }

  // Setup the canvas
  if (!m_canvas) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle("Signal");
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();

  int nBins;
  double t0, dt;
  m_sensor->GetTimeWindow(t0, dt, nBins);

  if (m_hSignal) {
    delete m_hSignal;
    m_hSignal = NULL;
  }
  std::string hname = "hSignal_0";
  int idx = 0;
  while (gDirectory->GetList()->FindObject(hname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << "hSignal_";
    ss << idx;
    hname = ss.str();
  }

  m_hSignal = new TH1D(hname.c_str(), label.c_str(), nBins, t0, t0 + nBins * dt);
  m_hSignal->SetLineColor(plottingEngine.GetRootColorLine1());
  m_hSignal->GetXaxis()->SetTitle("time [ns]");
  m_hSignal->GetYaxis()->SetTitle("signal [fC / ns]");

  double sig = 0.;
  for (int i = nBins; i--;) {
    sig = m_sensor->GetSignal(label, i);
    m_hSignal->SetBinContent(i + 1, sig);
  }

  if (m_gCrossings) {
    delete m_gCrossings;
    m_gCrossings = NULL;
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
