#include <iostream>

#include <TAxis.h>

#include "Plotting.hh"
#include "Sensor.hh"
#include "ViewSignal.hh"

namespace Garfield {

ViewSignal::ViewSignal() { plottingEngine.SetDefaultStyle(); }

ViewSignal::~ViewSignal() {

  if (!m_hasExternalCanvas && m_canvas) delete m_canvas;
}

void ViewSignal::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor: Null pointer.\n";
    return;
  }
  m_sensor = s;
}

void ViewSignal::SetCanvas(TCanvas* c) {

  if (!c) return;
  if (!m_hasExternalCanvas && m_canvas) {
    delete m_canvas;
    m_canvas = nullptr;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewSignal::PlotSignal(const std::string& label, const bool total,
                            const bool electron, const bool ion) {

  if (!m_sensor) {
    std::cerr << m_className << "::PlotSignal: Sensor is not defined.\n";
    return;
  }

  // Setup the canvas
  if (!m_canvas) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle("Signal");
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();

  unsigned int nBins = 100;
  double t0 = 0., dt = 1.;
  m_sensor->GetTimeWindow(t0, dt, nBins);
  const double t1 = t0 + nBins * dt;

  const auto title = label.c_str();
  if (total) {
    const auto hname = FindHistogramName("hSignal_").c_str();
    m_hSignal.reset(new TH1D(hname, title, nBins, t0, t1));
    m_hSignal->SetLineColor(plottingEngine.GetRootColorLine1());
    m_hSignal->GetXaxis()->SetTitle("time [ns]");
    m_hSignal->GetYaxis()->SetTitle("signal [fC / ns]");
    for (unsigned int i = 0; i < nBins; ++i) {
      const double sig = m_sensor->GetSignal(label, i);
      m_hSignal->SetBinContent(i + 1, sig);
    }
    m_hSignal->Draw("");

    // Get and plot threshold crossings.
    const auto nCrossings = m_sensor->GetNumberOfThresholdCrossings();
    if (nCrossings > 0) {
      m_gCrossings.reset(new TGraph(nCrossings));
      m_gCrossings->SetMarkerStyle(20);
      m_gCrossings->SetMarkerColor(plottingEngine.GetRootColorLine1());
      double time = 0., level = 0.;
      bool rise = true;
      for (unsigned int i = 0; i < nCrossings; ++i) {
        if (m_sensor->GetThresholdCrossing(i, time, level, rise)) {
          m_gCrossings->SetPoint(i, time, level);
        }
      }
      m_gCrossings->Draw("psame");
    }
    m_canvas->Update();
  }

  // Plot the electron and ion signals if requested.
  if (electron) {
    const auto hname = FindHistogramName("hSignalElectrons_").c_str();
    m_hSignalElectrons.reset(new TH1D(hname, title, nBins, t0, t1));
    m_hSignalElectrons->SetLineColor(plottingEngine.GetRootColorElectron());
    m_hSignalElectrons->GetXaxis()->SetTitle("time [ns]");
    m_hSignalElectrons->GetYaxis()->SetTitle("signal [fC / ns]");
    for (unsigned int i = 0; i < nBins; ++i) {
      const double sig = m_sensor->GetElectronSignal(label, i);
      m_hSignalElectrons->SetBinContent(i + 1, sig);
    }
    m_hSignalElectrons->Draw("same");
    m_canvas->Update();
  }
  if (ion) {
    const auto hname = FindHistogramName("hSignalIons_").c_str();
    m_hSignalIons.reset(new TH1D(hname, title, nBins, t0, t1));
    m_hSignalIons->SetLineColor(plottingEngine.GetRootColorIon());
    m_hSignalIons->GetXaxis()->SetTitle("time [ns]");
    m_hSignalIons->GetYaxis()->SetTitle("signal [fC / ns]");
    for (unsigned int i = 0; i < nBins; ++i) {
      const double sig = m_sensor->GetIonSignal(label, i);
      m_hSignalIons->SetBinContent(i + 1, sig);
    }
    m_hSignalIons->Draw("same");
    m_canvas->Update();
  }
}

std::string ViewSignal::FindHistogramName(const std::string& base) const {

  std::string hname = base + "_0";
  int idx = 0;
  while (gDirectory->GetList()->FindObject(hname.c_str())) {
    ++idx;
    hname = base + "_" + std::to_string(idx);
  }
  return hname;
}
}
