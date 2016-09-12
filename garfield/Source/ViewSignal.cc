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
      m_hSignal(NULL), m_hSignalElectrons(NULL), m_hSignalIons(NULL),
      m_gCrossings(NULL) {

  plottingEngine.SetDefaultStyle();
}

ViewSignal::~ViewSignal() {

  if (!m_hasExternalCanvas && m_canvas) delete m_canvas;
  if (m_hSignal) delete m_hSignal;
  if (m_hSignalElectrons) delete m_hSignalElectrons;
  if (m_hSignalIons) delete m_hSignalIons;
  if (m_gCrossings) delete m_gCrossings;
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
    m_canvas = NULL;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewSignal::PlotSignal(const std::string& label, const bool total,
                            const bool electron, const bool ion) {

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

  unsigned int nBins = 100;
  double t0 = 0., dt = 1.;
  m_sensor->GetTimeWindow(t0, dt, nBins);

  if (total) {
    if (m_hSignal) {
      delete m_hSignal;
      m_hSignal = NULL;
    }
    const std::string hname = FindHistogramName("hSignal_");
    m_hSignal = new TH1D(hname.c_str(), label.c_str(), nBins, t0, t0 + nBins * dt);
    m_hSignal->SetLineColor(plottingEngine.GetRootColorLine1());
    m_hSignal->GetXaxis()->SetTitle("time [ns]");
    m_hSignal->GetYaxis()->SetTitle("signal [fC / ns]");

    for (unsigned int i = 0; i < nBins; ++i) {
      const double sig = m_sensor->GetSignal(label, i);
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

  // Plot the electron and ion signals if requested.
  if (electron) {
    if (m_hSignalElectrons) {
      delete m_hSignalElectrons;
      m_hSignalElectrons = NULL;
    }
    const std::string hname = FindHistogramName("hSignalElectrons_");
    m_hSignalElectrons = new TH1D(hname.c_str(), 
                                  label.c_str(), nBins, t0, t0 + nBins * dt);
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
    if (m_hSignalIons) {
      delete m_hSignalIons;
      m_hSignalIons = NULL;
    }
    const std::string hname = FindHistogramName("hSignalIons_");
    m_hSignalIons = new TH1D(hname.c_str(), 
                                  label.c_str(), nBins, t0, t0 + nBins * dt);
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
    std::stringstream ss;
    ss << base;
    ss << idx;
    hname = ss.str();
  }
  return hname;
}

}
