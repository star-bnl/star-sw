#ifndef G_VIEW_SIGNAL
#define G_VIEW_SIGNAL

#include <string>

#include <TCanvas.h>
#include <TH1D.h>
#include <TGraph.h>

namespace Garfield {

class Sensor;

/// Plot the signal computed by a sensor as a ROOT histogram.

class ViewSignal {

 public:
  /// Constructor
  ViewSignal();
  /// Destructor
  ~ViewSignal();

  /// Set the sensor from which to retrieve the signal.
  void SetSensor(Sensor* s);
  /// Set the pad on which to draw the histogram. 
  void SetCanvas(TCanvas* c);

  /** Plot the signal.
    * \param label Identifier (weighting field) of the signal to be plotted.
    * \param total Flag whether to plot the total induced signal.
    * \param electron Flag whether to plot the electron-induced signal.
    * \param ion Flag whether to plot the ion/hole-induced signal.
    */
  void PlotSignal(const std::string& label, const bool total = true,
                  const bool electron = false, const bool ion = false);
  /** Retrieve the histogram for the induced signal.
    * \param h histogram to be returned 
               ('t': total, 'e': electron-induced, 'h': ion-induced).
    **/ 
  TH1D* GetHistogram(const char h = 't') { 
    return h == 'e' ? m_hSignalElectrons : 'i' ? m_hSignalIons : m_hSignal; 
  }

  /// Enable/disable debugging output.
  void EnableDebugging(const bool on = true) { m_debug = on; }

 private:
  std::string m_className;

  // Options
  bool m_debug;

  // Sensor
  Sensor* m_sensor;

  // Canvas
  TCanvas* m_canvas;
  bool m_hasExternalCanvas;

  // Histograms
  TH1D* m_hSignal;
  TH1D* m_hSignalElectrons;
  TH1D* m_hSignalIons;

  // Threshold crossings
  TGraph* m_gCrossings;

  // Find an unused histogram name.
  std::string FindHistogramName(const std::string& base) const;
};
}
#endif
