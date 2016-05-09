#ifndef G_VIEW_SIGNAL
#define G_VIEW_SIGNAL

#include <string>

#include <TCanvas.h>
#include <TH1D.h>
#include <TGraph.h>

namespace Garfield {

class Sensor;

class ViewSignal {

 public:
  // Constructor
  ViewSignal();
  // Destructor
  ~ViewSignal();

  void SetSensor(Sensor* s);
  void SetCanvas(TCanvas* c);

  void PlotSignal(const std::string& label);
  TH1D* GetHistogram() { return m_hSignal; }

  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 private:
  std::string m_className;

  // Options
  bool m_debug;

  // Sensor
  Sensor* m_sensor;

  // Canvas
  TCanvas* m_canvas;
  bool m_hasExternalCanvas;

  // Histogram
  TH1D* m_hSignal;

  // Threshold crossings
  TGraph* m_gCrossings;
};
}
#endif
