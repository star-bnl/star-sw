#ifndef G_VIEW_MEDIUM
#define G_VIEW_MEDIUM

#include <string>
#include <vector>

#include <TCanvas.h>
#include <TF1.h>
#include <TGraph.h>

namespace Garfield {

class Medium;

class ViewMedium {

 public:
  // Constructor
  ViewMedium();
  // Destructor
  ~ViewMedium();

  void SetCanvas(TCanvas* c);

  void SetMedium(Medium* m);

  void SetElectricFieldRange(const double emin, const double emax);
  void SetMagneticFieldRange(const double bmin, const double bmax);
  void SetBAngleRange(const double amin, const double amax);
  void SetFunctionRange(const double vmin, const double vmax);
  void SetFunctionRange();

  void PlotElectronVelocity(const char xaxis, const double e, const double b,
                            const double a);
  void PlotHoleVelocity(const char xaxis, const double e, const double b,
                        const double a);
  void PlotIonVelocity(const char xaxis, const double e, const double b,
                       const double a);
  void PlotElectronDiffusion(const char xaxis, const double e, const double b,
                             const double a);
  void PlotHoleDiffusion(const char xaxis, const double e, const double b,
                         const double a);
  void PlotIonDiffusion(const char xaxis, const double e, const double b,
                        const double a);
  void PlotElectronTownsend(const char xaxis, const double e, const double b,
                            const double a);
  void PlotHoleTownsend(const char xaxis, const double e, const double b,
                        const double a);
  void PlotElectronAttachment(const char xaxis, const double e, const double b,
                              const double a);
  void PlotHoleAttachment(const char xaxis, const double e, const double b,
                          const double a);
  void PlotElectronCrossSections();
  double EvaluateFunction(double* pos, double* par);

 private:
  std::string m_className;

  // Options
  bool m_debug;

  // Canvas
  TCanvas* m_canvas;
  bool m_hasExternalCanvas;

  Medium* m_medium;

  // Ranges for variable parameters
  double m_eMin, m_eMax;
  double m_bMin, m_bMax;
  double m_aMin, m_aMax;
  double m_vMin, m_vMax;

  // Fixed parameters
  double m_efield;
  double m_bfield;
  double m_angle;

  // Tolerances for marker plotting
  double m_etolerance;
  double m_btolerance;
  double m_atolerance;

  // Functions
  std::vector<TF1> m_functions;
  int m_nFunctions;
  // Graphs
  std::vector<TGraph> m_graphs;
  int m_nGraphs;

  void SetupCanvas();
  void AddFunction(const double xmin, const double xmax, const double ymin,
                   const double ymax, const bool keep, const std::string xlabel,
                   const std::string ylabel, const int type, const char xaxis,
                   const double e, const double b, const double a);
};
}
#endif
