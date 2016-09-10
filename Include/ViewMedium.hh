#ifndef G_VIEW_MEDIUM
#define G_VIEW_MEDIUM

#include <string>
#include <vector>

#include <TCanvas.h>
#include <TF1.h>
#include <TGraph.h>

#include "FundamentalConstants.hh"

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
                            const double a = HalfPi);
  void PlotHoleVelocity(const char xaxis, const double e, const double b,
                        const double a = HalfPi);
  void PlotIonVelocity(const char xaxis, const double e, const double b,
                       const double a = HalfPi);
  void PlotElectronDiffusion(const char xaxis, const double e, const double b,
                             const double a = HalfPi);
  void PlotHoleDiffusion(const char xaxis, const double e, const double b,
                         const double a = HalfPi);
  void PlotIonDiffusion(const char xaxis, const double e, const double b,
                        const double a = HalfPi);
  void PlotElectronTownsend(const char xaxis, const double e, const double b,
                            const double a = HalfPi);
  void PlotHoleTownsend(const char xaxis, const double e, const double b,
                        const double a = HalfPi);
  void PlotElectronAttachment(const char xaxis, const double e, const double b,
                              const double a = HalfPi);
  void PlotHoleAttachment(const char xaxis, const double e, const double b,
                          const double a = HalfPi);
  void PlotElectronCrossSections();
  double EvaluateFunction(double* pos, double* par);

  enum Property {
    ElectronVelocityE,
    ElectronTransverseDiffusion,
    ElectronLongitudinalDiffusion,
    ElectronTownsend,
    ElectronAttachment,
    HoleVelocityE = 10,
    HoleTransverseDiffusion,
    HoleLongitudinalDiffusion,
    HoleTownsend,
    HoleAttachment,
    IonVelocity = 20,
    IonTransverseDiffusion,
    IonLongitudinalDiffusion,
    ElectronVelocityB,
    ElectronVelocityExB,
    HoleVelocityB,
    HoleVelocityExB,
  };

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

  // Labels
  std::string m_labele;
  std::string m_labelb;
  std::string m_labela;
  std::string m_labelv;
  std::string m_labeld;

  // Functions
  std::vector<TF1> m_functions;
  // Graphs
  std::vector<TGraph> m_graphs;

  void SetupCanvas();
  void AddFunction(const double xmin, const double xmax, const double ymin,
                   const double ymax, const bool keep, 
                   const std::string& xlabel, const std::string& ylabel, 
                   const int type, const char xaxis,
                   const double e, const double b, const double a);
  int GetColor(const unsigned int property) const;
};
}
#endif
