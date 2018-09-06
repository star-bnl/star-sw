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

/// Plot transport coefficients as function of electric and magnetic field. 

class ViewMedium {

 public:
  /// Constructor
  ViewMedium();
  /// Destructor
  ~ViewMedium();

  /// Set the canvas to be painted on.
  void SetCanvas(TCanvas* c);
  // Set the medium from which to retrieve the transport coefficients.
  void SetMedium(Medium* m);

  /// Set the limits of the electric field.
  void SetElectricFieldRange(const double emin, const double emax);
  /// Set the limits of the magnetic field.
  void SetMagneticFieldRange(const double bmin, const double bmax);
  /// Set the limits of the angle between electric and magnetic field.
  void SetBAngleRange(const double amin, const double amax);
  /// Set the range of the function (velocity etc.) to be plotted. 
  void SetFunctionRange(const double vmin, const double vmax);
  /// Use the default function range.
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
  void PlotElectronLorentzAngle(const char xaxis, const double e, const double b,
                               const double a = HalfPi);
  void PlotElectronCrossSections();
  double EvaluateFunction(double* pos, double* par);

  enum Property {
    ElectronVelocityE,
    ElectronTransverseDiffusion,
    ElectronLongitudinalDiffusion,
    ElectronTownsend,
    ElectronAttachment,
    ElectronLorentzAngle,
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
    HoleVelocityExB
  };

 private:
  std::string m_className = "ViewMedium";

  // Options
  bool m_debug = false;

  // Canvas
  TCanvas* m_canvas = nullptr;
  bool m_hasExternalCanvas = false;

  Medium* m_medium = nullptr;

  // Ranges for variable parameters
  double m_eMin = 0., m_eMax = 1000.;
  double m_bMin = 0., m_bMax = 5.;
  double m_aMin = 0., m_aMax = 3.14;
  double m_vMin = 0., m_vMax = 0.;

  // Fixed parameters
  double m_efield = 500.;
  double m_bfield = 100.;
  double m_angle = 0.;

  // Tolerances for marker plotting
  double m_etolerance = 1.;
  double m_btolerance = 0.01;
  double m_atolerance = 0.05;

  // Labels
  std::string m_labele = "electric field [V/cm]";
  std::string m_labelb = "magnetic field [T]";
  std::string m_labela = "magnetic field angle [rad]";
  std::string m_labelv = "drift velocity [cm/ns]";
  std::string m_labeld = "diffusion coefficient [#sqrt{cm}]";

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
  int GetColor(const Property property) const;
};
}
#endif
