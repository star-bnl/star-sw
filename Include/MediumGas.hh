#ifndef G_MEDIUM_GAS_H
#define G_MEDIUM_GAS_H

#include <vector>
#include <cmath>

#include "Medium.hh"

namespace Garfield {

/// Base class for gas media.

class MediumGas : public Medium {

 public:
  /// Constructor
  MediumGas();
  /// Destructor
  virtual ~MediumGas() {}

  bool IsGas() const override { return true; }

  /// Set the gas mixture.
  bool SetComposition(const std::string& gas1, const double f1 = 1.,
                      const std::string& gas2 = "", const double f2 = 0.,
                      const std::string& gas3 = "", const double f3 = 0.,
                      const std::string& gas4 = "", const double f4 = 0.,
                      const std::string& gas5 = "", const double f5 = 0.,
                      const std::string& gas6 = "", const double f6 = 0.);
  /// Retrieve the gas mixture.
  void GetComposition(std::string& gas1, double& f1, std::string& gas2,
                      double& f2, std::string& gas3, double& f3,
                      std::string& gas4, double& f4, std::string& gas5,
                      double& f5, std::string& gas6, double& f6);
  void GetComponent(const unsigned int i, std::string& label, 
                    double& f) override;

  void SetAtomicNumber(const double z) override;
  double GetAtomicNumber() const override;
  void SetAtomicWeight(const double a) override;
  double GetAtomicWeight() const override;
  void SetNumberDensity(const double n) override;
  double GetNumberDensity() const override;
  void SetMassDensity(const double rho) override;
  double GetMassDensity() const override;

  bool LoadGasFile(const std::string& filename);
  bool WriteGasFile(const std::string& filename);

  void PrintGas();

  bool LoadIonMobility(const std::string& filename);

  void SetExtrapolationMethodExcitationRates(const std::string& extrLow,
                                             const std::string& extrHigh);
  void SetExtrapolationMethodIonisationRates(const std::string& extrLow,
                                             const std::string& extrHigh);
  void SetInterpolationMethodExcitationRates(const int intrp);
  void SetInterpolationMethodIonisationRates(const int intrp);

  // Scaling laws.
  // TODO: cache scaling factors.
  double ScaleElectricField(const double e) const override {
    return e * m_pressureTable / m_pressure;
  }
  double UnScaleElectricField(const double e) const override {
    return e * m_pressure / m_pressureTable;
  }
  double ScaleDiffusion(const double d) const override {
    return d * sqrt(m_pressureTable / m_pressure);
  }
  double ScaleDiffusionTensor(const double d) const override {
    return d * m_pressureTable / m_pressure;
  }
  double ScaleTownsend(const double alpha) const override {
    return alpha * m_pressure / m_pressureTable;
  }
  double ScaleAttachment(const double eta) const override {
    return eta * m_pressure / m_pressureTable;
  }
  double ScaleLorentzAngle(const double lor) const override {
    return lor * m_pressure / m_pressureTable;
  }

  bool GetPhotoabsorptionCrossSection(const double& e, double& sigma,
                                      const unsigned int& i);

 protected:
  static const unsigned int m_nMaxGases = 6;

  // Gas mixture
  std::string m_gas[m_nMaxGases];
  double m_fraction[m_nMaxGases];
  double m_atWeight[m_nMaxGases];
  double m_atNum[m_nMaxGases];

  // Penning transfer
  // Flag enabling/disabling Penning transfer
  bool m_usePenning = false;
  // Penning transfer probability
  double m_rPenningGlobal = 0.;
  double m_rPenningGas[m_nMaxGases];
  // Mean distance of Penning ionisation
  double m_lambdaPenningGlobal = 0.;
  double m_lambdaPenningGas[m_nMaxGases];

  // Pressure at which the transport parameter table was calculated
  double m_pressureTable;
  // Temperature at which the transport parameter table was calculated
  double m_temperatureTable;

  // Table of Townsend coefficients without Penning transfer
  std::vector<std::vector<std::vector<double> > > m_eTownsendNoPenning;

  // Tables for excitation and ionisation rates
  std::vector<std::vector<std::vector<std::vector<double> > > > m_excRates;
  std::vector<std::vector<std::vector<std::vector<double> > > > m_ionRates;

  // Store excitation and ionization information
  struct ExcListElement {
    std::string label;
    double energy;
    double prob;
    double rms;
    double dt;
  };
  std::vector<ExcListElement> m_excitationList;

  struct IonListElement {
    std::string label;
    double energy;
  };
  std::vector<IonListElement> m_ionisationList;

  // Extrapolation/interpolation for excitation and ionisation rates.
  std::pair<unsigned int, unsigned int> m_extrExcRates = {0, 1};
  std::pair<unsigned int, unsigned int> m_extrIonRates = {0, 1};
  unsigned int m_intpExcRates = 2;
  unsigned int m_intpIonRates = 2;

  bool GetGasInfo(const std::string& gasname, double& a, double& z) const;
  bool GetGasName(const int gasnumber, const int version, std::string& gasname);
  bool GetGasName(std::string input, std::string& gasname) const;
  bool GetGasNumberGasFile(const std::string& input, int& number) const;
};
}

#endif
