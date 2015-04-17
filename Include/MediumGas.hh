#ifndef G_MEDIUM_GAS_H
#define G_MEDIUM_GAS_H

#include <vector>
#include <cmath>

#include "Medium.hh"

namespace Garfield {

class MediumGas : public Medium {

 public:
  // Constructor
  MediumGas();
  // Destructor
  ~MediumGas() {}

  bool IsGas() const { return true; }

  // Set/get the gas mixture
  bool SetComposition(const std::string gas1, const double f1 = 1.,
                      const std::string gas2 = "", const double f2 = 0.,
                      const std::string gas3 = "", const double f3 = 0.,
                      const std::string gas4 = "", const double f4 = 0.,
                      const std::string gas5 = "", const double f5 = 0.,
                      const std::string gas6 = "", const double f6 = 0.);

  void GetComposition(std::string& gas1, double& f1, std::string& gas2,
                      double& f2, std::string& gas3, double& f3,
                      std::string& gas4, double& f4, std::string& gas5,
                      double& f5, std::string& gas6, double& f6);
  void GetComponent(const unsigned int& i, std::string& label, double& f);

  void SetAtomicNumber(const double& z);
  double GetAtomicNumber() const;
  void SetAtomicWeight(const double& a);
  double GetAtomicWeight() const;
  void SetNumberDensity(const double& n);
  double GetNumberDensity() const;
  void SetMassDensity(const double& rho);
  double GetMassDensity() const;

  bool LoadGasFile(const std::string& filename);
  bool WriteGasFile(const std::string& filename);

  void PrintGas();

  bool LoadIonMobility(const std::string& filename);

  void SetExtrapolationMethodExcitationRates(const std::string extrLow,
                                             const std::string extrHigh);
  void SetExtrapolationMethodIonisationRates(const std::string extrLow,
                                             const std::string extrHigh);
  void SetInterpolationMethodExcitationRates(const int intrp);
  void SetInterpolationMethodIonisationRates(const int intrp);

  // Scaling laws.
  double ScaleElectricField(const double& e) const {
    return e * pressureTable / m_pressure;
  }
  double UnScaleElectricField(const double& e) const {
    return e * m_pressure / pressureTable;
  }
  double ScaleDiffusion(const double& d) const {
    return d * sqrt(pressureTable / m_pressure);
  }
  double ScaleDiffusionTensor(const double& d) const {
    return d * pressureTable / m_pressure;
  }
  double ScaleTownsend(const double& alpha) const {
    return alpha * m_pressure / pressureTable;
  }
  double ScaleAttachment(const double& eta) const {
    return eta * m_pressure / pressureTable;
  }

  bool GetPhotoabsorptionCrossSection(const double& e, double& sigma,
                                      const unsigned int& i);

 protected:
  static const unsigned int m_nMaxGases = 6;

  // Gas mixture
  std::string gas[m_nMaxGases];
  double fraction[m_nMaxGases];
  double atWeight[m_nMaxGases];
  double atNum[m_nMaxGases];

  // Penning transfer
  // Flag enabling/disabling Penning transfer
  bool usePenning;
  // Penning transfer probability
  double rPenningGlobal;
  double rPenningGas[m_nMaxGases];
  // Mean distance of Penning ionisation
  double lambdaPenningGlobal;
  double lambdaPenningGas[m_nMaxGases];

  // Pressure and temperature at which the transport parameter
  // table was calculated
  double pressureTable, temperatureTable;

  // Table of Townsend coefficients without Penning transfer
  std::vector<std::vector<std::vector<double> > > tabTownsendNoPenning;

  // Tables for excitation and ionisation rates
  bool m_hasExcRates, m_hasIonRates;
  std::vector<std::vector<std::vector<std::vector<double> > > > tabExcRates;
  std::vector<std::vector<std::vector<std::vector<double> > > > tabIonRates;

  // Store excitation and ionization information
  int nExcListElements;
  struct excListElement {
    std::string label;
    double energy;
    double prob;
    double rms;
    double dt;
  };
  std::vector<excListElement> excitationList;

  int nIonListElements;
  struct ionListElement {
    std::string label;
    double energy;
  };
  std::vector<ionListElement> ionisationList;

  // Extrapolation/interpolation for excitation and ionisation rates.
  unsigned int m_extrLowExcRates, m_extrHighExcRates;
  unsigned int m_extrLowIonRates, m_extrHighIonRates;
  unsigned int m_intpExcRates;
  unsigned int m_intpIonRates;

  bool GetGasInfo(const std::string gasname, double& a, double& z) const;
  bool GetGasName(const int gasnumber, const int version, std::string& gasname);
  bool GetGasName(std::string input, std::string& gasname) const;
  bool GetGasNumberGasFile(const std::string input, int& number) const;
};
}

#endif
