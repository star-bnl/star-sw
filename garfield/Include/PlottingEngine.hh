// Abstract base class for plotting engines

#ifndef G_PLOTTING_ENGINE_H
#define G_PLOTTING_ENGINE_H

#include "Medium.hh"
#include "Sensor.hh"

namespace Garfield {

class PlottingEngine {

 public:
  // Constructor
  PlottingEngine()
      : m_className("PlottingEngine"),
        m_colorLine1("dark-blue"),
        m_colorLine2("olive"),
        m_colorElectron("orange"),
        m_colorHole("red"),
        m_colorIon("dark-red"),
        m_colorPhoton("blue"),
        m_colorChargedParticle("dark-green"),
        m_debug(false) {}

  // Destructor
  virtual ~PlottingEngine() {}

  // Set/get colors.
  void SetLineColor1(const std::string& col) { m_colorLine1 = col; }
  void SetLineColor2(const std::string& col) { m_colorLine2 = col; }
  void SetElectronColor(const std::string& col) { m_colorElectron = col; }
  void SetHoleColor(const std::string& col) { m_colorHole = col; }
  void SetIonColor(const std::string& col) { m_colorIon = col; }
  void SetPhotonColor(const std::string& col) { m_colorPhoton = col; }
  void SetChargedParticleColor(const std::string& col) {
    m_colorChargedParticle = col;
  }

  std::string GetLineColor1() const { return m_colorLine1; }
  std::string GetLineColor2() const { return m_colorLine2; }
  std::string GetElectronColor() const { return m_colorElectron; }
  std::string GetHoleColor() const { return m_colorHole; }
  std::string GetIonColor() const { return m_colorIon; }
  std::string GetPhotonColor() const { return m_colorPhoton; }
  std::string GetChargedParticleColor() const { return m_colorChargedParticle; }

  // Switch on/off debugging messages
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 protected:
  std::string m_className;

  std::string m_colorLine1, m_colorLine2;
  std::string m_colorElectron;
  std::string m_colorHole;
  std::string m_colorIon;
  std::string m_colorPhoton;
  std::string m_colorChargedParticle;

  bool m_debug;
};
}

#endif
