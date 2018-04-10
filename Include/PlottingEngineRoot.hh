#ifndef G_PLOTTING_ENGINE_ROOT_H
#define G_PLOTTING_ENGINE_ROOT_H

#include <TROOT.h>
#include <TStyle.h>
#include <TF1.h>

#include "PlottingEngine.hh"

namespace Garfield {

/// Definition of styles and color schemes.

class PlottingEngineRoot : public PlottingEngine {

 public:
  // Constructor
  PlottingEngineRoot();
  // Destructor
  virtual ~PlottingEngineRoot();

  void SetDefaultStyle();

  int GetRootColorLine1();
  int GetRootColorLine2();
  int GetRootColorElectron();
  int GetRootColorHole();
  int GetRootColorIon();
  int GetRootColorPhoton();
  int GetRootColorChargedParticle();

  bool GetRootColor(std::string color, int& rootcol);

 private:
  TStyle m_garfieldStyle;

  std::string m_colorLine1Default = "dark-blue";
  std::string m_colorLine2Default = "olive";
  std::string m_colorElectronDefault = "orange";
  std::string m_colorHoleDefault = "red";
  std::string m_colorIonDefault = "dark-red";
  std::string m_colorPhotonDefault = "blue";
  std::string m_colorChargedParticleDefault = "dark-green";
};
}

#endif
