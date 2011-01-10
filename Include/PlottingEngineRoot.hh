#ifndef G_PLOTTING_ENGINE_ROOT_H
#define G_PLOTTING_ENGINE_ROOT_H

#include <TROOT.h>
#include <TStyle.h>
#include <TF1.h> 

#include "PlottingEngine.hh"

namespace Garfield {

class PlottingEngineRoot : public PlottingEngine {

  public:
    // Constructor
    PlottingEngineRoot();
    // Destructor
    ~PlottingEngineRoot();

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
    TStyle* garfieldStyle;

    std::string colorLine1Default;
    std::string colorLine2Default;
    std::string colorElectronDefault;
    std::string colorHoleDefault;
    std::string colorIonDefault;
    std::string colorPhotonDefault;
    std::string colorChargedParticleDefault;

};

}

#endif
