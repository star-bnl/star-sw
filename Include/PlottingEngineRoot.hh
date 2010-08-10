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
    
  private:
    TStyle* garfieldStyle;
    TF1* f1;
    TF1* f2;
    void SetDefaultStyle();
    int  GetRootColor(std::string color);

};

}

#endif
