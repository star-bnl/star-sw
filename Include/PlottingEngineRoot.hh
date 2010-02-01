#ifndef G_PLOTTING_ENGINE_ROOT_H
#define G_PLOTTING_ENGINE_ROOT_H

#include <TROOT.h>
#include <TStyle.h>
#include <TF1.h> 
#include <TH1F.h>

#include "PlottingEngine.hh"

namespace Garfield {

class PlottingEngineRoot : public PlottingEngine {

  public:
    // Constructor
    PlottingEngineRoot();
    // Destructor
    ~PlottingEngineRoot();
    
    void PlotVelocity(Medium* m, 
                      const bool electron = true, 
                      const bool hole = false,
                      const bool ion = false);
    void PlotTownsend(Medium* m, 
                      const bool electron = true, 
                      const bool hole = false);
    void PlotAttachment(Medium* m,
                        const bool electron = true,
                        const bool hole = false);

    void PlotSignal(Sensor* s, const std::string label);

  private:
    TStyle* garfieldStyle;
    TF1* f1;
    TF1* f2;
    TH1F* hSignal;
    void SetDefaultStyle();
    int  GetRootColor(std::string color);

};

}

#endif
