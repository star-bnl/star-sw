// Abstract base class for plotting engines

#ifndef G_PLOTTING_ENGINE_H
#define G_PLOTTING_ENGINE_H

#include "Medium.hh"
#include "Sensor.hh"

namespace Garfield {

class PlottingEngine {

  public:
    // Constructor
    PlottingEngine() : xMin(0.), xMax(1.), yMin(0.), yMax(1.),
      xLabel("x"), yLabel("y"), title(""),
      color1("orange"), color2("dark-green"), 
      debug(false) {}

    // Destructor
    virtual ~PlottingEngine() {}
    
    void SetRangeX(const double xmin, const double xmax) {
      xMin = xmin; xMax = xmax;
    }
    void SetRangeY(const double ymin, const double ymax) {
      yMin = ymin; yMax = ymax;
    }
    
    void SetLabelX(const std::string label) {xLabel = label;}
    void SetLabelY(const std::string label) {yLabel = label;}
    void SetTitle(const std::string label)  {title = label;}
    
    void SetLineColor1(const std::string col) {color1 = col;}
    void SetLineColor2(const std::string col) {color2 = col;}
    
    // Switch on/off debugging messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:
    double xMin, xMax;
    double yMin, yMax;
    std::string xLabel;
    std::string yLabel;
    std::string title;    
    std::string color1, color2;
    
    bool debug;

};

}

#endif
