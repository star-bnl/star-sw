#ifndef G_VIEW_SIGNAL
#define G_VIEW_SIGNAL

#include <string>

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TH1D.h>
#include <TGraph.h>

namespace Garfield {

class Sensor;

class ViewSignal { 

  RQ_OBJECT("ViewSignal")
  
  public:
    // Constructor
    ViewSignal();
    // Destructor
    ~ViewSignal();
   
    void SetSensor(Sensor* s);
    void SetCanvas(TCanvas* c);
 
    void PlotSignal(const std::string label);

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:
  
    // Options
    bool debug;
    
    // Sensor
    Sensor* sensor;

    // Time window
    double tmin, tmax;
    
    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;

    // Histogram
    TH1D* hSignal;

    // Threshold crossings
    TGraph* gCrossings;
    
};

}
#endif
