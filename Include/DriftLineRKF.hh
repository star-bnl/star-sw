#ifndef G_DRIFTLINE_RKF_H
#define G_DRIFTLINE_RKF_H

#include <vector>
#include <string>

#include "Sensor.hh"
#include "ViewDrift.hh"
#include "Medium.hh"
#include "GeometryBase.hh"


namespace Garfield {
  
  class DriftLineRKF {
    
  public:
 
    DriftLineRKF();
    ~DriftLineRKF() {}

    void SetSensor(Sensor* s);
    void SetIntegrationAccuracy(double intAct);
    void SetMaximumStepSize(double maxStep);

    void EnablePlotting(ViewDrift* view);
    void DisablePlotting();

    void DriftLine(double x0, double y0, double z0, double t0,  
                   std::string particleType = "e-");
    
    void SetMaxSteps(int max){maxSteps = max;};


    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:

    Sensor* sensor;
    Medium* medium;

    double maxStepSize;
    double intAccuracy;

    int maxSteps;

    bool usePlotting;
    int iLine;
    ViewDrift* viewer;

    bool debug;

    //Used to drift a particle to the edge of a boundary.
    void EndDriftLine();
    //Used to drift a particle to a wire
    void DriftToWire(double x0, double y0, double z0);

    struct step{
      //position (initial and final)
      double xi,xf;
      double yi,yf;
      double zi,zf;
      //time (initial and final)
      double ti,tf;
      std::string status;
    };
    std::vector<step> path;

  };

}


#endif
