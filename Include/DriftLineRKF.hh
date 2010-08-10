#ifndef G_DRIFTLINE_RKF_H
#define G_DRIFTLINE_RKF_H

#include <vector>
#include <string>

#include "Sensor.hh"
#include "ViewDrift.hh"

namespace Garfield {
  
  class DriftLineRKF {
    
  public:
 
    DriftLineRKF();
    ~DriftLineRKF() {}

    void SetSensor(Sensor* s);
    void SetErrorBounds(const double min, const double max);
    void SetTimeStep(const double dt);

    void EnablePlotting(ViewDrift* view);
    void DisablePlotting();

    void DriftLine(double x0, double y0, double z0, double t0, double e0, 
                   double dx = 1., double dy = 0., double dz = 0.,
                   std::string particleType = "e-");

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:

    Sensor* sensor;
    Medium* medium;

    double minErr, maxErr;
    double timeStep;

    bool usePlotting;
    ViewDrift* viewer;

    bool debug;

    bool EquationOfMotion(const double x, const double y, const double z,
                          const double vx, const double vy, const double vz,
                          const double t, double& kx, double& ky, double& kz, 
                          const double qom);

  };

}


#endif
