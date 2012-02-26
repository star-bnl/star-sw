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

    void SetMaxSteps(int max) {maxSteps = max;};

    void DriftElectron(const double x0, const double y0, const double z0,
                       const double t0);
    void DriftHole(const double x0, const double y0, const double z0,
                   const double t0);
    void DriftIon(const double x0, const double y0, const double z0,
                  const double t0);

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

    void EnableVerbose()  {verbose = true;}
    void DisableVerbose() {verbose = false;}

  private:

    std::string className;

    Sensor* sensor;
    Medium* medium;

    double maxStepSize;
    double intAccuracy;

    int maxSteps;

    bool usePlotting;
    int iLine;
    ViewDrift* viewer;

    bool debug;
    bool verbose;

    void DriftLine(double x0, double y0, double z0, double t0,  
                   double& meanTime, double& rmsTime,                   
                   std::string particleType);
    // Used to drift a particle to the edge of a boundary.
    void EndDriftLine(const std::string particleType);
    // Used to drift a particle to a wire
    void DriftToWire(double x0, double y0, double z0, 
                     const std::string particleType);
    // Used to determine the diffusion over the drift length
    double IntegrateDiffusion(const double x,  const double y,  const double z,
			      const double xe, const double ye, const double ze,
                              const std::string particleType);
    
    // These variables store the position and radius ofa trapping wire
    double xWire, yWire, rWire;

    struct step {
      // Position (initial and final)
      double xi, xf;
      double yi, yf;
      double zi, zf;
      // Time (initial and final)
      double ti, tf;
      std::string status;
    };
    std::vector<step> path;

  };

}


#endif
