#ifndef G_AVALANCHE_MC_H
#define G_AVALANCHE_MC_H

#include <vector>

#include "Sensor.hh"

namespace Garfield {

class AvalancheMC {

  public:
    // Constructor
    AvalancheMC();
    // Destructor
    ~AvalancheMC();
  
    void SetSensor(Sensor* s);

    // Switch on/off calculation of induced currents
    void EnableSignalCalculation()  {useSignal = true;}
    void DisableSignalCalculation() {useSignal = false;}

    // Switch on/off calculation of induced charge
    void EnableInducedChargeCalculation()  {useInducedCharge = true;}
    void DisableInducedChargeCalculation() {useInducedCharge = false;}
    
    // Switch on/off equilibration of multiplication and attachment 
    // over the drift line
    void EnableProjectedPathIntegration()  {useEquilibration = true;}
    void DisableProjectedPathIntegration() {useEquilibration = false;}
    
    // Switch on/off diffusion (for debugging)
    void EnableDiffusion()  {useDiffusion = true;}
    void DisableDiffusion() {useDiffusion = false;}
    
    // Stepping model
    // Fixed time step (default 20 ps)
    void SetTimeSteps(const double d = 0.02);
    // Fixed distance step (default 10 um)
    void SetDistanceSteps(const double d = 0.001);
    // Exponentially distributed time step with mean equal 
    // to the specified multiple of the collision time
    void SetCollisionSteps(const int n = 100);
    
    // Treat positive charge carriers as holes or ions
    void SetHoles() {useIons = false;}
    void SetIons()  {useIons = true;}

    void GetAvalancheSize(int& ne, int& ni) const {
      ne = nElectrons; ni = nIons;
    }
    
    int  GetNumberOfDriftLinePoints() const {return nDrift;}
    void GetDriftLinePoint(const int i, double& x, double& y, double& z, double& t);
    
    int  GetNumberOfEndpoints() const {return nEndpoints;}
    void GetEndpoint(const int i,
                     double& x0, double& y0, double& z0, double& t0,
                     double& x1, double& y1, double& z1, double& t1,
                     int& status) const;

    bool DriftElectron(const double x0, const double y0, const double z0,
                       const double t0);
    bool DriftHole(const double x0, const double y0, const double z0,
                   const double t0);
    bool DriftIon(const double x0, const double y0, const double z0,
                      const double t0);
    bool AvalancheElectron(const double x0, const double y0, const double z0, 
                           const double t0, const bool hole = false);
    bool AvalancheHole(const double x0, const double y0, const double z0, 
                       const double t0, const bool electron = false);
    bool AvalancheElectronHole(const double x0, const double y0, const double z0, 
                               const double t0);

    // Switch on/off debugging messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:

    // Numerical prefactors
    static double c1;
    
    Sensor* sensor;

    int nDrift;
    struct driftPoint {
      double x, y, z, t;
      double alpha, eta;
    };
    std::vector<driftPoint> drift;

    int nAval;
    struct avalPoint {
      double x, y, z, t;
      int ne, ni;
    };
    std::vector<avalPoint> aval;

    // Step size model
    int stepModel;
    // Fixed time step
    double tMc;
    // Fixed distance step
    double dMc;
    // Sample step size according to collision time
    int nMc;

    // Number of electrons and ions produced
    int nElectrons;
    int nIons;
    // Number of endpoints (including captured electrons)
    int nEndpoints;

    bool useSignal;
    bool useInducedCharge;
    bool useEquilibration;
    bool useDiffusion;
    bool useIons;
    bool withElectrons;
    bool withHoles;
    bool debug;
    
    // Compute a drift line with starting point (x0, y0, z0)
    bool DriftLine(const double x0, const double y0, const double z0, const double t0, const int q);
    bool Avalanche();
    // Compute effective multiplication and ionisation for the current drift line
    bool ComputeAlphaEta(const int q);
    // Compute the induced signal for the current drift line
    void ComputeSignal(const int q);
    void ComputeInducedCharge(const int q);

    double Min(const double x1, const double x2) const {
      return x1 > x2 ? x2 : x1;
    }

    double Max(const double x1, const double x2) const {
      return x1 < x2 ? x2 : x1;
    }

};

}

#endif
