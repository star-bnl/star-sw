#ifndef G_AVALANCHE_MC_H
#define G_AVALANCHE_MC_H

#include <vector>
#include <string>

#include "Sensor.hh"
#include "ViewDrift.hh"

namespace Garfield {

class AvalancheMC {

  public:
    // Constructor
    AvalancheMC();
    // Destructor
    ~AvalancheMC();
  
    void SetSensor(Sensor* s);

    // Switch on/off drift line plotting
    void EnablePlotting(ViewDrift* view);
    void DisablePlotting();

    // Switch on/off calculation of induced currents (default: disabled)
    void EnableSignalCalculation()  {useSignal = true;}
    void DisableSignalCalculation() {useSignal = false;}

    // Switch on/off calculation of induced charge (default: disabled)
    void EnableInducedChargeCalculation()  {useInducedCharge = true;}
    void DisableInducedChargeCalculation() {useInducedCharge = false;}
    
    // Switch on/off equilibration of multiplication and attachment 
    // over the drift line (default: enabled)
    void EnableProjectedPathIntegration()  {useEquilibration = true;}
    void DisableProjectedPathIntegration() {useEquilibration = false;}
    
    // Switch on/off diffusion (default: enabled)
    void EnableDiffusion()  {useDiffusion = true;}
    void DisableDiffusion() {useDiffusion = false;}

    // Switch on/off attachment (and multiplication) for
    // drift line calculation (default: enabled)
    // For avalanches the flag is ignored
    void EnableAttachment()  {useAttachment = true;}
    void DisableAttachment() {useAttachment = false;}
   
    // Enable/disable magnetic field in stepping algorithm.
    void EnableMagneticField()  {useBfield = true;}
    void DisableMagneticField() {useBfield = false;}
 
    // Stepping model
    // Fixed time step (default 20 ps)
    void SetTimeSteps(const double d = 0.02);
    // Fixed distance step (default 10 um)
    void SetDistanceSteps(const double d = 0.001);
    // Exponentially distributed time step with mean equal 
    // to the specified multiple of the collision time (default model)
    void SetCollisionSteps(const int n = 100);

    void SetTimeWindow(const double t0, const double t1);
    void UnsetTimeWindow();
    
    // Treat positive charge carriers as holes or ions (default: ions)
    void SetHoles() {useIons = false;}
    void SetIons()  {useIons = true;}

    void SetElectronSignalScalingFactor(const double scale) {
      scaleElectronSignal = scale;
    }
    void SetHoleSignalScalingFactor(const double scale) {
      scaleHoleSignal = scale;
    }
    void SetIonSignalScalingFactor(const double scale) {
      scaleIonSignal = scale;
    }

    void GetAvalancheSize(int& ne, int& ni) const {
      ne = nElectrons; ni = nIons;
    }
    
    int  GetNumberOfDriftLinePoints() const {return nDrift;}
    void GetDriftLinePoint(const int i, 
                           double& x, double& y, double& z, double& t);
    
    int  GetNumberOfElectronEndpoints() const {return nEndpointsElectrons;}
    int  GetNumberOfHoleEndpoints() const     {return nEndpointsHoles;}
    int  GetNumberOfIonEndpoints() const      {return nEndpointsIons;}
    
    void GetElectronEndpoint(const int i,
                             double& x0, double& y0, double& z0, double& t0,
                             double& x1, double& y1, double& z1, double& t1,
                             int& status) const;
    void GetHoleEndpoint(const int i,
                         double& x0, double& y0, double& z0, double& t0,
                         double& x1, double& y1, double& z1, double& t1,
                         int& status) const;
    void GetIonEndpoint(const int i,
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
    bool AvalancheElectronHole(
                       const double x0, const double y0, const double z0, 
                       const double t0);

    // Switch on/off debugging messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:

    std::string className;

    // Numerical prefactors
    static double c1;
    
    Sensor* sensor;

    int nDrift;
    struct driftPoint {
      // Position
      double x, y, z, t;
      // Townsend and attachment coefficient
      double alpha, eta;
      // Number of secondaries produced at this point
      int ne, nh, ni;
    };
    std::vector<driftPoint> drift;

    struct avalPoint {
      double x, y, z, t;
      int ne, nh, ni;
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

    // Time window
    bool hasTimeWindow;
    double tMin, tMax;

    // Number of electrons, holes and ions produced
    int nElectrons, nHoles, nIons;

    // Number of endpoints (including captured electrons)
    int nEndpointsElectrons;
    int nEndpointsHoles;
    int nEndpointsIons;
    struct endpoint {
      double x0, y0, z0, t0;
      double x1, y1, z1, t1;
      int status;
    };
    std::vector<endpoint> endpointsElectrons;
    std::vector<endpoint> endpointsHoles;
    std::vector<endpoint> endpointsIons;

    bool usePlotting;
    ViewDrift* viewer;

    bool useSignal;
    bool useInducedCharge;
    bool useEquilibration;
    bool useDiffusion;
    bool useAttachment;
    bool useBfield;
    bool useIons;
    bool withElectrons;
    bool withHoles;
    double scaleElectronSignal;
    double scaleHoleSignal;
    double scaleIonSignal;

    bool debug;
    
    // Compute a drift line with starting point (x0, y0, z0)
    bool DriftLine(const double x0, const double y0, const double z0, 
                   const double t0, const int type, const bool aval = false);
    bool Avalanche();
    // Compute effective multiplication and ionisation 
    // for the current drift line
    bool ComputeAlphaEta(const int q);
    // Compute the induced signal for the current drift line
    void ComputeSignal(const double q);
    void ComputeInducedCharge(const double q);

    double Min(const double x1, const double x2) const {
      return x1 > x2 ? x2 : x1;
    }

    double Max(const double x1, const double x2) const {
      return x1 < x2 ? x2 : x1;
    }

};

}

#endif
