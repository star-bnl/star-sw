// Calculate electron avalanches using microscopic tracking

#ifndef G_AVALANCHE_MICROSCOPIC_H
#define G_AVALANCHE_MICROSCOPIC_H

#include <vector>

#include <TH1F.h>

#include "Sensor.hh"

namespace Garfield {

class AvalancheMicroscopic {

  public:
    // Constructor
    AvalancheMicroscopic();
    // Destructor
    ~AvalancheMicroscopic() {}

    void SetSensor(Sensor* sensor);

    // Switch on/off calculation of induced currents
    void EnableSignalCalculation()  {useSignal = true;}
    void DisableSignalCalculation() {useSignal = false;}

    // Switch on/off filling histograms for energy distribution
    void EnableEnergyHistogramming(TH1F* histo);
    void DisableEnergyHistogramming();
    
    // Switch on/off filling histograms for distance distribution
    void EnableDistanceHistogramming(TH1F* histo, const char opt = 'z');
    void DisableDistanceHistogramming();

    // Switch on/off storage of drift lines
    void EnableDriftLines() {useDriftLines = true;}
    void DisableDriftLines() {useDriftLines = false;}

    // Switch on/off photon transport
    void EnablePhotonTransport() {usePhotons = true;}
    void DisablePhotonTransport() {usePhotons = false;}

    // Set/get energy threshold for electron transport
    // (useful for delta electrons)
    void   SetElectronTransportCut(const double cut) {deltaCut = cut;}
    double GetElectronTransportCut() const {return deltaCut;}

    // Set/get energy threshold for photon transport
    void   SetPhotonTransportCut(const double cut) {gammaCut = cut;}
    double GetPhotonTransportCut() const {return gammaCut;}
    
    // Set number of collisions to be skipped
    void SetCollisionSteps(const int n = 100);

    void GetAvalancheSize(int& ne, int& ni) const {ne = nElectrons; ni = nIons;}

    int  GetNumberOfEndpoints() const {return nEndpoints;}
    void GetEndpoint(const int i, 
                double& x0, double& y0, double& z0, double& t0, double& e0,
                double& x1, double& y1, double& z1, double& t1, double& e1,
                int& status) const;
    int GetNumberOfDriftLinePoints(const int i = 0) const;
    void GetDriftLinePoint(double& x, double& y, double& z, double &t,
                           const int ip, const int iel = 0) const;

    int  GetNumberOfPhotons() const {return nPhotons;}
    void GetPhoton(const int i, double& e,
                   double& x0, double& y0, double& z0, double& t0,
                   double& x1, double& y1, double& z1, double& t1,
                   int& status) const;

    // Run an avalanche calculation for an electron with given
    // initial coordinates, energy and direction (random if not specified)
    bool AvalancheElectron(const double x0, const double y0, const double z0, 
                           const double t0, const double e0, 
                           const double dx0 = 0., 
                           const double dy0 = 0., 
                           const double dz0 = 0.);
                   
    // Set user handling procedures
    void SetUserHandleAttachment(void (*f)(double x, double y, double z, 
                                           double t, 
                                           int type, int level, Medium* m));
    void UnsetUserHandleAttachment();
    void SetUserHandleInelastic(void (*f)(double x, double y, double z, 
                                          double t, 
                                          int type, int level, Medium* m));
    void UnsetUserHandleInelastic();
    void SetUserHandleIonisation(void (*f)(double x, double y, double z, 
                                           double t, 
                                           int type, int level, Medium* m));
    void UnsetUserHandleIonisation();

    // Switch on/off debugging and warning messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}
    void EnableWarnings()   {warning = true;}
    void DisableWarnings()  {warning = false;}

  private:

    // Numerical prefactors
    static double c1;
    static double c2;

    Sensor* sensor;

    struct point {
      double x, y, z, t;
    };

    struct electron {
      // Status
      int status;
      // Starting point
      double x0, y0, z0, t0;
      // Initial energy
      double e0;
      // Current position
      double x, y, z, t;    
      // Current direction
      double dx, dy, dz;
      // Current energy
      double energy;
      // Drift line
      std::vector<point> driftLine;
      double xLast, yLast, zLast;
    };
    std::vector<electron> stack;
    std::vector<electron> endpoints;

    int nPhotons;
    struct photon {
      // Status
      int status;
      // Energy
      double energy;
      // Starting point
      double x0, y0, z0, t0;
      // End point
      double x1, y1, z1, t1;
    };
    std::vector<photon> photons;

    // Number of electrons and ions produced
    int nElectrons;
    int nIons;
    // Number of electron trajectories (including captured electrons)
    int nEndpoints;

    TH1F* histEnergy;
    bool hasEnergyHistogram; 
    TH1F* histDistance;
    bool hasDistanceHistogram;
    char distanceOption;
    bool useSignal;
    bool useDriftLines;
    bool usePhotons;
    
    // Transport cuts
    double deltaCut;
    double gammaCut;

    int nCollSkip;
    
    // User procedures
    bool hasUserHandleAttachment;
    bool hasUserHandleInelastic;
    bool hasUserHandleIonisation;
    void (*userHandleAttachment)
         (double x, double y, double z, double t, 
          int type, int level, Medium* m);
    void (*userHandleInelastic)
         (double x, double y, double z, double t, 
          int type, int level, Medium* m);
    void (*userHandleIonisation)
         (double x, double y, double z, double t, 
         int type, int level, Medium* m);

    // Switch on/off debugging and warning messages
    bool debug, warning;

    // Photon transport
    void TransportPhoton(const double x, const double y, const double z,
                         const double t, const double e);

    double Min(const double x1, const double x2) const {
      return x1 > x2 ? x2 : x1;
    }
    double Max(const double x1, const double x2) const {
      return x1 < x2 ? x2 : x1;
    }
    
};

}

#endif
