// Calculate electron avalanches using microscopic tracking

#ifndef G_AVALANCHE_MICROSCOPIC_H
#define G_AVALANCHE_MICROSCOPIC_H

#include <vector>
#include <string>

#include <TH1.h>

#include "Sensor.hh"
#include "ViewDrift.hh"

namespace Garfield {

class AvalancheMicroscopic {

  public:
    // Constructor
    AvalancheMicroscopic();
    // Destructor
    ~AvalancheMicroscopic() {}

    void SetSensor(Sensor* sensor);

    // Switch on/off drift line plotting
    void EnablePlotting(ViewDrift* view);
    void DisablePlotting();

    // Switch on/off calculation of induced currents
    void EnableSignalCalculation()  {useSignal = true;}
    void DisableSignalCalculation() {useSignal = false;}

   // Switch on/off calculation of total induced charge
   void EnableInducedChargeCalculation()  {useInducedCharge = true;}
   void DisableInducedChargeCalculation() {useInducedCharge = false;}

    // Switch on/off filling histograms for energy distribution
    void EnableElectronEnergyHistogramming(TH1* histo);
    void DisableElectronEnergyHistogramming();
    void EnableHoleEnergyHistogramming(TH1* histo);
    void DisableHoleEnergyHistogramming();
    
    // Switch on/off filling histograms for distance distribution
    void SetDistanceHistogram(TH1* histo, const char opt = 'r');
    void EnableDistanceHistogramming(const int type);
    void DisableDistanceHistogramming(const int type);
    void DisableDistanceHistogramming();

    void EnableSecondaryEnergyHistogramming(TH1* histo);
    void DisableSecondaryEnergyHistogramming();

    // Switch on/off storage of drift lines
    void EnableDriftLines()  {useDriftLines = true;}
    void DisableDriftLines() {useDriftLines = false;}

    // Switch on/off photon transport
    void EnablePhotonTransport()  {usePhotons = true;}
    void DisablePhotonTransport() {usePhotons = false;}
    
    // Switch on/off stepping according to band structure E(k)
    void EnableBandStructure()  {useBandStructureDefault = true;}
    void DisableBandStructure() {useBandStructureDefault = false;}

    // Switch on/off update of coordinates for null-collision steps
    void EnableNullCollisionSteps()  {useNullCollisionSteps = true;}
    void DisableNullCollisionSteps() {useNullCollisionSteps = false;}

    // Set/get energy threshold for electron transport
    // (useful for delta electrons)
    void   SetElectronTransportCut(const double cut) {deltaCut = cut;}
    double GetElectronTransportCut() const {return deltaCut;}

    // Set/get energy threshold for photon transport
    void   SetPhotonTransportCut(const double cut) {gammaCut = cut;}
    double GetPhotonTransportCut() const {return gammaCut;}

    // Enable/disable max. avalanche size
    void EnableAvalancheSizeLimit(const int size) {sizeCut = size;}
    void DisableAvalancheSizeLimit() {sizeCut = -1;}
    int  GetAvalancheSizeLimit() const {return sizeCut;}
    
    // Enable/disable magnetic field in stepping algorithm
    void EnableMagneticField()  {useBfield = true;}
    void DisableMagneticField() {useBfield = false;}
    
    // Set number of collisions to be skipped
    void SetCollisionSteps(const int n = 100);

    void GetAvalancheSize(int& ne, int& ni) const {
      ne = nElectrons; ni = nIons;
    }
    void GetAvalancheSize(int& ne, int& nh, int& ni) const {
      ne = nElectrons; nh = nHoles; ni = nIons;
    }

    int  GetNumberOfElectronEndpoints() const {return nElectronEndpoints;}
    void GetElectronEndpoint(const int i, 
                double& x0, double& y0, double& z0, double& t0, double& e0,
                double& x1, double& y1, double& z1, double& t1, double& e1,
                int& status) const;
    int GetNumberOfElectronDriftLinePoints(const int i = 0) const;
    int GetNumberOfHoleDriftLinePoints(const int i = 0) const;
    void GetElectronDriftLinePoint(double& x, double& y, double& z, 
                                   double &t,
                                   const int ip, const int iel = 0) const;
    void GetHoleDriftLinePoint(double& x, double& y, double& z,
                               double& t,
                               const int ip, const int iel = 0) const;

    int  GetNumberOfHoleEndpoints() const {return nHoleEndpoints;}
    void GetHoleEndpoint(const int i,
                double& x0, double& y0, double& z0, double& t0, double& e0,
                double& x1, double& y1, double& z1, double& t1, double& e1,
                int& status) const;

    int  GetNumberOfPhotons() const {return usePhotons ? nPhotons : 0;}
    // Status codes:
    //   -2: photon absorbed by gas molecule
    void GetPhoton(const int i, double& e,
                   double& x0, double& y0, double& z0, double& t0,
                   double& x1, double& y1, double& z1, double& t1,
                   int& status) const;

    // Calculate an electron drift line for an electron with given
    // initial coordinates, energy and direction (random if not specified)
    // Secondary electrons are not transported
    bool DriftElectron(const double x0, const double y0, const double z0,
                       const double t0, const double e0,
           const double dx0 = 0., const double dy0 = 0., const double dz0 = 0.);

    // Calculate an avalanche initiated by an electron with given
    // initial coordinates, energy and direction (random if not specified)
    bool AvalancheElectron(const double x0, const double y0, const double z0,
                           const double t0, const double e0,
          const double dx0 = 0., const double dy0 = 0., const double dz0 = 0.);

    // Set user handling procedures
    void SetUserHandleStep(void (*f)(double x, double y, double z, 
                                     double t, double e,
                                     double dx, double dy, double dz,
                                     bool hole));
    void UnsetUserHandleStep();
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

    // Switch on/off debugging messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:

    std::string className;

    Sensor* sensor;

    struct point {
      double x, y, z, t;
    };

    struct electron {
      // Status
      int status;
      // Electron or hole
      bool hole;
      // Starting point
      double x0, y0, z0, t0;
      // Initial energy
      double e0;
      // Band
      int band;
      // Current position
      double x, y, z, t;    
      // Current direction/wavevector
      double kx, ky, kz;
      // Current energy
      double energy;
      // Drift line
      std::vector<point> driftLine;
      double xLast, yLast, zLast;
    };
    std::vector<electron> stack;
    std::vector<electron> endpointsElectrons;
    std::vector<electron> endpointsHoles;

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

    // Number of electrons, holes and ions produced
    int nElectrons;
    int nHoles;
    int nIons;
    // Number of electron/hole trajectories 
    // (including captured electrons)
    int nElectronEndpoints;
    int nHoleEndpoints;

    bool usePlotting;
    ViewDrift* viewer;

    TH1* histElectronEnergy;
    TH1* histHoleEnergy;
    bool hasElectronEnergyHistogram;
    bool hasHoleEnergyHistogram; 
    TH1* histDistance;
    bool hasDistanceHistogram;
    char distanceOption;
    int nDistanceHistogramTypes;
    std::vector<int> distanceHistogramType;

    TH1* histSecondary;
    bool hasSecondaryHistogram;

    bool useSignal;
    bool useInducedCharge;
    bool useDriftLines;
    bool usePhotons;
    bool useBandStructureDefault;
    bool useNullCollisionSteps;
    bool useBfield;
    
    // Rotation matrices
    double rb11, rb12, rb13;
    double rb21, rb22, rb23;
    double rb31, rb32, rb33;
    double rx22, rx23, rx32, rx33;
    
    // Transport cuts
    double deltaCut;
    double gammaCut;

    // Max. avalanche size
    int sizeCut;

    int nCollSkip;
    
    // User procedures
    bool hasUserHandleStep;
    bool hasUserHandleAttachment;
    bool hasUserHandleInelastic;
    bool hasUserHandleIonisation;
    void (*userHandleStep)
         (double x, double y, double z, double t,
          double e, double dx, double dy, double dz,
          bool hole);
    void (*userHandleAttachment)
         (double x, double y, double z, double t, 
          int type, int level, Medium* m);
    void (*userHandleInelastic)
         (double x, double y, double z, double t, 
          int type, int level, Medium* m);
    void (*userHandleIonisation)
         (double x, double y, double z, double t, 
         int type, int level, Medium* m);

    // Switch on/off debugging messages
    bool debug;

    // Electron transport
    bool TransportElectron(
        const double x0, const double y0, const double z0, 
        const double t0, const double e0,
        const double dx0, const double dy0, const double dz0, 
        const bool aval, bool hole);
    // Photon transport
    void TransportPhoton(const double x, const double y, const double z,
                         const double t, const double e);
                         
    void ComputeRotationMatrix(
        const double bx, const double by, const double bz, const double bmag,
        const double ex, const double ey, const double ez);       

    void RotateGlobal2Local(double& dx, double& dy, double& dz);
    void RotateLocal2Global(double& dx, double& dy, double& dz);
    
};

}

#endif
